;;; parsnip.lisp - Parsnip library implementation

;;; Copyright 2021 Samuel Hunter <samuel (at) shunter (dot) xyz>
;;; BSD-3-Clause

(defpackage #:xyz.shunter.parsnip
  (:documentation "Parser combinator library")
  (:nicknames #:parsnip)
  (:use #:cl)
  (:import-from #:alexandria
                #:with-gensyms
                #:once-only
                #:curry
                #:rcurry)
  (:export #:cursor-pos
           #:cursor-line
           #:cursor-col
           #:parser-error
           #:parser-error-element
           #:parser-error-cursor

           #:parse-let
           #:defparser
           #:parse

           #:parse-map

           #:parse-progn
           #:parse-prog1
           #:parse-prog2

           #:parse-or
           #:parse-optional

           #:parse-collect
           #:parse-collect1
           #:parse-reduce
           #:parse-take

           #:parse-try
           #:parse-tag

           #:char-parser
           #:predicate-parser
           #:string-parser
           #:eof-parser

           #:parse-collect-string
           #:digit-parser
           #:integer-parser))

(in-package #:xyz.shunter.parsnip)



(defstruct (cursor (:predicate nil)
                   (:copier nil))
  (pos 0)
  (line 1)
  (col 0))

(defstruct (state (:predicate nil)
                  (:copier nil))
  (cursor (make-cursor))
  input)

(defstruct (failure (:predicate nil)
                    (:copier nil))
  state messages)

(defun failure (state format-control &rest format-args)
  (make-failure :state state
                :messages (list (apply #'format nil format-control format-args))))

(defun unexpected (state name)
  (failure state "Unexpected element ~A" name))

(defun expected (state name)
  (failure state "Expected element ~A" name))

(defun unknown (state)
  (failure state "Unknown error"))

(define-condition parser-error (error)
  ((cursor :initarg :cursor :reader parser-error-cursor)
   (messages :initarg :messages :reader parser-error-messages))
  (:report (lambda (condition stream)
             (format stream "~S" (parser-error-messages condition)))))

(defun failure-error (failure)
  "Signal an error depending on the given failure."
  (error 'parser-error
         :cursor (state-cursor (failure-state failure))
         :messages (Failure-messages failure)))

(defun ok-value (x state failure)
  (declare (ignore state failure))
  x)

(defun parse (parser input)
  "Run a parser through a list of elements and raise any failures as a condition."
  (funcall parser (make-state :input input)
           #'ok-value
           #'failure-error
           #'ok-value
           #'failure-error))



;; Utilities

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun ensure-let-bindings (macro-name bindings)
    (do ((bindings-left bindings (rest bindings-left))
         (binding (first bindings) (first bindings-left)))
        ((null bindings-left)

         (check-type binding list)
         (unless (= 2 (length binding))
           (error "~S binding must have a length of 2" macro-name))
         (check-type (first binding) symbol)))))

(defmacro nlet (name (&rest bindings) &body body)
  (ensure-let-bindings 'nlet bindings)
  `(labels ((,name ,(mapcar #'first bindings)
              .,body))
     (,name .,(mapcar #'second bindings))))



;; Primitives

(defun advance-state (state advance-cursor e es)
  (make-state :cursor (funcall advance-cursor (state-cursor state) e)
              :input es))

(defun token-prim (printer advance-cursor pred)
  (lambda (state cok cfail eok efail)
    (declare (ignore cfail eok))
    (with-accessors ((input state-input)
                     (cursor state-cursor)) state
      (if (null input)
          (funcall efail (unexpected state "EOF"))
          (destructuring-bind (e &rest es) input
            (if (funcall pred e)
                (funcall cok
                         e
                         (advance-state state advance-cursor e es)
                         (unknown state))
                (funcall efail
                         (unexpected state (funcall printer e)))))))))



;; Parser macros

(defmacro parse-let ((&rest bindings) &body body)
  (ensure-let-bindings 'parse-let bindings)
  `(parse-map (lambda ,(mapcar #'first bindings)
                .,body)
              .,(mapcar #'second bindings)))

(defmacro defparser (name () &body (form))
  (with-gensyms (state cok cfail eok efail)
    `(defun ,name (,state ,cok ,cfail ,eok ,efail)
       (funcall ,form ,state ,cok ,cfail ,eok ,efail))))



;; Parser combinators

(defun parse-map (function &rest parsers)
  (lambda (state cok cfail eok efail)
    (nlet iter ((state state)
                (eok eok)
                (efail efail)
                (args ())
                (parsers-left parsers))
      (if (null parsers-left)
          (funcall eok (apply function (nreverse args)) state (unknown state))
          (funcall (first parsers-left) state
                   ;; consumed-ok
                   (lambda (x state failure)
                     (declare (ignore failure))
                     (iter state cok cfail
                           (cons x args)
                           (rest parsers-left)))
                   ;; consumed-fail
                   cfail
                   ;; empty-ok
                   (lambda (x state failure)
                     (declare (ignore failure))
                     (iter state eok efail
                           (cons x args)
                           (rest parsers-left)))
                   ;; empty-fail
                   efail)))))

(defun parse-progn (&rest parsers)
  (lambda (state cok cfail eok efail)
    (nlet iter ((state state)
                (eok eok)
                (efail efail)
                (last-arg nil)
                (parsers-left parsers))
      (if (null parsers-left)
          (funcall eok last-arg state (unknown state))
          (funcall (first parsers-left) state
                   ;; consumed-ok
                   (lambda (x state failure)
                     (declare (ignore failure))
                     (iter state cok cfail
                           x
                           (rest parsers-left)))
                   ;; consumed-fail
                   cfail
                   ;; empty-ok
                   (lambda (x state failure)
                     (declare (ignore failure))
                     (iter state eok efail
                           x
                           (rest parsers-left)))
                   ;; empty-fail
                   efail)))))

(defun parse-prog1 (first-parser &rest rest-parsers)
  (parse-let ((first first-parser)
              (last (apply #'parse-progn rest-parsers)))
    (declare (ignore last))
    first))

(defun parse-prog2 (first-parser second-parser &rest rest-parsers)
  (parse-let ((first first-parser)
              (second second-parser)
              (last (apply #'parse-progn rest-parsers)))
    (declare (ignore first last))
    second))

(defun parse-or (&rest parsers)
  (lambda (state cok cfail eok efail)
    (nlet iter ((messages ())
                (parsers-left parsers))
      (if (null parsers-left)
          (funcall efail (make-failure :state state
                                       :messages messages))
          (funcall (first parsers-left) state
                   ;; consumed-ok
                   cok
                   ;; consumed-failure
                   cfail
                   ;; empty-ok
                   eok
                   ;; empty-failure
                   (lambda (failure)
                     (iter (append (failure-messages failure) messages)
                           (rest parsers-left))))))))

(defun parse-optional (parser &optional error-value)
  (lambda (state cok cfail eok efail)
    (declare (ignore efail))
    (funcall parser state
             ;; consumed-ok
             cok
             ;; consumed-fail
             cfail
             ;; empty-ok
             eok
             ;; empty-fail
             (lambda (failure)
               (funcall eok error-value state failure)))))

(defun collect-failure (state x)
  ;; Backing parser of collect combinators MUST always consume on success.
  (failure state "Parsed element '~S' without consuming input" x))

(defun parse-collect (parser)
  (lambda (state cok cfail eok efail)
    (nlet iter ((state state)
                (eok eok)
                (efail efail)
                (results ()))
      (funcall parser state
               ;; consumed-ok
               (lambda (x state failure)
                 (declare (ignore failure))
                 (iter state cok cfail (cons x results)))
               ;; consumed-failure
               cfail
               ;; empty-ok
               (lambda (x state failure)
                 (declare (ignore failure))
                 ;; Backing parser of collect MUST always consume
                 (funcall efail (collect-failure state x)))
               ;; empty-failure
               (lambda (failure)
                 (funcall eok (nreverse results) state failure))))))

(defun parse-collect1 (parser)
  (lambda (state cok cfail eok efail)
    (declare (ignore eok))
    (funcall parser state
             ;; consumed-ok
             (lambda (x state failure)
               (declare (ignore failure))
               (funcall (parse-map (curry #'cons x)
                                   (parse-collect parser))
                        state cok cfail cok cfail))
             ;; consumed-failure
             cfail
             ;; empty-ok
             (lambda (x state failure)
               (declare (ignore failure))
               (funcall efail (collect-failure state x)))
             ;; empty-failure
             efail)))

(defun parse-reduce (function parser initial-value)
  (lambda (state cok cfail eok efail)
    (nlet iter ((state state)
                (eok eok)
                (efail efail)
                (result initial-value))
      (funcall parser state
               ;; consumed-ok
               (lambda (x state failure)
                 (declare (ignore failure))
                 (iter state cok cfail (funcall function result x)))
               ;; consumed-failure
               cfail
               ;; empty-ok
               (lambda (x state failure)
                 (declare (ignore failure))
                 (funcall efail (collect-failure state x)))
               ;; empty-failure
               (lambda (failure)
                 (funcall eok result state failure))))))

(defun parse-take (n parser)
  (check-type n (integer 0 *))
  (lambda (state cok cfail eok efail)
    (nlet iter ((state state)
                (eok eok)
                (efail efail)
                (results ())
                (n-left n))
      (if (zerop n-left)
          (funcall eok (nreverse results) state (unknown state))
          (funcall parser state
                   ;; consumed-ok
                   (lambda (x state failure)
                     (declare (ignore failure))
                     (iter state cok cfail (cons x results) (1- n-left)))
                   ;; consumed-failure
                   cfail
                   ;; empty-ok
                   (lambda (x state failure)
                     (declare (ignore failure))
                     (funcall efail (collect-failure state x)))
                   ;; empty-failure
                   efail)))))

(defun parse-try (parser)
  (lambda (state cok cfail eok efail)
    (declare (ignore cfail))
    (funcall parser state
             cok      ;; consumed-ok
             efail    ;; consumed-failure
             eok      ;; error-ok
             efail))) ;; error-failure

(defun parse-tag (name parser)
  (lambda (state cok cfail eok efail)
    (flet ((wrap-fail-cont (fail-cont)
             (lambda (failure)
               (declare (ignore failure))
               (funcall fail-cont (expected state name)))))
      (funcall parser state
               cok ;; consumed-ok
               (wrap-fail-cont cfail) ;; consumed-failure
               eok ;; error-ok
               (wrap-fail-cont efail))))) ;; error-failure



;; Character parsers

(defparameter +tab-width+ 8)

(defun advance-cursor-char (cursor char)
  (with-accessors ((pos cursor-pos)
                   (line cursor-line)
                   (col cursor-col)) cursor
    (cond
      ((char= char #\Newline) (make-cursor :pos (1+ pos)
                                      :line (1+ line)
                                      :col 0))
      ((char= char #\Tab) (make-cursor :pos (1+ pos)
                                  :line line
                                  :col  (+ col (1- +tab-width+)
                                           (- (mod col +tab-width+)))))
      (t (make-cursor :pos (1+ pos)
                      :line line
                      :col (1+ col))))))

(defun print-char (char)
  (format nil "'~C'" char))

(defun char-parser (char)
  (token-prim #'print-char
              #'advance-cursor-char
              (curry #'char= char)))

(defun predicate-parser (pred)
  (token-prim #'print-char
              #'advance-cursor-char
              pred))

(defun string-parser (string)
  (lambda (state cok cfail eok efail)
    (nlet iter ((state state)
                (eok eok)
                (efail efail)
                (elms-left (coerce string 'list)))
      (with-accessors ((input state-input)
                       (cursor state-cursor)) state
        (cond
          ((null elms-left)
           (funcall eok string state (unknown state)))
          ((or (null input) (not (char= (first elms-left)
                                        (first input))))
           (funcall efail (expected state (format nil "\"~A\"" string))))
          (t (iter (advance-state state #'advance-cursor-char
                                  (first input) (rest input))
                   cok cfail
                   (rest elms-left))))))))

(defparameter +eof-parser+
  (lambda (state cok cfail eok efail)
    (declare (ignore cok cfail))
    (with-accessors ((input state-input)) state
      (if (null input)
          (funcall eok nil state (unknown state))
          (funcall efail (expected state "EOF"))))))

(defun eof-parser ()
  +eof-parser+)



;; Compound parsers

(defun parse-collect-string (parser)
  (parse-map (rcurry #'coerce 'string)
             (parse-collect parser)))

(defun digit-parser (&optional (radix 10))
  (parse-map (lambda (c)
               (if (<= (char-code #\0) (char-code c) (char-code #\9))
                   (- (char-code c) (char-code #\0))
                   (- (+ 10 (char-code (char-upcase c))) (char-code #\A))))
             (predicate-parser (rcurry #'digit-char-p radix))))

(defun integer-parser (&optional (radix 10))
  (parse-let ((digits (parse-collect1 (digit-parser radix))))
    (reduce (lambda (n d)
              (+ (* n radix) d))
            digits)))
