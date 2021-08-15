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
           #:parse

           #:char-parser
           #:predicate-parser
           #:string-parser
           #:eof-parser

           #:parse-map

           #:parse-progn
           #:parse-prog1
           #:parse-prog2

           #:parse-or
           #:parse-optional

           #:parse-collect
           #:parse-collect1
           #:parse-collect-string
           #:parse-reduce
           #:parse-take

           #:parse-try
           #:parse-tag

           #:parse-let
           #:parse-defer
           #:defparser

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
  cursor element format-control format-args)

(defun unexpected (state element string)
  (make-failure :cursor (state-cursor state)
                :element element
                :format-control "Unexpected element ~A"
                :format-args (list string)))

(defun expected (state element string)
  (make-failure :cursor (state-cursor state)
                :element element
                :format-control "Expected element ~A"
                :format-args (list string)))

(defun unknown (state)
  (make-failure :cursor (state-cursor state)
                :format-control "Unknown error"))

(define-condition parser-error (error)
  ((format-control :initarg :format-control)
   (format-args :initarg :format-args)
   (cursor :initarg :cursor :reader parser-error-cursor)
   (element :initarg :element :reader parser-error-element))
  (:report (lambda (condition stream)
             (with-slots (format-control format-args) condition
               (apply #'format stream format-control format-args)))))

(defun failure-error (failure)
  "Signal an error depending on the given failure."
  (error 'parser-error
         :format-control (failure-format-control failure)
         :format-args (failure-format-args failure)
         :element (failure-element failure)
         :cursor (failure-cursor failure)))

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
          (funcall efail (unexpected state :end-of-file "EOF"))
          (destructuring-bind (e &rest es) input
            (if (funcall pred e)
                (funcall cok
                         e
                         (advance-state state advance-cursor e es)
                         (unknown state))
                (funcall efail
                         (unexpected state e (funcall printer e)))))))))



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
           (funcall efail (expected state string
                                    (format nil "\"~A\"" string))))
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
          (funcall efail (expected state :end-of-file "EOF"))))))

(defun eof-parser ()
  +eof-parser+)
