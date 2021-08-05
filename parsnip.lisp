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
                #:rcurry
                #:compose)
  (:export #:parser-error
           #:parser-error-element
           #:parser-error-return-trace
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



(defstruct (just (:constructor just (value))
                 (:copier nil))
  value)

(defstruct (failure (:constructor expected (element stream partially-parsed))
                    (:print-function nil))
  element stream partially-parsed return-trace)

(define-condition parser-error (stream-error)
  ((element :initarg :element :reader parser-error-element)
   (return-trace :initarg :return-trace :reader parser-error-return-trace))
  (:report (lambda (condition stream)
             (format stream "Expected element ~S on ~S"
                     (parser-error-element condition)
                     (stream-error-stream condition)))))

(defun error-failure (failure)
  "Signal an error depending on the given failure."
  (error 'parser-error
         :element (failure-element failure)
         :stream (failure-stream failure)))

(defun parse (parser stream)
  "Run a parser through a given stream and raise any failures as a condition."
  (let ((result (funcall parser stream)))
    (etypecase result
      (just (just-value result))
      (failure (error-failure result)))))



;; Parsers

(defun char-parser (char)
  "Return a parser that accepts the given character value."
  (lambda (stream)
    (let ((actual-char (peek-char nil stream nil)))
      (cond
        ((null actual-char)
         (expected char stream nil))
        ((char= char actual-char)
         (just (read-char stream)))
        (t (expected char stream nil))))))

(defun predicate-parser (predicate)
  "Return a parser that accepts a character if the given predicate returns
   true."
  (lambda (stream)
    (let ((actual-char (peek-char nil stream nil)))
      (cond
        ((null actual-char)
         (expected predicate stream nil))
        ((funcall predicate actual-char)
         (just (read-char stream)))
        (t (expected predicate stream nil))))))

(defun string-parser (string)
  "Return a parser that accepts the given string value. May partially parse on
   failure."
  (lambda (stream)
    (let* ((actual-string (make-string (length string)))
           (chars-read (read-sequence actual-string stream)))
      (cond
        ((< chars-read (length string))
         (expected string stream (plusp chars-read)))
        ((string= actual-string string)
         (just actual-string))
        (t (expected string stream (plusp chars-read)))))))

(defun eof-parser (&optional value)
  (lambda (stream)
    (if (null (peek-char nil stream nil))
        (just value)
        (expected :eof stream nil))))



;; Internal helper macros

(defmacro with-just (form (&optional var) &body body)
  (once-only (form)
    `(etypecase ,form
       (just ,(if var
                  `(let ((,var (just-value ,form)))
                     ,@body)
                  `(progn ,@body)))
       (failure ,form))))

(defmacro with-failure (form () &body body)
  (once-only (form)
    `(etypecase ,form
       (just ,form)
       (failure (if (failure-partially-parsed ,form)
                    ,form
                    (progn ,@body))))))

(defmacro with-trace ((name) &body body)
  (with-gensyms (result)
    `(let ((,result (progn ,@body)))
       (etypecase ,result
         (just ,result)
         (failure (progn
                    (push (quote ,name) (failure-return-trace ,result))
                    ,result))))))



;; Combinators

(defun parse-map (function &rest parsers)
  "Compose multiple parsers to run in sequence, and apply the function to all
   parsers' values."
  (lambda (stream)
    (do ((parsers* parsers (rest parsers*))
         (partially-parsed nil t)
         list)
        ((null parsers*)
         (just (apply function (nreverse list))))
        (let ((result (funcall (first parsers*) stream)))
          (etypecase result
            (just (push (just-value result) list))
            (failure (setf (failure-partially-parsed result) partially-parsed)
                     (return result)))))))

(defun parse-progn (&rest parsers)
  "Compose multiple parsers to run in sequence, returning the last parser's
   value."
  (assert (plusp (length parsers)))
  (lambda (stream)
    (do* ((parsers-left parsers (rest parsers-left))
          (partially-parsed nil t)
          result)
      ((null parsers-left) result)
      (setf result (funcall (first parsers-left) stream))
      (when (failure-p result)
        (setf (failure-partially-parsed result) partially-parsed)
        (return result)))))

(defun parse-prog1 (first-parser &rest parsers)
  "Compose multiple parsers to run in sequence, returning the first parser's
   value."
  (when (null parsers)
    (return-from parse-prog1 first-parser))

  (let ((inner-parser (apply 'parse-progn parsers)))
    (lambda (stream)
      (let ((first-result (funcall first-parser stream)))
        (with-just first-result ()
          (let ((final-result (funcall inner-parser stream)))
            (etypecase final-result
              (just first-result)
              (failure (setf (failure-partially-parsed final-result) t)
                       final-result))))))))

(defun parse-prog2 (first-parser second-parser &rest parsers)
  "Compose multple parsers to run in sequence, returning the second parser's
   value."
  (parse-progn first-parser (apply 'parse-prog1 second-parser parsers)))

(defun parse-collect (parser)
  "Enhance the parser to keep running until failure, and collect the results.
   Passes through any partial-parse falures."
  (lambda (stream)
    (do ((list () (cons (just-value result) list))
         (result (funcall parser stream) (funcall parser stream)))
        ((failure-p result)
         (with-failure result ()
           (just (nreverse list)))))))

(defun parse-collect1 (parser)
  "Enhance the parser to keep running until failure, and collect AT LEAST one
   result. Passes through any partial-parse failure."
  (let ((collect-parser (parse-collect parser)))
    (lambda (stream)
      (with-just (funcall parser stream) (head)
        (with-just (funcall collect-parser stream) (tail)
          (just (cons head tail)))))))

(defun parse-reduce (function parser initial-value)
  "Enhance the parser to keep running until failure, and reduce the results
   into a single value. Passes through any partial-parse failure."
  (lambda (stream)
    (do ((value initial-value (funcall function value (just-value result)))
         (result (funcall parser stream) (funcall parser stream)))
        ((failure-p result)
         (with-failure result ()
           (just value))))))

(defun parse-take (times parser)
  "Enhance the partial to run EXACTLY the given number of times and collect the
   results. Passes through any partial-parse failure."
  (check-type times (integer 0 *))
  (labels ((take-iter (n stream)
             (if (zerop n)
                 (just ())
                 (with-just (funcall parser stream) (head)
                   (with-just (take-iter (1- n) stream) (tail)
                     (just (cons head tail)))))))
    (lambda (stream)
      (with-just (take-iter times stream) (list)
        (just list)))))

(defun parse-or (&rest parsers)
  "Attempts each parser in order until one succeeds. Passes through any
   partial-parse failure."
  (lambda (stream)
    (do ((parsers-left parsers (rest parsers-left))
         expected-elements)
        ((null parsers-left)
         (expected (nreverse expected-elements) stream nil))
        (let ((result (funcall (first parsers-left) stream)))
          (if (or (just-p result)
                  (failure-partially-parsed result))
              (return result)
              (push (failure-element result) expected-elements))))))

(defun parse-optional (parser &optional default)
  "Enhance the parser to resume from an error with a default value if it did
   not consume input."
  (lambda (stream)
    (with-failure (funcall parser stream) ()
      (just default))))

(defun parse-try (parser)
  "Enahnce the parser to try to rewind the stream on partial-parse failure.
   Only works on seekable streams."
  (lambda (stream)
    (let ((old-position (file-position stream))
          (result (funcall parser stream)))
      (when (and (failure-p result)
                 (failure-partially-parsed result))
        (file-position stream old-position)
        (setf (failure-partially-parsed result) nil))
      result)))

(defun parse-tag (tag parser)
  "Reports failures as expecting the given tag instead of an element"
  (lambda (stream)
    (let ((result (funcall parser stream)))
      (etypecase result
        (failure (progn (setf (failure-element result) tag)
                        result))
        (just result)))))

(defmacro parse-let (bindings &body body)
  "Compose multiple parsers together to bind their results to variables and
   return a value within the body."
  (flet ((bind-just (stream var result-form)
           `(with-just (funcall ,var ,stream) (,var)
              ,result-form)))
    (with-gensyms (stream)
      `(lambda (,stream)
         (let ,bindings
           ,(reduce (curry #'bind-just stream) (mapcar #'car bindings)
                    :initial-value `(just (progn ,@body))
                    :from-end t))))))

(defmacro parse-defer (form)
  "Defer evaluating the parser-returning FORM until the parser is called.
   Useful for circular-referencing parsers."
  (with-gensyms (stream)
    `(lambda (,stream) (funcall ,form ,stream))))

(defmacro defparser (name () &body body)
  "Define a parser as a function.
   Enables other parsers to forward-reference it before it is defined."
  (with-gensyms (stream)
    `(defun ,name (,stream)
       (with-trace (,name)
         (funcall (progn ,@body) ,stream)))))



;; Complex Parselets

(defun digit-parser (&optional (radix 10))
  (check-type radix (integer 2 36))
  (parse-map (lambda (ch)
               (let ((code (char-code ch)))
                 (if (<= #.(char-code #\0) code #.(char-code #\9))
                     (- code #.(char-code #\0))
                     (+ 10 (- (char-code (char-upcase ch)) #.(char-code #\A))))))
             (predicate-parser (rcurry #'digit-char-p radix))))

(defun integer-parser (&optional (radix 10))
  (parse-map (curry #'reduce (lambda (num dig) (+ (* num radix) dig)))
             (parse-collect1 (digit-parser radix))))
