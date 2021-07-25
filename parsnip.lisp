;;; parsnip.lisp - Parsnip library implementation

;;; Copyright 2021 Samuel Hunter <samuel (at) shunter (dot) xyz>
;;; BSD-3-Clause

(defpackage #:xyz.shunter.parsnip
  (:nicknames #:parsnip)
  (:use #:cl)
  (:import-from #:alexandria
                #:with-gensyms
                #:once-only
                #:curry
                #:rcurry
                #:compose)
  (:export #:parser-expected-element
           #:parser-element-name
           #:parse

           #:char-parser
           #:predicate-parser
           #:string-parser

           #:parse-map

           #:parse-progn
           #:parse-prog1
           #:parse-prog2

           #:parse-any
           #:parse-optional

           #:parse-collect
           #:parse-collect1
           #:parse-reduce
           #:parse-do
           #:parse-take

           #:parse-try
           #:parse-name

           #:parse-let
           #:parse-defer))

(in-package #:xyz.shunter.parsnip)



(define-condition parser-expected-element (stream-error)
  ((element-name :initarg :name :reader parser-element-name))
  (:report (lambda (condition stream)
             (format stream "Expected element ~S on ~S"
                     (parser-element-name condition)
                     (stream-error-stream condition)))))

(defclass result ()
  ())

(defclass just (result)
  ((value :initarg :value :reader just-value)))

(defclass failure (result)
  ((error :initarg :error :reader failure-error)
   (consumed-chars :initarg :consumed-chars
                   :reader failure-consumed-chars)))

(defmethod print-object ((object just) stream)
  (print-unreadable-object (object stream :type t)
    (print (just-value object) stream)))

(defmethod print-object ((object failure) stream)
  (print-unreadable-object (object stream :type t)
    (print (failure-error object) stream)))

(defun just (value)
  (make-instance 'just
                 :value value))

(defun failure (consumed-chars error)
  (make-instance 'failure
                 :error error
                 :consumed-chars consumed-chars))

(defun justp (result)
  (typep result 'just))

(defun failurep (result)
  (typep result 'failure))

(defun expected (name stream consumed-chars)
  (failure consumed-chars
           (make-condition 'parser-expected-element
                           :name name
                           :stream stream)))

(defun eof (stream consumed-chars)
  (failure consumed-chars
           (make-condition 'end-of-file
                           :stream stream)))

(defun parse (parser stream)
  (let ((result (funcall parser stream)))
    (etypecase result
      (just (just-value result))
      (failure (error (failure-error result))))))



;; Parsers

(defun char-parser (char)
  "Return a parser that accepts the given character value. Does not consume
   input on failure."
  (lambda (stream)
    (let ((actual-char (peek-char nil stream nil)))
      (cond
        ((null actual-char) (eof stream nil))
        ((char= char actual-char)
         (just (read-char stream)))
        (t (expected char stream nil))))))

(defun predicate-parser (predicate)
  "Return a parser that accepts a character if the given predicate returns
   true. Does not consume input on failure."
  (lambda (stream)
    (let ((actual-char (peek-char nil stream nil)))
      (cond
        ((null actual-char) (eof stream nil))
        ((funcall predicate actual-char)
         (just (read-char stream)))
        (t (expected predicate stream nil))))))

(defun string-parser (string)
  "Return a parser that accepts the given string value. May consume input on
   failure."
  (lambda (stream)
    (let* ((actual-string (make-string (length string)))
           (chars-read (read-sequence actual-string stream)))
      (cond
        ((< chars-read (length string))
         (eof stream (plusp chars-read)))
        ((string= actual-string string)
         (just actual-string))
        (t (expected string stream t))))))



;; Internal helper macros

(defmacro with-just (form (&optional value) &body body)
  (once-only (form)
    `(etypecase ,form
       (just ,(if value
                  `(let ((,value (just-value ,form)))
                     ,@body)
                  `(progn ,@body)))
       (failure ,form))))

(defmacro with-failure (form (&optional error) &body body)
  (once-only (form)
    `(etypecase ,form
       (just ,form)
       (failure (if (failure-consumed-chars ,form)
                    ,form
                    ,(if error
                         `(let ((,error (failure-error ,form)))
                            ,@body)
                         `(progn ,@body)))))))

(defmacro with-result (form
                       ((&optional just-value) &body just-body)
                       ((&optional failure-error) &body failure-body))
  (once-only (form)
    `(etypecase ,form
       (just ,(if just-value
                  `(let ((,just-value (just-value ,form)))
                     ,@just-body)
                  `(progn ,@just-body)))
       (failure (if (failure-consumed-chars ,form)
                    ,form
                    ,(if failure-error
                         `(let ((,failure-error (failure-error ,form)))
                            ,@failure-body)
                         `(progn ,@failure-body)))))))



;; Combinators

(defun parse-map (parser function)
  "Enhance the parser to map any return value to a different one."
  (lambda (stream)
    (with-just (funcall parser stream) (value)
      (just (funcall function value)))))

(defun parse-progn (&rest parsers)
  "Compose multiple parsers to run them in sequence, returning the last
   parser's value. Consumes input on failure when the first parser succeeds."
  (assert (plusp (length parsers)))
  (lambda (stream)
    (do* ((parsers-left parsers (rest parsers-left))
          result)
      ((null parsers-left) result)
      (setf result (funcall (first parsers-left) stream))
      (when (failurep result)
        (return result)))))

(defun parse-prog1 (first-parser &rest parsers)
  "Compose multiple parsers to run them in sequence, returning the first
   parser's value. Consumes input on failure when the first parser succeeds."
  (when (null parsers)
    (return-from parse-prog1 first-parser))

  (let ((inner-parser (apply 'parse-progn parsers)))
    (lambda (stream)
      (let ((first-result (funcall first-parser stream)))
        (with-just first-result ()
          (with-just (funcall inner-parser stream) ()
            first-result))))))

(defun parse-prog2 (first-parser second-parser &rest parsers)
  "Compose multple parsers to run them in sequence, returning the second
   parser's value. Consumes input on failure when the first parser succeeds."
  (parse-progn first-parser (apply 'parse-prog1 second-parser parsers)))

(defun parse-collect (parser)
  "Enhance the parser to keep running until failure, and collect the results.
   Consumes input on error when the last (errorful) parse consumes input.

   Fails when the containing parser consumes input on failure."
  (lambda (stream)
    (do ((list () (cons (just-value result) list))
         (result (funcall parser stream) (funcall parser stream)))
        ((failurep result)
         (with-failure result ()
           (just (nreverse list)))))))

(defun parse-collect1 (parser)
  "Enhance the parser to keep running until failure, and collect the results.
   Consumes input on error when the last (errorful) parse consumes input or if
   the first run failed.

   Fails when it could not succeed once.
   Fails when the containing parser consumes input during failure."
  (let ((collect-parser (parse-collect parser)))
    (lambda (stream)
      (with-just (funcall parser stream) (head)
        (with-just (funcall collect-parser stream) (tail)
          (just (cons head tail)))))))

(defun parse-reduce (function parser initial-value)
  "Enhance the parser to keep running until failure, and reduce the results
   into a single value."
  (lambda (stream)
    (do ((value initial-value (funcall function value (just-value result)))
         (result (funcall parser stream) (funcall parser stream)))
        ((failurep result)
         (with-failure result ()
           (just value))))))

(defun parse-do (parser &optional value)
  "Enhance the parser to keep running until it reaches failure. Returns the
   faliure only if the containing parser consums input during failure."
  (lambda (stream)
    (do ((result (funcall parser stream) (funcall parser stream)))
        ((failurep result)
         (with-failure result () (just value))))))

(defun parse-take (times parser)
  "Enhance the parser to run a given number of times and collect the results.
   Consumes input on error if at least one parse succeeded, or the containing
   parser consumed input on error."
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

(defun parse-any (&rest parsers)
  "Compose multiple parsers to try each one in sequence until one either
   succeeds, or has consumed input. Consumes input on failure when any child
   parser has the same error.

   Fails with a list of expected values if all parsers are exhausted.
   Fails when a failing parser consumes input."
  (lambda (stream)
    (do ((parsers-left parsers (rest parsers-left)))
        ((null parsers-left)
         (expected parsers stream nil))
        (let ((result (funcall (first parsers-left) stream)))
          (when (or (justp result)
                    (failure-consumed-chars result))
            (return result))))))

(defun parse-optional (parser &optional default)
  "Enhance the parser to resume from an error with a default value if it did
   not consume input."
  (lambda (stream)
    (with-failure (funcall parser stream) ()
      (just default))))

(defun parse-try (parser)
  "Enhance the parser to try to rewind the stream on failure, reversing any
   input consumption. Only works when parsing seekable streams."
  (lambda (stream)
    (let ((old-position (file-position stream))
          (result (funcall parser stream)))
      (if (and (failurep result)
               (failure-consumed-chars result))
          (progn
            (file-position stream old-position)
            (make-instance 'failure
                           :error (failure-error result)
                           :consumed-chars nil))
          result))))

(defun parse-name (name parser)
  "Maps parser-expected-element errors to provide the given name of the
   element."
  (lambda (stream)
    (let ((result (funcall parser stream)))
      (etypecase result
        (just result)
        (failure (let ((err (failure-error result)))
                   (if (typep err 'parser-expected-element)
                       (progn (setf (slot-value err 'element-name)
                                    name)
                              result)
                       result)))))))

(defmacro parse-let (bindings &body body)
  "Composes multiple parsers together to bind their results to variables and
   return a value within the body. Consumes input on error when the first parse
   succeeds."
  (flet ((bind-just (stream binding result-form)
           (destructuring-bind (var form) binding
             `(with-just (funcall ,form ,stream) (,var) ,result-form))))
    (with-gensyms (stream)
      `(lambda (,stream)
         ,(reduce (curry #'bind-just stream) bindings
                  :initial-value `(just (progn ,@body))
                  :from-end t)))))

(defmacro parse-defer (form)
  "Defer evaluating the parser-returning FORM until the parser is called.
   Useful for circular-referencing parsers."
  (with-gensyms (stream)
    `(lambda (,stream) (funcall ,form ,stream))))
