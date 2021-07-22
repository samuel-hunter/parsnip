;;; parsnip.lisp - Parsnip library implementation

;;; Copyright 2021 Samuel Hunter <samuel (at) shunter (dot) xyz>
;;; BSD-3-Clause

(defpackage #:xyz.shunter.parsnip
  (:nicknames #:parsnip)
  (:use #:cl)
  (:import-from #:alexandria
                #:with-gensyms
                #:curry
                #:rcurry
                #:compose)
  (:export #:parse
           #:parser-expected-element

           #:char-parser
           #:predicate-parser
           #:string-parser

           #:parse-flatmap
           #:parse-map

           #:parse-progn
           #:parse-prog1
           #:parse-prog2

           #:parse-any
           #:parse-many
           #:parse-many1
           #:parse-optional

           #:parse-try
           #:parse-name

           #:parse-let
           #:parse-defer))

(in-package #:xyz.shunter.parsnip)



(define-condition parser-expected-element ()
  ((name :initarg :name)
   (stream :initarg :stream)
   (position :initarg :position))
  (:report (lambda (condition stream)
             (with-slots (name position) condition
               (format stream "Expected element ~S (position ~D)"
                       name position)))))

(defclass result ()
  ((position :initarg :position :reader result-position)))

(defclass just (result)
  ((value :initarg :value :reader just-value)))

(defclass expected (result)
  ((name :initarg :name :reader expected-name)))

(defclass eof (result) ())

(defmethod print-object ((object just) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (value position) object
      (format stream "~S POS=~D" value position))))

(defmethod print-object ((object expected) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (name position) object
      (format stream "~S POS=~D" name position))))

(defmethod print-object ((object eof) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (position) object
      (format stream "POS=~D" position))))

(defun just (value stream)
  (make-instance 'just
                 :value value
                 :position (ignore-errors (file-position stream))))

(defun expected (name stream)
  (make-instance 'expected
                 :name name
                 :position (ignore-errors (file-position stream))))

(defun eof (stream)
  (make-instance 'eof :position (ignore-errors (file-position stream))))

(defun parse (parser stream)
  "Run the given parser."
  (let ((result (funcall parser stream)))
    (etypecase result
      (eof (error 'end-of-file :stream stream))
      (expected (error 'parser-expected-element
                       :name (expected-name result)
                       :position (result-position result)
                       :stream stream))
      (just (just-value result)))))



;; Higher-order parsing functions

(defun result-flatmap (result function)
  (if (typep result 'just)
      (funcall function (just-value result))
      result))

(defun result-errormap (result function)
  (if (typep result 'just)
      result
      (funcall function result)))

(defun result-expectmap (result function)
  (if (typep result 'expected)
      (funcall function result)
      result))



;; Parsers

(defun char-parser (char)
  "Return a parser that accepts the given character value."
  (lambda (stream)
    (let ((actual-char (peek-char nil stream nil)))
      (cond
        ((null actual-char) (eof stream))
        ((char= char actual-char)
         (just (read-char stream) stream))
        (t (expected char stream))))))

(defun predicate-parser (predicate)
  "Return a parser that accepts a character if the given predicate returns true."
  (lambda (stream)
    (let ((actual-char (peek-char nil stream nil)))
      (cond
        ((null actual-char) (eof stream))
        ((funcall predicate actual-char)
         (just (read-char stream) stream))
        (t (expected predicate stream))))))

(defun string-parser (string)
  "Return a parser that accepts the given string value."
  (lambda (stream)
    (let ((actual-string (make-string (length string))))
      (cond
        ((< (read-sequence actual-string stream)
            (length string))
         (eof stream))
        ((string= actual-string string)
         (just actual-string stream))
        (t (expected string stream))))))



;; Combinators

(defun parse-flatmap (parser function)
  "If the parser returns a value, apply the parser-bearing function to it and run the transformed parser"
  (lambda (stream)
    (result-flatmap (funcall parser stream)
                    (compose
                      (rcurry #'funcall stream)
                      function))))

(defun parse-map (parser function)
  "If the parser returns a value, apply the function to it and return the result."
  (lambda (stream)
    (result-flatmap (funcall parser stream)
                    (compose
                      (rcurry 'just stream)
                      function))))

(defun parse-progn (&rest parsers)
  "Return a parser that applies each parser in order and if successful, return the value of the last."
  (reduce (lambda (curr rest)
            (parse-flatmap curr (constantly rest)))
          parsers
          :from-end t))

(defun parse-prog1 (first-parser &rest parsers)
  "Return a parser that applies each parser in order and if successful, return the value of the first."
  (reduce (lambda (curr rest)
            (parse-flatmap
              curr (lambda (first-result)
                     (parse-map rest (constantly first-result)))))
          (cons first-parser parsers)
          :from-end t))

(defun parse-prog2 (first-parser second-parser &rest parsers)
  "Return a parser that applies each parser in order and if successful, return the value of the second."
  (parse-progn first-parser (apply 'parse-prog1 second-parser parsers)))

;; TODO figure out how to detect if characters have been consumed without file-position
(defun parse-any (&rest parsers)
  "Return a parser that applies each parser in order until success and return the value.

   Return a failure if all parsers are exhausted with a list of all expected values.
   Return a failure if one parser consumed from the stream and failed.
   Return a failure if any parser hit end-of-file."
  (lambda (stream)
    (loop
      :with position := (file-position stream)
      :for parser :in parsers
      :for result := (funcall parser stream)
      :when (typep result '(or just eof))
      :return result
      :collect (expected-name result) :into expecteds
      :until (> (file-position stream) position)
      :finally (return (expected expecteds stream)))))

;; TODO figure out how to detect if characters have been consumed without file-position
(defun parse-many (parser)
  "Return a parser that collects a list of successful values until failure.
   If there are no values, return an empty list."
  (lambda (stream)
    (loop
      :for position := (file-position stream)
      :for result := (funcall parser stream)
      :while (typep result 'just)
      :collect (just-value result) :into values
      :finally (return (if (and (typep result '(not just))
                                (> (file-position stream) position))
                           result
                           (just values stream))))))

(defun parse-many1 (parser)
  "Return a parser that collects a list of successful values until failure.
   If there are no values, signal a failure."
  (parse-flatmap parser
                 (lambda (first-result)
                   (parse-map (parse-many parser)
                              (curry #'cons first-result)))))

(defun parse-optional (parser &optional default)
  "If the given parser returns an error, give a default value instaed."
  (lambda (stream)
    (result-errormap (funcall parser stream)
                     (lambda (err)
                       (declare (ignore err))
                       (just default stream)))))

(defun parse-try (parser)
  "Return a parser that attempts to rewind the stream on failure. Only works when parsing seekable streams."
  (lambda (stream)
    (let ((position (file-position stream)))
      (result-errormap (funcall parser stream)
                       (lambda (err)
                         (file-position stream position)
                         err)))))

(defun parse-name (name parser)
  "Transform the parser to signal the name of the expected element if not found."
  (lambda (stream)
    (result-expectmap (funcall parser stream)
                      (lambda (err)
                        (declare (ignore err))
                        (expected name stream)))))

(defmacro parse-let (bindings &body body)
  "Return a parser that binds a new variable to a parser result in each
   binding, then returns the body."
  (flet ((result-bind (stream binding body)
           (destructuring-bind (var form) binding
             `(result-flatmap (funcall ,form ,stream)
                              (lambda (,var) ,body)))))
    (with-gensyms (stream)
      `(lambda (,stream)
         ,(reduce (curry #'result-bind stream) bindings
                  :initial-value `(just (progn ,@body) ,stream)
                  :from-end t)))))

(defmacro parse-defer (form &key (thread-safe t))
  "Defer evaluating the parser-returning FORM until the parser is called.
   Useful for circular-referencing parsers."
  (with-gensyms (stream parser)
    `(let ((,parser (trivial-lazy:delay ,form :thread-safe ,thread-safe)))
       (lambda (,stream)
         (funcall (trivial-lazy:force ,parser) ,stream)))))
