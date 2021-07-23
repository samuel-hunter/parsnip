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
   (stream :initarg :stream))
  (:report (lambda (condition stream)
             (with-slots (name position) condition
               (format stream "Expected element ~S (position ~D)"
                       name position)))))

(defclass result ()
  ())

(defclass just (result)
  ((value :initarg :value :reader just-value)
   (position :initarg :position :reader just-position)))

(defclass failure (result)
  ((error :initarg :error :reader failure-error)
   (consumed-chars :initarg :consumed-chars
                   :reader failure-consumed-chars)))

(defmethod print-object ((object just) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (value position) object
      (format stream "~S POS=~D" value position))))

(defmethod print-object ((object failure) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (error position) object
      (format stream "~S POS=~D" error position))))

(defun just (value position)
  (make-instance 'just
                 :value value
                 :position position))

(defun failure (consumed-chars condition-class &rest initargs)
  (make-instance 'failure
                 :error (apply #'make-condition condition-class initargs)
                 :consumed-chars consumed-chars))

(defun expected (name stream consumed-chars)
  (failure consumed-chars 'parser-expected-element
           :name name
           :stream stream))

(defun eof (stream consumed-chars)
  (failure consumed-chars 'end-of-file
           :stream stream))

(defun parse (parser stream)
  (let ((result (funcall parser stream)))
    (etypecase result
      (just (just-value result))
      (failure (error (failure-error result))))))



;; Higher-order parsing functions

(defun just-flatmap (result function)
  (etypecase result
    (just (funcall function (just-value result)))
    (failure result)))

(defun just-map (result function)
  (just-flatmap result
                (lambda (value)
                  (just (funcall function value)
                        (just-position result)))))

(defun failure-resume (result function)
  (etypecase result
    (just result)
    (failure (funcall function (failure-error result)))))

(defun failure-map (result function)
  (failure-resume
    result
    (lambda (err)
      (make-instance 'failure
                     :error (funcall function err)
                     :consumed-chars (failure-consumed-chars result)))))



;; Parsers

(defun char-parser (char)
  "Return a parser that accepts the given character value."
  (lambda (stream)
    (let ((actual-char (peek-char nil stream nil)))
      (cond
        ((null actual-char) (eof stream nil))
        ((char= char actual-char)
         (just (read-char stream) (ignore-errors (file-position stream))))
        (t (expected char stream nil))))))

(defun predicate-parser (predicate)
  "Return a parser that accepts a character if the given predicate returns true."
  (lambda (stream)
    (let ((actual-char (peek-char nil stream nil)))
      (cond
        ((null actual-char) (eof stream nil))
        ((funcall predicate actual-char)
         (just (read-char stream) (ignore-errors (file-position stream))))
        (t (expected predicate stream nil))))))

(defun string-parser (string)
  "Return a parser that accepts the given string value."
  (lambda (stream)
    (let* ((actual-string (make-string (length string)))
           (chars-read (read-sequence actual-string stream)))
      (cond
        ((< chars-read (length string))
         (eof stream (plusp chars-read)))
        ((string= actual-string string)
         (just actual-string (ignore-errors (file-position stream))))
        (t (expected string stream t))))))



;; Combinators

(defun parse-flatmap (parser function)
  "If the parser returns a value, apply the parser-bearing function to it and run the transformed parser"
  (lambda (stream)
    (just-flatmap (funcall parser stream)
                  (compose
                    (rcurry #'funcall stream)
                    function))))

(defun parse-map (parser function)
  "If the parser returns a value, apply the function to it and return the result."
  (lambda (stream)
    (just-map (funcall parser stream)
              function)))

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
              curr
              (lambda (first-result)
                (parse-map rest (constantly first-result)))))
          (cons first-parser parsers)
          :from-end t))

(defun parse-prog2 (first-parser second-parser &rest parsers)
  "Return a parser that applies each parser in order and if successful, return the value of the second."
  (parse-progn first-parser (apply 'parse-prog1 second-parser parsers)))

(defun parse-any (&rest parsers)
  "Return a parser that tries each parser in order (as long as no characters
   are consumed) and returns the first success.

  Return a failure with a list of expected values if all parsers are exhausted.
  Return a failure if a failing parser consumed characters from the stream."
  (declare (optimize debug))
  (reduce (lambda (outer-parser inner-parser)
            (lambda (stream)
              (let ((result (funcall outer-parser stream)))
                (failure-resume result
                                (lambda (err)
                                  (declare (ignore err))
                                  (if (failure-consumed-chars result)
                                      result
                                      (funcall inner-parser stream)))))))
          parsers
          :from-end t
          :initial-value (lambda (stream)
                           (expected parsers stream nil))))

;; TODO figure out how to detect if characters have been consumed without file-position
(defun parse-many (parser)
  "Return a parser that collects a list of successful values until failure.
   If there are no values, return an empty list."
  (lambda (stream)
    (loop
      :with position := (ignore-errors (file-position stream))
      :for result := (funcall parser stream)
      :while (typep result 'just)
      :collect (just-value result) :into values
      :finally (return (if (failure-consumed-chars result)
                           result
                           (just values position))))))

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
    (failure-resume (funcall parser stream)
                     (lambda (err)
                       (declare (ignore err))
                       (just default (ignore-errors (file-position stream)))))))

(defun parse-try (parser)
  "Return a parser that attempts to rewind the stream on failure. Only works when parsing seekable streams."
  (lambda (stream)
    (let ((old-position (file-position stream)))
      (failure-resume (funcall parser stream)
                      (lambda (failure)
                        (when (failure-consumed-chars failure)
                          (file-position stream old-position))
                        failure)))))

(defun parse-name (name parser)
  "Transform the parser to signal the name of the expected element if not found."
  (lambda (stream)
    (failure-map (funcall parser stream)
                 (lambda (err)
                   (if (typep err 'parser-expected-element)
                       (make-condition 'parser-expected-element
                                       :name name
                                       :stream (slot-value err 'stream))
                       err)))))

(defmacro parse-let (bindings &body body)
  "Return a parser that binds a new variable to a parser result in each
   binding, then returns the body."
  (flet ((just-bind (stream binding body)
           (destructuring-bind (var form) binding
             `(just-flatmap (funcall ,form ,stream)
                              (lambda (,var) ,body)))))
    (with-gensyms (stream)
      `(lambda (,stream)
         ,(reduce (curry #'just-bind stream) bindings
                  :initial-value `(just (progn ,@body) ,stream)
                  :from-end t)))))

(defmacro parse-defer (form &key (thread-safe t))
  "Defer evaluating the parser-returning FORM until the parser is called.
   Useful for circular-referencing parsers."
  (with-gensyms (stream parser)
    `(let ((,parser (trivial-lazy:delay ,form :thread-safe ,thread-safe)))
       (lambda (,stream)
         (funcall (trivial-lazy:force ,parser) ,stream)))))
