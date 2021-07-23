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
           #:parse-take
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
  "Map a return value to a different result."
  (etypecase result
    (just (funcall function (just-value result)))
    (failure result)))

(defun just-map (result function)
  "Map a return value to a different one."
  (just-flatmap result
                (lambda (value)
                  (just (funcall function value)
                        (just-position result)))))

(defun failure-resume (result function)
  "Map a failure to a different result if it did not consume chars."
  (etypecase result
    (just result)
    (failure (if (failure-consumed-chars result)
                 result
                 (funcall function (failure-error result))))))

(defun failure-map (result function)
  "Map an error to a diffferent one."
  (etypecase result
    (just result)
    (failure (make-instance
               'failure
               :error (funcall function (failure-error result))
               :consumed-chars (failure-consumed-chars result)))))



;; Parsers

(defun char-parser (char)
  "Return a parser that accepts the given character value. Does not consume
   input on failure."
  (lambda (stream)
    (let ((actual-char (peek-char nil stream nil)))
      (cond
        ((null actual-char) (eof stream nil))
        ((char= char actual-char)
         (just (read-char stream) (ignore-errors (file-position stream))))
        (t (expected char stream nil))))))

(defun predicate-parser (predicate)
  "Return a parser that accepts a character if the given predicate returns
   true. Does not consume input on failure."
  (lambda (stream)
    (let ((actual-char (peek-char nil stream nil)))
      (cond
        ((null actual-char) (eof stream nil))
        ((funcall predicate actual-char)
         (just (read-char stream) (ignore-errors (file-position stream))))
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
         (just actual-string (ignore-errors (file-position stream))))
        (t (expected string stream t))))))



;; Combinators

(defun parse-flatmap (parser function)
  "Enhance the parser to map any return value to a different parser and run it."
  (lambda (stream)
    (just-flatmap (funcall parser stream)
                  (compose
                    (rcurry #'funcall stream)
                    function))))

(defun parse-map (parser function)
  "Enhance the parser to map any return value to a different one."
  (lambda (stream)
    (just-map (funcall parser stream)
              function)))

(defun parse-progn (&rest parsers)
  "Compose multiple parsers to run them in sequence, returning the last
   parser's value. Consumes input on failure when the first parser succeeds."
  (reduce (lambda (curr rest)
            (parse-flatmap curr (constantly rest)))
          parsers
          :from-end t))

(defun parse-prog1 (first-parser &rest parsers)
  "Compose multiple parsers to run them in sequence, returning the first
   parser's value. Consumes input on failure when the first parser succeeds."
  (reduce (lambda (curr rest)
            (parse-flatmap
              curr
              (lambda (first-result)
                (parse-map rest (constantly first-result)))))
          (cons first-parser parsers)
          :from-end t))

(defun parse-prog2 (first-parser second-parser &rest parsers)
  "Compose multple parsers to run them in sequence, returning the second
   parser's value. Consumes input on failure when the first parser succeeds."
  (parse-progn first-parser (apply 'parse-prog1 second-parser parsers)))

(defun parse-any (&rest parsers)
  "Compose multiple parsers to try each one in sequence until one either
   succeeds, or has consumed input. Consumes input on failure when any child
   parser has the same error.

  Fails with a list of expected values if all parsers are exhausted.
  Fails when a failing parser consumes input."
  (declare (optimize debug))
  (reduce (lambda (outer-parser inner-parser)
            (lambda (stream)
              (failure-resume (funcall outer-parser stream)
                              (lambda (err)
                                (declare (ignore err))
                                (funcall inner-parser stream)))))
          parsers
          :from-end t
          :initial-value (lambda (stream)
                           (expected parsers stream nil))))

;; TODO figure out how to detect if characters have been consumed without file-position
(defun parse-many (parser)
  "Enhance the parser to run indefinitively until error, and collect the
   results. Consumes input on error when the last (errorful) parse consumes
   input.

   Fails when the containing parser consumes input on failure."
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
  "Enhance the parser to run indefinitively at least once until error, and
   collect the results. Consumes input on error when the last (errorful) parse
   consumes input.

   Fails when it could not succeed once.
   Fails when the containing parser consumes input during failure."
  (parse-flatmap parser
                 (lambda (first-result)
                   (parse-map (parse-many parser)
                              (curry #'cons first-result)))))

(defun parse-take (times parser)
  "Enhance the parser to run a given number of times and collect the results.
   Consumes input on error if at least one parse succeeded, or the containing
   parser consumed input on error."
  (check-type times (integer 1 *))
  (if (= times 1)
      (parse-map parser
                 #'list)
      (parse-flatmap parser
                     (lambda (first-result)
                       (parse-map (parse-take (1- times) parser)
                                  (curry #'cons first-result))))))

(defun parse-optional (parser &optional default)
  "Enhance the parser to resume from an error with a default value if it did
   not consume input."
  (lambda (stream)
    (failure-resume (funcall parser stream)
                    (lambda (err)
                      (declare (ignore err))
                      (just default (ignore-errors (file-position stream)))))))

(defun parse-try (parser)
  "Enhance the parser to try to rewind the stream on failure, reversing any
   input consumption. Only works when parsing seekable streams."
  (lambda (stream)
    (let ((old-position (file-position stream))
          (result (funcall parser stream)))
      (failure-resume result
                      (lambda (err)
                        (when (failure-consumed-chars result)
                          (file-position stream old-position))
                        (make-instance 'failure
                                       :error err
                                       :consumed-chars nil))))))

(defun parse-name (name parser)
  "Maps parser-expected-element errors to provide the given name of the
   element."
  (lambda (stream)
    (failure-map (funcall parser stream)
                 (lambda (err)
                   (if (typep err 'parser-expected-element)
                       (make-condition 'parser-expected-element
                                       :name name
                                       :stream (slot-value err 'stream))
                       err)))))

(defmacro parse-let (bindings &body body)
  "Composes multiple parsers together to bind their results to variables and
   return a value within the body. Consumes input on error when the first parse
   succeeds."
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
