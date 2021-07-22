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

           #:map-parser

           #:accept-char
           #:accept-char-if
           #:accept-string

           #:parser-progn
           #:parser-prog1
           #:parser-prog2

           #:parser-any
           #:parser-many
           #:parser-many1
           #:parser-or

           #:parser-try
           #:parser-name

           #:parser-let
           #:parser-defer))

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

(defun flatmap-result (result function)
  (if (typep result 'just)
      (funcall function (just-value result))
      result))

(defun errormap-result (result function)
  (if (typep result 'just)
      result
      (funcall function result)))

(defun expectmap-result (result function)
  (if (typep result 'expected)
      (funcall function result)
      result))

(defun flatmap-parser (parser function)
  (lambda (stream)
    (flatmap-result (funcall parser stream)
                    (compose
                      (rcurry #'funcall stream)
                      function))))

(defun map-parser (parser function)
  (lambda (stream)
    (flatmap-result (funcall parser stream)
                    (compose
                      (rcurry 'just stream)
                      function))))



;; Primitive Parsers

(defun accept-char (char)
  "Return a parser that accepts the given character value."
  (lambda (stream)
    (let ((actual-char (peek-char nil stream nil)))
      (cond
        ((null actual-char) (eof stream))
        ((char= char actual-char)
         (just (read-char stream) stream))
        (t (expected char stream))))))

(defun accept-char-if (predicate)
  "Return a parser that accepts a character if the given predicate returns true."
  (lambda (stream)
    (let ((actual-char (peek-char nil stream nil)))
      (cond
        ((null actual-char) (eof stream))
        ((funcall predicate actual-char)
         (just (read-char stream) stream))
        (t (expected predicate stream))))))

(defun accept-string (string)
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



;; Parser Combinators

(defun parser-progn (&rest parsers)
  "Compose all parsers in order and return the value of the last."
  (reduce (lambda (curr rest)
            (flatmap-parser curr (constantly rest)))
          parsers
          :from-end t))

(defun parser-prog1 (first-parser &rest parsers)
  "Compose all parsers in order and return the value of the first."
  (reduce (lambda (curr rest)
            (flatmap-parser
              curr (lambda (first-result)
                     (map-parser rest (constantly first-result)))))
          (cons first-parser parsers)
          :from-end t))

(defun parser-prog2 (first-parser second-parser &rest parsers)
  "Compose all parsers in order and return the value of the second."
  (parser-progn first-parser (apply 'parser-prog1 second-parser parsers)))

;; TODO figure out how to detect if characters have been consumed without file-position
(defun parser-any (&rest parsers)
  "Return a parser that attempts each parser while no input is consumed, until
   one parser succeeds."
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
(defun parser-many (parser)
  "Return a parser that keeps parsing the given parser until failure."
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

(defun parser-many1 (parser)
  "Return a parser that keeps parsing the given parser until failure, at least once."
  (flatmap-parser parser
                  (lambda (first-result)
                    (map-parser (parser-many parser)
                                (curry #'cons first-result)))))

(defun parser-or (parser &optional default)
  "If the given parser returns an error, give a default value instaed."
  (lambda (stream)
    (errormap-result (funcall parser stream)
                     (lambda (err)
                       (declare (ignore err))
                       (just default stream)))))

(defun parser-try (parser)
  "When the child parser fails, attempt to rewind the stream. Only works for
   seekable streams."
  (lambda (stream)
    (let ((position (file-position stream)))
      (errormap-result (funcall parser stream)
                       (lambda (err)
                         (file-position stream position)
                         err)))))

(defun parser-name (name parser)
  "Wraps a name around a parser, so that errors are given a keyword what to expect."
  (lambda (stream)
    (expectmap-result (funcall parser stream)
                      (lambda (err)
                        (declare (ignore err))
                        (expected name stream)))))

(defmacro parser-let (bindings &body body)
  "Return a parser that binds a new variable to a parser result in each
   binding, then returns the body."
  (flet ((bind-result (stream binding body)
           (destructuring-bind (var form) binding
             `(flatmap-result (funcall ,form ,stream)
                              (lambda (,var) ,body)))))
    (with-gensyms (stream)
      `(lambda (,stream)
         ,(reduce (curry #'bind-result stream) bindings
                  :initial-value `(just (progn ,@body) ,stream)
                  :from-end t)))))

(defmacro parser-defer (form &key (thread-safe t))
  "Defer evaluating the parser-returning FORM until the parser is called.
   Useful for circular-referencing parsers."
  (with-gensyms (stream parser)
    `(let ((,parser (trivial-lazy:delay ,form :thread-safe ,thread-safe)))
       (lambda (,stream)
         (funcall (trivial-lazy:force ,parser) ,stream)))))
