(defpackage #:parabeaker
  (:use #:cl)
  (:export #:parse

           #:accept-string
           #:accept-char
           #:accept-char-if
           #:accept-charbag
           #:parser-let
           #:parser-any
           #:parser-many
           #:parser-many1
           #:parser-or
           #:parser-name))

(in-package #:parabeaker)



(defmacro TODO ()
  `(error "Not implemented"))

(defun parse (parser stream)
  "Run the given parser."
  (declare (ignore parser stream))
  (TODO))

(defclass result ()
  ((value :initarg :value :reader result-value)
   (position :initarg :position :reader result-position)))

(defmethod print-object ((object result) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~S POS=~D"
            (result-value object)
            (result-position object))))

(defclass just (result) ())
(defclass expected (result) ())

(defun just (value stream)
  (make-instance 'just
                 :value value
                 :position (file-position stream)))

(defun expected (value stream)
  (make-instance 'expected
                 :value value
                 :position (file-position stream)))

(defun flatmap-result (result function)
  (etypecase result
    (just (funcall function result))
    (expected result)))

(defun expectmap-result (result function)
  (etypecase result
    (just result)
    (expected (funcall result function))))



(defun accept-string (string)
  "Return a parser that accepts the given string value."
  (lambda (stream)
    (let ((actual-string (make-string (length string))))
      ;; TODO deal with EOF here
      (assert (= (length string)
                 (read-sequence actual-string stream)))
      (if (string= string actual-string)
          (just actual-string stream)
          (expected string stream)))))

(defun accept-char (char)
  "Return a parser that accepts the given character value."
  (lambda (stream)
    ;; TODO deal with EOF here
    (let ((actual-char (peek-char nil stream)))
      (if (char= char actual-char)
          (prog1 (just actual-char stream)
            (read-char stream))
          (expected char stream)))))

(defun accept-char-if (predicate)
  "Return a parser that accepts a character if the given predicate returns true."
  (lambda (stream)
    (let ((actual-char (peek-char nil stream)))
      (if (funcall predicate actual-char)
          (prog1 (just actual-char stream)
            (read-char stream))
          (expected predicate stream)))))

(defmacro parser-let (bindings &body body)
  "Return a parser that binds a new variable to a parser result in each
   binding, then returns the body."
  (declare (ignore bindings body))
  (TODO))

(defun parser-any (&rest parsers)
  "Return a parser that attempts each parser while no input is consumed, until
   one parser succeeds."
  (declare (ignore parsers))
  (TODO))

(defun parser-many (&rest parsers)
  "Return a parser that keeps parsing the given parser until failure."
  (declare (ignore parsers))
  (TODO))

(defun parser-many1 (&rest parsers)
  "Return a parser that keeps parsing the given parser until failure, at least once."
  (declare (ignore parsers))
  (TODO))

(defun parser-try (parser)
  (declare (ignore parser))
  (TODO))

(defun parser-name (name parser)
  "Wraps a name around a parser, so that errors are given a keyword what to expect."
  (declare (ignore name parser))
  (TODO))

(defun accept-charbag (char-bag)
  "Return a parser that accepts any character within the given char bag."
  (parser-name char-bag (accept-char-if (lambda (c) (position c char-bag)))))
