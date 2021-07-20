(defpackage #:parabeaker
  (:use #:cl)
  (:import-from #:alexandria
                #:with-gensyms
                #:curry)
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

(define-condition expected-element ()
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
                 :position (file-position stream)))

(defun expected (name stream)
  (make-instance 'expected
                 :name name
                 :position (file-position stream)))

(defun eof (stream)
  (make-instance 'eof :position (file-position stream)))

(defun parse (parser stream)
  "Run the given parser."
  (let ((result (funcall parser stream)))
    (etypecase result
      (eof (error 'end-of-file :stream stream))
      (expected (error 'expected-element
                       :name (expected-name result)
                       :position (result-position result)
                       :stream stream))
      (result (just-value result)))))

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
  (lambda (stream)
    (flatmap-result (funcall (parser-many parser) stream)
                    (lambda (results)
                      (if results
                          (just results stream)
                          (expected parser stream))))))

(defun parser-try (parser)
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
                      (constantly (expected name stream)))))

(defun accept-charbag (char-bag)
  "Return a parser that accepts any character within the given char bag."
  (parser-name char-bag (accept-char-if (lambda (c) (position c char-bag)))))
