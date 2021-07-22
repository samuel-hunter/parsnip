(defpackage #:parabeaker
  (:use #:cl)
  (:import-from #:alexandria
                #:with-gensyms
                #:curry)
  (:export #:parse
           #:parser-expected-element

           #:accept-char
           #:accept-char-if
           #:accept-string

           #:parser-progn
           #:parser-prog1
           #:parser-prog2

           #:parser-any
           #:parser-many
           #:parser-many1

           #:parser-name

           #:parser-let
           #:parser-defer))

(in-package #:parabeaker)



(defmacro TODO ()
  `(error "Not implemented"))

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
      (expected (error 'parser-expected-element
                       :name (expected-name result)
                       :position (result-position result)
                       :stream stream))
      (just (just-value result)))))

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

(defun parser-progn (first-parser &rest parsers)
  "Compose all parsers in order and return the value of the last."
  (reduce (lambda (curr rest)
            (lambda (stream)
              (flatmap-result
                (funcall curr stream)
                (lambda (result)
                  (declare (ignore result))
                  (funcall rest stream)))))
          (cons first-parser parsers)
          :from-end t))

(defun parser-prog1 (first-parser &rest parsers)
  "Compose all parsers in order and return the value of the first."
  (let ((rest-parsers (apply 'parser-progn (cons first-parser parsers))))
    (lambda (stream)
      (flatmap-result
        (funcall first-parser stream)
        (lambda (first-result)
          (progn rest-parsers
                 (constantly (just first-result stream))))))))

(defun parser-prog2 (first-parser second-parser &rest parsers)
  "Compose all parsers in order and return the value of the second."
  (parser-progn first-parser (apply 'parser-prog1 second-parser parsers)))

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
