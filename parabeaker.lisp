(defpackage #:parabeaker
  (:use #:cl)
  (:export))

(in-package #:parabeaker)



(defun parse (parser source)
  "Run the given parser."
  (declare (ignore parser source))
  (TODO))



(defmacro TODO ())

(defun accept-string (string)
  "Return a parser that accepts the given string value."
  (declare (ignore string))
  (TODO))

(defun accept-char (char)
  "Return a parser that accepts the given character value."
  (declare (ignore char))
  (TODO))

(defmacro parser-let (bindings &body body)
  "Return a parser that binds a new variable to a parser result in each binding, then returns the body."
  (declare (ignore bindings body))
  (TODO))

(defun any (&rest parsers)
  "Return a parser that attempts each parser while no input is consumed, until
   one parser succeeds."
  (declare (ignore parsers))
  (TODO))

(defun many (&rest parsers)
  "Return a parser that keeps parsing the given parser until failure."
  (declare (ignore parsers))
  (TODO))

(defun many1 (&rest parsers)
  "Return a parser that keeps parsing the given parser until failure, at least once."
  (declare (ignore parsers))
  (TODO))

(defun try (parser)
  (declare (ignore parser))
  (TODO))

(defun parser-name (name parser)
  "Wraps a name around a parser, so that errors are given a keyword what to expect."
  (declare (ignore name parser))
  (TODO))
