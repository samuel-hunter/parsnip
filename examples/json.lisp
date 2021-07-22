(defpackage #:xyz.shunter.parsnip.examples.json
  (:nicknames #:parsnip.examples.json)
  (:use #:cl #:parsnip)
  (:export #:read-json-object
           #:read-json-object-from-string))

(in-package #:xyz.shunter.parsnip.examples.json)



(defun read-json-object (&optional (stream *standard-input*))
  (error "TODO: The JSON reader is not implemented yet"))

(defun read-json-object-from-string (string)
  (with-input-from-string (stream string)
    (read-json-object stream)))
