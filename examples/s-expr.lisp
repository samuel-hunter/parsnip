;;; s-expr.lisp - Parsnip example symbolic expression reader

;;; Copyright 2021 Samuel Hunter <samuel (at) shunter (dot) xyz>
;;; BSD-3-Clause

(defpackage #:xyz.shunter.parsnip.examples.s-expr
  (:use #:cl #:parsnip)
  (:export #:read-s-expr
           #:read-s-expr-from-string))

(in-package #:xyz.shunter.parsnip.examples.s-expr)



(defun read-s-expr (&optional (stream *standard-input*))
  (declare (ignore stream))
  (error "TODO: The symbolic expression reader is not implemented yet"))

(defun read-s-expr-from-string (string)
  (with-input-from-string (stream string)
    (read-s-expr stream)))
