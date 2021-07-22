;;; s-exp.lisp - Parsnip example symbolic expression reader

;;; Copyright 2021 Samuel Hunter <samuel (at) shunter (dot) xyz>
;;; BSD-3-Clause

(defpackage #:xyz.shunter.parsnip.examples.s-exp
  (:nicknames #:parsnip.examples.s-exp)
  (:use #:cl #:parsnip)
  (:export #:read-symbolic-expression
           #:read-symbolic-expression-from-string))

(in-package #:xyz.shunter.parsnip.examples.s-exp)



(defun read-symbolic-expression (&optional (stream *standard-input*))
  (declare (ignore stream))
  (error "TODO: The symbolic expression reader is not implemented yet"))

(defun read-symbolic-expression-from-string (string)
  (with-input-from-string (stream string)
    (read-symbolic-expression stream)))
