;;; coverage.lisp - generate coverage statistics
;;; Usage: sbcl --load coverage.lisp
;;;
;;; Copyright (c) 2021 Samuel Hunter <samuel (at) shunter (dot) xyz>.
;;; BSD 3-Clause License. See LICENSE for details.

(require :sb-cover)
(require :asdf)

(defpackage #:xyz.shunter.parsnip.coverage
  (:use #:cl)
  (:export #:report))

(in-package #:xyz.shunter.parsnip.coverage)



(defun report (directory)
  (declaim (optimize sb-cover:store-coverage-data))
  (asdf:oos 'asdf:load-op :parsnip :force t)
  (asdf:oos 'asdf:load-op :parsnip :force t)
  (asdf:test-system :parsnip)
  (prog1
    (sb-cover:report directory )
    (declaim (optimize (sb-cover:store-coverage-data 0)))))

(report #P"/tmp/parsnip-coverage/")
