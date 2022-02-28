;;; profile.lisp - profiling harness
;;; Usage: sbcl --load profile.lisp
;;;
;;; Copyright 2021 Samuel Hunter <samuel (at) shunter (dot) xyz>.
;;; BSD 3-Clause License. See LICENSE for details.

(require :sb-sprof)
(require :asdf)
(require :parsnip/examples)

(defpackage #:xyz.shunter.parsnip.profile
  (:use #:cl #:xyz.shunter.parsnip.examples.json))

(in-package #:xyz.shunter.parsnip.profile)



(defparameter +large-json-path+
  (merge-pathnames #P"stats/large.json"
                   (asdf:system-source-directory :parsnip)))

(defun process-large-file ()
  (with-open-file (stream +large-json-path+)
    (decode-json stream))
  (values))

(sb-sprof:with-profiling (:max-samples 1000 :report :flat :loop t)
  (process-large-file))
