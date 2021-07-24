;;; benchmark.lisp - Benchmarking harness

;;; Copyright 2021 Samuel Hunter <samuel (at) shunter (dot) xyz>
;;; BSD-3-Clause

(require :cl-json)
(require :parsnip/examples)

(defpackage #:xyz.shunter.parsnip.benchmark
  (:use #:cl #:xyz.shunter.parsnip.examples.json)
  (:export #:benchmark))

(in-package #:xyz.shunter.parsnip.benchmark)



(defparameter +large-json-path+
  (merge-pathnames #P"benchmark/large.json"
                   (asdf:system-source-directory :parsnip)))

(defun benchmark-decoder (decoder)
  "Measure the time a decoder reads a jarge json payload many times."
  (with-open-file (stream +large-json-path+)
    (time
      (dotimes (n 100)
        (funcall decoder stream)
        (file-position stream 0)))))

(defun benchmark ()
  (format t "PARSNIP:~%")
  (benchmark-decoder #'read-json)

  (format t "~%~%~%CL-JSON:~%")
  (benchmark-decoder #'cl-json:decode-json))
