;;; benchmark.lisp - Benchmarking harness

;;; Copyright 2021 Samuel Hunter <samuel (at) shunter (dot) xyz>
;;; BSD-3-Clause

(require :alexandria)
(require :cl-json)
(require :parsnip/examples)

(defpackage #:xyz.shunter.parsnip.benchmark
  (:use #:cl #:xyz.shunter.parsnip.examples.json)
  (:export #:benchmark))

(in-package #:xyz.shunter.parsnip.benchmark)



(defparameter +large-json-path+
  (merge-pathnames #P"benchmark/large.json"
                   (asdf:system-source-directory :parsnip)))

(defun benchmark-stream-decoder (decoder)
  "Measure the time a decoder reads a jarge json stream payload many times."
  (with-open-file (stream +large-json-path+)
    (time
      (dotimes (n 100)
        (funcall decoder stream)
        (file-position stream 0)))))

(defun benchmark-string-decoder (decoder)
  "Measure the time a decoder reads a large json string payload many times."
  (let ((payload (alexandria:read-file-into-string +large-json-path+)))
    (time
      (dotimes (n 100) (funcall decoder payload)))))

(defun benchmark ()
  (format t "===READING FROM STREAM~%")
  (format t "PARSNIP:~%")
  (benchmark-stream-decoder #'read-json)

  (format t "~%~%~%CL-JSON:~%")
  (benchmark-stream-decoder #'cl-json:decode-json)

  (format t "~%~%~%===READING FROM STRING===~%")
  (format t "PARSNIP:~%")
  (benchmark-string-decoder #'read-json-from-string)

  (format t "~%~%~%Cl-JSON:~%")
  (benchmark-string-decoder #'cl-json:decode-json-from-string))

(benchmark)
