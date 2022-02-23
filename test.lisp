;;; test.lisp - Parsnip library test suite

;;; Copyright 2021 Samuel Hunter <samuel (at) shunter (dot) xyz>
;;; BSD-3-Clause

(defpackage #:xyz.shunter.parsnip.test
  (:use #:cl
        #:xyz.shunter.parsnip
        #:xyz.shunter.parsnip.examples.json)
  (:local-nicknames (#:t #:parachute)))

(in-package #:xyz.shunter.parsnip.test)



(defun parse-string (parser string)
  (with-input-from-string (stream string)
    (parse parser stream)))

(defun capture-parse-error (parser string)
  (multiple-value-bind (always-nil err) (ignore-errors (parse-string parser string))
    (declare (ignore always-nil))
    err))

(defun close-enough (float-1 float-2)
  "Return whether two decimal numbers are within tolerance (0.05%)"
  (when (zerop (+ float-1 float-2))
    (return-from close-enough nil))
  (let ((difference (abs (/ (- float-1 float-2) (abs (+ float-1 float-2)) 2)))
        (tolerance 0.0005))
    (< difference tolerance)))



;; Unit Tests

(t:define-test ok
  (t:is eq :hello
        (parse-string (ok :hello) "whatever"))

  (t:is eq :hello
        (parse-string (ok :hello) "")))

(t:define-test fail
  (t:fail (parse-string (fail "terrible!") "whatever"))
  (t:fail (parse-string (fail "terrible!") ""))

  (t:is string= "terrible!"
        (parser-error-expected
          (capture-parse-error (fail "terrible!") "whatever"))))

(t:define-test char-if
  (t:is char= #\a
      (parse-string (char-if #'alpha-char-p) "a"))

  (t:is char= #\z
      (parse-string (char-if #'alpha-char-p) "z"))

  (t:fail (parse-string (char-if #'alpha-char-p) "0"))
  (t:fail (parse-string (char-if #'alpha-char-p) ""))

  (t:is string= "letter"
        (parser-error-expected
          (capture-parse-error (char-if #'alpha-char-p "letter") "0"))))

(t:define-test char-of
  :depends-on (char-if)
  (t:is char= #\a
        (parse-string (char-of #\a) "a"))
  (t:fail (parse-string (char-of #\a) "z"))
  (t:fail (parse-string (char-of #\a) nil))

  (t:is char= #\z
        (parse-string (char-of #\z) "z"))
  (t:fail (parse-string (char-of #\z) "a")))

(t:define-test char-in
  :depends-on (char-if)
  (t:is char= #\a
        (parse-string (char-in "abc") "a"))
  (t:is char= #\b
        (parse-string (char-in "abc") "b"))
  (t:is char= #\c
        (parse-string (char-in "abc") "c"))

  (t:fail (parse-string (char-in "abc") "z"))
  (t:fail (parse-string (char-in "abc") "")))

(t:define-test eof
  (t:is eq nil
        (parse-string (eof) ""))

  (t:is eq :end
        (parse-string (eof :end) ""))

  (t:fail (parse-string (eof) "something")))

(t:define-test flatmap
  :depends-on (char-if)
  (let ((parser (flatmap (lambda (c)
                           (if (char= c #\d)
                               (fail "not d")
                               (ok c)))
                         (char-if #'alpha-char-p))))
    (t:is char= #\a
          (parse-string parser "a"))

    (t:is char= #\z
          (parse-string parser "z"))

    (t:fail (parse-string parser "1"))

    (t:fail (parse-string parser "d"))

    (t:is string= "not d"
          (parser-error-expected
            (capture-parse-error parser "d"))))

  (let ((parser (flatmap (lambda (c)
                           (if (char= c #\Z)
                               (char-if #'digit-char-p)
                               (ok c)))
                         (char-if #'alpha-char-p))))
    (t:is char= #\a
          (parse-string parser "a"))

    (t:is char= #\1
          (parse-string parser "Z1"))

    (t:fail (parse-string parser "Z"))))

(t:define-test let!
  :depends-on (char-if flatmap)
  (let ((parser (let! ((c (char-if #'alpha-char-p)))
                  (ok (char-code c)))))
    (t:is = (char-code #\a)
          (parse-string parser "a"))

    (t:is = (char-code #\b)
          (parse-string parser "b")))

  (let ((parser (let! ((d1 (char-if #'digit-char-p))
                       (d2 (char-if #'digit-char-p))
                       (d3 (char-if #'digit-char-p)))
                  (ok (+ (* 100 (digit-char-p d1))
                         (* 10  (digit-char-p d2))
                         (* 1   (digit-char-p d3)))))))
    (t:is = 123
          (parse-string parser "123"))

    (t:fail (parse-string parser "12"))))

(t:define-test progn!
  :depends-on (char-of)
  (let ((a (progn! (char-of #\a)))
        (ab (progn! (char-of #\a) (char-of #\b)))
        (abc (progn! (char-of #\a) (char-of #\b) (char-of #\c))))


    (t:is char= #\a
          (parse-string a "a"))

    (t:fail (parse-string a "z"))

    (t:is char= #\b
          (parse-string ab "ab"))

    (t:fail (parse-string ab "az"))

    (t:is char= #\c
          (parse-string abc "abc"))

    (t:fail (parse-string abc "abz"))))

(t:define-test handle
  :depends-on (eof char-of progn!)

  (let ((parser (handle (eof :eof) (constantly (ok :not-eof)))))
    (t:is eq :eof
          (parse-string parser ""))

    (t:is eq :not-eof
          (parse-string parser "whatever")))

  (let ((parser (handle (progn! (char-of #\a) (char-of #\b))
                        (constantly (ok :recovered)))))
    (t:is eq #\b
          (parse-string parser "ab"))

    (t:is eq :recovered
          (parse-string parser "cd"))

    (t:fail (parse-string parser "ac"))))

(t:define-test handle-rewind
  :depends-on (progn! let!)
  (let ((parser (handle-rewind (progn! (char-of #\a) (char-of #\b))
                               (constantly (ok :recovered)))))
    (t:is eq :recovered
          (parse-string parser "ac"))))

(t:define-test prog1!
  :depends-on (char-of)
  (let ((a (prog1! (char-of #\a)))
        (ab (prog1! (char-of #\a) (char-of #\b)))
        (abc (prog1! (char-of #\a) (char-of #\b) (char-of #\c))))


    (t:is char= #\a (parse-string a "a"))

    (t:fail (parse-string a "z"))

    (t:is char= #\a
          (parse-string ab "ab"))

    (t:fail (parse-string ab "az"))

    (t:is char= #\a
          (parse-string abc "abc"))

    (t:fail (parse-string abc "abz"))))

(t:define-test prog2!
  :depends-on (char-of)
  (let ((ab (prog2! (char-of #\a) (char-of #\b)))
        (abc (prog2! (char-of #\a) (char-of #\b) (char-of #\c))))

    (t:is char= #\b
          (parse-string ab "ab"))

    (t:fail (parse-string ab "az"))

    (t:is char= #\b
          (parse-string abc "abc"))

    (t:fail (parse-string abc "abz"))))

(t:define-test or!
  :depends-on (char-of ok)
  (let ((a-or-b-or-c (or! (char-of #\a)
                          (char-of #\b)
                          (char-of #\c)))
        (a-or-b-or-default (or! (char-of #\a)
                                (char-of #\b)
                                (ok :default))))
    (t:is char= #\a
          (parse-string a-or-b-or-c "a"))
    (t:is char= #\b
          (parse-string a-or-b-or-c "b"))
    (t:is char= #\c
          (parse-string a-or-b-or-c "c"))

    (t:fail (parse-string a-or-b-or-c "z"))

    (t:is equal '(#\a #\b #\c)
          (parser-error-expected
            (capture-parse-error a-or-b-or-c "z")))

    (t:is char= #\a
          (parse-string a-or-b-or-default "a"))
    (t:is char= #\b
          (parse-string a-or-b-or-default "b"))
    (t:is eq :default
          (parse-string a-or-b-or-default "z"))))

(t:define-test collect
  :depends-on (char-of)
  (let ((as (collect (char-of #\a))))

    (t:is equal '(#\a)
          (parse-string as "a"))
    (t:is equal '(#\a #\a #\a)
          (parse-string as "aaa"))
    (t:is equal '(#\a #\a #\a)
          (parse-string as "aaaz"))

    (t:is equal ()
          (parse-string as "z"))
    (t:is equal ()
          (parse-string as ""))))

(t:define-test collect1
  :depends-on (char-of)
  (let ((as (collect1 (char-of #\a))))

    (t:is equal '(#\a)
          (parse-string as "a"))
    (t:is equal '(#\a #\a #\a)
          (parse-string as "aaa"))
    (t:is equal '(#\a #\a #\a)
          (parse-string as "aaaz"))

    (t:fail (parse-string as "z"))
    (t:fail (parse-string as ""))))

(t:define-test collect-into-string
  :depends-on (char-of)
  (let ((as (collect-into-string (char-of #\a))))

    (t:is string= "a"
          (parse-string as "a"))
    (t:is string= "aaa"
          (parse-string as "aaa"))
    (t:is string= "aaa"
          (parse-string as "aaaz"))

    (t:is string= ""
          (parse-string as "z"))
    (t:is string= ""
          (parse-string as ""))))

(t:define-test sep
  :depends-on (char-of)
  (let ((as (sep (char-of #\a)
                 (char-of #\,))))

    (t:is equal '(#\a)
          (parse-string as "a"))

    (t:is equal '(#\a #\a)
          (parse-string as "a,a"))

    (t:is equal '(#\a #\a #\a)
          (parse-string as "a,a,a"))

    (t:fail (parse-string as ""))

    (t:fail (parse-string as "a,a,a,"))))

(t:define-test reduce!
  :depends-on (char-if flatmap)
  (let ((parser (reduce! (lambda (num dig) (+ (* num 10) dig))
                         (flatmap (lambda (c) (ok (digit-char-p c)))
                                  (char-if #'digit-char-p)))))
    (t:is = 1
          (parse-string parser "1"))

    (t:is = 12
          (parse-string parser "12"))

    (t:is = 123
          (parse-string parser "123"))

    (t:is = 123
          (parse-string parser "123."))

    (t:fail (parse-string parser "something else")))

  (let ((parser (reduce! (lambda (cs c) (list* c cs))
                         (char-if #'alpha-char-p)
                         :initial-value ())))
    (t:is equal '(#\a)
          (parse-string parser "a"))

    (t:is equal '(#\b #\a)
          (parse-string parser "ab"))

    (t:is equal '(#\c #\b #\a)
          (parse-string parser "abc"))

    (t:is equal '(#\c #\b #\a)
          (parse-string parser "abc."))

    (t:is equal ()
          (parse-string parser ""))))



;; JSON Tests

(t:define-test json-numbers
  (t:is = 123
      (decode-json-from-string "123"))

  (t:is = 50
      (decode-json-from-string "000050"))

  (t:is = -123
      (decode-json-from-string "-123"))

  (t:is close-enough 1.5
      (decode-json-from-string "1.5"))

  (t:is close-enough 1.05
      (decode-json-from-string "1.05"))

  (t:is close-enough 1e5
      (decode-json-from-string "1e5"))

  (t:is close-enough 1e5
      (decode-json-from-string "1e+5"))

  (t:is close-enough 1e-5
      (decode-json-from-string "1e-5"))

  (t:is close-enough 15.0
      (decode-json-from-string "1.5e1")))

(t:define-test json-strings
  (t:is string= "hello, world"
      (decode-json-from-string "\"hello, world\""))

  (t:is string= (coerce #(#\" #\\ #\/ #\Backspace #\Page #\Newline #\Return #\Tab) 'string)
      (decode-json-from-string "\"\\\"\\\\\\/\\b\\f\\n\\r\\t\""))

  (t:is string= "(λ (n) (* n n))"
      (decode-json-from-string "\"(\\u03BB (n) (* n n))\"")))

(t:define-test json-arrays
  :depends-on (json-numbers json-strings)
  (t:is equal '(10 20 30)
      (decode-json-from-string "[10,20,30]"))

  (t:is equal '(10)
      (decode-json-from-string "[10]"))

  (t:is equal ()
      (decode-json-from-string "[]"))

  (t:is equal '(10 "string" (20 30 40))
      (decode-json-from-string "[10, \"string\", [20, 30, 40]]"))

  (t:is equal '(10 20 30)
      (decode-json-from-string " [ 10 , 20 , 30 ] ")))

(t:define-test json-objects
  :depends-on (json-numbers json-strings)
  (t:is equal '(("key" . "value"))
      (decode-json-from-string "{\"key\":\"value\"}"))

  (t:is equal '(("one" . 1) ("two" . 2) ("three" . 3))
      (decode-json-from-string "{\"one\":1,\"two\":2,\"three\":3}"))

  (t:is equal '(("object" . (("key" . "value"))))
      (decode-json-from-string "{\"object\":{\"key\":\"value\"}}"))

  (t:is equal '(("key" . "value") ("foo" . "bar"))
      (decode-json-from-string " { \"key\" : \"value\" , \"foo\" : \"bar\" }")))
