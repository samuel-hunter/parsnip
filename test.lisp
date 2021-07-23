;;; test.lisp - Parsnip library test suite

;;; Copyright 2021 Samuel Hunter <samuel (at) shunter (dot) xyz>
;;; BSD-3-Clause

(defpackage #:xyz.shunter.parsnip.test
  (:use #:cl #:parsnip #:parachute))

(in-package #:xyz.shunter.parsnip.test)



(defun parse-string (parser string)
  (with-input-from-string (stream string)
    (parse parser stream)))



;; Unit Tests

(define-test char-parser
  (let ((parser (char-parser #\a)))
    (is char= #\a
        (parse-string parser "abc"))

    (is char= #\a
        (parse-string parser "a"))

    (fail (parse-string parser "z")
          'parser-expected-element)

    (fail (parse-string parser "")
          'end-of-file)))

(define-test predicate-parser
  (let ((parser (predicate-parser #'digit-char-p)))
    (is char= #\0
        (parse-string parser "0"))

    (is char= #\0
        (parse-string parser "012"))

    (is char= #\9
        (parse-string parser "9"))

    (fail (parse-string parser "z")
          'parser-expected-element)

    (fail (parse-string parser "")
          'end-of-file)))

(define-test string-parser
  (let ((parser (string-parser "foo")))
    (is string= "foo"
        (parse-string parser "foo"))

    (is string= "foo"
        (parse-string parser "foobar"))

    (fail (parse-string parser "bar")
          'parser-expected-element)

    (fail (parse-string parser "fo")
          'end-of-file)

    (fail (parse-string parser "")
          'end-of-file)))

(define-test parse-progn
  :depends-on (char-parser)
  (let ((a (parse-progn (char-parser #\a))))
    ;; parse-progn with a single element should act like its child parser
    (is char= #\a
        (parse-string a "a"))

    (fail (parse-string a "z")
          'parser-expected-element))

  (let ((ab (parse-progn (char-parser #\a)
                          (char-parser #\b))))
    (is char= #\b
        (parse-string ab "ab"))

    (fail (parse-string ab "az")
          'parser-expected-element)

    (fail (parse-string ab "b")
          'parser-expected-element))

  (let ((abc (parse-progn (char-parser #\a)
                           (char-parser #\b)
                           (char-parser #\c))))
    (is char= #\c
        (parse-string abc "abc"))

    (fail (parse-string abc "abz")
          'parser-expected-element)

    (fail (parse-string abc "az")
          'parser-expected-element)

    (fail (parse-string abc "bc")
          'parser-expected-element)

    (fail (parse-string abc "c")
          'parser-expected-element)))

(define-test parse-prog1
  :depends-on (char-parser)
  (let ((a (parse-prog1 (char-parser #\a))))
    ;; parse-prog1 with a single element should act like its child parser
    (is char= #\a
        (parse-string a "a"))

    (fail (parse-string a "z")
          'parser-expected-element))

  (let ((ab (parse-prog1 (char-parser #\a)
                          (char-parser #\b))))
    (is char= #\a
        (parse-string ab "ab"))

    (fail (parse-string ab "az")
          'parser-expected-element)

    (fail (parse-string ab "b")
          'parser-expected-element))

  (let ((abc (parse-prog1 (char-parser #\a)
                           (char-parser #\b)
                           (char-parser #\c))))
    (is char= #\a
        (parse-string abc "abc"))

    (fail (parse-string abc "abz")
          'parser-expected-element)

    (fail (parse-string abc "az")
          'parser-expected-element)

    (fail (parse-string abc "bc")
          'parser-expected-element)

    (fail (parse-string abc "c")
          'parser-expected-element)))

(define-test parse-prog2
  :depends-on (char-parser)
  (let ((ab (parse-prog2 (char-parser #\a)
                          (char-parser #\b))))
    (is char= #\b
        (parse-string ab "ab"))

    (fail (parse-string ab "az")
          'parser-expected-element)

    (fail (parse-string ab "z")
          'parser-expected-element))

  (let ((abc (parse-prog2 (char-parser #\a)
                           (char-parser #\b)
                           (char-parser #\c))))
    (is char= #\b
        (parse-string abc "abc"))

    (fail (parse-string abc "abz")
          'parser-expected-element)

    (fail (parse-string abc "az")
          'parser-expected-element)

    (fail (parse-string abc "bc")
          'parser-expected-element)

    (fail (parse-string abc "c")
          'parser-expected-element)))

(define-test parse-any
  :depends-on (char-parser string-parser)
  (let ((a (parse-any (char-parser #\a))))
    (is char= #\a
        (parse-string a "a"))

    (fail (parse-string a "z")
          'parser-expected-element))

  (let ((abc (parse-any (char-parser #\a)
                         (char-parser #\b)
                         (char-parser #\c))))
    (is char= #\a
        (parse-string abc "a"))

    (is char= #\b
        (parse-string abc "b"))

    (is char= #\c
        (parse-string abc "c"))

    (fail (parse-string abc "z")
          'parser-expected-element))

  (let ((foobar (parse-any (string-parser "foo")
                            (string-parser "bar"))))
    (is string= "foo"
        (parse-string foobar "foo"))

    (fail (parse-string foobar "bar")
          'parser-expected-element
          "parse-any fails after more than one character is read.")))

(define-test parse-many
  :depends-on (char-parser)
  (let ((parser (parse-many (char-parser #\a))))
    (is equal '(#\a)
        (parse-string parser "a"))

    (is equal '(#\a #\a #\a)
        (parse-string parser "aaa"))

    (is equal ()
        (parse-string parser "")))

  (let ((foos (parse-many (string-parser "foo"))))
    (is equal '("foo" "foo" "foo")
        (parse-string foos "foofoofoo"))

    (fail (parse-string foos "foofoobar")
          'parser-expected-element
          "parse-many fails when the last (errorful) parse consumes input.")))

(define-test parse-many1
  :depends-on (char-parser)
  (let ((parser (parse-many1 (char-parser #\a))))
    (is equal '(#\a)
        (parse-string parser "a"))

    (is equal '(#\a #\a #\a)
        (parse-string parser "aaa"))

    (fail (parse-string parser "")
          'end-of-file))

  (let ((foos (parse-many (string-parser "foo"))))
    (is equal '("foo" "foo" "foo")
        (parse-string foos "foofoofoo"))

    (fail (parse-string foos "foofoobar")
          'parser-expected-element
          "parse-many1 fails when the last (errorful) parse consumes input.")))

(define-test parse-optional
  :depends-on (char-parser)
  (let ((maybe-a (parse-optional (char-parser #\a))))
    (is char= #\a
        (parse-string maybe-a "a"))

    (is eq nil
        (parse-string maybe-a "z"))

    (is eq nil
        (parse-string maybe-a "")))

  (let ((a-else-b (parse-optional (char-parser #\a)
                                  #\b)))
    (is char= #\a
        (parse-string a-else-b "a"))

    (is char= #\b
        (parse-string a-else-b "b"))

    (is char= #\b
        (parse-string a-else-b "")))

  (let ((foo (parse-optional (string-parser "foo"))))
    (is string= "foo"
        (parse-string foo "foo"))

    (fail (parse-string foo "bar")
           'parser-expected-element
           "parse-optional does not resume from input-consuming errors")))

(define-test parse-let
  :depends-on (predicate-parser)
  (let ((parser (parse-let ((digit (predicate-parser #'digit-char-p))
                             (alpha (predicate-parser #'alpha-char-p)))
                  (cons digit alpha))))
    (is equal (cons #\1 #\a)
        (parse-string parser "1a"))

    (is equal (cons #\2 #\b)
        (parse-string parser "2b"))

    (fail (parse-string parser "3!")
          'parser-expected-element)

    (fail (parse-string parser "!")
          'parser-expected-element)))



;; Component Tests

(define-test parse-symbolic-expression)

(define-test parse-json-number-array)
