(defpackage #:xyz.shunter.parsnip.test
  (:use #:cl #:parsnip #:parachute))

(in-package #:xyz.shunter.parsnip.test)



(defun parse-string (parser string)
  (with-input-from-string (stream string)
    (parse parser stream)))



;; Unit Tests

(define-test accept-char
  (let ((parser (accept-char #\a)))
    (is char= #\a
        (parse-string parser "abc"))

    (is char= #\a
        (parse-string parser "a"))

    (fail (parse-string parser "z")
          'parser-expected-element)

    (fail (parse-string parser "")
          'end-of-file)))

(define-test accept-char-if
  (let ((parser (accept-char-if #'digit-char-p)))
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

(define-test accept-string
  (let ((parser (accept-string "foo")))
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

(define-test parser-progn
  :depends-on (accept-char)
  (let ((a (parser-progn (accept-char #\a))))
    ;; parser-progn with a single element should act like its child parser
    (is char= #\a
        (parse-string a "a"))

    (fail (parse-string a "z")
          'parser-expected-element))

  (let ((ab (parser-progn (accept-char #\a)
                          (accept-char #\b))))
    (is char= #\b
        (parse-string ab "ab"))

    (fail (parse-string ab "az")
          'parser-expected-element)

    (fail (parse-string ab "b")
          'parser-expected-element))

  (let ((abc (parser-progn (accept-char #\a)
                           (accept-char #\b)
                           (accept-char #\c))))
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

(define-test parser-prog1
  :depends-on (accept-char)
  (let ((a (parser-prog1 (accept-char #\a))))
    ;; parser-prog1 with a single element should act like its child parser
    (is char= #\a
        (parse-string a "a"))

    (fail (parse-string a "z")
          'parser-expected-element))

  (let ((ab (parser-prog1 (accept-char #\a)
                          (accept-char #\b))))
    (is char= #\a
        (parse-string ab "ab"))

    (fail (parse-string ab "az")
          'parser-expected-element)

    (fail (parse-string ab "b")
          'parser-expected-element))

  (let ((abc (parser-prog1 (accept-char #\a)
                           (accept-char #\b)
                           (accept-char #\c))))
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

(define-test parser-prog2
  :depends-on (accept-char)
  (let ((ab (parser-prog2 (accept-char #\a)
                          (accept-char #\b))))
    (is char= #\b
        (parse-string ab "ab"))

    (fail (parse-string ab "az")
          'parser-expected-element)

    (fail (parse-string ab "z")
          'parser-expected-element))

  (let ((abc (parser-prog2 (accept-char #\a)
                           (accept-char #\b)
                           (accept-char #\c))))
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

(define-test parser-any
  :depends-on (accept-char accept-string)
  (let ((a (parser-any (accept-char #\a))))
    (is char= #\a
        (parse-string a "a"))

    (fail (parse-string a "z")
          'parser-expected-element))

  (let ((abc (parser-any (accept-char #\a)
                         (accept-char #\b)
                         (accept-char #\c))))
    (is char= #\a
        (parse-string abc "a"))

    (is char= #\b
        (parse-string abc "b"))

    (is char= #\c
        (parse-string abc "c"))

    (fail (parse-string abc "z")
          'parser-expected-element))

  (let ((foobar (parser-any (accept-string "foo")
                            (accept-string "bar"))))
    (is string= "foo"
        (parse-string foobar "foo"))

    (fail (parse-string foobar "bar")
          'parser-expected-element
          "parse-any fails after more than one character is read.")))

(define-test parser-many
  :depends-on (accept-char)
  (let ((parser (parser-many (accept-char #\a))))
    (is equal (list #\a)
        (parse-string parser "a"))

    (is equal (list #\a #\a #\a)
        (parse-string parser "aaa"))

    (is equal ()
        (parse-string parser ""))))

(define-test parser-many1
  :depends-on (accept-char)
  (let ((parser (parser-many1 (accept-char #\a))))
    (is equal (list #\a)
        (parse-string parser "a"))

    (is equal (list #\a #\a #\a)
        (parse-string parser "aaa"))

    (fail (parse-string parser "")
          'end-of-file)))

(define-test parser-let
  :depends-on (accept-char-if)
  (let ((parser (parser-let ((digit (accept-char-if #'digit-char-p))
                             (alpha (accept-char-if #'alpha-char-p)))
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
