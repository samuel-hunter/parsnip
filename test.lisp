;;; test.lisp - Parsnip library test suite

;;; Copyright 2021 Samuel Hunter <samuel (at) shunter (dot) xyz>
;;; BSD-3-Clause

(defpackage #:xyz.shunter.parsnip.test
  (:use #:cl #:parachute
        #:xyz.shunter.parsnip
        #:xyz.shunter.parsnip.examples.json))

(in-package #:xyz.shunter.parsnip.test)



(defun parse-string (parser string)
  (with-input-from-string (stream string)
    (parse parser stream)))

(defun close-enough (float-1 float-2)
  "Return whether two decimal numbers are within tolerance (0.05%)"
  (when (zerop (+ float-1 float-2))
    (return-from close-enough nil))
  (let ((difference (abs (/ (- float-1 float-2) (abs (+ float-1 float-2)) 2)))
        (tolerance 0.0005))
    (< difference tolerance)))



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

(define-test parse-map
  :depends-on (char-parser)
  (let ((parser (parse-map (char-parser #\a)
                           #'char-code)))
    (is = #.(char-code #\a)
        (parse-string parser "a"))

    (fail (parse-string parser "b")
          'parser-expected-element)))

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
          "parse-any fails when any input is consumed.")))

(define-test parse-optional
  :depends-on (char-parser string-parser)
  (let ((a (parse-optional (char-parser #\a))))
    (is char= #\a
        (parse-string a "a"))

    (is eq nil
        (parse-string a "z"))

    (is eq nil
        (parse-string a "")))

  (let ((a-or-b (parse-optional (char-parser #\a)
                                #\b)))
    (is char= #\b
        (parse-string a-or-b "z"))

    (is char= #\b
        (parse-string a-or-b "")))

  (let ((foo (parse-optional (string-parser "foo"))))
    (is string= "foo"
        (parse-string foo "foo"))

    (fail (parse-string foo "bar")
          'parser-expected-element
          "parse-optional fails when any input is consumed.")))

(define-test parse-collect
  :depends-on (predicate-parser string-parser)
  (let ((parser (parse-collect (predicate-parser #'alpha-char-p))))
    (is equal '(#\a)
        (parse-string parser "a"))

    (is equal '(#\a #\b #\c)
        (parse-string parser "abc"))

    (is equal ()
        (parse-string parser "")))

  (let ((foos (parse-collect (string-parser "foo"))))
    (is equal '("foo" "foo" "foo")
        (parse-string foos "foofoofoo"))

    (fail (parse-string foos "foofoobar")
          'parser-expected-element
          "parse-collect fails when the last (errorful) parse consumes input.")))

(define-test parse-collect1
  :depends-on (predicate-parser string-parser)
  (let ((parser (parse-collect1 (predicate-parser #'alpha-char-p))))
    (is equal '(#\a)
        (parse-string parser "a"))

    (is equal '(#\a #\b #\c)
        (parse-string parser "abc"))

    (fail (parse-string parser "")
          'end-of-file))

  (let ((foos (parse-collect (string-parser "foo"))))
    (is equal '("foo" "foo" "foo")
        (parse-string foos "foofoofoo"))

    (fail (parse-string foos "foofoobar")
          'parser-expected-element
          "parse-collect1 fails when the last (errorful) parse consumes input.")))

(define-test parse-reduce
  :depends-on (predicate-parser)
  (let ((number-parser (parse-reduce
                         (lambda (num digit)
                           (+ (* 10 num)
                              (- (char-code digit) #.(char-code #\0))))
                         (predicate-parser #'digit-char-p) 0)))
    (is = 123
        (parse-string number-parser "123"))

    (is = 0
        (parse-string number-parser ""))))

(define-test parse-do
  :depends-on (char-parser parse-progn)
  (let* ((spaces (parse-do (char-parser #\Space)))
         (letter (parse-progn spaces
                              (char-parser #\a))))
    (is eq nil
        (parse-string spaces "   "))

    (is eq nil
        (parse-string spaces "   1"))

    (is eq nil
        (parse-string spaces ""))

    (is eq :foo
        (parse-string (parse-do (char-parser #\Space) :foo)
                      "   "))

    (is eq #\a
        (parse-string letter "   a"))))

(define-test parse-take
  :depends-on (predicate-parser)
  (let ((letters (parse-take 3 (predicate-parser #'alpha-char-p))))
    (is equal '(#\a #\a #\a)
        (parse-string letters "aaa"))

    (is equal '(#\a #\b #\c)
        (parse-string letters "abc123"))

    (fail (parse-string letters "ab!")
          'parser-expected-element)

    (fail (parse-string letters "ab")
          'end-of-file)))

(define-test parse-try
  :depends-on (string-parser parse-any)
  (let* ((try-foo (parse-try (string-parser "foo")))
         (foo-or-bar (parse-any try-foo
                                (string-parser "bar"))))
    (is string= "foo"
        (parse-string try-foo "foo"))

    (is string= "foo"
        (parse-string foo-or-bar "foo"))

    (is string= "bar"
        (parse-string foo-or-bar "bar"))))

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

(define-test parse-defer
  :depends-on (char-parser)
  (let* (the-char
         (parser (parse-defer (char-parser the-char))))
    (setf the-char #\a)
    (is char= #\a
        (parse-string parser "a"))

    (setf the-char #\b)
    (is char= #\b
        (parse-string parser "b"))))



;; JSON Tests

(define-test json-numbers
  (is = 123
      (read-json-from-string "123"))

  (is = 50
      (read-json-from-string "000050"))

  (is = -123
      (read-json-from-string "-123"))

  (is close-enough 1.5
      (read-json-from-string "1.5"))

  (is close-enough 1.05
      (read-json-from-string "1.05"))

  (is close-enough 1e5
      (read-json-from-string "1e5"))

  (is close-enough 1e5
      (read-json-from-string "1e+5"))

  (is close-enough 1e-5
      (read-json-from-string "1e-5"))

  (is close-enough 15.0
      (read-json-from-string "1.5e1")))

(define-test json-strings
  (is string= "hello, world"
      (read-json-from-string "\"hello, world\""))

  (is string= (coerce #(#\" #\\ #\/ #\Backspace #\Page #\Newline #\Return #\Tab) 'string)
      (read-json-from-string "\"\\\"\\\\\\/\\b\\f\\n\\r\\t\""))

  (is string= "(λ (n) (* n n))"
      (read-json-from-string "\"(\\u03BB (n) (* n n))\"")))

(define-test json-arrays
  :depends-on (json-numbers json-strings)
  (is equal '(10 20 30)
      (read-json-from-string "[10,20,30]"))

  (is equal '(10)
      (read-json-from-string "[10]"))

  (is equal ()
      (read-json-from-string "[]"))

  (is equal '(10 "string" (20 30 40))
      (read-json-from-string "[10, \"string\", [20, 30, 40]]"))

  (is equal '(10 20 30)
      (read-json-from-string " [ 10 , 20 , 30 ] ")))

(define-test json-objects
  :depends-on (json-numbers json-strings)
  (is equal '(("key" . "value"))
      (read-json-from-string "{\"key\":\"value\"}"))

  (is equal '(("one" . 1) ("two" . 2) ("three" . 3))
      (read-json-from-string "{\"one\":1,\"two\":2,\"three\":3}"))

  (is equal '(("object" . (("key" . "value"))))
      (read-json-from-string "{\"object\":{\"key\":\"value\"}}"))

  (is equal '(("key" . "value") ("foo" . "bar"))
      (read-json-from-string " { \"key\" : \"value\" , \"foo\" : \"bar\" }")))
