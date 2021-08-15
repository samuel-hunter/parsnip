;;; test.lisp - Parsnip library test suite

;;; Copyright 2021 Samuel Hunter <samuel (at) shunter (dot) xyz>
;;; BSD-3-Clause

(defpackage #:xyz.shunter.parsnip.test
  (:use #:cl #:parachute
        #:xyz.shunter.parsnip
;;        #:xyz.shunter.parsnip.examples.json

        ))

(in-package #:xyz.shunter.parsnip.test)



(defun parse-string (parser string)
  (parse parser (coerce string 'list)))

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

(define-test char-parser
  (let ((parser (char-parser #\a)))
    (is char= #\a
        (parse-string parser "abc"))

    (is char= #\a
        (parse-string parser "a"))

    (fail (parse-string parser "z")
          'parser-error)

    (fail (parse-string parser "")
          'parser-error)))

(define-test predicate-parser
  (let ((parser (predicate-parser #'digit-char-p)))
    (is char= #\0
        (parse-string parser "0"))

    (is char= #\0
        (parse-string parser "012"))

    (is char= #\9
        (parse-string parser "9"))

    (fail (parse-string parser "z")
          'parser-error)

    (fail (parse-string parser "")
          'parser-error)))

(define-test string-parser
  (let ((parser (string-parser "foo")))
    (is string= "foo"
        (parse-string parser "foo"))

    (is string= "foo"
        (parse-string parser "foobar"))

    (fail (parse-string parser "bar")
          'parser-error)

    (fail (parse-string parser "fo")
          'parser-error)

    (fail (parse-string parser "")
          'parser-error)))

(define-test eof-parser
  (let ((parser (eof-parser)))
    (is eq nil
        (parse-string parser ""))

    (fail (parse-string parser "foo")
          'parser-error)))

(define-test parse-map
  :depends-on (char-parser)
  (let ((parser (parse-map #'char-code
                           (char-parser #\a))))
    (is = #.(char-code #\a)
        (parse-string parser "a"))

    (fail (parse-string parser "z")
          'parser-error))

  (let ((parser (parse-map #'list
                           (char-parser #\a)
                           (char-parser #\b))))
    (is equal '(#\a #\b)
        (parse-string parser "ab"))

    (fail (parse-string parser "a")
          'parser-error)))

(define-test parse-progn
  :depends-on (char-parser)
  (let ((a (parse-progn (char-parser #\a))))
    ;; parse-progn with a single element should act like its child parser
    (is char= #\a
        (parse-string a "a"))

    (fail (parse-string a "z")
          'parser-error))

  (let ((ab (parse-progn (char-parser #\a)
                         (char-parser #\b))))
    (is char= #\b
        (parse-string ab "ab"))

    (fail (parse-string ab "az")
          'parser-error)

    (fail (parse-string ab "b")
          'parser-error))

  (let ((abc (parse-progn (char-parser #\a)
                          (char-parser #\b)
                          (char-parser #\c))))
    (is char= #\c
        (parse-string abc "abc"))

    (fail (parse-string abc "abz")
          'parser-error)

    (fail (parse-string abc "az")
          'parser-error)

    (fail (parse-string abc "bc")
          'parser-error)

    (fail (parse-string abc "c")
          'parser-error)))

(define-test parse-prog1
  :depends-on (char-parser)
  (let ((a (parse-prog1 (char-parser #\a))))
    ;; parse-prog1 with a single element should act like its child parser
    (is char= #\a
        (parse-string a "a"))

    (fail (parse-string a "z")
          'parser-error))

  (let ((ab (parse-prog1 (char-parser #\a)
                         (char-parser #\b))))
    (is char= #\a
        (parse-string ab "ab"))

    (fail (parse-string ab "az")
          'parser-error)

    (fail (parse-string ab "b")
          'parser-error))

  (let ((abc (parse-prog1 (char-parser #\a)
                          (char-parser #\b)
                          (char-parser #\c))))
    (is char= #\a
        (parse-string abc "abc"))

    (fail (parse-string abc "abz")
          'parser-error)

    (fail (parse-string abc "az")
          'parser-error)

    (fail (parse-string abc "z")
          'parser-error)

    (fail (parse-string abc "bc")
          'parser-error)

    (fail (parse-string abc "c")
          'parser-error)))

(define-test parse-prog2
  :depends-on (char-parser)
  (let ((ab (parse-prog2 (char-parser #\a)
                         (char-parser #\b))))
    (is char= #\b
        (parse-string ab "ab"))

    (fail (parse-string ab "az")
          'parser-error)

    (fail (parse-string ab "z")
          'parser-error))

  (let ((abc (parse-prog2 (char-parser #\a)
                          (char-parser #\b)
                          (char-parser #\c))))
    (is char= #\b
        (parse-string abc "abc"))

    (fail (parse-string abc "abz")
          'parser-error)

    (fail (parse-string abc "az")
          'parser-error)

    (fail (parse-string abc "bc")
          'parser-error)

    (fail (parse-string abc "c")
          'parser-error)))

(define-test parse-or
  :depends-on (char-parser string-parser)
  (let ((a (parse-or (char-parser #\a))))
    (is char= #\a
        (parse-string a "a"))

    (fail (parse-string a "z")
          'parser-error))

  (let ((abc (parse-or (char-parser #\a)
                       (char-parser #\b)
                       (char-parser #\c))))
    (is char= #\a
        (parse-string abc "a"))

    (is char= #\b
        (parse-string abc "b"))

    (is char= #\c
        (parse-string abc "c"))

    (fail (parse-string abc "z")
          'parser-error))

  (let ((foobar (parse-or (string-parser "foo")
                          (string-parser "bar"))))
    (is string= "foo"
        (parse-string foobar "foo"))

    ;; Partially parsed on string "bar"
    (fail (parse-string foobar "baz")
          'parser-error
          "parse-or fails early on a partial parse.")))

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

  (let ((foo (parse-optional (string-parser "bar"))))
    (is string= "bar"
        (parse-string foo "bar"))

    (fail (parse-string foo "baz")
          'parser-error
          "parse-optional fails when any input is consumed.")))

(define-test parse-collect
  :depends-on (predicate-parser string-parser parse-optional)
  (let ((parser (parse-collect (predicate-parser #'alpha-char-p))))
    (is equal '(#\a)
        (parse-string parser "a"))

    (is equal '(#\a #\b #\c)
        (parse-string parser "abc"))

    (is equal ()
        (parse-string parser "")))

  (let ((bars (parse-collect (string-parser "bar"))))
    (is equal '("bar" "bar" "bar")
        (parse-string bars "barbarbar"))

    (fail (parse-string bars "barbarbaz")
          'parser-error
          "parse-collect fails when the last (errorful) parse consumes input."))

  (let ((maybe-bar (parse-collect (parse-optional (string-parser "bar")))))
    (fail (parse-string maybe-bar "foo")
          'parser-error
          "parse-collect's backing parser MUST consume on success.")))

(define-test parse-collect1
  :depends-on (predicate-parser string-parser)
  (let ((parser (parse-collect1 (predicate-parser #'alpha-char-p))))
    (is equal '(#\a)
        (parse-string parser "a"))

    (is equal '(#\a #\b #\c)
        (parse-string parser "abc"))

    (fail (parse-string parser "")
          'parser-error))

  (let ((bars (parse-collect (string-parser "bar"))))
    (is equal '("bar" "bar" "bar")
        (parse-string bars "barbarbar"))

    (fail (parse-string bars "barbarbaz")
          'parser-error
          "parse-collect1 fails when the last (errorful) parse consumes input."))

  (let ((maybe-bar (parse-collect (parse-optional (string-parser "bar")))))
    (fail (parse-string maybe-bar "foo")
          'parser-error
          "parse-collect1's backing parser MUST consume on success.")))

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

(define-test parse-take
  :depends-on (predicate-parser)
  (let ((letters (parse-take 3 (predicate-parser #'alpha-char-p))))
    (is equal '(#\a #\a #\a)
        (parse-string letters "aaa"))

    (is equal '(#\a #\b #\c)
        (parse-string letters "abc123"))

    (fail (parse-string letters "ab!")
          'parser-error)

    (fail (parse-string letters "ab")
          'parser-error)))

(define-test parse-try
  :depends-on (string-parser parse-or)
  (let* ((try-foo (parse-try (string-parser "foo")))
         (foo-or-bar (parse-or try-foo
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
          'parser-error)

    (fail (parse-string parser "!")
          'parser-error))

  (fail (eval '(parse-let (invalid-binding)
                 :whatever)))

  (fail (eval '(parse-let ((10 20 030 05 i5025 9258))
                 whatever))))

(define-test digit-parser
  (let ((decimal (digit-parser))
        (hex (digit-parser 16)))
    (is = 0
        (parse-string decimal "0"))

    (is = 9
        (parse-string decimal "9"))

    (fail (parse-string decimal "A")
          'parser-error)

    (is = 0
        (parse-string hex "0"))

    (is = 15
        (parse-string hex "F"))

    (is = 15
        (parse-string hex "f"))

    (fail (parse-string hex "G")
          'parser-error)))

(define-test integer-parser
  (let ((decimalparser (integer-parser))
        (hexparser (integer-parser 16)))
    (is = 0
        (parse-string decimalparser "0"))

    (is = 10
        (parse-string decimalparser "10"))

    (is = 5
        (parse-string decimalparser "00005"))

    (is = 123
        (parse-string decimalparser "123A"))

    (fail (parse-string decimalparser "A")
          'parser-error)

    (is = 255
        (parse-string hexparser "FF"))

    (is = 255
        (parse-string hexparser "ff"))

    (fail (parse-string hexparser "G")
          'parser-error)))



;; JSON Tests

;; (define-test json-numbers
;;   (is = 123
;;       (decode-json-from-string "123"))
;;
;;   (is = 50
;;       (decode-json-from-string "000050"))
;;
;;   (is = -123
;;       (decode-json-from-string "-123"))
;;
;;   (is close-enough 1.5
;;       (decode-json-from-string "1.5"))
;;
;;   (is close-enough 1.05
;;       (decode-json-from-string "1.05"))
;;
;;   (is close-enough 1e5
;;       (decode-json-from-string "1e5"))
;;
;;   (is close-enough 1e5
;;       (decode-json-from-string "1e+5"))
;;
;;   (is close-enough 1e-5
;;       (decode-json-from-string "1e-5"))
;;
;;   (is close-enough 15.0
;;       (decode-json-from-string "1.5e1")))
;;
;; (define-test json-strings
;;   (is string= "hello, world"
;;       (decode-json-from-string "\"hello, world\""))
;;
;;   (is string= (coerce #(#\" #\\ #\/ #\Backspace #\Page #\Newline #\Return #\Tab) 'string)
;;       (decode-json-from-string "\"\\\"\\\\\\/\\b\\f\\n\\r\\t\""))
;;
;;   (is string= "(λ (n) (* n n))"
;;       (decode-json-from-string "\"(\\u03BB (n) (* n n))\"")))
;;
;; (define-test json-arrays
;;   :depends-on (json-numbers json-strings)
;;   (is equal '(10 20 30)
;;       (decode-json-from-string "[10,20,30]"))
;;
;;   (is equal '(10)
;;       (decode-json-from-string "[10]"))
;;
;;   (is equal ()
;;       (decode-json-from-string "[]"))
;;
;;   (is equal '(10 "string" (20 30 40))
;;       (decode-json-from-string "[10, \"string\", [20, 30, 40]]"))
;;
;;   (is equal '(10 20 30)
;;       (decode-json-from-string " [ 10 , 20 , 30 ] ")))
;;
;; (define-test json-objects
;;   :depends-on (json-numbers json-strings)
;;   (is equal '(("key" . "value"))
;;       (decode-json-from-string "{\"key\":\"value\"}"))
;;
;;   (is equal '(("one" . 1) ("two" . 2) ("three" . 3))
;;       (decode-json-from-string "{\"one\":1,\"two\":2,\"three\":3}"))
;;
;;   (is equal '(("object" . (("key" . "value"))))
;;       (decode-json-from-string "{\"object\":{\"key\":\"value\"}}"))
;;
;;   (is equal '(("key" . "value") ("foo" . "bar"))
;;       (decode-json-from-string " { \"key\" : \"value\" , \"foo\" : \"bar\" }")))
