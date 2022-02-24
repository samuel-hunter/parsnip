;;; json.lisp - Parsnip example JSON decoder

;;; Copyright 2021 Samuel Hunter <samuel (at) shunter (dot) xyz>
;;; BSD-3-Clause

(defpackage #:xyz.shunter.parsnip.examples.json
  (:documentation "Parsnip example JSON decoder")
  (:use #:cl #:xyz.shunter.parsnip)
  (:import-from #:alexandria
                #:eswitch)
  (:export #:decode-json
           #:decode-json-from-string))

(in-package #:xyz.shunter.parsnip.examples.json)



;;; RFC 8259 § 2. JSON Grammar

;; whitespace := ( space | tab | newline | return )*
(defparameter *whitespace*
  (skip-many (char-in #(#\Space #\Tab #\Newline #\Return))))

(defun trim-ws (parser)
  (prog2! *whitespace* parser *whitespace*))

;; begin-array := ws '[' ws
(defparameter *begin-array*
  (trim-ws (char-of #\[)))

;; begin-object := ws '{' ws
(defparameter *begin-object*
  (trim-ws (char-of #\{)))

;; end-array := ws ']' ws
(defparameter *end-array*
  (trim-ws (char-of #\])))

;; end-object := ws '}' ws
(defparameter *end-object*
  (trim-ws (char-of #\})))

;; name-separator := ws ':' ws
(defparameter *name-separator*
  (trim-ws (char-of #\:)))

;; value-separator := ws ',' ws
(defparameter *value-separator*
  (trim-ws (char-of #\,)))

;; RFC 8259 § 7. Strings

(defparameter *char-code*
  (let! ((char (char-in "\"\\/bfnrt")))
    (ok (eswitch (char :test char=)
          (#\" #\") ;; " => " quotation mark
          (#\\ #\\) ;; \ => \ reverse solidus
          (#\/ #\/) ;; / => / solidus
          (#\b #\Backspace) ;; b => Backspace
          (#\f #\Page) ;; f => Form feed / New page
          (#\n #\Newline) ;; n => Line feed / New line
          (#\r #\Return) ;; r => Carriage return
          (#\t #\Tab))))) ;; t => Horizontal tab


(defparameter *unicode-char*
  (progn! (char-of #\u)
          (let! ((d1 (digit 16))
                 (d2 (digit 16))
                 (d3 (digit 16))
                 (d4 (digit 16)))
            (ok (code-char
                  (reduce (lambda (num dig)
                            (+ (* 16 num) dig))
                          (list d1 d2 d3 d4)))))))

;; char := unescaped | '\' char-code
(defparameter *char*
  (or! (progn! (char-of #\\)
               (or! *char-code* *unicode-char*))
            (char-if (lambda (c) (not (char= c #\"))))))

;; string := quotation char+ quotation

(defparser json-string ()
  (prog2!
    (char-of #\")
    (collect-into-string *char*)
    (char-of #\")))

;;; RFC 8259 § 4. Objects

;; member := string name-separator value
(defparameter *member*
  (let! ((name 'json-string)
         (value (progn! *name-separator* 'value)))
    (ok (cons name value))))

;; object := begin-object [ member ( value-separator member ) + ] end-object
(defparser json-object ()
  (prog2!
    *begin-object*
    (or! (sep *member* *value-separator*) (ok ()))
    *end-object*))
;;; RFC 8259 § 5. Arrays

;; array := begin-array [ value ( value-separator value )+ ] end-array
(defparser json-array ()
  (prog2!
    *begin-array*
    (or! (sep 'value *value-separator*) (ok ()))
    *end-array*))

;; RFC 8259 § 6. Numbers

;; frac := '.' int
(defparameter *frac*
  (progn! (char-of #\.)
          (let! ((val-and-digits
                   (reduce!
                     (lambda (v&d dig)
                       (destructuring-bind (val . ndigits) v&d
                         (cons (+ (* val 10) dig)
                               (1+ ndigits))))
                     (digit)
                     :initial-parser (ok (cons 0 0)))))
            (ok (destructuring-bind (val . ndigits) val-and-digits
                  (/ val (expt 10 ndigits)))))))

;; exp := 'e' ['+' | '-'] int
(defparameter *exp*
  (progn! (char-of #\e)
          (let! ((sign (or! (char-in "+-") (ok #\+)))
                 (num (natural)))
            (ok (* (if (char= sign #\+)
                       1 -1)
                   num)))))

;; number := [minus] int [frac] [exp]
(defparser json-number ()
  (let! ((sign (or! (char-of #\-) (ok nil)))
         (whole-part (natural))
         (frac-part (or! *frac* (ok 0)))
         (exp-part (or! *exp* (ok 0))))
    (setf sign (if sign -1 1))
    (ok (if (and (zerop frac-part)
                 (zerop exp-part))
            (* sign whole-part)
            (float (* sign (+ whole-part frac-part) (expt 10 exp-part)))))))

;; RFC 8259 § 3. Values

(defun literal (start remaining-name value)
  ;; Separate the starting char and remaining name so that the parser can peek
  ;; at a single char before reading the rest of the name. Possible since every
  ;; literal has a unique character (f, n, t).
  (let! ((remaining-actual (progn! (char-of start)
                                   (collect-into-string (char-if #'alpha-char-p)))))
    (if (string= remaining-actual remaining-name)
        (ok value)
        (fail (format nil "~C~A" start remaining-name)))))

;; value := 'false' | 'null' | 'true' | object | array | number | string
(defparser value ()
  (or! (literal #\f "alse" :false)
       (literal #\n "ull" :null)
       (literal #\t "rue" :true)
       'json-object
       'json-array
       'json-number
       'json-string))

;; text := ws value ws EOF
(defparameter *text*
  (prog2! *whitespace*
          #'value
          *whitespace*
          (eof)))



(defun decode-json (&optional (stream *standard-input*))
  (parse *text* stream))

(defun decode-json-from-string (string)
  (with-input-from-string (stream string)
    (decode-json stream)))
