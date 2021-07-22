;;; json.lisp - Parsnip example JSON format reader

;;; Copyright 2021 Samuel Hunter <samuel (at) shunter (dot) xyz>
;;; BSD-3-Clause

(defpackage #:xyz.shunter.parsnip.examples.json
  (:nicknames #:parsnip.examples.json)
  (:documentation "A Parsnip implementation of a JSON reader")
  (:use #:cl #:parsnip)
  (:import-from #:alexandria
                #:eswitch)
  (:export #:read-json
           #:read-json-from-string))

(in-package #:xyz.shunter.parsnip.examples.json)



(defun accept-char-bag (char-bag)
  (accept-char-if (lambda (c)
                    (position c char-bag))))

;;; RFC 8259 § 2. JSON Grammar

;; whitespace := ( space | tab | newline | return )*
(defparameter *whitespace*
  (map-parser (parser-many (accept-char-bag
                                  '(#\Space #\Tab #\Newline #\Return)))
              (constantly nil)))

(defun accept-ws (parser)
  "Trim all leading and following whitespace"
  (parser-prog2 *whitespace*
                parser
                *whitespace*))

;; begin-array := ws '[' ws
(defparameter *begin-array*
  (accept-ws (accept-char #\[)))

;; begin-object := ws '{' ws
(defparameter *begin-object*
  (accept-ws (accept-char #\{)))

;; end-array := ws ']' ws
(defparameter *end-array*
  (accept-ws (accept-char #\])))

;; end-object := ws '}' ws
(defparameter *end-object*
  (accept-ws (accept-char #\})))

;; name-separator := ws ':' ws
(defparameter *name-separator*
  (accept-ws (accept-char #\:)))

;; value-separator := ws ',' ws
(defparameter *value-separator*
  (accept-ws (accept-char #\,)))

;;; RFC 8259 § 4. Objects

;; member := string name-separator value
(defparameter *member*
  (parser-let ((name *string*)
               (value (parser-progn *name-separator*
                                    *value*)))
    (cons name value)))

;; object := begin-object [ member ( value-separator member ) + ] end-object
(defparameter *object*
  (parser-prog2
    *begin-object*
    (parser-or (parser-let ((first-member *member*)
                            (other-members
                              (parser-many (parser-progn
                                             *value-separator*
                                             *member*))))
                 (cons first-member other-members))
               nil)
    *end-object*))

;;; RFC 8259 § 5. Arrays

;; array := begin-array [ value ( value-separator value )+ ] end-array
(defparameter *array*
  (parser-prog2
    *begin-array*
    (parser-or (parser-let ((first-value *value*)
                            (other-values
                              (parser-many (parser-progn
                                              *value-separator*
                                              *value*))))
                 (cons first-value other-values))
               nil)
    *end-array*))

;; RFC 8259 § 6. Numbers

;; digit := [0123456789]
(defparameter *digit*
  (parser-name 'digit (accept-char-if #'digit-char-p)))

;; int := digit+
(defparameter *int*
  (map-parser (parser-many1 *digit*)
              (lambda (digits) (parse-integer (coerce digits 'string)))))

;; frac := '.' int
(defparameter *frac*
  (parser-progn (accept-char #\.)
                (parser-let ((digits (parser-many1 *digit*)))
                  (/ (parse-integer (coerce digits 'string))
                     (expt 10 (length digits))))))

;; exp := 'e' ['+' | '-'] int
(defparameter *exp*
  (parser-progn (accept-char #\e)
                (parser-let ((sign (parser-or (accept-char-bag "+-") #\+))
                             (number *int*))
                  (* (if (char= sign #\+)
                         1 -1)
                     number))))

;; number := [minus] int [frac] [exp]
(defparameter *number*
  (parser-let ((sign (parser-or (accept-char #\-)))
               (whole-part *int*)
               (frac-part (parser-or *frac* 0))
               (exp-part (parser-or *exp* 0)))
    (setf sign (if sign -1 1))
    (if (and (zerop frac-part)
             (zerop exp-part))
        (* sign whole-part)
        (float (* sign (+ whole-part frac-part) (expt 10 exp-part))))))

;; RFC 8259 § 7. Strings

(defparameter *char-code*
  (parser-let ((char (accept-char-bag "\"\\/bfnrtu")))
    (print char)
    (eswitch (char :test char=)
      (#\" #\") ;; " => " quotation mark
      (#\\ #\\) ;; \ => \ reverse solidus
      (#\/ #\/) ;; / => / solidus
      (#\b #\Backspace) ;; b => Backspace
      (#\f #\Page) ;; f => Form feed / New page
      (#\n #\Newline) ;; n => Line feed / New line
      (#\r #\Return) ;; r => Carriage return
      (#\t #\Tab)))) ;; t => Horizontal tab

;; char := unescaped | '\' char-code
(defparameter *char*
  (parser-any (parser-progn (accept-char #\\)
                            *char-code*)
              (accept-char-if (lambda (c) (not (char= c #\"))))))

;; string := quotation char+ quotation
(defparameter *string*
  (parser-let ((chars (parser-prog2
                        (accept-char #\")
                        (parser-many *char*)
                        (accept-char #\"))))
    (coerce chars 'string)))

;; RFC 8259 § 3. Values

(defun accept-literal (start remaining-name value)
  ;; Separate the starting char and remaining name so that the parser can peek
  ;; at a single char before reading the rest of the name. Possible since every
  ;; literal has a unique character (f, n, t).
  (map-parser (parser-progn (accept-char start)
                            (accept-string remaining-name))
              (constantly value)))

;; value := 'false' | 'null' | 'true' | object | array | number | string
(defparameter *value*
  (parser-any
    (accept-literal #\f "alse" :false)
    (accept-literal #\n "ull" :null)
    (accept-literal #\t "rue" :true)
    *object*
    *array*
    *number*
    *string*))

;; text := ws value ws
(defparameter *text*
  (accept-ws *value*))



(defun read-json (&optional (stream *standard-input*))
  (parse *text* stream))

(defun read-json-from-string (string)
  (with-input-from-string (stream string)
    (read-json stream)))
