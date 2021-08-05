;;; json.lisp - Parsnip example JSON decoder

;;; Copyright 2021 Samuel Hunter <samuel (at) shunter (dot) xyz>
;;; BSD-3-Clause

(defpackage #:xyz.shunter.parsnip.examples.json
  (:documentation "Parsnip example JSON decoder")
  (:use #:cl #:xyz.shunter.parsnip)
  (:import-from #:alexandria
                #:eswitch
                #:rcurry)
  (:export #:decode-json
           #:decode-json-from-string))

(in-package #:xyz.shunter.parsnip.examples.json)



(defun charbag-parser (charbag)
  (predicate-parser (lambda (c)
                      (position c charbag))))

;;; RFC 8259 § 2. JSON Grammar

;; whitespace := ( space | tab | newline | return )*
(defparameter *whitespace*
  (parse-reduce (constantly nil)
                (charbag-parser '(#\Space #\Tab #\Newline #\Return))
                nil))

(defun trim-ws (parser)
  (parse-prog2 *whitespace* parser *whitespace*))

;; begin-array := ws '[' ws
(defparameter *begin-array*
  (trim-ws (char-parser #\[)))

;; begin-object := ws '{' ws
(defparameter *begin-object*
  (trim-ws (char-parser #\{)))

;; end-array := ws ']' ws
(defparameter *end-array*
  (trim-ws (char-parser #\])))

;; end-object := ws '}' ws
(defparameter *end-object*
  (trim-ws (char-parser #\})))

;; name-separator := ws ':' ws
(defparameter *name-separator*
  (trim-ws (char-parser #\:)))

;; value-separator := ws ',' ws
(defparameter *value-separator*
  (trim-ws (char-parser #\,)))

;; RFC 8259 § 7. Strings

(defparameter *char-code*
  (parse-let ((char (charbag-parser "\"\\/bfnrt")))
    (eswitch (char :test char=)
      (#\" #\") ;; " => " quotation mark
      (#\\ #\\) ;; \ => \ reverse solidus
      (#\/ #\/) ;; / => / solidus
      (#\b #\Backspace) ;; b => Backspace
      (#\f #\Page) ;; f => Form feed / New page
      (#\n #\Newline) ;; n => Line feed / New line
      (#\r #\Return) ;; r => Carriage return
      (#\t #\Tab)))) ;; t => Horizontal tab


(defparameter *unicode-char*
  (parse-progn (char-parser #\u)
               (parse-map
                 (parse-take 4 (digit-parser 16))
                 (lambda (digits)
                   (code-char
                     (reduce (lambda (num dig) (+ (* 16 num) dig))
                             digits))))))

;; char := unescaped | '\' char-code
(defparameter *char*
  (parse-any (parse-progn (char-parser #\\)
                          (parse-any
                            *char-code*
                            *unicode-char*))
             (predicate-parser (lambda (c) (not (char= c #\"))))))

;; string := quotation char+ quotation

(defun add-to-string (string c)
  "Add a character to an adjustable string and return it."
  (vector-push-extend c string)
  string)

(defparameter *string*
  (parse-prog2
    (char-parser #\")
    (parse-defer
      (parse-reduce #'add-to-string
                    *char*
                    (make-array 0 :element-type 'character
                                :adjustable t
                                :fill-pointer 0)))
    (char-parser #\")))

;;; RFC 8259 § 4. Objects

;; member := string name-separator value
(defparameter *member*
  (parse-let ((name *string*)
              (value (parse-progn *name-separator*
                                  #'value)))
    (cons name value)))

;; object := begin-object [ member ( value-separator member ) + ] end-object
(defparameter *object*
  (parse-prog2
    *begin-object*
    (parse-optional (parse-let ((first-member *member*)
                                (other-members
                                  (parse-collect (parse-progn
                                                   *value-separator*
                                                   *member*))))
                      (cons first-member other-members))
                    nil)
    *end-object*))

;;; RFC 8259 § 5. Arrays

;; array := begin-array [ value ( value-separator value )+ ] end-array
(defparameter *array*
  (parse-prog2
    *begin-array*
    (parse-optional (parse-let ((first-value #'value)
                                (other-values
                                  (parse-collect (parse-progn
                                                   *value-separator*
                                                   #'value))))
                      (cons first-value other-values))
                    nil)
    *end-array*))

;; RFC 8259 § 6. Numbers

;; frac := '.' int
(defparameter *frac*
  (parse-progn (char-parser #\.)
               (parse-let ((digits (parse-collect1 (digit-parser))))
                 (/ (reduce (lambda (num dig) (+ (* num 10) dig))
                            digits)
                    (expt 10 (length digits))))))

;; exp := 'e' ['+' | '-'] int
(defparameter *exp*
  (parse-progn (char-parser #\e)
               (parse-let ((sign (parse-optional (charbag-parser "+-") #\+))
                           (num (integer-parser)))
                 (* (if (char= sign #\+)
                        1 -1)
                    num))))

;; number := [minus] int [frac] [exp]
(defparameter *number*
  (parse-let ((sign (parse-optional (char-parser #\-)))
              (whole-part (integer-parser))
              (frac-part (parse-optional *frac* 0))
              (exp-part (parse-optional *exp* 0)))
    (setf sign (if sign -1 1))
    (if (and (zerop frac-part)
             (zerop exp-part))
        (* sign whole-part)
        (float (* sign (+ whole-part frac-part) (expt 10 exp-part))))))

;; RFC 8259 § 3. Values

(defun literal-parser (start remaining-name value)
  ;; Separate the starting char and remaining name so that the parser can peek
  ;; at a single char before reading the rest of the name. Possible since every
  ;; literal has a unique character (f, n, t).
  (parse-map (parse-progn (char-parser start)
                          (string-parser remaining-name))
             (constantly value)))

;; value := 'false' | 'null' | 'true' | object | array | number | string
(defparser value ()
  (parse-any
    (literal-parser #\f "alse" :false)
    (literal-parser #\n "ull" :null)
    (literal-parser #\t "rue" :true)
    *object*
    *array*
    *number*
    *string*))

;; text := ws value ws EOF
(defparameter *text*
  (prog2 *whitespace*
         #'value
         *whitespace*
         (eof-parser)))



(defun decode-json (&optional (stream *standard-input*))
  (parse *text* stream))

(defun decode-json-from-string (string)
  (with-input-from-string (stream string)
    (decode-json stream)))
