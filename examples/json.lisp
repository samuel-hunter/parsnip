(defpackage #:xyz.shunter.parsnip.examples.json
  (:nicknames #:parsnip.examples.json)
  (:documentation "A Parsnip implementation of a JSON reader")
  (:use #:cl #:parsnip)
  (:import-from #:alexandria
                #:eswitch
                #:rcurry)
  (:export #:read-json-value
           #:read-json-value-from-string))

(in-package #:xyz.shunter.parsnip.examples.json)



(defun accept-char-bag (char-bag)
  (accept-char-if (lambda (c)
                    (position c char-bag))))


;; Digit := [0123456789]              (base 10)
;;        | [0123456789abcdefABCDEF]  (base 16)
(defun accept-digit (radix)
  (parser-name 'digit (accept-char-if (rcurry #'digit-char-p radix))))

;; Number := digit +
(defun accept-integer (base)
  (parser-let ((digits (parser-many1 (accept-digit base))))
    (parse-integer (coerce digits 'string) :radix base)))

(defparameter *whitespace*
  (map-parser (parser-many (accept-char-bag
                                  '(#\Space #\Tab #\Newline #\Return)))
              (constantly nil)))

(defun accept-ws (parser)
  "Trim all leading and following whitespace"
  (parser-prog2 *whitespace*
                parser
                *whitespace*))

(defparameter *escape-code*
  (parser-let ((char (accept-char-bag "\"\\/bfnrtu")))
    (print char)
    (eswitch (char :test char=)
      (#\" #\")
      (#\\ #\\)
      (#\/ #\/)
      (#\b #\Backspace)
      (#\f #\Page)
      (#\n #\Newline)
      (#\r #\Return)
      (#\t #\Tab))))

(defparameter *string-char*
  (parser-any (parser-progn (accept-char #\\)
                            *escape-code*)
              (accept-char-if (lambda (c) (not (char= c #\"))))))

(defparameter *string*
  (parser-let ((chars (parser-prog2
                        (accept-char #\")
                        (parser-many *string-char*)
                        (accept-char #\"))))
    (coerce chars 'string)))

(defparameter *begin-array*
  (accept-ws (accept-char #\[)))

(defparameter *begin-object*
  (accept-ws (accept-char #\{)))

(defparameter *end-array*
  (accept-ws (accept-char #\])))

(defparameter *end-object*
  (accept-ws (accept-char #\})))

(defparameter *name-separator*
  (accept-ws (accept-char #\:)))

(defparameter *value-separator*
  (accept-ws (accept-char #\,)))

(defparameter *member*
  (parser-let ((name *string*)
               (value (parser-progn *name-separator*
                                    *value*)))
    (cons name value)))

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

(defun accept-literal (start remaining-name value)
  ;; Separate the starting char and remaining name so that the parser can peek
  ;; at a single char before reading the rest of the name. Possible since every
  ;; literal has a unique character (f, n, t).
  (map-parser (parser-progn (accept-char start)
                            (accept-string remaining-name))
              (constantly value)))

(defparameter *frac*
  (parser-progn (accept-char #\.)
                (parser-let ((digits (parser-many1 (accept-digit 10))))
                  (/ (parse-integer (coerce digits 'string))
                     (expt 10 (length digits))))))

(defparameter *exp*
  (parser-progn (accept-char #\e)
                (parser-let ((sign (parser-or (accept-char-bag "+-") #\+))
                             (number (accept-integer 10)))
                  (* (if (char= sign #\+)
                         1 -1)
                     number))))

(defparameter *number*
  (parser-let ((sign (parser-or (accept-char #\-)))
               (whole-part (accept-integer 10))
               (frac-part (parser-or *frac* 0))
               (exp-part (parser-or *exp* 0)))
    (setf sign (if sign -1 1))
    (if (and (zerop frac-part)
             (zerop exp-part))
        (* sign whole-part)
        (float (* sign (+ whole-part frac-part) (expt 10 exp-part))))))

(defparameter *value*
  (parser-any
    (accept-literal #\f "alse" :false)
    (accept-literal #\n "ull" :null)
    (accept-literal #\t "rue" :true)
    *object*
    *array*
    *number*
    *string*))

(defparameter *text*
  (accept-ws *value*))



(defun read-json-value (&optional (stream *standard-input*))
  (parse *value* stream))

(defun read-json-value-from-string (string)
  (with-input-from-string (stream string)
    (read-json-value stream)))
