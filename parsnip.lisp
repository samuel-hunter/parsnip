;;; parsnip.lisp - Parsnip library implementation

;;; Copyright 2021 Samuel Hunter <samuel (at) shunter (dot) xyz>
;;; BSD-3-Clause

(defpackage #:xyz.shunter.parsnip
  (:documentation "Parser combinator library")
  (:nicknames #:parsnip)
  (:use #:cl)
  (:import-from #:alexandria
                #:with-gensyms
                #:once-only
                #:curry
                #:rcurry)
  (:export #:parser-error
           #:parser-error-expected
           #:parser-error-return-trace
           #:parse

           #:char-parser
           #:predicate-parser
           #:string-parser
           #:eof-parser

           #:parse-map

           #:parse-progn
           #:parse-prog1
           #:parse-prog2

           #:parse-or
           #:parse-optional

           #:parse-collect
           #:parse-collect1
           #:parse-collect-string
           #:parse-reduce
           #:parse-take

           #:parse-try
           #:parse-tag

           #:parse-let
           #:parse-defer
           #:defparser

           #:digit-parser
           #:integer-parser))

(in-package #:xyz.shunter.parsnip)



;; Result are cons cells tagged in the CAR and payload values in the CDR.
;; Ok value: (TAG . VALUE)
;; Fail value: (TAG STREAM EXPECTED . RETURN-TRACE)

(declaim (inline cok eok cfail efail
                 ok-value fail-stream fail-expected fail-return-trace
                 (setf fail-expected) (setf fail-return-trace)
                 cokp eokp cfailp efailp okp failp))
(defun cok (value)
  `(:cok .,value))

(defun eok (value)
  `(:eok .,value))

(defun cfail (stream expected)
  `(:cfail ,stream ,expected . nil))

(defun efail (stream expected)
  `(:efail ,stream ,expected . nil))

(defun ok-value (ok)
  (cdr ok))

(defun fail-stream (fail)
  (cadr fail))

(defun fail-expected (fail)
  (caddr fail))
(defun (setf fail-expected) (new-value fail)
  (setf (caddr fail) new-value))

(defun fail-return-trace (fail)
  (cdddr fail))
(defun (setf fail-return-trace) (new-value fail)
  (setf (cdddr fail) new-value))

(defun cokp (result)
  (eq (car result) :cok))
(defun eokp (result)
  (eq (car result) :eok))
(defun cfailp (result)
  (eq (car result) :cfail))
(defun efailp (result)
  (eq (car result) :efail))

(defun okp (result)
  (or (cokp result)
      (eokp result)))
(defun failp (result)
  (or (cfailp result)
      (efailp result)))

(defmacro eparsecase (result &body clauses)
  `(ecase (car ,result)
     .,clauses))

(defmacro parsecase (result &body clauses)
  (once-only (result)
    `(case (car ,result)
       ,@clauses
       (otherwise ,result))))

(defmacro with-ok (form (&optional var) &body body)
  (once-only (form)
    `(eparsecase ,form
       ((:cok :eok)
        ,(if var
             `(let ((,var (ok-value ,form)))
                .,body)
             `(progn .,body)))
       ((:efail :cfail)
        ,form))))

(defmacro with-trace ((name) &body body)
  (with-gensyms (result)
    `(let ((,result (progn .,body)))
       (eparsecase ,result
         ((:cok :eok)
          ,result)
         ((:cfail :efail)
          (progn
            (push (quote ,name) (fail-return-trace ,result))
            ,result))))))

(define-condition parser-error (stream-error)
  ((expected :initarg :expected :reader parser-error-expected)
   (return-trace :initarg :return-trace :reader parser-error-return-trace))
  (:report (lambda (condition stream)
             (let ((expected (parser-error-expected condition))
                   (err-stream (stream-error-stream condition)))
               (format stream "Expected element ~S on ~S (position ~S)"
                       expected
                       err-stream
                       (ignore-errors (file-position err-stream)))))))

(defun error-fail (fail)
  "Signal an error depending on the given fail."
  (error 'parser-error
         :stream (fail-stream fail)
         :expected (fail-expected fail)
         :return-trace (fail-return-trace fail)))

(defun parse (parser stream)
  "Run a parser through a given stream and raise any fails as a condition."
  (let ((result (funcall parser stream)))
    (eparsecase result
      ((:cok :eok) (ok-value result))
      ((:cfail :efail) (error-fail result)))))



;; Primitive Parselets

(defun char-parser (char)
  "Return a parser that accepts the given character value."
  (lambda (stream)
    (let ((actual-char (peek-char nil stream nil)))
      (cond
        ((null actual-char)
         (efail stream char))
        ((char= char actual-char)
         (cok (read-char stream)))
        (t (efail stream char))))))

(defun predicate-parser (predicate)
  "Return a parser that accepts a character if the given predicate returns
   true."
  (lambda (stream)
    (let ((actual-char (peek-char nil stream nil)))
      (cond
        ((null actual-char)
         (efail stream predicate))
        ((funcall predicate actual-char)
         (cok (read-char stream)))
        (t (efail stream predicate))))))

(defun string-parser (string)
  "Return a parser that accepts the given string value. May partially parse on
   fail."
  (lambda (stream)
    (let* ((actual-string (make-string (length string)))
           (chars-read (read-sequence actual-string stream)))
      (cond
        ((zerop chars-read)
         (efail stream string))
        ((string= actual-string string)
         (cok actual-string))
        (t (cfail stream string))))))

(defun eof-parser (&optional value)
  (lambda (stream)
    (if (null (peek-char nil stream nil))
        (cok value)
        (efail stream :eof))))



;; Combinators

(defun parse-map (function &rest parsers)
  "Compose multiple parsers to run in sequence, and apply the function to all
   parsers' values."
  (lambda (stream)
    (do ((parsers-left parsers (rest parsers-left))
         arguments)
        ((null parsers-left)
         (cok (apply function (nreverse arguments))))
        (let ((result (funcall (first parsers-left) stream)))
          (eparsecase result
            ((:cok :eok)
             (push (ok-value result) arguments))
            ((:efail :cfail)
             (return result)))))))

(defun parse-progn (&rest parsers)
  "Compose multiple parsers to run in sequence, returning the last parser's
   value."
  (assert (plusp (length parsers)))
  (lambda (stream)
    (do ((parsers-left parsers (rest parsers-left))
         consumed
         final-result)
        ((null parsers-left)
         (if consumed
             (cok (ok-value final-result))
             final-result))
        (let ((result (funcall (first parsers-left) stream)))
          (eparsecase result
            (:cok (setf consumed t
                        final-result result))
            (:eok (setf final-result result))
            ((:cfail :efail) (return result)))))))

(defun parse-prog1 (first-parser &rest parsers)
  "Compose multiple parsers to run in sequence, returning the first parser's
   value."
  (when (null parsers)
    (return-from parse-prog1 first-parser))

  (let ((inner-parser (apply 'parse-progn parsers)))
    (lambda (stream)
      (let ((first-result (funcall first-parser stream)))
        (parsecase first-result
          ((:eok :cok)
           (let ((final-result (funcall inner-parser stream)))
             (eparsecase final-result
               ((:cok :eok) first-result)
               (:cfail final-result)
               (:efail (if (cokp first-result)
                           (cfail (fail-stream final-result)
                                  (fail-expected final-result))
                           final-result))))))))))

(defun parse-prog2 (first-parser second-parser &rest parsers)
  "Compose multple parsers to run in sequence, returning the second parser's
   value."
  (parse-progn first-parser (apply 'parse-prog1 second-parser parsers)))

(defun parse-collect (parser)
  "Enhance the parser to keep running until fail, and collect the results.
   Passes through any partial-parse falures."
  (lambda (stream)
    (do ((last-result (funcall parser stream) (funcall parser stream))
         values)
        ((failp last-result)
         (if (efailp last-result)
             (cok (nreverse values))
             last-result))
        (push (ok-value last-result) values))))

(defun parse-collect1 (parser)
  "Enhance the parser to keep running until fail, and collect AT LEAST one
   result. Passes through any partial-parse fail."
  (let ((collect-parser (parse-collect parser)))
    (lambda (stream)
      (with-ok (funcall parser stream) (head)
        (with-ok (funcall collect-parser stream) (tail)
          (cok (cons head tail)))))))

(defun parse-collect-string (parser)
  "Enhance the parser to keep collecting chars until fail, and return a string."
  (lambda (stream)
    (do ((last-result (funcall parser stream) (funcall parser stream))
         (string (make-array 0
                             :element-type 'character
                             :adjustable t
                             :fill-pointer 0)))
        ((failp last-result)
         (if (efailp last-result)
             (cok string)
             last-result))
        (vector-push-extend (ok-value last-result) string))))

(defun parse-reduce (function parser initial-value)
  "Enhance the parser to keep running until fail, and reduce the results
   into a single value. Passes through any partial-parse fail."
  (lambda (stream)
    (do ((last-result (funcall parser stream) (funcall parser stream))
         (value initial-value (funcall function value (ok-value last-result))))
        ((failp last-result)
         (if (efailp last-result)
             (cok value)
             last-result)))))

(defun parse-take (times parser)
  "Enhance the partial to run EXACTLY the given number of times and collect the
   results. Passes through any partial-parse fail."
  (check-type times (integer 0 *))
  (labels ((take-iter (n stream)
             (if (zerop n)
                 (cok ())
                 (with-ok (funcall parser stream) (head)
                   (with-ok (take-iter (1- n) stream) (tail)
                     (cok (cons head tail)))))))
    (lambda (stream)
      (with-ok (take-iter times stream) (list)
        (cok list)))))

(defun parse-or (&rest parsers)
  "Attempts each parser in order until one succeeds. Passes through any
   partial-parse fail."
  (lambda (stream)
    (do ((parsers-left parsers (rest parsers-left))
         expected-elements)
        ((null parsers-left)
         (efail stream (nreverse expected-elements)))
        (let ((result (funcall (first parsers-left) stream)))
          (eparsecase result
            ((:cok :eok :cfail)
             (return result))
            (:efail
              (push (fail-expected result) expected-elements)))))))

(defun parse-optional (parser &optional default)
  "Enhance the parser to resume from an error with a default value if it did
   not consume input."
  (lambda (stream)
    (parsecase (funcall parser stream)
      (:efail (eok default)))))

(defun parse-try (parser)
  "Enahnce the parser to try to rewind the stream on partial-parse fail.
   Only works on seekable streams."
  (lambda (stream)
    (let ((old-position (file-position stream))
          (result (funcall parser stream)))
      (parsecase result
        (:cfail
          (file-position stream old-position)
          (efail stream (fail-expected result)))))))

(defun parse-tag (tag parser)
  "Reports fails as expecting the given tag instead of an element"
  (lambda (stream)
    (let ((result (funcall parser stream)))
      (parsecase result
        ((:efail :cfail) (setf (fail-expected result) tag)
                         result)))))

(defun ensure-bindings (bindings)
  (dolist (binding bindings)
    (check-type binding (cons symbol *))))

(defmacro parse-let (bindings &body body)
  "Compose multiple parsers together to bind their results to variables and
   return a value within the body."
  (ensure-bindings bindings)
  `(parse-map (lambda ,(mapcar #'first bindings)
                ,@body)
              ,@(mapcar #'second bindings)))

(defmacro parse-defer (form)
  "Defer evaluating the parser-returning FORM until the parser is called.
   Useful for circular-referencing parsers."
  (with-gensyms (stream)
    `(lambda (,stream) (funcall ,form ,stream))))

(defmacro defparser (name () &body (form))
  "Define a parser as a function.
   Enables other parsers to forward-reference it before it is defined."
  (with-gensyms (stream)
    `(defun ,name (,stream)
       (with-trace (,name)
         (funcall ,form ,stream)))))



;; Complex Parselets

(defun digit-parser (&optional (radix 10))
  (check-type radix (integer 2 36))
  (parse-tag (cons :digit radix)
             (parse-map (lambda (ch)
                          (let ((code (char-code ch)))
                            (if (<= #.(char-code #\0) code #.(char-code #\9))
                                (- code #.(char-code #\0))
                                (+ 10 (- (char-code (char-upcase ch)) #.(char-code #\A))))))
                        (predicate-parser (rcurry #'digit-char-p radix)))))

(defun integer-parser (&optional (radix 10))
  (parse-tag (cons :integer radix)
             (parse-map (curry #'reduce (lambda (num dig) (+ (* num radix) dig)))
                        (parse-collect1 (digit-parser radix)))))
