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
           #:parser-error-stream
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
           #:defparser

           #:digit-parser
           #:integer-parser))

(in-package #:xyz.shunter.parsnip)



;; Results are cons cells tagged in the CAR and payload values in the CDR.
;; Ok value: (TAG . VALUE)  (tags are :COK or :EOK)
;; Fail value: (TAG EXPECTED . RETURN-TRACE)  (tags are :CFAIL or :EFAIL)
;; The C or E prefix means whether input was consumed or empty.

;; I experimented with three types of results:
;; - Results as structs
;; - Results as lists
;; - Use callbacks instead of wrapping result-values
;;
;; I tried callbacks a la Haskell parsec (see the callback-evaluation branch on
;; Sourcehut), and the performance at the time was horrid (~8x slower than
;; cl-json instead of ~2.5x slower). Granted, I also changed how errors looked,
;; but a look into sb-sprof showed that most time was spent in the primitive
;; parser. So, that's off the table for now.
;;
;; Structs vs Lists were comparatively marginal, performance-wise. I can go for
;; structs if I have to, though tinkering with both, I like how `parsecase`
;; clauses look when going through the code, so I'm sticking with that. I'm
;; also "hand-coding" the list structure instead of using a defstruct with a
;; list type, because the way list-structs are structured are *juuust* slightly
;; off enough that I preferred going away from it.

(declaim (inline cok eok cfail efail
                 ok-value fail-expected fail-return-trace
                 (setf fail-expected) (setf fail-return-trace)
                 cokp eokp cfailp efailp okp failp))
(defun cok (value)
  `(:cok . ,value))

(defun eok (value)
  `(:eok . ,value))

(defun cfail (expected)
  `(:cfail ,expected . nil))

(defun efail (expected)
  `(:efail ,expected . nil))

(defun ok-value (ok)
  (cdr ok))

(defun fail-expected (fail)
  (cadr fail))
(defun (setf fail-expected) (new-value fail)
  (setf (cadr fail) new-value))

(defun fail-return-trace (fail)
  (cddr fail))
(defun (setf fail-return-trace) (new-value fail)
  (setf (cddr fail) new-value))

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

;; Helper macros

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

;; Toplevel evaluation converts results into something a little more
;; front-facing for library consumers.

(define-condition parser-error (#-abcl stream-error
                                #+abcl error)
  (#+abcl (stream :initarg :stream :reader parser-error-stream
                  :documentation
                  "ABCL throws an error on stream-error-stream, even though
                   parser-error would have been typep to stream-error. To
                   compenstate, parser-error-stream is a more portable function
                   while stream-error-stream remains available to other
                   impl's.")
   (expected :initarg :expected :reader parser-error-expected)
   (return-trace :initarg :return-trace :reader parser-error-return-trace))
  (:report (lambda (condition stream)
             (let ((expected (parser-error-expected condition))
                   (err-stream (stream-error-stream condition)))
               (format stream "Expected element ~S on ~S (position ~S)"
                       expected
                       err-stream
                       (ignore-errors (file-position err-stream)))))))

#-abcl
(defun parser-error-stream (parser-error)
  (stream-error-stream parser-error))

(defun error-fail (fail stream)
  "Signal an error depending on the given fail."
  (error 'parser-error
         :stream stream
         :expected (fail-expected fail)
         :return-trace (fail-return-trace fail)))

(defun parse (parser stream)
  "Run a parser through a given stream and raise any fails as a condition."
  (let ((result (funcall parser stream)))
    (eparsecase result
      ((:cok :eok) (ok-value result))
      ((:cfail :efail) (error-fail result stream)))))



;; Primitive Parselets

(defun char-parser (char)
  "Consume and return the given character."
  (lambda (stream)
    (let ((actual-char (peek-char nil stream nil)))
      (cond
        ((null actual-char)
         (efail char))
        ((char= char actual-char)
         (cok (read-char stream)))
        (t (efail char))))))

(defun predicate-parser (predicate)
  "Consume and return a character that passes the given predicate."
  (lambda (stream)
    (let ((actual-char (peek-char nil stream nil)))
      (cond
        ((null actual-char)
         (efail predicate))
        ((funcall predicate actual-char)
         (cok (read-char stream)))
        (t (efail predicate))))))

(defun string-parser (string)
  "Consume and return the given text. May partially parse on failure."
  (lambda (stream)
    (let* ((actual-string (make-string (length string)))
           (chars-read (read-sequence actual-string stream)))
      (cond
        ((zerop chars-read)
         (efail string))
        ((string= actual-string string)
         (cok actual-string))
        (t (cfail string))))))

(defun eof-parser (&optional value)
  "Return the given value (or NIL) if at EOF."
  (lambda (stream)
    (if (null (peek-char nil stream nil))
        (cok value)
        (efail :eof))))



;; Combinators

(defun parse-map (function &rest parsers)
  "Run the parsers in sequence and apply the given function to all results."
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
  "Run the parsers in sequence and return the last result."
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
  "Run the parsers in sequence and return the last result."
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
                           (cfail (fail-expected final-result))
                           final-result))))))))))

(defun parse-prog2 (first-parser second-parser &rest parsers)
  "Run the parsers in sequence and return the second result."
  (parse-progn first-parser (apply 'parse-prog1 second-parser parsers)))

(defun parse-collect (parser)
  "Run until failure, and then return the collected results."
  (lambda (stream)
    (do ((last-result (funcall parser stream) (funcall parser stream))
         values)
        ((failp last-result)
         (if (efailp last-result)
             (cok (nreverse values))
             last-result))
        (push (ok-value last-result) values))))

(defun parse-collect1 (parser)
  "Run until failure, and then return at LEAST one collected result."
  (let ((collect-parser (parse-collect parser)))
    (lambda (stream)
      (with-ok (funcall parser stream) (head)
        (with-ok (funcall collect-parser stream) (tail)
          (cok (cons head tail)))))))

(defun parse-collect-string (parser)
  "Run until failure, and then return the collected characters as a string."
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
  "Run until failure, and then reduce the results into one value."
  (lambda (stream)
    (do ((last-result (funcall parser stream) (funcall parser stream))
         (value initial-value (funcall function value (ok-value last-result))))
        ((failp last-result)
         (if (efailp last-result)
             (cok value)
             last-result)))))

(defun parse-take (times parser)
  "Run and collect EXACTLY the given number of results."
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
  "Attempt each parser in order until one succeeds. Passes through any
   partial-parse fail."
  (lambda (stream)
    (do ((parsers-left parsers (rest parsers-left))
         expected-elements)
        ((null parsers-left)
         (efail (nreverse expected-elements)))
        (let ((result (funcall (first parsers-left) stream)))
          (eparsecase result
            ((:cok :eok :cfail)
             (return result))
            (:efail
              (push (fail-expected result) expected-elements)))))))

(defun parse-optional (parser &optional default)
  "Resume from a failure with a default value."
  (lambda (stream)
    (parsecase (funcall parser stream)
      (:efail (eok default)))))

(defun parse-try (parser)
  "Try to rewind the stream on any partial-parse failure. Only works on
   seekable streams."
  (lambda (stream)
    (let ((old-position (file-position stream))
          (result (funcall parser stream)))
      (parsecase result
        (:cfail
          (file-position stream old-position)
          (efail (fail-expected result)))))))

(defun parse-tag (tag parser)
  "Report fails as expecting the given tag instead of an element"
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

(defmacro defparser (name () &body (form))
  "Define a parser as a function.
   Enables other parsers to forward-reference it before it is defined."
  (with-gensyms (stream)
    `(defun ,name (,stream)
       (with-trace (,name)
         (funcall ,form ,stream)))))



;; Complex Parselets

(defun digit-parser (&optional (radix 10))
  "Consume a single digit and return its integer value."
  (check-type radix (integer 2 36))
  (parse-tag (cons :digit radix)
             (parse-map (lambda (ch)
                          (let ((code (char-code ch)))
                            (if (<= #.(char-code #\0) code #.(char-code #\9))
                                (- code #.(char-code #\0))
                                (+ 10 (- (char-code (char-upcase ch)) #.(char-code #\A))))))
                        (predicate-parser (rcurry #'digit-char-p radix)))))

(defun integer-parser (&optional (radix 10))
  "Consume one or more digits and return its integer value."
  (parse-tag (cons :integer radix)
             (parse-map (curry #'reduce (lambda (num dig) (+ (* num radix) dig)))
                        (parse-collect1 (digit-parser radix)))))
