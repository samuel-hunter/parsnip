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
           #:parse-skip-many

           #:parse-try
           #:parse-tag

           #:parse-let
           #:defparser

           #:digit-parser
           #:integer-parser))

(in-package #:xyz.shunter.parsnip)



(deftype function-designator ()
  '(or function symbol))

(deftype parser ()
  'function-designator)

;; Toplevel evaluation converts results into something a little more
;; front-facing for library consumers.

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

(defun signal-failure (stream expected return-trace)
  "Signal an error depending on the given fail."
  (error 'parser-error
         :stream stream
         :expected expected
         :return-trace return-trace))

(defun parse (parser stream)
  "Run a parser through a given stream and raise any fails as a condition."
  (check-type parser parser)
  (flet ((take-value (stream value)
           (declare (ignore stream))
           value))
    (funcall parser stream
             #'take-value
             #'take-value
             #'signal-failure
             #'signal-failure)))



;; Primitive Parselets

(defun char-parser (char)
  "Consume and return the given character."
  (check-type char character)
  (lambda (stream eok cok efail cfail)
    (declare (ignore eok cfail))
    (let ((actual (peek-char nil stream nil)))
      (cond
        ((null actual)
         (funcall efail stream char ()))
        ((char= char actual)
         (funcall cok stream (read-char stream)))
        (t (funcall efail stream char ()))))))

(defun predicate-parser (predicate)
  "Consume and return a character that passes the given predicate."
  (check-type predicate function-designator)
  (lambda (stream eok cok efail cfail)
    (declare (ignore eok cfail))
    (let ((actual (peek-char nil stream nil)))
      (cond
        ((null actual)
         (funcall efail stream predicate ()))
        ((funcall predicate actual)
         (funcall cok stream (read-char stream)))
        (t (funcall efail stream predicate ()))))))

(defun string-parser (string)
  "Consume and return the given text. May partially parse on failure."
  (check-type string string)
  (lambda (stream eok cok efail cfail)
    (declare (ignore eok))
    (let* ((actual (make-string (length string)))
           (chars-read (read-sequence actual stream)))
      (cond
        ((zerop chars-read)
         (funcall efail stream string ()))
        ((string= actual string)
         (funcall cok stream actual))
        (t (funcall cfail stream string ()))))))

(defun eof-parser (&optional value)
  "Return the given value (or NIL) if at EOF."
  (lambda (stream eok cok efail cfail)
    (declare (ignore cok cfail))
    (if (null (peek-char nil stream nil))
        (funcall eok stream value)
        (funcall efail stream :eof ()))))



;; Combinators

(defun parse-map (function &rest parsers)
  "Run the parsers in sequence and apply the given function to all results."
  (lambda (stream eok cok efail cfail)
    (labels ((iter (stream args parsers-left eok)
               (if (null parsers-left)
                   (funcall eok stream (apply function (nreverse args)))
                   (funcall
                     (first parsers-left) stream
                     ;; eok
                     (lambda (stream value)
                       (iter stream (cons value args) (rest parsers-left)
                             eok))
                     ;; cok
                     (lambda (stream value)
                       (iter stream (cons value args) (rest parsers-left)
                             cok))
                     ;; efail
                     efail
                     ;; cfail
                     cfail))))
      (iter stream () parsers eok))))

(defun parse-progn (&rest parsers)
  "Run the parsers in sequence and return the last result."
  (lambda (stream eok cok efail cfail)
    (labels ((iter (stream arg parsers-left eok efail)
               (if (null parsers-left)
                   (funcall eok stream arg)
                   (funcall
                     (first parsers-left) stream
                     ;; eok
                     (rcurry #'iter (rest parsers-left) eok efail)
                     ;; cok
                     (rcurry #'iter (rest parsers-left) cok cfail)
                     ;; efail
                     efail
                     ;; cfail
                     cfail))))
      (iter stream nil parsers eok efail))))

(defun parse-prog1 (first-parser &rest parsers)
  "Run the parsers in sequence and return the last result."
  (parse-map (lambda (first last)
               (declare (ignore last))
               first)
             first-parser
             (apply 'parse-progn parsers)))

(defun parse-prog2 (first-parser second-parser &rest parsers)
  "Run the parsers in sequence and return the second result."
  (parse-prog1 (parse-progn first-parser second-parser)
               (apply 'parse-progn parsers)))

(defun parse-collect (parser)
  "Run until failure, and then return the collected results."
  (check-type parser parser)
  (lambda (stream eok cok efail cfail)
    (declare (ignore efail))
    (labels ((iter (stream args eok)
               (funcall
                 parser stream
                 ;; eok
                 (lambda (stream value)
                   (declare (ignore stream value))
                   (error "Unexpected eok in parse-collect"))
                 ;; cok
                 (lambda (stream value)
                   (iter stream (cons value args) cok))
                 ;; efail
                 (lambda (stream expected return-trace)
                   (declare (ignore expected return-trace))
                   (funcall eok stream (nreverse args)))
                 ;; cfail
                 cfail)))
      (iter stream () eok))))

(defun parse-collect1 (parser)
  "Run until failure, and then return at LEAST one collected result."
  (check-type parser parser)
  (parse-map #'cons parser (parse-collect parser)))

(defun parse-collect-string (parser)
  "Run until failure, and then return the collected characters as a string."
  (check-type parser parser)
  (lambda (stream eok cok efail cfail)
    (declare (ignore efail))
    (let ((string (make-array 0
                              :element-type 'character
                              :adjustable t
                              :fill-pointer 0)))
      (labels ((iter (stream eok)
                 (funcall
                   parser stream
                   ;; eok
                   (lambda (stream value)
                     (declare (ignore stream value))
                     (error "Unexpected eok in parse-collect-string"))
                   ;; cok
                   (lambda (stream value)
                     (vector-push-extend value string)
                     (iter stream cok))
                   ;; efail
                   (lambda (stream expected return-trace)
                     (declare (ignore expected return-trace))
                     (funcall eok stream string))
                   ;; cfail
                   cfail)))
        (iter stream eok)))))

(defun parse-reduce (function parser initial-value)
  "Run until failure, and then reduce the results into one value."
  (check-type function function-designator)
  (check-type parser parser)
  (lambda (stream eok cok efail cfail)
    (declare (ignore efail))
    (labels ((iter (stream arg eok)
               (funcall
                 parser stream
                 ;; eok
                 (lambda (stream value)
                   (declare (ignore stream value))
                   (error "Unexpected eok on parse-reduce"))
                 ;; cok
                 (lambda (stream value)
                   (iter stream (funcall function arg value) cok))
                 ;; efail
                 (lambda (stream expected return-trace)
                   (declare (ignore expected return-trace))
                   (funcall eok stream arg))
                 ;; cfail
                 cfail)))
      (iter stream initial-value eok))))

(defun parse-take (times parser)
  "Run and collect EXACTLY the given number of results."
  (check-type times (integer 0 *))
  (check-type parser parser)
  (lambda (stream eok cok efail cfail)
    (labels ((iter (stream times-left args eok efail)
               (if (zerop times-left)
                   (funcall eok stream (nreverse args))
                   (funcall
                     parser stream
                     ;; eok
                     (lambda (stream value)
                       (declare (ignore stream value))
                       (error "Unexected eok in parse-take"))
                     ;; cok
                     (lambda (stream value)
                       (iter stream (1- times-left)
                             (cons value args)
                             cok cfail))
                     ;; efail
                     efail
                     ;; cfail
                     cfail))))
      (iter stream times () eok efail))))

(defun parse-skip-many (parser)
  "Keep parsing until failure and pretend no input was consumed."
  (check-type parser parser)
  (lambda (stream eok cok efail cfail)
    (declare (ignore cok))
    (funcall (parse-reduce (constantly nil)
                           parser nil)
             stream
             eok eok
             efail cfail)))

(defun parse-or (&rest parsers)
  "Attempt each parser in order until one succeeds. Passes through any
   partial-parse fail."
  (lambda (stream eok cok efail cfail)
    (labels ((iter (stream parsers-left expected-elements)
               (if (null parsers-left)
                   (funcall efail stream (nreverse expected-elements) ())
                   (funcall
                     (first parsers-left) stream
                     eok cok
                     ;; efail
                     (lambda (stream expected return-trace)
                       (declare (ignore return-trace))
                       (iter stream (rest parsers-left)
                             (cons expected expected-elements)))
                     ;; cfail
                     cfail))))
      (iter stream parsers ()))))

(defun parse-optional (parser &optional default)
  "Resume from a failure with a default value."
  (check-type parser parser)
  (lambda (stream eok cok efail cfail)
    (declare (ignore efail))
    (funcall parser stream
             eok cok
             ;; efail
             (lambda (stream expected return-trace)
               (declare (ignore expected return-trace))
               (funcall eok stream default))
             ;; cfail
             cfail)))

(defun parse-try (parser)
  "Try to rewind the stream on any partial-parse failure. Only works on
   seekable streams."
  (lambda (stream eok cok efail cfail)
    (declare (ignore cfail))
    (let ((old-position (file-position stream)))
      (funcall parser stream
               eok cok
               efail
               ;; cfail
               (lambda (stream expected return-trace)
                 (file-position stream old-position)
                 (funcall efail stream expected return-trace))))))

(defun parse-tag (tag parser)
  "Report fails as expecting the given tag instead of an element"
  (check-type parser parser)
  (flet ((replace-expected (fail stream expected return-trace)
           (declare (ignore expected))
           (funcall fail stream tag return-trace)))
    (lambda (stream eok cok efail cfail)
      (funcall parser stream
        eok cok
        (curry #'replace-expected efail)
        (curry #'replace-expected cfail)))))

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

(defun parse-trace (name parser)
  (check-type parser parser)
  (flet ((add-to-trace (fail stream expected return-trace)
           (funcall fail stream expected (cons name return-trace))))
    (lambda (stream eok cok efail cfail)
      (funcall parser stream
        eok cok
        ;; efail
        (curry #'add-to-trace efail)
        ;; cfail
        (curry #'add-to-trace cfail)))))

(defmacro defparser (name () &body (form))
  "Define a parser as a function.
   Enables other parsers to forward-reference it before it is defined."
  (with-gensyms (parser stream eok cok efail cfail)
    `(let ((,parser (parse-trace ',name ,form)))
       (defun ,name (,stream ,eok ,cok ,efail ,cfail)
         (funcall ,parser ,stream ,eok ,cok ,efail ,cfail)))))



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
