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
           #:parser-error-line
           #:parser-error-column
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

(defstruct (state (:copier nil)
                  (:predicate nil))
  (stream nil :read-only t)
  (line 1)
  (column 0))

;; Toplevel evaluation converts results into something a little more
;; front-facing for library consumers.

(define-condition parser-error (stream-error)
  ((line :initarg :line :reader parser-error-line)
   (column :initarg :column :reader parser-error-column)
   (expected :initarg :expected :reader parser-error-expected)
   (return-trace :initarg :return-trace :reader parser-error-return-trace))
  (:report (lambda (condition stream)
             (with-accessors ((err-stream stream-error-stream)
                              (line parser-error-line)
                              (column parser-error-column)
                              (expected parser-error-expected)) condition
               (format stream "~A:~D:~D: Expected ~S on ~S"
                       (ignore-errors (pathname err-stream))
                       line
                       column
                       expected
                       err-stream)))))

(defun signal-failure (state expected return-trace)
  "Signal an error depending on the given fail."
  (with-accessors ((stream state-stream)
                   (line state-line)
                   (column state-column)) state
    (error 'parser-error
           :stream stream
           :line line
           :column column
           :expected expected
           :return-trace return-trace)))

(defun parse (parser stream)
  "Run a parser through a given stream and raise any fails as a condition."
  (check-type parser parser)
  (flet ((take-value (state value)
           (declare (ignore state))
           value))
    (funcall parser (make-state :stream stream)
             #'take-value
             #'take-value
             #'signal-failure
             #'signal-failure)))



(defun new-state (state line column)
  (make-state :stream (state-stream state)
              :line line
              :column column))

(defparameter +tab-width+ 8)

(defun advance-state (state c)
  (with-accessors ((line state-line)
                   (column state-column)) state
    (cond
      ((char= c #\Newline)
       (new-state state (1+ line) 0))
      ((char= c #\Tab)
       (new-state state line
                  (+ column +tab-width+
                     (- (mod (1- column) +tab-width+)))))
      (t
       (new-state state line (1+ column))))))

;; Primitive Parselets

(defun char-parser (char)
  "Consume and return the given character."
  (check-type char character)
  (lambda (state eok cok efail cfail)
    (declare (ignore eok cfail))
    (let ((stream (state-stream state)))
      (let ((actual (peek-char nil stream nil)))
        (cond
          ((null actual)
           (funcall efail state char ()))
          ((char= char actual)
           (read-char stream)
           (funcall cok (advance-state state actual)
                    actual))
          (t (funcall efail state char ())))))))

(defun predicate-parser (predicate)
  "Consume and return a character that passes the given predicate."
  (check-type predicate function-designator)
  (lambda (state eok cok efail cfail)
    (declare (ignore eok cfail))
    (let ((stream (state-stream state)))
      (let ((actual (peek-char nil stream nil)))
        (cond
          ((null actual)
           (funcall efail state predicate ()))
          ((funcall predicate actual)
           (read-char stream)
           (funcall cok (advance-state state actual)
                    actual))
          (t (funcall efail state predicate ())))))))

(defun string-parser (string)
  "Consume and return the given text. May partially parse on failure."
  (check-type string string)
  (lambda (state eok cok efail cfail)
    (declare (ignore eok))
    (let ((stream (state-stream state)))
      (let* ((actual (make-string (length string)))
             (chars-read (read-sequence actual stream)))
        (cond
          ((zerop chars-read)
           (funcall efail state string ()))
          ((string= actual string)
           (funcall cok
                    (reduce #'advance-state actual :initial-value state)
                    actual))
          (t (funcall cfail state string ())))))))

(defun eof-parser (&optional value)
  "Return the given value (or NIL) if at EOF."
  (lambda (state eok cok efail cfail)
    (declare (ignore cok cfail))
    (let ((stream (state-stream state)))
      (if (null (peek-char nil stream nil))
          (funcall eok state value)
          (funcall efail state :eof ())))))



;; Combinators

(defun parse-map (function &rest parsers)
  "Run the parsers in sequence and apply the given function to all results."
  (lambda (state eok cok efail cfail)
    (labels ((iter (state args parsers-left eok)
               (if (null parsers-left)
                   (funcall eok state (apply function (nreverse args)))
                   (funcall
                     (first parsers-left) state
                     ;; eok
                     (lambda (state value)
                       (iter state (cons value args) (rest parsers-left)
                             eok))
                     ;; cok
                     (lambda (state value)
                       (iter state (cons value args) (rest parsers-left)
                             cok))
                     ;; efail
                     efail
                     ;; cfail
                     cfail))))
      (iter state () parsers eok))))

(defun parse-progn (&rest parsers)
  "Run the parsers in sequence and return the last result."
  (lambda (state eok cok efail cfail)
    (labels ((iter (state arg parsers-left eok efail)
               (if (null parsers-left)
                   (funcall eok state arg)
                   (funcall
                     (first parsers-left) state
                     ;; eok
                     (rcurry #'iter (rest parsers-left) eok efail)
                     ;; cok
                     (rcurry #'iter (rest parsers-left) cok cfail)
                     ;; efail
                     efail
                     ;; cfail
                     cfail))))
      (iter state nil parsers eok efail))))

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
  (lambda (state eok cok efail cfail)
    (declare (ignore efail))
    (labels ((iter (state args eok)
               (funcall
                 parser state
                 ;; eok
                 (lambda (state value)
                   (declare (ignore state value))
                   (error "Unexpected eok in parse-collect"))
                 ;; cok
                 (lambda (state value)
                   (iter state (cons value args) cok))
                 ;; efail
                 (lambda (state expected return-trace)
                   (declare (ignore expected return-trace))
                   (funcall eok state (nreverse args)))
                 ;; cfail
                 cfail)))
      (iter state () eok))))

(defun parse-collect1 (parser)
  "Run until failure, and then return at LEAST one collected result."
  (check-type parser parser)
  (parse-map #'cons parser (parse-collect parser)))

(defun parse-collect-string (parser)
  "Run until failure, and then return the collected characters as a string."
  (check-type parser parser)
  (lambda (state eok cok efail cfail)
    (declare (ignore efail))
    (let ((string (make-array 0
                              :element-type 'character
                              :adjustable t
                              :fill-pointer 0)))
      (labels ((iter (state eok)
                 (funcall
                   parser state
                   ;; eok
                   (lambda (state value)
                     (declare (ignore state value))
                     (error "Unexpected eok in parse-collect-string"))
                   ;; cok
                   (lambda (state value)
                     (vector-push-extend value string)
                     (iter state cok))
                   ;; efail
                   (lambda (state expected return-trace)
                     (declare (ignore expected return-trace))
                     (funcall eok state string))
                   ;; cfail
                   cfail)))
        (iter state eok)))))

(defun parse-reduce (function parser initial-value)
  "Run until failure, and then reduce the results into one value."
  (check-type function function-designator)
  (check-type parser parser)
  (lambda (state eok cok efail cfail)
    (declare (ignore efail))
    (labels ((iter (state arg eok)
               (funcall
                 parser state
                 ;; eok
                 (lambda (state value)
                   (declare (ignore state value))
                   (error "Unexpected eok on parse-reduce"))
                 ;; cok
                 (lambda (state value)
                   (iter state (funcall function arg value) cok))
                 ;; efail
                 (lambda (state expected return-trace)
                   (declare (ignore expected return-trace))
                   (funcall eok state arg))
                 ;; cfail
                 cfail)))
      (iter state initial-value eok))))

(defun parse-take (times parser)
  "Run and collect EXACTLY the given number of results."
  (check-type times (integer 0 *))
  (check-type parser parser)
  (lambda (state eok cok efail cfail)
    (labels ((iter (state times-left args eok efail)
               (if (zerop times-left)
                   (funcall eok state (nreverse args))
                   (funcall
                     parser state
                     ;; eok
                     (lambda (state value)
                       (declare (ignore state value))
                       (error "Unexected eok in parse-take"))
                     ;; cok
                     (lambda (state value)
                       (iter state (1- times-left)
                             (cons value args)
                             cok cfail))
                     ;; efail
                     efail
                     ;; cfail
                     cfail))))
      (iter state times () eok efail))))

(defun parse-skip-many (parser)
  "Keep parsing until failure and pretend no input was consumed."
  (check-type parser parser)
  (lambda (state eok cok efail cfail)
    (declare (ignore cok))
    (funcall (parse-reduce (constantly nil)
                           parser nil)
             state
             eok eok
             efail cfail)))

(defun parse-or (&rest parsers)
  "Attempt each parser in order until one succeeds. Passes through any
   partial-parse fail."
  (lambda (state eok cok efail cfail)
    (labels ((iter (state parsers-left expected-elements)
               (if (null parsers-left)
                   (funcall efail state (nreverse expected-elements) ())
                   (funcall
                     (first parsers-left) state
                     eok cok
                     ;; efail
                     (lambda (state expected return-trace)
                       (declare (ignore return-trace))
                       (iter state (rest parsers-left)
                             (cons expected expected-elements)))
                     ;; cfail
                     cfail))))
      (iter state parsers ()))))

(defun parse-optional (parser &optional default)
  "Resume from a failure with a default value."
  (check-type parser parser)
  (lambda (state eok cok efail cfail)
    (declare (ignore efail))
    (funcall parser state
             eok cok
             ;; efail
             (lambda (state expected return-trace)
               (declare (ignore expected return-trace))
               (funcall eok state default))
             ;; cfail
             cfail)))

(defun parse-try (parser)
  "Try to rewind the state on any partial-parse failure. Only works on
   seekable states."
  (lambda (state eok cok efail cfail)
    (declare (ignore cfail))
    (let* ((stream (state-stream state))
           (old-position (file-position stream)))
      (funcall parser state
               eok cok
               efail
               ;; cfail
               (lambda (state* expected return-trace)
                 (declare (ignore state*))
                 (file-position (state-stream state) old-position)
                 (funcall efail state expected return-trace))))))

(defun parse-tag (tag parser)
  "Report fails as expecting the given tag instead of an element"
  (check-type parser parser)
  (flet ((replace-expected (fail stream expected return-trace)
           (declare (ignore expected))
           (funcall fail stream tag return-trace)))
    (lambda (state eok cok efail cfail)
      (funcall parser state
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

(defun add-to-trace (name fail old-state state expected return-trace)
  (funcall fail state expected
           (cons (list name (state-line old-state) (state-column old-state)) return-trace)))

(defmacro defparser (name () &body (form))
  "Define a parser as a function.
   Enables other parsers to forward-reference it before it is defined."
  `(let ((parser ,form))
     (defun ,name (state eok cok efail cfail)
       (funcall parser state
                eok cok
                (curry #'add-to-trace ',name efail state)
                (curry #'add-to-trace ',name cfail state)))))



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
