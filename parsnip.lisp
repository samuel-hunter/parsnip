;;; parsnip.lisp - Parsnip library implementation

;;; Copyright 2021 Samuel Hunter <samuel (at) shunter (dot) xyz>
;;; BSD-3-Clause

(defpackage #:xyz.shunter.parsnip
  (:documentation "Monadic parser combinator library")
  (:nicknames #:parsnip)
  (:use #:cl)
  (:import-from #:alexandria
                #:curry
                #:rcurry)
  (:export #:parser-error
           #:parser-error-line
           #:parser-error-column
           #:parser-error-expected
           #:parser-error-return-trace

           #:ok
           #:fail
           #:char-if
           #:char-of
           #:char-in
           #:eof

           #:flatmap
           #:let!
           #:handle
           #:handle-rewind

           #:progn!
           #:prog1!
           #:prog2!
           #:or!

           #:collect
           #:collect1
           #:collect-into-string
           #:sep

           #:reduce!
           #:skip
           #:skip-many

           #:digit
           #:natural

           #:defparser

           #:parse))

(in-package #:xyz.shunter.parsnip)



(defstruct (parse-stream (:constructor parse-stream (stream))
                         (:conc-name pstream-)
                         (:copier nil)
                         (:predicate nil))
  (stream nil :read-only t)
  (line 1)
  (column 0))

(defun save (pstream)
  (with-accessors ((stream pstream-stream)
                   (line pstream-line)
                   (column pstream-column)) pstream
    (list (file-position stream)
          line
          column)))

(defun rewind (pstream snapshot)
  (with-accessors ((stream pstream-stream)
                   (line pstream-line)
                   (column pstream-column)) pstream
    (destructuring-bind (position line* column*) snapshot
      (file-position stream position)
      (setf line line*
            column column*))))

(defun advance-pstream (pstream c)
  (with-accessors ((line pstream-line)
                   (column pstream-column)) pstream
    (if (char= c #\Newline)
        (setf line (1+ line)
              column 0)
        (setf column (1+ column))))
  pstream)

(defun peek (pstream)
  (peek-char nil (pstream-stream pstream) nil))

(defun consume (pstream)
  (advance-pstream pstream (read-char (pstream-stream pstream))))

(defmacro let@ ((&rest bindings) &body body)
  (let ((names (mapcar #'first bindings))
        (values (mapcar #'second bindings)))
    `(labels ((@ ,names ,@body))
       (@ ,@values))))



;; Primary parsers

(defun ok (value)
  "Return a parser that consumes nothing and returns the given value."
  (lambda (pstream eok cok efail cfail)
    (declare (ignore cok efail cfail))
    (funcall eok pstream value)))

(defun fail (expected &optional trace)
  "Return a parser that consumes nothing and fails, reporting the expected value."
  (lambda (pstream eok cok efail cfail)
    (declare (ignore eok cok cfail))
    (funcall efail pstream expected trace)))

(defun char-if (predicate &optional message)
  "Return a parser that consumes a character that satisfies the given predicate."
  (unless message
    (setf message (format nil "Satisfies ~S" predicate)))

  (lambda (pstream eok cok efail cfail)
    (declare (ignore eok cfail))
    (let ((actual (peek pstream)))
      (if (and actual (funcall predicate actual))
          (funcall cok (consume pstream) actual)
          (funcall efail pstream message ())))))

(defun char-of (char &optional message)
  "Return a parser that consumes the given character."
  (check-type char character)
  (char-if (curry #'char= char) (or message char)))

(defun char-in (charbag &optional message)
  "Return a parser that consumes a character that's only in the given charbag."
  (char-if (rcurry #'position charbag)
           (or message (format nil "One of ~S" charbag))))

(defun eof (&optional value)
  "Return a parser that consumes nothing and returns the given value (or nil) if the input stream is exhausted."
  (lambda (pstream eok cok efail cfail)
    (declare (ignore cok cfail))
    (if (peek pstream)
        (funcall efail pstream "EOF" ())
        (funcall eok pstream value))))

;; Parser combinators

(defun flatmap (function parser)
  "Return a new parser that applies the given function to the parser's result, and then runs the parser the function returns.
This function forms the basis of stringing multiple parsers together."
  (lambda (pstream eok cok efail cfail)
    (funcall parser pstream
             ;; eok
             (lambda (pstream* value)
               (funcall (funcall function value)
                        pstream* eok cok efail cfail))
             ;; cok
             (lambda (pstream* value)
               (funcall (funcall function value)
                        pstream* cok cok cfail cfail))
             ;; efail
             efail
             ;; cfail
             cfail)))

(defmacro let! ((&rest bindings) &body body)
  "Return ap arser that runs all given parsers, binds them all to their variables, evaluates the body, and then runs the parser the body returns."
  (reduce (lambda (binding body)
            (let ((name (first binding))
                  (value (second binding)))
              `(flatmap (lambda (,name) ,body)
                        ,value)))
          bindings
          :from-end t
          :initial-value `(progn ,@body)))

(defun handle (parser handler)
  "Return a new parser that, on failure, applies the handler function to the parser's expected value and parse trace (as a list), and then runs the parser the handler returns.
HANDLE does not handle partial-parse failures, which can be recovered from via HANDLE-REWIND."
  (lambda (pstream eok cok efail cfail)
    (funcall parser pstream
             ;; eok
             eok
             ;; cok
             cok
             ;; efail
             (lambda (pstream* expected trace)
               (funcall (funcall handler expected trace)
                        pstream* eok cok efail cfail))
             ;; cfail
             cfail)))

(defun handle-rewind (parser handler)
  "Return a new parser that saves the stream's current position and, on failure, rewinds the stream, applies the handler function to the parser's expected value and parse trace (as a list), and then runs the parser the handler returns.
HANDLE-REWIND only functions if the parser is given a seekable stream."
  (lambda (pstream eok cok efail cfail)
    (let ((snapshot (save pstream)))
      (funcall parser pstream
               ;; eok
               eok
               ;; cok
               cok
               ;; efail
               (lambda (pstream* expected trace)
                 (funcall (funcall handler expected trace)
                          pstream* eok cok efail cfail))
               ;; cfail
               (lambda (pstream* expected trace)
                 (funcall (funcall handler expected trace)
                          (rewind pstream* snapshot) eok cok efail cfail))))))

(defun progn! (&rest parsers)
  "Return a parser that strings together all given parsers and returns the last parser's result."
  (reduce (lambda (parser next)
            (flatmap (constantly next) parser))
          parsers
          :from-end t))

(defun prog1! (first &rest parsers)
  "Return a parser that strings together all given parsers and returns the first parser's result."
  (if parsers
      (flatmap (lambda (value)
                 (flatmap (constantly (ok value))
                          (apply #'progn! parsers)))
               first)
      first))

(defun prog2! (first second &rest parsers)
  "Return a parser that strings together all given parsers and returns the second parser's result."
  (if parsers
      (prog1! (progn! first second)
              (apply #'progn! parsers))
      (progn! first second)))

(defun or! (&rest parsers)
  "Return a parser that tries each given parser in order (until a partial-parse failure) and returns the result of the first successful parse."
  (reduce (lambda (parser next)
            (handle parser
                    (constantly next)))
          parsers
          :from-end t))

(defun collect (parser)
  "Return a parser that runs the given parser until failure, and collects all results into a list."
  (let@ ((items ()))
    (handle
      (let! ((item parser))
        (@ (cons item items)))
      (lambda (expected trace)
        (declare (ignore expected trace))
        (ok (nreverse items))))))

(defun collect1 (parser)
  "Return a parser that runs the given parser once, keeps parsing until failure, and then collects all results into a list."
  (let@ ((items ())
         (parsed nil))
    (handle
      (let! ((item parser))
        (@ (cons item items)
           t))
      (lambda (expected trace)
        (if parsed
            (ok (nreverse items))
            (fail expected trace))))))

(defun collect-into-string (char-parser)
  "Return a parser that runs the given character parser until failure, and collects all characters into a string."
  (let ((out (make-string-output-stream)))
    (let@ ()
      (handle
        (let! ((c char-parser))
          (write-char c out)
          (@))
        (lambda (expected trace)
          (declare (ignore expected trace))
          (ok (get-output-stream-string out)))))))

(defun sep (value-parser sep-parser)
  "Return a parser that accepts a sequence of VALUE-PARSER input separated by SEP-PARSER input; such as values separated by commas."
  (let! ((first value-parser)
         (rest (collect (progn! sep-parser
                                value-parser))))
    (ok (list* first rest))))

(defun reduce! (function parser &key (initial-value nil ivp))
  "Return a parser that keeps running until failure, and reduces its results into one value.
If INITIAL-VALUE is supplied, the parser may succeed without parsing by returning INITIAL-VALUE."
  (let! ((initial-value (if ivp (ok initial-value) parser)))
    (let@ ((result initial-value))
      (handle
        (let! ((obj parser))
          (@ (funcall function result obj)))
        (lambda (expected trace)
          (declare (ignore expected trace))
          (ok result))))))

(defun skip (parser)
  "Parse and pretend no input was consumed."
  (lambda (pstream eok cok efail cfail)
    (declare (ignore cok cfail))
    (funcall parser pstream
             eok eok
             efail efail)))

(defun skip-many (parser)
  "Keep parsing until failure and pretend no input was consumed."
  (let@ ()
    (handle (flatmap (lambda (value)
                       (declare (ignore value))
                       (@))
                     (skip parser))
            (constantly (ok nil)))))

;; Toplevel parsers

(defun digit (&optional (radix 10))
  "Consume and return the number value of a digit."
  (let! ((d (char-if (rcurry #'digit-char-p radix))))
    (ok (digit-char-p d radix))))

(defun natural (&optional (radix 10))
  "Consume and return a natural number."
  (reduce! (lambda (number d)
             (+ (* number radix) d))
           (digit radix)))

;; Misc. helpers

(defmacro defparser (name () &body (form))
  "Define a parser as a function. It can then be referenced as a function designator."
  `(let ((parser ,form))
     (defun ,name (pstream eok cok efail cfail)
       (funcall parser pstream
                eok cok efail cfail))))



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
                       err-stream))))
  (:documentation
    "If a parser fails to read text, it signals a parser-error, containing a stream,
its expected value, and a return trace of parsers."
    )
  )

(defun signal-failure (pstream expected return-trace)
  "Signal an error depending on the given fail."
  (with-accessors ((stream pstream-stream)
                   (line pstream-line)
                   (column pstream-column)) pstream
    (error 'parser-error
           :stream stream
           :line line
           :column column
           :expected expected
           :return-trace return-trace)))

(defun parse (parser stream)
  "Run a parser through a given stream and raise any failures as a PARSER-ERROR."
  (flet ((take-value (pstream value)
           (declare (ignore pstream))
           value))
    (funcall parser (parse-stream stream)
             #'take-value
             #'take-value
             #'signal-failure
             #'signal-failure)))
