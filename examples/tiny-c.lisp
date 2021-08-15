;;; tiny-c.lisp - Parsnip example parser for a tiny C

;;; Copyright 2021 Samuel Hunter <samuel (at) shunter (dot) xyz>
;;; BSD-3-Clause

(defpackage #:xyz.shunter.parsnip.examples.tiny-c
  (:documentation "Parsnip example parser for a tiny C")
  (:use #:cl #:xyz.shunter.parsnip)
  (:export #:parse-tiny-c-from-string))

(in-package #:xyz.shunter.parsnip.examples.tiny-c)



(defparameter *alpha*
  (parse-tag :alpha-char (predicate-parser #'alpha-char-p)))

(defparameter *alnum*
  (parse-tag :alphanumeric-char (predicate-parser #'alphanumericp)))

(defparameter *whitespace*
  (parse-reduce (constantly nil)
                (predicate-parser (lambda (c)
                                    (position c #(#\Space #\Tab #\Newline #\Return))))
                nil))

(defun wrap-ws (parser)
  (parse-prog2 *whitespace*
               parser
               *whitespace*))

(defmacro defwrapped (name parser)
  `(defparameter ,name (wrap-ws ,parser)))

;; punctuation
(defwrapped *comma* (char-parser #\,))
(defwrapped *paren-begin* (char-parser #\())
(defwrapped *paren-end* (char-parser #\)))
(defwrapped *brace-begin* (char-parser #\{))
(defwrapped *brace-end* (char-parser #\}))
(defwrapped *statement-end* (char-parser #\;))

(defun wrap-parens (parser)
  (parse-prog2 *paren-begin* parser *paren-end*))
(defun wrap-braces (parser)
  (parse-prog2 *brace-begin* parser *brace-end*))

;; keywords
(defwrapped *if* (string-parser "if"))
(defwrapped *else* (string-parser "else"))
(defwrapped *return* (string-parser "return"))

;; ident := alpha alnum*
(defparameter *ident*
  (parse-let ((head-char *alpha*)
              (tail-chars (parse-collect *alnum*)))
    (coerce (list* head-char tail-chars) 'string)))



;; stmt := block-stmt | expr-stmt | branch-stmt | return-stmt
(defparser stmt ()
  (parse-or 'block-stmt
            'branch-stmt
            'return-stmt
            'expr-stmt))

;; block-stmt := '{' stmt* '}'
(defparser block-stmt ()
  (wrap-braces
    (parse-map (lambda (stmts)
                 (list :block-stmt stmts))
               (parse-collect #'stmt))))

;; expr-stmt := expr ';'
(defparser expr-stmt ()
  (parse-map (lambda (expr)
               (list :expr-stmt expr))
             (parse-prog1 (parse-try 'expr)
                          *statement-end*)))

;; branch-stmt := 'if' '(' expr ')' stmt [ 'else' stmt ]
(defparser branch-stmt ()
  (parse-let ((if-kw (parse-try *if*))
              (condition (wrap-parens 'expr))
              (then-body #'stmt)
              (else-kw (parse-try *else*))
              (else-body #'stmt))
    (declare (ignore if-kw else-kw))
    (list :branch-stmt condition then-body else-body)))

;; return-stmt := 'return' expr ';'
(defparser return-stmt ()
  (parse-map
    (lambda (expr)
      (list :return-stmt expr))
    (parse-prog2 (parse-try *return*)
                 'expr
                 *statement-end*)))



;; args := [ expr ( ',' expr )* ]
(defparser args ()
  (parse-optional
    (parse-let ((head-expr 'expr)
                (tail-exprs (parse-collect
                              (parse-progn *comma*
                                           'expr))))
      (list* head-expr tail-exprs))))

;; primary-expr := '(' expr ')' | integer | ident [ '(' args ')' ]
(defparser primary-expr ()
  (parse-or (wrap-parens 'expr)
            (integer-parser)
            (parse-let ((name *ident*)
                        (call-args (parse-optional (wrap-parens #'args))))
              (if call-args
                  (list :call-expr name call-args)
                  name))))

;; mul-op := '*' | '/' | '%'
(defparameter *mul-op*
  (parse-or (wrap-ws (char-parser #\*))
            (wrap-ws (char-parser #\/))
            (wrap-ws (char-parser #\%))))

;; mul-expr := primary-expr [ mul-op mul-expr ]
(defparser mul-expr ()
  (parse-let ((lefthand #'primary-expr)
              (op-and-righthand (parse-optional
                                  (parse-let ((op *mul-op*)
                                              (righthand #'mul-expr))
                                    (list op righthand)))))
    (if op-and-righthand
        (list* :mul-expr lefthand op-and-righthand)
        lefthand)))

;; add-op := '+' | '-'
(defparameter *add-op*
  (parse-or (wrap-ws (char-parser #\+))
            (wrap-ws (char-parser #\-))))

;; add-expr := primary-expr [ add-op add-expr ]
(defparser add-expr ()
  (parse-let ((lefthand #'mul-expr)
              (op-and-righthand (parse-optional
                                  (parse-let ((op *add-op*)
                                              (righthand #'add-expr))
                                    (list op righthand)))))
    (if op-and-righthand
        (list* :add-expr lefthand op-and-righthand)
        lefthand)))

;; cmp-op := '==' | '!='
(defparameter *cmp-op*
  (parse-or (parse-try (wrap-ws (string-parser "==")))
            (parse-try (wrap-ws (string-parser "!=")))))

;; cmp-expr := primary-expr [ cmp-op cmp-expr ]
(defparser cmp-expr ()
  (parse-let ((lefthand #'add-expr)
              (op-and-righthand (parse-optional
                                  (parse-let ((op (parse-try *cmp-op*))
                                              (righthand #'cmp-expr))
                                    (list op righthand)))))
    (if op-and-righthand
        (list* :cmp-expr lefthand op-and-righthand)
        lefthand)))

;; expr := cmp-expr
(defparser expr () #'cmp-expr)



;; params := [ ident ( ',' ident )* ]
(defparameter *params*
  (parse-optional
    (parse-let ((head-param *ident*)
                (tail-params (parse-collect
                               (parse-prog2 *comma*
                                            *ident*))))
      (list* head-param tail-params))))

;; decl := ident '(' params ')' stmt
(defparameter *decl*
  (parse-let ((name *ident*)
              (params (wrap-parens *params*))
              (body #'stmt))
    (list :function name params body)))

;; program := decl+ EOF
(defparser program ()
  (parse-prog1 (parse-collect (wrap-ws *decl*))
               *whitespace*
               (eof-parser)))



(defun parse-tiny-c (stream)
  (parse #'program stream))

(defun parse-tiny-c-from-string (string)
  (parse #'program (coerce string 'list)))
