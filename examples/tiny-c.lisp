;;; tiny-c.lisp - Parsnip example tiny C parser

;;; Copyright 2022 Samuel Hunter <samuel (at) shunter (dot) xyz>
;;; BSD-3-Clause

(defpackage #:xyz.shunter.parsnip.examples.tiny-c
  (:documentation "Parsnip example tiny C parser")
  (:use #:cl #:xyz.shunter.parsnip)
  (:export
    #:parse-tiny-c
    #:parse-tiny-c-from-string))

(in-package #:xyz.shunter.parsnip.examples.tiny-c)



(defparameter *whitespace*
  (skip-many (char-in #(#\Space #\Tab #\Newline #\Return))))

(defun trim-ws (parser)
  (prog2! *whitespace* parser *whitespace*))

(defmacro deftrimmed (name &body (parser))
  `(defparameter ,name (trim-ws ,parser)))

(defun kw (string)
  (try! (let! ((ident 'ident))
          (if (string= ident string)
              (ok ident)
              (fail (format nil "keyword ~S" string))))))

(deftrimmed *block-begin* (char-of #\{))
(deftrimmed *block-end* (char-of #\}))
(deftrimmed *lparen* (char-of #\())
(deftrimmed *rparen* (char-of #\)))
(deftrimmed *comma* (char-of #\,))
(deftrimmed *semicolon* (char-of #\;))

(deftrimmed *if* (kw "if"))
(deftrimmed *while* (kw "while"))
(deftrimmed *return* (kw "return"))
(deftrimmed *assign* (char-of #\=))
(deftrimmed *plus* (char-of #\+))
(deftrimmed *minus* (char-of #\-))
(deftrimmed *mul* (char-of #\*))
(deftrimmed *div* (char-of #\/))
(deftrimmed *gtr* (try! (char-of #\>)))
(deftrimmed *lss* (try! (char-of #\<)))
(deftrimmed *equ* (try! (string-of "==")))
(deftrimmed *neq* (try! (string-of "!=")))

;; ident
(defparser ident ()
  (let! ((first (char-if #'alpha-char-p))
         (rest (collect-into-string
                 (char-if #'alphanumericp))))
    (ok (format nil "~C~A" first rest))))

;; primary := ident | num | "(" expr ")"
(defparser primary ()
  (or! 'ident
       (natural)
       (prog2! *lparen* 'expr *rparen*)))

;; call := call "(" args ")" | primary
;; i.e.    primary [ "(" args ")" ] +
(defparser call ()
  (reduce! (lambda (left args)
             (list* :call left args))
           (prog2! *lparen*
                   (or! (sep 'expr *comma*)
                        (ok ()))
                   *rparen*)
           :initial-parser 'primary))

(defun binary (operand op)
  (reduce! (lambda (left bin)
             (destructuring-bind (op right) bin
               (list op left right)))
           (let! ((op op)
                  (right operand))
             (ok (list op right)))
           :initial-parser operand))

;; term := term ("*" | "/") unary
;; i.e.    unary [ ("*" | "/") unary ] +
(defparser term ()
  (binary 'call (or! *mul* *div*)))

;; sum := sum ("+" | "-") term | term
;; i.e.   term [ ("+" | "-") term ] +
(defparser sum ()
  (binary 'term (or! *plus* *minus*)))

;; cmp := cmp (">" | "<" | "==" | "!=") | term
;; i.e.   term [ (">" | "<" | "==" | "!=") term ] +
(defparser cmp ()
  (binary 'sum (or! *gtr* *lss* *equ* *neq*)))

;; asg := cmp [ "=" asg ]
(defparser asg ()
  (let! ((dest 'cmp)
         (src (or! (prog2! *assign* 'asg)
                   (ok nil))))
    (ok (if src
            (list :assign dest src)
            dest))))

;; expr := sum
(defparser expr ()
  'asg)

;; if-stmt := "if" "(" expr ")" stmt
(defparser if-stmt ()
  (let! ((conditional (prog2! (progn! *if* *lparen*)
                              'expr
                              *rparen*))
         (body 'stmt))
    (ok (list :if conditional body))))

;; while-stmt := "while" "(" expr ")" stmt
(defparser while-stmt ()
  (let! ((conditional (prog2! (progn! *while*
                                      *lparen*)
                              'expr
                              *rparen*))
         (body 'stmt))
    (ok (list :while conditional body))))

;; expr-stmt := expr ";"
(defparser expr-stmt ()
  (let! ((expr (prog1! 'expr *semicolon*)))
    (ok (list :expr expr))))

;; return-stmt := "return" expr ";"
(defparser return-stmt ()
  (let! ((expr (prog2! *return*
                       'expr
                       *semicolon*)))
    (ok (list :return expr))))

;; block-stmt := "{" stmt* "}"
(defparser block-stmt ()
  (let! ((stmts (prog2! *block-begin*
                        (collect 'stmt)
                        *block-end*)))
    (ok (list* :block stmts))))

;; stmt := if-stmt | while-stmt | expr-stmt | return-stmt | block-stmt
(defparser stmt ()
  (or! 'block-stmt
       'if-stmt
       'while-stmt
       'return-stmt
       'expr-stmt))

;; params := [ ident [ "," ident ] + ]
(defparser params ()
  (or! (sep 'ident *comma*)
       (ok ())))

;; function-def := ident "(" params ")" block-stmt
(defparser function-def ()
  (let! ((name 'ident)
         (params (prog2! *lparen* 'params *rparen*))
         (body 'block-stmt))
    (ok (list* :function name params (rest body)))))

;; program := function+
(defparameter *program*
  (prog1! (collect1 'function-def)
          *whitespace*
          (eof)))



(defun parse-tiny-c (stream)
  (parse *program* stream))

(defun parse-tiny-c-from-string (string)
  (with-input-from-string (s string)
    (parse *program* s)))

(defun test (stream)
  (handler-bind ((parser-error
                   (lambda (err)
                     (dolist (item (parser-error-return-trace err))
                       (destructuring-bind (name line col) item
                         (format t "    on ~A:~A:~A~%"
                                 name line col))))))
    (parse-tiny-c stream)))
