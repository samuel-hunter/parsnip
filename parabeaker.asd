;;;; parabeaker.asd

(asdf:defsystem #:parabeaker
  :description "Parser Combinator library"
  :author "Samuel Hunter"
  :license  "BSD 3-Clause"
  :version "0.0.1"
  :depends-on (#:alexandria)
  :components ((:file "parabeaker")))
