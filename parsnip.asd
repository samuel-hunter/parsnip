;;; parsnip.asd - system definitions
;;;
;;; Copyright 2021 Samuel Hunter <samuel (at) shunter (dot) xyz>
;;; BSD 3-Clause License. See LICENSE for details.

(defsystem #:parsnip
  :description "Monadic parser combinator library"
  :long-description "Parsnip defines a package that hosts a parser combinator collection targeting user-facing compilers, interpreters, or other character-reading decoders."
  :author "Samuel Hunter"
  :license  "BSD 3-Clause"
  :version "0.0.8"

  :homepage "https://sr.ht/~shunter/parsnip/"
  :source-control (:git "https://git.sr.ht/~shunter/parsnip")
  :bug-tracker "https://todo.sr.ht/~shunter/parsnip"
  :mailto "\~\s\h\u\n\t\e\r\/\p\u\b\l\i\c\-\i\n\b\o\x\@\l\i\s\t\s\.\s\r\.\h\t"

  :depends-on (#:alexandria)
  :components ((:file "parsnip"))
  :in-order-to ((test-op (test-op :parsnip/test))))

(defsystem #:parsnip/examples
  :description "Parsnip library examples"
  :author "Samuel Hunter"
  :license "BSD 3-Clause"
  :version "0.0.8"

  :depends-on (#:parsnip
               #:alexandria)
  :components ((:module :examples
                :components ((:file "json")
                             (:file "tiny-c"))))
  :in-order-to ((test-op (test-op :parsnip/test))))

(defsystem #:parsnip/test
  :description "Parsnip library test suite"
  :author "Samuel Hunter"
  :license "BSD 3-Clause"
  :version "0.0.8"

  :depends-on (#:parsnip
               #:parsnip/examples
               #:parachute)
  :components ((:file "test"))
  :perform (test-op (op c)
             (uiop:symbol-call :parachute :test :xyz.shunter.parsnip.test)))
