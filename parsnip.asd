;;; parsnip.asd - Parsnip library system definitions

;;; Copyright 2021 Samuel Hunter <samuel (at) shunter (dot) xyz>
;;; BSD-3-Clause

(asdf:defsystem #:parsnip
  :description "Quickly combine small parsers together"
  :author "Samuel Hunter"
  :mailto "\s\a\m\u\e\l\@\s\h\u\n\t\e\r\.\x\y\z"
  :license  "BSD 3-Clause"
  :version "0.0.3"

  :homepage "https://sr.ht/~shunter/parsnip/"
  :source-control (:git "https://git.sr.ht/~shunter/parsnip")
  :bug-tracker "https://todo.sr.ht/~shunter/parsnip"

  :depends-on (#:alexandria)
  :components ((:file "parsnip"))
  :in-order-to ((asdf:test-op (asdf:test-op :parsnip/test))))

(asdf:defsystem #:parsnip/examples
  :description "Parsnip library examples"
  :author "Samuel Hunter"
  :license "BSD 3-Clause"
  :version "0.0.3"

  :depends-on (#:parsnip
               #:alexandria)
  :components ((:module :examples
                :components ((:file "json"))))
  :in-order-to ((asdf:test-op (asdf:test-op :parsnip/test))))

(asdf:defsystem #:parsnip/test
  :description "Parsnip library test suite"
  :author "Samuel Hunter"
  :license "BSD 3-Clause"
  :version "0.0.3"

  :depends-on (#:parsnip
               #:parsnip/examples
               #:parachute)
  :components ((:file "test"))
  :perform (asdf:test-op (op c)
             (uiop:symbol-call :parachute :test :xyz.shunter.parsnip.test)))
