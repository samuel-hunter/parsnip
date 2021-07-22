(asdf:defsystem #:parsnip
  :description "Quick and unopinionated parser combinators"
  :author "Samuel Hunter"
  :license  "BSD 3-Clause"
  :version "0.0.1"
  :depends-on (#:alexandria
               #:trivial-lazy)
  :components ((:file "parsnip"))
  :in-order-to ((asdf:test-op (asdf:test-op :parsnip/test))))

(asdf:defsystem #:parsnip/test
  :description "Parsnip testing suite"
  :author "Samuel Hunter"
  :license "BSD 3-Clause"
  :version "0.0.1"
  :depends-on (#:parsnip
               #:parachute)
  :components ((:file "test"))
  :perform (asdf:test-op (op c)
             (uiop:symbol-call :parachute :test :xyz.shunter.parsnip.test)))
