(asdf:defsystem #:parabeaker
  :description "Parser Combinator library"
  :author "Samuel Hunter"
  :license  "BSD 3-Clause"
  :version "0.0.1"
  :depends-on (#:alexandria
               #:trivial-lazy)
  :components ((:file "parabeaker"))
  :in-order-to ((asdf:test-op (asdf:test-op :parabeaker/test))))

(asdf:defsystem #:parabeaker/test
  :description "Parabeaker testing framework"
  :author "Samuel Hunter"
  :license "BSD 3-Clause"
  :version "0.0.1"
  :depends-on (#:parabeaker
               #:parachute)
  :components ((:file "test"))
  :perform (asdf:test-op (op c)
             (uiop:symbol-call :parachute :test :parabeaker.test)))
