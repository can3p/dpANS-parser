(in-package :cl-user)
(defpackage dpans-parser-test-asd
  (:use :cl :asdf))
(in-package :dpans-parser-test-asd)

(asdf:defsystem #:dpans-parser-test
  :author "Dmitry Petrov"
  :license "Public Domain"
  :depends-on (:dpans-parser
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "commands"))))
  :description "Test system for dpans-parser"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
