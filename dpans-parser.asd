(in-package :cl-user)
(defpackage dpans-parser-asd
  (:use :cl :asdf))
(in-package :dpans-parser-asd)

(asdf:defsystem #:dpans-parser
  :serial t
  :description "Parser for dpANS common lisp spec draft"
  :author "Dmitry Petrov <dpetroff@gmail.com>"
  :license "Public Domain"
  :components ((:module "src"
                :components
                (
                 (:file "packages")
                 (:file "read-token")
                 (:file "combinatory-parsing")
                 (:file "parse")
                 (:file "tex-command-parsers")
                 (:file "document")
                 (:file "commands")
                 (:file "print-xml")
                 )))
  :in-order-to ((test-op (test-op dpans-parser-test))))
