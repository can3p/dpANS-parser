(cl:in-package #:common-lisp-user)

(asdf:defsystem #:dpans-parser
  :serial t
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
                 ))))
