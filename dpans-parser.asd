(cl:in-package #:common-lisp-user)

(asdf:defsystem #:dpans-parser
  :serial t
  :components
  ((:file "packages")
   (:file "read-token")
   (:file "combinatory-parsing")
   (:file "parse")
   (:file "tex-command-parsers")
   (:file "document")
   (:file "print-xml")
   ))
