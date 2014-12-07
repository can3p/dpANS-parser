(cl:in-package #:common-lisp-user)

(asdf:defsystem #:dpans-parser
  :serial t
  :components
  ((:file "packages")
   (:file "read-token")
   (:file "combinatory-parsing")
   (:file "parse")))
