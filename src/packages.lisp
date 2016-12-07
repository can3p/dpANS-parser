(cl:in-package #:common-lisp-user)

(defpackage #:dpans-parser
  (:use #:common-lisp)
  (:export :parse-spec
           :<element>
           :parent
           :document
           :<metavar>
           :name
           :<link>
           :<seevar>
           :<seefuns>
           :<funref>
           :<varref>
           :<typeref>
           :<chapref>
           :<term>
           :term
           :text
           :<new-term>
           :<block-element>
           :children
           :<displaythree>
           :title
           :<displaythree-func>
           :<container-block-element>
           :title
           :<document>
           :props
           :sections
           :<paragraph>
           :<formula>
           :<formula-sub>
           :<formula-symbol>
   ))
