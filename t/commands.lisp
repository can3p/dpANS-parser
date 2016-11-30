(in-package :cl-user)
(defpackage dpans-parser-test
  (:use :cl
   :dpans-parser
        :prove))

(in-package :dpans-parser-test)

;; NOTE: To run this test file, execute `(asdf:test-system :dpans-parser)' in your Lisp.

(plan nil)

(multiple-value-bind (successp stream)
    (dpans-parser::file-parser (dpans-parser::tokenize-string "
\\beginSubSection{Test Section}
\\defineSection{abCdef}
"))
  (progn
    (ok successp "Document parsed successfully")
    (let* (
           (document (dpans-parser::create-document-from-stream stream))
           (element (gethash "abCdef" (dpans-parser::sections document)))
           (s (make-string-output-stream)))
      (ok element)
      (dpans-parser::print-xml s element)
      (is (get-output-stream-string s) "<subsection title=\"Test Section\">
</subsection>
")
      )
    ))

(multiple-value-bind (successp stream)
    (dpans-parser::file-parser (dpans-parser::tokenize-string "
This is a test sentence.

"))
  (progn
    (ok successp "Document parsed successfully")
    (let* (
           (document (dpans-parser::create-document-from-stream stream))
           (s (make-string-output-stream)))
      (dpans-parser::print-xml s document)
      (is (get-output-stream-string s) "<document>
  <paragraph>This is a test sentence.</paragraph>
</document>")
      )))

(multiple-value-bind (successp stream)
    (dpans-parser::file-parser (dpans-parser::tokenize-string "
This is a \\term{term test} sentence.

"))
  (progn
    (ok successp "Document parsed successfully")
    (let* (
          (document (dpans-parser::create-document-from-stream stream))
          (s (make-string-output-stream)))
      (dpans-parser::print-xml s document)
      (is (get-output-stream-string s) "<document>
  <paragraph>This is a <link term=\"term test\">term test</link> sentence.</paragraph>
</document>")
      )))

(finalize)
