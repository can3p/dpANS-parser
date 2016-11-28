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
    (ok successp "Document parse successfully")
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

(finalize)
