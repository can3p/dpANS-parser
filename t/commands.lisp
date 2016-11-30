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
This is \\ie a \\metavar{var-var} test sentence. \\Thenextfigure shows that.

"))
  (progn
    (ok successp "Document parsed successfully")
    (let* (
           (document (dpans-parser::create-document-from-stream stream))
           (s (make-string-output-stream)))
      (dpans-parser::print-xml s document)
      (is (get-output-stream-string s) "<document>
  <paragraph>This is i.e. a <metavar var=\"var-var\" /> test sentence. The next figure shows that.</paragraph>
</document>")
      )))

(multiple-value-bind (successp stream)
    (dpans-parser::file-parser (dpans-parser::tokenize-string "
This is a \\term{term test} and \\newterm{new term test} sentence.

"))
  (progn
    (ok successp "Document parsed successfully")
    (let* (
           (document (dpans-parser::create-document-from-stream stream))
           (s (make-string-output-stream)))
      (dpans-parser::print-xml s document)
      (is (get-output-stream-string s) "<document>
  <paragraph>This is a <link term=\"term test\">term test</link> and <link new-term=\"new term test\">new term test</link> sentence.</paragraph>
</document>")
      )))

(multiple-value-bind (successp stream)
    (dpans-parser::file-parser (dpans-parser::tokenize-string "
This is a \\typeref{type-name} and \\seevar{var-name} and \\varref{ref-var-name} and \\funref{fun-name} and \\seefuns{see-fun-name}.

"))
  (progn
    (ok successp "Document parsed successfully")
    (let* (
           (document (dpans-parser::create-document-from-stream stream))
           (s (make-string-output-stream)))
      (dpans-parser::print-xml s document)
      (is (get-output-stream-string s) "<document>
  <paragraph>This is a <link type=\"type-name\" /> and <link var=\"var-name\" /> and <link var=\"ref-var-name\" /> and <link fun=\"fun-name\" /> and <link fun=\"see-fun-name\" />.</paragraph>
</document>")
      )))

(multiple-value-bind (successp stream)
    (dpans-parser::file-parser (dpans-parser::tokenize-string "
This is a \\issue{SOMETHING:BIG}issue test\\endissue{SOMETHING:BIG} sentence.

"))
  (progn
    (ok successp "Document parsed successfully")
    (let* (
           (document (dpans-parser::create-document-from-stream stream))
           (s (make-string-output-stream)))
      (dpans-parser::print-xml s document)
      (is (get-output-stream-string s) "<document>
  <paragraph>This is a issue test sentence.</paragraph>
</document>")
      )))

(multiple-value-bind (successp stream)
    (dpans-parser::file-parser (dpans-parser::tokenize-string "
\\beginSection{First section}
\\beginSubSection{First sub section}
\\beginSubSubSection{First sub sub section}
\\beginSubSubsubsection{First sub sub sub section}
\\beginSubSubsubsubsection{First sub sub sub sub section}

I am there to check that we actually get that deep.

\\endSubSubsubsubsection%{First sub sub sub sub section}
\\endSubSubsubsection%{First sub sub sub section}
\\endSubSubSection%{First sub sub section}
\\endSubSection%{First sub section}
\\endSection%{First section}

I am there just to check that we actually come back to document.

"))
  (progn
    (ok successp "Document parsed successfully")
    (let* (
          (document (dpans-parser::create-document-from-stream stream))
          (s (make-string-output-stream)))
      (dpans-parser::print-xml s document)
      (is (get-output-stream-string s) "<document>
  <section title=\"First section\">
    <subsection title=\"First sub section\">
      <subsubsection title=\"First sub sub section\">
        <subsubsubsection title=\"First sub sub sub section\">
          <subsubsubsubsection title=\"First sub sub sub sub section\">
            <paragraph>I am there to check that we actually get that deep.</paragraph>
          </subsubsubsubsection>
        </subsubsubsection>
      </subsubsection>
    </subsection>
  </section>
  <paragraph>I am there just to check that we actually come back to document.</paragraph>
</document>")
      )))

(finalize)
