(cl:in-package #:dpans-parser)

;; we should evaluate files in this order to get spec
(defparameter *file-list* '(
                            setup.tex
                            chap-0.tex
                            chap-1.tex
                            chap-2.tex
                            chap-3.tex
                            chap-4.tex
                            chap-5.tex
                            chap-6.tex
                            chap-7.tex
                            chap-8.tex
                            chap-9.tex
                            chap-10.tex
                            chap-11.tex
                            chap-12.tex
                            chap-13.tex
                            chap-14.tex
                            chap-15.tex
                            chap-16.tex
                            chap-17.tex
                            chap-18.tex
                            chap-19.tex
                            chap-20.tex
                            chap-21.tex
                            chap-22.tex
                            chap-23.tex
                            chap-24.tex
                            chap-25.tex
                            chap-26.tex
                            chap-a.tex
                            ))

(defun tokenize-dpans-file (file)
  (let ((fname (make-pathname :directory '(:relative "dpans")
                              :name file :type "tex"))
        (asdf-location (asdf:system-source-file :dpans-parser)))
    (tokenize-file (merge-pathnames fname asdf-location))))

(defun test (&optional file)
  (let ((fname (make-pathname :directory '(:relative "dpans")
                              :name (or file "concept-arrays") :type "tex"))
        (asdf-location (asdf:system-source-file :dpans-parser)))
    (with-open-file (in (merge-pathnames fname asdf-location))
      (let ((tokens (tokenize-stream in)))
        (file-parser tokens)))))

(defun test-string ()
  (file-parser (tokenize-string "\\beginsubsection{List element}
")))

(defun test-create-document ()
  (multiple-value-bind (successp stream)
      (file-parser (tokenize-string "\\beginsubsection{List element}
"))
    ()
    (if successp
        (print-xml t (create-document-from-stream stream))
        "failed to parse stream")))

(defun test-create-document-from-file (&optional file)
  (let ((fname (make-pathname :directory '(:relative "dpans")
                              :name (or file "concept-arrays") :type "tex"))
        (asdf-location (asdf:system-source-file :dpans-parser)))
    (with-open-file (in (merge-pathnames fname asdf-location))
      (let ((tokens (tokenize-stream in)))
        (multiple-value-bind (successp stream) (file-parser tokens)
          (if successp
              (print-xml t (create-document-from-stream stream))
              "failed to parse stream"))))))
