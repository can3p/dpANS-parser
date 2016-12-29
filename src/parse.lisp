(cl:in-package #:dpans-parser)

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

(defun test-create-and-print-document-from-file (&optional (file "concept-arrays"))
  (let ((document (test-create-document-from-file file)))
    (if document
        (print-xml t document)
        "failed-to-parse-stream")))

(defun test-create-document-from-file (&optional (file "concept-arrays"))
  (let ((fname (make-pathname :directory '(:relative "dpans")
                              :name file :type "tex"))
        (asdf-location (asdf:system-source-file :dpans-parser)))
    (with-open-file (in (merge-pathnames fname asdf-location))
      (let ((tokens (tokenize-stream in)))
        (multiple-value-bind (successp stream) (file-parser tokens)
          (if successp
              (create-document-from-stream stream)
              nil))))))

(defun parse-spec ()
  (test-create-document-from-file "chap-15"))
