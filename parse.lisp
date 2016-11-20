(cl:in-package #:dpans-parser)

(defun test (&optional file)
  (with-open-file (in (or file (merge-pathnames #p"dpans/concept-arrays-mod.tex" (asdf:system-source-file :parse-lisp-spec))))
    (let ((tokens (tokenize-stream in)))
      (file-parser tokens))))

(defun test-string ()
  (tex-command-parser (tokenize-string "\\term{List element}")))
