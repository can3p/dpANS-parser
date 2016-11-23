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

(defun test (&optional file)
  (with-open-file (in (or file (merge-pathnames #p"dpans/concept-arrays-mod.tex" (asdf:system-source-file :parse-lisp-spec))))
    (let ((tokens (tokenize-stream in)))
      (file-parser tokens))))

(defun test-string ()
  (tex-command-parser (tokenize-string "\\term{List element}")))
