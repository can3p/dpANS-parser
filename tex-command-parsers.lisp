(cl:in-package #:dpans-parser)

;;; This parser succeeds for a backslash followed by a word W with
;;; nothing in between the two.  It returns W as the result of the
;;; parse.
(define-parser tex-command-parser
  (consecutive (lambda (backslash command-word)
		 (declare (ignore backslash))
		 command-word)
	       'backslash-parser
	       'word-parser))

;;; This parser succeeds for the TeX command \label.  It returns an
;;; IDENTIFIER token containing the string "label".
(define-parser label-command-parser
  (narrow (lambda (word) (string= (contents word) "label"))
	  'tex-command-parser))
