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

;;; This parser succeeds for the TeX command \beginchapter.  It returns an
;;; IDENTIFIER token containing the string "beginchapter".
(define-parser beginchapter-command-parser
  (narrow (lambda (word) (string= (contents word) "beginchapter"))
	  'tex-command-parser))

;;; This parser succeeds for the TeX command \endchapter.  It returns an
;;; IDENTIFIER token containing the string "endchapter".
(define-parser endchapter-command-parser
  (narrow (lambda (word) (string= (contents word) "endchapter"))
	  'tex-command-parser))

;;; This parser succeeds for the TeX command \beginSection.  It returns an
;;; IDENTIFIER token containing the string "beginSection".
(define-parser beginsection-command-parser
  (narrow (lambda (word) (string= (contents word) "beginSection"))
	  'tex-command-parser))

;;; This parser succeeds for the TeX command \endSection.  It returns an
;;; IDENTIFIER token containing the string "endSection".
(define-parser endsection-command-parser
  (narrow (lambda (word) (string= (contents word) "endSection"))
	  'tex-command-parser))

;;; This parser succeeds for the TeX command \beginsubSection.  It returns an
;;; IDENTIFIER token containing the string "beginsubSection".
(define-parser beginsubsection-command-parser
  (narrow (lambda (word) (string= (contents word) "beginsubSection"))
	  'tex-command-parser))

;;; This parser succeeds for the TeX command \endsubSection.  It returns an
;;; IDENTIFIER token containing the string "endsubSection".
(define-parser endsubsection-command-parser
  (narrow (lambda (word) (string= (contents word) "endsubSection"))
	  'tex-command-parser))
