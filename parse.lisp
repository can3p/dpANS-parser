(cl:in-package #:dpans-parser)

(define-parser word-parser
  (singleton #'identity
	     (lambda (token)
	       (and (typep token 'identifier)
		    (not (eql (char (contents token) 0) #\\))))))

(define-parser macro-parser
  (singleton #'identity
	     (lambda (token)
	       (and (typep token 'identifier)
		    (eql (char (contents token) 0) #\\)))))

(define-parser punctuation-parser
  (singleton #'identity
	     (lambda (token)
	       (typep token 'punctuation))))

(define-parser whitespace-parser
  (singleton #'identity
	     (lambda (token)
	       (typep token 'whitespace))))

(define-parser single-newline-parser
  (singleton #'identity
	     (lambda (token)
	       (and (typep token 'newlines)
		    (= 1 (length (contents token)))))))

(define-parser multiple-newline-parser
  (singleton #'identity
	     (lambda (token)
	       (and (typep token 'newlines)
		    (> (length (contents token)) 1)))))

(define-parser text-element-parser
  (alternative 'word-parser
	       'punctuation-parser
	       'whitespace-parser
	       'single-newline-parser))

(define-parser text-parser
  (consecutive #'cons
	       'word-parser
	       (repeat* #'list 'text-element-parser)))

;;; This parser succeeds for a single punctuation token that contains
;;; a single backslash character.  It returns the punctuation token as
;;; the result of the parse.
(define-parser backslash-parser
  (narrow (lambda (punctuation) (string= (contents punctuation) "\\"))
	  'punctuation-parser))

;;; This parser succeeds for a backslash followed by a word W with
;;; nothing in between the two.  It returns W as the result of the
;;; parse.
(define-parser tex-command-parser
  (consecutive (lambda (backslash command-word)
		 (declare (ignore backslash))
		 command-word)
	       'backslash-parser
	       'word-parser))
