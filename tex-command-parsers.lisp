(cl:in-package #:dpans-parser)

;; Text = WHITESPACE | BLOCK

;; WHITESPACE = ' ' | NEW_LINE | COMMENT

;; COMMENT = NEW_LINE? '%' Not(NEW_LINE) NEW_LINE

;; BLOCK = (BLOCK_OPERATOR | TEXT) ((NEW_LINE NEW_LINE) | EOF)

;; BLOCK_OPERATOR = NEW_LINE? OPERATOR NEW_LINE

;; OPERATOR = "\\" WORD '%'? OPERATOR_ARGUMENT+

;; OPERATOR_ARGUMENT = "{" [^}]+ "}"

;; TEXT = ((WORD | OPERATOR) WHITESPACE*)

(defclass <command> ()
    (
     (is-new :initarg :is-new :initform nil :reader is-new)
     (name :initarg :name :reader name)
     ))

(defmethod print-object ((instance <command>) stream)
  (format stream "<command ~s>~%" (name instance)))

;;; This parser succeeds for a backslash followed by a word W with
;;; nothing in between the two.  It returns W as the result of the
;;; parse.
(define-parser tex-command-parser
  (consecutive (lambda (backslash command-word)
		 (declare (ignore backslash))
                 (make-instance '<command>
                                :name (contents command-word)))
	       'backslash-parser
	       'word-parser))

(define-parser word-parser
  (singleton #'identity #'identifierp))

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

