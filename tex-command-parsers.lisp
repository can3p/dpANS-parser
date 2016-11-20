(cl:in-package #:dpans-parser)

;; Text = WHITESPACE | BLOCK

;; WHITESPACE = ' ' | NEW_LINE | COMMENT

;; COMMENT = NEW_LINE? '%' Not(NEW_LINE) NEW_LINE

;; BLOCK = (BLOCK_OPERATOR | TEXT) ((NEW_LINE NEW_LINE) | EOF)

;; BLOCK_OPERATOR = NEW_LINE? OPERATOR NEW_LINE

;; OPERATOR = "\\" WORD '%'? OPERATOR_ARGUMENT+

;; OPERATOR_ARGUMENT = "{" [^}]+ "}"

;; TEXT = ((WORD | OPERATOR) WHITESPACE*)

(defun pass-args (&rest args) args)
(defun flatten-args (&rest args)
  (apply #'concatenate 'list args))

(defclass <file> ()
  (
   (contents :initarg :contents :initform nil :reader contents)
   ))

(defmethod print-object ((instance <file>) stream)
  (format stream "<file>~%File contents:~a~%"
          (contents instance)))

(defclass <block> ()
  (
   (contents :initarg :contents :initform nil :reader contents)
   ))

(defmethod print-object ((instance <block>) stream)
  (format stream "<block>~%Block contents:~a~%"
          (contents instance)))

(defclass <command> ()
    (
     (is-definition :initarg :is-definition :initform nil :reader is-definition)
     (args :initarg :args :initform nil :reader args)
     (name :initarg :name :reader name)
     ))

(defmethod print-object ((instance <command>) stream)
  (format stream "<command ~s>~%Command args:~a~%"
          (name instance)
          (args instance)
          ))

(define-parser file-parser
  (consecutive (lambda (_ contents)
                 (declare (ignore _))
                 (make-instance '<file>
                                :contents contents))
               (repeat+ (lambda (&rest r)
                          (declare (ignore r))
                          nil)
                        (alternative 'single-newline-parser 'multiple-newline-parser))
               (repeat+ 'pass-args
                        'block-parser)))

(define-parser block-parser
  (consecutive (lambda (contents &rest rest)
                 (declare (ignore rest))
                 (make-instance '<block>
                                :contents contents))
               (alternative 'tex-command-parser 'text-parser)
               (alternative 'multiple-newline-parser 'single-newline-parser)))

(define-parser text-element-parser
  (alternative 'word-parser
               'punctuation-parser
               'whitespace-parser
               'tex-command-parser
               'single-newline-parser))

(define-parser text-parser
  (consecutive #'cons
               'word-parser
               (repeat* #'list 'text-element-parser)))



;;; This parser succeeds for a backslash followed by a word W with
;;; nothing in between the two.  It returns W as the result of the
;;; parse.
(define-parser tex-command-parser
  (consecutive (lambda (backslash command-word args)
                 (declare (ignore backslash))
                 (make-instance '<command>
                                :name (contents command-word)
                                :args args))
               'backslash-parser
               'word-parser
               (repeat* 'pass-args
                        'tex-command-argument-parser)))

(define-parser tex-command-argument-parser
  (consecutive (lambda (start contents stop)
                 (declare (ignore start stop)) contents)
               'start-argument-parser
               'tex-command-argument-inner-parser
               'end-argument-parser))

(define-parser tex-command-argument-inner-parser
  (repeat* 'flatten-args
           (consecutive 'flatten-args
                        (repeat+ 'pass-args
                                 (alternative 'word-parser
                                              (narrow (lambda (punctuation) (not (string= (contents punctuation) "}"))) 'punctuation-parser)))
                        (repeat* 'pass-args
                                 (alternative 'whitespace-parser 'single-newline-parser)))))

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

;;; This parser succeeds for a single punctuation token that contains
;;; a single backslash character.  It returns the punctuation token as
;;; the result of the parse.
(define-parser backslash-parser
  (narrow (lambda (punctuation) (string= (contents punctuation) "\\"))
	  'punctuation-parser))

(define-parser start-argument-parser
  (narrow (lambda (punctuation) (string= (contents punctuation) "{"))
          'punctuation-parser))

(define-parser end-argument-parser
  (narrow (lambda (punctuation) (string= (contents punctuation) "}"))
          'punctuation-parser))

(define-parser ratio-parser
  (narrow (lambda (punctuation) (string= (contents punctuation) "%"))
          'punctuation-parser))

