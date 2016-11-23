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

(defun pass-first (first &rest args)
  (declare (ignore args))
  first)

(defclass <file> ()
  (
   (contents :initarg :contents :initform nil :reader contents)
   ))

(defmethod print-object ((instance <file>) stream)
  (format stream "<file>~%File contents:~a~%"
          (contents instance)))

(defclass <text-block> ()
  (
   (contents :initarg :contents :initform nil :reader contents)
   ))

(defmethod print-object ((instance <text-block>) stream)
  (format stream "<text-block>~%Text Block contents:~a~%"
          (contents instance)))

(defclass <command> ()
    (
     (is-closing :initarg :is-closing :initform nil :reader is-closing)
     (args :initarg :args :initform nil :reader args)
     (name :initarg :name :reader name)
     ))

(defmethod print-object ((instance <command>) stream)
  (let ((status (if (is-closing instance) "CLOSE" "OPEN")))
    (format stream "<command ~s ~a>~%Command args:~a~%"
            (name instance)
            status
            (args instance)
            )))

(define-parser file-parser
  (consecutive (lambda (_ contents)
                 (declare (ignore _))
                 (make-instance '<file>
                                :contents contents))
               (optional nil 'block-terminator-parser)
               (repeat+ 'pass-args
                        'block-parser)))

(define-parser block-parser
  (consecutive (lambda (contents &rest rest)
                 (declare (ignore rest))
                 contents)
               (alternative
                (consecutive 'pass-first 'tex-command-parser 'block-terminator-parser)
                (consecutive 'pass-first 'text-parser 'block-terminator-parser))))

(define-parser block-terminator-parser
  (repeat+ 'pass-args
           (consecutive 'pass-args
                        (optional nil 'whitespace-parser)
                        (alternative 'multiple-newline-parser 'single-newline-parser)
                        (optional nil 'whitespace-parser)
                        )))

(define-parser text-element-parser
  (alternative 'word-parser
               'tex-command-parser
               'punctuation-parser
               'whitespace-parser
               'single-newline-parser))

(define-parser text-parser
  (consecutive (lambda (first second rest)
                 (make-instance '<text-block>
                                :contents (cons first (cons second rest))))
               (alternative 'word-parser 'tex-command-parser)
               'text-element-parser
               (repeat+ #'list 'text-element-parser)))

;;; This parser succeeds for a backslash followed by a word W with
;;; nothing in between the two.  It returns W as the result of the
;;; parse.
(define-parser tex-command-parser
  (consecutive (lambda (backslash command-word closing args)
                 (declare (ignore backslash))
                 (make-instance '<command>
                                :name (contents command-word)
                                :is-closing (not (null closing))
                                :args args))
               'backslash-parser
               'word-parser
               (optional nil 'ratio-parser)
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

