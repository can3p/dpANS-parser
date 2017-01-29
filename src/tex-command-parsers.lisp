(cl:in-package #:dpans-parser)

(defun pass-args (&rest args) args)
(defun flatten-args (&rest args)
  (apply #'concatenate 'list args))

(defun pass-first (first &rest args)
  (declare (ignore args))
  first)

(defun pass-second (first second &rest args)
  (declare (ignore first args))
  second)

(defun start-argument-p (token)
  (and (typep token 'punctuation) (string= (contents token) "{")))

(defun end-argument-p (token)
  (and (typep token 'punctuation) (string= (contents token) "}")))

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
                 contents)
               (optional nil 'block-terminator-parser)
               (repeat+ 'pass-args
                        'block-parser)))

(define-parser block-parser
  (consecutive (lambda (contents &rest rest)
                 (declare (ignore rest))
                 contents)
               (alternative
                (consecutive 'pass-first 'input-command-parser
                             ;; block terminator is optional because we may or may not encounter whitspace at the beginning of the included file
                             (optional nil 'block-terminator-parser))
                (consecutive 'pass-first 'tex-label-command-parser 'block-terminator-parser)
                (consecutive 'pass-first 'tex-command-parser 'block-terminator-parser)
                (consecutive 'pass-first 'text-parser 'block-terminator-parser))))

(define-parser block-terminator-parser
  (repeat+ 'pass-args
           (consecutive 'pass-args
                        (optional nil 'whitespace-parser)
                        (alternative 'multiple-newline-parser 'single-newline-parser)
                        (optional nil 'whitespace-parser)
                        )))

(define-parser tex-label-command-parser
  (consecutive (lambda (slash label ws text &rest rest)
                 (declare (ignore slash label ws rest))
                 (make-instance '<command>
                                :name "label"
                                :args (list (remove-if #'null text))))
               'backslash-parser
               (narrow (lambda (w) (string= (contents w) "label"))
                       'word-parser)
               (repeat+ 'pass-args 'whitespace-parser)
               (repeat+ 'flatten-args
                        (consecutive (lambda (&rest rest)
                                       (mapcar #'(lambda (item)
                                                   (if (listp item)
                                                       (car item)
                                                       item))
                                               rest))
                                     'word-parser
                                     (repeat* 'pass-args 'whitespace-parser)))
               (alternative
                (narrow (lambda (punctuation) (string= (contents punctuation) "::"))
                        'punctuation-parser)
                (consecutive 'pass-args
                             'colon-parser
                             'backslash-parser
                             (narrow (lambda (w) (string= (contents w) "None"))
                                     'word-parser)
                             'punctuation-parser
                             ))
               ))

;; input parser incorporated included file into a list of tokens
(define-parser input-command-parser
  (lambda (tokens)
    (multiple-value-bind (successp command remaining-tokens) (input-command-real-parser tokens)
      (if (not successp) (values nil nil tokens)
          (let ((new-tokens (run-command (name command) (args command))))
            (values t
                    nil
                    (concatenate 'list new-tokens remaining-tokens)))))))

(define-parser input-command-real-parser
  (consecutive (lambda (slash input ws filename)
                 (declare (ignore slash input ws))
                 (make-instance '<command>
                                :name "input"
                                :args `((,filename))))
               'backslash-parser
               (narrow (lambda (w) (string= (contents w) "input"))
                       'word-parser)
               (repeat+ 'pass-args 'whitespace-parser)
               'word-parser
               ;; (repeat* 'pass-args 'whitespace-parser)
               ))

(define-parser text-element-parser
  (alternative 'word-parser
               'tex-command-parser
               'inline-formula-parser
               'punctuation-parser
               'whitespace-parser
               'single-newline-parser))

(define-parser text-parser
  (consecutive (lambda (first second rest)
                 (make-instance '<command>
                                :name "text-block"
                                :args (list (cons first (cons second rest)))))
               (alternative 'word-parser 'tex-command-parser)
               'text-element-parser
               (repeat* #'list 'text-element-parser)))

;;; This parser succeeds for a backslash followed by a word W with
;;; nothing in between the two.  It returns W as the result of the
;;; parse.
(define-parser tex-command-parser
  (consecutive (lambda (backslash command-word closing args)
                 (declare (ignore backslash))
                 (make-instance '<command>
                                :name (string-downcase (contents command-word))
                                :is-closing (not (null closing))
                                :args args))
               'backslash-parser
               'word-parser
               (optional nil 'ratio-parser)
               (repeat* 'pass-args
                        'tex-command-argument-parser)))

(define-parser tex-command-argument-parser
  (alternative 'tex-command-braced-argument-parser
               'tex-command-commandlike-argument-parser))

(define-parser tex-command-commandlike-argument-parser
  (consecutive (lambda (backslash command-word)
                 (declare (ignore backslash))
                 `(,command-word))
               'backslash-parser
               'word-parser))

(define-parser tex-command-braced-argument-parser
  (consecutive (lambda (start contents stop)
                 (declare (ignore start stop)) contents)
               'start-argument-parser
               'tex-command-argument-inner-parser
               'end-argument-parser))

(define-parser tex-command-argument-inner-parser
  (lambda (tokens)
    (let ((results '())
          (br-count 0))
	    (loop with remaining-tokens = tokens
            do (let ((first-token (car remaining-tokens)))
                 (when (start-argument-p first-token)
                   (incf br-count))
                 (when (and (end-argument-p first-token) (= 0 br-count))
                   (return (values (not (null results))
                                   (reverse results)
                                   remaining-tokens)))
                 (when (end-argument-p first-token)
                   (decf br-count))

                 (progn (setf remaining-tokens (cdr remaining-tokens))
                        (push first-token results)))))))

(define-parser tex-displaythree-argument-parser
  (consecutive 'pass-second
               (optional nil 'single-newline-parser)
               (repeat+ 'pass-args
                        (consecutive 'pass-first
                                     'tex-displaythree-func-name-parser
                                     'tex-displaythree-func-separator-parser))))

(define-parser tex-displaythree-func-name-parser
  (consecutive 'pass-args
               'word-parser
               (optional nil 'tt-parser)))

(define-parser tex-displaythree-func-separator-parser
  (repeat+ 'pass-args (alternative 'amp-parser
                                   (consecutive 'pass-args
                                                'cr-parser
                                                'single-newline-parser))))

(define-parser inline-formula-parser
  (consecutive (lambda (d formula dd)
                 (declare (ignore d dd))
                 (make-instance '<command>
                                :name "formula"
                                :args (list formula)))
               'dollar-parser
               (repeat+ 'pass-args 'formula-element-parser)
               'dollar-parser
               ))

(define-parser formula-element-parser
  (alternative 'formula-symbol-parser
               'formula-command-parser))

(define-parser formula-symbol-parser
  (singleton (lambda (word)
               (make-instance '<command>
                              :name "formula-symbol"
                              :args `((,word))))
             #'identifierp))

(define-parser formula-command-parser
  (consecutive (lambda (d command-word dd symbol)
                 (declare (ignore d dd))
                 (make-instance '<command>
                                :name (concatenate 'string
                                                   "formula-"
                                                   (string-downcase (contents command-word)))
                                :args `((,symbol))))
               'backslash-parser
               'word-parser
               'whitespace-parser
               'formula-element-parser
               ))

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

(define-parser non-end-argument-parser
  (singleton #'identity
             (lambda (token)
               (string/= (contents token) "}"))))

(define-parser amp-parser
  (narrow (lambda (punctuation) (string= (contents punctuation) "&"))
          'punctuation-parser))

(define-parser dollar-parser
  (narrow (lambda (punctuation) (string= (contents punctuation) "$"))
          'punctuation-parser))

(define-parser dash-parser
  (narrow (lambda (punctuation) (string= (contents punctuation) "-"))
          'punctuation-parser))

(define-parser colon-parser
  (narrow (lambda (punctuation) (string= (contents punctuation) ":"))
          'punctuation-parser))

(define-parser ratio-parser
  (narrow (lambda (punctuation) (string= (contents punctuation) "%"))
          'punctuation-parser))

(define-parser cr-parser
  (consecutive #'pass-args
               'backslash-parser
               (narrow (lambda (w) (string= (contents w) "cr"))
                       'word-parser)))

(define-parser tt-parser
  (consecutive #'(lambda (&rest rest)
                   (make-instance '<command>
                                  :name "tt"
                                  :args (cadr (cdddr rest))))
               'start-argument-parser
               'backslash-parser
               (narrow (lambda (w) (string= (contents w) "tt"))
                       'word-parser)
               (repeat+ 'pass-args 'whitespace-parser)
               (repeat+ 'pass-args
                        'non-end-argument-parser)
               'end-argument-parser))

