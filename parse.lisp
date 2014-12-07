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

(define-parser text-element-parser
  (alternative 'word-parser
	       'punctuation-parser
	       'whitespace-parser))

(define-parser text-parser
  (consecutive #'cons
	       'word-parser
	       (repeat* #'list 'text-element-parser)))
