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

(define-parser text-element-parser
  (alternative 'word-parser 'punctuation-parser))
