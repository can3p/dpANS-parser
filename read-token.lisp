(cl:in-package #:dpans-parser)

(defun maybe-unread-char (char stream)
  (unless (null char)
    (unread-char char stream)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class TOKEN.

(defclass token ()
  ((%contents :initarg :contents :reader contents)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class PUNCTUATION.

(defclass punctuation (token)
  ())

(defgeneric punctuationp (token)
  (:method (token) nil)
  (:method ((token punctuation)) t))

(defmethod print-object ((object punctuation) stream)
  (print-unreadable-object (object stream)
    (format stream "P ~s" (contents object))))

(defun punctuation-p (char)
  (find char "!\"#$%&'()*+,-.:;<=>?@[\\]^_`{|}~"))

(defun read-punctuation (stream)
  (let ((char (read-char stream nil nil)))
    (cond ((eql char #\-)
	   ;; We want to check whether we might have two or three
	   ;; minus signs in a sequence, because TeX attaches special
	   ;; meaning to those.
	   (let ((char2 (read-char stream nil nil)))
	     (if (eql char2 #\-)
		 (let ((char3 (read-char stream nil nil)))
		   (if (eql char3 #\-)
		       (make-instance 'punctuation
			 :contents "---")
		       (progn (maybe-unread-char char3 stream)
			      (make-instance 'punctuation
				:contents "--"))))
		 (progn (maybe-unread-char char2 stream)
			(make-instance 'punctuation
			  :contents "-")))))
	  ((eql char #\:)
	   ;; While TeX does not attach any special meaning to two
	   ;; colons in a sequence, dpANS does, so we want to
	   ;; recognize that token separately.
	   (let ((char2 (read-char stream nil nil)))
	     (if (eql char2 #\:)
		 (make-instance 'punctuation
		   :contents "::")
		 (progn (maybe-unread-char char2 stream)
			(make-instance 'punctuation
			  :contents ":")))))
	  (t
	   (make-instance 'punctuation
	     :contents (string char))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class WHITESPACE.

(defclass whitespace (token)
  ())

(defmethod print-object ((object whitespace) stream)
  (print-unreadable-object (object stream)
    (format stream "W ~s" (contents object))))

(defun whitespace-p (char)
  (or (eql char #\Space) (eql char #\Tab)))

(defun read-whitespace (stream)
  (make-instance 'whitespace
    :contents
    (with-output-to-string (string-stream)
      (loop for char = (read-char stream nil nil)
	    while (whitespace-p char)
	    do (write-char char string-stream)
	    finally (maybe-unread-char char stream)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class NEWLINES.

(defclass newlines (token)
  ())

(defmethod print-object ((object newlines) stream)
  (print-unreadable-object (object stream)
    (format stream "N ~s" (length (contents object)))))

(defun read-newlines (stream)
  (make-instance 'newlines
    :contents
    (with-output-to-string (string-stream)
      (loop for char = (read-char stream nil nil)
	    while (eql char #\Newline)
	    do (write-char char string-stream)
	    finally (maybe-unread-char char stream)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class COMMENT.

(defclass comment (token)
  ())

(defmethod print-object ((object comment) stream)
  (print-unreadable-object (object stream)
    (format stream "C")))

(defun read-comment (stream)
  (make-instance 'comment
    :contents
    (with-output-to-string (string-stream)
      (loop for char = (read-char stream nil nil)
	    until (or (null char) (eql char #\Newline))
	    do (write-char char string-stream)
            finally (maybe-unread-char char stream)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class IDENTIFIER.

(defclass identifier (token)
  ())

(defgeneric identifierp (token)
  (:method (token) nil)
  (:method ((token identifier)) t))

(defmethod print-object ((object identifier) stream)
  (print-unreadable-object (object stream)
    (format stream "ID ~s" (contents object))))

(defun read-identifier (stream)
  (make-instance 'identifier
    :contents
    (with-output-to-string (string-stream)
      (loop for char = (read-char stream nil nil)
	    until (null char)
	    while (or (eql char #\\)
		      (alphanumericp char)
		      (eql char #\-))
	    do (write-char char string-stream)
	    finally (maybe-unread-char char stream)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class OTHER

(defclass OTHER (token)
  ())

(defmethod print-object ((object other) stream)
  (print-unreadable-object (object stream)
    (format stream "? ~s" (contents object))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Read a token.

(defun read-token (stream)
  (let ((char (read-char stream nil nil)))
    (cond ((null char)
	   nil)
	  ((eql char #\Newline)
	   (unread-char char stream)
	   (read-newlines stream))
	  ((whitespace-p char)
	   (unread-char char stream)
	   (read-whitespace stream))
	  ((eq char #\%)
	   (unread-char char stream)
	   (read-comment stream))
	  ((punctuation-p char)
	   (unread-char char stream)
	   (read-punctuation stream))
	  ((and (not (alphanumericp char)) (not (eql char #\\)))
	   (make-instance 'other
	     :contents (string char)))
	  (t
	   (unread-char char stream)
	   (read-identifier stream)))))
		   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tokenize a stream or a file.

(defun tokenize-string (str)
  (with-input-from-string (in str)
    (tokenize-stream in)))

(defun tokenize-stream (stream)
  (loop for token = (read-token stream)
        until (null token)
        when (not (typep token 'comment))
        collect token))

(defun tokenize-file (filename)
  (with-open-file (stream filename :direction :input)
    (tokenize-stream stream)))
