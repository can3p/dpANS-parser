(cl:in-package #:dpans-parser)


(defun make-indent (indent)
  (if (= indent 0) ""
      (concatenate 'string " " (make-indent (- indent 1)))))

(defun print-children (stream element indent)
  (when (> (length (children element)) 0)
    (loop with new-indent = (+ 2 indent)
          for child in (children element)
          do (print-xml stream child new-indent))))

(defgeneric print-xml (stream element &optional indent))

(defmethod print-xml (stream (element <element>) &optional (indent 0))
  (declare (ignore indent))
  (error "<element> class should never be used for real element, try children"))

(defmethod print-xml (stream (element <document>) &optional (indent 0))
  (format stream "<document>~%")
  (print-children stream element indent)
  (format stream "</document>"))

(defmethod print-xml (stream (element <container-block-element>) &optional (indent 0))
  (let ((pad (make-indent indent)))
    (format stream "~a<~a title=~S>~%"
            pad (name element) (title element))
    (print-children stream element indent)
    (format stream "~a</~a>~%"
            pad (name element))))

(defmethod print-xml (stream (element <paragraph>) &optional (indent 0))
  (let ((pad (make-indent indent)))
    (format stream "~a<paragraph>" pad)
    (print-children stream element indent)
    (format stream "</paragraph>~%")))

(defmethod print-xml (stream (element string) &optional (indent 0))
  (declare (ignore indent))
  (format stream "~a" element))

(defmethod print-xml (stream (element <term>) &optional (indent 0))
  (declare (ignore indent))
  (format stream "<link term=~s>~a</link>" (term element) (text element)))

(defmethod print-xml (stream (element <new-term>) &optional (indent 0))
  (declare (ignore indent))
  (format stream "<link new-term=~s>~a</link>" (term element) (text element)))

(defmethod print-xml (stream (element <seevar>) &optional (indent 0))
  (declare (ignore indent))
  (format stream "<link var=~s />" (name element)))

(defmethod print-xml (stream (element <seefuns>) &optional (indent 0))
  (declare (ignore indent))
  (format stream "<link fun=~s />" (name element)))

(defmethod print-xml (stream (element <funref>) &optional (indent 0))
  (declare (ignore indent))
  (format stream "<link fun=~s />" (name element)))

(defmethod print-xml (stream (element <varref>) &optional (indent 0))
  (declare (ignore indent))
  (format stream "<link var=~s />" (name element)))
