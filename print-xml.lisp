(cl:in-package #:dpans-parser)


(defun make-indent (indent)
  (if (= indent 0) ""
      (concatenate 'string " " (make-indent (- indent 1)))))

(defun print-children (element indent)
  (when (> (length (children element)) 0)
    (loop with new-indent = (+ 2 indent)
          for child in (children element)
          do (print-xml child new-indent))))

(defgeneric print-xml (element &optional indent))

(defmethod print-xml ((element <element>) &optional (indent 0))
  (declare (ignore indent))
  (error "<element> class should never be used for real element, try children"))

(defmethod print-xml ((element <document>) &optional (indent 0))
  (format t "<document>~%")
  (print-children element indent)
  (format t "</document>"))

(defmethod print-xml ((element <block-element>) &optional (indent 0))
  (let ((pad (make-indent indent)))
    (format t "~a<~a title=~S>~%"
            pad (name element) (title element))
    (print-children element indent)
    (format t "~a</~a>~%"
            pad (name element))))

(defmethod print-xml ((element <paragraph>) &optional (indent 0))
    (let ((pad (make-indent indent)))
      (format t "~a<paragraph>~a</paragraph>~%"
              pad (apply #'concatenate 'string (children element)))))
