(cl:in-package #:dpans-parser)


(defun make-indent (indent)
  (if (= indent 0) ""
      (concatenate 'string " " (make-indent (- indent 1)))))

(defun print-children (stream element indent)
  (when (> (length (children element)) 0)
    (print-elements stream (children element) indent)))

(defun print-elements (stream elements indent)
    (loop with new-indent = (+ 2 indent)
          for child in elements
          do (print-xml stream child new-indent)))

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

(defmethod print-xml (stream (element <dict-article>) &optional (indent 0))
  (let ((pad (make-indent indent)))
    (format stream "~a<~a term=~S type=~S>~%"
            pad (name element) (title element) (atype element))
    (print-children stream element indent)
    (format stream "~a</~a>~%"
            pad (name element))))

(defmethod print-xml (stream (element <dict-section>) &optional (indent 0))
  (let ((pad (make-indent indent)))
    (format stream "~a<~a label=~S>~%"
            pad (name element) (title element))
    (print-children stream element indent)
    (format stream "~a</~a>~%"
            pad (name element))))

(defmethod print-xml (stream (element <chapter>) &optional (indent 0))
  (let ((pad (make-indent indent)))
    (format stream "~a<chapter number=~S title=~S ref=~S ref-title=~S>~%"
            pad (chap-number element) (title element) (ref element) (ref-title element))
    (print-children stream element indent)
    (format stream "~a</chapter>~%"
            pad)))

(defmethod print-xml (stream (element <displaythree>) &optional (indent 0))
  (let ((pad (make-indent indent)))
    (format stream "~a<displaythree title=~S>~%"
            pad (title element))
    (print-children stream element indent)
    (format stream "~a</displaythree>~%"
            pad)))

(defmethod print-xml (stream (element <displaythree-func>) &optional (indent 0))
  (let ((pad (make-indent indent)))
    (format stream "~a<func name=~S />~%"
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

(defmethod print-xml (stream (element <typeref>) &optional (indent 0))
  (declare (ignore indent))
  (format stream "<link type=~s />" (name element)))

(defmethod print-xml (stream (element <chapref>) &optional (indent 0))
  (let ((name (name element)))
    (format stream "<link section=~s>" name)
    (print-elements stream
                    (run-command name nil :pass-if-empty t)
                    indent)
    (format stream "</link>")))

(defmethod print-xml (stream (element <metavar>) &optional (indent 0))
  (declare (ignore indent))
  (format stream "<metavar var=~s />" (name element)))

(defmethod print-xml (stream (element <formula>) &optional (indent 0))
  (format stream "<formula>")
  (print-children stream element indent)
  (format stream "</formula>"))

(defmethod print-xml (stream (element <formula-sub>) &optional (indent 0))
  (format stream "<sub>")
  (print-children stream element indent)
  (format stream "</sub>"))

(defmethod print-xml (stream (element <formula-symbol>) &optional (indent 0))
  (declare (ignore indent))
  (format stream "<symbol>~a</symbol>" (name element)))
