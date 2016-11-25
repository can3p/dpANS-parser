(cl:in-package #:dpans-parser)


(defvar *document* nil)
(defvar *current-element* nil)

(defclass <element> ()
  (
   (parent :initarg :parent :initform nil :accessor parent)
   ))

(defclass <block-element> (<element>)
  (
   (name :initarg :name :initform nil :reader name)
   (title :initarg :title :initform nil :reader title)
   (children :initarg :children :initform nil :accessor children)
   ))


(defclass <document> (<block-element>)
  (
   (props :initarg :props :initform nil :reader props)
   ))

(defclass <paragraph> (<block-element>) ())

(defun reset-document ()
  (let ((document (make-instance '<document>)))
    (setf *document* document)
    (setf *current-element* document)))

(defun add-child (element)
  (setf (parent element) *current-element*)
  (setf (children *current-element*) (cons element (children *current-element*))))

(defun add-child-and-enter (element)
  (add-child element)
  (setf *current-element* element))

(defun close-child-and-go-up (element-name element-title)
  (when (not (string= element-name (name *current-element*)))
    (error "Attempt to close element with different name: ~S vs ~S"
           (name *current-element*)
           element-name))

  (when (not (string= element-title (title *current-element*)))
    (error "Attempt to close element with different title: ~S vs ~S"
           (title *current-element*)
           element-title))

  (setf *current-element* (parent *current-element*)))

;; (defmethod print-object ((instance <command>) stream)
;;   (let ((status (if (is-closing instance) "CLOSE" "OPEN")))
;;     (format stream "<command ~s ~a>~%Command args:~a~%"
;;             (name instance)
;;             status
;;             (args instance)
;;             )))
