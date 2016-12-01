(cl:in-package #:dpans-parser)


(defvar *document* nil)
(defvar *current-element* nil)

(defclass <element> ()
  (
   (parent :initarg :parent :initform nil :accessor parent)
   (document :initform nil :accessor document)
   ))

(defmethod initialize-instance :after ((element <element>) &key)
  (setf (document element) *document*))

(defclass <metavar> (<element>)
  (
   (name :initarg :name :initform nil :reader name)
   ))

(defclass <link> (<element>)
  (
   (name :initarg :name :initform nil :reader name)
   ))

(defclass <seevar> (<link>) ())

(defclass <seefuns> (<link>) ())

(defclass <funref> (<link>) ())

(defclass <varref> (<link>) ())

(defclass <typeref> (<link>) ())

(defclass <chapref> (<link>) ())

(defclass <term> (<link>)
  (
   (term :initarg :term :initform nil :reader term)
   (text :initarg :text :initform nil :reader text)))

(defclass <new-term> (<term>) ())

(defclass <block-element> (<element>)
  (
   (children :initarg :children :initform nil :accessor children)
   ))

(defclass <displaythree> (<block-element>)
  (
   (title :initarg :title :initform nil :reader title)
   ))

(defclass <displaythree-func> (<element>)
  (
   (name :initarg :name :initform nil :reader name)
   ))

(defclass <container-block-element> (<block-element>)
  (
   (name :initarg :name :initform nil :reader name)
   (title :initarg :title :initform nil :reader title)
   ))


(defclass <document> (<block-element>)
  (
   (props :initform (make-hash-table :test 'equal) :reader props)
   (sections :initform (make-hash-table :test 'equal) :reader sections)
   ))

(defclass <paragraph> (<block-element>) ())

(defclass <formula> (<element>)
  (
   (children :initarg :children :initform nil :accessor children)
   ))

(defclass <formula-sub> (<formula>)
  ((name :initarg :name :initform nil :reader name)))

(defclass <formula-symbol> (<element>)
  (
   (name :initarg :name :initform nil :reader name)
   ))

(defun reset-document ()
  (let ((document (make-instance '<document>)))
    (setf *document* document)
    (setf *current-element* document)))

(defun create-document-from-stream (stream)
  (reset-document)
  (loop for command in stream
        do (run-command (name command) (args command)))
  *document*)

(defun add-child (element)
  (setf (parent element) *current-element*)
  (setf (children *current-element*) (reverse (cons element (reverse (children *current-element*))))))

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

(defun mark-as-section (name)
  (setf (gethash name (sections *document*)) *current-element*))

(defun set-property (name value)
  (setf (gethash name (props *document*)) value))

;; (defmethod print-object ((instance <command>) stream)
;;   (let ((status (if (is-closing instance) "CLOSE" "OPEN")))
;;     (format stream "<command ~s ~a>~%Command args:~a~%"
;;             (name instance)
;;             status
;;             (args instance)
;;             )))
