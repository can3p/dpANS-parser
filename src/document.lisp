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

(defclass <chapter> (<container-block-element>)
  (
   (name :initarg :name :initform "chapter" :reader name)
   (chap-number :initarg :chap-number :initform nil :reader chap-number)
   (ref :initarg :ref :initform nil :reader ref)
   (ref-title :initarg :ref-title :initform nil :reader ref-title)
   ))


(defclass <document> (<block-element>)
  (
   (commands :initform (make-hash-table :test 'equal) :reader commands)
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
        do (when (not (null command)) ;; input command produces null item in a list, let's ignore it
             (run-command (name command) (args command))))
  *document*)

(defun add-child (element)
  (setf (parent element) *current-element*)
  (setf (children *current-element*) (reverse (cons element (reverse (children *current-element*))))))

(defun add-child-and-enter (element)
  (add-child element)
  (setf *current-element* element))

(defun close-child-and-go-up (element-name &optional (element-title nil))
  (when (not (string= element-name (name *current-element*)))
    (error "Attempt to close element with different name: ~S vs ~S"
           (name *current-element*)
           element-name))

  (when (and (not (null element-title)) (not (string= element-title (title *current-element*))))
    (error "Attempt to close element with different title: ~S vs ~S"
           (title *current-element*)
           element-title))

  (setf *current-element* (parent *current-element*)))

(defun mark-as-section (name)
  (setf (gethash name (sections *document*)) *current-element*))

(defun user-command-defined-p (name)
  (not (null (gethash (string-downcase name)
                      (commands *document*)))))

(defun define-user-command (name func)
  (setf (gethash (string-downcase name) (commands *document*))
        func))

(defun get-user-command (name)
  (gethash (string-downcase name) (commands *document*)))

;; (defmethod print-object ((instance <command>) stream)
;;   (let ((status (if (is-closing instance) "CLOSE" "OPEN")))
;;     (format stream "<command ~s ~a>~%Command args:~a~%"
;;             (name instance)
;;             status
;;             (args instance)
;;             )))
