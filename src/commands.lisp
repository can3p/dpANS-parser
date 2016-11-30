(cl:in-package #:dpans-parser)

(defparameter *commands* (make-hash-table :test 'equal))
(defparameter *command-argparsers* (make-hash-table :test 'equal))

(defmacro defcommand-argparser (name &body body)
  `(setf (gethash (string-downcase (symbol-name ',name)) *command-argparsers*)
         (lambda (argument) ,@body)))

(defun run-command-argparser (name argument)
  (if (gethash name *command-argparsers*)
      (funcall (gethash name *command-argparsers*) argument)
      (error "argparser with a name ~s is not defined" name)))

(defmacro defcommand (name args &body body)
  (let* ((lambda-args (mapcar #'(lambda (&rest rest)
                                  (declare (ignore rest))
                                  (gensym)) args))
         (let-args (mapcar #'(lambda (args larg)
                               `(,(car args) (run-command-argparser
                                              ,(string-downcase (symbol-name (cadr args)))
                                              ,larg)))
                               args lambda-args)))
    `(setf (gethash (string-downcase (symbol-name ',name)) *commands*)
           (lambda ,lambda-args
             (let ,let-args ,@body)))))

(defun run-command (name arguments)
  (if (gethash name *commands*)
      (apply (gethash name *commands*) arguments)
      (error "command with a name ~s is not defined" name)))


(defcommand-argparser list->string
  (apply #'concatenate 'string argument))

(defcommand-argparser string
  (apply #'concatenate 'string (mapcar #'contents argument)))

(defcommand-argparser text-block
  (let ((nodes '())
        (accumulator '()))
    (labels ((flush-accumulator ()
               (when (not (null accumulator))
                 (push (apply #'concatenate 'string (reverse accumulator)) nodes)
                 (setf accumulator '())))
             (add-command (command)
               (flush-accumulator)
               (let ((result (run-command (name command) (args command))))
                 (if (not (null result))
                     (push result nodes))))
             (add-string (token)
               (push (contents token) accumulator)))

      (loop for token in argument
            do (cond
                 ((typep token 'token) (add-string token))
                 ((typep token '<command>) (add-command token))
                 (t (error "Unknown token inside of text block ~a" token))))
      (flush-accumulator)
      nodes)))

(defcommand beginsubsection ((title string))
  (add-child-and-enter (make-instance '<container-block-element>
                                      :name "subsection"
                                      :title title)))

(defcommand definesection ((name string))
  (mark-as-section name))

(defcommand text-block ((contents text-block))
  (add-child (make-instance '<paragraph>
                            :children contents)))
