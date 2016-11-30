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
      (reverse nodes))))

(defcommand beginsection ((title string))
  (add-child-and-enter (make-instance '<container-block-element>
                                      :name "section"
                                      :title title)))

(defcommand beginsubsection ((title string))
  (add-child-and-enter (make-instance '<container-block-element>
                                      :name "subsection"
                                      :title title)))

(defcommand beginsubsubsection ((title string))
  (add-child-and-enter (make-instance '<container-block-element>
                                      :name "subsubsection"
                                      :title title)))

(defcommand beginsubsubsubsection ((title string))
  (add-child-and-enter (make-instance '<container-block-element>
                                      :name "subsubsubsection"
                                      :title title)))

(defcommand beginsubsubsubsubsection ((title string))
  (add-child-and-enter (make-instance '<container-block-element>
                                      :name "subsubsubsubsection"
                                      :title title)))

(defcommand endsection ((title string))
  (close-child-and-go-up "section" title))

(defcommand endsubsection ((title string))
  (close-child-and-go-up "subsection" title))

(defcommand endsubsubsection ((title string))
  (close-child-and-go-up "subsubsection" title))

(defcommand endsubsubsubsection ((title string))
  (close-child-and-go-up "subsubsubsection" title))

(defcommand endsubsubsubsubsection ((title string))
  (close-child-and-go-up "subsubsubsubsection" title))

(defcommand definesection ((name string))
  (mark-as-section name))

(defcommand text-block ((contents text-block))
  (add-child (make-instance '<paragraph>
                            :children contents)))

(defcommand term ((term string))
  (make-instance '<term>
                 :term term
                 :text term))

(defcommand newterm ((term string))
  (make-instance '<new-term>
                 :term term
                 :text term))

(defcommand seevar ((name string))
  (make-instance '<seevar>
                 :name name))

(defcommand seefuns ((name string))
  (make-instance '<seefuns>
                 :name name))

(defcommand funref ((name string))
  (make-instance '<funref>
                 :name name))

(defcommand varref ((name string))
  (make-instance '<varref>
                 :name name))

;; we deliberately don't do anything with issues for now
(defcommand issue ((term string))
  (declare (ignore term))
  nil)

(defcommand endissue ((term string))
  (declare (ignore term))
  nil)

(defcommand ie () "i.e.")
