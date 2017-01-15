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

(defun command-defined-p (name)
  (not (null (gethash (string-downcase name)
                      *commands*))))

(defun define-command (name func)
  (setf (gethash name *commands*) func))

(defmacro defcommand (name args &body body)
  (let* ((lambda-args (mapcar #'(lambda (&rest rest)
                                  (declare (ignore rest))
                                  (gensym)) args))
         (let-args (mapcar #'(lambda (args larg)
                               `(,(car args) (run-command-argparser
                                              ,(string-downcase (symbol-name (cadr args)))
                                              ,larg)))
                               args lambda-args)))
    `(define-command (string-downcase (symbol-name ',name))
           (lambda ,lambda-args
             (let ,let-args ,@body)))))

(defun run-command (name arguments &key (pass-if-empty nil))
  (let ((lc-name (string-downcase name)))
    (cond
      ((gethash lc-name *commands*) (apply (gethash lc-name *commands*) arguments))
      ((and *document* (user-command-defined-p lc-name))
       (apply (get-user-command lc-name) arguments))
      (t (if pass-if-empty
             (list (format nil "command with a name ~s is not defined" lc-name))
             (error "command with a name ~s is not defined" lc-name))))))

(defcommand-argparser string
  (apply #'concatenate 'string (mapcar #'contents argument)))

(defcommand-argparser noop argument)

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
                     (if (listp result)
                         (loop for item in result
                               do (push item nodes))
                         (push result nodes)))))
             (add-string (token)
               (push (contents token) accumulator)))

      (loop for token in argument
            do (cond
                 ;; catch case to discover formulas that we cannot parse (yet)
                 ((and (typep token 'token) (string= (contents token) "$"))
                  (error "Orphan formula marker found."))
                 ((typep token 'token) (add-string token))
                 ((typep token '<command>) (add-command token))
                 (t (error "Unknown token inside of text block ~a" token))))
      (flush-accumulator)
      (reverse nodes))))

(defcommand-argparser displaythree-list
  (labels ((cast-elem (token)
             (cond
               ((typep token 'token) (contents token))
               ((null token) "")
               ((and (typep token '<command>)
                     (string= (name token) "tt"))
                (cast-list (args token))
               )))
           (cast-list (list)
             (apply #'concatenate 'string
                    (mapcar #'cast-elem list)))
           (create-func (name)
             (make-instance '<displaythree-func>
                            :name name))
           )
  (multiple-value-bind (successp arguments rest) (tex-displaythree-argument-parser argument)
    (progn
      (when (not successp)
        (error "Unable to parse second argument for displaythree command"))
      (when (> (length rest) 0)
        (error "After successful parsing of sedond argument of display three command some trail of tokens remains which is ferbidden"))
      (mapcar #'create-func
              (mapcar #'cast-list arguments))))))

(defcommand-argparser formula-arg
  (labels ((run (command) (run-command (name command) (args command))))
    (mapcar #'run argument)))

(defcommand input ((fname string))
  (tokenize-dpans-file fname))

(defcommand beginchapter ((chapnum string) (title string) (ref string) (ref-title string) )
  (add-child-and-enter (make-instance '<chapter>
                                      :chap-number chapnum
                                      :title title
                                      :ref ref
                                      :ref-title ref-title)))

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

(defcommand endchapter ()
  (close-child-and-go-up "chapter"))

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

(defcommand def ((name string) (value text-block))
  (if (or (user-command-defined-p name) (user-command-defined-p name))
      (format t "Command ~s is already defined, skipping~%" name)
      (define-user-command name (lambda () value))))

(defcommand definesection ((name string))
  (mark-as-section name))

(defcommand text-block ((contents text-block))
  (add-child (make-instance '<paragraph>
                            :children contents)))

(defcommand term ((term string))
  (make-instance '<term>
                 :term term
                 :text term))

(defcommand newtermidx ((text string) (term string))
  (make-instance '<term>
                 :term term
                 :text text))

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

(defcommand typeref ((name string))
  (make-instance '<typeref>
                 :name name))

(defcommand chapref ((name string))
  (make-instance '<chapref>
                 :name name))

(defcommand oftype ((type noop))
  (list "of "
        (run-command "term" `((,(make-instance 'token :contents "type"))))
        " "
        (run-command "typeref" (list type))))

(defcommand thefunction ((name noop))
  (list "The "
        (run-command "term" `((,(make-instance 'token :contents "function"))))
        " "
        (run-command "funref" (list name))))

(defcommand metavar ((name string))
  (make-instance '<metavar>
                 :name name))

;; we deliberately don't do anything with issues for now
(defcommand issue ((term string))
  (declare (ignore term))
  nil)

(defcommand endissue ((term string))
  (declare (ignore term))
  nil)

(defcommand bye () "")

(defcommand ie () "i.e.")

(defcommand thenextfigure () "The next figure")

(defcommand displaythree ((title string) (contents displaythree-list))
  (add-child (make-instance '<displaythree>
                            :title title
                            :children contents
                            )))

;; formula stuff
(defcommand formula ((contents formula-arg))
  (make-instance '<formula>
                 :children contents))

(defcommand formula-sub ((contents formula-arg))
  (make-instance '<formula-sub>
                 :children contents))

(defcommand formula-symbol ((symbol string))
  (make-instance '<formula-symbol>
                 :name symbol))
