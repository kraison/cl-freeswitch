;; This software is Copyright (c) Chatsubo.net, LLC, May 1, 2011.
;; Chatsubo.net, LLC grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package #:cl-freeswitch)


(defvar *operator-methods* (make-hash-table :test #'equalp))

(defun my-scan (a b)
  (cl-ppcre:scan a b))

(defun export-as-operator-method (func function-name)
  (setf (gethash function-name *operator-methods*) func))

(defmacro set-dtmf-handler (lst &key (break? t))
  "Set DTMF handlers from a list of pairs. Handler will send a break command and then execute the
continuation.  If break? is true, event-lock will be used and the DTMF continuation will not be
called until after the break command returns."
  (let ((input (gensym)) (status (gensym)) (digit (gensym)) (action (gensym)) 
	(i (gensym)) (s (gensym)) (l (gensym)) (b? (gensym)))
    `(let ((,l ',lst) (,b? ,break?))
       (sb-ext:compare-and-swap
	(dtmf-handler *session*)
	(dtmf-handler *session*)
	#'(lambda (,input ,status)
	    (let ((,digit (fs-fetch :dtmf-digit ,input)))
	      (logger :debug "DTMF HANDLER GOT ~A" ,digit)
	      (let ((,action (second (find ,digit ,l :test 'equalp :key 'first))))
		(if (and ,action (or (fboundp ,action) (functionp ,action)))
		    (if ,b?
			(progn
			  (logger :debug "DTMF HANDLER CALLING BREAK AND THEN ~A FOR ~A" 
				  ,action ,digit)
			  (set-continuation #'(lambda (,i ,s) (funcall ,action ,i ,s))
					    (recognizer *session*))
			  (handler-case
			      (fs-command (fs-stream *session*) 
					  :break nil
					  :uuid (uuid *session*)
					  :event-lock ,b?)
			    (stream-error (c)
			      (logger :err "PROBLEM WITH FS-COMMAND: ~A" c))))
			(progn
			  (logger :debug "DTMF HANDLER CALLING ~A FOR ~A" ,action ,digit)
			  (funcall ,action ,input ,status)))
		    (logger :err "NO DTMF HANDLER DEFINED FOR ~A" ,digit)))))))))

(defmacro def-operator-action (name continuation recognizer &rest args)
  "Defines a function and registers it as an operator target."
  `(progn
     (defun ,name (input status)
       (declare (ignore input status))
       (logger :debug "IN OPERATOR METHOD: ~A" ',name)
       (set-continuation ,continuation ,recognizer)
       (handler-case
	   (fs-command (fs-stream *session*) 
		       ,recognizer 
		       ,(if args `(list ,@args) nil) 
		       :uuid (uuid *session*))
	 (stream-error (c)
	   (logger :err "PROBLEM WITH FS-COMMAND: ~A" c))))
     (export-as-operator-method #',name (intern (string-downcase (symbol-name ',name)) 'keyword))
     (export-as-operator-method #',name (string-downcase (symbol-name ',name)))))

(defmacro def-operator-menu (name failure-func choices min max tries timeout terminator menu-file 
			invalid-file &optional regex)
  (let ((choice (gensym)) (i (gensym)) (s (gensym)) (args (gensym)) (dtmf-handler (gensym)))
    `(progn
       (defun ,name (input status)
	 (let ((,dtmf-handler (dtmf-handler *session*)))
	   (logger :debug "IN OPERATOR METHOD: ~A" ',name)
	   (unset-dtmf-handler)
	   (set-continuation 
	    #'(lambda (,i ,s)
		(when (functionp ,dtmf-handler)
		  (setf (dtmf-handler *session*) ,dtmf-handler))
		(if (eql ,s :user-failure)
		    (funcall ,failure-func ,i ,s)
		    (let ((,choice (fs-fetch :variable-user-input ,i)))
		      (logger :debug "EXAMINING USER INPUT ~A" ,choice)
		      (cond
			,@(mapcar #'(lambda (c)
				      (cond ((and (consp (first c)) 
						  (eql (first (first c)) :regex))
					     `((my-scan ,(second (first c)) ,choice)
					       (logger :debug "INPUT ~A MATCHED REGEX ~A" 
						       ,choice ,(second (first c)))
					       (set-session-var :user-input ,choice)
					       (set-session-var :retries 0)
					       (funcall ,(second c) input status)))
					    ((stringp (first c))
					     `((equalp ,choice ,(first c))
					       (set-session-var :user-input ,choice)
					       (set-session-var :retries 0)
					       (funcall ,(second c) input status)))))
				  choices)
			(t (funcall #',name input status))))))
	    :play-and-get-digits)
	   (let ((,args 
		  ,(if regex
		       `(list ,min ,max ,tries ,timeout ,terminator ,menu-file ,invalid-file ,regex)
		       `(list ,min ,max ,tries ,timeout ,terminator ,menu-file ,invalid-file))))
	     (handler-case
		 (fs-command (fs-stream *session*) 
			     :play-and-get-digits 
			     ,args
			     :uuid (uuid *session*))
	       (stream-error (c)
		 (logger :err "PROBLEM WITH FS-COMMAND: ~A" c))))))
       (export-as-operator-method #',name (intern (string-downcase (symbol-name ',name)) 'keyword))
       (export-as-operator-method #',name (string-downcase (symbol-name ',name))))))

(defun handle-menu-dtmf (input status choices min max tries menu-file invalid-file old-dtmf-handler)
  (let ((digit (fs-fetch :dtmf-digit input))
	(menu-digits (get-session-var :menu-digits)))
    (logger :debug "HANDLE-MENU-DTMF GOT ~A" digit)
    (let* ((digits (format nil "~A~A" menu-digits digit))
	   (digit-length (length digits)))
      (set-session-var :menu-digits digits)
      (cond ((> digit-length max)
	     ;; break, play invalid file, check retries, replay menu file
	     )
	    ((>= digit-length min)
	     (let ((cont 
		    (loop for c in choices do
			 (destructuring-bind (regex action) c
			   (when (cl-ppcre:scan (format nil "^~A$" regex) digits)
			     (return action))))))
	       (if cont
		   (handler-case
		       (progn
			 (fs-command (fs-stream *session*) 
				     :break nil
				     :uuid (uuid *session*)
				     :event-lock t)
			 (funcall action input status))
		     (stream-error (c)
		       (logger :err "PROBLEM IN handle-menu-dtmf: ~A" c)))
		   ;; Handle bad choices
		   nil)))))))

#|
(defmacro def-operator-menu-new (name failure-func choices min max tries timeout terminator 
				 menu-file invalid-file)
  (with-gensyms (i s dtmf-handler dtmf-input dtmf-status)
    `(progn
       (defun ,name (input status)
	 (let ((,dtmf-handler (dtmf-handler *session*)))
	   (logger :debug "IN OPERATOR METHOD: ~A" ',name)
	   ;; Reset DTMF handler to our special closure
	   (unset-dtmf-handler)
	   (set-session-var :menu-digits "")
	   (sb-ext:compare-and-swap (dtmf-handler *session*)
				    (dtmf-handler *session*)
				    #'(lambda (,dtmf-input ,dtmf-status)
					(handle-menu-dtmf ,dtmf-input 
							  ,dtmf-status 
							  ',choices)))
	   (set-next-step #'(lambda (,i ,s)
			      ;; FIXME: use timer, check retires, replay file
			      )
			  :play ,menu-file)))
       (export-as-operator-method #',name (intern (string-downcase (symbol-name ',name)) 'keyword))
       (export-as-operator-method #',name (string-downcase (symbol-name ',name))))))
|#

(defmacro def-dtmf-combo-menu (name failure-func choices min max tries timeout terminator menu-file
			       invalid-file &optional regex)
  (let ((choice (gensym)) (i (gensym)) (s (gensym)) (args (gensym)) (dtmf-handler (gensym)))
    `(progn
       (defun ,name (input status)
         (let ((,dtmf-handler (dtmf-handler *session*)))
           (logger :debug "IN OPERATOR METHOD: ~A" ',name)
           ;;(unset-dtmf-handler)
           (set-continuation
            #'(lambda (,i ,s)
                (when (functionp ,dtmf-handler)
                  (setf (dtmf-handler *session*) ,dtmf-handler))
                (if (eql ,s :user-failure)
                    (funcall ,failure-func ,i ,s)
                    (let ((,choice (fs-fetch :variable-user-input ,i)))
                      (logger :debug "EXAMINING USER INPUT ~A" ,choice)
                      (cond
                        ,@(mapcar #'(lambda (c)
                                      (cond ((and (consp (first c))
                                                  (eql (first (first c)) :regex))
                                             `((my-scan ,(second (first c)) ,choice)
                                               (logger :debug "INPUT ~A MATCHED REGEX ~A" ,choice ,(second (first c)))
                                               (set-session-var :user-input ,choice)
                                               (set-session-var :retries 0)
                                               (funcall ,(second c) input status)))
                                            ((stringp (first c))
                                             `((equalp ,choice ,(first c))
                                               (set-session-var :user-input ,choice)
                                               (set-session-var :retries 0)
                                               (funcall ,(second c) input status)))))
                                  choices)
                        (t (funcall #',name input status))))))
            :play-and-get-digits)
           (let ((,args
                  ,(if regex
                       `(list ,min ,max ,tries ,timeout ,terminator ,menu-file ,invalid-file ,regex)
                       `(list ,min ,max ,tries ,timeout ,terminator ,menu-file ,invalid-file))))
             (handler-case
                 (fs-command (fs-stream *session*)
                             :play-and-get-digits
                             ,args
                             :uuid (uuid *session*))
               (stream-error (c)
                 (logger :err "PROBLEM WITH FS-COMMAND: ~A" c))))))
       (export-as-operator-method #',name (intern (string-downcase (symbol-name ',name)) 'keyword))
       (export-as-operator-method #',name (string-downcase (symbol-name ',name))))))

(defmacro def-operator-choice-handler (name tries parent-func failure-func choices)
  (let ((choice (gensym)))
    `(progn
       (defun ,name (input status)
	 (logger :debug "IN OPERATOR METHOD: ~A" ',name)
	 (if (eql status :user-failure)
	     (funcall ,failure-func input status)
	     (let ((,choice (fs-fetch :variable-user-input input)))
	       (cond
		 ,@(mapcar #'(lambda (c)
			       `((equalp ,choice ,(first c))
				 (set-session-var :user-input ,choice)
				 (set-session-var :retries 0)
				 (funcall ,(second c) input status)))
			   choices)
		  ((and (numberp (get-session-var :tries)) (>= (get-session-var :tries) ,tries))
		      (funcall ,failure-func input status))
		  (t
		   (if (numberp (get-session-var :tries))
		       (set-session-var :tries (+ 1 (get-session-var :tries)))
		       (set-session-var :tries 1))
		   (funcall ,parent-func input status))))))
       (export-as-operator-method #',name (intern (string-downcase (symbol-name ',name)) 'keyword))
       (export-as-operator-method #',name (string-downcase (symbol-name ',name))))))

(defmacro def-custom-operator-action (name lambda-list &body body)
  `(progn
    (defun ,name ,lambda-list 
      (logger :debug "IN OPERATOR METHOD: ~A" ',name)
      ,@body)
    (export-as-operator-method #',name (intern (string-downcase (symbol-name ',name)) 'keyword))
    (export-as-operator-method #',name (string-downcase (symbol-name ',name)))))

(defmacro set-next-step (continuation recognizer &rest args)
  `(progn
     (set-continuation ,continuation ,recognizer)
     (handler-case
	 (fs-command (fs-stream *session*)
		     ,recognizer 
		     ,(if args `(list ,@args) nil)
		     :uuid (uuid *session*))
       (stream-error (c)
	 (logger :err "PROBLEM WITH FS-COMMAND: ~A" c)))))

(defmacro def-operator-plan (&rest steps)
  `(progn
     ,@(mapcar #'(lambda (s)
		   `(def-operator-action ,@(mapcar #'(lambda (i) i) s)))
	       steps)))

(defun operator-method? (name)
  (gethash name *operator-methods*))

(defun def-extension (extension function)
  (if (or (functionp function) (fboundp function))
      (setf (gethash (format nil "~A" extension) *extension-table*) function)
      (error "def-extension must be given a valid function.  You gave ~A" function)))

(defun extension-exists? (extension)
  (gethash (format nil "~A" extension) *extension-table*))

(defun undef-extension (extension)
  (remhash (format nil "~A" extension) *extension-table*))
