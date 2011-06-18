;; This software is Copyright (c) Chatsubo.net, LLC, May 1, 2011.
;; Chatsubo.net, LLC grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package #:cl-freeswitch)

(defvar *syslog-program* "cl-freeswitch")
(defvar *syslog-facility* :local6)

(defun prompts-dir (file)
  (format nil "~A/~A" *prompts-dir* file))

(defmacro when-let ((var form) &body body)
  "Evaluates FORM and binds VAR to the result, then executes BODY if VAR has a true value."
  `(let ((,var ,form))
     (when ,var ,@body)))

(defun true (i)
  (not (null i)))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defun gen-uuid (&optional name)
  (with-output-to-string (s)
    (when name (format s "~A-" name))
    (uuid:print-bytes s (uuid:make-v1-uuid))))

(defun logger (level msg &rest args)
  (when (or (not (eql level :debug)) *debug*)
    (syslog:log *syslog-program* 
		*syslog-facility* 
		level 
		(if (session? *session*) 
		    (format nil "~A : ~A" (uuid *session*) (apply #'format nil msg args))
		    (apply #'format nil msg args))
		syslog:+log-pid+)))

(defun parse-any-number (str)
  (cond ((stringp str)
	 (let ((str (remove #\, str)))
	   (cond ((cl-ppcre:scan "^[0-9]+$" str)
		  (parse-integer str))
		 ((cl-ppcre:scan "^[0-9]*[\.]*[0-9]+$" str)
		  (handler-case
		      (parse-number:parse-number str)
		    (error (c)
		      (declare (ignore c))
		      nil)))
		 ((cl-ppcre:scan "^[0-9]+[\.]*[0-9]*$" str)
		  (handler-case
		      (parse-number:parse-number str)
		    (error (c)
		      (declare (ignore c))
		      nil)))
		 (t
		  nil))))
	((numberp str)
	 str)
	(t nil)))

(defun my-split (string &key max (ws '(#\Space #\Tab)))
  "Split `string' along whitespace as defined by the sequence `ws'.
Whitespace which causes a split is elided from the result.  The whole
string will be split, unless `max' is provided, in which case the
string will be split into `max' tokens at most, the last one
containing the whole rest of the given `string', if any."
  (flet ((is-ws (char) (find char ws)))
    (nreverse
     (let ((list nil) (start 0) (words 0) end)
       (loop
	  (when (and max (>= words (1- max)))
	    (return (cons (subseq string start) list)))
	  (setf end (position-if #'is-ws string :start start))
	  (push (subseq string start end) list)
	  (incf words)
	  (unless end (return list))
	  (setf start (1+ end)))))))

(let ((ws '(#\Space #\Tab #\Newline #\Linefeed #\Return)))
  (defun trim-ws (str)
    (string-left-trim ws (string-right-trim ws str))))

(defun is-newline (s)
  "Match a newline or return character."
  (or (equalp s #\Newline) (equalp s #\Return) (equalp s #\Linefeed)))

(defun scan-newline (s)
  "Match a line of input if it consists of only line ending characters."
  (let ((matches 0))
    (map nil #'(lambda (c)
		 (when (or (char= #\Linefeed c)
			   (char= #\Newline c)
			   (char= #\Return c))
		   (incf matches))) s)
    (if (= matches (length s)) t nil)))

(defun flatten-alist (alist)
  (mapcan #'(lambda (item)
	      (list (car item) (cdr item)))
	  alist))

