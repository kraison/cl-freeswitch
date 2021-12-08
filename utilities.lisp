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
  (syslog:log *syslog-program* 
	      *syslog-facility* 
	      level 
	      ;;(let ((msg (format nil "TEST ~A" msg)))
	      (if (session? *session*) 
		  (format nil "~A : ~A" (uuid *session*) (apply #'format nil msg args))
		  (apply #'format nil msg args))
	      ;;)
	      syslog:+log-pid+))

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

(defun force-gc? (&optional (max 250000000))
  (> (memory-check) max))

(defun memory-check ()
  (let ((room (with-output-to-string (r)
                (let ((*standard-output* r))
                  (room)))))
    (with-input-from-string (s room)
      (do ((input (read-line s nil nil)
                  (read-line s nil nil)))
          ((null input))
        (cl-ppcre:register-groups-bind (memory-area size)
            ("^(.*)\:\\s+([0-9\,]+) bytes\." input)
          ;;(format t "GOT: ~A/~A~%" memory-area size)
          (when (cl-ppcre:scan "Dynamic space usage is" memory-area)
            (return-from memory-check
              (parse-integer (cl-ppcre:regex-replace-all "\," size "")))))))))

;;              ((cl-ppcre:scan "Read-only space usage is" memory-area)
;;               (format t "Read-only space: ~A~%" size))
;;              ((cl-ppcre:scan "Static space usage is" memory-area)
;;               (format t "Static space: ~A~%" size))
;;              ((cl-ppcre:scan "Control stack usage is" memory-area)
;;               (format t "Control stack space: ~A~%" size))
;;              ((cl-ppcre:scan "Binding stack usage is" memory-area)
;;               (format t "Binding stack space: ~A~%" size))
;;              ))))))


(defun flatten-alist (alist)
  (mapcan #'(lambda (item)
	      (list (car item) (cdr item)))
	  alist))

