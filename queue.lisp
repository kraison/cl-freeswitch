;; This software is Copyright (c) Chatsubo.net, LLC, May 1, 2011.
;; Chatsubo.net, LLC grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package #:cl-freeswitch)

;; The following queueing code was borrowed and adapted from Russell & Norvig's
;; "Introduction to AI"
(defun print-queue (q stream depth)
  (format stream "<QUEUE: ~a>" (queue-elements q)))

(defstruct (queue
             (:print-function print-queue))
  (key #'identity)
  (last nil)
  (lock (make-recursive-lock))
  (elements nil))

(defun make-empty-queue () (make-queue))

(defun empty-queue? (q)
  (with-recursive-lock-held ((queue-lock q))
    (= (length (queue-elements q)) 0)))

(defun queue-front (q)
  (with-recursive-lock-held ((queue-lock q))
    (elt (queue-elements q) 0)))

(defun dequeue (q)
  (with-recursive-lock-held ((queue-lock q))
    (pop (queue-elements q))))

(defun enqueue-at-front (q items)
  (with-recursive-lock-held ((queue-lock q))
    (cond ((null items) nil)
	  ((or (null (queue-last q)) (null (queue-elements q)))
	   (setf (queue-last q) (last items)
		 (queue-elements q) (nconc items (queue-elements q))))
	  (t (setf (queue-elements q) (nconc items (queue-elements q)))))))

(defun enqueue-at-front-1 (q item)
  (enqueue-at-front q (list item)))

(defun enqueue (q items)
  (with-recursive-lock-held ((queue-lock q))
    (cond ((null items) nil)
	  ((or (null (queue-last q)) (null (queue-elements q)))
	   (setf (queue-last q) (last items)
		 (queue-elements q) (nconc (queue-elements q) items)))
	  (t (setf (cdr (queue-last q)) items
		   (queue-last q) (last items))))))

(defun enqueue-1 (q item)
  (enqueue q (list item)))

(defun queue-length (q)
  (with-recursive-lock-held ((queue-lock q))
    (length (queue-elements q))))
;; End of adapted code
