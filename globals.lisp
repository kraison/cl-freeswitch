(in-package #:cl-freeswitch)

(defconstant +new-connection-wait-time+ 1)
(defvar *listener-thread* nil)
(defvar *stop-listener* nil)
(defvar *thread-pool-size* 30)
(defvar *id* 1)
(defvar *port* 8084)
(defvar *fs-host* "127.0.0.1")
(defvar *fs-port* 8021)
(defvar *fs-auth* "miadmin")
(defvar *configuration-file* "cl-freeswitch.cfg")
(defvar *configuration* nil)
(defvar *pstn-gateway* "mi-pstn-gw.corp.mostgifted.com")
(defvar *prompts-dir* "/home/io/prompts")

(defvar *thread-list-lock* (make-recursive-lock))
(defvar *thread-list* nil)

(defconstant +buflen+ 64)

#+sbcl (defvar *extension-table* (make-hash-table :test 'equalp :synchronized t))
#+lispworks (defvar *extension-table* (make-hash-table :test 'equalp :single-thread nil))
#+sbcl (defvar *sessions* (make-hash-table :test 'equalp :synchronized t))
#+lispworks (defvar *sessions* (make-hash-table :test 'equalp :single-thread nil))

(defvar *session-history-recording* nil)
(defvar *session* nil)
(defvar *input* nil)

(defvar *mail-relay* "mail.chatsubo.net")
(defvar *admin-emails* (list "pager@chatsubo.net"))
