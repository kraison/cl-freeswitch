(in-package #:cl-user)
#+sbcl (require 'sb-queue)

(defpackage #:cl-freeswitch
  (:use #:cl #:bordeaux-threads)
  (:export #:start
	   #:stop
	   #:logger
	   #:all-threads
	   #:all-scheduled-functions
	   #:all-calls
	   #:notify
	   #:*id*

	   ;; Session slot accessors and methods
	   #:*session*
	   #:*sessions*
	   #:session?
	   #:lookup-session
	   #:uuid
	   #:finished?
	   #:handler
	   #:fs-stream
	   #:fs-host
	   #:caller-id
	   #:destination
	   #:db-connection
	   #:raw-connect-info
	   #:shutdown-session
	   #:get-session-var
	   #:set-session-var
	   #:get-session-vars
	   #:set-session-vars
	   #:*session-history-recording*
	   #:session-history
	   #:flush-session-inputs
	   #:lock
	   #:ipc-lock
	   #:set-continuation
	   #:kill-invalid-sessions
	   #:list-valid-session-uuids
	   #:list-invalid-sessions

	   ;; Freeswitch API
	   #:*extension-table*
	   #:fs-command
	   #:fs-command-ok?
	   #:fs-fetch
	   #:def-extension
	   #:def-operator-action
	   #:def-operator-menu
	   #:def-dtmf-combo-menu
	   #:def-operator-choice-handler
	   #:def-custom-operator-action
	   #:def-operator-plan
           #:session-let
	   #:set-next-step
           #:take-operator-action
	   #:set-dtmf-handler
	   #:unset-dtmf-handler
	   #:set-end-call-handler
	   #:unset-end-call-handler
	   #:set-error-handler
	   #:unset-error-handler
	   #:*operator-methods*
	   #:export-as-operator-method
	   #:operator-method?
	   #:originate

	   ;; FIFO Queue
	   #:create-fifo-queue
	   #:fifo-queue-status
	   #:fifo-login
	   #:fifo-logout
	   #:fifo-record-current-call
	   #:fifo-view-current-call
	   ))

