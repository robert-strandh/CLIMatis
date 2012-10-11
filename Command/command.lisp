(in-package #:clim3-command)

(defclass value-descriptor ()
  ((%presentation-type :initarg :presentation-type :reader presentation-type)
   (%prompt :initarg :prompt :reader prompt)
   (%message-function :initarg :message-function :reader message-function)))

;;; Temporary definition to test other things
;;; Should add a stream parameter for instance.
(defun accept (value-descriptor)
  (format *query-io* "~a: " (prompt value-descriptor))
  (finish-output *query-io*)
  (loop for input = (read *query-io*)
	until (typep input (presentation-type value-descriptor))
	do (funcall (message-function value-descriptor) input)
	   (format *query-io* "~a: " (prompt value-descriptor))
	   (finish-output *query-io*)
	finally (return input)))

(defclass function-invoker ()
  ((%called-function :initarg :called-function :reader called-function)
   (%value-descriptors :initarg :value-descriptors
			   :reader value-descriptors)
   (%invoker-function :initarg :invoker-function :reader invoker-function)))

(defun invoke-function (function-invoker)
  (funcall (invoker-function function-invoker)
	   (called-function function-invoker)
	   (value-descriptors function-invoker)))

(defun simple-invoker-function (called-function value-descriptors)
  (apply called-function
	 (mapcar #'accept value-descriptors)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Command table

(defclass command-table () ())

;;; A command table with entries indexed by characters or
;;; other elementary gestures.
(defclass keyboard-command-table (command-table)
  ((%entries :initform (make-hash-table :test #'equal) :reader entries)))

;;; A command table with an ordered set of entries.
(defclass menu-command-table (command-table)
  ((%entries :initfarg :entries :initform '() :accessor entries)))

;;; A flat command table with entries being indexed by character
;;; strings.
(defclass name-command-table (command-table)
  ((%entries :initform (make-hash-table :test #'equal) :reader entries)))
