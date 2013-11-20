(in-package #:clim3-command)

(defparameter *parameter-types* nil)

(defmacro clim3:define-command (name params &body body)
  (let ((params-sym (gensym))
	(parameter-types (loop for param in params
			       collect (if (symbolp param)
					   t
					   (cadr param))))
	(parameter-names (loop for param in params
			       collect (if (symbolp param)
					   param
					   (car param)))))
    `(defun ,name (&rest ,params-sym)
       (flet ((aux ,parameter-names
		,@body))
	 (if *parameter-types*
	     ',parameter-types
	     (apply #'aux ,params-sym))))))

(defun acquire-arguments (parameter-types initial-arguments)
  (loop for parameter-type in parameter-types
	for argument in initial-arguments
	collect (if (eq argument 'clim3:?)
		    (let ((clim3-ext:*input-context* parameter-type))
		      (catch :object
			(clim3:event-loop clim3:*port*)))
		    argument)))

(defclass clim3:command-table ()
  ((%commands-names :initarg :command-names :reader command-names)))

(defgeneric clim3:active-command-p (command-name command-table))

(defclass clim3:hashed-command-table (clim3:command-table) ())

(defmethod clim3:active-command-p
    (command-name (command-table clim3:hashed-command-table))
  (gethash command-name (command-names command-table)))

(defun clim3:command-loop (command-table)
  (loop do (catch 'abort
	     (let* ((action
		      (let ((clim3-ext:*input-context* 'clim3:command-table)
			    (clim3-ext:*command-table* command-table))
			(catch :object
			  (clim3:event-loop clim3:*port*))))
		    (command-name
		      (if (symbolp action) action (car action)))
		    (initial-arguments 
		      (if (symbolp action) '() (cdr action)))
		    (parameter-types
		      (let ((*parameter-types* t))
			(funcall command-name)))
		    (arguments
		      (acquire-arguments parameter-types initial-arguments)))
	       (apply command-name arguments)))))

(clim3:define-command clim3:abort ()
  (throw 'abort nil))
