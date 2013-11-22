(in-package #:clim3-command)

(defparameter *required-types* nil)

(defmacro clim3:define-command (name params &body body)
  (let* ((pos (position-if (lambda (item)
			     (member item lambda-list-keywords
				     :test #'eq))
			   params))
	 (required (subseq params 0 pos))
	 (rest (if (null pos) '() (subseq params pos)))
	 (params-sym (gensym))
	 (required-types (loop for param in required
			       collect (if (symbolp param)
					   t
					   (cadr param))))
	 (required-names (loop for param in params
			       collect (if (symbolp param)
					   param
					   (car param)))))
    `(defun ,name (&rest ,params-sym)
       (flet ((aux ,(append required-names rest)
		,@body))
	 (if *required-types*
	     ',required-types
	     (apply #'aux ,params-sym))))))

(defun acquire-arguments (required-types initial-arguments)
  (loop for argument in initial-arguments
	for i from 0
	collect (if (eq argument 'clim3:?)
		    (let ((clim3-ext:*input-context* (elt required-types i)))
		      (catch :accept
			(clim3:event-loop clim3:*port*)))
		    argument)))

(defgeneric command-name-in-table-p (command-name command-table))

(defclass clim3:command-table ()
  ((%commands-names :initarg :command-names :reader command-names)))

(defclass clim3:hashed-command-table (clim3:command-table) ())

(defmethod command-name-in-table-p
    (command-name (command-table clim3:hashed-command-table))
  (gethash command-name (command-names command-table)))

(defun clim3:active-command-p (command-name)
  (and (typep clim3-ext:*command-table* 'clim3:command-table)
       (command-name-in-table-p command-name clim3-ext:*command-table*)))

(defun clim3:command-loop (command-table)
  (loop do (catch 'abort
	     (let* ((action
		      (let ((clim3-ext:*input-context* 'nil)
			    (clim3-ext:*command-table* command-table))
			(catch :accept
			  (clim3:event-loop clim3:*port*))))
		    (command-name
		      (if (symbolp action) action (car action)))
		    (initial-arguments 
		      (if (symbolp action) '() (cdr action)))
		    (required-types
		      (let ((*required-types* t))
			(funcall command-name)))
		    (arguments
		      (acquire-arguments required-types initial-arguments)))
	       (apply command-name arguments)))))

(clim3:define-command clim3:abort ()
  (throw 'abort nil))
