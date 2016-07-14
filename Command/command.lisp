(in-package #:clim3-command)

(defclass command (standard-generic-function)
  ((%types :initarg :types :reader types))
  (:metaclass #.(class-name (class-of (find-class 'standard-generic-function)))))

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
	 (required-names (loop for param in required
			       collect (if (symbolp param)
					   param
					   (car param)))))
    `(defun ,name (&rest ,params-sym)
       (flet ((aux ,(append required-names rest)
		,@body))
	 (if *required-types*
	     ',required-types
	     (apply #'aux ,params-sym))))))

(defgeneric clim3:command-table (view))

(defgeneric clim3:acquire-action (view))

(defmethod clim3:acquire-action (view)
  (let ((clim3-ext:*command-table* (clim3:command-table view)))
    (catch :accept
      (clim3:event-loop clim3:*port*))))

(defun acquire-arguments (required-types initial-arguments)
  (loop for argument in initial-arguments
	for i from 0
	collect (if (eq argument 'clim3:?)
		    (let ((clim3-ext:*input-context* (elt required-types i)))
		      (catch :accept
			(clim3:event-loop clim3:*port*)))
		    argument)))

(defgeneric clim3:command-name-in-table-p (command-name command-table))

(defclass clim3:command-table ()
  ((%commands-names :initarg :command-names :reader command-names)))

(defclass clim3:hashed-command-table (clim3:command-table) ())

(defmethod clim3:command-name-in-table-p
    (command-name (command-table clim3:hashed-command-table))
  (gethash command-name (command-names command-table)))

(defclass clim3:list-command-table (clim3:command-table) ())

(defmethod clim3:command-name-in-table-p
    (command-name (command-table clim3:list-command-table))
  (member command-name (command-names command-table)))

(defun clim3:active-command-p (command-name)
  (and (typep clim3-ext:*command-table* 'clim3:command-table)
       (clim3:command-name-in-table-p command-name clim3-ext:*command-table*)))

(defgeneric clim3:command-loop-iteration (application view))

(defmethod clim3:command-loop-iteration (application view)
  (declare (ignore application))
  (let* ((action (clim3:acquire-action view))
	 (command-name
	   (if (symbolp action) action (car action)))
	 (initial-arguments 
	   (if (symbolp action) '() (cdr action)))
	 (required-types
	   (let ((*required-types* t))
	     (funcall command-name)))
	 (arguments
	   (acquire-arguments required-types initial-arguments)))
    (apply command-name arguments)))
  

(defun clim3:command-loop ()
  (let ((clim3-ext:*input-context* 'nil))
    (loop do (let ((view (clim3:current-view clim3:*application*)))
	       (catch 'abort
		 (clim3:command-loop-iteration clim3:*application* view))))))

(clim3:define-command clim3:abort ()
  (throw 'abort nil))
