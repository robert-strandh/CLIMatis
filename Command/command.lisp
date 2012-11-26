(in-package #:clim3-command)

(defparameter *acquire-arguments* nil)

(defun acquire-args (parameters args)
  (declare (ignore parameters args))
  nil)

(defmacro define-command (name params &body body)
  (let ((params-sym (gensym)))
    `(defun ,name (&rest ,params-sym)
       (flet ((aux ,(mapcar #'car params)
		,@body))
	 (if *acquire-arguments*
	     (apply #'aux (acquire-args ,(mapcar #'cadr params) ,params-sym))
	     (apply #'aux ,params-sym))))))

