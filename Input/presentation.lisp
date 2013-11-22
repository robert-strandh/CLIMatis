(cl:in-package #:clim3-input)

(defparameter clim3-ext:*input-context* nil)

(defparameter clim3-ext:*command-table* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function PTYPEP. 
;;;
;;; At the moment, this function is equivalent to the Common Lisp
;;; function TYPEP.  Later, it will also check additional parameters
;;; of the presentation type. 

(defun clim3:ptypep (object ptype)
  (typep object ptype))  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class PRESENTATION.
;;;
;;; This class is a subclass of VISIT.  In addition, it contains an
;;; underlying object, called the GEM.  

(defclass clim3:presentation (clim3:visit clim3:clickable)
  ((%gem :initarg :gem :reader clim3:gem)))

(defmethod clim3:enter progn ((zone clim3:presentation))
  (when (clim3:ptypep (clim3:gem zone) clim3-ext:*input-context*)
    (clim3:highlight zone)
    (clim3:attention zone)))

(defmethod clim3:leave progn ((zone clim3:presentation))
  (clim3:unhighlight zone)
  (clim3:at-ease zone))

(defmethod clim3:button-press progn ((zone clim3:presentation) button)
  (when (and (clim3:ptypep (clim3:gem zone) clim3-ext:*input-context*)
	     (equal button '(:button-1)))
    (throw :accept (clim3:gem zone))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class ACTION

(defgeneric clim3:action (zone))

(defclass clim3:action (clim3:visit clim3:clickable)
  ((%command-name :initarg :command-name :reader command-name)))
    
(defmethod clim3:enter progn ((zone clim3:action))
  (when (and (eq clim3-ext:*input-context* 'nil)
	     (clim3:active-command-p (command-name zone)))
    (clim3:highlight zone)
    (clim3:attention zone)))

(defmethod clim3:leave progn ((zone clim3:action))
  (clim3:unhighlight zone)
  (clim3:at-ease zone))

(defmethod clim3:button-press progn ((zone clim3:action) button)
  (when (and (eq clim3-ext:*input-context* 'nil)
	     (equal button '(:button-1)))
    (throw :accept (clim3:action zone))))

