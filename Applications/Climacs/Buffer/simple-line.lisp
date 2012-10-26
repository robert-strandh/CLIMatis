(in-package #:climacs-buffer-simple-line)

(defclass location ()
  ((%prev :initarg :prev :accessor prev)
   (%next :initarg :next :accessor next)
   (%object :initarg :object :accessor object)))
   
(defclass simple-line (line)
  ((%objects
    :initform (make-instance 'location :prev nil :next nil :object nil)
    :accessor objects)))

(defclass simple-cursor (attached-cursor)
  ((%location :initarg :location :accessor location)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Line contents.

(defmethod object-count ((line simple-line))
  (loop for location = (next (objects line)) then (next location)
	until (null location)
	count t))

(defmethod contents ((line simple-line) &key (start 0) (end nil))
  (let ((object-count (object-count line)))
    (unless (typep start `(integer 0 ,object-count))
      (error "wrong type"))
    (unless (typep end `(or null (integer 0 ,object-count)))
      (error "wrong type"))
    (when (null end)
      (setf end object-count)))
  (let ((first (next (objects line))))
    (loop repeat start
	  do (setf first (next first)))
    (let ((result (make-array (- end start))))
      (loop repeat (- end start)
	    for i from 0
	    do (setf (aref result i) (object first))
	       (setf first (next first)))
      result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Cursor operations.

(defun normalize-cursor (cursor)
  (with-accessors ((location location)) cursor
    (loop while (and (not (null (prev location)))
		     (not (null (next location)))
		     (eq (prev location) (next location)))
	  do (setf location (next location)))))

(defmethod beginning-of-line-p ((cursor simple-cursor))
  (normalize-cursor cursor)
  (null (prev (location cursor))))

(defmethod end-of-line-p ((cursor simple-cursor))
  (normalize-cursor cursor)
  (null (next (location cursor))))

(defmethod object-at-cursor ((cursor simple-cursor))
  (normalize-cursor cursor)
  (object (next (location cursor))))

(defmethod move-forward ((cursor simple-cursor) &optional (count 1))
  (normalize-cursor cursor)
  (with-accessors ((location location)) cursor
    (let ((new location))
      (loop repeat count
	    until (null new)
	    do (setf new (next new)))
      (if (null new)
	  (error "end of line")
	  (setf location new)))))

(defmethod move-backward ((cursor simple-cursor) &optional (count 1))
  (normalize-cursor cursor)
  (with-accessors ((location location)) cursor
    (let ((new location))
      (loop repeat count
	    until (null new)
	    do (setf new (prev new)))
      (if (null new)
	  (error "beginning of line")
	  (setf location new)))))

(defmethod insert-object ((cursor simple-cursor) object)
  (normalize-cursor cursor)
  (with-accessors ((location location)) cursor
    (let ((new (make-instance 'location
			      :prev location
			      :next (next location)
			      :object object)))
      (unless (null (next location))
	(setf (prev (next location)) new))
      (setf (next location) new)
      (setf location new))))

(defmethod delete-object ((cursor simple-cursor))
  (normalize-cursor cursor)
  (with-accessors ((location location)) cursor
    (let ((next (next location)))
      (unless (null (next next))
	(setf (prev (next next)) location))
      (setf (next location) (next next))
      (setf (object next) nil)
      (setf (next next) location))))
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Attaching a cursor to a simple line.

(defmethod (setf line) :before ((new-line simple-line) (cursor unattached-cursor))
  (change-class cursor 'simple-cursor)
  (setf (location cursor) (objects new-line)))
