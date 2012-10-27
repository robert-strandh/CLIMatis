(in-package #:climacs-buffer-simple-buffer)

(defclass hook ()
  ((%line :initarg :line :reader line)))

(defun make-initial-line ()
  (make-instance 'climacs-buffer-simple-line:simple-line))

(defun make-initial-hook ()
  (make-instance 'hook :line (make-initial-line)))

(defun make-initial-contents ()
  (vector (make-initial-hook)))

(defclass simple-buffer ()
  ((%hooks :initform (make-initial-contents) :accessor hooks)))

(defmethod line-count ((buffer simple-buffer))
  (length (hooks buffer)))

(defmethod object-count ((buffer simple-buffer))
  (+ (loop for hook in (hooks buffer)
	   sum (climacs-buffer-line:object-count (line hook)))
     (* (1- (length (hooks buffer)))
	(newline-size buffer))))

(defmethod preceding-object-count ((buffer simple-buffer) line)
  (let ((hook-of-line (climacs-buffer-line:buffer-hook line)))
    (+ (loop for hook in (hooks buffer)
	     until (eq hook hook-of-line)
	     sum (climacs-buffer-line:object-count (line hook)))
       (* (position hook-of-line (hooks buffer))
	  (newline-size buffer)))))

(defmethod hook-location ((buffer simple-buffer) (hook hook))
  (position hook (hooks buffer)))

(defmethod insert-line ((buffer simple-buffer) line location)
  (with-accessors ((hooks hooks)) buffer
    (setf hooks
	  (concatenate 'vector
		       (subseq hooks 0 location)
		       (list (make-instance 'hook :line line))
		       (subseq hooks location)))))

(defmethod delete-line ((buffer simple-buffer) location)
  (with-accessors ((hooks hooks)) buffer
    (setf hooks
	  (concatenate 'vector
		       (subseq hooks 0 location)
		       (subseq hooks (1+ location))))))

