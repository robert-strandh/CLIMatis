(in-package #:climacs-buffer-simple-line)

(defclass simple-line (line)
  ((%objects :initform (vector) :accessor objects)
   (%cursors :initform '() :accessor cursors)))

(defclass simple-cursor (attached-cursor)
  ((%location :initform 0 :initarg :location :accessor location)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Line contents.

(defmethod object-count ((line simple-line))
  (length (objects line)))

(defmethod contents ((line simple-line) &key (start 0) (end nil))
  
  (let ((object-count (object-count line)))
    (unless (typep start `(integer 0 ,object-count))
      (error "wrong type"))
    (unless (typep end `(or null (integer 0 ,object-count)))
      (error "wrong type"))
    (subseq (objects line) start end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Method for MAP-OVER-CURSORS.

(defmethod map-over-cursors (function (line simple-line))
  (mapc function (cursors line)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Splitting and joining.

(defmethod split ((line simple-line) location)
  (let* ((new (make-instance 'simple-line
		 :objects (subseq (objects line) location)))
	 (cursors (cursors line))
	 (cursors1 (remove location cursors :key #'location :test #'<))
	 (cursors2 (remove location cursors :key #'location :test-not #'<)))
    (loop for cursor in cursors2
	  do (let ((new-location (- (location cursor) location)))
	       (setf (line cursor) new)
	       (setf (location cursor) new-location)))
    (setf (objects line) (subseq (objects line) 0 location))
    (setf (cursors line) cursors1)
    new))

(defmethod join ((line1 simple-line) line2)
  (let ((old-object-count (object-count line1)))
    (setf (objects line1)
	  (concatenate 'vector (objects line1) (contents line2)))
    (map-over-cursors
     (lambda (cursor)
       (let ((new-location (+ (location cursor) old-object-count)))
	 (setf (line cursor) line1)
	 (setf (location cursor) new-location)))
     line2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Cursor operations.

(defmethod object-at-cursor ((cursor simple-cursor))
  (aref (objects (line cursor)) (location cursor)))

(defmethod insert-object ((cursor simple-cursor) object)
  (let ((location (location cursor)))
    (with-accessors ((objects objects) (cursors cursors)) (line cursor)
      (setf objects
	    (concatenate 'vector
			 (subseq objects 0 location)
			 (list object)
			 (subseq objects location)))
      (loop for cursor in cursors
	    do (when (> (location cursor) location)
		 (incf (location cursor))))))
  (move-forward cursor))

(defmethod delete-object ((cursor simple-cursor))
  (let ((location (location cursor)))
    (with-accessors ((objects objects) (cursors cursors)) (line cursor)
      (setf objects
	    (concatenate 'vector
			 (subseq objects 0 location)
			 (subseq objects (1+ location))))
      (loop for cursor in cursors
	    do (when (> (location cursor) location)
		 (decf (location cursor)))))))
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Attaching a cursor to a simple line.

(defmethod (setf line) :before ((new-line simple-line) (cursor unattached-cursor))
  (change-class cursor 'simple-cursor)
  (setf (location cursor) (objects new-line)))
