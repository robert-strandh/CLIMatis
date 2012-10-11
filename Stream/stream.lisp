;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Stream zone

(defparameter *text-color* nil)

(defclass stream-zone (compound-sequence-zone)
  ((%current-line :initform nil :accessor current-line)))

(defmethod stream-write-char ((stream stream-zone) char)
  (case char
    ((#\Newline #\Return)
       (setf (current-line stream) nil))
    (t
       (when (null (current-line stream))
	 (let ((new-line (make-instance 'text-zone
					;; do this better when we have text styles.
					:color +brown+
					:text-style (make-text-style :sans :roman 14)
					;; do this much better
					:hpos 0 :vpos 0 )))
	   (setf (children stream)
		 (append (children stream) (list new-line)))
	   (setf (current-line stream) new-line)))
       (add-text (current-line stream) char))))

(defmethod stream-terpri ((stream stream-zone))
  (setf (current-line stream) nil))

(defmethod stream-fresh-line ((stream stream-zone))
  (unless (null (current-line stream))
    (setf (current-line stream) nil)))

;;; Add more Gray stream methods here. 

