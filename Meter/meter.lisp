(cl:in-package #:clim3-meter)

(defclass meter ()
  ((%sample-count :initform 0 :accessor sample-count)
   (%min-run-time :initform 0 :accessor min-run-time)
   (%max-run-time :initform 0 :accessor max-run-time)
   (%sum-run-time :initform 0 :accessor sum-run-time)
   (%square-sum-run-time :initform 0 :accessor square-sum-run-time)
   (%min-real-time :initform 0 :accessor min-real-time)
   (%max-real-time :initform 0 :accessor max-real-time)
   (%sum-real-time :initform 0 :accessor sum-real-time)
   (%square-sum-real-time :initform 0 :accessor square-sum-real-time)))

(defgeneric reset (meter))

(defmethod reset ((meter meter))
  (setf (sample-count meter) 0)
  (setf (sum-run-time meter) 0)
  (setf (square-sum-run-time meter) 0)
  (setf (sum-real-time meter) 0)
  (setf (square-sum-real-time meter) 0))

(defmacro with-meter (meter &body body)
  (let ((meter-var (gensym))
	(total-run-time-var (gensym))
	(total-real-time-var (gensym))
	(run-time-var (gensym))
	(real-time-var (gensym)))
    `(let ((,meter-var ,meter)
	   (,total-run-time-var (get-internal-run-time))
	   (,total-real-time-var (get-internal-real-time)))
       ,@body
       (let ((,run-time-var (- (get-internal-run-time) ,total-run-time-var))
	     (,real-time-var (- (get-internal-real-time) ,total-real-time-var)))
	 (if (zerop (sample-count ,meter-var))
	     (setf (max-run-time ,meter-var) ,run-time-var
		   (min-run-time ,meter-var) ,run-time-var
		   (max-real-time ,meter-var) ,real-time-var
		   (min-real-time ,meter-var) ,real-time-var)
	     (setf (max-run-time ,meter-var) (max (max-run-time ,meter-var) ,run-time-var)
		   (min-run-time ,meter-var) (min (min-run-time ,meter-var) ,run-time-var)
		   (max-real-time ,meter-var) (max (max-real-time ,meter-var) ,real-time-var)
		   (min-real-time ,meter-var) (min (min-real-time ,meter-var) ,real-time-var)))
	 (incf (sum-run-time ,meter-var) ,run-time-var)
	 (incf (square-sum-run-time ,meter-var) (* ,run-time-var ,run-time-var))
	 (incf (sum-real-time ,meter-var) ,real-time-var)
	 (incf (square-sum-real-time ,meter-var) (* ,real-time-var ,real-time-var))
	 (incf (sample-count ,meter-var))))))
  
(defgeneric report (meter &optional stream))

(defmethod report ((meter meter) &optional (stream *standard-output*))
  (format stream "Sample count: ~a~%" (sample-count meter))
  (format stream "Run time information:~%")
  (format stream "   Min: ~a~%" (min-run-time meter))
  (format stream "   Max: ~a~%" (max-run-time meter))
  (format stream "   Average: ~a~%"
	  (/ (sum-run-time meter) (sample-count meter)))
  (format stream "   Standard deviation: ~a~%"
	  (sqrt (- (/ (square-sum-run-time meter) (sample-count meter))
		   (expt  (/ (sum-run-time meter) (sample-count meter)) 2))))
  (format stream "Real time information:~%")
  (format stream "   Min: ~a~%" (min-real-time meter))
  (format stream "   Max: ~a~%" (max-real-time meter))
  (format stream "   Average: ~a~%"
	  (/ (sum-real-time meter) (sample-count meter)))
  (format stream "   Standard deviation: ~a~%"
	  (sqrt (- (/ (square-sum-real-time meter) (sample-count meter))
		   (expt  (/ (sum-real-time meter) (sample-count meter)) 2)))))
