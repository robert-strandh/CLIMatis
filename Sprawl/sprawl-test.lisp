(in-package #:clim3-sprawl-test)

(defun hugep (sprawl)
  (null (clim3-sprawl:max-size sprawl)))

(defun test-combine-in-parallel ()
  ;; Test normal case.
  (loop repeat 1000000
	do (let* ((length (1+ (random 20)))
		  (size (random 20))
		  (sprawls (loop repeat length
				 collect (clim3-sprawl:sprawl 
					  (random (1+ size))
					  size
					  (if (zerop (random 2))
					      nil
					      (+ size (random 10))))))
		  (combined (clim3-sprawl:combine-in-parallel sprawls)))
	     (assert (<= (clim3-sprawl:min-size combined) size))
	     (assert (<= (clim3-sprawl:min-size combined)
			 (clim3-sprawl:size combined)))
	     (if (every #'hugep sprawls)
		 (assert (hugep combined))
		 (progn 
		   (assert (<= size (clim3-sprawl:max-size combined)))
		   (assert (<= (clim3-sprawl:size combined)
			       (clim3-sprawl:max-size combined))))))))

(defun test()
  (test-combine-in-parallel))