(in-package #:clim3-sprawl-test)

(defun hugep (sprawl)
  (null (clim3-sprawl:max-size sprawl)))

(defun test-combine-in-parallel-normal-case ()
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

(defun test-combine-in-parallel-all-huge ()
  (loop repeat 1000000
	do (let* ((length (1+ (random 20)))
		  (sprawls (loop repeat length
				 collect (let ((min-size (random 20)))
					   (clim3-sprawl:sprawl 
					    min-size
					    (+ min-size (random 5))
					    nil))))
		  (combined (clim3-sprawl:combine-in-parallel sprawls)))
	     (assert (null (clim3-sprawl:max-size combined)))
	     (assert (= (clim3-sprawl:min-size combined)
			(reduce #'max sprawls :key #'clim3-sprawl:min-size))))))

(defun test-combine-in-parallel ()
  (test-combine-in-parallel-normal-case)
  (test-combine-in-parallel-all-huge))

(defun test-combine-in-series ()
  (loop repeat 1000000
	do (let* ((length (1+ (random 20)))
		  (sprawls (loop repeat length
				 collect (let* ((min-size (random 10))
						(size (+ min-size (random 5)))
						(max-size (if (zerop (random 2))
							      nil
							      (+ size (random 5)))))

					   (clim3-sprawl:sprawl
					    min-size size max-size))))
		  (combined (clim3-sprawl:combine-in-series sprawls)))
	     (assert (= (clim3-sprawl:min-size combined)
			(reduce #'+ sprawls :key #'clim3-sprawl:min-size)))
	     (assert (= (clim3-sprawl:size combined)
			(reduce #'+ sprawls :key #'clim3-sprawl:size)))
	     (if (some #'hugep sprawls)
		 (assert (hugep combined))
		 (assert (= (clim3-sprawl:max-size combined)
			(reduce #'+ sprawls :key #'clim3-sprawl:max-size)))))))

(defun test()
  (test-combine-in-parallel)
  (test-combine-in-series))