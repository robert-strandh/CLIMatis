(require :split-sequence)

(defparameter *za1*
  (clim3:hframe
   200 200 200
   (clim3:vframe
    200 200 200
    (clim3:opaque (clim3:make-color 0.2 0.2 1.0)))))
		       
(defparameter *za2*
  (clim3:hbox*
   (clim3:hframe
    200 200 200
    (clim3:vframe
     200 200 200
     (clim3:opaque (clim3:make-color 0.2 0.2 1.0))))
   (clim3:hframe
    100 100 100
    (clim3:vframe
    200 200 200
    (clim3:opaque (clim3:make-color 1.0 0.2 0.1))))))

(defparameter *za3*
  (let ((a (make-array '(100 100))))
    (let ((h (array-dimension a 0))
	  (w (array-dimension a 1)))
      (loop for r from 0 below h
	    do (loop for c from 0 below w
		     do (setf (aref a r c)
			      (float (* (/ r (1- h)) (/ c (1- w))))))))
    (clim3:masked (clim3:make-color 1.0 0.5 0.5) a)))
	    
(defparameter *za5*
  (let* ((text-color (clim3:make-color 0.1 0.4 0.1))
	 (background-color (clim3:make-color 0.9 0.7 0.7)))
    (clim3:vframe
     200 200 200
     (clim3:hframe
      200 200 200
      (clim3:bboard*
       (clim3:pile* 
	(clim3:vbox
	 (loop for i from 0 below 100
	       collect (clim3:hbox*
			(clim3-text:text
			 (format nil "hello ~a" i)
			 (clim3:text-style :camfer :sans :roman 12)
			 text-color)
			(clim3:hframe 0 0 nil))))
	(clim3:opaque background-color)))))))

(defparameter *zz2*
  (let ((text-color (clim3:make-color 0.3 0.1 0.0))
	(text "this is a nonsense text to use as a test"))
    (clim3:vbox*
     (clim3:vframe 20 20 20)
     (clim3:hbox*
      (clim3:hbox
       (loop for word in (split-sequence:split-sequence #\Space text)
	     collect (clim3-text:text
		      word
		      (clim3:text-style :camfer :sans :roman 12)
		      text-color)
	     collect (clim3:hframe 7 7 7)))
      (clim3:hframe 0 0 nil))
     (clim3:vframe 20 20 20))))

(defun test-zone (zone)
  (let ((port (clim3:make-port :clx-framebuffer)))
    (clim3:connect zone port)
    (clim3:event-loop port)))
