(require :split-sequence)

(defparameter *za1*
  (clim3-layout:brick*
   200 200
   (clim3-graphics:opaque (clim3-color:make-color 0.2 0.2 1.0))))
		       
(defparameter *za2*
  (clim3-layout:hbox*
   (clim3-layout:brick*
    200 200
    (clim3-graphics:opaque (clim3-color:make-color 0.2 0.2 1.0)))
   (clim3-layout:brick*
    100 200
    (clim3-graphics:opaque (clim3-color:make-color 1.0 0.2 0.1)))))

(defparameter *za3*
  (let ((a (make-array '(100 100))))
    (let ((h (array-dimension a 0))
	  (w (array-dimension a 1)))
      (loop for r from 0 below h
	    do (loop for c from 0 below w
		     do (setf (aref a r c)
			      (float (* (/ r (1- h)) (/ c (1- w))))))))
    (clim3-graphics:masked (clim3-color:make-color 1.0 0.5 0.5) a)))
	    


(defparameter *zz2*
  (let ((text-color (clim3-color:make-color 0.3 0.1 0.0))
	(text "this is a nonsense text to use as a test"))
    (clim3-layout:vbox*
     (clim3-layout:vbrick* 20)
     (clim3-layout:hbox*
      (clim3-layout:hbox
       (loop for word in (split-sequence:split-sequence #\Space text)
	     collect (clim3-text:text word nil text-color)
	     collect (clim3-layout:hbrick* 7)))
      (clim3-layout:sponge*))
     (clim3-layout:vbrick* 20))))

(defun test-zone (zone)
  (let ((port (clim3-port:make-port :clx-framebuffer)))
    (clim3-port:connect zone port)
    (clim3-clx-framebuffer::event-loop port)))
