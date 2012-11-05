(require :split-sequence)

(defparameter *za1*
  (clim3-layout:hframe
   200 200 200
   (clim3-layout:vframe
    200 200 200
    (clim3-graphics:opaque (clim3-color:make-color 0.2 0.2 1.0)))))
		       
(defparameter *za2*
  (clim3-layout:hbox*
   (clim3-layout:hframe
    200 200 200
    (clim3-layout:vframe
     200 200 200
     (clim3-graphics:opaque (clim3-color:make-color 0.2 0.2 1.0))))
   (clim3-layout:hframe
    100 100 100
    (clim3-layout:vframe
    200 200 200
    (clim3-graphics:opaque (clim3-color:make-color 1.0 0.2 0.1))))))

(defparameter *za3*
  (let ((a (make-array '(100 100))))
    (let ((h (array-dimension a 0))
	  (w (array-dimension a 1)))
      (loop for r from 0 below h
	    do (loop for c from 0 below w
		     do (setf (aref a r c)
			      (float (* (/ r (1- h)) (/ c (1- w))))))))
    (clim3-graphics:masked (clim3-color:make-color 1.0 0.5 0.5) a)))
	    
(defparameter *za4*
  (let* ((color (clim3-color:make-color 1.0 0.0 0.0))
	 (str "hello")
	 (text (clim3-text:text str nil color))
	 (hbox (clim3-layout:hbox*
		text
		(clim3-layout:hframe
		 20 20 20
		 (clim3-layout:vframe
		  10 10 10
		  (clim3-graphics:opaque (clim3-color:make-color 0.0 0.0 1.0))))
		(clim3-layout:hframe 0 0 nil))))
    (clim3-layout:pile*
     (clim3-input:button
      (lambda (zone button modifiers)
	(declare (ignore zone button modifiers))
	(setf str (concatenate 'string str "a"))
	(let ((new-text (clim3-text:text str nil color)))
	  (setf (clim3-zone:children hbox)
		(substitute new-text text (clim3-zone:children hbox)))
	  (setf text new-text)))
      (lambda (zone button modifiers)
	(declare (ignore zone button modifiers))
	nil))
     (clim3-layout:hframe
      500 500 500
      hbox))))

(defparameter *za5*
  (let* ((text-color (clim3-color:make-color 0.1 0.4 0.1))
	 (background-color (clim3-color:make-color 0.9 0.7 0.7)))
    (clim3-layout:vframe
     200 200 200
     (clim3-layout:hframe
      200 200 200
      (clim3-layout:bboard*
       (clim3-layout:pile* 
	(clim3-layout:vbox
	 (loop for i from 0 below 100
	       collect (clim3-layout:hbox*
			(clim3-text:text (format nil "hello ~a" i) nil text-color)
			(clim3-layout:hframe 0 0 nil))))
	(clim3-graphics:opaque background-color)))))))

(defparameter *zz2*
  (let ((text-color (clim3-color:make-color 0.3 0.1 0.0))
	(text "this is a nonsense text to use as a test"))
    (clim3-layout:vbox*
     (clim3-layout:vframe 20 20 20)
     (clim3-layout:hbox*
      (clim3-layout:hbox
       (loop for word in (split-sequence:split-sequence #\Space text)
	     collect (clim3-text:text word nil text-color)
	     collect (clim3-layout:hframe 7 7 7)))
      (clim3-layout:hframe 0 0 nil))
     (clim3-layout:vframe 20 20 20))))

(defun test-zone (zone)
  (let ((port (clim3-port:make-port :clx-framebuffer)))
    (clim3-port:connect zone port)
    (clim3-clx-framebuffer::event-loop port)))
