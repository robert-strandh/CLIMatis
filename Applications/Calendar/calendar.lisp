(cl:in-package #:clim3-calendar)

(defparameter *dayname-text-style*
  (clim3-text-style:text-style :free :sans :roman 10))

(defparameter *day-number-text-style*
  (clim3-text-style:text-style :free :sans :bold 20))

(defparameter *hour-text-style*
  (clim3-text-style:text-style :free :fixed :roman 10))

(defparameter *follow-hour-space* 5)

(defparameter *black* (clim3-color:make-color 0.0d0 0.0d0 0.0d0))

(defun hour-zone ()
  (clim3-layout:sponge))

(defun vline ()
  (clim3-layout:hbrick
   1
   (clim3-graphics:opaque (clim3-color:make-color 0.3d0 0d0 0d0))))

(defun hline ()
  (clim3-layout:vbrick
   1
   (clim3-graphics:opaque (clim3-color:make-color 0.3d0 0d0 0d0))))

(defun dayname-zone (name number)
  (clim3-layout:vbrick
   40
   (clim3-layout:vbox*
    (clim3-layout:sponge)
    (clim3-layout:hbox*
     (clim3-layout:hbrick 5)
     (clim3-layout:hbrick
      40
      (clim3-layout:vbox*
       (clim3-layout:sponge)
       (clim3-text:text (format nil "~2,'0d" number)
			*day-number-text-style*
			*black*)))
     (clim3-layout:hbrick
      40
      (clim3-layout:vbox*
       (clim3-layout:sponge)
       (clim3-text:text name
			*dayname-text-style*
			*black*)
       (clim3-layout:sponge)))
     (clim3-layout:sponge))
    (clim3-layout:vbrick 2))))

(defun day-names (days)
  (clim3-layout:hbox 
   (loop for name in '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")
	 for number in days
	 collect (dayname-zone name number))))

(defun day-zone ()
  (clim3-layout:hbox*
   (clim3-layout:vbox
    (cons (hline)
	  (loop repeat 24
		collect (hour-zone)
		collect (hline))))
   (vline)))

(defun grid-zones ()
  (clim3-layout:hbox
   (cons (vline)
	 (loop repeat 7
	       collect (day-zone)))))

(defun hours ()
  (let ((color (clim3-color:make-color 0.0d0 0.0d0 0.0d0 )))
    (clim3-layout:vbox
     (cons (clim3-text:text "00:00" *hour-text-style* color)
	   (loop for hour from 1 to 24
		 collect (clim3-layout:sponge)
		 collect (clim3-text:text (format nil "~2,'0d:00" (mod hour 24))
					  *hour-text-style* color))))))
(defun time-plane ()
  (clim3-layout:hbox*
   (hours)
   (clim3-layout:hbrick *follow-hour-space*)
   (clim3-layout:vbox*
    (clim3-layout:vbrick 10)
    (grid-zones)
    (clim3-layout:vbrick 10))))

(defun calendar-zones (days)
  (clim3-layout:pile*
   (clim3-layout:brick
    1000 700
    (clim3-layout:hbox*
     (clim3-layout:vbox*
      (clim3-layout:hbox*
       (clim3-layout:hbrick 60)
       (day-names days))
      (time-plane))
     (clim3-layout:hbrick 10)))
   (clim3-graphics:opaque (clim3-color:make-color 0.95d0 0.95d0 0.95d0))))

(defun calendar ()
  (let ((port (clim3-port:make-port :clx-framebuffer))
	(root (calendar-zones '(28 29 30 31 1 2 3))))
    (clim3-port:connect root port)
    (let ((clim3-port:*new-port* port))
      (loop for keystroke = (clim3-port:read-keystroke)
	    until (eql (car keystroke) #\q)))))