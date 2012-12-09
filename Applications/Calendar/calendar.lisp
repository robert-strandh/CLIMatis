(cl:in-package #:clim3-calendar)

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

(defun dayname-zone (name)
  (clim3-layout:vbrick
   40
   (clim3-layout:vbox*
    (clim3-layout:sponge)
    (clim3-layout:hbox*
     (clim3-layout:hbrick 5)
     (clim3-text:text
      name
      (clim3-text-style:text-style :free :sans :bold 16)
      (clim3-color:make-color 0.0d0 0.0d0 0.0d0 ))
     (clim3-layout:sponge))
    (clim3-layout:vbrick 2))))

(defun day-names ()
  (clim3-layout:hbox 
   (loop for name in '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")
	 collect (dayname-zone name))))

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

(defun calendar-zones ()
  (clim3-layout:pile*
   (clim3-layout:brick
    1000 700
    (clim3-layout:vbox*
     (day-names)
     (grid-zones)))
   (clim3-graphics:opaque (clim3-color:make-color 0.95d0 0.95d0 0.95d0))))

(defun calendar ()
  (let ((port (clim3-port:make-port :clx-framebuffer))
	(root (calendar-zones)))
    (clim3-port:connect root port)
    (let ((clim3-port:*new-port* port))
      (loop for keystroke = (clim3-port:read-keystroke)
	    until (eql (car keystroke) #\q)))))