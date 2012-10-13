(require :split-sequence)

(defparameter *z1*
  (let ((text-color (clim3-color:make-color 0.3 0.1 0.0))
	(text "this is a nonsense text to use as a test"))
    (clim3-layout:vbox*
     (clim3-layout:vbrick* 20)
     (clim3-layout:hbox*
      (clim3-layout:hbox
       (loop for word in (split-sequence:split-sequence #\Space text)
	     collect (clim3-text:text word nil text-color)
	     collect (clim3-layout:hbrick* 5)))
      (clim3-layout:sponge*))
     (clim3-layout:vbrick* 20))))

(defun test1 ()
  (let ((port (clim3-port:make-port :clx-framebuffer)))
    (clim3-port:connect *z1* port)
    (clim3-clx-framebuffer::event-loop port)))
