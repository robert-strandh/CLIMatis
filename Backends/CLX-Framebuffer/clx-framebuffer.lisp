(in-package #:clim3-clx-framebuffer)

(defparameter *port* nil)

(defclass clx-framebuffer-port (clim3-port:port)
  ((%display :accessor display)
   (%screen :accessor screen)
   (%root :accessor root)
   (%white :accessor white)
   (%black :accessor black)
   (%shift-state :initform nil :accessor shift-state)
   (%zone-entries :initform '() :accessor zone-entries)
   ;; A vector of interpretations.  Each interpretation is a
   ;; short vector of keysyms
   (%keyboard-mapping :initform nil :accessor keyboard-mapping)
   ;; Implement text-style to font mappings instead
   (%font :accessor font)))

(defmethod clim3-port:make-port ((display-server (eql :clx-framebuffer)))
  (let ((port (make-instance 'clx-framebuffer-port)))
    (setf (display port) (xlib:open-default-display))
    (setf (xlib:display-after-function (display port))
	  #'xlib:display-finish-output)
    (setf (screen port) (car (xlib:display-roots (display port))))
    (setf (white port) (xlib:screen-white-pixel (screen port)))
    (setf (black port) (xlib:screen-black-pixel (screen port)))
    (setf (root port) (xlib:screen-root (screen port)))
    (setf (keyboard-mapping port)
	  (let* ((temp (xlib:keyboard-mapping (display port)))
		 (result (make-array (array-dimension temp 0))))
	    (loop for row from 0 below (array-dimension temp 0)
		  do (let ((interpretations (make-array (array-dimension temp 1))))
		       (loop for col from 0 below (array-dimension temp 1)
			     do (setf (aref interpretations col)
				      (aref temp row col)))
		       (setf (aref result row) interpretations)))
	    result))
    ;; Implement text-style to font mappings instead
    (setf (font port) (camfer:make-font 10 100))
    port))

(defclass zone-entry ()
  ((%zone :accessor zone)
   (%gcontext :accessor gcontext)
   (%window :accessor window)
   (%pixel-array :accessor pixel-array)
   (%image :accessor image)
   (%pointer-zones :initform '() :accessor pointer-zones)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Handle connect.
;;;

(defmethod clim3-zone:notify-connect
    ((port clx-framebuffer-port) child parent)
  (declare (ignore child parent))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Handle disconnect.
;;;
;;; We maintain an invariant whereby the connected zones with a client
;;; slot of nil represent a suffix of the zone hierarcy.  We connect
;;; lazily, so that a zone that requires a client, but that has a
;;; client slot of nil, recursively asks for the client of its parent.  

;;; To maintain the invariant cited above when a zone is disconnected
;;; and then connected again, perhaps after some modifications, we
;;; prefer to set the client of every zone in the hierarchy to nil
;;; when the root of the hierarchy is disconnected.  
(defmethod clim3-zone:notify-disconnect
    ((port clx-framebuffer-port) child parent)
  (declare (ignore parent))
  (setf (clim3-zone:parent child) nil)
  (clim3-zone:map-over-children
   (lambda (grandchild) (clim3-zone:notify-disconnect port grandchild child))
   child))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Handle pointer position

(defun handle-pointer-positions (zone-entry)
  (let ((new-zone-entries '()))
    (multiple-value-bind (hpos vpos same-screen-p)
	(xlib:pointer-position (window zone-entry))
      ;; FIXME: Figure out what to do with same-screen-p
      (declare (ignore same-screen-p))
      (labels ((traverse (zone hpos vpos)
		 (unless (or (< hpos 0)
			     (< vpos 0)
			     (> hpos (clim3-zone:width zone))
			     (> vpos (clim3-zone:height zone)))
		   (when (or (typep zone 'clim3-input:enter)
			     (typep zone 'clim3-input:leave)
			     (typep zone 'clim3-input:motion))
		     (push (list zone hpos vpos)
			   new-zone-entries))
		   (clim3-zone:map-over-children-top-to-bottom
		    (lambda (child)
		      (traverse child
				(- hpos (clim3-zone:hpos child))
				(- vpos (clim3-zone:vpos child))))
		    zone))))
	(traverse (zone zone-entry) hpos vpos)))
    (setf new-zone-entries (nreverse new-zone-entries))
    ;; Handle leave zones.
    (loop for zone in (pointer-zones zone-entry)
	  do (when (and (typep zone 'clim3-input:leave)
			(not (member zone new-zone-entries
				     :test #'eq :key #'car)))
	       (funcall (clim3-input:handler zone) zone)))
    ;; Handle enter zones.
    (loop for entry in new-zone-entries
	  for zone = (car entry)
	  do (when (and (typep zone 'clim3-input:enter)
			(not (member zone (pointer-zones zone-entry) :test #'eq)))
	       (funcall (clim3-input:handler zone) zone)))
    ;; Handle motion zones.
    (loop for entry in new-zone-entries
	  for zone = (car entry)
	  do (when (and (typep zone 'clim3-input:motion)
			(member zone (pointer-zones zone-entry) :test #'eq))
	       (funcall (clim3-input:handler zone) zone (cadr entry) (caddr entry))))
    ;; Save new zones
    (setf (pointer-zones zone-entry)
	  (mapcar #'car new-zone-entries))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Update zones.

(defgeneric paint (zone port hstart vstart hend vend))

(defun update (zone-entry)
  (with-accessors ((zone zone)
		   (gcontext gcontext)
		   (window window)
		   (pixel-array pixel-array)
		   (image image))
      zone-entry
    (let ((hpos (xlib:drawable-x window))
	  (vpos (xlib:drawable-y window))
	  (width (xlib:drawable-width window))
	  (height (xlib:drawable-height window)))
      ;; Someone might have resized the window.  Make the zone
      ;; adjust to the current size if so. 
      (unless (and (= hpos (clim3-zone:hpos zone))
		   (= vpos (clim3-zone:vpos zone))
		   (= width (clim3-zone:width zone))
		   (= height (clim3-zone:height zone)))
	(clim3-zone:impose-layout zone hpos vpos width height))
      ;; Make sure the pixmap and the image object have the same
      ;; dimensions as the window.
      (if (and (= width (array-dimension pixel-array 1))
	       (= height (array-dimension pixel-array 0)))
	  (loop for r from 0 below (array-dimension pixel-array 0)
		do (loop for c from 0 below (array-dimension pixel-array 1)
			 do (setf (aref pixel-array r c) #xaaaaaaaa)))
	  (progn
	    ;; FIXME: give the right pixel values
	    (setf pixel-array
		  (make-array (list height width)
			      :element-type '(unsigned-byte 32)
			      :initial-element #xaaaaaaaa))	  
	    (setf (image zone-entry)
		  (xlib:create-image :bits-per-pixel 32
				     :data (pixel-array zone-entry)
				     :depth 24
				     :width width :height height
				     :format :z-pixmap))))
      ;; Paint.
      (let ((*hpos* 0)
	    (*vpos* 0)
	    (*pixel-array* pixel-array))
	(paint zone *port* 0 0 width height))
      ;; Transfer the image. 
      (xlib::put-image window
		       gcontext
		       image
		       :x 0 :y 0
		       :width width :height height))))

(defmethod clim3-port:connect ((zone clim3-zone:zone)
			       (port clx-framebuffer-port))
  ;; Make sure every zone has this port as a client.
  (labels ((set-client (zone)
	     (setf (clim3-zone:client zone) port)
	     (clim3-zone:map-over-children #'set-client zone)))
    (set-client zone))
  ;; Compute the gives of every zone.
  (clim3-zone:compute-gives zone)
  (let ((zone-entry (make-instance 'zone-entry)))
    ;; Ask for a window that has the natural size of the zone.  We may
    ;; not get it, though.
    ;;
    ;; FIXME: take into account the position of the zone
    (setf (window zone-entry)
	  (multiple-value-bind (width height) (clim3-zone:natural-size zone)
	    (xlib:create-window :parent (root port)
				:x 0 :y 0 :width width :height height
				:class :input-output
				:visual :copy
				:background (white port)
				:event-mask
				'(:key-press :key-release
				  :button-motion :button-press :button-release
				  :enter-window :leave-window
				  :exposure
				  :pointer-motion))))
    (xlib:map-window (window zone-entry))
    ;; Create an adequate gcontext for this window.
    (setf (gcontext zone-entry)
	  (xlib:create-gcontext :drawable (window zone-entry)
				:background (white port)
				:foreground (black port)))
    ;; Make the array of the pixels.  The dimensions don't matter,
    ;; because we reallocate it if necessary every time we need to
    ;; draw, so as to reflect the current size of the window.
    (setf (pixel-array zone-entry)
	  (make-array '(0 0)
		      :element-type '(unsigned-byte 32)))
	  
    ;; Make the image object.  Again, the size is of no importance, 
    ;; but we must remember to change it later. 
    (setf (image zone-entry)
	  (xlib:create-image :bits-per-pixel 32
			     :data (pixel-array zone-entry)
			     :depth 24
			     :width 0 :height 0
			     :format :z-pixmap))
    (setf (zone zone-entry) zone)
    (setf (clim3-zone:parent zone) port)
    ;; Register this zone entry.
    (push zone-entry (zone-entries port))
    ;; Make sure the window has the right contents.
    (let ((*port* port))
      (update zone-entry))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Painting.

(defparameter *hpos* nil)
(defparameter *vpos* nil)
(defparameter *pixel-array* nil)

(defmethod paint ((zone clim3-zone:atomic-zone)
		  (port clx-framebuffer-port)
		  hstart vstart hend vend)
  (declare (ignore port hstart vstart hend vend))
  nil)

(defmethod paint ((zone clim3-zone:compound-zone)
		  (port clx-framebuffer-port)
		  hstart vstart hend vend)
  (clim3-zone:map-over-children-bottom-to-top
   (lambda (child)
     (let ((chstart (max 0 (- hstart (clim3-zone:hpos child))))
	   (cvstart (max 0 (- vstart (clim3-zone:vpos child))))
	   (chend (min (clim3-zone:width child)
		       (- hend (clim3-zone:hpos child))))
	   (cvend (min (clim3-zone:height child)
		       (- vend (clim3-zone:vpos child)))))
       (when (and (> chend chstart)
		  (> cvend cvstart))
	 (let ((*hpos* (+ *hpos* (clim3-zone:hpos child)))
	       (*vpos* (+ *vpos* (clim3-zone:vpos child))))
	   (paint child port chstart cvstart chend cvend)))))
   zone))

(defun paint-pixel (hpos vpos r g b alpha)
  (let* ((pa *pixel-array*)
	 (hp (+ hpos *hpos*))
	 (vp (+ vpos *vpos*))
	 (pixel (aref pa vp hp))
	 (old-r (/ (logand 255 (ash pixel -16)) 255d0))
	 (old-g (/ (logand 255 (ash pixel -8)) 255d0))
	 (old-b (/ (logand 255 pixel) 255d0))
	 (new-r (+ (* r alpha) (* old-r (- 1d0 alpha))))
	 (new-g (+ (* g alpha) (* old-g (- 1d0 alpha))))
	 (new-b (+ (* b alpha) (* old-b (- 1d0 alpha))))
	 (new-pixel (+ (ash (round (* 255 new-r)) 16)
		       (ash (round (* 255 new-g)) 8)
		       (ash (round (* 255 new-b)) 0))))
    (setf (aref pa vp hp) new-pixel)))

;;; Speed up painting of opaque zones by not calling
;;; paint-pixel in an inner loop.
(defun fill-area (color hstart vstart hend vend)
  (let ((pa *pixel-array*)
	(hs (+ hstart *hpos*))
	(vs (+ vstart *vpos*))
	(he (+ hend *hpos*))
	(ve (+ vend *vpos*))
	(pixel (+ (ash (round (* 255 (clim3-color:red color))) 16)
		  (ash (round (* 255 (clim3-color:green color))) 8)
		  (ash (round (* 255 (clim3-color:blue color))) 0))))
    (loop for vpos from vs below ve
	  do (loop for hpos from hs below he
		   do (setf (aref pa vpos hpos) pixel)))))

(defmethod paint ((zone clim3-graphics:opaque)
		  (port clx-framebuffer-port)
		  hstart vstart hend vend)
  (fill-area (clim3-graphics:color zone) hstart vstart hend vend))

;;; Pait an array of opacities.  The bounding rectangle defined by
;;; hstart, hend, vstart, and vend is not outside the array
;;; coordinates.  The special variables *vpos* and *hpos* must have
;;; values that correspond to the absolute upper-left corner of the
;;; position of the mask. 
(defun paint-mask (mask color hstart vstart hend vend)
  (loop for hpos from hstart below hend
	do (loop for vpos from vstart below vend
		 do (paint-pixel hpos vpos
				 (clim3-color:red color)
				 (clim3-color:green color)
				 (clim3-color:blue color)
				 (aref mask vpos hpos)))))

(defmethod paint ((zone clim3-graphics:masked)
		  (port clx-framebuffer-port)
		  hstart vstart hend vend)
  (let* ((opacities (clim3-graphics:opacities zone))
	 (width (min hend (array-dimension opacities 1)))
	 (height (min vend (array-dimension opacities 1))))
    (paint-mask opacities (clim3-graphics:color zone)
		hstart vstart width height)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Text styles.

(defmethod clim3-port:text-style-ascent ((port clx-framebuffer-port)
					 text-style)
  (camfer:ascent (font port)))

(defmethod clim3-port:text-style-descent ((port clx-framebuffer-port)
					  text-style)
  (camfer:descent (font port)))

(defmethod clim3-port:text-ascent ((port clx-framebuffer-port)
				   text-style
				   string)
  (let ((font (font port)))
    (reduce #'min string
	    :key (lambda (char)
		   (camfer:y-offset (camfer:find-glyph font char))))))

(defmethod clim3-port:text-descent ((port clx-framebuffer-port)
				    text-style
				    string)
  (let ((font (font port)))
    (reduce #'max string
	    :key (lambda (char)
		   (+ (array-dimension (camfer:mask (camfer:find-glyph font char)) 0)
		      (camfer:y-offset (camfer:find-glyph font char)))))))

(defun glyph-space (font char1 char2)
  (- (* 2 (camfer::stroke-width font))
     (camfer:kerning font
		     (camfer:find-glyph font char1)
		     (camfer:find-glyph font char2))))

(defun glyph-width (font char)
  (array-dimension (camfer:mask (camfer:find-glyph font char)) 1))

(defmethod clim3-port:text-width ((port clx-framebuffer-port)
				  text-style
				  string)
  (let ((font (font port)))
    (if (zerop (length string))
	0
	(+ (glyph-width font (char string 0))
	   (loop for i from 1 below (length string)
		 sum (+ (glyph-width font (char string i))
			(glyph-space font
				     (char string (1- i))
				     (char string i))))))))

(defmethod paint ((zone clim3-text:text)
		  (port clx-framebuffer-port)
		  hstart vstart hend vend)
  (let ((color (clim3-graphics:color zone))
	(font (font port))
	(string (clim3-text:chars zone))
	(ascent (clim3-port:text-style-ascent port (clim3-text:style zone))))
    (unless (zerop (length string))
      (flet ((paint-glyph (mask pos-x pos-y)
	       ;; pos-x and pos-y are the coordinates of the upper-left
	       ;; corner of the mask relative to the zone
	       (let ((width (array-dimension mask 1))
		     (height (array-dimension mask 0)))
		 (let ((*hpos* (+ *hpos* pos-x))
		       (*vpos* (+ *vpos* pos-y)))
		   (paint-mask mask color
			       (max 0 (- hstart pos-x))
			       (max 0 (- vstart pos-y))
			       (min width (- hend pos-x))
			       (min height (- vend pos-y)))))))
	;; paint the first character
	(paint-glyph (camfer:mask (camfer:find-glyph font (char string 0)))
		     0
		     (+ ascent (camfer:y-offset (camfer:find-glyph font (char string 0)))))
	(loop with pos-x = 0
	      for i from 1 below (length string)
	      for glyph = (camfer:find-glyph font (char string i))
	      do (progn
		   ;; compute the new x position
		   (incf pos-x
			 (+ (glyph-width font (char string (1- i)))
			    (glyph-space font (char string (1- i)) (char string i))))
		   (paint-glyph (camfer:mask glyph)
				pos-x
				(+ ascent (camfer:y-offset glyph)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Keycodes, keysyms, etc.

(defparameter *modifier-names*
  #(:shift :lock :control :meta :hyper :super :modifier-4 :modifier-5))

(defparameter +shift-mask+      #b00000001)
(defparameter +lock-mask+       #b00000010)
(defparameter +control-mask+    #b00000100)
(defparameter +meta-mask+       #b00001000)
(defparameter +hyper-mask+      #b00010000)
(defparameter +super-mask+      #b00100000)
(defparameter +modifier-4-mask+ #b01000000)
(defparameter +modifier-5-mask+ #b10000000)

(defun modifier-names (mask)
  (loop for i from 0 below 8
	when (plusp (logand (ash 1 i) mask))
	  collect (aref *modifier-names* i)))

(defun keycode-is-modifier-p (port keycode)
  (some (lambda (modifiers)
	  (member keycode modifiers))
	(multiple-value-list (xlib:modifier-mapping (display port)))))

(defmethod clim3-port:port-standard-key-processor
    ((port clx-framebuffer-port) handler-fun keycode modifiers)
  (if (keycode-is-modifier-p port keycode)
      nil
      (let ((keysyms (vector (xlib:keycode->keysym (display port) keycode 0)
			     (xlib:keycode->keysym (display port) keycode 1)
			     (xlib:keycode->keysym (display port) keycode 2)
			     (xlib:keycode->keysym (display port) keycode 3))))
	(funcall handler-fun
		 (cond ((= (aref keysyms 0) (aref keysyms 1))
			(cons (code-char (aref keysyms 0))
			      (modifier-names modifiers)))
		       ((plusp (logand +shift-mask+ modifiers))
			(cons (code-char (aref keysyms 1))
			      (modifier-names
			       (logandc2 modifiers +shift-mask+))))
		       (t
			(cons (code-char (aref keysyms 0))
			      (modifier-names modifiers))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Handling events.

(defun handle-other (zone-entry)
  (handle-pointer-positions zone-entry)
  (update zone-entry))

;;; Some code factoring is needed here. 
(defun handle-key-press (zone-entry code state hpos vpos)
  (labels ((traverse (zone hpos vpos)
	     (unless (or (< hpos 0)
			 (< vpos 0)
			 (> hpos (clim3-zone:width zone))
			 (> vpos (clim3-zone:height zone)))
	       (when (typep zone 'clim3-input:key-press)
		 (funcall (clim3-input:handler zone) zone code state))
	       (clim3-zone:map-over-children-top-to-bottom
		(lambda (child)
		  (traverse child
			    (- hpos (clim3-zone:hpos child))
			    (- vpos (clim3-zone:vpos child))))
		zone))))
	(traverse (zone zone-entry) hpos vpos))
  (update zone-entry))
  
(defun handle-key-release (zone-entry code state hpos vpos)
  (labels ((traverse (zone hpos vpos)
	     (unless (or (< hpos 0)
			 (< vpos 0)
			 (> hpos (clim3-zone:width zone))
			 (> vpos (clim3-zone:height zone)))
	       (when (typep zone 'clim3-input:key-release)
		 (funcall (clim3-input:handler zone) zone code state))
	       (clim3-zone:map-over-children-top-to-bottom
		(lambda (child)
		  (traverse child
			    (- hpos (clim3-zone:hpos child))
			    (- vpos (clim3-zone:vpos child))))
		zone))))
	(traverse (zone zone-entry) hpos vpos))
  (update zone-entry))
  
(defun handle-button-press (zone-entry code state hpos vpos)
  (labels ((traverse (zone hpos vpos)
	     (unless (or (< hpos 0)
			 (< vpos 0)
			 (> hpos (clim3-zone:width zone))
			 (> vpos (clim3-zone:height zone)))
	       (when (typep zone 'clim3-input:button-press)
		 (funcall (clim3-input:handler zone) zone code state))
	       (clim3-zone:map-over-children-top-to-bottom
		(lambda (child)
		  (traverse child
			    (- hpos (clim3-zone:hpos child))
			    (- vpos (clim3-zone:vpos child))))
		zone))))
	(traverse (zone zone-entry) hpos vpos))
  (update zone-entry))
  
(defun handle-button-release (zone-entry code state hpos vpos)
  (labels ((traverse (zone hpos vpos)
	     (unless (or (< hpos 0)
			 (< vpos 0)
			 (> hpos (clim3-zone:width zone))
			 (> vpos (clim3-zone:height zone)))
	       (when (typep zone 'clim3-input:button-release)
		 (funcall (clim3-input:handler zone) zone code state))
	       (clim3-zone:map-over-children-top-to-bottom
		(lambda (child)
		  (traverse child
			    (- hpos (clim3-zone:hpos child))
			    (- vpos (clim3-zone:vpos child))))
		zone))))
	(traverse (zone zone-entry) hpos vpos))
  (update zone-entry))

(defun event-loop (port)
  (let ((*port* port))
    (xlib:event-case ((display port))
      (:key-press
       (window code state x y)
       (let ((entry (find window (zone-entries port) :test #'eq :key #'window)))
	 (unless (null entry)
	   (handle-key-press entry code state x y)))
       nil)
      (:key-release
       (window code state x y)
       (let ((entry (find window (zone-entries port) :test #'eq :key #'window)))
	 (unless (null entry)
	   (handle-key-release entry code state x y)))
       nil)
      (:button-press
       (window code state x y)
       (let ((entry (find window (zone-entries port) :test #'eq :key #'window)))
	 (unless (null entry)
	   (handle-button-press entry code state x y)))
       nil)
      (:button-release
       (window code state x y)
       (let ((entry (find window (zone-entries port) :test #'eq :key #'window)))
	 (unless (null entry)
	   (handle-button-release entry code state x y)))
       nil)
      ((:exposure :motion-notify :enter-notify :leave-notify)
       (window)
       (let ((entry (find window (zone-entries port) :test #'eq :key #'window)))
	 (unless (null entry)
	   (handle-other entry))))
      (t
       ()
       (loop for zone-entry in (zone-entries port)
	     do (handle-other zone-entry))
       nil))))
