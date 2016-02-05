(in-package #:clim3-clx-framebuffer)

(defparameter *hpos* nil)
(defparameter *vpos* nil)
(defparameter *hstart* nil)
(defparameter *vstart* nil)
(defparameter *hend* nil)
(defparameter *vend* nil)
(defparameter *pixel-array* nil)

(defclass clx-framebuffer-port (clim3:port)
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
   (%meter :initform (make-instance 'clim3-meter:meter) :reader meter)))

(defmethod clim3-ext:call-with-zone ((port clx-framebuffer-port) thunk zone)
  (let ((zone-hpos (clim3:hpos zone))
	(zone-vpos (clim3:vpos zone))
	(zone-width (clim3:width zone))
	(zone-height (clim3:height zone)))
    (let ((new-hpos (+ *hpos* zone-hpos))
	  (new-vpos (+ *vpos* zone-vpos)))
      (let ((new-hstart (max *hstart* new-hpos))
	    (new-vstart (max *vstart* new-vpos))
	    (new-hend (min *hend* (+ new-hpos zone-width)))
	    (new-vend (min *vend* (+ new-vpos zone-height))))
	(when (and (< new-hstart new-hend)
		   (< new-vstart new-vend))
	  (let ((*vpos* new-vpos)
		(*hpos* new-hpos)
		(*hstart* new-hstart)
		(*vstart* new-vstart)
		(*hend* new-hend)
		(*vend* new-vend))
	    (funcall thunk)))))))
	
(defmethod clim3-ext:call-with-area
    ((port clx-framebuffer-port) function hpos vpos width height)
  (let ((new-hpos (+ *hpos* hpos))
	(new-vpos (+ *vpos* vpos)))
    (let ((new-hstart (max *hstart* new-hpos))
	  (new-vstart (max *vstart* new-vpos))
	  (new-hend (min *hend* (+ new-hpos width)))
	  (new-vend (min *vend* (+ new-vpos height))))
      (when (and (< new-hstart new-hend)
		 (< new-vstart new-vend))
	(let ((*vpos* new-vpos)
	      (*hpos* new-hpos)
	      (*hstart* new-hstart)
	      (*vstart* new-vstart)
	      (*hend* new-hend)
	      (*vend* new-vend))
	  (funcall function))))))
	
;;; Do not change the clipping region, just the 
;;; origin of the coordinate system. 
(defmethod clim3-ext:call-with-position
    ((port clx-framebuffer-port) function hpos vpos)
  (let ((*hpos* (+ *hpos* hpos))
	(*vpos* (+ *vpos* vpos)))
    (funcall function)))

(defmethod clim3:make-port ((display-server (eql :clx-framebuffer)))
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
    port))

(defclass zone-entry ()
  ((%port :initarg :port :reader port)
   (%zone :initarg :zone :accessor zone)
   (%gcontext :accessor gcontext)
   (%window :accessor window)
   (%pixel-array :accessor pixel-array)
   (%image :accessor image)
   (%motion-zones :initform '() :accessor motion-zones)
   (%zone-containing-pointer :initform nil :accessor zone-containing-pointer)
   (%prev-pointer-hpos :initform 0 :accessor prev-pointer-hpos)
   (%prev-pointer-vpos :initform 0 :accessor prev-pointer-vpos)))

(defmethod clim3-ext:notify-child-hsprawl-changed
    (zone (port clx-framebuffer-port))
  nil)

(defmethod clim3-ext:notify-child-vsprawl-changed
    (zone (port clx-framebuffer-port))
  nil)

;;; Later, we might try to do something more fancy here, such as
;;; attempting to move the window on the screen.
(defmethod clim3-ext:notify-child-position-changed
    (zone (port clx-framebuffer-port))
  nil)

(defmethod clim3-ext:notify-child-depth-changed
    (zone (port clx-framebuffer-port))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Handle pointer position

(defmethod (setf clim3-ext:client) :after
  ((new-client clx-framebuffer-port) (zone clim3:motion))
  ;; Find the root zone of the zone.
  (let ((root zone))
    (loop while (clim3:zone-p (clim3-ext:parent root))
	  do (setf root (clim3-ext:parent root)))
    ;; Find the zone entry with that root zone.
    (let ((zone-entry (find root
			    (zone-entries new-client)
			    :key #'zone
			    :test #'eq)))
      (assert (not (null zone-entry)))
      ;; Add the motion zone to the list of motion zones. 
      (push zone (motion-zones zone-entry)))))

(defun handle-motion-zones (zone-entry)
  (multiple-value-bind (hpos vpos same-screen-p)
      (xlib:pointer-position (window zone-entry))
    ;; FIXME: Figure out what to do with same-screen-p
    (declare (ignore same-screen-p))
    ;; Only alert if the pointer is at a different position.
    ;; This optimization is valuable if many consecutive events
    ;; are non-pointer events such as key-press and key-release. 
    (unless (and (= hpos (prev-pointer-hpos zone-entry))
		 (= vpos (prev-pointer-vpos zone-entry)))
      (setf (prev-pointer-hpos zone-entry) hpos)
      (setf (prev-pointer-vpos zone-entry) vpos)
      ;; If any of the zones on the list has a client other than
      ;; the port of this zone-entry, then remove it. 
      (setf (motion-zones zone-entry)
	    (remove (port zone-entry)
		    (motion-zones zone-entry)
		    :key #'clim3-ext:client
		    :test-not #'eq))
      ;; For the remaining ones, call the handler.
      (loop for zone in (motion-zones zone-entry)
	    do (let ((absolute-hpos 0)
		     (absolute-vpos 0)
		     (parent zone))
		 ;; Find the absolute position of the zone.
		 (loop while (clim3:zone-p parent)
		       do (incf absolute-hpos (clim3:hpos parent))
			  (incf absolute-vpos (clim3:vpos parent))
			  (setf parent (clim3-ext:parent parent)))
		 (funcall (clim3:handler zone)
			  zone
			  (- hpos absolute-hpos)
			  (- vpos absolute-vpos)))))))

(defun handle-visit (zone-entry)
  (let ((prev (zone-containing-pointer zone-entry)))
    (multiple-value-bind (hpos vpos same-screen-p)
	(xlib:pointer-position (window zone-entry))
      ;; FIXME: Figure out what to do with same-screen-p
      (declare (ignore same-screen-p))
      (let ((zone
	      (block found
		(labels ((traverse (zone hpos vpos)
			   (unless (or (< hpos 0)
				       (< vpos 0)
				       (> hpos (clim3:width zone))
				       (> vpos (clim3:height zone)))
			     (if (and (typep zone 'clim3:visit)
				      (funcall (clim3:inside-p zone)
					       hpos vpos))
				 (return-from found zone)
				 (progn 
				   (clim3-ext:map-over-children-top-to-bottom
				    (lambda (child)
				      (traverse child
						(- hpos (clim3:hpos child))
						(- vpos (clim3:vpos child))))
				    zone)
				   nil)))))
		  (traverse (zone zone-entry) hpos vpos)))))
	(unless (eq zone prev)
	  (unless (null prev)
	    (clim3:leave prev))
	  (unless (null zone)
	    (clim3:enter zone))
	  (setf (zone-containing-pointer zone-entry) zone))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Experiment: build a skeleton zone hierarchy. 

(defclass skeleton ()
  ((%zone :initarg :zone :reader zone)
   (%hpos :initarg :hpos :reader hpos)
   (%vpos :initarg :vpos :reader vpos)
   (%width :initarg :width :reader width)
   (%height :initarg :height :reader height)
   (%children :initform '() :initarg :children :accessor children)))

(defmethod print-object ((object skeleton) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream
	    "zone: ~s hpos: ~s :vpos ~s :width ~s :height ~s"
	    (zone object)
	    (hpos object)
	    (vpos object)
	    (width object)
	    (height object))))	    

;;; The purpose of this function is to build a skeletal version of the
;;; visible zone hierarchy, so that this hierarchy can be compared to
;;; what was there last time around the event loop.  
;;;
;;; The parameters HMIN, VMIN, HMAX, and VMAX are in the coordinate
;;; system of ZONE, and they are inside ZONE.  In other words, HMIN
;;; and VMIN are non-negative, HMAX is less than or equal to the width
;;; of ZONE, and VMAX is less than or equal to the height of ZONE.
(defun build-visible-hierarchy (zone hmin vmin hmax vmax)
  (let ((children '()))
    (clim3-ext:map-over-children-top-to-bottom
     (lambda (child)
       (let ((chpos (clim3:hpos child))
	     (cvpos (clim3:vpos child))
	     (cwidth (clim3:width child))
	     (cheight (clim3:height child)))
	 (when (and (< chpos hmax)
		    (< cvpos vmax)
		    (> (+ chpos cwidth) hmin)
		    (> (+ cvpos cheight) vmin))
	   ;; The child is at least partially inside the area. 
	   (let ((new-hmin (max 0 (- hmin chpos)))
		 (new-vmin (max 0 (- vmin cvpos)))
		 (new-hmax (min cwidth (- hmax chpos)))
		 (new-vmax (min cheight (- vmax cvpos))))
	     (push (build-visible-hierarchy
		    child new-hmin new-vmin new-hmax new-vmax)
		   children)))))
     zone)
    (make-instance 'skeleton
		   :zone zone
		   :hpos (clim3:hpos zone)
		   :vpos (clim3:vpos zone)
		   :width (clim3:width zone)
		   :height (clim3:height zone)
		   :children (nreverse children))))

(defparameter *hierarchy* nil)

(defun build-top-level-hierarchy (zone)
  (setf *hierarchy*
	(let ((width (clim3:width zone))
	      (height (clim3:height zone)))
	  (build-visible-hierarchy zone 0 0 width height))))

;;; The purpose of this function is to scan the visible zone hierarchy
;;; and compare it to what was painted last time around the event
;;; loop.  This information is then used to determine whether we need
;;; to paint anything at all.  
;;;
(defun visible-hierarchy-unchanged-p (hierarchy zone hmin vmin hmax vmax)
  (and (eq (zone hierarchy) zone)
       (= (hpos hierarchy) (clim3:hpos zone))
       (= (vpos hierarchy) (clim3:vpos zone))
       (= (width hierarchy) (clim3:width zone))
       (= (height hierarchy) (clim3:height zone))
       (let ((children (children hierarchy)))
	 (clim3-ext:map-over-children-top-to-bottom
	  (lambda (child)
	    (if (or (null children)
		    (not (eq (zone (car children)) child))
		    (let ((chpos (clim3:hpos child))
			  (cvpos (clim3:vpos child))
			  (cwidth (clim3:width child))
			  (cheight (clim3:height child)))
		      (when (and (< chpos hmax)
				 (< cvpos vmax)
				 (> (+ chpos cwidth) hmin)
				 (> (+ cvpos cheight) vmin))
			;; The child is at least partially inside the
			;; area.
			(let ((new-hmin (max 0 (- hmin chpos)))
			      (new-vmin (max 0 (- vmin cvpos)))
			      (new-hmax (min cwidth (- hmax chpos)))
			      (new-vmax (min cheight (- vmax cvpos))))
			  (not (visible-hierarchy-unchanged-p
				(car children) child
				new-hmin new-vmin new-hmax new-vmax))))))
		(return-from visible-hierarchy-unchanged-p nil)
		(pop children)))
	  zone)
	 ;; If there are any children left in the hierarchy, then
	 ;; something has changed. 
	 (null children))))

(defun top-level-hierarchy-unchanged-p (zone)
  (and (not (null *hierarchy*))
       (let ((width (clim3:width zone))
	     (height (clim3:height zone)))
	 (visible-hierarchy-unchanged-p
	  *hierarchy* zone 0 0 width height))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Update zones.

(defun layout-visible-zones (zone width height)
  (labels ((aux (zone)
	     (when (typep zone 'clim3-ext:compound-mixin)
	       (clim3-ext:impose-child-layouts zone)
	       (clim3-ext:map-over-children
		(lambda (child)
		  (clim3:with-zone child
		    (aux child)))
		zone))))
    (let ((*hpos* 0)
	  (*vpos* 0)
	  (*hstart* 0)
	  (*vstart* 0)
	  (*hend* width)
	  (*vend* height))
      (aux zone))))
    
(defun paint-visible-area (zone width height pixel-array)
  (let ((*hpos* 0)
	(*vpos* 0)
	(*hstart* 0)
	(*vstart* 0)
	(*hend* width)
	(*vend* height)
	(*pixel-array* pixel-array))
    (clim3-ext:paint zone)))

(defun draw-default-background (pixel-array)
  (let* ((size (array-total-size pixel-array))
	 (v (make-array size
			:element-type '(unsigned-byte 32)
			:displaced-to pixel-array)))
    (fill v #xffffffff)))

(defun update (zone-entry)
  (with-accessors ((zone zone)
		   (gcontext gcontext)
		   (window window)
		   (pixel-array pixel-array)
		   (image image))
      zone-entry
    (let ((width (xlib:drawable-width window))
	  (height (xlib:drawable-height window)))
      (clim3-ext:impose-size zone width height)
      ;; Make sure the pixmap and the image object have the same
      ;; dimensions as the window.
      (when (or (/= width (array-dimension pixel-array 1))
		(/= height (array-dimension pixel-array 0)))
	(setf *hierarchy* nil)
	(setf pixel-array
	      (make-array (list height width)
			  :element-type '(unsigned-byte 32)))
	(setf image
	      (xlib:create-image :bits-per-pixel 32
				 :data pixel-array
				 :depth 24
				 :width width :height height
				 :format :z-pixmap)))
      (layout-visible-zones zone width height)
      (unless (top-level-hierarchy-unchanged-p zone)
	(build-top-level-hierarchy zone)
	(draw-default-background pixel-array)
	(paint-visible-area zone width height pixel-array))
      ;; Transfer the image. 
      (xlib::put-image window
		       gcontext
		       image
		       :x 0 :y 0
		       :width width :height height))))

(defmethod clim3-ext:repaint ((port clx-framebuffer-port))
  (loop for zone-entry in (zone-entries port)
	do (update zone-entry)))

(defmethod clim3:connect ((zone clim3:zone)
			       (port clx-framebuffer-port))
  (let ((zone-entry (make-instance 'zone-entry :port port :zone zone))
	(clim3:*port* port))
    ;; Register this zone entry.  We need to do this right away, because 
    ;; it has to exist when we set the parent of the zone. 
    (push zone-entry (zone-entries port))
    (setf (clim3-ext:parent zone) port)
    (clim3-ext:ensure-hsprawl-valid zone)
    (clim3-ext:ensure-vsprawl-valid zone)
    ;; Ask for a window that has the natural size of the zone.  We may
    ;; not get it, though.
    ;;
    ;; FIXME: take into account the position of the zone
    (setf (window zone-entry)
	  (multiple-value-bind (width height) (clim3:natural-size zone)
	    (xlib:create-window :parent (root port)
				:x 0 :y 0 :width width :height height
				:class :input-output
				:visual :copy
				:background (white port)
				:bit-gravity :north-west
				:event-mask
				'(:key-press :key-release
				  :button-motion :button-press :button-release
				  :enter-window :leave-window
				  :exposure
				  :structure-notify
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
    (setf *hierarchy* nil)
    ;; Make sure the window has the right contents.
    (update zone-entry)))

(defmethod clim3:disconnect (zone (port clx-framebuffer-port))
  (let ((zone-entry (find zone (zone-entries port) :key #'zone)))
    (xlib:free-gcontext (gcontext zone-entry))
    (xlib:destroy-window (window zone-entry))
    (setf (zone-entries port) (remove zone-entry (zone-entries port)))
    (when (null (zone-entries port))
      (xlib:close-display (display port)))
    (setf (clim3-ext:parent zone) nil)))

(defmethod clim3:text-style-ascent ((port clx-framebuffer-port)
                                    text-style)
  (clim3-fonts:ascent (clim3-fonts:text-style-to-font text-style)))

(defmethod clim3:text-style-descent ((port clx-framebuffer-port)
                                     text-style)
  (clim3-fonts:descent (clim3-fonts:text-style-to-font text-style)))

;;; These are not used for now.
;; (defmethod clim3:text-ascent ((port clx-framebuffer-port)
;; 				   text-style
;; 				   string)
;;   (let ((font (font port)))
;;     (reduce #'min string
;; 	    :key (lambda (char)
;; 		   (camfer:y-offset (camfer:find-glyph font char))))))

;; (defmethod clim3:text-descent ((port clx-framebuffer-port)
;; 				    text-style
;; 				    string)
;;   (let ((font (font port)))
;;     (reduce #'max string
;; 	    :key (lambda (char)
;; 		   (+ (array-dimension (camfer:mask (camfer:find-glyph font char)) 0)
;; 		      (camfer:y-offset (camfer:find-glyph font char)))))))


(defmethod clim3:text-width
    ((port clx-framebuffer-port) text-style string)
  (clim3-fonts:text-width (clim3-fonts:text-style-to-font text-style) string))

(defmethod clim3:text-prefix-length
    ((port clx-framebuffer-port) text-style string width)
  (clim3-fonts:text-prefix-length (clim3-fonts:text-style-to-font text-style) string width))

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

(defmethod clim3-ext:standard-key-decoder
    ((port clx-framebuffer-port) keycode modifiers)
  (if (keycode-is-modifier-p port keycode)
      nil
      (let ((keysyms (vector (xlib:keycode->keysym (display port) keycode 0)
			     (xlib:keycode->keysym (display port) keycode 1)
			     (xlib:keycode->keysym (display port) keycode 2)
			     (xlib:keycode->keysym (display port) keycode 3))))
	(cond ((= (aref keysyms 0) (aref keysyms 1))
	       ;; FIXME: do this better
	       (if (= (aref keysyms 0) 65293)
		   (cons #\Return
			 (modifier-names modifiers))
		   (cons (code-char (aref keysyms 0))
			 (modifier-names modifiers))))
	      ((plusp (logand +shift-mask+ modifiers))
	       (cons (code-char (aref keysyms 1))
		     (modifier-names
		      (logandc2 modifiers +shift-mask+))))
	      (t
	       (cons (code-char (aref keysyms 0))
		     (modifier-names modifiers)))))))

(defmethod clim3-ext:standard-button-decoder
    ((port clx-framebuffer-port) button-code modifiers)
  (cons (ecase button-code
	  (1 :button-1)
	  (2 :button-2)
	  (3 :button-3)
	  (4 :button-4)
	  (5 :button-5))
	(modifier-names modifiers)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Handling events.

(defmethod clim3:event-loop ((port clx-framebuffer-port))
  (let ((clim3:*port* port))
    (loop do (let ((event (xlib:event-case ((display port))
			    (:key-press
			     (window code state)
			     `(:key-press ,window ,code ,state))
			    (:key-release
			     (window code state)
			     `(:key-release ,window ,code ,state))
			    (:button-press
			     (window code state)
			     `(:button-press ,window ,code ,state))
			    (:button-release
			     (window code state)
			     `(:button-release ,window ,code ,state))
			    (:exposure
			     (window)
			     `(:exposure ,window))
			    (:enter-notify
			     (window)
			     `(:enter-notify ,window))
			    (:leave-notify
			     (window)
			     `(:leave-notify ,window))
			    (:motion-notify
			     (window display)
			     `(:motion-notify ,display ,window))
			    (t
			     ()
			     `(:other)))))
	       (cond  ((eq (car event) :other)
		       (loop for zone-entry in (zone-entries port)
			     do (handle-motion-zones zone-entry)
				(handle-visit zone-entry)
				(update zone-entry)))
		      ((eq (car event) :motion-notify)
		       (let ((display (cadr event)))
			 (when (null (xlib:event-listen display))
			   (let ((entry (find (caddr event) (zone-entries port)
					      :test #'eq :key #'window)))
			     (unless (null entry)
			       (handle-motion-zones entry)
			       (handle-visit entry)
			       (update entry))))))
		      (t
		       (let ((entry (find (cadr event) (zone-entries port)
					  :test #'eq :key #'window)))
			 (unless (null entry)
			   (handle-motion-zones entry)
			   (handle-visit entry)
			   (ecase (car event)
			     (:key-press 
			      (destructuring-bind (code state) (cddr event)
				(clim3:handle-key-press
				 clim3:*key-handler* code state)))
			     (:key-release 
			      (destructuring-bind (code state) (cddr event)
				(clim3:handle-key-release
				 clim3:*key-handler* code state)))
			     (:button-press 
			      (destructuring-bind (code state) (cddr event)
				(clim3:handle-button-press
				 clim3:*button-handler* code state)))
			     (:button-release 
			      (destructuring-bind (code state) (cddr event)
				(clim3:handle-button-release
				 clim3:*button-handler* code state)))
			     ((:exposure :enter-notify :leave-notify)
			      nil))
			   (update entry)))))))))
		     
		 
		
