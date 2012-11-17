(cl:in-package #:clim3-clx-framebuffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Paint a single pixel.

;;; Paint a pixel at *hpos* *vpos* provided that that
;;; pixel is in the clipping region.
(defmethod clim3-port:new-port-paint-pixel
    ((port clx-framebuffer-port) r g b alpha)
  (when (and (>= *hpos* *hstart*)
	     (>= *vpos* *vstart*)
	     (< *hpos* *hend*)
	     (< *vpos* *vend*))
    (let* ((pa *pixel-array*)
	   (hp *hpos*)
	   (vp *vpos*)
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
      (setf (aref pa vp hp) new-pixel))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Paint the entire clipping region with an opaque color.

(defmethod clim3-port:new-port-paint-opaque
    ((port clx-framebuffer-port) color)
  (let ((pa *pixel-array*)
	(pixel (+ (ash (round (* 255 (clim3-color:red color))) 16)
		  (ash (round (* 255 (clim3-color:green color))) 8)
		  (ash (round (* 255 (clim3-color:blue color))) 0)))
	(hstart *hstart*)
	(hend *hend*)
	(vstart *vstart*)
	(vend *vend*))
    (declare (type (simple-array (unsigned-byte 32) (* *)) pa)
	     (type (unsigned-byte 32) pixel)
	     (type (integer 0) hstart hend vstart vend))
    (if (and (typep hstart 'fixnum)
	     (typep hend 'fixnum)
	     (typep vstart 'fixnum)
	     (typep vend 'fixnum))
	(locally (declare (type fixnum hstart hend vstart vend))
	  (loop for vp from vstart below vend
		do (loop for hp from hstart below hend
			 do (setf (aref pa vp hp) pixel))))
	(loop for vp from vstart below vend
	      do (loop for hp from hstart below hend
		       do (setf (aref pa vp hp) pixel))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Paint a mask.

(defmethod clim3-port:new-port-paint-mask
    ((port clx-framebuffer-port) mask color)
  (let ((width (array-dimension mask 1))
	(height (array-dimension mask 0))
	(r (clim3-color:red color))
	(g (clim3-color:green color))
	(b (clim3-color:blue color)))
    (loop for vpos from (max *vstart* *vpos*) below *vend*
	  for row from (max 0 (- *vstart* *vpos*)) below height
	  do (loop for hpos from (max *hstart* *hpos*) below *hend*
		   for col from (max 0 (- *hstart* *hpos*)) below width
		   do (let* ((pa *pixel-array*)
			     (pixel (aref pa vpos hpos))
			     (alpha (aref mask row col))
			     (old-r (/ (logand 255 (ash pixel -16)) 255d0))
			     (old-g (/ (logand 255 (ash pixel -8)) 255d0))
			     (old-b (/ (logand 255 pixel) 255d0))
			     (new-r (+ (* r alpha) (* old-r (- 1d0 alpha))))
			     (new-g (+ (* g alpha) (* old-g (- 1d0 alpha))))
			     (new-b (+ (* b alpha) (* old-b (- 1d0 alpha))))
			     (new-pixel (+ (ash (round (* 255 new-r)) 16)
					   (ash (round (* 255 new-g)) 8)
					   (ash (round (* 255 new-b)) 0))))
			(setf (aref pa vpos hpos) new-pixel))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Paint text

(defmethod clim3-port:new-port-paint-text
    ((port clx-framebuffer-port) text text-style color)
  (let* ((font (font port))
	 (ascent (clim3-port:text-style-ascent port text-style)))
    (unless (zerop (length text))
      (flet ((y-pos (char)
	       ;; I don't know why the -1 is necessary
	       (+ -1 ascent (camfer:y-offset (camfer:find-glyph font char))))
	     (mask (char)
	       (camfer:mask (camfer:find-glyph font char))))
	;; paint the first character
	(clim3-port:with-position (0 (y-pos (char text 0)))
	  (clim3-port:new-port-paint-mask port (mask (char text 0)) color))
 	(loop with pos-x = 0
 	      for i from 1 below (length text)
 	      for glyph = (camfer:find-glyph font (char text i))
 	      do (progn
 		   ;; compute the new x position
 		   (incf pos-x
 			 (+ (glyph-width font (char text (1- i)))
 			    (glyph-space font (char text (1- i)) (char text i))))
		   (clim3-port:with-position (pos-x (y-pos (char text i)))
		     (clim3-port:new-port-paint-mask port (mask (char text i)) color))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Paint trapezoids.

(defmethod clim3-port:new-port-paint-trapezoids
    ((port clx-framebuffer-port) trapezoids color)
  (multiple-value-bind (opacities min-x min-y)
      (clim3-rendering:render-trapezoids trapezoids)
    (clim3-port:with-position (min-x min-y)
      (clim3-port:new-paint-mask opacities color))))
