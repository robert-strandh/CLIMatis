(cl:in-package #:clim3-clx-framebuffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Paint a single pixel.

;;; Paint a pixel at *hpos* *vpos* provided that that
;;; pixel is in the clipping region.
(defmethod clim3-ext:paint-pixel
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

(defmethod clim3-ext:paint-opaque
    ((port clx-framebuffer-port) color)
  (let* ((pa *pixel-array*)
	 (height (array-dimension pa 0))
	 (width (array-dimension pa 1))
	 (pixel (+ (ash (round (* 255 (clim3:red color))) 16)
		   (ash (round (* 255 (clim3:green color))) 8)
		   (ash (round (* 255 (clim3:blue color))) 0)))
	 (hstart *hstart*)
	 (hend *hend*)
	 (vstart *vstart*)
	 (vend *vend*))
    (declare (type (simple-array (unsigned-byte 32) (* *)) pa)
	     (type (unsigned-byte 32) pixel)
	     (type (integer 0) hstart hend vstart vend))
    (let ((v (make-array (* height width)
			 :element-type '(unsigned-byte 32)
			 :displaced-to pa)))
      (loop for vp from vstart below vend
	    do (fill v pixel
		     :start (+ (* vp width) hstart)
		     :end (+ (* vp width) hend))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Paint a mask.

(defmethod clim3-ext:paint-mask
    ((port clx-framebuffer-port) mask color)
  (let ((width (array-dimension mask 1))
	(height (array-dimension mask 0))
	(r (clim3:red color))
	(g (clim3:green color))
	(b (clim3:blue color))
	(pa *pixel-array*))
    (flet ((do-pixel (vpos hpos row col)
	     (let  ((alpha (aref mask row col)))
	       (cond ((< alpha 0.002)
		      nil)
		     ((> alpha 0.998)
		      (setf (aref pa vpos hpos)
			    (+ (ash (round (* 255 r)) 16)
			       (ash (round (* 255 g)) 8)
			       (ash (round (* 255 b)) 0))))
		     (t
		      (let* ((pixel (aref pa vpos hpos))
			     (old-r (/ (logand 255 (ash pixel -16)) 255d0))
			     (old-g (/ (logand 255 (ash pixel -8)) 255d0))
			     (old-b (/ (logand 255 pixel) 255d0))
			     (new-r (+ (* r alpha) (* old-r (- 1d0 alpha))))
			     (new-g (+ (* g alpha) (* old-g (- 1d0 alpha))))
			     (new-b (+ (* b alpha) (* old-b (- 1d0 alpha))))
			     (new-pixel (+ (ash (round (* 255 new-r)) 16)
					   (ash (round (* 255 new-g)) 8)
					   (ash (round (* 255 new-b)) 0))))
			(setf (aref pa vpos hpos) new-pixel)))))))
      (loop for vpos from (max *vstart* *vpos*) below *vend*
	    for row from (max 0 (- *vstart* *vpos*)) below height
	    do (loop for hpos from (max *hstart* *hpos*) below *hend*
		     for col from (max 0 (- *hstart* *hpos*)) below width
		     do (do-pixel vpos hpos row col))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Paint a uniformly translucent area. 

(defmethod clim3-ext:paint-translucent
    ((port clx-framebuffer-port) color opacity)
  (let ((r (clim3:red color))
	(g (clim3:green color))
	(b (clim3:blue color)))
    (loop for vpos from (max *vstart* *vpos*) below *vend*
	  do (loop for hpos from (max *hstart* *hpos*) below *hend*
		   do (let* ((pa *pixel-array*)
			     (pixel (aref pa vpos hpos))
			     (old-r (/ (logand 255 (ash pixel -16)) 255d0))
			     (old-g (/ (logand 255 (ash pixel -8)) 255d0))
			     (old-b (/ (logand 255 pixel) 255d0))
			     (new-r (+ (* r opacity) (* old-r (- 1d0 opacity))))
			     (new-g (+ (* g opacity) (* old-g (- 1d0 opacity))))
			     (new-b (+ (* b opacity) (* old-b (- 1d0 opacity))))
			     (new-pixel (+ (ash (round (* 255 new-r)) 16)
					   (ash (round (* 255 new-g)) 8)
					   (ash (round (* 255 new-b)) 0))))
			(setf (aref pa vpos hpos) new-pixel))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Paint text

(defgeneric font-instance-paint-text (port font-instance text color))

(defmethod font-instance-paint-text
    (port text (font-instance camfer:font) color)
  (let ((ascent (font-instance-ascent font-instance)))
    (unless (zerop (length text))
      (flet ((y-pos (char)
	       ;; I don't know why the -1 is necessary
	       (+ -1 ascent (camfer:y-offset (camfer:find-glyph font-instance char))))
	     (mask (char)
	       (camfer:mask (camfer:find-glyph font-instance char))))
	;; paint the first character
	(unless (eql (char text 0) #\Space)
	  (clim3:with-position (0 (y-pos (char text 0)))
	    (clim3-ext:paint-mask port (mask (char text 0)) color)))
 	(loop with pos-x = 0
 	      for i from 1 below (length text)
 	      for glyph = (camfer:find-glyph font-instance (char text i))
 	      do (progn
 		   ;; compute the new x position
 		   (incf pos-x
 			 (+ (glyph-width font-instance
					 (char text (1- i)))
 			    (glyph-space font-instance
					 (char text (1- i))
					 (char text i))))
		   (unless (eql (char text i) #\Space)
		     (clim3:with-position (pos-x (y-pos (char text i)))
		       (clim3-ext:paint-mask port (mask (char text i)) color)))))))))

(defmethod font-instance-paint-text
    (port text (font-instance clim3-truetype:font-instance) color)
  (let ((ascent (font-instance-ascent font-instance))
	(glyphs (map 'list
		     (lambda (char)
		       (clim3-truetype:find-glyph-instance font-instance char))
		     text)))
    (unless (zerop (length text))
      (flet ((y-pos (glyph)
	       (+ ascent (clim3-truetype:y-offset glyph))))
	(clim3:with-position
	    ((- (clim3-truetype:x-offset (car glyphs))) 0)
	  (loop for x = 0 then (+ x (clim3-truetype:advance-width glyph))
		for glyph in glyphs
		do (clim3:with-position ((+ x (clim3-truetype:x-offset glyph))
					      (y-pos glyph))
		     (clim3-ext:paint-mask
		      port (clim3-truetype:mask glyph) color))))))))

(defmethod clim3-ext:paint-text
    ((port clx-framebuffer-port) text text-style color)
  (font-instance-paint-text
   port text (text-style-to-font-instance text-style) color))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Paint trapezoids.

(defmethod clim3-ext:paint-trapezoids
    ((port clx-framebuffer-port) trapezoids color)
  (multiple-value-bind (opacities min-x min-y)
      (clim3-rendering:render-trapezoids trapezoids)
    (clim3:with-position (min-x min-y)
      (clim3:paint-mask opacities color))))
