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

(defun font-instance-paint-text (port text font color)
  (unless (zerop (length text))
    (let ((ascent (clim3-fonts:ascent font)))
      (flet ((y-pos (glyph)
               (+ ascent (clim3-fonts:glyph-y-offset glyph))))
        ;; paint the first character
        (let ((glyph (clim3-fonts:find-glyph font (char text 0))))
          (clim3:with-position ((- (clim3-fonts:glyph-x-offset glyph))
                                (y-pos glyph))
            (clim3-ext:paint-mask port (clim3-fonts:glyph-mask glyph) color)))
        (loop with x = 0
              for i from 1 below (length text)
              do (let* ((curr (char text i))
                        (prev (char text (1- i)))
                        (glyph (clim3-fonts:find-glyph font curr)))
                   ;; compute the new x position
                   (incf x (+ (clim3-fonts:glyph-width font prev)
                              (clim3-fonts:glyph-space font prev curr)))
                   (clim3:with-position (x (y-pos glyph))
                     (clim3-ext:paint-mask port (clim3-fonts:glyph-mask glyph) color))))))))

(defmethod clim3-ext:paint-text
    ((port clx-framebuffer-port) text text-style color)
  (font-instance-paint-text
   port text (clim3-fonts:text-style-to-font text-style) color))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Paint trapezoids.

(defmethod clim3-ext:paint-trapezoids
    ((port clx-framebuffer-port) trapezoids color)
  (multiple-value-bind (opacities min-x min-y)
      (clim3-rendering:render-trapezoids trapezoids)
    (clim3:with-position (min-x min-y)
      (clim3-ext:paint-mask port opacities color))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Paint path.

(defmethod clim3-ext:paint-path
    ((port clx-framebuffer-port) path color)
  (multiple-value-bind (opacities min-x min-y)
      (clim3-rendering:render-path path)
    (clim3:with-position (min-x min-y)
      (clim3-ext:paint-mask port opacities color))))
