(cl:in-package #:clim3-clx-framebuffer)

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

;;; Paint the part of the mask that falls within
;;; the clipping region.
(defmethod clim3-port:new-port-paint-mask
    ((port clx-framebuffer-port) mask color)
  (loop for hpos from hstart below hend
	do (loop for vpos from vstart below vend
		 do (clim3-port:paint-pixel
		     *port* hpos vpos
		     (clim3-color:red color)
		     (clim3-color:green color)
		     (clim3-color:blue color)
		     (aref mask vpos hpos)))))

