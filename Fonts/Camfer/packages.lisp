(defpackage :mf
  (:use #:cl)
  (:export #:make-bezier-segment #:bezier-segment
	   #:make-open-path #:make-closed-path
	   #:closed-path #:concatenate-paths #:path-start
	   #:close-path
	   #:polygonalize
	   #:path-bounding-box
	   #:scan-lines
	   #:first-line #:nb-lines #:crossings
	   #:translate #:rotate #:scale #:slant #:reverse-path
	   #:draw-path #:with-pen
	   #:+razor+ #:+unit-square+
	   #:+quarter-circle+ #:+half-circle+ #:+full-circle+
	   #:superellipse
	   ;; mf-like stuff
	   #:paths #:mf #:paths #:control #:controls #:tension #:tensions
	   #:& #:-- #:--- #:curl #:direction #:cycle
	   #:left #:right #:up #:down))

(defpackage #:camfer
    (:use #:common-lisp #:mf)
  (:export #:ascent #:descent #:width
	   #:font #:make-font
	   #:glyph #:make-glyph
	   #:find-glyph #:x-offset #:y-offset #:kerning #:mask
	   #:draw-text))
