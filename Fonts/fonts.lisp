(in-package :clim3-fonts)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mapping from text styles to font instances.
;;;
;;; Do this better later.

(defparameter *camfer-sans-roman* (make-hash-table))

(defparameter *freefont-directory*
  (or (cl-fad:directory-exists-p #p"/usr/share/fonts/truetype/freefont/")
      (cl-fad:directory-exists-p #p"/usr/local/share/fonts/freefont/")
      (cl-fad:directory-exists-p #p"/usr/share/fonts/TTF/")))

(defparameter *freefont-files*
  '(((:fixed :roman)            "FreeMono.ttf")
    ((:fixed :bold)             "FreeMonoBold.ttf")
    ((:fixed :oblique)          "FreeMonoOblique.ttf")
    ((:fixed (:bold :oblique))  "FreeMonoBoldOblique.ttf")
    ((:sans :roman)             "FreeSans.ttf")
    ((:sans :bold)              "FreeSansBold.ttf")
    ((:sans :oblique)           "FreeSansOblique.ttf")
    ((:sans (:bold :oblique))   "FreeSansBoldOblique.ttf")
    ((:serif :roman)            "FreeSerif.ttf")
    ((:serif :bold)             "FreeSerifBold.ttf")
    ((:serif :italic)           "FreeSerifItalic.ttf")
    ((:serif (:bold :italic))   "FreeSerifBoldItalic.ttf")))

;;; Keys are lists of the form (<family> <face>).  Values are of the
;;; form (<font loader> . <instances>) where <instances> is a hash
;;; table with the size as a key and a font instance as the value.
(defparameter *freefont-fonts* (make-hash-table :test #'equal))
  
(defparameter *ubuntufont-directory*
  (or (cl-fad:directory-exists-p #p"/usr/share/fonts/truetype/ubuntu-font-family/")
      (cl-fad:directory-exists-p #p"/usr/local/share/fonts/ubuntu/")
      (cl-fad:directory-exists-p #p"/usr/share/fonts/TTF/")))

(defparameter *ubuntufont-files*
  '(((:fixed :roman)            "UbuntuMono-R.ttf")
    ((:fixed :bold)             "UbuntuMono-B.ttf")
    ((:fixed :oblique)          "UbuntuMono-RI.ttf")
    ((:fixed (:bold :oblique))  "UbuntuMono-BI.ttf")
    ((:sans :roman)             "Ubuntu-R.ttf")
    ((:sans :bold)              "Ubuntu-B.ttf")
    ((:sans :oblique)           "Ubuntu-RI.ttf")
    ((:sans (:bold :oblique))   "Ubuntu-BI.ttf")))

;;; Keys are lists of the form (<family> <face>).  Values are of the
;;; form (<font loader> . <instances>) where <instances> is a hash
;;; table with the size as a key and a font instance as the value.
(defparameter *ubuntufont-fonts* (make-hash-table :test #'equal))

(defun text-style-to-font (text-style)
  (with-accessors ((foundry clim3:foundry)
		   (family clim3:family)
		   (face clim3:face)
		   (size clim3:size))
      text-style
    (ecase foundry
      (:camfer (progn (unless (and (eq family :sans)
				   (eq face :roman))
			(error "Camfer font currently has only sans roman"))
		      (or (gethash size *camfer-sans-roman*)
			  (setf (gethash size *camfer-sans-roman*)
				(camfer:make-font size 100)))))
      (:free
       (let* ((family-face (list (clim3:family text-style)
				 (clim3:face text-style)))
	      (font (gethash family-face *freefont-fonts*)))
	 (cond ((null font)
		(let ((filename (cadr (find family-face *freefont-files*
					    :key #'car
					    :test #'equal))))
		  (when (null filename)
		    (error "No font file found"))
		  (let* ((pathname (merge-pathnames filename *freefont-directory*))
			 (font-loader (zpb-ttf:open-font-loader pathname))
			 (instances (make-hash-table)))
		    (setf (gethash family-face *freefont-fonts*)
			  (cons font-loader instances))
		    (let ((instance (clim3-truetype:instantiate-font
				     font-loader
				     size)))
		      (setf (gethash size instances) instance)))))
	       ((null (gethash size (cdr font)))
		(let ((instance (clim3-truetype:instantiate-font
				 (car font)
				 size)))
		  (setf (gethash size (cdr font)) instance)))
	       (t
		(gethash size (cdr font))))))
      (:ubuntu
       (let* ((family-face (list (clim3:family text-style)
				 (clim3:face text-style)))
	      (font (gethash family-face *ubuntufont-fonts*)))
	 (cond ((null font)
		(let ((filename (cadr (find family-face *ubuntufont-files*
					    :key #'car
					    :test #'equal))))
		  (when (null filename)
		    (error "No font file found"))
		  (let* ((pathname (merge-pathnames filename *ubuntufont-directory*))
			 (font-loader (zpb-ttf:open-font-loader pathname))
			 (instances (make-hash-table)))
		    (setf (gethash family-face *ubuntufont-fonts*)
			  (cons font-loader instances))
		    (let ((instance (clim3-truetype:instantiate-font
				     font-loader
				     size)))
		      (setf (gethash size instances) instance)))))
	       ((null (gethash size (cdr font)))
		(let ((instance (clim3-truetype:instantiate-font
				 (car font)
				 size)))
		  (setf (gethash size (cdr font)) instance)))
	       (t
		(gethash size (cdr font)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Text styles.

(defgeneric ascent (font))

(defmethod ascent ((font camfer:font))
  (camfer:ascent font))

(defmethod ascent ((font clim3-truetype:font-instance))
  (- (clim3-truetype:ascender font)))

(defgeneric descent (font))

(defmethod descent ((font camfer:font))
  (camfer:descent font))

(defmethod descent ((font clim3-truetype:font-instance))
  (clim3-truetype:descender font))

(defun glyph-space (font char1 char2)
  ;; Wing it for the space character for now.
  (if (or (eql char1 #\Space) (eql char2 #\Space))
      0
      (- (* 2 (camfer::stroke-width font))
	 (camfer:kerning font
			 (camfer:find-glyph font char1)
			 (camfer:find-glyph font char2)))))

(defun glyph-width (font char)
  ;; Wing it for the space character for now.
  (if (eql char #\Space)
      10
      (array-dimension (camfer:mask (camfer:find-glyph font char)) 1)))

(defgeneric text-width (font text))

(defmethod text-width ((font camfer:font) string)
  (if (zerop (length string))
      0
      (+ (glyph-width font (char string 0))
	 (loop for i from 1 below (length string)
	       sum (+ (glyph-width font (char string i))
		      (glyph-space font
				   (char string (1- i))
				   (char string i)))))))

(defmethod text-width ((font clim3-truetype:font-instance) string)
  (let ((length (length string))
	(glyphs (map 'vector
		     (lambda (char)
		       (clim3-truetype:find-glyph-instance font char))
		     string)))
    (if (zerop length)
	0
	(- (+ ;; This is not perfect, but also not too bad
	    (clim3-truetype:x-offset (aref glyphs (1- length)))
	    (loop for i from 0 to (1- length)
		  sum (clim3-truetype:advance-width (aref glyphs i))))
	   (clim3-truetype:x-offset (aref glyphs 0))))))

(defgeneric text-prefix-length (font text width))

(defmethod text-prefix-length
    ((font camfer:font) string width)
  (if (or (zerop (length string))
	  (> (glyph-width font (char string 0)) width))
      0
      (loop with w = (glyph-width font (char string 0))
	    for i from 1 below (length string)
	    for prev = (char string (1- i))
	    for cur = (char string i)
	    for space = (glyph-space font prev cur)
	    for cur-width = (glyph-width font cur)
	    do (incf w (+ space cur-width))
	       (when (> w width)
		 (return w))
	    finally (return width))))

(defmethod text-prefix-length
    ((font clim3-truetype:font-instance) string width)
  (let ((length (length string))
	(glyphs (map 'vector
		     (lambda (char)
		       (clim3-truetype:find-glyph-instance font char))
		     string)))
    (loop with w = 0
	  for i from 0 below length
	  for glyph = (aref glyphs i)
	  for advance-width = (clim3-truetype:advance-width glyph)
	  do (incf w advance-width)
	     (when (> w width)
	       (return i))
	  finally (return length))))

(defgeneric glyphs-string (font))

(defmethod glyphs-string ((font camfer:font))
  (coerce (loop for k being the hash-keys in (camfer::glyphs font)
                when (characterp k)
                  collect k) 'string))

(defmethod glyphs-string ((font clim3-truetype:font-instance))
  (format t "~&~a~%" (clim3-truetype::glyph-instances font))
  "The quick fox jumps over the lazy dog")
