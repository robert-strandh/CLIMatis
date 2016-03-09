;;; FIXME: the bends in the `f', the `t', and the `j' should look the
;;; same, but currently the code is duplicated.  Improve by making
;;; the width and the start of the bend font-wide parameters. 

(in-package #:camfer)

(defgeneric ascent (font))
(defgeneric descent (font))
(defgeneric width (font))

(defclass font ()
  ((%ascent :initarg :ascent :reader ascent)
   (%lower-case-ascent :initarg :lower-case-ascent :reader lower-case-ascent)
   (%stroke-width :initarg :stroke-width :reader stroke-width)
   (%stroke-height :initarg :stroke-height :reader stroke-height)
   (%upper-case-h-width :initarg :upper-case-h-width :reader upper-case-h-width)
   (%upper-case-o-width :initarg :upper-case-o-width :reader upper-case-o-width)
   (%digit-width :initarg :digit-width :reader digit-width)
   (%upper-case-h-bar-position :initarg :upper-case-h-bar-position
			       :reader upper-case-h-bar-position)
   (%width :initarg :width :reader width)
   (%descent :initarg :descent :reader descent)
   (%j-width :initarg :j-width :reader j-width)
   ;; The horizontal distance from the right edge of the `j'
   ;; to the min-point of the hook
   (%j-hook-extreme :initarg :j-hook-extreme :reader j-hook-extreme)
   ;; The vertical distance beteween the bottom of the j and
   ;; the point where the hook starts. 
   (%j-hook-start :initarg :j-hook-start :reader j-hook-start)
   (%slash-width :initarg :slash-width :reader slash-width)
   (%bracket-descent :initarg :bracket-descent :reader bracket-descent)
   (%bracket-width :initarg :bracket-width :reader bracket-width)
   (%bracket-ascent :initarg :bracket-ascent :reader bracket-ascent)
   (%kerning-info :initform (make-hash-table :test #'equal) :reader kerning-info)
   (%glyphs :initform (make-hash-table) :reader glyphs)))

(defclass glyph ()
  ((%x-offset :initarg :x-offset :reader x-offset)
   (%y-offset :initarg :y-offset :reader y-offset)
   (%left-shape :initarg :left-shape :reader left-shape)
   (%right-shape :initarg :right-shape :reader right-shape)
   (%mask :initarg :mask :reader mask)))

(defun make-glyph (paths left-shape right-shape)
  (let ((x-min most-positive-fixnum)
	(y-min most-positive-fixnum)
	(x-max most-negative-fixnum)
	(y-max most-negative-fixnum))
    (flet ((convert-path-to-knots (path)
	     (let* ((dpath (paths:make-discrete-path path))
		    (iterator (paths:path-iterator dpath)))
	       (paths:path-iterator-reset iterator)
	       (loop with end = nil
		     collect (multiple-value-bind (interpolation knot end-p)
				 (paths:path-iterator-next iterator)
			       (declare (ignore interpolation))
			       (setf end end-p)
			       knot)
		     until end)))
	   (determine-min-max (x y alpha)
	     (declare (ignore alpha))
	     (setf x-min (min x-min x)
		   y-min (min y-min y)
		   x-max (max x-max x)
		   y-max (max y-max y))))
      (let ((state (aa:make-state))
	    (knot-paths (mapcar #'convert-path-to-knots paths)))
	(loop for path in knot-paths
	      do (loop for (p1 p2) on path
		       until (null p2)
		       do (aa:line-f state
				     (paths:point-x p1) (paths:point-y p1)
				     (paths:point-x p2) (paths:point-y p2))))
	(aa:cells-sweep state #'determine-min-max)
	(let* ((height (1+ (- y-max y-min)))
	       (width (1+ (- x-max x-min)))
	       (mask (make-array (list height width)
				  :element-type 'double-float
				  :initial-element 0d0)))
	  (aa:cells-sweep state (lambda (x y alpha)
				  (setf alpha (min 256 (max 0 alpha)))
				  (setf (aref mask (- y-max y) (- x x-min))
					(/ alpha 256d0))))
	  (make-instance 'glyph
	    :x-offset x-min :y-offset (- y-max)
	    :left-shape left-shape :right-shape right-shape
	    :mask mask))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Basic character elements


;;;         1
;;;         |
;;;         **
;;;     0 -****- 2
;;;         **
;;;         |
;;;         3

;;; If the stroke width is odd, then the x coordinate of the
;;; center point will be in the middle of a pixel, and if is
;;; even, then the x coordinate will be an integer.  A similar
;;; thing holds for the stroke height and the y coordinate.
(defun make-dot (font center)
  (with-accessors ((stroke-width stroke-width)
		   (stroke-height stroke-height))
    font
    (flet ((c (x y) (complex x y)))
      (let* ((x (realpart center))
	     (y (imagpart center))
	     (sw/2 (/ stroke-width 2))
	     (sh/2 (/ stroke-height 2))
	     (p0 (c (- x sw/2) y))
	     (p1 (c x (+ y sh/2)))
	     (p2 (c (+ x sw/2) y))
	     (p3 (c x (- y sh/2))))
	(mf p0 up ++ p1 right ++ p2 down ++ p3 left ++ cycle)))))

;;;
;;;
;;;                2
;;;                |     
;;;             *******
;;;          *************
;;;         ****   |   ****
;;;        ****    6    ****
;;;        ****         ****
;;;        ****         ****
;;;     1- ****-5     7-****-3
;;;        ****         ****
;;;        ****         ****
;;;        ****    4    ****
;;;         ****   |   ****
;;;          *************
;;;             *******
;;;                |
;;;                0
;;;
(defun make-o-paths (font)
  (with-accessors ((width width)
		   (lower-case-ascent lower-case-ascent)
		   (stroke-width stroke-width)
		   (stroke-height stroke-height))
    font
    (let* ((p0 (complex (/ width 2) 0))
	   (p1 (complex 0 (/ lower-case-ascent 2)))
	   (p2 (complex (/ width 2) lower-case-ascent))
	   (p3 (complex width (/ lower-case-ascent 2)))
	   (p4 (+ p0 (complex 0 stroke-height)))
	   (p5 (+ p1 stroke-width))
	   (p6 (- p2 (complex 0 stroke-height)))
	   (p7 (- p3 stroke-width)))
      (list (mf p0 left ++ p1 up ++ p2 right ++ p3 down ++ cycle)
	    (mf p4 right ++ p7 up ++ p6 left ++ p5 down ++ cycle)))))

;;;       
;;;       
;;;       |    |
;;;       |    |
;;;       |    |
;;;       |    |
;;;       |    |
;;;       |    |
;;;       |    |    3
;;;       |    |   /
;;;       |    |******
;;;       |  **|*********   2
;;;       | ***|* |  ***** /
;;;     4-|****|  6 7- ****    \
;;;       |    |\5     ****     |
;;;       |    |       ****     |
;;;       |    |       ****     |
;;;       |    |       ****     | bend-start
;;;       |    |       ****     |
;;;       |    |       ****     |
;;;       |____|       ****    /
;;;                   /    \
;;;                  0      1
;;;       
;;;       \_______________/
;;;               |
;;;             width
;;;
(defun make-h-m-n-hook (font p0 width)
  (with-accessors ((lower-case-ascent lower-case-ascent)
		   (stroke-width stroke-width)
		   (stroke-height stroke-height))
    font
    (flet ((c (x y) (complex x y)))
      (let* ((bend-start (- lower-case-ascent (* stroke-height 3.0)))
	     (p1 (+ p0 stroke-width))
	     (p2 (+ p1 (c 0 bend-start)))
	     (p4 (- p2 width))
	     (p5 (+ p4 (* 0.5 stroke-width)))
	     (p3 (c (realpart (* 1/2 (+ p2 p4))) lower-case-ascent))
	     (p6 (- p3 (c 0 stroke-height)))
	     (p7 (- p2 stroke-width)))
	(mf p0 --
	    p7 up ++
	    p6 left ++
	    down p5 --
	    p4 up ++
	    p3 right ++
	    down p2 --
	    p1 -- cycle)))))

;;;
;;;      1      2
;;;       \    /
;;;        ++++
;;;        ++++
;;;        ++++
;;;        ++++
;;;        ++++
;;;        ++++
;;;        ++++
;;;        ++++
;;;        ++++
;;;        ++++
;;;        ++++
;;;        ++++
;;;        ++++
;;;        ++++
;;;        ++++
;;;        ++++
;;;        ++++
;;;        ++++
;;;        ++++
;;;        ++++
;;;        ++++
;;;        ++++
;;;       /    \
;;;      0      3
;;;

(defun make-vertical-stroke (font p0 height)
  (with-accessors ((stroke-width stroke-width))
    font
    (let* ((p1 (+ p0 (complex 0 height)))
	   (p2 (+ p1 stroke-width))
	   (p3 (+ p0 stroke-width)))
      (mf p0 -- p1 -- p2 -- p3 -- cycle))))

;;;      
;;;      
;;;     1                                         2
;;;      *****************************************
;;;      *****************************************
;;;     0                                         3
;;;      
;;;      

(defun make-horizontal-stroke (font p0 width)
  (with-accessors ((stroke-height stroke-height))
    font
    (let* ((p1 (+ p0 (complex 0 stroke-height)))
	   (p2 (+ p1 width))
	   (p3 (+ p0 width)))
      (mf p0 -- p1 -- p2 -- p3 -- cycle))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The characters


;;;        
;;;        
;;;        
;;;        
;;;        
;;;        
;;;        
;;;        
;;;               13
;;;                |
;;;            *********
;;;          *************
;;;      12- ***   10  ****                          -
;;;           11 1   9-****- 14                -      |
;;;               \    ****                     |     |
;;;           *************              -      |     |
;;;         ***************               |     |     | h4
;;;        ****    5   ****               |     | h3  |
;;;     0 -****- 4   6-****- 2   -        | h2  |     |
;;;        ****    7   ****       |       |     |     |
;;;         ***************       | h1    |     |     |
;;;           ******** ****      _       _|    _|    _|
;;;                |   |   \
;;;                3   8    15

(defun make-glyph-lower-case-a (font)
  (with-accessors ((width width)
		   (lower-case-ascent lower-case-ascent )
		   (stroke-width stroke-width )
		   (stroke-height stroke-height))
    font
    (flet ((c (x y) (complex x y)))
      (let* ((h1 (* lower-case-ascent 0.3))
	     (h2 (round (* lower-case-ascent 0.65)))
	     (h3 (* lower-case-ascent 0.7))
	     (h4 (round (* lower-case-ascent 0.75)))
	     (p0 (c 0 h1))
	     (p3 (c (* width 0.5) 0))
	     (p1 (+ p3 (c 0 h2)))
	     (p2 (+ p0 width))
	     (p4 (+ p0 stroke-width))
	     (p5 (- p1 (c 0 stroke-height)))
	     (p6 (- p2 stroke-width))
	     (p7 (+ p3 (c 0 stroke-height)))
	     (p8 (c (- width stroke-width) 0))
	     (p9 (c (- width stroke-width) h3))
	     (p10 (c (* width 0.6) (- lower-case-ascent stroke-height)))
	     (p12 (c (round (* width 0.1)) h4))
	     (p11 (+ p12 stroke-width))
	     (p13 (+ p10 (c 0 stroke-height)))
	     (p14 (+ p9 (c stroke-width 0)))
	     (p15 (c width 0)))
	(make-glyph
	 (list (mf p0 up ++ p1 right ++ p2 down ++ p3 left ++ cycle)
	       (mf p4 down ++ p7 right ++ p6 up ++ p5 left ++ cycle)
	       (mf p8 -- p9 up ++ p10 left ++ down p11 --
		   p12 up ++ p13 right ++ down p14 -- p15 -- cycle))
	 #\a #\i)))))

(defun make-glyph-lower-case-b (font)
  (with-accessors ((ascent ascent))
    font
    (make-glyph (cons (make-vertical-stroke font #c(0 0) ascent)
		      (make-o-paths font))
		#\l #\o)))
	
;;;                     
;;;             *******
;;;          ************* 
;;;         ****       ****
;;;        ****         */ -2
;;;        ****        /
;;;        ****      /  
;;;        ****  0-*            
;;;        ****      \  
;;;        ****        \
;;;        ****         *\ -1 \
;;;         ****       ****    |
;;;          *************     | h
;;;             *******       /
;;;                
(defun make-glyph-lower-case-c (font)
  (with-accessors ((width width)
		   (lower-case-ascent lower-case-ascent)
		   (stroke-height stroke-height))
    font
    (let* ((h (* stroke-height (if (< width 5) 1 1.7)))
	   (p0 (complex (/ width 2) (/ lower-case-ascent 2)))
	   (p1 (complex width h))
	   (p2 (complex width (- lower-case-ascent h))))
      (make-glyph (cons (mf p0 -- p1 -- p2 -- cycle)
			(make-o-paths font))
		  #\o #\c))))

(defun make-glyph-lower-case-d (font)
  (with-accessors ((ascent ascent)
		   (width width)
		   (stroke-width stroke-width))
    font
    (make-glyph (cons (make-vertical-stroke font (- width stroke-width) ascent)
		      (make-o-paths font))
		#\o #\l)))
	
;;;                     
;;;             *******
;;;          ************* 
;;;         ****       ****
;;;        **** 4     5 ****
;;;        ****/       \****
;;;        *****************
;;;        *****************
;;;        *****************- 2       -
;;;        ****\   |   /               |
;;;        **** 3  0  6 *\ - 1         |
;;;         ****       ****   \        | m
;;;          *************     | h     |
;;;             *******       /       _|
;;;                
(defun make-glyph-lower-case-e (font)
  (with-accessors ((width width)
		   (lower-case-ascent lower-case-ascent)
		   (stroke-width stroke-width)
		   (stroke-height stroke-height))
    font
    (let* ((h (* stroke-height 1.7))
	   (m (ceiling (- lower-case-ascent stroke-height) 2))
	   (p0 (complex (/ width 2) m))
	   (p1 (complex width h))
	   (p2 (complex width m))
	   (p3 (complex stroke-width m))
	   (p4 (complex stroke-width (+ m stroke-height)))
	   (p5 (complex (- width stroke-width) (+ m stroke-height)))
	   (p6 (complex (- width stroke-width) m)))
      (make-glyph (list* (mf p0 -- p1 -- p2 -- cycle)
			 (mf p4 -- p5 -- p6 -- p3 -- cycle)
			 (make-o-paths font))
		  #\o #\o))))

;;;             w2
;;;          ________
;;;         |        |
;;;         
;;;            w3
;;;          _____
;;;         |     |
;;;               6    7
;;;               |   /
;;;               ****
;;;            *******\
;;;          **** \    8
;;;      5 -****10 9     -
;;;         ****          |
;;;         ****          |
;;;         ****          |
;;;         ****          |
;;;       4 **** 11       |
;;;        \****/         |
;;;    3 -********- 12    |
;;;    2 -********- 13    |
;;;        /****\14       | h
;;;       1 ****          |
;;;         ****|_|       |
;;;         **** w1       |
;;;         ****          |
;;;         ****          |
;;;         ****          |
;;;         ****          |
;;;         ****          |
;;;         ****          |
;;;         ****          |
;;;         ****          |
;;;         ****         _|
;;;        /    \ 
;;;       0     15
;;;         

(defun make-glyph-lower-case-f (font)
  (with-accessors ((ascent ascent)
		   (stroke-width stroke-width)
		   (stroke-height stroke-height)
		   (j-width j-width)
		   (j-hook-start j-hook-start)
		   (lower-case-ascent lower-case-ascent))
    font
    (flet ((c (x y) (complex x y)))
      (let* ((w1 (max 1 (round (* ascent 0.1))))
	     (w2 j-width)
	     (w3 (* ascent 0.2))
	     (h (- ascent j-hook-start))
	     (p0 (c w1 0))
	     (p1 (+ p0 (c 0 (- lower-case-ascent stroke-height))))
	     (p2 (- p1 (c w1 0)))
	     (p3 (+ p2 (c 0 stroke-height)))
	     (p4 (+ p1 (c 0 stroke-height)))
	     (p5 (+ p0 (c 0 h)))
	     (p6 (c (+ w1 w3) ascent))
	     (p7 (c (+ w1 w2) ascent))
	     (p8 (- p7 (c 0 stroke-height)))
	     (p9 (- p6 (c 0 stroke-height)))
	     (p10 (+ p5 stroke-width))
	     (p11 (+ p4 stroke-width))
	     (p12 (+ p3 stroke-width (* 2 w1)))
	     (p13 (+ p2 stroke-width (* 2 w1)))
	     (p14 (+ p1 stroke-width))
	     (p15 (+ p0 stroke-width)))
	(make-glyph
	 (list (mf p0 -- p1 -- p2 -- p3 -- p4 -- p5 up ++
		   right p6 -- p7 -- p8 -- p9 left ++
		   down p10 -- p11 -- p12 -- p13 -- p14 -- p15 -- cycle))
	 #\f #\f)))))

;;;           
;;;           
;;;                     0    1
;;;                     |   /
;;;             ******* ****
;;;          ***************
;;;         ****       *****
;;;        ****         ****
;;;        ****         ****
;;;        ****         ****
;;;        ****         ****
;;;        ****         ****
;;;        ****         ****
;;;        ****         ****
;;;         ****       *****
;;;          ***************
;;;      ------ *******-****----  -     -
;;;                     ****       | h1  |
;;;               6  7 -****- 2   _|     |
;;;       5 \     |    ***               | h2
;;;          ************                |
;;;       4 -**********                 _|
;;;               | 
;;;               3
;;;
;;;        |_|
;;;         w2
;;;
;;;        |______|   
;;;           w1

(defun make-glyph-lower-case-g (font)
  (with-accessors ((ascent ascent)
		   (width width)
		   (lower-case-ascent lower-case-ascent)
		   (stroke-width stroke-width)
		   (stroke-height stroke-height)
		   (descent descent))
    font
    (flet ((c (x y) (complex x y)))
      (let* ((h1 (* ascent 0.07))
	     (h2 descent)
	     (w1 (* width 0.5))
	     (w2 (round (* 0.2 width)))
	     (p0 (c (- width stroke-width) lower-case-ascent))
	     (p1 (+ p0 stroke-width))
	     (p2 (c width (- h1)))
	     (p3 (c w1 (- h2)))
	     (p4 (c w2 (- h2)))
	     (p5 (+ p4 (c 0 stroke-height)))
	     (p6 (+ p3 (c 0 stroke-height)))
	     (p7 (- p2 stroke-width)))
	(make-glyph
	 (cons (mf p0 -- p1 -- p2 down ++ left p3 -- p4 --
		   p5 -- p6 right ++ up p7 -- cycle)
	       (make-o-paths font))
	 #\o #\q)))))

(defun make-glyph-lower-case-h (font)
  (with-accessors ((ascent ascent)
		   (width width)
		   (stroke-width stroke-width))
    font
    (let ((w (round (* 0.9 width))))
      (make-glyph (list (make-vertical-stroke font #c(0 0) ascent)
			(make-h-m-n-hook font (- w stroke-width) w))
		  #\l #\i))))

(defun make-glyph-lower-case-i (font)
  (with-accessors ((lower-case-ascent lower-case-ascent)
		   (stroke-width stroke-width)
		   (stroke-height stroke-height))
    font
    (make-glyph (list (make-vertical-stroke font #c(0 0) lower-case-ascent)
		      (make-dot font
				(complex (/ stroke-height 2)
					 (+ lower-case-ascent (* 3/2 stroke-height)))))
		#\i #\i)))

;;;           
;;;           **
;;;          ****
;;;        1  **  2
;;;         \    /
;;;          ****
;;;          ****
;;;          ****
;;;          ****
;;;          ****
;;;     w1   ****
;;;    _____ ****
;;;   |     |****
;;;          ****
;;;          ****
;;;        0-****     -
;;;          ****      |
;;;          ****      |
;;;          ****      | h
;;;  6  7  8-****-3   _|
;;;   \  \  **** 
;;;    *******
;;;    ****
;;;   /   |   
;;;  5    4    
;;;    |__|
;;;     w2
;;; 
(defun make-glyph-lower-case-j (font)
  (with-accessors ((ascent ascent)
		   (descent descent)
		   (lower-case-ascent lower-case-ascent)
		   (stroke-width stroke-width)
		   (stroke-height stroke-height)
		   (j-width j-width)
		   (j-hook-extreme j-hook-extreme)
		   (j-hook-start j-hook-start)
		   (width width))
    font
    (flet ((c (x y) (complex x y)))
      (let* ((h (- descent j-hook-start))
	     ;;(h (* 0.1 descent))
	     ;; FIXME, make these the same as for the `f'
	     (w1 (- j-width stroke-width))
	     (w2 (- j-width j-hook-extreme))
	     ;; (w2 (* 1.0 stroke-width))
	     (p0 (c w1 0))
	     (p1 (+ p0 (c 0 lower-case-ascent)))
	     (p2 (+ p1 stroke-width))
	     (p3 (- p2 (c 0 (+ lower-case-ascent h))))
	     (p4 (c w2 (- descent)))
	     (p5 (c 0 (- descent)))
	     (p6 (+ p5 (c 0 stroke-height)))
	     (p7 (+ p4 (c 0 stroke-height)))
	     (p8 (- p3 stroke-width)))
	(make-glyph (list (make-dot font
				    (c (+ w1 (/ stroke-width 2))
				       (+ lower-case-ascent (* 3/2 stroke-height))))
			  (mf p0 -- p1 -- p2 -- p3 down ++
			      left p4 -- p5 -- p6 -- p7 right ++
			      up p8 -- cycle))
		    #\j #\q)))))
;;;
;;;
;;;          1      2
;;;           \    /
;;;            ****
;;;            ****
;;;            ****
;;;            ****
;;;            ****
;;;            ****
;;;            ****
;;;            ****
;;;            ****      4      5
;;;            ****       \    /
;;;            ****        ****
;;;            ****      ****
;;;            ****    ****   
;;;            ****3 ****
;;;            **********- 6        -
;;;            ******9****           |
;;;      -     ****10  ****          |
;;;     |      ****     ****         |  h2
;;;  h1 |      ****      ****        |
;;;     |      ****       ****       |
;;;     |_     ****        ****     _|
;;;           /    \      /    \
;;;          0      11   8      7
;;;
;;;            |_______________|
;;;                   w

(defun make-glyph-lower-case-k (font)
  (with-accessors ((ascent ascent)
		   (lower-case-ascent lower-case-ascent)
		   (stroke-width stroke-width)
		   (stroke-height stroke-height)
		   (width width))
    font
    (flet ((c (x y) (complex x y)))
      (let* ((h1 (* 0.4 lower-case-ascent))
	     (h2 (* 0.6 lower-case-ascent))
	     (w (round (* (if (< width 5) 1.1 0.9) width)))
	     (p0 (c 0 0))
	     (p1 (c 0 ascent))
	     (p2 (+ p1 stroke-width))
	     (p10 (c stroke-width h1))
	     ;; average stroke for angled lines
	     (stroke (* 0.5 (+ stroke-width stroke-height)))
	     ;; assume angles are around 45 degrees
	     (p3 (+ p10 (c 0 (* stroke 1.2))))
	     (p5 (c w lower-case-ascent))
	     (p4 (- p5 (* stroke 1.4)))
	     (dx10-5 (- w stroke-width))
	     (dy10-5 (- lower-case-ascent h1))
	     (dy10-6 (- h2 h1))
	     (dx10-6 (* dy10-6 (/ dx10-5 dy10-5)))
	     (p6 (c (+ stroke-width dx10-6) h2))
	     (p7 (c w 0))
	     (p8 (- p7 (* 1.4 stroke)))
	     (dy10-9 (- dy10-6 (* 0.7 stroke)))
	     (dx10-9 (* dy10-9 (/ dx10-5 dy10-5)))
	     (p9 (+ p10 (c dx10-9 dy10-9)))
	     (p11 (c stroke-width 0)))
	(make-glyph
	 (list (mf p0 -- p1 -- p2 -- p3 -- p4 -- p5 -- p6 --
		   p7 -- p8 -- p9 -- p10 -- p11 -- cycle))
	 #\l #\x)))))

(defun make-glyph-lower-case-l (font)
  (make-glyph (list (make-vertical-stroke font 0 (ascent font)))
	      #\l #\l))

(defun make-glyph-lower-case-m (font)
  (with-accessors ((lower-case-ascent lower-case-ascent)
		   (stroke-width stroke-width)
		   (width width))
    font
    (let ((d (round (* 0.5 width)))
	  (sw stroke-width))
      (make-glyph
       (list (make-vertical-stroke font #c(0 0) lower-case-ascent)
	     (make-h-m-n-hook font (+ sw d) (+ sw sw d))
	     (make-h-m-n-hook font (+ sw d sw d) (+ sw sw d)))
       #\i #\i))))

(defun make-glyph-lower-case-n (font)
  (with-accessors ((lower-case-ascent lower-case-ascent)
		   (width width)
		   (stroke-width stroke-width))
    font
    (let ((w (round (* 0.9 width))))
      (make-glyph (list (make-vertical-stroke font #c(0 0) lower-case-ascent)
			(make-h-m-n-hook font (- w stroke-width) w))
		  #\i #\i))))

(defun make-glyph-lower-case-o (font)
  (make-glyph (make-o-paths font) #\o #\o))

(defun make-glyph-lower-case-p (font)
  (with-accessors ((lower-case-ascent lower-case-ascent)
		   (descent descent))
    font
    (make-glyph (cons (make-vertical-stroke font
					    (complex 0 (- descent))
					    (+ lower-case-ascent descent))
		      (make-o-paths font))
		#\p #\o)))
	
(defun make-glyph-lower-case-q (font)
  (with-accessors ((lower-case-ascent lower-case-ascent)
		   (width width)
		   (stroke-width stroke-width)
		   (descent descent))
    font
    (make-glyph (cons (make-vertical-stroke font
					    (complex (- width stroke-width)
						     (- descent))
					    (+ lower-case-ascent descent))
		      (make-o-paths font))
		#\o #\q)))
	
;;;             
;;;          
;;;                           1
;;;           ----           /     
;;;          |    |     *****
;;;          |    |**********
;;;          |  *****        \
;;;          | ***|           2
;;;       0 -|*** |
;;;          | /  |
;;;          |3   |
;;;          |    |
;;;          |    |
;;;          |    |
;;;          |    |
;;;          |    |
;;;          |    |
;;;          |    |
;;;          |    |
;;;          |    |
;;;          |    |
;;;           ----
;;;          
;;;             

(defun make-glyph-lower-case-r (font)
  (with-accessors ((lower-case-ascent lower-case-ascent)
		   (stroke-width stroke-width)
		   (stroke-height stroke-height)
		   (width width))
    font
    (flet ((c (x y) (complex x y)))
      (let* ((bend-start (- lower-case-ascent (* stroke-height 3.0)))
	     (w (* 0.5 width))
	     (p0 (c 0 bend-start))
	     (p1 (c w lower-case-ascent))
	     (p2 (- p1 (c 0 stroke-height)))
	     (p3 (+ p0 (* 0.5 stroke-width))))
	(make-glyph
	 (list (make-vertical-stroke font (c 0 0) lower-case-ascent)
	       (mf p0 up ++ right p1 -- p2 left ++ down p3 -- cycle))
	 #\i #\r)))))
	     
;;;           
;;;           
;;;           
;;;                      6
;;;                      |
;;;                   *******
;;;                ************
;;;              *****************- 7
;;;            ****      |   ****    
;;;           ****       9     *- 8
;;;        5 -****- 10 
;;;           ****       11
;;;            ******    |
;;;              ***************
;;;               ****************
;;;                      |    ******
;;;                      4      ****
;;;                          3 -****- 12 -
;;;           1 -*       2      ****      |
;;;             ****     |     ****       |
;;;     -   0 -******************         |  h2
;;; h1 |          **************          |
;;;    |_             *******            _|
;;;                      |
;;;                      13
;;;           
;;;           
;;;           
;;;           

(defun make-glyph-lower-case-s (font)
  (with-accessors ((lower-case-ascent lower-case-ascent)
		   (stroke-width stroke-width)
		   (stroke-height stroke-height)
		   (width width))
    font
    (flet ((c (x y) (complex x y)))
      (let* ((upper-shrink (round (* width 0.1)))
	     (h1 (* lower-case-ascent 0.15))
	     (h2 (* lower-case-ascent 0.3))
	     (delta (if (or (and (oddp lower-case-ascent)
				 (evenp stroke-height))
			    (and (evenp lower-case-ascent)
				 (oddp stroke-height)))
			1/2
			0))
	     (p0 (c 0 h1))
	     ;; assume angle about 45 degrees
	     (p1 (+ p0 (* stroke-width (c 0.7 0.7))))
	     (p13 (c (* 0.5 width) 0))
	     (p2 (+ p13 (c 0 stroke-height)))
	     (p12 (c width h2))
	     (p3 (- p12 stroke-width))
	     (p4 (c (* 0.5 width) (+ (* 1/2 (- lower-case-ascent stroke-height)) delta)))
	     (p11 (+ p4 (c 0 stroke-height)))
	     (p5 (c upper-shrink (- lower-case-ascent h2)))
	     (p10 (+ p5 stroke-width))
	     (p6 (c (* 0.5 width) lower-case-ascent))
	     (p9 (- p6 (c 0 stroke-height)))
	     (p7 (c (- width upper-shrink) (- lower-case-ascent h1)))
	     (p8 (+ p7 (- p0 p1))))
	(make-glyph
	 (list (mf p0 -- p1 ++ right p2 ++ up p3 ++ left p4 ++
		   up p5 ++ right p6 ++ p7 -- p8 ++ left p9 ++
		   down p10 ++ right p11 ++ down p12 ++ left p13 ++ cycle))
	 #\o #\o)))))

;;;          5       6
;;;           \     /
;;;             ****
;;;             ****
;;;             ****
;;;             ****
;;;             ****
;;;             ****
;;;             ****
;;;             ****
;;;           4 **** 7  
;;;            \****/  
;;;       3 -********** - 8
;;;       2 -********** - 9
;;;            /****\
;;;           1 **** 10
;;;             ****
;;;             ****
;;;             ****
;;;             ****
;;;             ****
;;;             ****
;;;          0 -****- 11
;;;             ****  12   13
;;;              **** |   /
;;;                *******
;;;                   ****
;;;                   |   \
;;;                  15    14

(defun make-glyph-lower-case-t (font)
  (with-accessors ((ascent ascent)
		   (descent descent)
		   (lower-case-ascent lower-case-ascent)
		   (stroke-width stroke-width)
		   (stroke-height stroke-height)
		   (j-width j-width)
		   (j-hook-extreme j-hook-extreme)
		   (j-hook-start j-hook-start)
		   (width width))
    font
    (flet ((c (x y) (complex x y)))
      (let* ((w1 (max 1 (round (* ascent 0.1))))
	     (p0 (c w1 j-hook-start))
	     (p1 (c w1 (- lower-case-ascent stroke-height)))
	     (p2 (- p1 w1))
	     (p3 (+ p2 (c 0 stroke-height)))
	     (p4 (+ p3 w1))
	     (p5 (c w1 (round (* 0.8 ascent))))
	     (p6 (+ p5 stroke-width))
	     (p7 (+ p4 stroke-width))
	     (p8 (+ p7 w1))
	     (p9 (- p8 (c 0 stroke-height)))
	     (p10 (+ p1 stroke-width))
	     (p11 (+ p0 stroke-width))
	     (p12 (c (+ w1 j-hook-extreme) stroke-height))
	     (p13 (c (+ w1 j-width) stroke-height))
	     (p14 (+ w1 j-width))
	     (p15 (+ w1 j-hook-extreme)))
	(make-glyph
	 (list (mf p0 -- p1 -- p2 -- p3 -- p4 -- p5 -- p6 -- p7 -- p8 --
		   p9 -- p10 -- p11 down ++ right p12 -- p13 --
		   p14 -- p15 left ++ up cycle))
	 #\l #\t)))))

(defun make-glyph-lower-case-u (font)
  (with-accessors ((lower-case-ascent lower-case-ascent)
		   (width width)
		   (stroke-width stroke-width))
    font
    (let ((w (round (* 0.9 width))))
      (make-glyph (list (make-vertical-stroke font (- width stroke-width) lower-case-ascent)
			(paths:path-rotate 
			 (make-h-m-n-hook font (- w stroke-width) w)
			 pi
			 (paths:make-point (* 0.5 width) (* 0.5 lower-case-ascent))))
		  #\u #\i))))

;;;                 w1
;;;            _____________
;;;           |             |
;;;           
;;;            w3
;;;            __
;;;           |  |
;;;
;;;         1 **** 2   4 **** 5
;;;           ****       ****
;;;            ****     **** 
;;;            ****     ****
;;;             ****   ****
;;;             ****   ****
;;;              **** ****
;;;              ****3****
;;;               *******
;;;               *******
;;;                *****
;;;              0 ***** 6
;;;           
;;;                |___|
;;;                  w2

(defun make-glyph-lower-case-v (font)
  (with-accessors ((lower-case-ascent lower-case-ascent)
		   (width width)
		   (stroke-width stroke-width))
    font
    (flet ((c (x y) (complex x y)))
      (let* ((w1 (max 4 (round (* 0.9 width))))
	     (w2 (* 1.1 stroke-width))
	     (w3 (* 1.0 stroke-width))
	     (p0 (* 0.5 (- w1 w2)))
	     (p1 (c 0 lower-case-ascent))
	     (p2 (+ p1 w3))
	     (p3 (c (* 0.5 w1) (* 1.1 stroke-width))) ; guess and adjust y coordinate
	     (p4 (+ p1 (- w1 w3)))
	     (p5 (+ p1 w1))
	     (p6 (+ p0 w2)))
	(make-glyph (list (mf p0 -- p1 -- p2 -- p3 -- p4 -- p5 -- p6 -- cycle))
		    #\v #\v)))))

;;;                      w1
;;;            ______________________
;;;           |                      |
;;;           
;;;            w3
;;;            __
;;;           |  |
;;;
;;;          b****c             h****i
;;;           ****               ****
;;;            ****             ****
;;;            ****    e  f     ****
;;;             ****   ****    ****
;;;             ****   ****    ****
;;;              **** ******  ****
;;;              ****d***l***g****
;;;               ******* *******
;;;               ******* *******
;;;                *****   *****
;;;              a *****m k*****j
;;;           
;;;          |_____|___|
;;;            w4    w2

(defun make-glyph-lower-case-w (font)
  (with-accessors ((lower-case-ascent lower-case-ascent)
		   (width width)
		   (stroke-height stroke-height)
		   (stroke-width stroke-width))
    font
    (flet ((c (x y) (complex x y)))
      (let* ((w1 (max 5 (round (* 1.2 width))))
	     (w2 (* 1.0 stroke-width))
	     (w3 (* 1.0 stroke-width))
	     (w4 (* 0.2 w1))
	     (pa (c w4 0))
	     (pb (c 0 lower-case-ascent))
	     (pc (+ pb w3))
	     (pd (c (+ w4 (* 0.5 w2)) (* 1.0 stroke-height)))
	     (pe (c (* 0.5 (- w1 w2)) (* 0.7 lower-case-ascent)))
	     (pf (+ pe w2))
	     (pg (c (- w1 (+ w4 (* 0.5 w2))) (* 1.0 stroke-height)))
	     (ph (c (- w1 w3) lower-case-ascent))
	     (ppi (c w1 lower-case-ascent))
	     (pj (c (- w1 w4) 0))
	     (pk (- pj w2))
	     (pl (c (* 0.5 w1) (- (imagpart pe) (* 1.0 stroke-height))))
	     (pm (+ pa w2)))
	(make-glyph (list (mf pa -- pb -- pc -- pd -- pe -- pf -- pg --
			      ph -- ppi -- pj -- pk -- pl -- pm -- cycle))
		    #\v #\v)))))

;;;               
;;;                  w1
;;;            ______________   
;;;           |              |
;;;               
;;;               
;;;          c****d      f****g
;;;            ****      ****
;;;             ****    ****
;;;              **** e****   _
;;;               ********     |
;;;                ******      | h1
;;;               b******h     | 
;;;               ********    _|
;;;              **** k****
;;;             ****    ****
;;;            ****      ****
;;;          a****l      j****i
;;;
;;;           |____|      |___|
;;;             w2          w3
;;;               
;;;               

(defun make-glyph-lower-case-x (font)
  (with-accessors ((lower-case-ascent lower-case-ascent)
		   (width width)
		   (stroke-height stroke-height)
		   (stroke-width stroke-width))
    font
    (flet ((c (x y) (complex x y)))
      (let* ((w1 (max 4 (round (* 0.9 width))))
	     (w2 (* 0.5 (- w1 (* 0.8 stroke-width))))
	     (w3 (* 1.3 stroke-width))
	     (h1 (* 1.0 stroke-width))
	     (pa (c 0 0))
	     (pb (c w2 (* 0.5 lower-case-ascent)))
	     (pc (c 0 lower-case-ascent))
	     (pd (+ pc w3))
	     (pe (c (* 0.5 w1) (+ (imagpart pb) (* 0.5 h1))))
	     (pf (c (- w1 w3) lower-case-ascent))
	     (pg (c w1 lower-case-ascent))
	     (ph (+ pb (- w1 (* 2 w2))))
	     (ppi (c w1 0))
	     (pj (c (- w1 w3) 0))
	     (pk (- pe (c 0 h1)))
	     (pl (c w3 0)))
	(make-glyph
	 (list (mf pa -- pb -- pc -- pd -- pe -- pf --
		   pg -- ph -- ppi -- pj -- pk -- pl -- cycle))
	 #\x #\x)))))

;;;             
;;;                  w1
;;;           __________________   
;;;          |                  |
;;;             
;;;           w2
;;;           __
;;;          |  |
;;;
;;;         e****f          h****i
;;;           ****          ****
;;;            ****        ****
;;;             ****      ****
;;;              ****    ****
;;;               ****g ****
;;;                ********
;;;                 ******
;;;                 d**** 
;;;                 **** 
;;;                ****
;;;             c ****j
;;;           b******
;;;           a****
;;;             k
;;;          |_|   
;;;           w3  
;;;             
;;;          |______|   
;;;             w4

(defun make-glyph-lower-case-y (font)
  (with-accessors ((lower-case-ascent lower-case-ascent)
		   (width width)
		   (stroke-height stroke-height)
		   (stroke-width stroke-width)
		   (descent descent))
    font
    (flet ((c (x y) (complex x y)))
      (let* ((w1 (max 4 (round (* 0.9 width))))
	     (w2 (* 1.0 stroke-width))
	     (w3 (* 0.1 w1))
	     (w4 (* 0.5 (- w1 (* 0.9 stroke-width))))
	     (pa (c w3 (- descent)))
	     (pb (+ pa (c 0 stroke-height)))
	     (pc (+ pb (* 0.03 w4)))
	     (pd (c w4 0))
	     (pe (c 0 lower-case-ascent))
	     (pf (+ pe w2))
	     (pg (c (* 0.5 w1) (* 0.9 stroke-height)))
	     (ph (c (- w1 w2) lower-case-ascent))
	     (ppi (c w1 lower-case-ascent))
	     (pj (+ ppi (* 1.2 (- pd ph))))
	     (pk (- pc (c 0 stroke-height))))
	(make-glyph
	 (list (mf pa -- pb -- pc right ++ pd -- pe -- pf -- pg -- ph --
		   ppi -- pj (direction (- pd ph)) ++ left pk -- cycle))
	 #\v #\v)))))


;;;
;;;             w1
;;;       _______________
;;;      |               |
;;;      
;;;     e****************f
;;;     d**********c*****g
;;;                 ****   
;;;                ****   
;;;               ****   
;;;              ****   
;;;             ****   
;;;            ****   
;;;           ****   
;;;          ****   
;;;         ****   
;;;        ****h   
;;;      b***************i
;;;      a***************j
;;;      
;;;       |____|
;;;         w2
;;;      
;;;      

(defun make-glyph-lower-case-z (font)
  (with-accessors ((lower-case-ascent lower-case-ascent)
		   (width width)
		   (stroke-height stroke-height)
		   (stroke-width stroke-width))
    font
    (flet ((c (x y) (complex x y)))
      (let* ((w1 (max 4 (round (* 0.9 width))))
	     (w2 (* 1.1 stroke-width))
	     (pa (c 0 0))
	     (pb (c 0 stroke-height))
	     (pc (c (- w1 w2) (- lower-case-ascent stroke-height)))
	     (pd (c 0 (- lower-case-ascent stroke-height)))
	     (pe (c 0 lower-case-ascent))
	     (pf (c w1 lower-case-ascent))
	     (pg (c w1 (- lower-case-ascent stroke-height)))
	     (ph (c w2 stroke-height))
	     (ppi (c w1 stroke-height))
	     (pj (c w1 0)))
	(make-glyph
	 (list (mf pa -- pb -- pc -- pd -- pe -- pf -- pg --
		   ph -- ppi -- pj -- cycle))
	 #\z #\z)))))
	       
;;;                        w2
;;;                        ___
;;;                       |   |
;;;                       be cf 
;;;                       *****
;;;                      *******
;;;                      *******
;;;                     *********
;;;                     **** ****
;;;                    ****   ****
;;;                    ****   ****
;;;                   ****     ****    
;;;                   ****     ****    
;;;                  ****       ****
;;;                  ****       ****
;;;                 ****         ****      
;;;                 ****         ****      
;;;                ****           ****      
;;;                ****           ****           _
;;;               **j***************k**           |
;;;               *i*****************l*           |
;;;              ****               ****          |
;;;              ****               ****          |
;;;             ****                 ****         | h1
;;;             ****                 ****         |
;;;            ****                   ****        |
;;;            ****                   ****        |
;;;           ****                     ****       |
;;;           ****                     ****      -
;;;          a    d                   h    g
;;;           |____________________________|
;;;                       w1
;;;                       
;;;                       
;;;           |__|
;;;            w3

(defun make-glyph-upper-case-a (font)
  (with-accessors ((ascent ascent)
		   (lower-case-ascent lower-case-ascent)
		   (upper-case-h-width upper-case-h-width)
		   (j-hook-start j-hook-start)
		   (stroke-height stroke-height)
		   (stroke-width stroke-width))
    font
    (flet ((c (x y) (complex x y)))
      (let* ((w2 (round (* 1.2 stroke-width)))
	     (w1 (let ((w (round (* 1.2 upper-case-h-width))))
		   (if (= (mod w2 2) (mod w 2)) w (1- w))))
	     (w3 (* 1.05 stroke-width))
	     (h1 (round (* 0.4 ascent)))
	     (xb (* 1/2 (- w1 w2)))
	     (xj (* (/ xb ascent) h1))
	     (xi (* (/ xb ascent) (- h1 stroke-height)))
	     (pa (c 0 0))
	     (pb (c xb ascent))
	     (pc (+ pb w3))
	     (pd (+ pa w3))
	     (pf (c (* 1/2 (+ w1 w2)) ascent))
	     (pe (- pf w3))
	     (pg (c w1 0))
	     (ph (- pg w3))
	     (ppi (c xi (- h1 stroke-height)))
	     (pj (c xj h1))
	     (pk (c (- w1 xj) h1))
	     (pl (c (- w1 xi) (- h1 stroke-height))))
	(make-glyph
	 (list (mf pa -- pb -- pc -- pd -- cycle)
	       (mf pe -- pf -- pg -- ph -- cycle)
	       (mf ppi -- pj -- pk -- pl -- cycle))
	 #\l #\l)))))


;;;        
;;;        
;;;       g              h                  _
;;;        ***************                   |
;;;        ******************                |
;;;       f              e****               |
;;;                        ****              |
;;;                         ****             |
;;;                        d****j            | h
;;;                         ****             |
;;;                        ****              |
;;;       b              c****               |
;;;        ******************                |
;;;        ***************                  -
;;;       a              k
;;;        |_____________|
;;;              w1
;;;        |____________________|
;;;                 w2
;;;        
;;;        

(defun make-loop (font pa w1 w2 h)
  (with-accessors ((stroke-height stroke-height)
		   (stroke-width stroke-width))
    font
    (flet ((c (x y) (complex x y)))
      (let* ((pb (+ pa (c 0 stroke-height)))
	     (pc (+ pb w1))
	     (pd (+ pa (c (- w2 stroke-width) (/ h 2))))
	     (pf (+ pa (c 0 (- h stroke-height))))
	     (pe (+ pf w1))
	     (pg (+ pa (c 0 h)))
	     (ph (+ pg w1))
	     (pj (+ pd stroke-width))
	     (pk (+ pa w1)))
	(mf pa -- pb -- pc right ++ up pd ++ left pe -- pf --
	    pg -- ph right ++ down pj ++ left pk -- cycle)))))




;;;              w2
;;;         ______________
;;;        |              |
;;;            
;;;        **********
;;;        ************
;;;        ****      ****
;;;        ****       ****
;;;        ****        ****
;;;        ****        ****
;;;        ****       ****
;;;        ****      ****         _
;;;        *************           |
;;;        ***************         |
;;;        ****        ****        |
;;;        ****         ****       |
;;;        ****          ****      |
;;;        ****          ****      |  h
;;;        ****          ****      |
;;;        ****         ****       |
;;;        ****        ****        |
;;;        ***************         |
;;;        *************          -
;;;            
;;;        
;;;        |________________|
;;;               w1
;;;        


(defun make-glyph-upper-case-b (font)
  (with-accessors ((ascent ascent)
		   (upper-case-h-width upper-case-h-width)
		   (upper-case-h-bar-position upper-case-h-bar-position)
		   (stroke-height stroke-height)
		   (stroke-width stroke-width))
    font
    (flet ((c (x y) (complex x y)))
      (let* ((w1 (round (* 0.88 upper-case-h-width)))
	     (w2 (round (* 0.8 upper-case-h-width)))
	     (h upper-case-h-bar-position))
	(make-glyph
	 (list (make-vertical-stroke font 0 ascent)
	       (make-loop font
			  (c stroke-width 0)
			  (* 0.5 w1)
			  w1
			  h)
	       (make-loop font
			  (c stroke-width (- h stroke-height))
			  (* 0.5 w2)
			  w2
			  (+ (- ascent h) stroke-height)))
	 #\l #\B)))))

;;;         
;;;                   f
;;;                *******       
;;;              ************     
;;;            ****   e    ****    
;;;           ****           ***g   
;;;          ****             ***  
;;;        ****                **  
;;;        ****                 *
;;;       ****                  h
;;;       **** 
;;;       **** 
;;;       **** 
;;;       ****                  c     _
;;;        ****                 *      |
;;;        ****                **      | h2
;;;          ****             ***     -
;;;           ****           ***d      |
;;;            ****   b    ****        | h1
;;;              ************          |
;;;                *******            - 
;;;                   a
;;;                   |__________|
;;;                        w1
;;;                       

(defun make-glyph-upper-case-c (font)
  (with-accessors ((ascent ascent)
		   (upper-case-o-width upper-case-o-width)
		   (stroke-height stroke-height)
		   (stroke-width stroke-width))
    font
    (flet ((c (x y) (complex x y)))
      (let* ((w1 (if (oddp upper-case-o-width)
		     (+ 1/2 (floor (* 1/3 upper-case-o-width)))
		     (round (* 1/3 upper-case-o-width))))
	     (h1 (* 0.12 ascent))
	     (h2 (* 1.1 stroke-height))
	     (pa (c (* 1/2 upper-case-o-width) 0))
	     (pb (+ pa (c 0 stroke-height)))
	     (pc (c (+ (* 1/2 upper-case-o-width) w1) (+ h1 h2)))
	     (pd (- pc (c 0 h2)))
	     (pf (+ pa (c 0 ascent)))
	     (pe (- pf (c 0 stroke-height)))
	     (ph (c (realpart pc) (- ascent (imagpart pc))))
	     (pg (c (realpart pd) (- ascent (imagpart pd))))
	     (center (paths:make-point (* 1/2 upper-case-o-width)
				       (* 1/2 ascent))))
	(make-glyph
	 (list (paths:path-rotate
		(make-loop font pa 0 (* 1/2 upper-case-o-width) ascent)
		pi
		center)
	       (mf pa -- pb right ++ pc -- pd ++ left cycle)
	       (mf pe -- pf right ++ pg -- ph ++ left cycle))
	       
	 #\l #\C)))))

;;;         
;;;         
;;;                   
;;;         *************
;;;         ***************
;;;         ****       *****
;;;         ****         ****
;;;         ****          ****
;;;         ****          ****
;;;         ****          ****
;;;         ****          ****
;;;         ****          ****
;;;         ****          ****
;;;         ****          ****
;;;         ****          ****
;;;         ****          ****
;;;         ****          **** 
;;;         ****          ****
;;;         ****         ****  
;;;         ****       *****   
;;;         ***************    
;;;         *************      
;;;         |________________|          
;;;                w1
;;;         

(defun make-glyph-upper-case-d (font)
  (with-accessors ((ascent ascent)
		   (stroke-height stroke-height))
    font
    (flet ((c (x y) (complex x y)))
      (let* ((w1 (round (* 0.71 ascent))))
	(make-glyph
	 (list (make-vertical-stroke font 0 ascent)
	       (make-loop font (c stroke-height 0) (* 0.3 w1) w1 ascent))
	 #\l #\l)))))

;;;                   w1
;;;          ____________________
;;;         |                    |
;;;         
;;;         **********************
;;;         **********************
;;;         ****
;;;         ****
;;;         ****
;;;         ****
;;;         ****
;;;         ****
;;;         ****
;;;         ****
;;;         ***************
;;;         ***************
;;;         ****
;;;         ****|_________|
;;;         ****    w2
;;;         ****
;;;         ****
;;;         ****
;;;         ****
;;;         ****
;;;         *********************
;;;         *********************
;;;         

(defun make-glyph-upper-case-e (font)
  (with-accessors ((ascent ascent)
		   (upper-case-h-width upper-case-h-width)
		   (upper-case-h-bar-position upper-case-h-bar-position)
		   (stroke-height stroke-height)
		   (stroke-width stroke-width))
    font
    (flet ((c (x y) (complex x y)))
      (let* ((w1 (round (* 0.75 upper-case-h-width)))
	     (w2 (round (* 0.88 w1))))
	(make-glyph
	 (list (make-vertical-stroke font 0 ascent)
	       (make-horizontal-stroke font 0 w1)
	       (make-horizontal-stroke
		font (c 0 (- ascent stroke-height)) w1)
	       (make-horizontal-stroke
		font (c 0 (- upper-case-h-bar-position stroke-height)) w2))
	 #\l #\E)))))

;;;                   w1
;;;          ____________________
;;;         |                    |
;;;         
;;;         **********************
;;;         **********************
;;;         ****
;;;         ****
;;;         ****
;;;         ****
;;;         ****
;;;         ****
;;;         ****
;;;         ****
;;;         ***************
;;;         ***************
;;;         ****
;;;         ****|_________|
;;;         ****    w2
;;;         ****
;;;         ****
;;;         ****
;;;         ****
;;;         ****
;;;         ****
;;;         ****

(defun make-glyph-upper-case-f (font)
  (with-accessors ((ascent ascent)
		   (upper-case-h-width upper-case-h-width)
		   (upper-case-h-bar-position upper-case-h-bar-position)
		   (stroke-height stroke-height)
		   (stroke-width stroke-width))
    font
    (flet ((c (x y) (complex x y)))
      (let* ((w1 (round (* 0.75 upper-case-h-width)))
	     (w2 (round (* 0.88 w1))))
	(make-glyph
	 (list (make-vertical-stroke font 0 ascent)
	       (make-horizontal-stroke
		font (c 0 (- ascent stroke-height)) w1)
	       (make-horizontal-stroke
		font (c 0 (- upper-case-h-bar-position stroke-height)) w2))
	 #\l #\E)))))


;;;         
;;;           w2
;;;        ________ 
;;;       |        |
;;;         
;;;         
;;;                   f
;;;                *******       
;;;              ************     
;;;            ****   e    ****    
;;;           ****           ***g   
;;;          ****             ***  
;;;        ****                **  
;;;        ****                 *
;;;       ****                  h
;;;       ****     m            n            _
;;;       ****      *************             |
;;;       ****      *************             |
;;;       ****     l        k****     _       |
;;;        ****              ***c      |      |
;;;        ****              ****      | h2   | h3
;;;          ****            ****     -       |
;;;           ****          j***d      |      |
;;;            ****   b    ****        | h1   |
;;;              ************          |      |
;;;                *******            -      -
;;;                   a
;;;                   |__________|
;;;                        w1
;;;                       

(defun make-glyph-upper-case-g (font)
  (with-accessors ((ascent ascent)
		   (upper-case-o-width upper-case-o-width)
		   (upper-case-h-bar-position upper-case-h-bar-position)
		   (stroke-height stroke-height)
		   (stroke-width stroke-width))
    font
    (flet ((c (x y) (complex x y)))
      (let* ((w1 (if (oddp upper-case-o-width)
		     (+ 1/2 (floor (* 0.37 upper-case-o-width)))
		     (round (* 0.37 upper-case-o-width))))
	     (w2 (* 0.43 upper-case-o-width))
	     (h1 (* 0.06 ascent))
	     (h2 (* 1.1 stroke-height))
	     (h3 upper-case-h-bar-position)
	     (pa (c (* 1/2 upper-case-o-width) 0))
	     (pb (+ pa (c 0 stroke-height)))
	     (pc (c (+ (* 1/2 upper-case-o-width) w1) (+ h1 h2)))
	     (pd (- pc (c 0 h2)))
	     (pf (+ pa (c 0 ascent)))
	     (pe (- pf (c 0 stroke-height)))
	     (ph (c (realpart pc) (- ascent (imagpart pc))))
	     (pg (c (realpart pd) (- ascent (imagpart pd))))
	     (pj (- pd stroke-width))
	     (pk (c (- (+ (* 1/2 upper-case-o-width) w1) stroke-width)
		    (- h3 stroke-height)))
	     (pl (c w2 (- h3 stroke-height)))
	     (pm (c w2 h3))
	     (pn (c (+ (* 1/2 upper-case-o-width) w1) h3))
	     (center (paths:make-point (* 1/2 upper-case-o-width)
				       (* 1/2 ascent))))
	(make-glyph
	 (list (paths:path-rotate
		(make-loop font pa 0 (* 1/2 upper-case-o-width) ascent)
		pi
		center)
	       (mf pa -- pb right ++ pc -- pd ++ left cycle)
	       (mf pe -- pf right ++ pg -- ph ++ left cycle)
	       (mf pj -- pk -- pl -- pm -- pn -- pd -- cycle))
	 #\l #\C)))))

;;;         
;;;         
;;;         
;;;         ****              ****
;;;         ****              ****
;;;         ****              ****
;;;         ****              ****
;;;         ****              ****
;;;         ****              ****
;;;         ****              ****
;;;         ****              ****
;;;         ****              ****
;;;         ****              ****
;;;         **********************
;;;         **********************
;;;         ****a             ****
;;;         ****              ****
;;;         ****              ****
;;;         ****              ****
;;;         ****              ****
;;;         ****              ****
;;;         ****              ****
;;;         ****              ****
;;;         ****              ****
;;;         ****              ****
;;;         

(defun make-glyph-upper-case-h (font)
  (with-accessors ((ascent ascent)
		   (upper-case-h-width upper-case-h-width)
		   (upper-case-h-bar-position upper-case-h-bar-position)
		   (stroke-height stroke-height)
		   (stroke-width stroke-width))
    font
    (flet ((c (x y) (complex x y)))
      (let* ((w upper-case-h-width)
	     (pa (c stroke-width (- upper-case-h-bar-position stroke-height))))
	(make-glyph
	 (list (make-vertical-stroke font (c 0 0) ascent)
	       (make-vertical-stroke font (c (- w stroke-width) 0) ascent)
	       (make-horizontal-stroke font pa (- w (* 2 stroke-width))))
	 #\l #\l)))))

(defun make-glyph-upper-case-i (font)
  (with-accessors ((ascent ascent)
		   (stroke-width stroke-width)
		   (stroke-height stroke-height))
    font
    (flet ((c (x y) (complex x y)))
      (let* ((w (max 1 (round (* ascent 0.1))))
	     (h (- ascent (* 2 stroke-height))))
	(make-glyph
	 (list (make-horizontal-stroke font (c 0 0) (+ (* 2 w) stroke-width))
	       (make-horizontal-stroke font (c 0 (- ascent stroke-height))
				       (+ (* 2 w) stroke-width))
	       (make-vertical-stroke font (c w stroke-height) h))
	 #\I #\I)))))
	     
;;;
;;;
;;;            w2
;;;            __
;;;           |  |g           h
;;;               ************
;;;               ************
;;;              f       e**** 
;;;                       ****
;;;                       ****       
;;;                       ****
;;;                       ****
;;;                       ****
;;;                       ****
;;;                       ****
;;;                       ****
;;;                       ****
;;;                       ****
;;;                       ****
;;;                       ****
;;;                      d****i -
;;;                      ****    |
;;;            b      c*****     | h1
;;;             ***********      |
;;;             *********       _|
;;;            a      j
;;;           |______________|
;;;                 w1
;;;           

(defun make-glyph-upper-case-j (font)
  (with-accessors ((ascent ascent)
		   (upper-case-h-width upper-case-h-width)
		   (j-hook-start j-hook-start)
		   (stroke-width stroke-width)
		   (stroke-height stroke-height))
    font
    (flet ((c (x y) (complex x y)))
      (let* ((w1 (max 3 (round (* 0.60 upper-case-h-width))))
	     (w2 (max 1 (round (* 0.250 upper-case-h-width))))
	     (h1 j-hook-start)
	     (pa (c 0 0))
	     (pb (+ pa (c 0 stroke-height)))
	     (pc (c (* 0.3 w1) stroke-height))
	     (pd (c (- w1 stroke-width) h1))
	     (pe (c (- w1 stroke-width) (- ascent stroke-height)))
	     (pf (c w2 (- ascent stroke-height)))
	     (pg (c w2 ascent))
	     (ph (c w1 ascent))
	     (ppi (+ pd stroke-width))
	     (pj (- pc (c 0 stroke-height))))
	(make-glyph
	 (list (mf pa -- pb right ++ pc right ++ up pd -- pe -- pf --
		   pg -- ph -- ppi down ++ left pj -- cycle))
	 #\J #\l)))))

;;;                          w2
;;;                         ___
;;;                        |   |
;;;                       c    d
;;;         ****           ****
;;;         ****          ****
;;;         ****         ****
;;;         ****        ****
;;;         ****      ****
;;;         ****     ****
;;;         ****    ****
;;;         ****  ****
;;;         ****b**f*               _
;;;         *********                |
;;;         **********               |
;;;         *****  ****              |
;;;         ****a   ****             |
;;;         ****     ****            |
;;;         ****      ****           | h1
;;;         ****       ****          |
;;;         ****        ****         |
;;;         ****         ****        |
;;;         ****          ****       |
;;;         ****           ****     _|
;;;                       e    g
;;;         
;;;         |__________________|
;;;                 w1
;;;         

(defun make-glyph-upper-case-k (font)
  (with-accessors ((ascent ascent)
		   (upper-case-h-width upper-case-h-width)
		   (lower-case-ascent lower-case-ascent)
		   (stroke-width stroke-width)
		   (stroke-height stroke-height))
    font
    (flet ((c (x y) (complex x y)))
      (let* ((w1 (round (* 0.9  upper-case-h-width)))
	     (w2 (* 1.1 stroke-width))
	     (h1 (round (* 1.0 lower-case-ascent)))
	     (pb (c stroke-width h1))
	     (pa (- pb (c 0 (* 1.3 stroke-height))))
	     (pd (c w1 ascent))
	     (pc (- pd w2))
	     (pf (+ pb w2))
	     (pg (c w1 0))
	     (pe (- pg w2)))
	(make-glyph
	 (list (make-vertical-stroke font (c 0 0) ascent)
	       (mf pa -- pb -- pc -- pd -- cycle)
	       (mf pb -- pf -- pg -- pe -- cycle))
	 #\l #\K)))))

(defun make-glyph-upper-case-l (font)
  (with-accessors ((ascent ascent)
		   (upper-case-h-width upper-case-h-width)
		   (stroke-width stroke-width)
		   (stroke-height stroke-height))
    font
    (flet ((c (x y) (complex x y)))
      (let* ((w (round (* 0.71 upper-case-h-width))))
	(make-glyph
	 (list (make-vertical-stroke font (c 0 0) ascent)
	       (make-horizontal-stroke font (c 0 0) w))
	 #\l #\L)))))
	     

;;;
;;;                   w1
;;;          ____________________
;;;         |                    |
;;;        a    b            e    f
;;;         ****              ****
;;;         *****            *****
;;;         ******          ******
;;;         *******        *******
;;;         ********      ********
;;;         **** ****    **** ****
;;;         ****  ****  ****  ****
;;;         ****   ********   ****
;;;         ****    ******    ****
;;;         ****     ****     ****
;;;         ****    c    d    ****
;;;         ****     |__|     ****
;;;         ****      w2      ****
;;;         ****              ****
;;;         ****              ****
;;;         ****              ****
;;;         ****              ****
;;;         ****              ****
;;;         ****              ****
;;;         
;;;         

(defun make-glyph-upper-case-m (font)
  (with-accessors ((ascent ascent)
		   (upper-case-h-width upper-case-h-width)
		   (lower-case-ascent lower-case-ascent)
		   (stroke-width stroke-width)
		   (stroke-height stroke-height))
    font
    (flet ((c (x y) (complex x y)))
      (let* ((w1 (round (* 1.2 upper-case-h-width)))
	     (w2 (* 1.0 stroke-width))
	     (pa (c 0 ascent))
	     (pb (+ pa w2))
	     (pc (c (* 0.5 (- w1 w2)) (- lower-case-ascent (* 1.3 stroke-height))))
	     (pd (+ pc w2))
	     (pe (c (- w1 w2) ascent))
	     (pf (+ pe w2)))
	(make-glyph
	 (list (make-vertical-stroke font (c 0 0) ascent)
	       (make-vertical-stroke font (c (- w1 stroke-width) 0) ascent)
	       (mf pa -- pb -- pd -- pc -- cycle)
	       (mf pc -- pe -- pf -- pd -- cycle))
	 #\l #\l)))))

;;;
;;;                   w1
;;;          ____________________
;;;         |                    |
;;;        a    b                 
;;;         ****              ****
;;;         *****             ****
;;;         ******            ****
;;;         *******           ****
;;;         ********          ****
;;;         **** ****         ****
;;;         ****  ****        ****
;;;         ****   ****       ****
;;;         ****    ****      ****
;;;         ****     ****     ****
;;;         ****      ****    ****
;;;         ****       ****   ****
;;;         ****        ****  ****
;;;         ****         **** ****
;;;         ****          ********
;;;         ****           *******
;;;         ****            ******
;;;         ****             *****
;;;         ****              ****
;;;                          d    c
;;;                           |__|
;;;                            w2
;;;             

(defun make-glyph-upper-case-n (font)
  (with-accessors ((ascent ascent)
		   (upper-case-h-width upper-case-h-width)
		   (lower-case-ascent lower-case-ascent)
		   (stroke-width stroke-width)
		   (stroke-height stroke-height))
    font
    (flet ((c (x y) (complex x y)))
      (let* ((w1 (round (* 1.0 upper-case-h-width)))
	     (w2 (* 1.0 stroke-width))
	     (pa (c 0 ascent))
	     (pb (+ pa w2))
	     (pc (c w1 0))
	     (pd (- pc w2)))
	(make-glyph
	 (list (make-vertical-stroke font (c 0 0) ascent)
	       (make-vertical-stroke font (c (- w1 stroke-width) 0) ascent)
	       (mf pa -- pb -- pc -- pd -- cycle))
	 #\l #\l)))))

;;;         
;;;         
;;;                  
;;;               *******
;;;            ************
;;;           *****    *****
;;;          ****        ****
;;;         ****          ****
;;;         ****          ****
;;;         ****          ****
;;;         ****          ****
;;;         ****          ****
;;;         ****          ****
;;;         ****          ****
;;;         ****          ****
;;;         ****          ****
;;;         ****          ****
;;;         ****          ****
;;;          ****        ****
;;;           *****    *****
;;;            ************
;;;               *******
;;;                  a
;;;         

(defun make-glyph-upper-case-o (font)
  (with-accessors ((ascent ascent)
		   (upper-case-o-width upper-case-o-width))
    font
    (flet ((c (x y) (complex x y)))
      (let* ((pa (c (* 1/2 upper-case-o-width) 0))
	     (center (paths:make-point (* 1/2 upper-case-o-width)
				       (* 1/2 ascent))))
	(make-glyph
	 (list (paths:path-rotate
		(make-loop font pa 0 (* 1/2 upper-case-o-width) ascent)
		pi
		center)
	       (make-loop font pa 0 (* 1/2 upper-case-o-width) ascent))
	       
	 #\l #\l)))))

;;;              w1
;;;         ______________
;;;        |              |
;;;            
;;;        **********
;;;        ************
;;;        ****      ****
;;;        ****       ****
;;;        ****        ****
;;;        ****        ****
;;;        ****       ****
;;;        ****      ****         _
;;;        *************           |
;;;        ***********             |
;;;        ****                    |
;;;        ****                    |
;;;        ****                    |
;;;        ****                    |  h
;;;        ****                    |
;;;        ****                    |
;;;        ****                   _|
;;;            
;;;        
;;;        


(defun make-glyph-upper-case-p (font)
  (with-accessors ((ascent ascent)
		   (upper-case-h-width upper-case-h-width)
		   (upper-case-h-bar-position upper-case-h-bar-position)
		   (stroke-height stroke-height)
		   (stroke-width stroke-width))
    font
    (flet ((c (x y) (complex x y)))
      (let* ((w1 (round (* 0.83 upper-case-h-width)))
	     (h (* 0.84 upper-case-h-bar-position)))
	(make-glyph
	 (list (make-vertical-stroke font 0 ascent)
	       (make-loop font
			  (c 0 (- h stroke-height))
			  (* 0.55 w1)
			  w1
			  (+ (- ascent h) stroke-height)))
	 #\l #\P)))))

;;;         
;;;         
;;;                  
;;;               *******
;;;            ************
;;;           *****    *****
;;;          ****        ****
;;;         ****          ****
;;;         ****          ****
;;;         ****          ****
;;;         ****          ****
;;;         ****          ****
;;;         ****          ****
;;;         ****          ****
;;;         ****          ****
;;;         ****          ****
;;;         ****      b   ****     
;;;         ****       *  ****     - 
;;;          ****      ******       |
;;;           *****     *****       | h1
;;;            ****************     |
;;;               *******   ****   -
;;;                  a       ****   | h2
;;;         |__________|      **** -
;;;              w2          d    c
;;;         
;;;         |________________|____|
;;;                 w1         w3

(defun make-glyph-upper-case-q (font)
  (with-accessors ((ascent ascent)
		   (upper-case-o-width upper-case-o-width)
		   (stroke-width stroke-width)
		   (stroke-height stroke-height))
    font
    (flet ((c (x y) (complex x y)))
      (let* ((w1 (* 0.9 upper-case-o-width))
	     (w2 (* 0.5 upper-case-o-width))
	     (w3 (* 0.15 ascent))
	     (h1 (* 0.2 ascent))
	     (h2 (* 0.1 ascent))
	     (pa (c (* 1/2 upper-case-o-width) 0))
	     (pb (c w2 h1))
	     (pd (c w1 (- h2)))
	     (pc (+ pd w3))
	     (center (paths:make-point (* 1/2 upper-case-o-width)
				       (* 1/2 ascent))))
	(make-glyph
	 (list (paths:path-rotate
		(make-loop font pa 0 (* 1/2 upper-case-o-width) ascent)
		pi
		center)
	       (make-loop font pa 0 (* 1/2 upper-case-o-width) ascent)
	       (mf pb -- pc -- pd -- cycle))
	 #\l #\Q)))))

;;;              w1
;;;         ______________
;;;        |              |
;;;            
;;;        **********
;;;        ************
;;;        ****      ****
;;;        ****       ****
;;;        ****        ****
;;;        ****        ****
;;;        ****       ****
;;;        ****      ****         _
;;;        *************           |
;;;        *****b**c**             |
;;;        ****  ****              |
;;;        ****   ****             |
;;;        ****    ****            |
;;;        ****     ****           |  h
;;;        ****      ****          |
;;;        ****       ****         |
;;;        ****        ****       _|
;;;                    a  d
;;;        
;;;        |____|      |__|
;;;          w3         w2
;;;        
;;;        |______________|
;;;               w4
;;;

(defun make-glyph-upper-case-r (font)
  (with-accessors ((ascent ascent)
		   (upper-case-h-width upper-case-h-width)
		   (upper-case-h-bar-position upper-case-h-bar-position)
		   (stroke-height stroke-height)
		   (stroke-width stroke-width))
    font
    (flet ((c (x y) (complex x y)))
      (let* ((w1 (round (* 0.83 upper-case-h-width)))
	     (w2 (* 1.2 stroke-width))
	     (w3 (* 0.6 w1))
	     (w4 (* 0.90 upper-case-h-width))
	     (h upper-case-h-bar-position)
	     (pa (c (- w4 w2) 0))
	     (pb (c w3 (- h (* 0.5 stroke-height))))
	     (pc (+ pb w2))
	     (pd (+ pa w2)))
	(make-glyph
	 (list (make-vertical-stroke font 0 ascent)
	       (make-loop font
			  (c 0 (- h stroke-height))
			  (* 0.6 w1)
			  w1
			  (+ (- ascent h) stroke-height))
	       (mf pa -- pb -- pc -- pd -- cycle))
	 #\l #\R)))))

;;;               
;;;                 w2
;;;           _______________    
;;;          |               |
;;;                  g             _
;;;               *********         | h1
;;;             *************       | 
;;;           ****   k     **h     -|
;;;          ****           **      | h2
;;;         f****l           *     -
;;;          ****            j
;;;           ****   m
;;;             ***********
;;;                 **********
;;;         b        e     ****
;;;         *              ****
;;;         **            d****n
;;;         a**      c     ****
;;;          ****************
;;;             **********      
;;;                  o
;;;         |_________________|
;;;                 w1
;;;               

(defun make-glyph-upper-case-s (font)
  (with-accessors ((ascent ascent)
		   (upper-case-h-width upper-case-h-width)
		   (upper-case-h-bar-position upper-case-h-bar-position)
		   (stroke-height stroke-height)
		   (stroke-width stroke-width))
    font
    (flet ((c (x y) (complex x y)))
      (let* ((w1 (round (* 0.87 upper-case-h-width)))
	     (w2 (+ (* 2 (round (* 0.37 upper-case-h-width)))
		    (if (oddp w1) 1 0)))
	     (h1 (* 0.06 ascent))
	     (h2 (* 1.1 stroke-height))
	     (pa (c 0 h1))
	     (pb (+ pa (c 0 h2)))
	     (xo (* 1/2 w1))
	     (po (c xo 0))
	     (pc (+ po (c 0 stroke-height)))
	     (pm (c w1 (* 0.25 ascent)))
	     (pd (- pm stroke-width))
	     (ym (+ (* 1/2 (+ ascent stroke-height))
		    (if (= (mod ascent 2) (mod stroke-height 2)) 0 1/2)))
	     (pm (c xo ym))
	     (pe (- pm (c 0 stroke-height)))
	     (pf (c (* 1/2 (- w1 w2)) (* 0.75 ascent)))
	     (pg (c xo ascent))
	     (ph (c (* 1/2 (+ w1 w2)) (- ascent h1)))
	     (pj (- ph (c 0 h2)))
	     (pk (- pg (c 0 stroke-height)))
	     (pl (+ pf stroke-width))
	     (pn (+ pd stroke-width)))
	(make-glyph
	 (list (mf pa -- pb ++ right pc ++ up pd ++ left pe ++
		   up pf ++ right pg ++ ph -- pj ++ left pk ++
		   down pl ++ right pm ++ down pn ++ left po ++ cycle))
	 #\S #\S)))))

;;;                 
;;;                         w
;;;               ____________________  
;;;              |                    |
;;;                 
;;;              **********************
;;;              **********************
;;;                       ****
;;;                       ****
;;;                       ****
;;;                       ****
;;;                       ****
;;;                       ****
;;;                       ****
;;;                       ****
;;;                       ****
;;;                       ****
;;;                       ****
;;;                       ****
;;;                       ****
;;;                       ****
;;;                       ****
;;;                       ****
;;;                       ****
;;;                       ****
;;;                 
;;;                 
;;;                 

(defun make-glyph-upper-case-t (font)
  (with-accessors ((ascent ascent)
		   (upper-case-h-width upper-case-h-width)
		   (stroke-height stroke-height)
		   (stroke-width stroke-width))
    font
    (flet ((c (x y) (complex x y)))
      (let* ((w (- (* 2 (round (* 0.5 upper-case-h-width)))
		   (if (oddp stroke-width) 1 0))))
	(make-glyph (list (make-vertical-stroke
			   font (c (* 1/2 (- w stroke-width)) 0) ascent)
			  (make-horizontal-stroke
			   font (c 0 (- ascent stroke-height)) w))
		    #\T #\T)))))

;;;           
;;;           
;;;           
;;;           
;;;           
;;;           ****          ****
;;;           ****          ****
;;;           ****          ****
;;;           ****          ****
;;;           ****          ****
;;;           ****          ****
;;;           ****          ****
;;;           ****          ****
;;;           ****          ****
;;;           ****          ****
;;;           ****          ****
;;;           ****          ****
;;;           ****          ****
;;;           ****          ****
;;;          b****c        e****f   -
;;;            ****    d   ****      |
;;;              ************        | h
;;;                 *******         -
;;;                    a
;;;           
;;;           |________________|
;;;                   w
;;;           
;;;           

(defun make-glyph-upper-case-u (font)
  (with-accessors ((ascent ascent)
		   (upper-case-h-width upper-case-h-width)
		   (stroke-height stroke-height)
		   (stroke-width stroke-width))
    font
    (flet ((c (x y) (complex x y)))
      (let* ((w (round (* 1.0 upper-case-h-width)))
	     (h (* 0.29 ascent))
	     (pa (c (* 1/2 w) 0))
	     (pb (c 0 h))
	     (pc (+ pb stroke-width))
	     (pd (+ pa (c 0 stroke-height)))
	     (pf (c w h))
	     (pe (- pf stroke-width)))
	(make-glyph (list (make-vertical-stroke font pb (- ascent h))
			  (make-vertical-stroke font pe (- ascent h))
			  (mf pa left ++ pb -- pc down ++ right
			      pd ++ up pe -- pf down ++ left cycle))
		    #\l #\l)))))

;;;                       w1
;;;           _____________________________
;;;          |                             |
;;;            w3
;;;           ___
;;;          |   |
;;;          a    d                   h    g
;;;           ****                     ****
;;;           ****                     ****
;;;            ****                   ****
;;;            ****                   ****
;;;             ****                 ****
;;;             ****                 ****
;;;              ****               ****
;;;              ****               ****
;;;               ****             ****
;;;               ****             ****
;;;                ****           **** 
;;;                ****           ****      
;;;                 ****         ****      
;;;                 ****         ****      
;;;                  ****       ****
;;;                  ****       ****
;;;                   ****     ****    
;;;                   ****     ****    
;;;                    ****   ****
;;;                    ****   ****
;;;                     **** ****
;;;                     *********
;;;                      *******
;;;                      *******
;;;                       *****
;;;                       be cf 
;;;
;;;                       |___|
;;;                        w2
;;;                       
;;;                       
;;;           

(defun make-glyph-upper-case-v (font)
  (with-accessors ((ascent ascent)
		   (lower-case-ascent lower-case-ascent)
		   (upper-case-h-width upper-case-h-width)
		   (j-hook-start j-hook-start)
		   (stroke-height stroke-height)
		   (stroke-width stroke-width))
    font
    (flet ((c (x y) (complex x y)))
      (let* ((w2 (round (* 1.2 stroke-width)))
	     (w1 (let ((w (round (* 1.2 upper-case-h-width))))
		   (if (= (mod w2 2) (mod w 2)) w (1- w))))
	     (w3 (* 1.05 stroke-width))
	     (xb (* 1/2 (- w1 w2)))
	     (pa (c 0 ascent))
	     (pb (c xb 0))
	     (pc (+ pb w3))
	     (pd (+ pa w3))
	     (pf (c (* 1/2 (+ w1 w2)) 0))
	     (pe (- pf w3))
	     (pg (c w1 ascent))
	     (ph (- pg w3)))
	(make-glyph (list (mf pa -- pd -- pc -- pb -- cycle)
			  (mf ph -- pg -- pf -- pe -- cycle))
		    #\V #\V)))))

;;;                                 w1
;;;           _______________________________________________
;;;          |                                               |
;;;            w3
;;;           ___
;;;          |   |
;;;          a    b                                     o    p
;;;           ****                                       ****
;;;           ****                                       ****
;;;            ****                                     ****
;;;            ****                                     ****
;;;             ****                                   ****
;;;             ****                                   ****
;;;              ****                                 ****
;;;              ****                                 ****
;;;               ****             fj gk             ****
;;;               ****             *****             ****   -
;;;                ****           *******           ****     |
;;;                ****           *******           ****     |
;;;                 ****         *********         ****      |
;;;                 ****         **** ****         ****      |
;;;                  ****       ****   ****       ****       |
;;;                  ****       ****   ****       ****       |
;;;                   ****     ****     ****     ****        |
;;;                   ****     ****     ****     ****        | h
;;;                    ****   ****       ****   ****         |
;;;                    ****   ****       ****   ****         |
;;;                     **** ****         **** ****          |
;;;                     *********         *********          |
;;;                      *******           *******           |
;;;                      *******           *******           |
;;;                       *****             *****           -
;;;                       de ch             mn lq
;;;
;;;                       |___|
;;;                        w2
;;;                       
;;;                       
;;; We try to maintain the same slope of all the strokes.
;;; 

(defun make-glyph-upper-case-w (font)
  (with-accessors ((ascent ascent)
		   (lower-case-ascent lower-case-ascent)
		   (upper-case-h-width upper-case-h-width)
		   (upper-case-h-bar-position upper-case-h-bar-position)
		   (stroke-height stroke-height)
		   (stroke-width stroke-width))
    font
    (flet ((c (x y) (complex x y)))
      (let* ((w1 (round (* 1.5 upper-case-h-width)))
	     (w2 (round (* 1.2 stroke-width)))
	     (h (round (* 1.5 upper-case-h-bar-position)))
	     (w3 (* 1.05 stroke-width))
	     (xd (/ (- w1 w2) 2 (+ 1 (/ h ascent))))
	     (pa (c 0 ascent))
	     (pb (+ pa w3))
	     (pd (c xd 0))
	     (pc (+ pd w3))
	     (ph (+ pd w2))
	     (pe (- ph w3))
	     (pf (c (* 1/2 (- w1 w2)) h))
	     (pg (+ pf w3))
	     (pk (+ pf w2))
	     (pj (- pk w3))
	     (pq (c (- w1 xd) 0))
	     (pn (- pq w3))
	     (pm (- pq w2))
	     (pl (+ pm w3))
	     (pp (c w1 ascent))
	     (po (- pp w3)))
	(make-glyph (list (mf pa -- pb -- pc -- pd -- cycle)
			  (mf pe -- pf -- pg -- ph -- cycle)
			  (mf pj -- pk -- pl -- pm -- cycle)
			  (mf pn -- po -- pp -- pq -- cycle))
		    #\V #\V)))))

;;;               
;;;                  w1
;;;            ______________   
;;;           |              |
;;;               
;;;               
;;;          c****d      f****g
;;;            ****      ****
;;;             ****    ****
;;;              **** e****   _
;;;               ********     |
;;;                ******      | h1
;;;               b******h     | 
;;;               ********    _|
;;;              **** k****
;;;             ****    ****
;;;            ****      ****
;;;          a****l      j****i
;;;
;;;           |____|      |___|
;;;             w2          w3
;;;               
;;;               

(defun make-glyph-upper-case-x (font)
  (with-accessors ((ascent ascent)
		   (upper-case-h-width upper-case-h-width)
		   (stroke-height stroke-height)
		   (stroke-width stroke-width))
    font
    (flet ((c (x y) (complex x y)))
      (let* ((w1 (round (* 1.1 upper-case-h-width)))
	     (w2 (* 0.5 (- w1 (* 0.8 stroke-width))))
	     (w3 (* 1.1 stroke-width))
	     (h1 (* 1.0 stroke-width))
	     (pa (c 0 0))
	     (pb (c w2 (* 0.5 ascent)))
	     (pc (c 0 ascent))
	     (pd (+ pc w3))
	     (pe (c (* 0.5 w1) (+ (imagpart pb) (* 0.5 h1))))
	     (pf (c (- w1 w3) ascent))
	     (pg (c w1 ascent))
	     (ph (+ pb (- w1 (* 2 w2))))
	     (ppi (c w1 0))
	     (pj (c (- w1 w3) 0))
	     (pk (- pe (c 0 h1)))
	     (pl (c w3 0)))
	(make-glyph
	 (list (mf pa -- pb -- pc -- pd -- pe -- pf --
		   pg -- ph -- ppi -- pj -- pk -- pl -- cycle))
	 #\x #\x)))))

;;;
;;;
;;;                     w1
;;;             __________________
;;;            |                  |
;;;              
;;;             w2
;;;             __ 
;;;            |  |
;;;           b    c          e    f
;;;            ****            ****
;;;             ****          ****
;;;              ****        ****
;;;               ****      ****
;;;                ****    ****
;;;                 ****  ****
;;;                  ********
;;;                   ******
;;;                   a****d  -
;;;                    ****    |
;;;                    ****    |
;;;                    ****    |
;;;                    ****    | h
;;;                    ****    |
;;;                    ****    |
;;;                    ****    |
;;;                    ****   -
;;;

(defun make-glyph-upper-case-y (font)
  (with-accessors ((ascent ascent)
		   (upper-case-h-width upper-case-h-width)
		   (upper-case-h-bar-position upper-case-h-bar-position)
		   (stroke-height stroke-height)
		   (stroke-width stroke-width))
    font
    (flet ((c (x y) (complex x y)))
      (let* ((w1 (+ (* 2 (round (* 0.5 upper-case-h-width)))
		    (if (oddp stroke-width) 1 0)))
	     (w2 (* 1.1 stroke-width))
	     (h (round (* 0.6 upper-case-h-bar-position)))
	     (xa (* 1/2 (- w1 stroke-width)))
	     (pa (c xa h))
	     (pb (c 0 ascent))
	     (pc (+ pb w2))
	     (pd (+ pa stroke-width))
	     (pe (c (- w1 w2) ascent))
	     (pf (+ pe w2)))
	(make-glyph (list (make-vertical-stroke
			   font (c (* 1/2 (- w1 w2)) 0) h)
			  (mf pa -- pb -- pc -- pd -- cycle)
			  (mf pa -- pe -- pf -- pd -- cycle))
		    #\Y #\Y)))))


;;;           
;;;           
;;;           
;;;                   w1
;;;            ___________________
;;;           |                   |
;;;           
;;;           *********************
;;;           ****************b****c
;;;                           ****    
;;;                          ****    
;;;                         ****    
;;;                        ****    
;;;                       ****    
;;;                      ****    
;;;                     ****    
;;;                    ****    
;;;                   ****    
;;;                  ****    
;;;                 ****    
;;;                ****    
;;;               ****    
;;;              ****    
;;;             ****
;;;            ****
;;;          a****d****************
;;;           *********************
;;;
;;;
;;;           |___|
;;;             w2           
;;;           
;;;           

(defun make-glyph-upper-case-z (font)
  (with-accessors ((ascent ascent)
		   (upper-case-h-width upper-case-h-width)
		   (stroke-height stroke-height)
		   (stroke-width stroke-width))
    font
    (flet ((c (x y) (complex x y)))
      (let* ((w1 (round (* 0.9 upper-case-h-width)))
	     (w2 (* 1.1 stroke-width))
	     (pa (c 0 stroke-height))
	     (pc (c w1 (- ascent stroke-height)))
	     (pb (- pc w2))
	     (pd (+ pa w2)))
	(make-glyph (list (make-horizontal-stroke
			   font (c 0 0) w1)
			  (make-horizontal-stroke
			   font (c 0 (- ascent stroke-height)) w1)
			  (mf pa -- pb -- pc -- pd -- cycle))
		    #\Z #\Z)))))

;;;        
;;;        
;;;            c
;;;          ****
;;;       b********d
;;;       **********
;;;      a**********e
;;;       h********f
;;;          ****
;;;            g

(defun make-disk (center radius)
  (flet ((c (x y) (complex x y)))
    (let* ((pa (- center radius))
	   (pe (+ center radius))
	   (pc (+ center (c 0 radius)))
	   (pg (- center (c 0 radius)))
	   (pb (+ center (/ (c (- radius) radius) (sqrt 2))))
	   (pd (+ center (/ (c radius radius) (sqrt 2))))
	   (pf (+ center (/ (c radius (- radius)) (sqrt 2))))
	   (ph (+ center (/ (c (- radius) (- radius)) (sqrt 2)))))
      (mf pa up ++ pb ++ pc right ++ pd ++ pe down ++
	  pf ++ pg left ++ ph ++ cycle))))
  
(defun make-glyph-period (font)
  (with-accessors ((stroke-width stroke-width))
    font
    (flet ((c (x y) (complex x y)))
      (make-glyph (list (make-disk
			 (* 1 (c stroke-width stroke-width)) (* 1 stroke-width)))
		  #\. #\.))))

;;;     w1
;;;    _____    
;;;   |     |
;;;        
;;;         a        b   
;;;          ********    
;;;          ********    _
;;;          ********     |
;;;          *******      |
;;;          ******       |
;;;          *****        |  h1
;;;         ****          |
;;;       ****            |
;;;   c****              _|
;;;       

(defun make-comma-hook (font)
  (with-accessors ((stroke-width stroke-width)
		   (stroke-height stroke-height))
    font
    (flet ((c (x y) (complex x y)))
      (let* ((w1 (* 1.5 stroke-width))
	     (h1 (* 2.0 stroke-height))
	     (cw1 (ceiling w1))
	     (pa (c (+ cw1 (* 0.7 stroke-width)) stroke-width))
	     (pb (c (+ cw1 (* 2 stroke-width)) stroke-width))
	     (pc (c (- cw1 w1) (- h1))))
	(mf pa -- pb down ++ left pc & pc ++ up cycle)))))

(defun make-glyph-comma (font)
  (with-accessors ((stroke-width stroke-width))
    font
    (flet ((c (x y) (complex x y)))
      (let* ((w1 (* 2.0 stroke-width))
	     (cw1 (ceiling w1)))
	(make-glyph (list (make-disk
			   (c (+ stroke-width cw1) stroke-width)
			   stroke-width)
			  (make-comma-hook font))
		    #\, #\.)))))

(defun make-glyph-colon (font)
  (with-accessors ((stroke-width stroke-width)
		   (lower-case-ascent lower-case-ascent))
    font
    (flet ((c (x y) (complex x y)))
      (let* ((w1 stroke-width)
	     (h1 (max 5 lower-case-ascent)))
	(make-glyph (list (make-disk (c w1 w1) w1)
			  (make-disk (c w1 (- h1 w1)) w1))
		    #\: #\:)))))
		    
(defun make-glyph-semicolon (font)
  (with-accessors ((stroke-width stroke-width)
		   (lower-case-ascent lower-case-ascent))
    font
    (flet ((c (x y) (complex x y)))
      (let* ((w1 (* 2.0 stroke-width))
	     (cw1 (ceiling w1))
	     (h1 (max 5 lower-case-ascent)))
	(make-glyph (list (make-disk
			   (c (+ stroke-width cw1) stroke-width)
			   stroke-width)
			  (make-disk
			   (c (+ stroke-width cw1)
			      (- h1 stroke-width))
			   stroke-width)
			  (make-comma-hook font))
		    #\, #\.)))))

;;;        
;;;        
;;;        
;;;            
;;;          ****       -
;;;        ********      |
;;;       **********     |
;;;      c*****a****d    |
;;;       **********     |
;;;       **********     |
;;;       **********     |
;;;       **********     |
;;;        ********      |
;;;        ********      |
;;;        ********      |
;;;        ********      |
;;;        ********      |
;;;        ********      |
;;;         ******       |
;;;         ******       |
;;;         ******       |
;;;         ******       |
;;;         ******       |
;;;         ******       |
;;;          ****        |
;;;          ****        |  h
;;;          ****        |
;;;          ****        |
;;;          ****        |
;;;          ****        |
;;;           **         |
;;;           **         |
;;;           **         |
;;;           **         |
;;;           **         |
;;;           *b         |
;;;                      |
;;;          ****        |
;;;        ********      |
;;;       **********     |
;;;       **********     |
;;;        ********      |
;;;          ****       -
;;;            

(defun make-glyph-exclamation-mark (font)
  (with-accessors ((ascent ascent)
		   (stroke-width stroke-width))
    font
    (flet ((c (x y) (complex x y)))
      (let* ((h (round (* 1.0 ascent)))
	     (pa (c (* 1/2 stroke-width) (- h stroke-width)))
	     (pb (c (* 1/2 stroke-width) (* 2 stroke-width)))
	     (pc (- pa (* 1/2 stroke-width)))
	     (pd (+ pa (* 1/2 stroke-width))))
	(make-glyph (list (make-disk
			   (* 1/2 (c stroke-width stroke-width))
			   (* 1/2 stroke-width))
			  (make-disk pa (* 1/2 stroke-width))
			  (mf pc -- pd down ++ pb & pb ++ up cycle))
		    #\. #\.)))))

;;; Make this similar to the exclamation mark.

(defun make-quote (font xpos)
  (with-accessors ((ascent ascent)
		   (stroke-width stroke-width))
    font
    (flet ((c (x y) (complex x y)))
      (let* ((h1 (round (* 1.0 ascent)))
	     (h2 (* 0.3 ascent))
	     (pa (c (+ xpos (* 1/2 stroke-width)) (- h1 stroke-width)))
	     (pb (c (+ xpos (* 1/2 stroke-width)) (- ascent h2)))
	     (pc (- pa (* 1/2 stroke-width)))
	     (pd (+ pa (* 1/2 stroke-width))))
	(list (make-disk pa (* 1/2 stroke-width))
	      (mf pc -- pd down ++ pb & pb ++ up cycle))))))

(defun make-glyph-double-quote (font)
  (with-accessors ((ascent ascent)
		   (stroke-width stroke-width))
    font
    (let ((w (max (* 2 stroke-width) (round (* 0.2 ascent)))))
      (make-glyph (append (make-quote font 0)
			  (make-quote font w))
		  #\" #\"))))

;;; Make a "slash", i.e., a slanted line the height of the ascent
;;; of the font.
;;;
;;;  x1
;;;  ___
;;; |   |
;;;      ****
;;;       ****
;;;        ****
;;;         ****
;;;          ****
;;;           ****
;;;           |__|
;;;             w
;;;
;;; |____________|
;;;
;;;       x2

(defun make-slash (font x1 x2)
  (with-accessors ((ascent ascent)
		   (stroke-width stroke-width))
    font
    (flet ((c (x y) (complex x y)))
      (let* ((angle (atan (- x2 x1 stroke-width) ascent))
	     (w (/ stroke-width (cos angle)))
	     (pa (c x1 ascent))
	     (pb (+ pa w))
	     (pc (c x2 0))
	     (pd (- pc w)))
	(mf pa -- pb -- pc -- pd -- cycle)))))

;;;                
;;;                
;;;                  ****       ****
;;;                  ****       ****
;;;                 ****       ****
;;;                 ****       ****
;;;                ****       ****
;;;                ****       ****
;;;        **************************
;;;        **************************
;;;              ****       ****
;;;              ****       ****
;;;             ****       ****
;;;             ****       ****           _
;;;        **************************      |
;;;        **************************      |
;;;           ****       ****              |
;;;           ****       ****              |
;;;          ****       ****               | h1
;;;          ****       ****               |
;;;         ****       ****                |
;;;         ****       ****               _|
;;;       |_|         
;;;        w2        
;;;                
;;;                   |____|
;;;                     w4
;;;                
;;;       |___________|
;;;            w3
;;;                
;;;       |__________________________|         
;;;                   w1

(defun make-glyph-hash (font)
  (with-accessors ((ascent ascent)
		   (stroke-width stroke-width)
		   (stroke-height stroke-height))
    font
    (flet ((c (x y) (complex x y)))
      (let* ((w1 (round (* 0.6 ascent)))
	     (w2 (* 0.05 ascent))
	     (w3 (round (* 0.3 ascent)))
	     (w4 (* 1.2 stroke-width)) ; roughly
	     (h1 (round (* 0.40 ascent))))
	(make-glyph (list (make-horizontal-stroke
			   font
			   (c 0 (- h1 stroke-height))
			   w1)
			  (make-horizontal-stroke
			   font
			   (c 0 (- ascent h1))
			   w1)
			  (make-slash font (- w1 w3 w4) (+ w2 w4))
			  (make-slash font (- w1 w2 w4) (+ w3 w4)))
		    #\# #\#)))))

;;;                
;;;                
;;;                
;;;                ******
;;;              **********  
;;;             ****    ****
;;;             ****   
;;;             ****
;;;              ****
;;;              ****
;;;               ****  
;;;                ****   
;;;               *****  
;;;             ******** 
;;;            ****  ****   ****
;;;           ****   ****   ****
;;;           ****    **** ****
;;;          ****     *** ****
;;;          ****      *******
;;;         ****       *****
;;;         ****       *****
;;;         ****      ******
;;;          ****    ********
;;;          ****   **** ****
;;;           *********   ****
;;;             ******     ****
;;;                
;;;                
;;;                
;;;                
;;;                
;;;                
;;;                
;;;                
;;;                
;;;                
;;;                
;;;                
;;;                
;;;                

;;;
;;;          
;;;                     w
;;;               _____________
;;;              |             |
;;;                     f
;;;                  ********
;;;                *****c******   
;;;               ****      ****   
;;;              e****d    b****g    -
;;;                        ****       |
;;;                       ****        |
;;;                      ****         |
;;;                     ****          |
;;;                   a****h    -     |
;;;                    ****      |    |
;;;                    ****      |    | h2
;;;                    ****      |    |
;;;                    ****      |    |
;;;                    ****      | h1 |
;;;                              |    |
;;;                     **       |    |
;;;                    ****      |    |
;;;                     **      -    -
;;;                  
;;;                  
;;;                  

(defun make-glyph-question-mark (font)
  (with-accessors ((ascent ascent)
		   (stroke-width stroke-width)
		   (stroke-height stroke-height)
		   (upper-case-h-width upper-case-h-width))
    font
    (flet ((c (x y) (complex x y)))
      (let* ((h0 (* 2.0 stroke-width))
	     (h1 (* 0.4 ascent))
	     (h2 (* 0.75 ascent))
	     (w (+ (* 2 (round (* 0.25 upper-case-h-width)))
		   (if (oddp stroke-width) 1 0)))
	     (pa (c (* 1/2 (- w stroke-width)) h1))
	     (pb (c (- w stroke-width) h2))
	     (pf (c (* 1/2 w) ascent))
	     (pc (- pf (c 0 stroke-height)))
	     (pe (c 0 h2))
	     (pd (+ pe stroke-width))
	     (pg (+ pb stroke-width))
	     (ph (+ pa stroke-width)))
	(make-glyph (list (make-disk
			   (* 1/2 (c w stroke-width))
			   (* 1/2 stroke-width))
			  (make-vertical-stroke
			   font
			   (c (* 1/2 (- w stroke-width)) h0)
			   (- h1 h0))
			  (mf pa up ++ pb up ++ pc left ++ down pd --
			      pe up ++ pf right ++ pg down ++ down ph -- cycle))
		    #\? #\?)))))
    
(defun make-glyph-slash (font)
  (with-accessors ((ascent ascent)
		   (slash-width slash-width)
		   (stroke-width stroke-width))
    font
    (make-glyph (list (make-slash font
				  (- slash-width (* 1.3 stroke-width))
				  (* 1.3 stroke-width)))
		#\/ #\/)))


;;;            
;;;    _         b     _  
;;;   |          ****   |
;;;   |         *****   |
;;;   |        ******   |h2
;;;   |       *******  _|
;;; h1|      ****c***
;;;   |     **** ****
;;;   |_   ****  ****
;;;       a   d  ****
;;;              ****
;;;              ****
;;;              ****
;;;              ****
;;;              ****
;;;              ****
;;;              ****
;;;              ****
;;;              ****
;;;              ****
;;;              ****
;;;              ****
;;;              ****
;;;              ****
;;;              ****
;;;              ****
;;;              ****
;;;              ****
;;;              ****
;;;              ****
;;;              ****
;;;              ****
;;;        |____|    
;;;          w1   
;;;            
;;;            

(defun make-glyph-digit-1 (font)
  (with-accessors ((ascent ascent)
		   (stroke-width stroke-width)
		   (stroke-height stroke-height))
    font
    (flet ((c (x y) (complex x y)))
      (let* ((h1 (round (* 0.3 ascent)))
	     (h2 (* 1.2 stroke-height))
	     (w1 (* 0.15 ascent))
	     (pa (c (- (ceiling w1) w1) (- ascent h1)))
	     (pb (c (ceiling w1) ascent))
	     (pc (c (ceiling w1) (- ascent h2)))
	     (pd (+ pa (* 1.0 stroke-width))))
	(make-glyph (list (make-vertical-stroke font (ceiling w1) ascent)
			  (mf pa -- pb -- pc -- pd -- cycle))
		    #\1 #\l)))))

;;;           w1
;;;          ____
;;;         |    |
;;;         
;;;     _         g        _
;;;    |        ****        |
;;;  h1|      ****d***      |
;;;    |     ****  ****     | h2
;;;    |_  f****e   ****   _|
;;;                c****h
;;;                ****
;;;               ****
;;;              ****
;;;             ****
;;;            ****
;;;           ****
;;;          ****
;;;        b****j          -
;;;         ****k       l   |
;;;         ************    | h3
;;;         ************   -
;;;        a            m
;;;         
;;;         
;;;         

(defun make-glyph-digit-2 (font)
  (with-accessors ((ascent ascent)
		   (stroke-width stroke-width)
		   (stroke-height stroke-height)
		   (digit-width digit-width))
    font
    (flet ((c (x y) (complex x y)))
      (let* ((h1 (round (* 0.3 ascent)))
	     (h2 (round (* 0.25 ascent)))
	     (h3 (* 0.15 ascent))
	     (w0 digit-width)
	     (w1 (* 1/2 digit-width))
	     (pa (c 0 0))
	     (pb (c 0 h3))
	     (pc (c (- w0 stroke-width) (- ascent h2)))
	     (pd (c w1 (- ascent stroke-height)))
	     (pe (c stroke-width (- ascent h1)))
	     (pf (c 0 (- ascent h1)))
	     (pg (c w1 ascent))
	     (ph (+ pc stroke-width))
	     (pj (+ pb stroke-width))
	     (pk (c stroke-width stroke-height))
	     (pl (c w0 stroke-height))
	     (pm (c w0 0)))
	(make-glyph (list (mf pa -- pb up ++ pc up ++ pd left ++ down pe --
			      pf up ++ pg right ++ ph down ++ down pj -- pk --
			      pl -- pm -- cycle))
		    #\2 #\2)))))

;;;            m       _
;;;          *****      |
;;;        *********    | h3
;;;       **** j ****  _|
;;;      l****k h****n 
;;;           g ****
;;;       -w1--****p
;;;           f****
;;;    _  b  c  ****   
;;;   |   ****  e****q _
;;; h1|   **** d ****   |
;;;   |    *********    | h2
;;;   |_     *****     _|
;;;            a
;;;       |____|
;;;         w2

(defun make-glyph-digit-3 (font)
  (with-accessors ((ascent ascent)
		   (stroke-width stroke-width)
		   (stroke-height stroke-height)
		   (digit-width digit-width))
    font
    (flet ((c (x y) (complex x y)))
      (let* ((h1 (round (* 0.3 ascent)))
	     (h2 (* 0.3 ascent))
	     (h3 (* 0.25 ascent))
	     (w0 digit-width)
	     (w1 (round (* 0.4 w0)))
	     (w2 (* 1/2 w0))
	     (pa (c w2 0))
	     (pb (c 0 h1))
	     (pc (+ pb stroke-width))
	     (pd (+ pa (c 0 stroke-height)))
	     (pe (c (- w0 stroke-width) h2))
	     (pf (c w1 (- (* 0.55 ascent) (* 1/2 stroke-height))))
	     (pg (+ pf (c 0 stroke-height)))
	     (ph (c (- w0 stroke-width) (- ascent h3)))
	     (pj (c w2 (- ascent stroke-height)))
	     (pk (c stroke-width (- ascent h1)))
	     (pl (c 0 (- ascent h1)))
	     (pm (c w2 ascent))
	     (pn (+ ph stroke-width))
	     (pp (c (+ w1 stroke-width) (* 0.55 ascent)))
	     (pq (+ pe stroke-width)))
	(make-glyph (list (mf pa left ++ up pb -- pc down ++ pd right ++
			      pe up ++ pf -- pg ++ up ph ++ left pj ++
			      down pk -- pl up ++ right pm ++ down pn ++
			      left pp & pp right ++ down pq ++ left cycle))
		    #\3 #\3)))))


;;;          
;;;               b
;;;               ****
;;;              *****
;;;             ******
;;;            *******
;;;           ****c***
;;;          **** ****
;;;          **** **** 
;;;         ****  ****
;;;         ****  ****   
;;;        ****   ****
;;;        ****   ****   
;;;       ****    ****
;;;       ****d   ****  e    _
;;;    aa***************      |
;;;      ***************f     |
;;;     a         ****        |
;;;               ****        |
;;;               ****        | h1
;;;               ****        |
;;;               ****        |
;;;               ****        |
;;;               ****        |
;;;               ****       _|
;;;               ****       
;;;      |_______|    |_|
;;;           
;;;         w1         w2
;;;          

(defun make-glyph-digit-4 (font)
  (with-accessors ((ascent ascent)
		   (stroke-width stroke-width)
		   (stroke-height stroke-height)
		   (digit-width digit-width))
    font
    (flet ((c (x y) (complex x y)))
      (let* ((h1 (round (* 0.4 ascent)))
	     (w0 digit-width)
	     (w1 (* 0.6 w0))
	     (w2 (round (* 0.2 w0)))
	     (pa (c (- (ceiling w1) w1) (- h1 stroke-height)))
	     (paa (+ pa (c 0 stroke-height)))
	     (pb (c (ceiling w1) ascent))
	     (pc (c (ceiling w1) (- ascent (* 1.6 stroke-height))))
	     (pd (+ paa stroke-width))
	     (pe (c (+ (ceiling w1) stroke-width w2) h1))
	     (pf (- pe (c 0 stroke-height))))
	(make-glyph (list (mf pa -- paa -- pb -- pc -- pd -- pe -- pf -- cycle)
			  (make-vertical-stroke font (c (ceiling w1) 0) ascent))
		    #\4 #\4)))))

;;;       
;;;    _   
;;;   |   ****************
;;;   |   ****************
;;;   |   ****
;;;   |   ****
;;; h4|   ****
;;;   |   ****
;;;   |   ****
;;;   |   ****     j        _
;;;   |   ****    ****       |
;;;   |   ****h ********     |
;;;   |   *******  f ****    |
;;;   |_  *****       ****   |
;;;           g       ****   |    _
;;;       |________| e****k  |h2   |
;;;     _        w2   ****   |     |
;;;    | b****c       ****   |     |
;;;    |  ****        ****   |     |
;;;  h1|   ****      ****    |     | h3
;;;    |    ****  d ****     |     |
;;;    |      *********      |     |
;;;    |_       *****       _|    _|
;;;               a
;;;       |_______|
;;;          w1
;;;       

(defun make-glyph-digit-5 (font)
  (with-accessors ((ascent ascent)
		   (stroke-width stroke-width)
		   (stroke-height stroke-height)
		   (digit-width digit-width))
    font
    (flet ((c (x y) (complex x y)))
      (let* ((h1 (round (* 0.25 ascent)))
	     (h2 (round (* 0.65 ascent)))
	     (h3 (* 1/2 h2))
	     (h4 (round (* 0.55 ascent)))
	     (w0 digit-width)
	     (w1 (* 0.5 w0))
	     (w2 (* 0.5 w0))
	     (pa (c w1 0))
	     (pb (c 0 h1))
	     (pc (+ pb stroke-width))
	     (pd (+ pa (c 0 stroke-height)))
	     (pe (c (- digit-width stroke-width) h3))
	     (pf (c w2 (- h2 stroke-height)))
	     (pg (c stroke-width (- ascent h4)))
	     (ph (+ pg (c 0 (* 0.8 stroke-height))))
	     (pj (+ pf (c 0 stroke-height)))
	     (pk (+ pe stroke-width)))
	(make-glyph (list (mf pa left ++ up pb -- pc down ++ pd right ++
			      pe up ++ pf left ++ pg -- ph ++ right pj ++
			      down pk ++ left cycle)
			  (make-vertical-stroke
			   font (c 0 (- ascent h4)) h4)
			  (make-horizontal-stroke
			   font
			   (c stroke-width (- ascent stroke-height))
			   (round (* 0.9 (- w0 stroke-width)))))
		    #\5 #\5)))))

;;;                   w1
;;;                ________     
;;;               |        |
;;;                        d            _
;;;                     *******          |
;;;                   ***********        |
;;;                 ****   g  ****       | h4
;;;                ****        ****      |
;;;              c****h       f****e    _|
;;;               ****      
;;;               ****     p             _
;;;               ****   *****            |
;;;               ****o**********         |
;;;               ******** m  ****        |
;;;   -           ****n        ****       |
;;;  |      -    b****j       l****q -    |
;;;  |     |       ****        ****   |   | h1
;;;  |     |       ****       ****    |   |
;;;h5|  h3 |        ****  k  ****     |h2 |
;;;  |     |          **********      |   |
;;;  |_    |_           ******       _|  _|
;;;                       a
;;;                     
;;;                     
;;;                     
;;;                     

(defun make-glyph-digit-6 (font)
  (with-accessors ((ascent ascent)
		   (stroke-width stroke-width)
		   (stroke-height stroke-height)
		   (digit-width digit-width))
    font
    (flet ((c (x y) (complex x y)))
      (let* ((h1 (round (* 0.65 ascent)))
	     (h2 (* 0.32 ascent))
	     (h3 (* 0.3 ascent))
	     (h4 (* 0.25 ascent))
	     (h5 (* 0.45 ascent))
    	     (w0 digit-width)
	     (w1 (* 0.5 w0))
	     (pa (c w1 0))
	     (pb (c 0 h3))
	     (pc (c 0 (- ascent h3)))
	     (pd (c w1 ascent))
	     (pe (c w0 (- ascent h4)))
	     (pf (- pe stroke-width))
	     (pg (- pd (c 0 stroke-height)))
	     (ph (+ pc stroke-width))
	     (pj (+ pb stroke-width))
	     (pk (+ pa (c 0 stroke-height)))
	     (pl (c (- w0 stroke-width) h2))
	     (pm (c w1 (- h1 stroke-height)))
	     (pn (c stroke-width h5))
	     (po (+ pn (c 0 stroke-height)))
	     (pp (+ pm (c 0 stroke-height)))
	     (pq (+ pl stroke-width)))
	(make-glyph (list (mf pa left ++ up pb -- pc up ++ right pd ++
			      down pe -- pf up ++ pg left ++ down ph --
			      pj down ++ right pk ++ up pl ++ left pm ++
			      pn -- po ++ right pp ++ down pq ++ left cycle))
		    #\6 #\6)))))

;;;          
;;;          
;;;          
;;;          
;;;          ********************
;;;          ********************
;;;                        b****c
;;;                         ****
;;;                        ****
;;;                        ****
;;;                       ****
;;;                       ****
;;;                      ****
;;;                      ****
;;;                     ****
;;;                     ****
;;;                    ****
;;;                    ****
;;;                   ****
;;;                   ****
;;;                  ****
;;;                  ****
;;;                 ****
;;;                 ****
;;;                ****
;;;                ****
;;;               a    d
;;;          |_____|
;;;            w1
;;;          

(defun make-glyph-digit-7 (font)
  (with-accessors ((ascent ascent)
		   (stroke-width stroke-width)
		   (stroke-height stroke-height)
		   (digit-width digit-width))
    font
    (flet ((c (x y) (complex x y)))
      (let* ((w0 digit-width)
	     (w1 (* 0.3 w0))
	     (pa (c w1 0))
	     (pb (c (- w0 stroke-width) (- ascent stroke-height)))
	     (pc (+ pb stroke-width))
	     (pd (+ pa stroke-width)))
	(make-glyph (list (mf pa -- pb -- pc -- pd -- cycle)
			  (make-horizontal-stroke
			   font (c 0 (- ascent stroke-height)) digit-width))
		    #\7 #\7)))))

;;;                    w1
;;;                  _______    
;;;                 |       |
;;;                         k           _
;;;                      *******         |
;;;                    ***********       | h2
;;;                   ****  n  ****      |
;;;                 j****p     m****l   -
;;;                  ****       ****    
;;;                   ****  c  ****     _
;;;                    ***********       |
;;;                    ***********       |
;;;                   ****  g  ****      |
;;;                  ****       ****     |
;;;                 ****         ****    | h1
;;;      -         b****h       f****d   |    
;;;     |            ****        ****    |
;;; h3  |             ****   e  ****     |
;;;     |              ************      |
;;;     |_               ********       _|
;;;                          a
;;;                    

(defun make-glyph-digit-8 (font)
  (with-accessors ((ascent ascent)
		   (stroke-width stroke-width)
		   (stroke-height stroke-height)
		   (digit-width digit-width))
    font
    (flet ((c (x y) (complex x y)))
      (let* ((w0 digit-width)
	     (ww1 (* 1/2 (round (* 0.8 w0))))
	     (w1 (+ ww1 (if (integerp ww1) 0 1/2)))
	     (h1 (* 0.60 ascent))
	     (h2 (* 0.25 ascent))
	     (h3 (* 0.30 ascent))
	     (pa (c (* 1/2 w0) 0))
	     (pb (c 0 h3))
	     (pc (c (* 1/2 w0) h1))
	     (pd (c w0 h3))
	     (pe (+ pa (c 0 stroke-height)))
	     (pf (- pd stroke-width))
	     (pg (- pc (c 0 stroke-height)))
	     (ph (+ pb stroke-width))
	     (pj (c (- (* 1/2 w0) w1) (- ascent h2)))
	     (pk (c (* 1/2 w0) ascent))
	     (pl (c (+ (* 1/2 w0) w1) (- ascent h2)))
	     (pm (- pl stroke-width))
	     (pn (- pk (c 0 stroke-height)))
	     (pp (+ pj stroke-width)))
	(make-glyph (list (mf pa left ++ pb up ++ pc right ++
			      pd down ++ left cycle)
			  (mf pe right ++ pf up ++ pg left ++
			      ph down ++ right cycle)
			  (mf pg left ++ pj up ++ pk right ++
			      pl down ++ left cycle)
			  (mf pc right ++ pm up ++ pn left ++
			      pp down ++ right cycle))
		    #\8 #\8)))))

;;;                   w1
;;;                ________     
;;;               |        |
;;;        _               o            _    _   _
;;;       |             *******          |    |   |
;;;       |           ***********        |    |   |
;;;     h2|         ****   g  ****       | h3 |   |
;;;       |        ****        ****      |    |   |
;;;        -     n****h       f****p    _|    |   | h1
;;;               ****         ****          _|   |
;;;               ****        k****               |
;;;                ****   j  ******               |
;;;   -              *********l****               |
;;;  |                  *****  ****              _|
;;;  |                    m    ****        
;;;  |                        e****q 
;;;  |      -     b****c       ****  
;;;  |     |       ****       ****   
;;;h5|  h4 |        ****  d  ****    
;;;  |     |          **********     
;;;  |_    |_           ******       
;;;                       a
;;;                     
;;;                     
;;;                     
;;;                     

(defun make-glyph-digit-9 (font)
  (with-accessors ((ascent ascent)
		   (stroke-width stroke-width)
		   (stroke-height stroke-height)
		   (digit-width digit-width))
    font
    (flet ((c (x y) (complex x y)))
      (let* ((h1 (round (* 0.65 ascent)))
	     (h2 (* 0.32 ascent))
	     (h3 (* 0.3 ascent))
	     (h4 (* 0.25 ascent))
	     (h5 (* 0.45 ascent))
    	     (w0 digit-width)
	     (w1 (* 0.5 w0))
	     (pa (c w1 0))
	     (pb (c 0 h4))
	     (pc (+ pb stroke-width))
	     (pd (+ pa (c 0 stroke-height)))
	     (pe (c (- w0 stroke-width) h3))
	     (pf (c (- w0 stroke-width) (- ascent h3)))
	     (pg (c w1 (- ascent stroke-height)))
	     (ph (c stroke-width (- ascent h2)))
	     (pj (c w1 (+ (- ascent h1) stroke-height)))
	     (pk (c (- w0 stroke-width) (+ h5 stroke-height)))
	     (pl (- pk (c 0 stroke-height)))
	     (pm (- pj (c 0 stroke-height)))
	     (pn (c 0 (- ascent h2)))
	     (po (+ pg (c 0 stroke-height)))
	     (pp (+ pf stroke-width))
	     (pq (+ pe stroke-width)))
	(make-glyph (list (mf pa left ++ up pb -- pc down ++ right pd ++
			      up pe -- pf up ++ pg left ++ down ph ++
			      pj right ++ pk -- pl ++ left pm ++
			      up pn ++ right po ++ down pp -- pq down ++ left cycle))
		    #\9 #\9)))))

;;;                     
;;;                     
;;;                     
;;;                     
;;;                          d    
;;;                     ***********
;;;                   ***************  
;;;                  ****    k    ****    
;;;                c****l         j****e
;;;                 ****           ****    
;;;                 ****           ****    
;;;                 ****           ****    
;;;                 ****           ****    
;;;                 ****           ****    
;;;                 ****           ****    
;;;                 ****           ****    
;;;                 ****           ****    
;;;                 ****           ****    
;;;                 ****           ****    
;;;                 ****           ****    
;;;                b****m         h****f   -
;;;                  ****    g    ****      |
;;;                   ***************       | h1
;;;                     ***********        _|
;;;                          a
;;;                     
;;;                     
;;;                     

(defun make-glyph-digit-0 (font)
  (with-accessors ((ascent ascent)
		   (stroke-width stroke-width)
		   (stroke-height stroke-height)
		   (digit-width digit-width))
    font
    (flet ((c (x y) (complex x y)))
      (let* ((w0 digit-width)
	     (w1 (* 1/2 w0))
	     (h1 (round (* 0.3 ascent)))
	     (pa (c w1 0))
	     (pb (c 0 h1))
	     (pc (c 0 (- ascent h1)))
	     (pd (c w1 ascent))
	     (pe (c w0 (- ascent h1)))
	     (pf (c w0 h1))
	     (pg (+ pa (c 0 stroke-height)))
	     (ph (- pf stroke-width))
	     (pj (- pe stroke-width))
	     (pk (- pd (c 0 stroke-height)))
	     (pl (+ pc stroke-width))
	     (pm (+ pb stroke-width)))
	(make-glyph (list (mf pa left ++ up pb -- pc up ++
			      pd right ++ down pe -- pf down ++ left cycle)
			  (mf pg right ++ up ph -- pj up ++
			      left pk ++ down pl -- pm down ++ right cycle))
		    #\l #\l)))))

(defun make-glyph-left-bracket (font)
  (with-accessors ((bracket-width bracket-width)
		   (bracket-descent bracket-descent)
		   (bracket-ascent bracket-ascent)
		   (stroke-height stroke-height))
    font
    (flet ((c (x y) (complex x y)))
      (make-glyph (list (make-vertical-stroke font (c 0 (- bracket-descent)) (+ bracket-ascent bracket-descent))
			(make-horizontal-stroke font (c 0 (- bracket-descent)) bracket-width)
			(make-horizontal-stroke font (c 0 (- bracket-ascent stroke-height)) bracket-width))
		  #\l #\[))))

(defun make-glyph-backslash (font)
  (with-accessors ((ascent ascent)
		   (slash-width slash-width))
    font
    (make-glyph (list (make-slash font 0 slash-width))
		#\/ #\/)))

(defun make-glyph-right-bracket (font)
  (with-accessors ((bracket-width bracket-width)
		   (bracket-descent bracket-descent)
		   (bracket-ascent bracket-ascent)
		   (stroke-height stroke-height)
		   (stroke-width stroke-width))
    font
    (flet ((c (x y) (complex x y)))
      (make-glyph (list (make-vertical-stroke font (c (- bracket-width stroke-width) (- bracket-descent)) (+ bracket-ascent bracket-descent))
			(make-horizontal-stroke font (c 0 (- bracket-descent)) bracket-width)
			(make-horizontal-stroke font (c 0 (- bracket-ascent stroke-height)) bracket-width))
		  #\] #\l))))

;;;            
;;;         w2
;;;         ___
;;;        |   |
;;;            g   
;;;            *******h
;;;          *******
;;;        f****j
;;;         ****   
;;;         ****   
;;;         ****   
;;;         ****   
;;;         ****   
;;;         ****   
;;;         ****   
;;;         ****   
;;;         ****   
;;;        e****k   -
;;;        ****      |
;;;     d****l       | h2
;;;        ****      |
;;;        c****m   -
;;;         **** 
;;;         **** 
;;;         **** 
;;;         **** 
;;;         **** 
;;;         **** 
;;;         **** 
;;;         **** 
;;;         **** 
;;;        b****n       _   
;;;          *******     |  
;;;            *******o _| h1
;;;            a
;;;      |_|
;;;       w1     
;;;            
;;;            

(defun make-glyph-left-brace (font)
  (with-accessors ((ascent ascent)
		   (bracket-width bracket-width)
		   (bracket-descent bracket-descent)
		   (bracket-ascent bracket-ascent)
		   (stroke-height stroke-height)
		   (stroke-width stroke-width))
    font
    (flet ((c (x y) (complex x y)))
      (let* ((w1 (* 0.1 ascent))
	     (ww1 (ceiling w1))
	     (w2 (* 0.6 bracket-width))
	     (h0 (+ bracket-ascent bracket-descent))
	     (h1 (* 0.26 ascent))
	     (h2 (* 0.4 ascent))
	     (pa (c (+ ww1 w2) (- bracket-descent)))
	     (pb (c ww1 (- h1 bracket-descent)))
	     (pc (c ww1 (- (* 0.5 (- h0 h2)) bracket-descent)))
	     (pd (c (- ww1 w1) (- (* 0.5 h0) bracket-descent)))
	     (pe (c ww1 (- bracket-ascent (* 0.5 (- h0 h2)))))
	     (pf (c ww1 (- bracket-ascent h1)))
	     (pg (+ pa (c 0 h0)))
	     (ph (c (+ ww1 bracket-width) bracket-ascent))
	     (pj (+ pf stroke-width))
	     (pk (+ pe stroke-width))
	     (pl (+ pd (* 0.7 stroke-width)))
	     (pm (+ pc stroke-width))
	     (pn (+ pb stroke-width))
	     (po (c (+ ww1 bracket-width) (- bracket-descent))))
	(make-glyph (list (mf pa left ++ up pb ++ up pc ++ pd &
			      pd ++ up pe ++ up pf ++ right pg ++
			      ph & ph ++ down pj ++ down pk ++
			      pl & pl ++ down pm ++ down pn ++
			      po & po ++ left cycle))
		    #\{ #\{)))))

;;;            
;;;                    w2
;;;                    ___
;;;                   |   |
;;;                   g      
;;;            f*******
;;;               *******
;;;                  e****h
;;;                   ****   
;;;                   ****   
;;;                   ****   
;;;                   ****   
;;;                   ****   
;;;                   ****   
;;;                   ****   
;;;                   ****   
;;;                   ****   
;;;                  d****j     -
;;;                    ****      |
;;;                     c****k   | h2
;;;                    ****      |
;;;                  b****l     -
;;;                   **** 
;;;                   **** 
;;;                   **** 
;;;                   **** 
;;;                   **** 
;;;                   **** 
;;;                   **** 
;;;                   **** 
;;;                   **** 
;;;                  a****m    _   
;;;               *******       |  
;;;            o*******        _| h1
;;;                   n    
;;;                       |_|
;;;                        w1     
;;;            
;;;            

(defun make-glyph-right-brace (font)
  (with-accessors ((ascent ascent)
		   (bracket-width bracket-width)
		   (bracket-descent bracket-descent)
		   (bracket-ascent bracket-ascent)
		   (stroke-height stroke-height)
		   (stroke-width stroke-width))
    font
    (flet ((c (x y) (complex x y)))
      (let* ((w1 (* 0.1 ascent))
	     (w2 (* 0.6 bracket-width))
	     (h0 (+ bracket-ascent bracket-descent))
	     (h1 (* 0.26 ascent))
	     (h2 (* 0.4 ascent))
	     (pa (c (- bracket-width stroke-width) (- h1 bracket-descent)))
	     (pb (c (- bracket-width stroke-width) (- (* 0.5 (- h0 h2)) bracket-descent)))
	     (pc (c (- (+ bracket-width w1) (* 0.7 stroke-width)) (- (* 0.5 h0) bracket-descent)))
	     (pd (c (- bracket-width stroke-width) (- bracket-ascent (* 0.5 (- h0 h2)))))
	     (pe (c (- bracket-width stroke-width) (- bracket-ascent h1)))
	     (pf (c 0 bracket-ascent))
	     (pg (c (- bracket-width w2) bracket-ascent))
	     (ph (+ pe stroke-width))
	     (pj (+ pd stroke-width))
	     (pk (c (+ bracket-width w1) (- (* 0.5 h0) bracket-descent)))
	     (pl (+ pb stroke-width))
	     (pm (+ pa stroke-width))
	     (pn (c (- bracket-width w2) (- bracket-descent)))
	     (po (c 0 (- bracket-descent))))
	(make-glyph (list (mf pa up ++ pb up ++ pc & pc ++ up pd ++
			      up pe ++ pf & pf ++ right pg ++ down ph ++
			      down pj ++ pk & pk ++ down pl ++ down pm ++
			      left pn +++ po & po ++ up cycle))
		    #\} #\})))))

(defun add-glyph (character glyphs glyph)
  (setf (gethash character glyphs) glyph))

(defun add-kerning-info (font left-char right-char delta)
  (setf (gethash (cons left-char right-char) (kerning-info font))
	delta))

(defun compute-kerning-info (font)
  (with-accessors ((stroke-width stroke-width)
		   (width width))
    font
    (add-kerning-info font #\i #\o (round (* 0.2 stroke-width)))
    (add-kerning-info font #\o #\i (round (* 0.2 stroke-width)))
    (add-kerning-info font #\o #\o (round (* 0.3 stroke-width)))
    (add-kerning-info font #\x #\o (round (* 1.5 stroke-width)))
    (add-kerning-info font #\c #\o (round (* 1.0 stroke-width)))
    (add-kerning-info font #\o #\x (round (* 0.5 stroke-width)))
    (add-kerning-info font #\f #\o (round (* 1.6 stroke-width)))
    (add-kerning-info font #\o #\f (round (* 1.0 stroke-width)))
    (add-kerning-info font #\f #\l (round (* 1.0 stroke-width)))
    ;; move the j closer to anything without descent 
    (add-kerning-info font #\i #\j (round (* 2.0 stroke-width)))
    (add-kerning-info font #\l #\j (round (* 2.0 stroke-width)))
    (add-kerning-info font #\o #\j (round (* 2.0 stroke-width)))
    (add-kerning-info font #\c #\j (round (* 2.0 stroke-width)))
    (add-kerning-info font #\x #\j (round (* 2.0 stroke-width)))
    (add-kerning-info font #\f #\j (round (* 2.0 stroke-width)))
    ;; things that follow an `r' can sometimes move closer
    (add-kerning-info font #\r #\o
		      (round (* (if (< width 6) 0 1.0) stroke-width)))))

(defun make-font (size resolution)
  (let* ((pixel-size (round (/ (* size resolution) 72)))
	 (ascent (max 4 pixel-size))
	 (stroke-width (max 1 (round (* ascent 0.08))))
	 (font (make-instance 'font
		 :ascent ascent
		 :lower-case-ascent (max 2 (round (* ascent 0.6)))
		 :stroke-width stroke-width
		 :stroke-height (max 1 (round (* ascent 0.08)))
		 :j-width (* 3.0 stroke-width)
		 :j-hook-start (* 0.4 ascent)
		 :j-hook-extreme (* 2.0 stroke-width)
		 :slash-width (round (* 0.5 ascent))
		 :bracket-descent (* 2 (max 1 (round (* ascent 0.08)))) ; 2 *  stroke-height 
		 :bracket-width (max 2 (round (* ascent 0.2)))
		 :bracket-ascent (round (* ascent 1.0))
		 :descent (max 2 (round (* ascent 0.25)))
		 :width (max 3 (round (* ascent 0.50)))
		 :upper-case-h-width (round (* ascent 0.71))
		 :upper-case-o-width (round (* ascent 0.88))
		 :digit-width (round (* ascent 0.4))
		 :upper-case-h-bar-position (round (* ascent 0.56))))
	 (glyphs (glyphs font)))
    (compute-kerning-info font)
    ;; Make a glyph for the space character
    (let ((mask (make-array (list 1 (width font))
			    :element-type 'double-float
			    :initial-element 0d0)))
      (add-glyph #\Space glyphs
		 (make-instance 'glyph
				:x-offset 0 :y-offset -1
				:left-shape #\l :right-shape #\l
				:mask mask)))
    ;; Make a default glyph
    (let ((mask (make-array (list 1 (width font))
			    :element-type 'double-float
			    :initial-element 05.d0)))
      (add-glyph 'default glyphs
		 (make-instance 'glyph
				:x-offset 0 :y-offset -1
				:left-shape #\l :right-shape #\l
				:mask mask)))
    (add-glyph #\a glyphs (make-glyph-lower-case-a font))
    (add-glyph #\b glyphs (make-glyph-lower-case-b font))
    (add-glyph #\c glyphs (make-glyph-lower-case-c font))
    (add-glyph #\d glyphs (make-glyph-lower-case-d font))
    (add-glyph #\e glyphs (make-glyph-lower-case-e font))
    (add-glyph #\f glyphs (make-glyph-lower-case-f font))
    (add-glyph #\g glyphs (make-glyph-lower-case-g font))
    (add-glyph #\h glyphs (make-glyph-lower-case-h font))
    (add-glyph #\i glyphs (make-glyph-lower-case-i font))
    (add-glyph #\j glyphs (make-glyph-lower-case-j font))
    (add-glyph #\k glyphs (make-glyph-lower-case-k font))
    (add-glyph #\l glyphs (make-glyph-lower-case-l font))
    (add-glyph #\m glyphs (make-glyph-lower-case-m font))
    (add-glyph #\n glyphs (make-glyph-lower-case-n font))
    (add-glyph #\o glyphs (make-glyph-lower-case-o font))
    (add-glyph #\p glyphs (make-glyph-lower-case-p font))
    (add-glyph #\q glyphs (make-glyph-lower-case-q font))
    (add-glyph #\r glyphs (make-glyph-lower-case-r font))
    (add-glyph #\s glyphs (make-glyph-lower-case-s font))
    (add-glyph #\t glyphs (make-glyph-lower-case-t font))
    (add-glyph #\u glyphs (make-glyph-lower-case-u font))
    (add-glyph #\v glyphs (make-glyph-lower-case-v font))
    (add-glyph #\w glyphs (make-glyph-lower-case-w font))
    (add-glyph #\x glyphs (make-glyph-lower-case-x font))
    (add-glyph #\y glyphs (make-glyph-lower-case-y font))
    (add-glyph #\z glyphs (make-glyph-lower-case-z font))
    (add-glyph #\A glyphs (make-glyph-upper-case-a font))
    (add-glyph #\B glyphs (make-glyph-upper-case-b font))
    (add-glyph #\C glyphs (make-glyph-upper-case-c font))
    (add-glyph #\D glyphs (make-glyph-upper-case-d font))
    (add-glyph #\E glyphs (make-glyph-upper-case-e font))
    (add-glyph #\F glyphs (make-glyph-upper-case-f font))
    (add-glyph #\G glyphs (make-glyph-upper-case-g font))
    (add-glyph #\H glyphs (make-glyph-upper-case-h font))
    (add-glyph #\I glyphs (make-glyph-upper-case-i font))
    (add-glyph #\J glyphs (make-glyph-upper-case-j font))
    (add-glyph #\K glyphs (make-glyph-upper-case-k font))
    (add-glyph #\L glyphs (make-glyph-upper-case-l font))
    (add-glyph #\M glyphs (make-glyph-upper-case-m font))
    (add-glyph #\N glyphs (make-glyph-upper-case-n font))
    (add-glyph #\O glyphs (make-glyph-upper-case-o font))
    (add-glyph #\P glyphs (make-glyph-upper-case-p font))
    (add-glyph #\Q glyphs (make-glyph-upper-case-q font))
    (add-glyph #\R glyphs (make-glyph-upper-case-r font))
    (add-glyph #\S glyphs (make-glyph-upper-case-s font))
    (add-glyph #\T glyphs (make-glyph-upper-case-t font))
    (add-glyph #\U glyphs (make-glyph-upper-case-u font))
    (add-glyph #\V glyphs (make-glyph-upper-case-v font))
    (add-glyph #\W glyphs (make-glyph-upper-case-w font))
    (add-glyph #\X glyphs (make-glyph-upper-case-x font))
    (add-glyph #\Y glyphs (make-glyph-upper-case-y font))
    (add-glyph #\Z glyphs (make-glyph-upper-case-z font))
    (add-glyph #\! glyphs (make-glyph-exclamation-mark font))
    (add-glyph #\" glyphs (make-glyph-double-quote font))
    (add-glyph #\# glyphs (make-glyph-hash font))
    (add-glyph #\. glyphs (make-glyph-period font))
    (add-glyph #\, glyphs (make-glyph-comma font))
    (add-glyph #\: glyphs (make-glyph-colon font))
    (add-glyph #\; glyphs (make-glyph-semicolon font))
    (add-glyph #\? glyphs (make-glyph-question-mark font))
    (add-glyph #\/ glyphs (make-glyph-slash font))
    (add-glyph #\1 glyphs (make-glyph-digit-1 font))
    (add-glyph #\2 glyphs (make-glyph-digit-2 font))
    (add-glyph #\3 glyphs (make-glyph-digit-3 font))
    (add-glyph #\4 glyphs (make-glyph-digit-4 font))
    (add-glyph #\5 glyphs (make-glyph-digit-5 font))
    (add-glyph #\6 glyphs (make-glyph-digit-6 font))
    (add-glyph #\7 glyphs (make-glyph-digit-7 font))
    (add-glyph #\8 glyphs (make-glyph-digit-8 font))
    (add-glyph #\9 glyphs (make-glyph-digit-9 font))
    (add-glyph #\0 glyphs (make-glyph-digit-0 font))
    (add-glyph #\[ glyphs (make-glyph-left-bracket font))
    (add-glyph #\\ glyphs (make-glyph-backslash font))
    (add-glyph #\] glyphs (make-glyph-right-bracket font))
    (add-glyph #\{ glyphs (make-glyph-left-brace font))
    (add-glyph #\} glyphs (make-glyph-right-brace font))
    font))

(defun find-glyph (font char)
  (or (gethash char (glyphs font))
      (gethash 'default (glyphs font))))

(defun kerning (font left-glyph right-glyph)
  (or (gethash (cons (right-shape left-glyph)
		     (left-shape right-glyph))
	       (kerning-info font))
      0))

(defun draw-text (text font x y glyph-mask-fun)
  (flet ((show-glyph (glyph x y)
	   (let ((mask (mask glyph))
		 (x-offset (x-offset glyph))
		 (y-offset (y-offset glyph)))
	     (funcall glyph-mask-fun mask (+ x x-offset) (+ y y-offset)))))
    (show-glyph (find-glyph font (aref text 0)) x y)
    (loop for i from 1 below (length text)
	  do (let* ((previous-glyph (find-glyph font (aref text (1- i))))
		    (this-glyph (find-glyph font (aref text i)))
		    (advance (if (null previous-glyph)
				 (width font)
				 (+ (x-offset previous-glyph)
				    (array-dimension (mask previous-glyph) 1)
				    (* 2 (stroke-width font)))))
		    (kerning (if (or (null previous-glyph)
				     (null this-glyph))
				 0
				 (kerning font previous-glyph this-glyph))))
	       (incf x (- advance kerning))
	       (unless (null this-glyph)
		 (show-glyph this-glyph x y))))))
