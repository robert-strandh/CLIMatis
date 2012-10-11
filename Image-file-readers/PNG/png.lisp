(in-package #:climatis-png-file-reader)

(defmethod read-image-file-with-type (filename (type (eql :png)))
  (let* ((p (png-read:read-png-file filename))
	 (data (png-read:image-data p))
	 (width (array-dimension data 0))
	 (height (array-dimension data 1))
	 (new-data (make-array (list height width))))
    (loop for x from 0 below (array-dimension new-data 1)
	  do (loop for y from 0 below (array-dimension new-data 0)
		   do (setf (aref new-data y x)
			    (make-instance 'pixel
				 :color (make-instance 'color
						       :red (/ (aref data x y 0) 255d0)
						       :green (/ (aref data x y 1) 255d0)
						       :blue  (/ (aref data x y 2) 255d0))
				 :opacity (/ (aref data x y 3) 255d0)))))
    (make-instance 'image-zone
		   :pos-x 25 :pos-y 5
		   :desired-width 16 :desired-height 16
		   :image-data new-data)))