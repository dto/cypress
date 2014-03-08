(in-package :cypress)

(defun make-quest (&optional (terrain-class 'meadow))
  (let ((geoffrey (new 'geoffrey))
	(buffer (new terrain-class)))
    (setf *map-row* 0)
    (setf *map-column* 0)
    (setf *map-screen* nil)
    (with-buffer buffer
      (drop-object buffer (new 'lucius) 
		   (- (field-value :width buffer)
		      (units 8))
		   (- (field-value :height buffer)
		      (units 8)))
      (let ((scroll (new 'scroll)))
	(drop-object buffer scroll (units 10) (units 4))
	(activate scroll))
      (current-buffer))))

