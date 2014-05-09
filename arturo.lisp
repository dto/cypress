(in-package :cypress)

;;; Arturo Pentaquin

(defthing (arturo monk) 
  :met-player nil
  :next-target nil
  :description "Arturo")

(defparameter *arturo-walk* 
  '(:repeat t
    :scale 920
    :frames (("arturo-walk-1.png" 4)
	     ("arturo-walk-2.png" 4)
	     ("arturo-walk-3.png" 4)
	     ("arturo-walk-4.png" 4))))

(defmethod walking-animation ((self arturo))
  *arturo-walk*)

(defparameter *arturo-stand*
  '(:scale 920
    :frames (("arturo-stand-1.png" 19)
	     ("arturo-stand-2.png" 16)
	     ("arturo-stand-3.png" 24))))

(defmethod standing-animation ((self arturo))
  *arturo-stand*)

(defmethod choose-target ((self arturo))
  (setf (field-value :next-target self)
	(let ((targets (find-instances (current-scene) 'silverwood)))
	  (when targets (random-choose targets)))))

(defmethod return-home ((self arturo))
  (multiple-value-bind (x y) (below (find-pentaquin-house))
    (walk-to self x y)))

(defmethod run ((self arturo))
  (with-fields (next-target met-player gump waypoints) self
    (call-next-method)
    (choose-target self)
    (let ((distance (distance-to-cursor self)))
      (cond 
	;; ((< distance 110)
	;;  (setf waypoints nil))
	;; ((< distance 220)
	;;  (walk-to-thing self (geoffrey)))
	((and next-target (null waypoints))
	     (percent-of-time 4 (walk-to-thing self next-target)))
	((and (null next-target)
	      (null waypoints))
	 (percent-of-time 4
			  (walk-to-thing self (find-pentaquin-house))))))))
	
	   
	  
	  
			    
	
    
