(in-package :cypress)

(defthing (alistair monk) 
  :met-player nil
  :next-target nil
  :description "Alistair")

(defparameter *alistair-walk* 
  '(:repeat t
    :scale 980
    :frames (("alistair-walk-1.png" 4)
	     ("alistair-walk-2.png" 4)
	     ("alistair-walk-3.png" 4)
	     ("alistair-walk-4.png" 4))))

(defmethod walking-animation ((self alistair))
  *alistair-walk*)

(defparameter *alistair-stand*
  '(:scale 980
    :frames (("alistair-stand-1.png" 19)
	     ("alistair-stand-2.png" 16)
	     ("alistair-stand-3.png" 24))))

(defmethod standing-animation ((self alistair))
  *alistair-stand*)

(defparameter *alistair-cast*
  '(:scale 980
    :frames (("alistair-stand-1.png" 19)
	     ("alistair-stand-2.png" 16)
	     ("alistair-stand-3.png" 24))))

(defmethod casting-animation ((self alistair))
  *alistair-cast*)

(defmethod choose-target ((self alistair))
  (setf (field-value :next-target self)
	(let ((targets (find-instances (current-scene) 'bone-dust)))
	  (when targets (random-choose targets)))))

(defmethod run ((self alistair))
  (with-fields (next-target met-player gump waypoints) self
    (call-next-method)
    (choose-target self)
    (let ((distance (distance-to-cursor self)))
      (cond 
	((and (< distance 220) (> distance 200))
	 (walk-to-thing self (geoffrey)))
	((or gump (<= distance 200))
	 (setf waypoints nil))
	(t (percent-of-time 4 (walk-to-thing self next-target)))))))

