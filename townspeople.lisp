(in-package :cypress)

(defthing (maxwell monk) 
  :next-target nil
  :description "Maxwell")

(defparameter *maxwell-walk* 
  '(:repeat t
    :scale 980
    :frames (("man-walk-1.png" 4)
	     ("man-walk-2.png" 4)
	     ("man-walk-3.png" 4)
	     ("man-walk-4.png" 4))))

(defmethod walking-animation ((self maxwell))
  *maxwell-walk*)

(defparameter *maxwell-stand*
  '(:scale 980
    :frames (("man-stand-1.png" 19)
	     ("man-stand-2.png" 16)
	     ("man-stand-3.png" 24))))

(defmethod standing-animation ((self maxwell))
  *maxwell-stand*)

(defmethod choose-target ((self maxwell))
  (setf (field-value :next-target self)
	(let ((targets (find-instances (current-scene) 'ruin-wall)))
	  (when targets (random-choose targets)))))

;; (defmethod return-home ((self maxwell))
;;   (multiple-value-bind (x y) (below (find-pentaquin-house))
;;     (walk-to self x y)))

(defmethod run ((self maxwell))
  (with-fields (next-target met-player gump waypoints) self
    (call-next-method)
    (unless gump (choose-target self))
    (let ((distance (distance-to-cursor self)))
      (cond 
	((> distance 500)
	 (when (and next-target (null waypoints))
	   (percent-of-time 4 (walk-to-thing self next-target))))
	((and (< distance 220) (> distance 200))
	 (show-hint "Double-click Maxwell to talk.")
	 (walk-to-thing self (geoffrey)))
	((or gump (<= distance 200))
	 (setf waypoints nil))))))

