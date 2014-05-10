(in-package :cypress)

(defparameter *copper-gear-images* (image-set "copper-lock" 5))

(defthing copper-gear 
  :image (random-choose *copper-gear-images*))

(defparameter *copper-seal-images* (image-set "copper-seal" 4))

(defthing copper-seal
  :image (random-choose *copper-seal-images*)
  :touching nil
  :tags '(:fixed))

(defmethod collide ((seal copper-seal) (gear copper-gear))
  (setf (field-value :touching seal) t))

(defthing copper-plate 
  :tags '(:fixed) 
  :image (random-choose '("copper-plate-1.png" "copper-plate-2.png")))

(defthing copper-stairwell  :tags '(:fixed) :image (random-choose '("copper-stairwell-1.png" "copper-stairwell-2.png")))

(defparameter *copper-door-closed-images* (image-set "copper-door-closed" 2))
(defparameter *copper-door-opening-images* (image-set "copper-door-opening" 2))
(defparameter *copper-door-open-images* (image-set "copper-door-open" 2))

(defthing copper-door
  :image (random-choose *copper-door-closed-images*)
  :seal nil
  :open nil
  :tags '(:solid :fixed)
  :timer 0)

(defmethod lock ((door copper-door) (seal copper-seal))
  (setf (field-value :seal door) seal))

(defmethod door-image ((door copper-door) n)
  (cond ((> n 90) "copper-door-open-2")
	((> n 80) "copper-door-open-1")
	((> n 60) "copper-door-opening-1")
	((> n 30) "copper-door-opening-2")
	((> n 10) "copper-door-closed-1")
	((t "copper-door-closed-2"))))

(defmethod run ((door copper-door))
  (with-fields (timer open image seal) door
    (when seal
      (with-fields (touching) seal
	(setf timer 
	      (max 0
		   (min (+ timer (if touching 1 -1))
			100)))
	(setf image (door-image door timer))
	(if (plusp timer)
	    (progn (setf open t)
		   (remove-tag door :solid))
	    (progn (setf open nil)
		   (add-tag door :solid)))))))
      
	    
      




