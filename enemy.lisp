(in-package :cypress)

(defresource "knock.wav" :volume 20)

;;; Wraiths

(defsprite enemy)

(defmethod can-pick ((enemy enemy)) nil)

(defmethod activate ((enemy enemy))
  (attack (cursor) enemy))

(defthing (wraith enemy)
  :seen-player nil
  :image-scale 1200
  :sprite-height 130
  :sprite-width 130
  :tags '(:enemy)
  :health 15
  :image (random-choose *wraith-images*))

(defmethod die ((self wraith))
  (let ((remains (new 'remains)))
    (if (percent-of-time 50 t)
	(progn 
	  (add-inventory-item remains
			      (random-choose (list 
					      (new 'warrior-key)
					      (new 'triangle-key))))
	  (add-inventory-item remains (random-choose (list 
						      (new 'xalcium-mail)
						      (new 'copper-lock)
						      (new 'skull)))))
	(progn
	  (add-inventory-item remains
			      (random-choose (list 
					      (new 'xalcium-armor)
					      (new 'xalcium-leggings))))
	  (add-inventory-item remains (new 'wolf-skull))))

    (drop self remains))
  (drop self (new 'skull))
  (play-sound self "lichscream.wav")
  (destroy self))

(defmethod run ((self wraith))
  (with-fields (image heading seen-player) self
  (when (< (distance-to-cursor self) 500)
    (unless seen-player
      (play-sample "lichscream.wav")
      (setf seen-player t))
    (percent-of-time 16 (setf image (random-choose *wraith-images*)))
    (let ((heading0 (heading-to-cursor self)))
      (percent-of-time 13 
	(setf heading heading0))
      (percent-of-time 30
	(percent-of-time 12 (play-sample (random-choose '("growl-1.wav" "growl-2.wav"))))
	(move self heading0 4))))))
