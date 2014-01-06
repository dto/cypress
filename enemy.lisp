(in-package :cypress)

(defresource "knock.wav" :volume 20)
(defresource "death.wav" :volume 20)
(defresource "lichscream.wav" :volume 20)

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
    (when (percent-of-time 70 t)
      (when (percent-of-time 60 t)
	(add-inventory-item remains (new 'scroll-fragment)))
      (add-inventory-item remains (new (random-choose '(skull wolf-skull stone item-box))))
      (if (percent-of-time 70 t)
	  (if (percent-of-time 50 t)
	      (add-inventory-item remains (reagent-bag))
	      (add-inventory-item remains (grab-bag)))
	  (add-inventory-item remains (new 'stone))))
    (drop self remains))
  (drop self (new 'skull))
  (play-sound self "death.wav")
  (destroy self))

(defmethod run ((self wraith))
  (with-fields (image heading seen-player) self
  (when (< (distance-to-cursor self) 640)
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

;;; Wolf

(defparameter *wolf-images* (image-set "wolf" 3))

(defresource "yelp.wav" :volume 20)
(defresource "howl.wav" :volume 20)
(defresource "bark.wav" :volume 20)
(defresource "yelp.wav" :volume 20)

(defthing (wolf enemy)
  :seen-player nil
  :image-scale 1500
  :sprite-height 130
  :sprite-width 130
  :tags '(:enemy)
  :health 20
  :image (random-choose *wolf-images*))

(defmethod die ((self wolf))
  (let ((remains (new 'remains)))
    (add-inventory-item remains (new 'wolf-skull))
    (percent-of-time 30 (add-inventory-item remains (new 'jerky)))
    (drop self remains))
  (destroy self))

(defmethod modify-health :after ((wolf wolf) points)
  (play-sound wolf (random-choose '("bark.wav" "yelp.wav"))))

(defmethod run ((self wolf))
  (when (> (distance-to-cursor self) 700) 
    (setf (field-value :waypoints self) nil))
  (with-fields (image heading seen-player) self
    (percent-of-time 17 (setf image (random-choose *wolf-images*)))
    (when (< (distance-to-cursor self) 700)
      (unless seen-player
	(with-fields (x y) (cursor)
	  (walk-to self x y)
	  (play-sample "howl.wav")
	  (setf seen-player t)))
      (if (< (distance-to-cursor self) 200)
	  (progn 
	    (percent-of-time 3 (play-sample (random-choose '("growl-1.wav" "growl-2.wav"))))
	    (let ((heading0 (heading-to-cursor self)))
	      (percent-of-time 25 
		(setf heading heading0))
	      (percent-of-time 70
		(move self heading0 2.2))))
	  (when (movement-heading self)
	    (setf (field-value :heading self) (movement-heading self))
	    (move self (movement-heading self) 2.5))))))

	    
