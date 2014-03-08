(in-package :cypress)

(defresource "groar.wav" :volume 20)
(defresource "grak.wav" :volume 20)
(defresource "grunt-1.wav" :volume 20)
(defresource "grunt-2.wav" :volume 20)
(defresource "grunt-3.wav" :volume 20)
(defresource "grunt-4.wav" :volume 20)
(defresource "knock.wav" :volume 20)
(defresource "death.wav" :volume 20)
(defresource "lichscream.wav" :volume 20)
(defresource "lichdeath.wav" :volume 20)
(defresource "lichdie.wav" :volume 20)

(defsprite enemy)

(defmethod can-pick ((enemy enemy)) nil)

(defmethod activate ((enemy enemy))
  (resume)
  (attack (cursor) enemy))

(defmethod modify-health :around ((enemy enemy) points)
  (call-next-method enemy (* points (compute-modifier (geoffrey) :attack))))
		    

;;; Wraiths

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
      (add-inventory-item remains (new (random-choose '(skull wolf-corpse stone item-box))))
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
  (when (< (distance-to-cursor self) 680)
    (unless seen-player
      (play-sample (random-choose '("lichscream.wav" "lichdie.wav")))
      (setf seen-player t))
    (percent-of-time 22 (setf image (random-choose *wraith-images*)))
    (let ((heading0 (heading-to-cursor self)))
      (percent-of-time 30 
	(setf heading heading0))
      (percent-of-time 30
	(percent-of-time 8 (play-sample (random-choose '("grak.wav" "groar.wav"))))
	(move self heading0 6))))))

;;; Grave hags

(defparameter *grave-hag-corpse-image* "grave-hag-corpse.png")
(defparameter *grave-hag-stalk-images* (image-set "grave-hag-stalk" 2))
(defparameter *grave-hag-attack-images* (image-set "grave-hag-attack" 3))

(defthing (grave-hag enemy)
  :seen-player nil
  :image-scale 1800
  :sprite-height 150
  :sprite-width 150
  :tags '(:enemy)
  :health 15
  :image (random-choose *grave-hag-stalk-images*))

(defmethod die ((self grave-hag))
  (let ((remains (new 'remains)))
    (when (percent-of-time 70 t)
      (when (percent-of-time 60 t)
	(add-inventory-item remains (new 'scroll-fragment)))
      (add-inventory-item remains (new (random-choose '(skull wolf-corpse stone item-box))))
      (if (percent-of-time 70 t)
	  (if (percent-of-time 50 t)
	      (add-inventory-item remains (reagent-bag))
	      (add-inventory-item remains (grab-bag)))
	  (add-inventory-item remains (new 'stone))))
    (drop self remains))
  (drop self (new 'skull))
  (play-sound self "death.wav")
  (destroy self))

(defmethod run ((self grave-hag))
  (with-fields (image heading seen-player) self
    (when (< (distance-to-cursor self) 700)
      (unless seen-player
	(play-sample "lichdeath.wav")
	(setf seen-player t))
      (let ((heading0 (heading-to-cursor self)))
	(percent-of-time 30 
	  (setf heading heading0))
	(percent-of-time 30
	  (move self heading0 8)))
      (if (< (distance-to-cursor self) 250)
	  (progn
	    (percent-of-time 8 
	      (play-sample (random-choose '("grunt-1.wav" "grunt-2.wav" "grunt-3.wav" "grunt-4.wav"))))
	    (percent-of-time 15 (setf image (random-choose *grave-hag-attack-images*))))
	  (percent-of-time 12 (setf image (random-choose *grave-hag-stalk-images*)))))))

;;; Wolf

(defparameter *wolf-images* (image-set "wolf" 3))

(defresource "yelp.wav" :volume 20)
(defresource "howl.wav" :volume 20)
(defresource "bark.wav" :volume 20)

(defthing (wolf enemy)
  :seen-player nil
  :image-scale 1500
  :sprite-height 130
  :sprite-width 130
  :tags '(:enemy)
  :health 25
  :image (random-choose *wolf-images*))

(defmethod die ((self wolf))
  (let ((remains (new 'remains)))
    (add-inventory-item remains (new 'wolf-corpse))
    (drop self remains))
  (destroy self))

(defmethod modify-health :after ((wolf wolf) points)
  (play-sound wolf (random-choose '("bark.wav" "yelp.wav"))))

(defmethod run ((self wolf))
  (when (> (distance-to-cursor self) 800) 
    (setf (field-value :waypoints self) nil)
    (setf (field-value :seen-player self) nil))
  (with-fields (image heading seen-player) self
    (percent-of-time 17 (setf image (random-choose *wolf-images*)))
    (when (<= (distance-to-cursor self) 800)
      (unless seen-player
	(with-fields (x y) (cursor)
	  (walk-to self x y)
	  (play-sample "bark.wav")
	  (play-sample "howl.wav")
	  (setf seen-player t)))
      (when seen-player
	(if (< (distance-to-cursor self) 200)
	    (progn 
	      (percent-of-time 3 (play-sample (random-choose '("growl-1.wav" "growl-2.wav"))))
	      (let ((heading0 (heading-to-cursor self)))
		(percent-of-time 25 
		  (setf heading heading0))
		(percent-of-time 80
		  (move self heading0 3.5))))
	    (progn
	      (when (null (field-value :waypoints self))
		(percent-of-time 5
		  (with-fields (x y) (cursor)
		    (walk-to self x y))))
	      (when (movement-heading self)
		(setf (field-value :heading self) (movement-heading self))
		(move self (movement-heading self) 4))))))))


	    
