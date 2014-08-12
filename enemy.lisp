(in-package :cypress)

(defparameter *attack-hint*
"Double-click enemy to attack.
Press Spacebar to pause action, 
or right-click Geoffrey.")

(defresource "creep-1.wav" :volume 10)
(defresource "creep-2.wav" :volume 10)
(defresource "creep-3.wav" :volume 10)
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

;; (defmethod encountered ((enemy enemy)) 
;;   (field-value :encountered enemy))

;; (defmethod end-encounter ((enemy enemy))
;;   (setf (field-value :encountered enemy) nil))

;; (defmethod encounter-hint ((enemy enemy))
;;   (format nil (random-choose '("The ~A is attacking!"
;; 			       "The ~A draws near!"
;; 			       "~A is approaching!"))
;; 	  (find-description enemy)))

;; (defmethod begin-encounter ((enemy enemy))
;;   (setf (field-value :encountered enemy) t)
;;   (pause)
;;   (show-hint (encounter-hint enemy) :force))

;; (defparameter *encounter-distance* 340)

;; (defmethod run :after ((enemy enemy))
;;   (when (and (not (encountered enemy))
;; 	     (< (distance-between enemy (geoffrey))
;; 		*encounter-distance*))
;;     (begin-encounter enemy)))

(defmethod draw :after ((enemy thing))
  (with-fields (stasis) enemy
    (when stasis
      (multiple-value-bind (top left right bottom)
	  (bounding-box enemy)
	(let ((boil (* (random 4) (sin (/ *updates* 3)))))
	  (draw-image "sparkle.png"
		      (- left (/ (- right left) 2))
		      (- top (/ (- bottom top) 2))
		      :height (* 2 (+ boil (- bottom top)))
		      :width (* 2 (+ boil (- right left)))
		      :blend :additive))))))

(defmethod can-pick ((enemy enemy)) nil)

(defmethod activate-maybe ((enemy enemy))
  (activate enemy))

(defmethod activate ((enemy enemy))
  (resume)
  (attack (cursor) enemy))

(defmethod modify-health :around ((enemy enemy) points)
  (call-next-method enemy (* points (compute-modifier (geoffrey) :attack))))

;;; Cryptghasts! 

(defparameter *cryptghast-skull-images* (image-set "cryptghast-skull" 2))
(defparameter *cryptghast-walk-images* (image-set "cryptghast-walk" 3))

(defthing (cryptghast enemy)
  :image-scale 1000
  :sprite-height 130
  :sprite-width 130
  :tags '(:enemy)
  :health 15
  :image (random-choose *cryptghast-walk-images*))

(defmethod die ((self cryptghast))
  (play-sound self "death.wav")
  (destroy self))

(defmethod run ((self cryptghast))
  (let ((distance (distance-to-cursor self)))
    (if (> distance 600)
	(setf (field-value :image self) "cryptghast-skull-1.png")
	(percent-of-time 40
	  (setf (field-value :heading self) (heading-to-cursor self))
	  (forward self 7)
	  (percent-of-time 30 (play-sound self (random-choose '("creep-1.wav" "creep-2.wav" "creep-3.wav"))))
	  (setf (field-value :image self) (random-choose *cryptghast-walk-images*))))))

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
      (add-inventory-item remains (new (random-choose '(skull wolf-corpse stone))))
      (if (percent-of-time 70 t)
	  (if (percent-of-time 50 t)
	      (add-inventory-item remains (reagent-bag))
	      (add-inventory-item remains (grab-bag)))
	  (add-inventory-item remains (new 'stone))))
    (add-inventory-item remains (new 'skull))
    (show-hint "Double-click remains to search.")
    (drop self remains))
  (play-sound self "death.wav")
  (destroy self))

(defmethod run ((self wraith))
  (with-fields (image heading seen-player) self
  (when (< (distance-to-cursor self) 920)
    (unless seen-player
      (play-sample (random-choose '("lichscream.wav" "lichdie.wav")))
      (show-hint *attack-hint*)
      (setf seen-player t))
    (percent-of-time 22 (setf image (random-choose *wraith-images*)))
    (let ((heading0 (heading-to-cursor self)))
      (percent-of-time 30 
	(setf heading heading0))
      (percent-of-time 30
	(percent-of-time 8 (play-sample (random-choose '("grak.wav" "groar.wav"))))
	(move self heading0 8))))))

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
  :health 23
  :image (random-choose *grave-hag-stalk-images*))

(defthing grave-hag-corpse :tags '(:fixed) :image *grave-hag-corpse-image*)

(defmethod can-accept ((grave-hag-corpse grave-hag-corpse)) t)

(defmethod activate ((grave-hag-corpse grave-hag-corpse))
  (replace-gump grave-hag-corpse (new 'browser :container grave-hag-corpse)))

(defmethod die ((self grave-hag))
  (let ((corpse (new 'grave-hag-corpse)))
    (when (percent-of-time 70 t)
      (add-inventory-item corpse (new (random-choose '(skull wolf-corpse stone item-box))))
      (if (percent-of-time 70 t)
	  (if (percent-of-time 50 t)
	      (add-inventory-item corpse (reagent-bag))
	      (add-inventory-item corpse (grab-bag)))
	  (add-inventory-item corpse (quantity-of 'bone-dust (random-choose '(2 3 4))))))
    (drop self corpse))
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
	  (move self heading0 11)))
      (if (< (distance-to-cursor self) 250)
	  (progn
	    (percent-of-time 8 
	      (play-sample (random-choose '("grunt-1.wav" "grunt-2.wav" "grunt-3.wav" "grunt-4.wav"))))
	    (percent-of-time 15 (setf image (random-choose *grave-hag-attack-images*))))
	  (percent-of-time 12 (setf image (random-choose *grave-hag-stalk-images*)))))))

;;; Wolf

(defparameter *wolf-images* (image-set "wolf" 3))

(defresource "yelp.wav" :volume 25)
(defresource "howl.wav" :volume 25)
(defresource "bark.wav" :volume 10)

(defthing (wolf enemy)
  :seen-player nil
  :running-away nil
  :image-scale 1500
  :sprite-height 130
  :sprite-width 130
  :tags '(:enemy)
  :health 25
  :speed 5.2
  :image (random-choose *wolf-images*))

(defmethod die ((self wolf))
  (let ((remains (new 'remains)))
    (add-inventory-item remains (new 'wolf-corpse))
    (show-hint "Double-click remains to harvest corpse.")
    (drop self remains))
  (destroy self))

(defmethod modify-health :after ((wolf wolf) points)
  (play-sound wolf (random-choose '("bark.wav" "yelp.wav"))))

(defmethod random-frame ((wolf wolf))
  (random-choose *wolf-images*))

(defmethod run-away ((self wolf))
  (with-local-fields
    (setf %running-away t)
    (multiple-value-bind (x y) (center-point (current-scene))
      (walk-to self x y))))

(defmethod run :around ((self wolf))
  (with-local-fields
    (if (not %running-away)
	(call-next-method)
	(or (percent-of-time 1 (setf %running-away nil))
	    (when (movement-heading self)
	      (setf (field-value :heading self) (movement-heading self))
	      (move self (movement-heading self) %speed))))))

(defmethod run ((self wolf))
  (when (> (distance-to-cursor self) 1000) 
    (setf (field-value :waypoints self) nil)
    (setf (field-value :seen-player self) nil))
  (with-fields (image heading seen-player speed) self
    (percent-of-time 17 (setf image (random-frame self)))
    (when (<= (distance-to-cursor self) 1000)
      (unless seen-player
	(with-fields (x y) (cursor)
	  (walk-to self x y)
	  (play-sample "bark.wav")
	  (play-sample "howl.wav")
	  (show-hint *attack-hint*)
	  (setf seen-player t)))
      (when seen-player
	(if (< (distance-to-cursor self) 200)
	    (progn 
	      (percent-of-time 3 (play-sample (random-choose '("growl-1.wav" "growl-2.wav"))))
	      (let ((heading0 (heading-to-cursor self)))
		(percent-of-time 25 
		  (setf heading heading0))
		(percent-of-time 80
		  (move self heading0 speed))))
	    (progn
	      (when (null (field-value :waypoints self))
		(percent-of-time 5
		  (with-fields (x y) (cursor)
		    (walk-to self x y))))
	      (when (movement-heading self)
		(setf (field-value :heading self) (movement-heading self))
		(move self (movement-heading self) speed)))))
      ;; don't camp on player 
      (when (< (distance-to-cursor self) 40)
	(run-away self)))))

;; Blackwolves

(defthing (black-wolf wolf)
  :image-scale 3000
  :description "cursed Dire wolf"
  :tags '(:enemy :cursed)
  :image (random-choose *black-wolf-images*)
  :sprite-height 130
  :sprite-width 130
  :speed 5.6
  :health 45)

(defmethod die ((self black-wolf))
  (let ((remains (new 'remains)))
    (drop self remains)
    (destroy self)))
	    
(defmethod random-frame ((wolf black-wolf))
  (random-choose *black-wolf-images*))
