(in-package :cypress)

(defparameter *copper-gear-images* (image-set "copper-lock" 5))

(defthing copper-gear 
  :scale 0.7
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
      
;;; Ancient caves

(defparameter *ancient-cave-images* (image-set "ancient-cave" 3))

(defthing (cave scene)
  :darkness-image "darkness.png"
  :background-image (random-choose *ancient-cave-images*))

(defmethod make-terrain ((scene cave))
  (percent-of-time 40
  (with-border (units 10)
    (spray '(ruin-wall) :trim nil :count (random-choose '(2 3 4))))))

(defmethod initialize :after ((scene cave) &key)
  (resize-to-background-image scene)
  (with-fields (height width) scene
    (percent-of-time 80
      (dotimes (n (1+ (random 5)))
	(drop-object scene (new 'bone-dust) (random width) (random height))))))

(defthing (eastern-cave cave))
(defthing (southern-cave cave))

;;; Alonso's ruins

(defthing (alonso-ruins scene)
  :background-image "paynes-meadow.png")

(defmethod find-description ((scene alonso-ruins)) "forest")

(defmethod map-icon ((scene alonso-ruins))
  (random-choose *forest-icons*))

(defthing (alonso-basement scene)
  :darkness-image "darkness.png"
  :background-image (random-choose *basement-images*))

(defmethod begin-scene :after ((scene alonso-basement))
  (cue-music scene "ruins.ogg")
  (resize-to-background-image scene))

(defthing alonso-corpse 
  :activated nil
  :description "Alonso Pentaquin"
  :image (random-choose *corpse-images*) 
  :tags '(:fixed)
  :stacking nil)

(defmethod can-accept ((alonso alonso-corpse)) t)

(defmethod activate ((alonso alonso-corpse))
  (with-fields (activated) alonso
    (when (not activated)
      (setf activated t)
      (add-inventory-item alonso (new 'copper-gear))
      (add-inventory-item alonso (make-scroll "Letter from Alonso" *alonso-letter*)))
    (replace-gump alonso (new 'browser :container alonso))))

(defmethod make-terrain ((scene alonso-basement))
  (with-border (units 12)
    (lined-up (spray '(bone-dust bone-dust nightshade) :trim nil :count 4)
	      (singleton (new 'ruin-wall))
	      (singleton (new 'alonso-corpse)))))

(defthing alonso-stairwell 
  :tags '(:fixed) 
  :scale 0.8
  :image (random-choose *gray-stairwell-images*) 
  :basement nil)

(defmethod activate ((self alonso-stairwell))
  (narrate "You descend the stairs and enter a moldering basement.")
  (with-fields (basement) self
    (when (null basement)
      (setf basement (new 'alonso-basement)))
    (switch-to-scene basement)))

(defmethod starting-x ((self alonso-stairwell) dir)
  (units 8))

(defmethod starting-y ((self alonso-stairwell) dir)
  (units 8))

(defthing alonso-pentaquin-house 
  :description "Alonso Pentaquin's house"
  :scale 2.0
  :tags '(:fixed)
  :image "ruin-1.png")

(defparameter *alonso-letter* 
"Arturo,

It is a cruel fate that befalls me. My
illness traps me here. Indeed I am too
sick to hunt for food, and will surely
die soon. 

My final hope is that you will someday
venture to find me, and to discover,
from this letter, that you are forgiven.

 --- Alonso")

(defun alonso-pentaquin-house ()
  (combine (singleton (new 'alonso-pentaquin-house))
	   (with-border (units 8) (singleton (new 'alonso-stairwell)))))
  
(defmethod make-terrain ((scene alonso-ruins))
  (with-border (units 10)
    (lined-up-randomly
     (stacked-up-randomly (singleton (new 'ruin-wall)) (lone-wraith) (alonso-pentaquin-house) (dead-trees))
     (stacked-up-randomly (lone-wraith) (lone-wraith) (singleton (new 'ruin-wall)) (lone-wraith) (dead-trees)))))
    
























