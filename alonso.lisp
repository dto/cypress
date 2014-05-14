(in-package :cypress)

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
