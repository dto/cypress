(in-package :cypress)

;;; As the monk Geoffrey, the player drives the action

(defvar *geoffrey* nil)

(defun geoffrey () *geoffrey*)
  
(defthing (geoffrey monk) :description "Geoffrey")

(defmethod eat :after ((monk geoffrey) (food food))
  (narrate "Very good! You feel better."))

(defmethod alternate-tap ((self geoffrey) x y)
  (replace-gump self (new 'scroll-gump :text (status-text))))

(defmethod initialize :after ((monk geoffrey) &key)
  (setf *geoffrey* monk)
  (add-inventory-item monk (new 'spellbook))
  (add-inventory-item monk (new 'camp))
  (add-inventory-item monk (new 'bag))
  (add-inventory-item monk (quantity-of 'ginseng 2))
  (add-inventory-item monk (quantity-of 'stone 2))
  (add-inventory-item monk (quantity-of 'white-bread 2))
  (add-inventory-item monk (quantity-of 'wooden-arrow 16))
  (equip monk (find-arrow monk)))

(defmethod humanp ((monk geoffrey)) t)

(defmethod activate ((monk geoffrey))
  (resume)
  (replace-gump monk (new 'browser :container monk)))

(defmethod collide :after ((monk geoffrey) (gump gump))
  (when (field-value :waypoints monk)
    (bring-to-front gump)))

(defmethod modify-health :after ((monk geoffrey) points)
  (with-fields (alive health) monk
    (when alive
      (when (< health 20) 
	(bark monk "I'm dying!")))
    (when (and alive
	       (not (plusp health)))
      (die monk))))

(defmethod modify-cold :after ((monk geoffrey) points)
  (with-fields (cold) monk
    (bark-cold monk)
    (narrate "You feel colder. Currently at ~S percent." cold)))

(defparameter *monk-hide-weapon-time* (seconds->frames 10))

(defmethod standing-animation ((self geoffrey))
  (with-fields (aiming-bow last-fire-time) self
      (if aiming-bow
	  *monk-stand-bow-ready*
	  (if (> *monk-hide-weapon-time* 
		 (- *updates* last-fire-time))
	      *monk-stand-bow*
	      *monk-stand*))))

(defmethod walking-animation ((self geoffrey))
  (with-fields (aiming-bow last-fire-time) self
      (if aiming-bow
	  *monk-walk-bow-ready*
	  (if (> *monk-hide-weapon-time* 
		 (- *updates* last-fire-time))
	      *monk-walk-bow*
	      *monk-walk*))))

(defmethod casting-animation ((self monk)) *monk-cast*)

;;; Geoffrey's magic tent

(defparameter *fire-images* (image-set "fire" 4))

(defthing fire :image (random-choose *fire-images*) :scale 1.1 :tags '(:ethereal))

(defmethod run ((fire fire))
  (percent-of-time 14 (setf (field-value :image fire) (random-choose *fire-images*))))

(defthing camp
  :description "Geoffrey's magic tent"
  :stacking nil
  :fire nil
  :timer nil
  :contained-image "tent-2.png"
  :image "tent-3.png"
  :tags '(:solid))

(defmethod activate ((camp camp))
  (replace-gump camp (new 'browser :container camp)))

(defmethod can-accept ((camp camp)) t)

(defmethod will-accept ((thing thing) (camp camp)) nil)

(defmethod will-accept ((geoffrey geoffrey) (camp camp)) t)

(defmethod ignite ((camp camp))
  (with-fields (fire timer) camp
    (when (not fire)
      (mark-camped (current-scene))
      (setf fire (new 'fire))
      (setf timer (seconds->frames 15))
      (drop camp fire 25 125)
      (bring-to-front fire))))

(defmethod recover ((monk monk))
  (modify-health monk +12)
  (modify-magic monk +50)
  (modify-cold monk -100)
  (narrate "You rest at the campfire, and feel much better."))
  
(defmethod ignite :after ((camp camp))
  (recover (geoffrey)))

(defmethod douse ((camp camp))
  (with-fields (fire timer) camp
    (when fire
      (destroy fire)
      (setf fire nil)
      (setf timer nil))))

(defmethod can-pick ((camp camp)) 
  (not (null (field-value :container camp))))

(defmethod return-to-geoffrey ((camp camp))
  (with-fields (fire) camp
    (remove-object (current-scene) camp)
    (when fire
      (remove-object (current-scene) fire))
    (add-inventory-item (geoffrey) camp)))

(defmethod run ((camp camp))
  (with-fields (fire timer) camp
    (when (and fire timer)
      (decf timer)
      (unless (plusp timer)
	(douse camp)))))

