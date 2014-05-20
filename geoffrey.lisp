(in-package :cypress)

;;; As the monk Geoffrey, the player drives the action

(defvar *geoffrey* nil)

(defun geoffrey () *geoffrey*)
  
(defthing (geoffrey monk) :description "Geoffrey")

(defmethod stomach-full-p ((monk monk))
  (<= (field-value :hunger monk) 12))

(defmethod eat :after ((monk geoffrey) (food food))
  (let ((messages (if (stomach-full-p monk)
		      '("I'm full now." "My stomach is full." "I feel full.")
		      '("Very good!" "That's much better." "Delicious!"))))
    (bark monk (random-choose messages))))

(defmethod alternate-tap ((self geoffrey) x y)
  (replace-gump self (new 'scroll-gump :text (status-text))))

(defmethod initialize :after ((monk geoffrey) &key)
  (setf *geoffrey* monk)
  ;; temp
  ;; (add-inventory-item monk (quantity-of 'copper-gear 2))
  ;; (learn-spell (geoffrey) (new 'travel))
  ;; (add-inventory-item monk (new 'bone-flute))
  ;; (add-inventory-item monk (quantity-of 'neume 3))
  ;; 
  (add-inventory-item monk (new 'spellbook))
  (add-inventory-item monk (new 'camp))
  (add-inventory-item monk (new 'bag))
  (add-inventory-item monk (quantity-of 'ginseng 2))
  (add-inventory-item monk (quantity-of 'stone 2))
  (add-inventory-item monk (quantity-of 'white-bread 3))
  (add-inventory-item monk (quantity-of 'wooden-arrow 16))
  (add-inventory-item monk (make-scroll "Summons from Dr. Quine" *quine-summons*))
  (equip monk (find-arrow monk)))

(defmethod equip :after ((monk geoffrey) (self thing))
  (when (not (field-value :equipper self))
    (narrate "You cannot equip this item in its current location.")))

(defmethod eat :after ((monk geoffrey) (snowdrop snowdrop))
  (bark monk (random-choose '("I gained one Magic Point."))))

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
      (or (when (< health 40) 
	    (bark monk "I'm badly hurt!"))
	  (when (< health 20) 
	    (bark monk "I'm dying!"))))
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

(defmethod walk-to :after ((monk geoffrey) x y)
  (with-fields (waypoints) monk
    (when (null waypoints)
      ;; pathfinding failed
      (show-error monk x y)
      (narrate "That destination is obstructed."))))

(defmethod casting-animation ((self monk)) *monk-cast*)

;;; Learning new spells

(defmethod learn-spell ((self geoffrey) (spell spell))
  (add-spell (find-spellbook) spell))

(defmethod learn-spell :after ((self geoffrey) (spell spell))
  (narrate "You learned a new magic spell: ~A" (find-description spell))
  (magical-flourish))

(defthing tome
  :stacking nil
  :image (random-choose *book-images*)
  :spell nil)

(defmethod initialize :after ((tome tome) &key spell)
  (assert spell)
  (setf (field-value :spell tome) spell))

(defmethod activate ((tome tome))
  (learn-spell (geoffrey) (field-value :spell tome))
  (destroy tome))

(defmethod find-description ((tome tome))
  (format nil "Magic tome of ~A" 
	  (find-description (field-value :spell tome))))

(defun tome-of (spell-class)
  (new 'tome :spell (new spell-class)))

;;; Party members

(defmethod enter-scene ((self geoffrey))
  (when (lucius-in-party-p)
    (multiple-value-bind (x y) (left-of self)
      (add-object (current-scene) (lucius) (- x 10) (- y 10)))))

;; (defmethod add-object :after ((scene scene) (geoffrey geoffrey) &optional x y z) nil)

(defmethod exit-scene ((self geoffrey))
  (stop-walking self)
  (when (lucius-in-party-p)
    (exit-scene (lucius)))
  (when (current-scene) 
    (remove-object (current-scene) self)))

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

(defmethod can-pick ((camp camp)) t)
;;  (not (null (field-value :container camp))))

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

 
