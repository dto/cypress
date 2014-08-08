(in-package :cypress)

(defresource "chimes-1.wav" :volume 10)
(defresource "chimes-2.wav" :volume 10)
(defresource "chimes-3.wav" :volume 10)

(defun magical-flourish ()
  (play-sample (random-choose '("chimes-1.wav" "chimes-2.wav" "chimes-3.wav"))))

;;; Basic spell definitions

(defthing spell reagents)

(defparameter *magic-hint* 
"Your magic power is low.
Try camping to restore MP.
Or eat some Snowdrops.")

(defmethod use ((caster thing) (spell spell))
  (with-fields (reagents) spell
    (if (have-reagents caster reagents)
	(progn 
	  (expend-reagents caster reagents)
	  (cast caster spell))
	(progn
	  (show-error caster (window-pointer-x) (window-pointer-y))
	  (when (< (field-value :magic caster) 10)
	    (show-hint *magic-hint*))
	  (narrate "You don't have enough ingredients to cast ~A"
		   (find-description spell))))))

(defmethod cast ((caster thing) (spell spell)))

(defmethod cast :after ((monk monk) (spell spell))
  (let ((browser (get-gump monk)))
    (when browser (refresh browser)))
  (magical-flourish)
  (resume)
  (begin-animation monk (casting-animation monk)))

(defmethod can-pick ((spell spell)) nil)

;;; Seance spell to get lore from skulls

(defthing (seance spell)
  :description "Seance (8 mp, 1 forget-me-not, skull catalyst)"
  :reagents '(:magic 8 forget-me-not 1)
  :image "danger-1.png")

(defmethod cast ((caster thing) (spell seance)) nil)

(defmethod use :around ((caster thing) (spell seance))
  (let ((skull (or (find-inventory-item caster 'roberto-skull)
		   (find-inventory-item caster 'skull))))
    (if skull
	(multiple-value-bind (x y)
	    (gump-cascade-position)
	  (drop-object (current-buffer) 
		       (new 'scroll-gump :text (find-lore skull))
		       x y)
	  (call-next-method caster spell)
	  (destroy skull))
	(show-hint "You need a skull to conduct a seance."))))

;;; Spark spell to light fire 

(defthing (spark spell)
  :description "Ignite (3 mp, 3 wood)"
  :reagents '(:magic 3 wood 3)
  :image "spark.png")

(defmethod use :around ((caster thing) (spell spark))
  (if (nearby-enemies-p)
      (narrate "You cannot safely make camp when enemies are near!") 
      (let ((camp (find-camp)))
	(if (not camp)
	    (narrate "You haven't made camp yet.")
	    (if (camped (current-scene))
		(narrate "You can't camp here again without traveling first.")
		(call-next-method caster spell))))))
  
(defmethod cast ((caster thing) (spell spark))
  (ignite (find-camp)))

;;; Cure light wounds

(defthing (cure spell)
  :description "Cure light wounds (25 mp, 1 ginseng)"
  :reagents '(:magic 25 ginseng 1)
  :image "fancy-heart.png")

(defmethod cast ((caster thing) (spell cure))
  (modify-health caster (random-choose '(15 20 20 25)))
  (cancel-bleeding caster)
  (narrate "Some of your wounds have been healed. You feel better."))

;;; Craft wooden arrows
      
(defthing (craft-wooden-arrows spell)
  :description "Craft arrows (5 mp, 2 wood, 1 stone)"
  :reagents '(:magic 5 wood 2 stone 1)
  :image "craft-arrows.png")
  
(defmethod cast ((caster thing) (spell craft-wooden-arrows))
  (add-inventory-item caster (quantity-of 'wooden-arrow 12))
  (narrate "You crafted 12 wooden arrows."))

;;; Craft silver arrows
      
(defthing (craft-silver-arrows spell)
  :description "Craft silver arrows (8 mp, 2 silverwood, 1 stone)"
  :reagents '(:magic 10 silverwood 2 stone 1)
  :image "craft-silver-arrows.png")
  
(defmethod cast ((caster thing) (spell craft-silver-arrows))
  (add-inventory-item caster (quantity-of 'silver-arrow 6))
  (narrate "You crafted 6 silver arrows."))

(defparameter *silver-arrow-hint*
"Double-click silver arrows
in inventory to equip them.")

(defmethod cast :after ((caster thing) (spell craft-silver-arrows))
  (show-hint *silver-arrow-hint*))

;;; Travel

(defthing (travel spell)
  :description "Travel (3 magic, some hunger)"
  :reagents nil
  :image "mountain-5.png")

(defmethod use :around ((caster thing) (spell travel))
  (if (nearby-enemies-p)
      (bark (geoffrey) "Not with enemies nearby!")
      (progn 
	(when (eq :here *travel-direction*)
	  (mark-traversed (current-scene)))
	(if (not (traversed (current-scene)))
	    (bark (geoffrey) (format nil "I need to travel farther ~A first." (string-downcase (symbol-name (compass-direction *travel-direction*)))))
	    (call-next-method caster spell)))))

(defmethod cast ((caster thing) (spell travel))
  (when (find-camp)
    (return-to-geoffrey (find-camp)))
  (switch-to-map))

;;; Cure meat

(defthing (cure-meat spell)
  :description "Cure meat (8 mp, 2 thornweed, 1 corpse)"
  :image "cure.png"
  :reagents '(:magic 8 thornweed 2 wolf-corpse 1))

(defmethod cast ((caster thing) (spell cure-meat))
  (add-inventory-item (geoffrey) (quantity-of 'jerky 2))
  (narrate "You cured enough meat for two meals."))

;;; Hold creature

(defthing (hold-creature spell)
  :description "Hold creature (10 mp, 2 nightshade)"
  :image "hold-creature.png"
  :reagents '(:magic 10 nightshade 2))

(defmethod use :around ((caster thing) (spell hold-creature))
  (if (nearby-enemies-p)
      (call-next-method caster spell)
      (narrate "No enemies are near enough to Hold!")))

(defmethod cast ((caster thing) (spell hold-creature))
  (when (nearby-enemies-p)
    (let ((enemies (find-enemies)))
      (labels ((distance-to-geoffrey (x)
		 (distance-between x (geoffrey))))
	(setf enemies (sort enemies #'< :key #'distance-to-geoffrey))
	(let ((target (first enemies)))
	  (add-stasis target (random-choose '(12 14 17))))
	(narrate "The enemy has fallen into stasis!")))))

;;; Translation

(defthing (translation spell)
  :description "Translation (10 mp, 2 nightshade)"
  :image "mind.png"
  :reagents '(:magic 10 nightshade 2))

(defmethod cast ((caster thing) (spell translation))
  (narrate "You sense the ability to read ancient languages.")
  (enable-translation (geoffrey)))

;;; Spellbook 

(defthing spellbook :image "notebook-2.png")

(defmethod find-description ((book spellbook))
  "Geoffrey's spellbook")

(defmethod initialize ((book spellbook) &key)
  (dolist (spell (list (new 'spark)
		       (new 'cure)
		       (new 'craft-wooden-arrows)))
    (add-inventory-item book spell)))

(defmethod can-pick ((book spellbook))
  (not (field-value :container book)))

(defmethod can-accept ((book spellbook)) nil)
(defmethod will-accept ((book spellbook) (thing thing)) nil)
(defmethod will-accept ((book spellbook) (spell spell)) t)

(defmethod add-spell ((spellbook spellbook) (spell spell))
  (unless (find-inventory-item spellbook (class-name (class-of spell)))
    (add-inventory-item spellbook spell)))

(defmethod activate ((spellbook spellbook))
  (show-hint 
"Click a spell to show its name.
Double-click the mountain symbol to cast
the Travel spell, or press M.")
  (replace-gump spellbook (new 'browser :container spellbook)))

(defmethod activate :after ((book spellbook))
  (when (nearby-enemies-p)
    (pause)))

(defun find-spellbook ()
  (find-inventory-item (geoffrey) 'spellbook))
