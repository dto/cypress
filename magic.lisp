(in-package :cypress)

(defresource "chimes-1.wav" :volume 20)
(defresource "chimes-2.wav" :volume 20)
(defresource "chimes-3.wav" :volume 20)

;;; Basic spell definitions

(defthing spell reagents)

(defmethod use ((caster thing) (spell spell))
  (with-fields (reagents) spell
    (if (have-reagents caster reagents)
	(progn 
	  (expend-reagents caster reagents)
	  (cast caster spell))
	(progn
	  (show-error caster (window-pointer-x) (window-pointer-y))
	  (narrate-now "You don't have enough ingredients to cast ~A"
		   (find-description spell))))))

(defmethod cast ((caster thing) (spell spell)))

(defmethod cast :after ((monk monk) (spell spell))
  (let ((browser (get-gump monk)))
    (when browser (refresh browser)))
  (play-sample (random-choose '("chimes-1.wav" "chimes-2.wav" "chimes-3.wav")))
  (begin-animation monk (casting-animation monk)))

(defmethod can-pick ((spell spell)) nil)

;;; Spark spell to light fire 

(defthing (spark spell)
  :description "Ignite (3 mp, 3 wood)"
  :reagents '(:magic 3 wood 3)
  :image "spark.png")

(defmethod use :around ((caster thing) (spell spark))
  (if (camped (current-scene))
      (narrate-now "You can't camp here again.")
      (call-next-method caster spell)))

(defmethod cast ((caster thing) (spell spark))
  (let ((camp (find-camp)))
    (if (not camp)
	(narrate-now "You haven't made camp yet.")
	(ignite camp))))

;;; Cure light wounds

(defthing (cure spell)
  :description "Cure light wounds (25 mp, 1 ginseng)"
  :reagents '(:magic 25 ginseng 1)
  :image "fancy-heart.png")

(defmethod cast ((caster thing) (spell cure))
  (modify-fatigue caster -1)
  (modify-health caster (random-choose '(15 20 20 25)))
  (narrate-now "Some of your wounds have been healed. You feel better."))

;;; Craft wooden arrows
      
(defthing (craft-wooden-arrows spell)
  :description "Craft arrows (5 mp, 2 wood, 1 stone)"
  :reagents '(:magic 5 wood 2 stone 1)
  :image "craft-arrows.png")
  
(defmethod cast ((caster thing) (spell craft-wooden-arrows))
  (add-inventory-item caster (quantity-of 'wooden-arrow 12))
  (narrate-now "You crafted 12 wooden arrows."))

;;; Craft silver arrows
      
(defthing (craft-silver-arrows spell)
  :description "Craft silver arrows (8 mp, 2 silverwood, 1 stone)"
  :reagents '(:magic 10 silverwood 2 stone 1)
  :image "craft-silver-arrows.png")
  
(defmethod cast ((caster thing) (spell craft-silver-arrows))
  (add-inventory-item caster (quantity-of 'silver-arrow 6))
  (narrate-now "You crafted 6 silver arrows."))

;;; Travel

(defthing (travel spell)
  :description "Travel (15 fatigue, 12 hunger)"
  :image "mountain-5.png")

(defmethod cast ((caster thing) (spell travel))
  (modify-fatigue caster 15)
  (modify-hunger caster 12)
  (when (find-camp)
    (return-to-geoffrey (find-camp)))
  (at-next-update
    (let ((old-buffer (current-buffer)))
      (remove-object (current-buffer) (geoffrey))
      (switch-to-buffer (ildran)))))

;;; Cure meat

(defthing (cure-meat spell)
  :description "Cure meat (8 mp, 1 thornweed)"
  :image "cure.png"
  :reagents '(:magic 8 thornweed 2))

(defmethod cast ((caster thing) (spell cure-meat))
  (add-inventory-item (geoffrey) (quantity-of 'jerky 2))
  (narrate-now "You cured enough meat for two meals."))

(defmethod use :around ((caster thing) (spell cure-meat))
  (let ((corpse (find-inventory-item (geoffrey) 'wolf-corpse)))
    (if corpse
	(progn
	  (destroy corpse)
	  (call-next-method))
	(narrate-now "You don't have any meat to cure."))))

;;; Spellbook 

(defthing spellbook :image "notebook-2.png")

(defmethod find-description ((book spellbook))
  "Geoffrey's spellbook")

(defmethod initialize ((book spellbook) &key)
  (dolist (spell (list (new 'spark)
		       (new 'travel)
		       (new 'cure)
		       (new 'cure-meat)
		       (new 'craft-wooden-arrows)
		       (new 'craft-silver-arrows)))
    (add-inventory-item book spell)))

(defmethod can-pick ((book spellbook))
  (not (field-value :container book)))

(defmethod can-accept ((book spellbook)) t)
(defmethod will-accept ((book spellbook) (thing thing)) nil)
(defmethod will-accept ((book spellbook) (spell spell)) t)

(defmethod activate ((spellbook spellbook))
  (replace-gump spellbook (new 'browser :container spellbook)))

(defun find-spellbook ()
  (find-inventory-item (geoffrey) 'spellbook))
