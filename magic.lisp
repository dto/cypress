(in-package :cypress)

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
  (begin-animation monk (casting-animation monk)))

(defmethod can-pick ((spell spell)) nil)

;;; Spark

(defthing (spark spell)
  :description "Spark (5 mp)"
  :reagents '(:magic 5)
  :image "spark.png")

(defmethod cast ((caster thing) (spell spark))
  (message "Nothing happens."))

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
  (equip caster (find-arrow caster))
  (narrate-now "You crafted 12 wooden arrows."))

;;; Spellbook 

(defthing spellbook :image "spellbook.png")

(defmethod find-description ((book spellbook))
  "Geoffrey's spellbook")

(defmethod initialize ((book spellbook) &key)
  (dolist (spell (list (new 'spark)
		       (new 'cure)
		       (new 'craft-wooden-arrows)))
    (add-inventory-item book spell)))

(defmethod can-pick ((book spellbook))
  (not (field-value :container book)))

(defmethod can-accept ((book spellbook)) t)
(defmethod will-accept ((book spellbook) (thing thing)) nil)
(defmethod will-accept ((book spellbook) (spell spell)) t)

(defmethod activate ((spellbook spellbook))
  (replace-gump spellbook (new 'browser :container spellbook)))


