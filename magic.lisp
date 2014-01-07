(in-package :cypress)

;;; Actions

(defthing spell reagents)

(defmethod use ((caster thing) (spell spell))
  (with-fields (reagents) spell
    (if (have-reagents caster reagents)
	(progn 
	  (expend-reagents caster reagents)
	  (cast caster spell))
	(show-error caster (window-pointer-x) (window-pointer-y)))))

(defmethod cast ((caster thing) (spell spell)))

(defmethod can-pick ((spell spell)) nil)

(defthing (spark spell)
  :description "Spark (2 mp)"
  :reagents '(:magic 2)
  :image "spark.png")

(defmethod cast ((caster thing) (spell spark))
  (message "Nothing happens."))

(defthing (cure spell)
  :description "Cure light wounds (25 mp, 1 ginseng)"
  :reagents '(:magic 25 ginseng 1)
  :image "fancy-heart.png")

(defmethod cast ((caster thing) (spell cure))
  (modify-fatigue caster -1)
  (modify-health caster (random-choose '(15 20 20 25))))
      
(defthing (craft-wooden-arrows spell)
  :description "Craft arrows (2 mp, 2 wood, 1 stone)"
  :reagents '(:magic 2 wood 2 stone 1)
  :image "craft-arrows.png")
  
(defmethod cast ((caster thing) (spell craft-wooden-arrows))
  (add-inventory-item caster (quantity-of 'wooden-arrow 10)))



