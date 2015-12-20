(in-package :cypress)

(defparameter *house-images* (image-set "house" 3))

(defthing house
  :scale 1.4
  :tags '(:solid :fixed)
  :image (random-choose *house-images*))

(define (nothbehem scene)
  :background-image "stone-road.png"
  :cold 0)

(defparameter *ask-lucius-hint*
  "Talk to Lucius for help on finding
his grandfather, Arturo.")

(defmethod begin-scene :after ((scene nothbehem))
  (set-objective "Find Arturo.")
  (add-journal-entry *arrival-in-nothbehem*)
  (cue-music scene (random-choose '("drum.ogg" "dusk.ogg"))))

(define (pentaquin-house house)
  :scale 1.4
  :locked t
  :description "Arturo Pentaquin's house"
  :image "pentaquin-house.png")

(defmethod unlock ((house pentaquin-house))
  (when (field-value :locked house)
    (set-objective "Visit Arturo Pentaquin's house for supplies.")
    (setf (field-value :locked house) nil)
    (let ((bag (new 'bag)))
      (add-inventory-item bag  (quantity-of 'ginseng 2))
      (add-inventory-item bag (quantity-of 'silver-elixir 2))
      (add-inventory-item bag (quantity-of 'thornweed 2))
      (add-inventory-item bag (quantity-of 'wheat-bread 2))
      (add-inventory-item house bag) 
      (add-inventory-item house (new 'leather-leggings))
      (add-inventory-item house (new 'leather-shirt))
      (add-inventory-item house (tome-of 'craft-silver-arrows))
      (add-inventory-item house (tome-of 'cure-meat))
      (add-inventory-item house (make-scroll "Expedition report" *amalia-report* *after-amalia-report*))
      (add-inventory-item house (new 'sextant)))))

(defmethod activate ((house pentaquin-house))
  (if (field-value :locked house)
      (narrate "You don't have permission to enter Arturo's house.")
      (progn 
	(add-thought *before-woods*)
	(replace-gump house (new 'browser :container house))
	(set-objective "Search the northwestern woods for Alonso."))))  

(defun random-house ()
  (with-border (units 6) (singleton (new 'house))))

(defun pentaquin-house ()
  (with-border (units 10) (singleton (new 'pentaquin-house))))

(defun find-pentaquin-house ()
  (first (find-instances (current-scene) 'pentaquin-house)))

(defun nothbehem-p ()
  (typep (current-scene) (find-class 'nothbehem)))

(defmethod make-terrain ((self nothbehem))
  (with-border (units 12)
    (lined-up-randomly
     (stacked-up-randomly (flowers) (some-trees) (random-house) (wood-pile))
     (stacked-up-randomly (random-house) (singleton (new 'arturo))
			  (some-trees) (flowers) (wood-pile))
     (stacked-up-randomly (random-house) (dead-trees) 
			  (stacked-up-randomly
			   (pentaquin-house)
			   (singleton (new 'maxwell))
			   (with-border (units 4) (singleton (new 'stone-patio)))
			   (singleton (new 'silverwood))
			   (lined-up-randomly
			    (spray 'ruin-wall :trim t :count 2)
			    (spray 'silverwood :trim t :count 2)
			    (singleton (new 'ruin-wall))
			   (with-border (units 4) (singleton (new 'stone-patio)))
			    (stacked-up (singleton (new 'well))
					(with-border (units 4) (singleton (new 'waystone))))
			    (spray 'silverwood :trim t :count 2))
			   (with-border (units 4) (singleton (new 'stone-patio)))
			   (with-border (units 3) (lined-up (singleton (new 'madeline))
							    (singleton (new 'maribel))))
			   (flowers))))))
