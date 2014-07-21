(in-package :cypress)

;;; Balsalvan boxes with silver books

(defparameter *ancient-stairwell-images* (image-set "ancient-stairwell" 2))
(defparameter *cobble-images* (image-set "cobble" 5))

(defthing cobblestone :image (random-choose *cobble-images*) :scale 1.2 :tags '(:fixed))

(defparameter *small-silver-basement-image* "small-basement-1.png")
(defparameter *large-silver-basement-image* "small-basement-2.png")

(defthing ancient-book :image (random-choose *silver-book-images*))

(defmethod activate ((book ancient-book))
  (bark (geoffrey) "I can't read this language."))

;;; The basement

(defthing (valisade-basement scene)
  :darkness-image "darkness.png"
  :background-image *large-silver-basement-image*)

(defmethod begin-scene :after ((scene valisade-basement))
  (mark-traversed scene)
  (resize-to-background-image scene))

(defmethod starting-x ((self valisade-basement) dir)
  (units 8))

(defmethod starting-y ((self valisade-basement) dir)
  (units 8))

(defmethod make-terrain ((scene valisade-basement))
  (with-border (units 12)
    (lined-up (with-border (units 3) (singleton (new 'crumbling-stairwell)))
		(lined-up (spray '(ruined-book silver-book) :trim t :count 6) (singleton (new 'bone-flute)))
		(singleton (new 'cryptghast)))))

;;; Warrior sigil gateway into valisade

(defthing warrior-key :image "warrior-key.png")
(defthing triangle-key :image "triangle-key.png")

(defthing warrior-sigil
  :tags '(:solid :fixed) 
  :image "warrior-coverstone.png"
  :scale 1.2
  :scene nil
  :open nil)

(defmethod find-description ((self warrior-sigil))
  (if (field-value :open self)
      "crumbling stairwell"
      "warrior sigil"))

(defmethod activate ((self warrior-sigil))
  (with-fields (open image scene) self
    (if (not open)
	(when (find-inventory-item (geoffrey) 'warrior-key)
	  (setf open t)
	  (narrate "The ancient coverstone opens.")
	  (setf image (random-choose *ancient-stairwell-images*)))
	(progn (when (null scene)
		 (setf scene (new 'valisade-basement)))
	       (save-excursion)
	       (switch-to-scene scene)))))

;;; The 2nd basement 

(defthing (outpost-basement scene)
  :darkness-image "darkness.png"
  :background-image *small-silver-basement-image*)

(defmethod begin-scene :after ((scene outpost-basement))
  (mark-traversed scene)
  (resize-to-background-image scene))

(defmethod starting-x ((self outpost-basement) dir)
  (units 8))

(defmethod starting-y ((self outpost-basement) dir)
  (units 8))

(defmethod make-terrain ((scene outpost-basement))
  (with-border (units 12)
    (lined-up (with-border (units 3) (singleton (new 'crumbling-stairwell)))
	      (singleton (new 'warrior-key))
	      (singleton (new 'silver-book)))))

;;; Triangle sigil into outpost

(defthing triangle-sigil
  :tags '(:solid :fixed) 
  :image "triangle-coverstone.png"
  :scale 1.2
  :scene nil
  :open nil)

(defmethod find-description ((self triangle-sigil))
  (if (field-value :open self)
      "crumbling stairwell"
      "triangle sigil"))

(defmethod activate ((self triangle-sigil))
  (with-fields (open image scene) self
    (if (not open)
	(when (find-inventory-item (geoffrey) 'triangle-key)
	  (setf open t)
	  (narrate "The ancient coverstone opens.")
	  (setf image (random-choose *ancient-stairwell-images*)))
	(progn (when (null scene)
		 (setf scene (new 'outpost-basement)))
	       (save-excursion)
	       (switch-to-scene scene)))))

;;; The ground

(defparameter *ancient-road-images* (image-set "ancient-road" 10))

(defthing ancient-road
  :tags '(:fixed)
  :scale 2
  :description "ancient ruin"
  :image (random-choose *ancient-road-images*))

(defparameter *ancient-road-debris-images* (image-set "ancient-road-debris" 5))

(defthing ancient-road-debris
  :tags '(:fixed)
  :description "ancient ruin"
  :scale 2
  :image (random-choose *ancient-road-debris-images*))

;;; Valisade ruins scene

(defparameter *valisade-background-image* "golden-meadow.png")

(defthing (valisade scene)
  :background-image *valisade-background-image*)

(defmethod find-description ((self valisade)) 
  (if (field-value :generated self)
      "ruined abbey"
      "clearing"))

(defmethod map-icon ((self valisade))
  (if (field-value :generated self)
      "castle-1.png"
      "meadow-1.png"))

(defparameter *ruin-hint*
"You have discovered an
enormous stone ruin.")

(defmethod begin-scene :after ((self valisade))
  (percent-of-time 40 (cue-music self (random-choose '("spiritus.ogg" "kosmium.ogg" "monks.ogg"))))
  (show-hint *ruin-hint*))

(defmethod make-terrain ((self valisade))
  (with-border (units 15)
    (lined-up-randomly
     (stacked-up-randomly (spray '(ruin-wall gray-rock cobblestone ginseng) :trim nil :count 4)
			  (singleton (new 'small-ruin)))

   (stacked-up
    (spray '(ancient-road-debris ancient-road cobblestone ruin-wall cobblestone) 
	   :trim t
	   :count 14)
    (lined-up 
     (spatter '(forget-me-not snowdrop snowdrop) :trim t :count 8)
     (spray 'cobblestone :trim t :count 7)
     (with-border (units 8) (singleton (new 'warrior-sigil)))
     (spatter '(forget-me-not snowdrop snowdrop) :trim t :count 8)
     (spray 'cobblestone :trim t :count 7))
    (spray '(ancient-road-debris ancient-road ancient-road cobblestone)
	   :trim t
	   :count 14)))))

;;; Northern ruins

(defparameter *northern-ruins-background-image* "paynes-meadow.png")

(defthing (northern-ruins scene)
  :background-image *northern-ruins-background-image*)

(defmethod find-description ((self northern-ruins)) 
  (if (field-value :generated self)
      "ruined outpost"
      "frozen clearing"))

(defmethod map-icon ((self northern-ruins))
  (if (field-value :generated self)
      "castle-2.png"
      "frozen-forest-1.png"))

(defparameter *outpost-hint*
"This appears to be a ruined outpost or
settlement.")

(defmethod begin-scene :after ((self northern-ruins))
  (percent-of-time 30 (cue-music self (random-choose '("ancient-fanfare.ogg" "kosmium.ogg" "monks.ogg" "passageway.ogg"))))
  (show-hint *outpost-hint*))

(defmethod expend-travel-cost ((self northern-ruins))
  (modify-hunger (geoffrey) +6)
  (chill (geoffrey) +40))

(defmethod make-terrain ((self northern-ruins))
  (with-border (units 15)
    (with-border (units 12)
      (stacked-up-randomly
       (spray '(ruin-wall ancient-road-debris dead-tree ancient-road cobblestone) 
	      :trim t
	      :count 12)
       (lined-up-randomly
	(dead-trees) (singleton (new 'triangle-sigil)))
       (spray '(ruin-wall ancient-road-debris dead-tree ancient-road cobblestone) 
	      :trim t
	      :count 12)))))















