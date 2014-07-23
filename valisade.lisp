(in-package :cypress)

;;; Ruin debris

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

;;; Balsalvan boxes with silver books

(defparameter *ancient-stairwell-images* (image-set "ancient-stairwell" 2))
(defparameter *cobble-images* (image-set "cobble" 5))

(defthing cobblestone :image (random-choose *cobble-images*) :scale 1.2 :tags '(:fixed))

(defparameter *small-silver-basement-image* "small-basement-1.png")
(defparameter *large-silver-basement-image* "small-basement-2.png")

(defthing ancient-book
  :stacking nil
  :text *lorem-ipsum*
  :image (random-choose *silver-book-images*)
  :description "Ancient book")

(defmethod activate ((self ancient-book))
  (if (not (can-translate (geoffrey)))
      (bark (geoffrey) "I can't read this language.")
      (replace-gump self (new 'scroll-gump :text (field-value :text self)))))

(defun make-ancient-book (description text)
  (let ((ancient-book (new 'ancient-book)))
    (setf (field-value :description ancient-book) description)
    (setf (field-value :text ancient-book) text)
    ancient-book))

;;; Silver journal with notes about ossuary

(defparameter *silver-journal-text*
"23rd Day, Month of Silver. It feels
good to write on fresh pages. I feel
like working here will focus my thoughts
and help me calm my Spirit. Just the
sight of Mount Pisgah carries my soul to
a lofty place! I am up every day at
Sunrise, simply to watch the light creep
across the mountain's snowy expanse.

25th Day. It's been very quiet.
Yesterday I visited the Western Retreat
for the first time while delivering the
day's letters. I can't imagine a more
tranquil place to rest and recuperate.
I'll probably spend a few hours reading
before bed tonight.

27th Day. One of the other lay Brothers
has been killed! Brother Alfred's
mangled body was discovered just outside
town this morning. The teeth-marks of a
beast are upon him, and the corpse lay
in a state of putrid rotting, despite
being there for only a few hours. The
Mecha-doctor says it must be some kind
of curse, and that the beast who slew
Alfred must be hunted down and
destroyed.

28th Day. I haven't been here long
enough to know Alfred, but it appears he
was loved by the whole community.
Everyone is sullen, and it seems all the
life has gone out of the day's work.
And the possible invasion of the town by
unnamed Beasts now weighs heavily on our
minds. I've been meditating on the white
light of Peace in order to gather my
wits about me and carry on.

30th Day. The funeral of Brother Alfred
took place yesterday. We led a
procession to the Retreat where some
prayers were said, and then we deposited
his ashes in a stone ossuary. I was
surprised to see them using the archaic
style of burial, but apparently Alfred
comes from \"old blood\" and wished to
conform to tradition. 

I've divided my time between chores,
exercise, and contemplating the
temporary nature of things. Just trying
to remain quiet. 

2nd Day, Month of Cypress. They were
going to give me Alfred's spare key, but
now Iggy says he can't find it. I'm
officially a Brother now, I don't see
why this hasn't been done weeks ago. 

3rd Day, Month of Cypress. Awoke to a
sharp boom and a distant rumbling
sound. Everyone came out of doors in
their pajamas to see what the calamity
had been, but nothing was amiss. It was
all we could talk about at dinner.

4th Day. What in the world is happening?
It is snowing some kind of gray
dust! The stone paths outside are
becoming slippery with the stuff.

5th Day. I could barely open my door
this morning, for the ashfall had
intensified overnight. The sun is
virtually hidden by gray clouds. The
Brothers are in a near-panic, and the
superstitious among them are debating
various signs and Prophecies.

6th Day. We are running out of fresh
water. Brother Rabson is nowhere to be
found, either; he went to check up on
the Retreat, and never returned. You
can't go outside without the stuff
getting in your eyes and mouth and ears.

7th Day. Can't get out. Everything is
buried. I will try to meditate and
collect my thoughts. 
") 

(defthing (journal ancient-book)
  :description "Silver journal"
  :text *silver-journal-text*)

;;; Monk keys

(defthing warrior-key :image "warrior-key.png")
(defthing triangle-key :image "triangle-key.png")

;;; The outpost basement 

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
	      (singleton (new 'triangle-key))
	      (singleton (new 'corpse))
	      (singleton (new 'silver-book)))))

;;; Warrior sigil gateway into outpost

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
		 (setf scene (new 'outpost-basement)))
	       (save-excursion)
	       (switch-to-scene scene)))))

;;; Valisade basement

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

;;; Triangle sigil into valisade

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
		 (setf scene (new 'valisade-basement)))
	       (save-excursion)
	       (switch-to-scene scene)))))

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
     (with-border (units 8) (singleton (new 'triangle-sigil)))
     (spatter '(forget-me-not snowdrop snowdrop) :trim t :count 8)
     (spray 'cobblestone :trim t :count 7))
    (spray '(ancient-road-debris ancient-road ancient-road cobblestone)
	   :trim t
	   :count 14)))))

;;; Northern outpost ruin

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
	(dead-trees) (singleton (new 'small-ruin)) (singleton (new 'warrior-sigil)))
       (spray '(ruin-wall ancient-road-debris dead-tree ancient-road cobblestone) 
	      :trim t
	      :count 12)))))

;;; Retreat with ossuary

(defthing ossuary
  :tags '(:solid :fixed) 
  :image "coverstone.png"
  :scale 1.2
  :scene nil
  :open nil)

(defmethod find-description ((self ossuary))
  (if (field-value :open self)
      "ossuary"
      "ossuary coverstone"))

(defmethod enter-scene :after ((self ossuary))
  (add-inventory-item self (quantity-of 'bone-dust (+ 2 (random 3)))))

(defparameter *ossuary-hint*
"Double-click opened ossuary to
browse contents.")

(defmethod activate ((self ossuary))
  (mark-traversed (current-scene))
  (with-fields (open image scene) self
    (if (not open)
	(progn
	  (setf open t)
	  (show-hint *ossuary-hint*)
	  (setf image "ossuary.png")
	  (layout self))
	(replace-gump self (new 'browser :container self)))))

(defthing (alfred-ossuary ossuary))

(defmethod enter-scene :after ((self alfred-ossuary))
  (add-inventory-item self (new 'warrior-key)))

(defparameter *retreat-background-image* "paynes-meadow.png")

(defthing (retreat scene)
  :background-image *retreat-background-image*)

(defmethod find-description ((self retreat)) 
  (if (field-value :generated self)
      "western retreat"
      "frozen meadow"))

(defmethod map-icon ((self retreat))
  (if (field-value :generated self)
      "ruins-2.png"
      "frozen-forest-1.png"))

(defmethod begin-scene :after ((self retreat))
  (percent-of-time 30 (cue-music self (random-choose '("passageway.ogg")))))

(defmethod expend-travel-cost ((self retreat))
  (modify-hunger (geoffrey) +6)
  (chill (geoffrey) +32))

(defmethod make-terrain ((self retreat))
    (with-border (units 15)
      (stacked-up-randomly
       (spray '(ruin-wall dead-tree ossuary cobblestone pine-tree) 
	      :trim nil
	      :count 8)
       (lined-up-randomly
	(lined-up-randomly (with-border (units 4) (singleton (new 'ossuary))) (dead-trees) 	(with-border (units 5) (singleton (new 'black-wolf)))
	(with-border (units 5) (singleton (new 'black-wolf)))
  (with-border (units 4) (singleton (new 'alfred-ossuary)))))
       (spray '(ruin-wall dead-tree gray-rock cobblestone) 
	      :trim nil
	      :count 8))))















