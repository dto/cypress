(in-package :cypress)

(defparameter *alonso-letter* 
"Arturo,

It is a cruel fate that befalls me. For
my illness, and my guilt, have taught me
the value of mercy just when they also
robbed all hope from me. I wished to
forget our troubles, and travel to see
you once again. But my isolation has
made me weak. Indeed I am too sick to
hunt for food, and will surely die soon.

My final hope is that you will someday
venture to find me, and to discover,
from this letter, that you are forgiven.

I confess now, I feigned my injury, and
thus forced upon you the decision
whether to cross the Abyss. I wanted to
find Amalia just as badly, but was too
cowardly to volunteer. I cannot blame
you, then, for the death of Alfonse.
That burden lies on these bones, not
yours.

I leave also my Ancient key, in the hope
that you will someday combine it with
your own and travel through the Gates.
May you discover the answers you seek,
where the Ancients laid them down inside
the mountain.

 --- Alonso")

(defparameter *bird-notes* 
"June 1st: Saw several gorgeous
blue-jays chasing one another around the
yard.

June 3rd: Today there was a fearsome row
among the Chickadees! Some animal must
have disturbed their nest in the big
pine tree. One of the baby birds fell
out and died; I gave him a tiny little
burial out behind the house.

June 5th: I awakened in the night to
make water, and heard a strange,
wavering bird-song. I've never heard
such a thing before in my life! For a
time I wondered if a traveler had come
through the forest playing a flute, or
whether a real beast made such a sound!

June 18th: I can't find him. I know that
he has some sort of message. Each time
the song leads me farther East. Does
that mean he is allied with the Sun? It
could be an illusion.
")

(defthing (alonso-basement scene)
  :darkness-image "darkness.png"
  :background-image (random-choose *basement-images*))

(defmethod begin-scene :after ((scene alonso-basement))
  (mark-traversed scene)
  (cue-music scene (random-choose '("ruins.ogg" "believe-me2.ogg")))
  (resize-to-background-image scene))

(defthing alonso-corpse 
  :activated nil
  :description "Alonso Pentaquin"
  :image (random-choose *corpse-images*) 
  :tags '(:fixed)
  :stacking nil)

(defthing (alonso-letter scroll)
  :text *alonso-letter*
  :description "Letter to Arturo")

(defmethod can-accept ((alonso alonso-corpse)) t)

(defmethod activate ((alonso alonso-corpse))
  (add-journal-entry *after-alonso*)
  (with-fields (activated) alonso
    (when (not activated)
      (setf activated t)
      (set-objective "Return to Nothbehem with news for Arturo.")
      (add-inventory-item alonso (new 'copper-gear))
      (add-inventory-item alonso (make-scroll "Journal entries" *bird-notes*))
      (add-inventory-item alonso (new 'alonso-letter)))
    (replace-gump alonso (new 'browser :container alonso))))

(defmethod make-terrain ((scene alonso-basement))
  (with-border (units 12)
    (stacked-up (with-border (units 4) (singleton (new 'crumbling-stairwell)))
		(lined-up (spray '(bone-dust bone-dust nightshade) :trim t :count 4)
			  (singleton (new 'ruin-wall))
			  (singleton (new 'alonso-corpse)))
		(lined-up (singleton (new 'ruin-wall))
			  (singleton (new 'cryptghast)) 
			  (singleton (new 'cryptghast))))))
  
(defthing alonso-stairwell 
  :tags '(:fixed) 
  :scale 0.8
  :image (random-choose *gray-stairwell-images*) 
  :basement nil)

(defmethod run :after ((self alonso-stairwell))
  (bring-to-front self))

(defmethod activate ((self alonso-stairwell))
  (narrate "You descend the stairs and enter a moldering basement.")
  (with-fields (basement) self
    (when (null basement)
      (setf basement (new 'alonso-basement)))
    (save-excursion)
    (load-scene basement)))

(defmethod starting-x ((self alonso-basement) dir)
  (units 8))

(defmethod starting-y ((self alonso-basement) dir)
  (units 8))

(defthing alonso-pentaquin-house 
  :description "ruined cottage"
  :scale 2.0
  :tags '(:fixed)
  :image "ruin-1.png")

(defun alonso-pentaquin-house ()
  (combine (singleton (new 'alonso-pentaquin-house))
	   (with-border (units 8) (singleton (new 'alonso-stairwell)))))

(defthing (alonso-ruins scene)
  :background-image "paynes-meadow.png")

(defparameter *alonso-hint*
"This place appears to be a ruined
settlement. Could it be Alonso's?")

(defmethod begin-scene :after ((scene alonso-ruins))
  (mark-traversed scene)
  (add-journal-entry *before-alonso*)
  (show-hint *alonso-hint*)
  (percent-of-time 50 (cue-music scene (random-choose '("passageway.ogg" "lutey.ogg" "dusk.ogg")))))

(defmethod find-description ((scene alonso-ruins)) "forest")

(defmethod map-icon ((scene alonso-ruins))
  (random-choose *forest-icons*))
  
(defmethod make-terrain ((scene alonso-ruins))
  (with-border (units 10)
    (lined-up-randomly
     (stacked-up-randomly (singleton (new 'ruin-wall)) (alonso-pentaquin-house) (singleton (new 'well)) (dead-trees))
     (stacked-up-randomly (lone-wraith) (lone-wraith) (singleton (new 'ruin-wall)) (lone-wraith) (dead-trees)))))

;; - 3 wiz0rds
;; - dead trees
;; - tents with silverwool clothing for quest
