(in-package :cypress)

;;; Hamlet

(defthing (roberto-skull skull) :description "Roberto's skull" :image (random-choose '("skull-1.png" "skull-2.png" "skull-3.png")))

(defparameter *roberto-lore*
"...is someone there? I haven't much time,
I grow weaker each day. 

I'm not bitter, for my sons and
daughters are all grown; Rachael and
Adam have followed in my footsteps as
musicians; Sheldon and little Amalia
have become talented scribes and
illustrators. I leave behind a happy
family; our troubles with the cold
weather will pass soon, I'm sure of it!

I hope they bury me in that lovely
cemetery up Northeast, with something
from my music engraved on the stone in
the grand tradition of olden times;
perhaps another flute player will pass
by my grave, and rouse my Spirit just a
little from its slumber?

If I may be so blessed, I would be the
luckiest man who ever saw the sky of
Ildron.")

(defparameter *burned-letter* 
"My Lord,

My deepest apologies---we have not yet
been able to find the skull of the
Father. It seems that a wraith may have
absconded with the remains. Can you
spare for us another Talisman? We might
double the rate of our search. Keep in
mind that Morbius will carry the
Cylindrophone when we split up the
party, so you should send your
replies to him. 

 *the rest of the scroll is blackened*
")

(defmethod find-lore ((skull roberto-skull)) *roberto-lore*)

(defmethod find-lore :after ((skull roberto-skull))
  (add-thought *after-roberto*))

(defthing (hamlet-basement scene)
  :darkness-image "darkness.png"
  :background-image (random-choose *basement-images*))

(defmethod begin-scene :after ((scene hamlet-basement))
  (mark-traversed scene)
  (cue-music scene (random-choose '("path.ogg" "believe-me2.ogg" "lutey.ogg")))
  (resize-to-background-image scene))

(defmethod starting-x ((self hamlet-basement) dir)
  (units 8))

(defmethod starting-y ((self hamlet-basement) dir)
  (units 8))

;;; Stairs to basement

(defthing hamlet-stairwell 
  :tags '(:fixed) 
  :scale 0.8
  :image (random-choose *gray-stairwell-images*) 
  :basement nil)

(defmethod activate ((self hamlet-stairwell))
  (narrate "You descend the stairs and enter a moldering basement.")
  (with-fields (basement) self
    (when (null basement)
      (setf basement (new 'hamlet-basement)))
    (save-excursion)
    (load-scene basement)))

(defmethod run :after ((self hamlet-stairwell))
  (bring-to-front self))

(defthing (hamlet-chest chest) :description "wooden chest")

(defmethod enter-scene :after ((chest hamlet-chest))
  (when (null (field-value :inventory chest))
    (add-inventory-item chest (tome-of 'seance))
    (add-inventory-item chest (quantity-of 'nightshade 2)) 
    (add-inventory-item chest (make-scroll "partly-burned letter" *burned-letter* *after-morbius-letter*))))

(defmethod activate :after ((chest hamlet-chest)) 
  (add-thought *after-hamlet*))

(defmethod make-terrain ((scene hamlet-basement))
  (with-border (units 12)
    (stacked-up (with-border (units 4) (singleton (new 'crumbling-stairwell)))
		(lined-up-randomly (spray '(corpse bone-dust bone-dust) :trim nil :count 5)
			  (stacked-up (singleton (new 'corpse))
				      (singleton (new 'hamlet-chest))))
		(lined-up-randomly (singleton (new 'ruin-wall))
			  (singleton (new 'cryptghast))))))

(defthing (ruined-hamlet scene)
  :background-image (random-choose *snowy-meadow-images*))

(defmethod begin-scene :after ((scene ruined-hamlet))
  (add-journal-entry *before-hamlet*)
  (percent-of-time 20 (cue-music scene (random-choose '("passageway.ogg" "lutey.ogg" "dusk.ogg")))))

(defthing (roberto-wraith wraith) :description "Wraith")

(defmethod die ((self roberto-wraith))
  (let ((remains (new 'remains)))
    (add-inventory-item remains (new 'roberto-skull))
    (drop self remains)
    (play-sound self "death.wav")
    (destroy self)))

(defun hamlet-house ()
  (combine (singleton (new 'ruined-house))
	   (with-border (units 8) (singleton (new 'hamlet-stairwell)))))

(defmethod make-terrain ((scene ruined-hamlet))
  (with-border (units 10)
    (lined-up-randomly
     (stacked-up-randomly (dead-trees) (singleton (new 'stone-patio)))
     (stacked-up-randomly (singleton (new 'ruin-wall)) (singleton (new 'ruined-house)) (singleton (new 'well)) (dead-trees))
     (stacked-up-randomly  (singleton (new 'ruined-house)) (singleton (new 'ruin-wall)) (hamlet-house) (singleton (new 'roberto-wraith)) (dead-trees)))))

