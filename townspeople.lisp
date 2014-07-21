(in-package :cypress)

(defthing (maxwell monk) 
  :next-target nil
  :description "Maxwell")

(defparameter *maxwell-walk* 
  '(:repeat t
    :scale 980
    :frames (("man-walk-1.png" 4)
	     ("man-walk-3.png" 4)
	     ("man-walk-2.png" 4)
	     ("man-walk-4.png" 4))))

(defmethod walking-animation ((self maxwell))
  *maxwell-walk*)

(defparameter *maxwell-stand*
  '(:scale 980
    :frames (("man-stand-1.png" 19)
	     ("man-stand-2.png" 16)
	     ("man-stand-3.png" 24))))

(defmethod standing-animation ((self maxwell))
  *maxwell-stand*)

(defmethod choose-target ((self maxwell))
  (setf (field-value :next-target self)
	(let ((targets (find-instances (current-scene) 'stone-patio)))
	  (when targets (random-choose targets)))))

;; (defmethod return-home ((self maxwell))
;;   (multiple-value-bind (x y) (below (find-pentaquin-house))
;;     (walk-to self x y)))

(defmethod run ((self maxwell))
  (with-fields (next-target gump waypoints) self
    (call-next-method)
    (unless gump (choose-target self))
    (let ((distance (distance-between (geoffrey) self)))
      (cond 
	((and (> distance 500)
	     (not gump))
	 (when (and next-target (null waypoints))
	   (percent-of-time 4 (walk-to-thing self next-target))))
	((and (< distance 220) (> distance 200))
	 (show-hint "Double-click Maxwell to talk.")
	 (when gump (walk-to-thing self (geoffrey))))
	((or gump (<= distance 200))
	 (setf waypoints nil))))))

;; maxwell has been cataloging stone walls and knows about the valisade ruins

(defmethod activate ((self maxwell))
  (destroy-gump self)
  (play-talk-sound self)
  (discuss self :hello))

(define-topic hello maxwell "Good morning!" :name)
(define-topic name maxwell 
  "My name is Maxwell Montana. I work at
the Geological Society here in town, and
also at the Library on week-ends! And
who might you be?"
:i-am-geoffrey-of-valisade)

(define-topic i-am-geoffrey-of-valisade maxwell
  "Valisade? I've not heard of it. You
must have come a long way! Of course, I
don't get out of the Valley much."
:explain-situation)

(define-topic explain-situation maxwell 
"O, God of Gods! Had you not told me the
name Valisade, I would have taken you
for a madman or a charlatan. But I know
that your story of time-travel is
true. For I can hear the curious
similarity of your V and B sounds, and
the way your terminal vowels tend to
slur just a little. For I am a man
trained in the science of Language, and
of the changing of its forms over
time. It seems that \"Valisade\" must
have been the ancient way of saying what
today is called \"Balsalva.\" by
archaeologists." :balsalva)

(define-topic balsalva maxwell
"Ahh... I see.

Now I understand your look of pain, when
we speak of the vanished Empire. For
everything you knew has long since
crumbled to dust; it must be a terrible
weight on your Soul to have lost all
your friends and loved ones.

Strictly speaking, all the stones I
catalogue for the Society are ruins of
the Balsalva. But The Monastery's ruins
are to the North of here. I'm sorry to
say that very little remains of the
Abbey itself. 

Something just occurred to me. In the
Abbey and in a few other places in the
Vale, there are certain megaliths which
appear to be seals or coverstones. I
have always wondered what might lay
underneath them!  There is some record
of the Balsalvan monks using a system of
magic Sigils to seal doors. A Sigil can
only be unlocked by a key whose symbol
matches the Sigil. There was a \"Warrior
symbol\" for lay Brothers, and another
symbol for ordained Monks; but we have
never found a way to dispel the magic of
Sigils. Perhaps a man of your Era will
be better suited to the task?" :symbol :abbey)

(define-topic symbol maxwell 
"The lay Brother's key symbol is a
circle with a hooked line running
through it. The circle represents the
Warrior's shield, and the hooked line is
his Spear. Coincidentally, it is one of
the few ideograms to survive into the
post-modern Era.

You might find that meditation on this
symbol can manifest a white light of
spiritual protection." :abbey)

(define-topic abbey maxwell 
"You will find the ruins of the Abbey to
the north of town. Beware, Geoffrey.
Sometimes glowing lights have been seen
in that clearing from afar; perhaps
ancient Spirits inhabit those stones.

Even farther North, at the frozen
foothills of Mount Pisgah, there is a
second ruin, perhaps a sister city of
some kind. It has deteriorated horribly,
but perhaps you will be able to learn
something there.

You must return soon! If you will but
allow me to question you for knowledge
of your Times, and of the people of
Balsalva. For we at the Library know
that the world is in a Dark Age; we are
sworn to preserve the histories of past
Aeons, in hope that one day a
newly-prosperous Humankind might balance
Righteousness with Knowledge. For the
Dark Age came by the hand of those who
made Knowledge slave to Righteousness.

Go in safety, Traveler. If I may fill
your tankard, let the pages of my
notebook fill with your stories, when
you return!" :bye)



