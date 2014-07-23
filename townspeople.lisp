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
  (if (event-occurred-p :completed-alonso-quest)
      (discuss self :hello)
      (discuss self :busy)))

(define-topic busy maxwell 
  "Oh, hello! You look interesting. You
must be from outside the Vale? So much
to do. I'm terribly busy right now,
inspecting these ancient stones. The
town's walls have got to be
assessed. Repairs will need to be
made. Budgets and schedules will need to
be written up! Please come back later."
:bye)

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
"I believe you, Stranger. Had you not
told me the name Valisade, I would have
taken you for a madman or a
charlatan. But I know that your story of
time-travel is true. For I am a man
trained in the science of Language, and
of the changing of its forms over
time. I can hear the curious similarity
of your V and B sounds, and the way your
terminal vowels tend to slur just a
little. The linguist in me thinks
\"Valisade\" must have been the ancient
way of saying what today is called
\"Balsalva.\" by archaeologists." 
:balsalva)

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
the Balsalva; even the old stone walls
and patios here in town. But I suspect
you will want to know about the
Monastery. I'm sorry to say that very
little remains of the Abbey itself. The
ruins are to the North of here.

Something just occurred to me. In the
Abbey and in a few other places in the
Vale, there are certain megaliths which
appear to be seals or coverstones. I
have always wondered what might lay
underneath them! Many have tried, but
none have succeeded in prying loose the
stones. There is some record of the
Balsalvan monks using a system of magic
Sigils to seal doors. A Sigil can only
be unlocked by one holding a key whose
symbol matches the Sigil. There was a
\"Warrior symbol\" for lay Brothers, and
another, triangular symbol for ordained
Monks; but we have never found a way to
dispel the magic of Sigils. Perhaps a
man of your Era will be better suited to
the task?"
:symbol :abbey)

(define-topic symbol maxwell 
"The lay Brother's key symbol is a
circle with a hooked line running
through it. The circle represents the
Warrior's shield, and the hooked line is
his Spear. Coincidentally, it is one of
the few ideograms to survive into the
post-modern Era, and some regiments used
it to adorn armor, banners, and even
their skin.

You might find that meditation on this
symbol can manifest a white light of
spiritual protection." :abbey)

(define-topic abbey maxwell 
"You will find the ruins of the Abbey to
the north of town. Beware, Geoffrey.
Sometimes glowing lights have been seen
in that clearing from afar; perhaps
ancient Spirits wander among the stones.

Even farther North, at the frozen
foothills of Mount Pisgah, there is a
second ruin. It has deteriorated badly,
but perhaps you will be able to learn
something there. There are probably
other ruins in the Northwest, but we
know very little, owing to the extreme
cold conditions now prevailing there."
:cold)

(define-topic cold maxwell 
"Yes. In last century Ildron has seen a
sharp drop in temperature, and things
are getting worse. The Sun seems to have
cooled all of a sudden; we don't know
why.

The Ancient astrologers collected their
writings in what is now called the Book
of the Sun, but only fragments remain of
this text. We hope that finding more
pieces will help us understand what is
happening.
" :ancients)

(define-topic ancients maxwell
"You must return soon! If you will but
allow me to question you for knowledge
of your Times, and of the people of
Balsalva, a great deal of Good might be
done. For some of us at the Library
believe that the world is in a Dark Age;
we are sworn to preserve the histories
of past Aeons, in hope that one day a
newly-prosperous Humankind might balance
Righteousness with Knowledge. For the
Dark Age came by the hand of those who
made Knowledge slave to Righteousness.

Perhaps I spend too much time digging in
the dirt, to speak of such high
ideals. But if the wisdom of the
Ancients can help us survive this age of
Trial, then what shame is it to have
dirt beneath one's fingernails?

Oh, and please take this with you. It's
one of the Ancients' silver books, with
their special silvery paper that lasts
forever.  It's a little hand-written
journal, mostly blank but with a few
pages of writing at the start. I found
it in the Northern ruins I told you
about, and I've carried it since then as
a good-luck charm. We've never
deciphered the script, and I didn't have
the heart to write anything new in
it. But you might find it useful if
you're going cross-country, for the book
and its pages are completely
water-resistant.

Go in safety, Traveler. If I may fill
your tankard when you return, then let
the pages of my notebook fill with your
stories!" 
:bye)

(defmethod discuss :after ((maxwell maxwell) (topic (eql :ancients)))
  (unless (event-occurred-p :maxwell-given-journal)
    (add-event :maxwell-given-journal)
    (drop maxwell (new 'journal)
	  (units 5) (units 5))))

;;; Madeline

(defthing (madeline monk) 
  :next-target nil
  :description "Madeline")

(defparameter *madeline-walk* 
  '(:repeat t
    :scale 1000
    :frames (("woman-walk-1.png" 4)
	     ("woman-walk-3.png" 4)
	     ("woman-walk-2.png" 4)
	     ("woman-walk-4.png" 4))))

(defmethod walking-animation ((self madeline))
  *madeline-walk*)

(defparameter *madeline-stand*
  '(:scale 1000
    :frames (("woman-stand-1.png" 19)
	     ("woman-stand-2.png" 16)
	     ("woman-stand-3.png" 24))))

(defmethod standing-animation ((self madeline))
  *madeline-stand*)

(defmethod choose-target ((self madeline))
  (setf (field-value :next-target self)
	(let ((targets (find-instances (current-scene) 'flower)))
	  (when targets (random-choose targets)))))

(defmethod run ((self madeline))
  (with-fields (next-target gump waypoints) self
    (call-next-method)
    (unless gump (choose-target self))
    (let ((distance (distance-between (geoffrey) self)))
      (cond 
	((and (> distance 500)
	     (not gump))
	 (when (and next-target (null waypoints))
	   (percent-of-time 2 (walk-to-thing self next-target))))
	((and (< distance 220) (> distance 200))
	 (show-hint "Double-click Madeline to talk.")
	 (when gump (walk-to-thing self (geoffrey))))
	((or gump (<= distance 200))
	 (setf waypoints nil))))))

(defmethod activate ((self madeline))
  (destroy-gump self)
  (play-talk-sound self)
  (discuss self :hello))

(define-topic hello madeline 
  "Well hello there! And who might you
be?" :i-am-geoffrey-of-valisade)

(define-topic i-am-geoffrey-of-valisade madeline 
  "Good day to you, Geoffrey.
My name is Madeline Montana. You must be
from out of town? Um, oh, hum, I'm sorry
but my daughter and I must be
going. We're quite busy!" :bye)

;;; Maribel

(defthing (maribel monk) 
  :next-target nil
  :description "Maribel")

(defparameter *maribel-walk* 
  '(:repeat t
    :scale 980
    :frames (("girl-walk-1.png" 4)
	     ("girl-walk-3.png" 4)
	     ("girl-walk-2.png" 4)
	     ("girl-walk-4.png" 4))))

(defmethod walking-animation ((self maribel))
  *maribel-walk*)

(defparameter *maribel-stand*
  '(:scale 980
    :frames (("girl-stand-1.png" 19)
	     ("girl-stand-2.png" 16)
	     ("girl-stand-3.png" 24))))

(defmethod standing-animation ((self maribel))
  *maribel-stand*)

(defmethod choose-target ((self maribel))
  (setf (field-value :next-target self)
	(let ((targets (find-instances (current-scene) 'madeline)))
	  (when targets (random-choose targets)))))

(defmethod run ((self maribel))
  (with-fields (next-target gump waypoints) self
    (call-next-method)
    (unless gump (choose-target self))
    (when next-target
      (let ((distance (distance-between self next-target)))
      (cond 
	((> distance 320)
	 (when (and next-target (null waypoints))
	   (percent-of-time 2 (walk-to-thing self next-target))))
	((or gump (<= distance 200))
	 (setf waypoints nil)))))))

(defmethod activate ((self maribel))
  (bark self (random-choose '("I'm five years old!" "Hi there!" "Where's my teddy bear?" "I love birds." "Hey." "Hello."))))
