(in-package :cypress)

(defvar *journal* nil)

(defparameter *journal-image* "notebook-3.png")

(defun current-journal-entry ()
  (first *journal*))

(defthing hintbook
  :image *journal-image*
  :description "Geoffrey's journal"
  :unread-p nil)

(defmethod unread-p ((hintbook hintbook))
  (field-value :unread-p hintbook))

(defmethod set-unread-p ((hintbook hintbook) unread-p)
  (setf (field-value :unread-p hintbook) unread-p))

(defun find-journal ()
  (find-inventory-item (geoffrey) 'hintbook))

(defun add-journal-entry (string)
  (unless (find string *journal* :test 'equal) 
    (bark (geoffrey) "I've made a new journal entry.")
    (push string *journal*)
    (set-unread-p (find-journal) t)
    (magical-flourish)))

(defparameter *journal-footer* " ....................................................... ")

(defun journalize (string)
  (concatenate 'string string (string #\Newline) *journal-footer* (string #\Newline) (string #\Newline)))

(defmethod find-text ((self hintbook))
  (if (null *journal*)
      "(The journal is presently blank.)"
      (apply #'concatenate 'string (mapcar #'journalize *journal*))))

(defmethod activate ((self hintbook))
  (set-unread-p self nil)
  (let ((gump (new 'scroll-gump :text (find-text self))))
    (replace-gump self gump)
    (set-target-position gump (+ (window-x) (units 80)) (+ (window-y) (units 2)))))

(defmethod can-pick ((book hintbook))
  (not (field-value :container book)))

(defmethod can-accept ((book hintbook)) nil)
(defmethod will-accept ((book hintbook) (thing thing)) nil)

(defparameter *first-journal-entry*
"I can't get Dr. Quine's letter off my
mind. What could be so urgent? I should
re-read his letter, and also review the
Adventurer's Guide scroll he left me;
perhaps there's something more to be
learned.

(Right-click this scroll to close it,
then open your inventory by pressing the
letter \"I\" or by double-clicking
Geoffrey.)
")

(defparameter *journey-to-nothbehem*
"I met a queer fellow named Lucius, who
has graciously offered to accompany me
to the nearest town. He seems rather
confused---I've never heard of any great
explorer named Eavesbury---nor of any
wars in the West---nor of the Sun's
light fading! Though it is unusually
cold in this vale... still, the man's
speech is a tad unusual. I wonder if he
has been drinking...?

He says Nothbehem is just to the North
of here---perhaps that is the source of
its name? We should move quickly and try
to stay warm. Now to open my Spellbook,
and try out this Travel Scrying magic I
learned from Lucius.

But first, I want to collect a few of
these white Snowdrops; they might be
useful, if, as Lucius tells me, they've
got a touch of Magic energy in each
blossom.
")

(defparameter *arrival-in-nothbehem*
"Well, isn't this a splendid little
Town? I should ask Lucius where we can
find his Grandfather.
")

(defparameter *after-arturo* 
"I'm not sure I can believe what the old
man says. Could I really have crossed
into another century as I breached the
mountain pass?

In any event, I've certainly lost my way
to Valisade; if I'm to help Dr. Quine, I
must keep my Spirits up, and trudge
onward.

I shall visit Arturo's house---the one
with the three skylights---and pick up
the supplies he told me about. But
before I go, I should give my thanks to
Lucius, and bid him goodbye.
")

(defparameter *before-woods*
"The Wilmont Woods are far to the
Northwest; I should look for the other
Waystone near there, as well.

But first I'll rest, and craft some of
those Silver Arrows with my new spell!

This would also be a good time to review
Dr. Quine's Guide for Adventurers; I'll
have to keep things fresh in my mind, if
I'm to succeed.
")

(defparameter *before-hamlet* 
"How frigid the weather has become! And
a hamlet where life must have ended ere
a century past. 

I doubted the old man's story at first,
but now it seems difficult to deny---
the world I knew has crumbled away into
dust. 

What became of my family?  What about
Francis and Dr. Quine? Still, I soldier
onward; for if the Magicks that brought
me forward into this Age could be
undone, mayhaps I could return home
someday.

This ruin must be the place Lucius
warned me about; I shall investigate
carefully.
")

(defparameter *after-hamlet*
"What horrid creatures I discovered in
the moldering cellar! The Spirits who
inhabit the Wraith are but friendly
passersby, compared with these wicked
crawling skulls, with their fingerbones
for legs and their soot-black sockets!

And who is the Morbius mentioned in this
letter? What sort of Lord does he
address? Why was it left here, and not
delivered?

Perhaps I might find the Wraith nearby
who took the Skull they were searching
for.

In any event, Alonso Pentaquin's cabin
must be somewhere in the woods to the
North of here. I'll keep going.
")

(defparameter *after-roberto* 
"I am shaken by the voice of this
Spirit; though he is filled with
kindness and devoid of Regret, I cannot
resist feeling that I have done
something dangerous by the use of this
Seance magick. For if one can hear the
Dead speak, perhaps a wicked soul might
seek to deceive the living who listen?

I noticed that Roberto named an 'Amalia'
as one of his children. If this be the
same as the fortune-teller who fled
Nothbehem (as told in the Expedition
report) then why was this Morbius
looking for her father?
")

(defparameter *before-alonso* 
"I see the remains of a cottage nestled
in the trees! Could this have been
Alonso's home?
")

(defparameter *after-alonso*
"So Alonso has passed onward to the Land
of the Dead; Arturo will be crestfallen,
I'm sure. I'll camp here tonight and set
out for Nothbehem tomorrow to deliver
this letter. Perhaps the message of
forgiveness it carries will soften the
blow.")

;; (defparameter *before-maxwell* express sadness at Arturo's loss, and compare to self... talk about seeing maxwell

;; (defparameter *after-maxwell*  discuss abbey, sigils, ruins to North at foothills of Pisgah, going to southern cave

;; (defparameter *after-alistair* ;;; translation of book

;; (defparameter *after-owl* ;;; refer to book and hint at where flute might be

;; (defparameter *before-outpost*

;; (defparameter *after-outpost*

;; (defparameter *before-retreat*

;; (defparameter *after-retreat*

;; (defparameter *after-outpost-basement*

;; (defparameter *anytime-valisade*

;; (defparameter *before-valisade* 

;; (defparameter *after-valisade*

;; (defparameter *before-neumes*

;; (defparameter *after-neumes*

;; (defparameter *before-cave*

;; (defparameter *after-cave*

;; (defparameter *after-wizards*

;; (defparameter *before-crossing*

;; (defparameter *after-crossing*
