(in-package :cypress)

(defvar *journal* nil)
(defvar *thoughts* nil)

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

(defparameter *journal-hint* 
"Geoffrey has written a note in his
Journal. Press J to read it, or press I
and then double-click the Journal in
your inventory.")

(defparameter *thought-hint* 
"Geoffrey will write a new entry in his
Journal at the next campfire. Press I to
open your inventory, then drag the tent
to an open area and double-click it.")

(defun add-journal-entry (string &optional (hint *journal-hint*))
  (unless (find string *journal* :test 'equal) 
    (when hint (show-hint hint :force))
    (push string *journal*)
    (set-unread-p (find-journal) t)
    (magical-flourish)))

(defun add-thought (string)
  (unless (find string *thoughts* :test 'equal)
    (show-hint *thought-hint* :force)
    (push string *thoughts*)
    (magical-flourish)))

(defparameter *update-journal-hint*
"Geoffrey wrote a new journal entry.
Press J to read it, or press I and then
double-click the Journal in your
inventory.")

(defun update-journal ()
  (when *thoughts*
    (close-all-gumps (current-scene))
    (dolist (entry (reverse *thoughts*))
      (add-journal-entry entry nil))
    (setf *thoughts* nil)
    (pause)
    (show-hint *update-journal-hint* :force (seconds->frames 10))))

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

(defparameter *equip-hint*
"Equip clothing by dragging it into your
inventory and double-clicking.")

(defparameter *arrow-hint* 
"Equip your new Silver arrows by
double-clicking their icon in your
inventory.")

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

(defparameter *after-amalia-report*
"Perhaps if this Amalia could see the
future, she can also see the past?
")

(defparameter *before-hamlet* 
"How frigid the weather has become! And
a hamlet where life must have ended ere
a century past. 

I doubted Arturo's story at first, but
now it seems difficult to deny--- the
world I knew has crumbled away into
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
")

(defparameter *after-morbius-letter*
"Who is the Morbius mentioned in this
letter? What sort of Lord does he
address? Why was the letter left here,
and not delivered?

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

(defparameter *anytime-valisade*
"What a desolate place this is. It seems
as if a small city once stood here.
I feel as if I am being watched.
")

(defparameter *remembering-valisade*
"So this is what remains of Valisade...
Its towers have fallen, and its grand
plaza has become a patch of gravel.

Unlike most Ordained, I have never seen
the grave of my parents. But this place
is the grave of my true Family, the Holy
Order of Valisade. 

While I slept, I felt an unusual
silence, as if a minstrel had suddenly
fallen mute.  For the first time I feel
like one of the Ordained---somehow the
shame of being an orphan has been
dispelled. 'Tis a shame that the loss of
my Spiritual family was the price.
")

(defparameter *before-maxwell*
"I've got to stop thinking about it, and
find Maxwell.")

(defparameter *after-maxwell* 
"Very well. Whatever hopes I have are
now pinned to these two copper gears in
my satchel. I should rest and gather my
supplies before heading southeast to the
ancient cave, and into the Unknown.")

(defparameter *after-quine-letter* 
"To hear from Francis and Dr. Quine has
filled me with hope! But this is
tempered with trepidation at hearing of
this \"dark horse\", Samuel.  

I must head due North and find these
mysterious Screech Owls.
")

(defparameter *after-translation* 
"
")

;; (defparameter *after-owl* ;;; refer to book and hint at where flute might be

;; (defparameter *before-outpost*

;; (defparameter *after-outpost*

;; (defparameter *before-retreat*

;; (defparameter *after-retreat*

;; (defparameter *after-outpost-basement*

;; (defparameter *before-valisade* 

;; (defparameter *after-valisade*

;; (defparameter *before-neumes*

;; (defparameter *after-neumes*

;; (defparameter *before-cave*

;; (defparameter *after-cave*

;; (defparameter *after-wizards*

;; (defparameter *before-crossing*

;; (defparameter *after-crossing*

;; (defparameter *after-shayol*
