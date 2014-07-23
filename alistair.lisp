(in-package :cypress)

(defthing (alistair monk) 
  :met-player nil
  :next-target nil
  :given-letter nil
  :description "Alistair")

(defparameter *alistair-walk* 
  '(:repeat t
    :scale 1080
    :frames (("alistair-walk-1.png" 4)
	     ("alistair-walk-2.png" 4)
	     ("alistair-walk-3.png" 4)
	     ("alistair-walk-4.png" 4))))

(defmethod walking-animation ((self alistair))
  *alistair-walk*)

(defparameter *alistair-stand*
  '(:scale 1080
    :frames (("alistair-stand-1.png" 19)
	     ("alistair-stand-2.png" 16)
	     ("alistair-stand-3.png" 24))))

(defmethod standing-animation ((self alistair))
  *alistair-stand*)

(defparameter *alistair-cast*
  '(:scale 1080
    :frames (("alistair-stand-1.png" 19)
	     ("alistair-stand-2.png" 16)
	     ("alistair-stand-3.png" 24))))

(defmethod casting-animation ((self alistair))
  *alistair-cast*)

(defmethod choose-target ((self alistair))
  (setf (field-value :next-target self)
	(let ((targets (find-instances (current-scene) 'ruined-book)))
	  (when targets (random-choose targets)))))

(defmethod run ((self alistair))
  (with-fields (next-target given-letter met-player gump waypoints) self
    (call-next-method)
    (choose-target self)
    (let ((distance (distance-to-cursor self)))
      (cond 
	((or given-letter gump (<= distance 200))
	 (setf waypoints nil))
	((and (< distance 220) (> distance 200))
	 (show-hint "Double-click Alistair to talk.")
	 (walk-to-thing self (geoffrey)))
	(t (percent-of-time 4 (walk-to-thing self next-target)))))))

(defmethod activate ((self alistair))
  (destroy-gump self)
  (setf (field-value :gump self) nil)
  (play-talk-sound self)
  (with-fields (met-player given-letter) self
    (if (not met-player)
	(progn (setf met-player t)
	       (discuss self :hello))
	(if given-letter
	    (discuss self :busy)
	    (discuss self :hello)))))

(define-topic hello alistair 
  "Greetings!" :name)

(define-topic name alistair 
"I am Alistair, a mechanical monk imbued
with Spirit. A Spirit I must work hard
to maintain, seeing as my creators are
long-gone, and my fellow mecha-monks are
all so busy studying in other caves." 
:mecha :creators)

(defmethod discuss :after ((alistair alistair) (topic (eql :name)))
  (setf (field-value :met-player alistair) t))

(define-topic mecha alistair
"The crafting and animation of
mechanical men came from the alliance of
the Black Wizards and the
Industrialists, ages ago. While the
Empire used such mecha for violence and
domination, we mecha-monks of the
Quinian Order are sworn to preserve the
knowledge of the peaceful Ages in the
recesses of our clock-work minds. And of
course, in the books you see around here
everywhere. I'm always re-organizing
them!" :quine :wizards :creators)

(define-topic wizards alistair
"You shall undoubtedly encounter the
lying Black Wizards on your journey. You
cannot trust them! While I cannot jump
ahead of the tale for Quine's sake, I
must warn you of at least that." :quine :creators :mecha)

(define-topic creators alistair 
  "Oh, that's a tricky one. I'll let
Quine's letter explain the situation.
As I said---I'm not allowed to jump
ahead of the tale." :quine :wizards :mecha)

(define-topic quine alistair 
  "I hold here a letter for Geoffrey of
Valisade, from the famous Dr. Quine
himself! To be delivered upon his
arrival through the gate of the Southern
Cave." :letter)

(define-topic letter alistair
  "Here's the letter. 

I will also bestow upon you the language
of the Ancients, by means of their Spell
of Translation. If you focus the mind by
means of this spell, you will be able to
read the ancient's writings for a short
time. 

And that finishes my duties for now. I'm
quite busy, actually, organizing all
these books. So.. umm...." :bye :talk-more)

(defmethod discuss :after ((alistair alistair) (topic (eql :letter)))
  (when (not (field-value :given-letter alistair))
    (drop alistair (make-scroll "Letter from Dr. Quine" *first-quine-letter*)
	  (units 3) (units 3))
    (learn-spell (geoffrey) (new 'translation))
    (set-objective "Find the Screech Owl in the forests to the North.")
    (setf (field-value :given-letter alistair) t)))

(define-topic talk-more alistair 
"Very well, what would you like to talk
about?" :mecha :creators :wizards :quine :bye)

(define-topic busy alistair 
  "I'm very busy! I don't have time to talk." :bye :talk-more)





