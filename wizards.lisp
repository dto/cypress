(in-package :cypress)

(defthing cylindrophone :image "cylindrophone.png")

(defparameter *cylinder-hint* 
"Double-click the chosen cylinder to
play the message.")

(defmethod activate ((self cylindrophone))
  (if (not (find-inventory-item (geoffrey) 'wax-cylinder))
      (bark (geoffrey) "I don't have anything to use with this.")
      (show-hint *cylinder-hint*)))

(defthing (black-wizard monk)
  :description "Black Wizard"
  :magic 100
  :clock 0
  :health 55)

(defparameter *wizard-casting-time* (seconds->frames 0.7))

(defparameter *black-wizard-cast*
  '(:scale 1400
    :frames (("black-wizard-cast-1.png" 5)
	     ("black-wizard-cast-2.png" 5)
	     ("black-wizard-cast-3.png" 5)
	     ("black-wizard-cast-4.png" 5)
	     ("black-wizard-cast-5.png" 6))))

(defparameter *black-wizard-stand*
  '(:scale 900
    :frames (("black-wizard-stand-1.png" 19)
	     ("black-wizard-stand-2.png" 24)
	     ("black-wizard-stand-3.png" 18))))

(defparameter *black-wizard-walk* 
  '(:repeat t
    :scale 900
    :frames (("black-wizard-walk-2.png" 4)
	     ("black-wizard-walk-4.png" 4)
	     ("black-wizard-walk-1.png" 4)
	     ("black-wizard-walk-3.png" 3))))

(defmethod standing-animation ((self black-wizard)) *black-wizard-stand*)

(defmethod walking-animation ((self black-wizard)) *black-wizard-walk*)

(defmethod casting-animation ((self black-wizard)) *black-wizard-cast*)

;;; Magic arrow seeks geoffrey

(defthing (magic-arrow arrow)
  :images *crystal-arrow-images*
  :speed 14
  :image (random-choose *crystal-arrow-images*))

(defmethod collide ((self magic-arrow) (enemy enemy)) nil)
(defmethod collide ((self magic-arrow) (enemy black-wizard)) nil)

(defmethod collide ((self wooden-arrow) (enemy black-wizard))
  (damage enemy (random-choose '(-3 -5 -7)))
  (destroy self))

(defmethod collide ((self silver-arrow) (enemy black-wizard))
  (damage enemy (random-choose '(-8 -12 -17)))
  (destroy self))

(defmethod collide ((monk geoffrey) (arrow magic-arrow))
  (damage (geoffrey) (random-choose '(-12 -16 -19)))
  (destroy arrow))

(defmethod run :after ((arrow magic-arrow))
  (with-fields (image) arrow
    (setf image (random-choose *crystal-arrow-images*))))

(defmethod begin-attack ((wizard black-wizard))
  (stop-walking wizard)
  (setf (field-value :heading wizard) (heading-to-cursor wizard))
  (begin-animation wizard (casting-animation wizard))
  (setf (field-value :clock wizard) *wizard-casting-time*))

(defmethod fire-magic-arrow ((wizard black-wizard))
  (play-sample "bow.wav")
  (multiple-value-bind (x y) (fire-location wizard)
    (drop-object (current-scene)
		 (new 'magic-arrow :heading (heading-to-cursor wizard))
		 x y)))

(defmethod collide ((wizard black-wizard) (other black-wizard)) nil)
  ;; (restore-location wizard)
  ;; (stop-walking wizard))

(defmethod run ((wizard black-wizard)) 
  (call-next-method)
  (with-fields (clock waypoints) wizard
    (when (zerop clock)
      (percent-of-time 2 (begin-attack wizard)))
    (when (plusp clock)
      (decf clock)
      (when (zerop clock)
	(fire-magic-arrow wizard)))
    (percent-of-time 1
      (walk-to-thing wizard (geoffrey)))
    (when (< (distance-between wizard (geoffrey)) 300)
      (or (percent-of-time 2 (prog1 t (stop-walking wizard)))
	  (or (percent-of-time 1 (prog1 t (walk-to-thing wizard (geoffrey))))
	      (percent-of-time 1 (begin-attack wizard)))))))

(defmethod die ((self black-wizard))
  (play-sound self "death.wav")
  (destroy self))

(defmethod activate-maybe ((wizard black-wizard))
  (activate wizard))

(defmethod activate ((wizard black-wizard))
  (resume)
  (attack (geoffrey) wizard))

;;; Ruins

(defthing tent :scale 0.9 :image (random-choose '("tent-1.png" "tent-2.png")) :tags '(:solid :fixed))

(defthing (morbius-tent tent) :description "Morbius' tent" :activated nil)

(defparameter *half-burnt-letter* 
"Lord Shayol,

We are nearly ready to strike! For no
one can outlast an archer with a
bottomless quiver. We shall follow the
Traveler northward toward the Abyss, and
sieze him as he crosses its black
waters.

Thank you for the supply of Silverwool,
my Lord. It will make our own crossing
easier, seeing as we must make do with
this wretched, tormenting flesh!

After the Traveler, we will press onward
to the first search area. We will burn
you another report when the time comes.

Before closing, if I may ask, what is
the significance of the woman? If she is
indeed dead, you should be able to
search the river for her? A soul can't
have traveled far in a just a few years,

 (the rest of the scroll is burnt)")

(defthing wax-cylinder :image (random-choose '("wax-cylinder-1.png" "wax-cylinder-2.png")))

(defthing (morbius-cylinder wax-cylinder) :description "morbius' wax cylinder")
(defthing (shayol-cylinder wax-cylinder) :description "shayol's wax cylinder")

(defmethod activate ((tent morbius-tent))
  (with-fields (activated) tent
    (when (not activated)
      (setf activated t)
      (add-inventory-item tent (new 'silverwool-leggings))
      (add-inventory-item tent (new 'silverwool-shirt))
      (add-inventory-item tent (make-scroll "half-burnt letter" *half-burnt-letter*))
      (add-inventory-item tent (new 'morbius-cylinder))
      (add-inventory-item tent (new 'cylindrophone)))
    (replace-gump tent (new 'browser :container tent))))

(defthing (wizard-ruins scene)
  :background-image "paynes-meadow.png")

(defmethod find-description ((ruins wizard-ruins)) "ruins")

(defmethod begin-scene :after ((ruins wizard-ruins))
  (mark-traversed ruins)
  (cue-music ruins (random-choose '("procession4.ogg"))))

(defmethod make-terrain ((scene wizard-ruins))
  (with-border (units 18)
    (lined-up-randomly
     (stacked-up-randomly (singleton (new 'ruined-house)) (singleton (new 'black-wizard)) (singleton (new 'well)) (dead-trees))
     (stacked-up-randomly (singleton (new 'ruined-house)) (singleton (new 'black-wizard)) (singleton (new 'ruin-wall)) (dead-trees))
     (stacked-up-randomly (dead-trees) (singleton (new 'black-wizard)) (stacked-up (singleton (new 'morbius-tent))
										   (spray 'tent :count 2)
										   (singleton (new 'ruin-wall)))
			  (singleton (new 'ruined-house)) (dead-trees)))))


(defmethod make-footstep-sound ((self black-wizard)) nil)

;;; Gray wizard

(defthing (gray-wizard monk)
  :description "Man in gray robes"
  :magic 100
  :clock 0
  :health 100)

(defparameter *gray-wizard-firing-time* (seconds->frames 0.7))

(defparameter *gray-wizard-fire*
  '(:scale 1000
    :frames (("skeleton-archer-fire-1.png" 5)
	     ("skeleton-archer-fire-2.png" 6)
	     ("skeleton-archer-fire-3.png" 7))))

(defparameter *gray-wizard-stand*
  '(:scale 900
    :frames (("skeleton-archer-stand-1.png" 19)
	     ("skeleton-archer-stand-2.png" 24)
	     ("skeleton-archer-stand-3.png" 18))))

(defparameter *gray-wizard-walk* 
  '(:repeat t
    :scale 900
    :frames (("skeleton-archer-walk-2.png" 4)
	     ("skeleton-archer-walk-4.png" 4)
	     ("skeleton-archer-walk-1.png" 4)
	     ("skeleton-archer-walk-3.png" 3))))

(defmethod standing-animation ((self gray-wizard)) *gray-wizard-stand*)

(defmethod walking-animation ((self gray-wizard)) *gray-wizard-walk*)

(defmethod casting-animation ((self gray-wizard)) *gray-wizard-fire*)

(defmethod begin-attack ((wizard gray-wizard))
  (stop-walking wizard)
  (setf (field-value :heading wizard) (heading-to-cursor wizard))
  (begin-animation wizard (casting-animation wizard))
  (setf (field-value :clock wizard) *wizard-casting-time*))

(defmethod fire-magic-arrow ((wizard gray-wizard))
  (play-sample "bow.wav")
  (multiple-value-bind (x y) (fire-location wizard)
    (drop-object (current-scene)
		 (new 'magic-arrow :heading (heading-to-cursor wizard))
		 x y)))

(defmethod run ((wizard gray-wizard))
  (with-fields (clock waypoints) wizard
    (if (event-occurred-p :final-battle)
	;; fight 
	(progn 
	  (call-next-method)
	  (when (zerop clock)
		 (percent-of-time 0.7 (begin-attack wizard)))
	       (when (plusp clock)
		 (decf clock)
		 (when (zerop clock)
		   (fire-magic-arrow wizard)))
	       (percent-of-time 1
		 (walk-to-thing wizard (geoffrey)))
	       (when (< (distance-between wizard (geoffrey)) 300)
		 (or (percent-of-time 2 (prog1 t (stop-walking wizard)))
		     (or (percent-of-time 1 (prog1 t (walk-to-thing wizard (geoffrey))))
			 (percent-of-time 1 (begin-attack wizard))))))
	  ;; talk
	  (let ((distance (distance-between wizard (geoffrey))))
	    (cond 
	      ((< distance 500)
	       (unless (event-occurred-p :wizard-meets-geoffrey)
		 (add-event :wizard-meets-geoffrey)
		 (walk-to-thing wizard (geoffrey))
		 (bark wizard "Hold, Traveler. Let us speak.")))
	      ((< distance 340)
	       (stop-walking wizard)))
	    (call-next-method)))))

(defmethod update :before ((wizard gray-wizard))
  (with-fields (stasis) wizard
    (when (and stasis (plusp stasis))
      (decf stasis 2))))

(defmethod activate-maybe ((wizard gray-wizard))
  (activate wizard))

(defmethod activate ((wizard gray-wizard))
  (resume)
  (if (event-occurred-p :final-battle)
      (attack (geoffrey) wizard)
      (discuss wizard :hello)))

(define-topic hello gray-wizard 
  "Greetings, Geoffrey. Yes, I am a
fellow Traveler. And you've arrived just
in Time, as they say." :name)

(define-topic name gray-wizard 
"My name isn't important. Nor is the
wretched body beneath these robes. No,
I'd rather talk about YOU.

Why do you press on like this, Geoffrey?

Not much to live for, is there? Family
and friends, all dead for Aeons. You
could try to find a happy life here, but
the Sun grows colder by the day. Not
exactly the kind of place you'd want to
raise a child, is it? 

Of course, THEY don't know the Sun is
doomed. But now YOU know." :why-are-you-telling-me-this?)

(define-topic why-are-you-telling-me-this? gray-wizard
"Because, I can help you, Geoffrey. You
can be re-united with your family, as I
have been. You can live for thousands of
years, as I have. Everything you dream
of is possible through the power of the
Ur." :the-ur?)

(define-topic the-ur? gray-wizard
"Yes. This is not his true name, for he
has none. \"The Ur\" is just one of the
many names given to him by the early
Humans of Ildron." 
:where-is-the-prescient-woman?)

(define-topic where-is-the-prescient-woman? gray-wizard 
"She isn't here anymore."
:then-why-are-you-still-here?)

(define-topic then-why-are-you-still-here? gray-wizard
"I came to see you, Geoffrey. 

For if the Bishop is to take the Knight,
they must meet on the chessboard
sometime---isn't it so? 

But there is an alternative to blood."
:alternative)

(define-topic alternative gray-wizard
"My master has a proposal for you.
His voice is recorded on the wax
cylinder I hold. If you'll only listen
to the message, all your questions will
be answered." :no-way!)

(define-topic no-way! gray-wizard
"Think carefully about your decision,
Geoffrey! Are you sure you don't want to
listen?  

So what if we fight, and you succeed in
destroying this putrid flesh-puppet?
Then you could simply take the cylinder!

Won't you listen? Would you throw it
away? Or burn it, without ever hearing
the secrets engraved on its surface?

Choose wisely, Geoffrey. For I will tell
you one of the secrets in advance. Even
if you best me in combat and destroy the
cylinder, you and those you love will
regardless one day be taken by the
Ur. You and everyone else on this
planet, such is its power. 

It will be better for you, and for them,
if you will simply listen to the message
the cylinder contains.

Just listen! What harm could it do to
hear him out?" :no!)

(define-topic no! gray-wizard
"So I see. It was prophesied that you
would refuse. It is also foretold that I
will die here. 

Only Amalia stopped writing, and died on
the spot, before she could prophesy what
you would do with the cylinder! The
serum was too much for her aging body.

So! Let us see if Fate obeys Amalia's
gift!

To the Death, now!" :fight)

(defmethod die ((self gray-wizard))
  (play-sound self "death.wav")
  (let ((remains (new 'remains)))
    (add-inventory-item remains (new 'shayol-cylinder))
    (drop self remains))
  (destroy self))

(defmethod discuss :after ((self gray-wizard) (topic (eql :no!)))
  (add-event :final-battle)
  (cue-music (current-scene) "presence.ogg")
  (pause))

(defmethod collide ((self wooden-arrow) (enemy gray-wizard))
  (damage enemy (random-choose '(-3 -5 -7)))
  (destroy self))

(defmethod collide ((self silver-arrow) (enemy gray-wizard))
  (damage enemy (random-choose '(-8 -12 -17)))
  (destroy self))

(defparameter *morbius-letter* 
  "Morbius! You must notify me the
moment you have her in your possession!
Tell Crito to keep a fire burning at the
camp for this purpose.")

(defparameter *traveler-letter*
  "Listen to me, O Traveler!
   
When God created his first seven
followers, they made for themselves a
secret Language.

The word for Time was itself a gateway
to the future and past; the word for
Killing could itself kill; and so on.

I have chosen you now, to receive the
Tongue!

Close your eyes.

I will now count backward from ten to
one.

Ten... nine... eight... seven...

six... five... four...

three... two... one...")

(defresource "morbius-cylinder.wav" :volume 20)

(defmethod activate ((cylinder morbius-cylinder))
  (if (not (find-inventory-item (geoffrey) 'cylindrophone))
      (bark (geoffrey) "I don't have anything to use with this.")
      (progn
	(play-sample "morbius-cylinder.wav")
	(replace-gump cylinder (new 'scroll-gump :text *morbius-letter*)))))
	    
;;; Ending story screen 

(defresource "shayol.ogg" :volume 30)

(defthing ending-card :image "ending.png")

(defmethod initialize :after ((ending-card ending-card) &key)
  (resize ending-card *nominal-screen-width* *nominal-screen-height*)
  (move-to ending-card 0 (- *nominal-screen-height* 100)))
  
(defmethod update ((card ending-card))
  (with-fields (x y) card
    (when (plusp y)
      (move-to card x (- y 0.6)))))

(define-buffer ending 
  (quadtree-depth :initform 4))

(defmethod initialize :after ((ending ending) &key)
  (drop-object ending (new 'ending-card))
  (play-music "shayol.ogg" :loop nil)
  (resize ending *nominal-screen-width* *nominal-screen-height*))

(defmethod tap ((ending ending) x y) nil)
(defmethod tap ((ending-card ending-card) x y) nil)
(defmethod scroll-tap ((ending ending) x y) nil)
(defmethod alternate-tap ((ending ending) x y) nil)

;;; Final game action

(defmethod activate ((cylinder shayol-cylinder))
  (if (not (find-inventory-item (geoffrey) 'cylindrophone))
      (bark (geoffrey) "I don't have anything to use with this.")
      (switch-to-buffer (new 'ending))))


      







