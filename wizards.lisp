(in-package :cypress)

(defthing cylindrophone :image "cylindrophone.png")

(defmethod activate ((self cylindrophone))
  (if (not (find-inventory-item (geoffrey) 'wax-cylinder))
      (bark (geoffrey) "I don't have anything to use with this.")
      (at-next-update (switch-to-buffer (new 'prologue)))))

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
"My Lord,

We are nearly ready to strike! We
prepare our crystal potions by night.
We shall follow the Traveler northward
toward the Abyss, and sieze him as he
crosses its black waters. 

Thank you for the supply of Silverwool,
my Lord. It has made our journey easier.

After the Traveler, we will move on to
the North to find the Prescient woman...

 (the rest of the scroll is burnt)")

(defthing wax-cylinder :image (random-choose '("wax-cylinder-1.png" "wax-cylinder-2.png")))

(defmethod activate ((tent morbius-tent))
  (with-fields (activated) tent
    (when (not activated)
      (setf activated t)
      (add-inventory-item tent (new 'silverwool-leggings))
      (add-inventory-item tent (new 'silverwool-shirt))
      (add-inventory-item tent (make-scroll "half-burnt letter" *half-burnt-letter*))
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
  :health 82)

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
	      ((< distance 200)
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
wretched body beneath these robes.
What matters is the will of Shayol." :shayol)

(define-topic shayol gray-wizard
"Our master is sleeping at the moment.
But he will rise soon enough." :prescient-woman)

(define-topic prescient-woman gray-wizard 
"Aha! She isn't here. We think she's
long since died, actually." 
:then-why-are-you-here?)

(define-topic then-why-are-you-here? gray-wizard
"Because we want to ask you something.

Who is your true master? Whose Will do
you serve?" :quine)

(define-topic quine gray-wizard 
"Nonsense! Aren't you a servant of
Enceladus? You must be helping Her.
There is no other explanation! Don't
worry, I will deliver your Soul to her,
just like all the souls in the
Abyss!" :bye)

(defmethod die ((self gray-wizard))
  (play-sound self "death.wav")
  (let ((remains (new 'remains)))
    (add-inventory-item remains (new 'wax-cylinder))
    (drop self remains))
  (destroy self))

(defmethod discuss :after ((self gray-wizard) (topic (eql :quine)))
  (add-event :final-battle)
  (cue-music (current-scene) "presence.ogg")
  (pause))

(defmethod collide ((self wooden-arrow) (enemy gray-wizard))
  (damage enemy (random-choose '(-3 -5 -7)))
  (destroy self))

(defmethod collide ((self silver-arrow) (enemy gray-wizard))
  (damage enemy (random-choose '(-8 -12 -17)))
  (destroy self))


		  












