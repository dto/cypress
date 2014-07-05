(in-package :cypress)

(defun find-owl ()
  (first (find-instances (current-scene) 'owl)))

(defparameter *neume-images* (image-set "neumes" 7))

(defthing neume :scale 0.6 :image (random-choose *neume-images*) :stacking t :description "neumes")

(defthing bone-flute :scale 0.7 :image "bone-flute.png" :stacking nil)

(defmethod activate ((flute bone-flute))
  (if (has-quantity (geoffrey) 'neume 3)
      (progn (cue-music (current-scene) "flutism.ogg")
	     (narrate "You play the flute!")
	     (when (find-owl)
	       (setf (field-value :heard-flute (find-owl)) t)))
      (bark (geoffrey) "I don't have any music to play.")))

(defthing music-book :scale 0.8 :image "music-book.png" :stacking nil)

(defmethod find-description ((book music-book))
  "Musical compendium")

(defmethod can-accept ((book music-book)) t)
(defmethod will-accept ((book music-book) (thing thing)) nil)
(defmethod will-accept ((book music-book) (neume neume)) t)

(defmethod activate ((music-book music-book))
  (replace-gump music-book (new 'browser :container music-book)))

(defun find-music-book ()
  (find-inventory-item (geoffrey) 'music-book))

(defresource "owl.wav" :volume 08)

(defparameter *owl-images* (image-set "owl" 5))
(defparameter *owl-flap-images* '("owl-6.png" "owl-7.png"))

(defthing (owl sprite)
  :given-gears nil
  :image-scale 700
  :heard-flute nil
  :sprite-height 130
  :sprite-width 130
  :health 15
  :description "giant owl"
  :image (random-choose *owl-images*))

(defmethod can-pick ((owl owl)) nil)

(defmethod activate ((owl owl))
  (destroy-gump owl)
  (setf (field-value :gump owl) nil)
  (cue-music (current-scene)
	     (random-choose '("believe-me2.ogg" "xolaros3.ogg")))
  (if (not (field-value :heard-flute owl))
      (discuss owl :hello)
      (discuss owl :confirmed)))

(defmethod enter-scene ((owl owl))
  (play-sample "owl.wav")
  (show-hint "You hear a strange bird-song."))

(defmethod run ((owl owl))
  (percent-of-time 9
    (setf (field-value :image owl) (random-choose *owl-images*))
    (setf (field-value :heading owl) (heading-to-cursor owl))))

(define-topic hello owl 
  "Greetings, Geoffrey." :who-are-you?)

(define-topic who-are-you? owl
  "I am the emissary Screech Owl of this forest.
I am sworn to deliver an ancient
message to a Brother Geoffrey of Valisade.
Are you not he?" :i-am-geoffrey)

(define-topic i-am-geoffrey owl
  "Good! Nonetheless, I am bound by oath
to await the proper musical
introduction, as proof that the correct
Geoffrey has come to claim the
message. These things have to be done in
a particular order, you see. We can't
have messages being delivered out of
order. Always creates problems!
Always. So. Unless you're ready with the
music, I haven't got anything for
you." :music)

(define-topic music owl
  "I must have proof! And the Geoffrey I
want is supposed to be playing
particular notes on a bone flute, so
that I know it's him. Come back when
you've got the flute, and maybe I'll
give you a hint about the music."
:hint)

(define-topic hint owl 
  "Hmm. Well, seeing as you don't have
the flute, I'll help you just a
little. You'll need to play a three-part
funeral dirge. But I can't tell ye where
to get the music, nor where to find the
flute. Have fun!" :bye)

(define-topic confirmed owl
"Beautifully played! It's an honor to
meet the real Geoffrey. You must be
awfully confused by all this talk of
time travel! So let me summarize the
situation. Dr. Quine is not dead!" 
:quine)

(define-topic quine owl
"Actually, he is engaged in a most
extraordinary game of Chess. But the
chessboard is Time itself, and the
pieces are such mortals as you and
Lucius. Even Dr. Quine himself is one of
the pieces. And his opponent is the
Wizard Master, Shayol---the Lord High
Priest of the black-robed Acolytes of
the Abyss." :abyss)

(define-topic abyss owl 
"Their cult worships a mountain God who
gives life to the River Abyss. In truth,
they drink only Undeath from those
waters---and are slowly, by degrees,
turned into Shades by its influence." 
  :shades)

(define-topic shades owl
  "These odious, ichor-swilling Black
Wizards serve the will of Shayol. There
are three who've stepped into this Time,
into this Vale, and they are preparing
to slay you at this very moment. For
Lord Shayol desires very much to best
Dr. Quine in that fateful game of Chess!
And you are a very valuable
piece." :where-are-they?)

(define-topic where-are-they? owl 
  "You must confront them now, while
they gather their charms and mix their
potions. Their camp is in the ruins far
to the southeast. There's a cave just
south of the Waystone near the ruins; go
to the cave first! You'll find what you
need to survive against the Wizards.
Take these ancient Gears; you can
use them to unlock the cave. 
Go now, Geoffrey!" :bye)

(defmethod discuss :after ((owl owl) (topic (eql :where-are-they?)))
  (with-fields (given-gears) owl
    (when (not given-gears)
      (setf given-gears t)
      (drop owl (quantity-of 'copper-gear 1)
	    (units 5) (units 5))
      (drop owl (quantity-of 'copper-gear 1)
	    (units 6) (units 6))
      (drop owl (quantity-of 'copper-gear 1)
	    (units 7) (units 7)))))

;;; Hidden owl garden

(defthing (owl-garden scene)
  :background-image "stone-road.png")

(defmethod map-icon ((self owl-garden)) (random-choose *forest-icons*))

(defmethod find-description ((self owl-garden)) "forest")

(defun small-fence ()
  (with-border (units 3)
    (apply #'lined-up (mapcar #'singleton
				(list 
				(new 'iron-fence)
				(new 'iron-fence)
				(new 'iron-fence)
				(new 'iron-fence)
				(new 'iron-fence)
				(new 'iron-fence)
				(new 'iron-fence)
				(new 'iron-fence)
				(new 'iron-fence)
				(new 'iron-fence))))))

(defthing special-gravestone 
  :tags '(:solid :fixed) 
  :description "gravestone with neumes"
  :image (random-choose *gravestone-images*))

(defmethod enter-scene ((self special-gravestone))
  (add-inventory-item self (new 'neume)))

(defmethod activate ((self special-gravestone))
  (replace-gump self (new 'browser :container self)))

(defun gravestone ()
  (singleton (new 'gravestone)))

(defun gravestone-with-neume ()
  (singleton (new 'special-gravestone)))

(defun graves-with-neumes ()
  (let (rows)
    (dotimes (n 3)
      (push (lined-up-randomly (gravestone) 
			       (with-border (units 2) (gravestone))
			       (gravestone-with-neume)
			       (gravestone) 
			       (with-border (units 3) (gravestone))
			       (gravestone))
	    rows))
    (with-border (units 4) 
      (apply #'stacked-up-randomly rows))))

(defun owl-garden ()
  (stacked-up (small-fence)
	      (with-border (units 10)
		(stacked-up 
		 (lined-up-randomly (flowers) (gravestone) (gravestone))
		 (lined-up-randomly (with-border (units 8) (singleton (new 'waystone))) 	
				    (with-border (units 4) (singleton (new 'owl))))
		 
		 (lined-up-randomly (meadow-debris) (flowers) (gravestone) (gravestone))))
	      (small-fence)))

(defmethod make-terrain ((self owl-garden))
  (with-border (units 12)
    (stacked-up 
     (lined-up (some-trees) (some-trees) (some-trees))
     (lined-up (flowers) (owl-garden) (flowers))
     (lined-up (some-trees) (some-trees) (some-trees)))))

;;; Cemetery

(defthing (cemetery scene)
  :background-image (random-choose '("forgotten-meadow.png" "paynes-meadow.png")))

(defun row-of-graves ()
  (with-border (units (+ 2 (random 3)))
    (apply #'lined-up-randomly (mapcar #'singleton (grab '(gravestone) (+ 2 (random 4)))))))

(defun some-graves ()
  (let (rows)
    (dotimes (n (+ 2 (random 2)))
      (push (row-of-graves) rows))
    (with-border (units 4) 
      (apply #'stacked-up-randomly rows))))

(defmethod make-terrain ((cemetery cemetery))
  (with-border (units 10)
    (lined-up-randomly 
     (stacked-up-randomly (wood-pile) (spatter 'bone-dust) (spray 'iron-fence :count (+ 2 (random 3))) (some-graves) (dense-trees) (singleton (new 'grave-hag)) (spatter '(nightshade ginseng)))
     (stacked-up-randomly (dead-trees) (spray 'iron-fence :count (+ 2 (random 3)))
			  ;; (singleton (new 'stone-stairwell)) 
			  (some-trees))
     (stacked-up-randomly (dead-trees) (spray 'iron-fence :count (+ 2 (random 3))) (graves-with-neumes) (spray 'iron-fence :count (+ 2 (random 3))) (singleton (new 'grave-hag)) (spray 'bone-dust) (singleton (new 'iron-fence)) (flowers)))))

(defmethod expend-travel-cost ((cemetery cemetery))
  (modify-hunger (geoffrey) 8)
  (chill (geoffrey) +35))





