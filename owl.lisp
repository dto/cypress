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
      (bark (geoffrey) "I don't have enough neumes.")))

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
  :image-scale 700
  :heard-flute nil
  :sprite-height 130
  :sprite-width 130
  :health 15
  :image (random-choose *owl-images*))

(defmethod can-pick ((owl owl)) nil)

(defmethod activate ((owl owl))
  (destroy-gump owl)
  (setf (field-value :gump owl) nil)
  (cue-music (current-scene)
	     (random-choose '("believe-me2.ogg" "xolaros3.ogg")))
  (if (not (field-value :heard-flute owl))
      (discuss owl :hello)
      (discuss owl :scroll)))

(defmethod enter-scene ((owl owl))
  (play-sample "owl.wav")
  (bark (geoffrey) "I hear something!"))

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
scroll. These things have to be done in
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

(define-topic scroll owl
"It's an honor to meet the real Geoffrey.
Here is the scroll I've been instructed
to give you." :bye)

(defmethod discuss :after ((owl owl) (topic (eql :scroll)))
  (drop owl (make-scroll "strange poem" *amalia-poem*)
	(units 5) (units 5)))




;;; Hidden cemetery

(defthing (hidden-cemetery scene)
  :background-image "stone-road.png")

(defmethod map-icon ((self hidden-cemetery)) (random-choose *forest-icons*))

(defmethod find-description ((self hidden-cemetery)) "forest")

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
	      (stacked-up 
	       (flowers)
	       (with-border (units 8) (singleton (new 'waystone)))
	       (with-border (units 4) (singleton (new 'owl)))
	       (flowers) 
	      (small-fence))))

;; (learn-spell (geoffrey) (new 'travel))

(defmethod make-terrain ((self hidden-cemetery))
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





