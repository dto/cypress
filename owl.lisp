(in-package :cypress)

(defresource "owl.wav" :volume 08)

(defparameter *owl-images* (image-set "owl" 5))
(defparameter *owl-flap-images* '("owl-6.png" "owl-7.png"))

(defthing (owl sprite)
  :image-scale 700
  :sprite-height 130
  :sprite-width 130
  :health 15
  :image (random-choose *owl-images*))

(defmethod can-pick ((owl owl)) nil)

(defmethod activate ((owl owl))
  (cue-music (current-scene)
	     (random-choose '("believe-me2.ogg" "xolaros3.ogg")))
  (discuss owl :hello))

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
you've got it!" :bye)

;;; Hidden cemetery

(defthing (hidden-cemetery scene)
  :background-image "stone-road.png")

(defmethod map-icon ((self hidden-cemetery)) (random-choose *forest-icons*))

(defmethod find-description ((self hidden-cemetery)) "forest")

(defun small-fence ()
  (with-border (units 3)
    (apply #'stacked-up (mapcar #'singleton
				(list 
				(new 'iron-fence)
				(new 'iron-fence)
				(new 'iron-fence)
				(new 'iron-fence)
				(new 'iron-fence))))))
(defun small-cemetery ()
  (lined-up (small-fence)
	    (stacked-up 
	     (flowers)
	     (some-graves)
	     (singleton (new 'owl))
	     (flowers))
	    (small-fence)))

(defmethod make-terrain ((self hidden-cemetery))
  (with-border (units 12)
    (stacked-up 
     (lined-up (some-trees) (some-trees) (some-trees))
     (lined-up (flowers) (small-cemetery) (flowers))
     (lined-up (some-trees) (some-trees) (some-trees)))))





