(in-package :cypress)

;;; cypress tree, amalia house

(defthing quill-pen :image "quill-pen.png")

(defthing inkwell :image "inkwell.png")

(defthing white-cypress
  :image "cypress.png" 
  :scale 2.0
  :tags '(:solid :fixed))

(defthing burnt-cabin
  :activated nil
  :scale 1.4
  :tags '(:solid :fixed)
  :image "burnt-cabin.png")

(defparameter *amalia-prophecy* 
"There is a storm coming.
A fury called \"Aegis\". The tears of
Ildron will fall, like sand, for a
hundred thousand years when \"Aegis\"
comes. Lost souls shall fill the Abyss
to overflowing, O \"Aegis\". For its
form is that ... s..g de... sk....

 (the rest of the scroll is burnt)
")

(defmethod activate ((cabin burnt-cabin))
  (if (find-enemies)
      (bark (geoffrey) "Not with enemies nearby!")
      (with-fields (activated) cabin
	(when (not activated)
	  (setf activated t)
	  (add-inventory-item cabin (new 'inkwell))
	  (add-inventory-item cabin (new 'quill-pen))
	  (add-inventory-item cabin (make-scroll "scroll fragment" *amalia-prophecy*)))
	(replace-gump cabin (new 'browser :container cabin)))))

(defparameter *amalia-poem*
"My name is Amalia.

I am the spirit who lives 
in the White Cypress.

Like those who dwelt therein before, 
I guide the souls of Ildron 
as they pass between worlds.

I will tell thee now of an age 
whose very stones have crumbled,
so long ago began 
this tapestry of Sorrows.

When ash and smoke concealed 
the fury of the Sun.

When famine and despair
swept through Ildron.

When the Cypress died, 
leaving souls to roam
in Shadow.

When a man of humility,
with little more than
robe and longbow, 
passed into the history
of Heroes.")

(defthing (amalia-ruins scene)
  :background-image (random-choose *frozen-meadow-images*))

(defmethod begin-scene :after ((scene amalia-ruins))
  (cue-music scene "constellation.ogg"))

(defmethod find-description ((scene amalia-ruins)) "Frozen forest")

(defmethod update :after ((scene amalia-ruins))
  (with-fields (height width) scene
    (when (< width *nominal-screen-width*) 
      (resize scene (+ width 100) height))))

(defmethod make-terrain ((scene amalia-ruins))
  (with-border (units 10)
    (lined-up-randomly
     (stacked-up-randomly (singleton (new 'ruin-wall)) 
			  (lined-up (singleton (new 'burnt-cabin)) 
				    (with-border (units 5) (singleton (new 'gray-wizard))))
			  (singleton (new 'well)) (dead-trees))
     (stacked-up-randomly (singleton (new 'puddle)) (spray 'gray-rock :trim nil :count 5) (singleton (new 'white-cypress)) (singleton (new 'ruin-wall)) (dead-trees)))))

