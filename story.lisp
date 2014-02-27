(in-package :cypress)

(defparameter *wax-cylinder-letter*
"The alliance between the Black Wizards
and the Industrialists sparked the
invention of a most extraordinary
machine. The Wizards had secretly
discovered a method for making very thin
sheets of volcanic glass, with strange
optical and vibrational properties.
Pieces of a certain shape could condense
sunlight into flame; others could emit
ringing tones when struck.

The Industrialists affixed a carving
needle to a certain sheet of this glass,
made to an even more rarefied thinness,
such that highly subtle vibrations of
the Aether would be registered in its
movements. By setting this needle so
that it just barely scratches a rotating
wooden cylinder, which has been
beforehand coated with a thin layer of
wax, the spiritual or magical
vibrations, as such, are actually
recorded in the subtle wigglings of the
spiral left on the cylinder's
surface. By means of another similar
machine, these wigglings are magically
transformed into sound by vibrating
sheets of this same volcanic glass, and
can be listened to repeatedly.

Soon it was discovered that additional
voices could be heard on some cylinders,
that had not been witnessed by those
present at the recording. 

For this clever machine had suddenly
ripped open the doorway to communication
with previously unkown Spirits, both
good and Evil; inhabiting strange
domains of Being which had never
heretofore been imagined, by even the
Sages.")

(defparameter *quine-summons*
"Dear Geoffrey,

Francis and I are leaving for Valisade
immediately. We should have arrived by
the time you receive this letter. Please
meet us at the Cathedral as soon as you
possibly can. This is an urgent matter
but I cannot commit more to writing, as
I must depart at once. 

Make haste to Valisade, and be safe, my
friend.

--- Dr. Quine
")

(defparameter *letter-text*
  "...to reconstruct what happened in
the centuries after our
disappearance. It would seem that
Valisade became the seat of a vast
imperial power due to its harnessing of
the magic mineral Xalcium. Valisade's
warrior-priest kings ruled over Ildran
for a thousand years, but this was
disrupted by a sudden catastrophe whose
cause and nature are not yet fully clear
to us. An explosion of some kind,
perhaps a volcanic eruption, obliterated
completely the isles of Einhold and Mir;
much of the surrounding coastal areas
were laid waste.

A rain of ash fell over the entire
continent; this was followed by a year
of perpetual dusk, in which the sun was
barely visible through the black
clouds. The pollution of the water
supply and failure of their crops led to
widespread famine, disease, and death.

 (the rest of the scroll is unreadable.)
")

(defparameter *letter-text-2*
  "Dear Geoffrey, I have to abandon my
second camp near the ruins! Take what
you like from my tent, and follow me to
the south. But watch out for Wraiths!

Sorry for the wild goose-chase. 

-- Dr. Quine
")

(defparameter *letter-text-4* 
  "Dear Geoffrey,

I'm sending this letter by Raven to my
camp so that you'll know what happened.
I had to leave quite suddenly, and left
a few things behind!

You will find in my tent a spare
Hearthstone, my expedition journal, and
a few more things in a satchel beneath
my bed. I'll be back tomorrow evening
and can meet you at the Library
then. Can you bring my things with you?
Although you can keep the Hearthstone as
a gift.

I've got to get moving! See you soon.

 -- Dr. Quine
")

(defparameter *letter-text-3* 
  "Dear Geoffrey,

Our fondest hope is that this letter
finds you, and in good health. For the
message it contains will seem
predestined to break your Spirit.

Surely you noticed that the weather had
suddenly grown cold, and that the
vegetation had withered as if touched by
some sort of plague?

We failed to understand this ourselves,
until nightfall came and we saw that the
positions of the stars had changed so
much as to render several constellations
nearly unrecognizable. As Francis had
studied the ancients' commentaries on
Rama's maps of the Cosmos, we drew
charts of our own and found that ages
had passed since our departure from
Nothbess.

If we read these charts aright, then by
the time you read these words your
brother and I will have been dead for
more than five thousand years. For when
we crossed into Valisade, each of us
fell prey to magicks involving Time,
such that a delay in one day's departure
meant the passage of five millennia.

You stand on the soil of of a continent
ten thousand years older than the land
you left.

 (the rest of the scroll has crumbled)
")

(defthing scroll-fragment
  :stacking nil
  :image (random-choose *scroll-images*) 
  :text  (random-choose
	  (list *letter-text*
		*amalia-poem*
		*letter-text-3*)))

(defmethod activate ((self scroll-fragment))
  (drop self (new 'scroll-gump
		  :text (field-value :text self))))

(defresource "passageway.ogg" :volume 20)
(defresource "home.ogg" :volume 10)
(defresource "kosmium.ogg" :volume 20)
(defresource "believe-me2.ogg" :volume 20)
(defresource "3-against-2.ogg" :volume 20)
(defresource "xolaros3.ogg" :volume 20)
(defresource "dusk.ogg" :volume 20)
(defresource "ruins.ogg" :volume 10)
(defresource "standing-by-the-river.ogg" :volume 10)
(defresource "spiritus.ogg" :volume 20)
(defresource "path.ogg" :volume 20)
(defresource "flutism.ogg" :volume 20)

(defparameter *soundtrack*
'("passageway.ogg" "home.ogg" "kosmium.ogg" "believe-me2.ogg" "xolaros3.ogg" "path.ogg"
  "3-against-2.ogg" "dusk.ogg" "ruins.ogg" "standing-by-the-river.ogg" "spiritus.ogg"))

(defresource "prologue.ogg" :volume 70)

(defparameter *prologue-height* 1578)

(define-buffer prologue 
  (start-time :initform *updates*)
  (quadtree-depth :initform 4)
  (background-color :initform "black"))

(defthing mountain-foreground
  :scale 2.2
  :image "mountain-foreground.png")

(defmethod run ((self mountain-foreground))
  (move-toward self :right 0.17))

(defthing mountain-background
  :scale 2.2
  :image "mountain-background.png")

(defmethod run ((self mountain-background))
  (move-toward self :left 0.4))

(defthing mountain-sky
  :scale 2.2
  :image "mountain-sky.png")

(defmethod run ((self mountain-sky))
  (move-toward self :right 0.10))

(defthing fine-map
  :scale 2.2
  :image "fine-map.png")

(defmethod run ((self fine-map))
  (move-toward self :upleft 0.14))

(defthing smoke-map
  :scale 2.1
  :image "smoke.png")

(defmethod run ((self smoke-map))
  (move-toward self :upleft 0.12)
  (with-local-fields
    (resize self (* %width 1.0003) (* %height 1.0003))))

(defthing shadows
  :scale 2.3
  :image "souls.png")

(defmethod run ((self shadows))
  (move-toward self :left 0.25))

(defthing destiny
  :scale 2.6
  :image "destiny.png")

(defmethod run ((self destiny))
  (move-toward self :left 0.25))

(defthing guiding
  :scale 2.4
  :image "guiding.png")

(defmethod run ((self guiding))
  (move-toward self :upleft 0.1)
  (with-local-fields
    (resize self (* %width 1.0003) (* %height 1.0003))))

(defthing amalia
  :opacity 0.0
  :scale 2.4
  :image "amalia.png")

(defmethod run ((self amalia))
  (with-local-fields
    (move-toward self :upleft 0.24)
    (resize self (* %width 1.0003) (* %height 1.0003))
    (setf %opacity (min 1.0 (+ %opacity 0.001)))))

(defmethod draw ((self amalia))
  (with-local-fields 
    (draw-textured-rectangle %x %y 0.0
			     %width %height
			     (find-texture %image)
			     :opacity 0.1)))

(defthing hero
  :scale 2.4
  :image "hero.png")

(defmethod run ((self hero))
  (move-toward self :upleft 0.24)
  (with-local-fields
    (resize self (* %width 1.0003) (* %height 1.0003))))

(defthing shade
  :scale 2.2
  :image "shade.png")

(defmethod run ((self shade))
  (move-toward self :upleft 0.2)
  (with-local-fields
    (resize self (* %width 1.0003) (* %height 1.0003))))

(defparameter *amalia* (seconds->frames 8))
(defparameter *guiding* (seconds->frames 18))
(defparameter *tell-thee-now* (seconds->frames 33))
(defparameter *famine* (seconds->frames 42))
(defparameter *souls* (seconds->frames 50.5))
(defparameter *map-time* (seconds->frames 58))
(defparameter *destinies* (seconds->frames 71))
(defparameter *shade* (seconds->frames 79))

(defmethod initialize :after ((self prologue) &key)
  (resize self 1280 720)
  (drop-object self (new 'amalia) -240 -240)
  (play-music "prologue.ogg" :loop nil))

(defmethod momentp ((self prologue) time)
  (= *updates* (+ time (field-value :start-time self))))

(defmethod clear-objects ((self prologue))
  (dolist (object (get-objects self))
    (destroy object)))

(defmethod update :after ((self prologue))
  (cond ((momentp self *guiding*)
	 (clear-objects self)
	 (drop-object self (new 'guiding) 0 -100))
	((momentp self *famine*)
	 (clear-objects self)
	 (drop-object self (new 'smoke-map) -50 -150))
	((momentp self *tell-thee-now*)
	 (clear-objects self)
	 ;; (drop-object self (new 'mountain-sky) -200 0)
	 ;; (drop-object self (new 'mountain-foreground) -250 100)
	 (drop-object self (new 'mountain-background) -100 0))
	((momentp self *souls*)
	 (clear-objects self)
	 (drop-object self (new 'shadows) -100 -100))
	((momentp self *destinies*)
	 (clear-objects self)
	 (drop-object self (new 'destiny) 0 0))
	((momentp self *map-time*)
	 (clear-objects self)
	 (drop-object self (new 'hero) -200 -300))
	((momentp self *shade*)
	 (clear-objects self)
	 (drop-object self (new 'shade) 0 0))))
	 

	   


