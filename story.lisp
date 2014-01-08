(in-package :cypress)

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

(defparameter *help-text* 
"Welcome to Cypress. 

Press Control-R to get a new forest.
Press Spacebar to pause the action.
Single-click an object to show its name.
Right-click a destination to move there.
Drag objects to move them.
Double-click monster to attack it.
Drag object onto monks to take item.
Double click container for inventory.
Right click scrolls to close.
Drag items into/out of inventory.")

(defparameter *poem-1*
"My name is Amalia.

I am the spirit who lives 
 in the White Cypress.
Like those who dwelt therein before, 
I guide the souls of Ildran 
 as they pass between worlds.

I will tell you now 
of an ancient time,

When ash and smoke concealed 
the fury of Sol.")

(defparameter *poem-2*
"When ash and smoke concealed 
the fury of Sol.

When famine and despair
swept over Ildran.

When the Cypress died, 
leaving souls to wander
without rest.

When a man of humility,
with little more than
robe and longbow, 
passed into the history
of Heroes.")

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
		*poem-1*
		*poem-2*
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

(defparameter *soundtrack*
'("passageway.ogg" "home.ogg" "kosmium.ogg" "believe-me2.ogg" "xolaros3.ogg"
  "3-against-2.ogg" "dusk.ogg" "ruins.ogg" "standing-by-the-river.ogg" "spiritus.ogg"))

(defresource "nice-map.png")

(defparameter *map-scroll-speed* 0.3)
(defparameter *map-zoom-speed* 0.9)

(defthing old-map :image "fine-map.png" :width (* 3200 0.9) :height (* 2502 0.9))

(define-method run old-map ()
  (move-toward self :left *map-scroll-speed*)
  (resize self 
	  (- %width *map-zoom-speed*)
	  (- %height *map-zoom-speed*)))

(defresource "dusk.ogg" :volume 40)

(defun show-old-map ()
  (switch-to-buffer (new 'buffer))
  (resize (current-buffer) 3000 3000)
  (let ((map (new 'old-map)))
    (insert map -400 0))
  (play-music "dusk.ogg" :loop t))

(defun make-meadow ()
  (let ((geoffrey (new 'geoffrey))
;;	(lucius (new 'lucius))
	(buffer (new 'scene))
	(forest (trim (make-forest))))
    (with-buffer buffer
      (let ((height (field-value :height forest))
	    (width (field-value :width forest)))
	(paste-from buffer (with-border 250 forest))
	(resize buffer (+ width 600) (+ height 400)))
      (drop-object buffer geoffrey 120 120)
;;      (drop-object buffer lucius 180 80)
      (drop-object buffer (new 'scroll) 270 100 0)
      ;; adjust scrolling parameters 
      (setf (%window-scrolling-speed buffer) (cfloat (/ *monk-speed* 3))
	    (%horizontal-scrolling-margin buffer) 2/5
	    (%vertical-scrolling-margin buffer) 4/7)
      ;;
      (set-cursor buffer geoffrey)
      (snap-window-to-cursor buffer)
      (glide-window-to-cursor buffer)
      (follow-with-camera buffer geoffrey)
      
      ;; allocate
      (install-quadtree buffer)
      ;; (play-music (random-choose *soundtrack*) :loop t)
      (current-buffer))))


