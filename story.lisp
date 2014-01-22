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
"Welcome to Cypress v0.8 (alpha) 

This is the Scroll of Helping. If you
click the scroll, you will advance to
the next page. Use the right mouse
button (or the Control key with the left
button) to close scrolls.

Right-click a destination to move
Geoffrey there.

Click an object to show its name.
Drag objects to move them.
Drag objects onto Geoffrey to take them.
 (Take and keep this scroll 
  for easy reference.)
Double-click an object to activate it.
Double-click a monster to attack it.
Double click Geoffrey for his inventory.
Drag items into/out of inventory scrolls.

Click spells in spellbook for
description.  Double click spells in
spellbook to cast.

Use the Travel spell to explore the
land.  Double-click land symbols to
explore regions.  If you die, press
Control-R for a new quest.

You must eat. Traveling and combat cause
hunger. Double-click food to eat it.
Use the \"Cure meat\" spell to make
jerky from freshly killed wolves.

Geoffrey must stay warm. He will get
colder by progressing through the
terrain (or by touching certain
objects). You have a magic tent and
campfire which you can use to heal and
warm yourself up. To use the tent, drag
it out of your inventory onto an open
space on the ground, and then cast
Spark.

There are several new keyboard shortcuts:

Press \"I\" to see Geoffrey's inventory.
Press \"S\" to open the spellbook.
Press \"P\" to pause (or spacebar).
Press \"M\" to open the travel map.

I hope you enjoy this work-in-progress
demonstration of Cypress.  Please submit
bug reports and feedback to me at
dto@blocky.io

-- David O'Toole
")

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
(defresource "path.ogg" :volume 20)
(defresource "flutism.ogg" :volume 20)

(defparameter *soundtrack*
'("passageway.ogg" "home.ogg" "kosmium.ogg" "believe-me2.ogg" "xolaros3.ogg" "path.ogg"
  "3-against-2.ogg" "dusk.ogg" "ruins.ogg" "standing-by-the-river.ogg" "spiritus.ogg"))

(defun make-quest (&optional (terrain-class 'meadow))
  (let ((geoffrey (new 'geoffrey))
	(buffer (new terrain-class)))
    (setf *map-row* 0)
    (setf *map-column* 0)
    (setf *map-screen* nil)
    (with-buffer buffer
      ;; (drop-object buffer (new 'lucius) (units 12) (units 8))
      (let ((scroll (new 'scroll)))
	(drop-object buffer scroll (units 10) (units 4))
	(activate scroll))
      (current-buffer))))


