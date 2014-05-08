(in-package :cypress)

(defparameter *amalia-poem*
"My name is Amalia.

I am the spirit who lives 
in the White Cypress.

Like those who dwelt therein before, 
I guide the souls of Ildran 
as they pass between worlds.

I will tell thee now of an age 
whose very stones have crumbled,
so long ago began 
this tapestry of Sorrows.

When ash and smoke concealed 
the fury of the Sun.

When famine and despair
swept through Ildran.

When the Cypress died, 
leaving souls to roam
in Shadow.

When a man of humility,
with little more than
robe and longbow, 
passed into the history
of Heroes.")

(defparameter *skull-lore*
  '("...who disturbs me?
 ....
I can't find my way home...
 ...
Do you know which way lies 
the town of Nothbehem?"

"One, two, three.
That's violets here, 
Forget-me-nots!
Little ones shouldn't speak
to the Dead!
Oh, Goodness.

... Who is that?"

"Go away! I've got nothing to say to
anybody, living or dead!"

"Stay away from the caves, my son.
Ancient mecha-men dwell there; watchmen
of the Ages, who cannot die.
Ye may die to learn their secrets!"

"So many thousands of years...
trapped beneath Mount Einhall... 

their voices came to us through the
Wizards' magic glass..."

"We built a digging machine called the
Diamond Anvil. For nine years my family
and I slaved at the construction."))


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
Nothbehem.

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

