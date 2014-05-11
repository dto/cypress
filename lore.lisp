(in-package :cypress)

(defparameter *amalia-report*
"In Nothbehem before the Western Wars,
there was born a girl-child, Amalia,
with the seeming power of Premonition.

While in a trance-like state she would
write mysterious messages in a strange,
angular hand. These prophesied an
eclipse of the Sun not once but three
times in her long life. Her visions of a
great Hurricane helped spare countless
souls from drowning.

One day when Amalia had grown old, the
residents of Nothbehem awoke to discover
her little house burning furiously,
while she stood outside watching.

\"I shall give no more prophecies. I
shall leave for the mountains, and never
speak again.\" The townspeople begged
her to stay, but she would not listen.

They say she must have written something
terrible while in her altered state, and
burned the house in order to destroy
it. Now and then, travelers would claim
to have seen an old woman up in the
mountains---but only ever from a
distance. Stories about Amalia
circulated for centuries.")

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

(defparameter *quine-letter-1* 
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
Rama's Maps of the Cosmos, we drew
charts of our own and found that ages
had passed since our departure from
Nothbehem.

If we read these charts aright, then by
the time you read these words your
brother and I will have been dead for
more than five thousand years. For as we
crossed the mountains into the North,
each of us fell prey to magicks
involving Time, such that a delay in one
day's departure meant the passage of
five millennia.

You stand on the soil of of a continent
ten thousand years older than the land
you left.

 (the rest of the scroll has crumbled)
")

(defparameter *quine-letter-2*
  "Dear Geoffrey, 

Your brother and I have begun to
reconstruct what happened in the
centuries after our disappearance. It
would seem that Valisade became the seat
of a vast imperial power due to its
harnessing of the magic mineral
Xalcium. Valisade's warrior-priest kings
ruled over Ildron for a thousand years,
but this was disrupted by a sudden
catastrophe whose cause and nature are
not yet fully clear to us. An explosion
of some kind, perhaps a volcanic
eruption, obliterated completely the
isles of Einhall and Mir; much of the
surrounding coastal areas were laid
waste.

A rain of ash fell over the entire
continent; this was followed by a year
of perpetual dusk, in which the sun was
barely visible through the black
clouds. The pollution of the water
supply and failure of their crops led to
widespread famine, disease, and death.

 (the rest of the scroll is unreadable.)
")

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
Sages.

 (the rest of the scroll has crumbled.)")

(defthing scroll-fragment
  :stacking nil
  :image (random-choose *scroll-images*) 
  :text  (random-choose
	  (list 
		*amalia-poem*)))

(defmethod activate ((self scroll-fragment))
  (drop self (new 'scroll-gump
		  :text (field-value :text self))))


