(in-package :cypress)

(defun make-meadow ()
    (let ((geoffrey (new 'geoffrey))
	  (lucius (new 'lucius))
	  (buffer (new 'cypress)))
      (add-object buffer geoffrey 320 120)
      (add-object buffer lucius 350 80)
      (add-object buffer (new 'scroll) 800 500)
      (add-object buffer (new 'white-bread) 860 600)
      (add-object buffer (new 'wheat-bread) 900 700)
      (add-object buffer (new 'wraith) 1000 900)
      ;; adjust scrolling parameters 
      (setf (%window-scrolling-speed buffer) (cfloat (/ *monk-speed* 3))
	    (%horizontal-scrolling-margin buffer) 2/5
	    (%vertical-scrolling-margin buffer) 4/7)
      ;;
      (resize-to-background-image buffer)
      (set-cursor buffer geoffrey)
      (snap-window-to-cursor buffer)
      (glide-window-to-cursor buffer)
      (follow-with-camera buffer geoffrey)

      (drop-object buffer (new 'tent) 400 400)
      (drop-object buffer (new 'tent) 800 800)
      
      ;; (drop-object buffer (new 'circle-key) 420 700)
      ;; (drop-object buffer (new 'triangle-key) 420 850)
      ;; (drop-object buffer (new 'xalcium-leggings) 400 400)
      ;; (drop-object buffer (new 'xalcium-armor) 420 700)
      ;; (drop-object buffer (new 'xalcium-mail) 420 850)
      ;; (dotimes (n 8)
      ;; 	(let ((x (+ 300 (random 1500)))
      ;; 	      (y (+ 300 (random 1000))))
      ;; 	  (drop-object buffer (new 'gray-rock))) x y)

      ;; allocate
       (install-quadtree buffer)
      buffer))

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
  "to reconstruct what happened in the
centuries after our disappearance. It
would seem that Valisade became the seat
of a vast imperial power due to its
harnessing of the magic mineral
Xalcium. Valisade's warrior-priest kings
ruled over Ildran for a thousand years,
but this was disrupted by a sudden
catastrophe whose cause and nature are
not yet fully clear to us. An explosion
of some kind, perhaps a volcanic
eruption, obliterated completely the
isles of Einhold and Mir; much of the
surrounding coastal areas were laid
waste. 

A rain of ash fell over the entire
continent; this was followed by a year
of perpetual dusk, in which the sun was
barely visible through the black
clouds. The pollution of the water
supply and failure of their crops led to
widespread famine, disease, and death.
")

(defparameter *letter-text-2* 
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
")

