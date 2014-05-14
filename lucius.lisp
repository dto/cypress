(in-package :cypress)

;;; Thrown pebbles

(defthing (pebble sprite) 
  :image-scale 200
  :height 20
  :width 20
  :sprite-height 20
  :sprite-width 20
  :clock 400
  :heading (/ pi 2)
  :image (random-choose *stone-images*))

(defmethod initialize ((self pebble) &key heading)
  (when heading
    (setf (field-value :heading self) heading)))

(defmethod collide ((self pebble) (thing thing))
  (when (solidp thing) 
    (destroy self)))

(defmethod run ((self pebble))
  (with-fields (clock heading) self
    (decf clock)
    (incf heading 0.002)
    (if (minusp clock)
	(destroy self)
	(forward self 14))))

(defmethod collide ((self pebble) (enemy enemy))
  (damage enemy (random-choose '(-2 -4)))
  (destroy self))

;;; Lucius Pentaquin

(defvar *lucius* nil)

(defun lucius () *lucius*)

(defthing (lucius monk) 
  :next-flower nil 
  :leader nil
  :clock 10 
  :met-player nil
  :description "Lucius")

(defmethod initialize :after ((lucius lucius) &key)
  (setf *lucius* lucius))

(defmethod throw-pebble ((self lucius) heading)
  (multiple-value-bind (x y) (center-point self)
    (play-sample "bow.wav")
    (percent-of-time 80 (bark self (random-choose '("Aha!" "Oh-ho!" "Ha-ha!" "Take that!" "Eat this!"))))
    (drop-object *current-scene* (new 'pebble :heading heading) x y)))

(defmethod attack :after ((monk geoffrey) (enemy enemy))
  (when (and (lucius) (field-value :leader (lucius)))
    (percent-of-time 70
      (throw-pebble (lucius) (heading-to-thing (lucius) enemy)))))

(defmethod collide ((monk lucius) (enemy enemy)) nil)

(defparameter *monk-2-walk* 
  '(:repeat t
    :scale 820
    :frames (("monk-2-walk-1.png" 4)
	     ("monk-2-walk-2.png" 4)
	     ("monk-2-walk-3.png" 4)
	     ("monk-2-walk-4.png" 4))))

(defmethod walking-animation ((self lucius))
  *monk-2-walk*)

(defparameter *monk-2-stand*
  '(:scale 820
    :frames (("monk-2-stand-1.png" 19)
	     ("monk-2-stand-2.png" 24))))

(defmethod standing-animation ((self lucius))
  *monk-2-stand*)
  
(defmethod walk-to :after ((monk lucius) x y)
  (with-fields (waypoints clock) monk
    (when (null waypoints)
      (choose-flower monk)
      (setf clock 20))))

(defmethod choose-flower ((self lucius))
  (setf (field-value :next-flower self)
	(let ((flowers (find-instances (current-scene) 'flower)))
	  (when flowers (random-choose flowers)))))

(defmethod follow ((self lucius) (leader monk))
  (setf (field-value :leader self) leader))

(defmethod unfollow ((self lucius))
  (setf (field-value :leader self) nil))

(defmethod run ((self lucius))
  (with-fields (next-flower leader met-player gump waypoints clock) self
    (call-next-method)
    (decf clock)
    (let ((distance (distance-between self (geoffrey))))
      (when (cursor)
	;; handle no-flowers
	(when (null (choose-flower self))
	  (walk-to-thing self (geoffrey)))
	;; handle first meeting
	(when (and (not met-player)
		   (< distance 300))
	  (setf met-player t)
	  (bark self "Ho, stranger!")
	  (walk-to-thing self (geoffrey)))
	(if (or leader gump)
	    ;; follow geoffrey
	    (progn (when (> distance 150)
		     (unless (or waypoints (plusp clock))
		       (multiple-value-bind (x y) (at (geoffrey))
			 (walk-to self x y)))))
		   ;; ;; warp to geoffrey when can't pathfind
		   ;; (multiple-value-bind (top left right bottom) 
		   ;;     (bounding-box (geoffrey))
		   ;;   (when (not (can-walk-to self left top))
		   ;;     (move-to self (- left 2) (- top 2)))))
	    ;; pick flowers
	    (when (and (null gump) 
		  (> distance 240))
	     (unless (plusp clock)
	       (if (null next-flower)
		   (choose-flower self)
		   (if (null waypoints)
		       (walk-to-thing self next-flower)
		       ;; are we near flower?
		       (when (< (distance-between self next-flower) 80)
			 (stop-walking self)
			 (setf clock 30)
			 (unless (nothbehem-p)
			   (take self next-flower))
			 (setf next-flower nil)))))))
	;; stop near geoffrey
	(when (< distance 110)
	  (prog1 nil (stop-walking self) (setf clock 10)))))))

(defmethod will-accept ((self lucius) (thing thing)) nil)

(defmethod activate ((self lucius))
  (destroy-gump self)
  (play-talk-sound self)
  (if (nothbehem-p)
      (if (field-value :locked (find-pentaquin-house))
	  (discuss self :house)
	  (discuss self :farewell))
      (if (null (field-value :leader self))
	  (discuss self :hello)
	  (discuss self :chat))))

(define-topic hello lucius
"Greetings, brother! Well met. I don't
recall ever seeing robes like yours! I
must admit you startled me; when I heard
your footsteps, I thought at first you
were a Wraith! But you seem human enough
to me. Salutations!"
:name :job :robes :wraith :where-are-we?)

(define-topic wraith lucius 
"You really aren't from around here, are
you? Wraiths are hideous conglomerations
of rags and bones, brought to living
Undeath by foul wandering Spirits who
crave the possession of a body. Wraiths
must be killed, lest they take your body
as their own."
:name :job :robes :where-are-we?)

(define-topic where-are-we? lucius
"You've reached the Vale of
Eavesbury. It must have been a difficult
journey through the mountains!"
:name :job :robes :quine :vale)

(define-topic name lucius
"My name is Lucius Pentaquin. And who
are you?  A monk, it seems, but of what
Order?"
:i-am-geoffrey-of-valisade)

(define-topic i-am-geoffrey-of-valisade lucius 
"It's nice to meet you, Brother Geoffrey
of Valisade! Welcome to our humble home."
:town :wraith :robes :quine :vale)

(define-topic vale lucius
"Yes, and a beautiful place it is!
Mountains to the south, and east, and
west. This is the vale of Eavesbury,
named for its discoverer, who found it
way, way back in whorememebers-time. In
those ages, the Sun was hotter than it
is now, and Eavesbury was a busy town
because of the mountain trade
routes. Now it's a frigid ruin, but
Nothbehem and a few other towns remain
in the warmer South. I've not been far
to the northeast, but I bet my
grandfather will know more."
:name :town :grandfather)

(define-topic job lucius 
"I'm a librarian at the Nothbehem
monastery. I'm also a maker and mender
of shirts, shoes, pants, robes, and
leather armor." :town :robes :quine)

(define-topic quine lucius 
"I don't think I've met anyone named
Quine myself, but we do see travelers
pass through town from time to
time. Maybe my grandfather would know?"
:grandfather)

(define-topic robes lucius 
"Yes. I haven't seen a style quite like
it. Although, the general fit, and the
stitching around the leather portions,
do remind me a bit of my grandfather's
old war gear. Tell me, are you a
soldier? Did you come across the
mountains from the West?" 
:west :grandfather :town)

(define-topic grandfather lucius 
  "Yes, the great Arturo Pentaquin the
Fourth! A decorated officer of the Wars
of the West. We should visit him in
Nothbehem; it's a short distance to the
north of here. He knows all about
Westerners, and stories about old
times." 
:west :town)

(define-topic west lucius 
  "I've never been out West myself, but
of course I've heard all the old
stories and read all the old books." 
:quine :robes :books :town)

(define-topic books lucius 
  "There are plenty of books, maps, and
scrolls at the Library where I work. 
You should visit the Monastery in town." 
:town)

(define-topic town lucius 
  "Nothbehem is my family's home, a
quiet farming town. I'm headed there
now, why don't you follow me? I'll teach
you a magic incantation I use to aid in
traveling through the valley. This spell
projects a parchment map into the mind
of the caster, with knowlege of nearby
terrain gathered from the eyes of birds
above. If you open your spellbook and
speak the incantation, you'll gain the
presence of mind required to travel in
this inhospitable clime.
Shall we get moving?"
 :go-with-lucius :talk-more)

(define-topic go-with-lucius lucius 
  "Very well! Let's head North." :bye)

(defmethod discuss :after ((self lucius) (topic (eql :go-with-lucius)))
  (destroy-gump self)
  (follow self (geoffrey))
  (learn-spell (geoffrey) (new 'travel))
  (cue-music (current-scene) (random-choose '("path.ogg" "lutey.ogg" "tumbling.ogg")))
  (activate (find-spellbook))
  (bark self "Excellent! Now, cast your travel spell!"))

(define-topic talk-more lucius 
  "Sure. What else do you want to talk
about?" :quine :wraith :grandfather :west :town)

(define-topic chat lucius
  "Let's keep moving. We don't have time
to sit around and talk, with the sun
setting." :bye)

(define-topic house lucius 
  "Here's my hometown! I'm so excited to
find out what my grandfather thinks of
you! Let's get to his house; it's the
fancy one with the three skylights." :bye)

(define-topic farewell lucius 
  "Best of luck on your journey, Geoffrey.
I'd come with you but, as you can see,
I'm something of a homebody. Besides, as
you've seen, I'm not very useful in a
fight! 

Oh! I almost forgot to tell you. I've
been hearing a weird sound in the forest
up to the northeast. It sounds like a
very strange bird, and plays a sad
song. I've never heard such a sound in
my life! I wonder if it's connected to
your appearance?

Take care, Traveler, and come
back soon.")

;;; Lucius can comment on things Geoffrey picks up.
