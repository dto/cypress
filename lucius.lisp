(in-package :cypress)

;;; Lucius Pentaquin

(defthing (lucius monk) 
  :next-flower nil 
  :clock 10 
  :seen-player nil
  :description "Lucius")

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
	(let ((flowers (find-instances (current-buffer) 'flower)))
	  (when flowers (random-choose flowers)))))

(defmethod run ((self lucius))
  (with-fields (next-flower seen-player gump waypoints clock) self
    (call-next-method)
    (decf clock)
    (let ((distance (distance-to-cursor self)))
      (when (cursor)
	(when (and (not seen-player)
		   (< distance 300))
	  (setf seen-player t)
	  (bark self "Ho, stranger!")
	  (walk-to-thing self (cursor)))
	(cond 
	  ((and (null gump) 
		(> distance 240))
	   ;; pick flowers
	   (unless (plusp clock)
	     (if (null next-flower)
		 (choose-flower self)
		 (if (null waypoints)
		     (walk-to-thing self next-flower)
		     ;; are we near flower?
		     (when (< (distance-between self next-flower) 80)
		       (stop-walking self)
		       (setf clock 30)
		       (take self next-flower)
		       (setf next-flower nil))))))
	  ((> distance 150)
	   (unless (or waypoints (null gump) (plusp clock))
	     (multiple-value-bind (x y) (at (cursor))
	       (walk-to self x y))))
	  ((> distance 110)
	   (prog1 nil (stop-walking self) (setf clock 10))))))))

(defmethod will-accept ((self lucius) (thing thing)) nil)

(defmethod activate ((self lucius))
  (destroy-gump self)
  (discuss self :hello))

(define-topic hello lucius
"Greetings, brother. Well met. I don't
recall ever seeing robes like yours!
You must be a traveler?"
  :robes :where-are-we?)

(define-topic where-are-we? lucius
"We're just outside the little town of
Nothbehem. You really have no idea where
you are, do you?" :robes :quine)

(define-topic name lucius
"My name is Lucius Pentaquin. And who
are you?  A monk, it seems, but of what
Order?"
:i-am-geoffrey-of-valisade)

(define-topic i-am-geoffrey-of-valisade lucius 
"It's nice to meet you, Brother
Geoffrey of Valisade! Welcome to our
little town." :town :robes :quine)

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
:west :grandfather)

(define-topic grandfather lucius 
  "Yes, the great Arturo Pentaquin the
Fourth! A decorated officer of the Wars
of the West. We should visit him in
Nothbehem; it's a short distance to the
north of here. He knows all about
Westerners." :west)

(define-topic west lucius 
  "I've never been out West myself, but
of course I've heard all the old
stories and read all the old books." :quine :robes :books)

(define-topic books lucius 
  "There are plenty of books, maps, and
scrolls at the Library where I work. 
You should visit the Monastery in town." :town)

(define-topic town lucius 
  "Nothbehem is my family's home, a
quiet town to the north of here.
There's also a monastery.")

;; (defmethod discuss :after ((self lucius) (topic (eql :letter)))
;;   (drop self (new 'scroll) 0 (field-value :height self)))
