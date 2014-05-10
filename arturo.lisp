(in-package :cypress)

;;; Arturo Pentaquin

(defthing (arturo monk) 
  :met-player nil
  :next-target nil
  :description "Arturo")

(defparameter *arturo-walk* 
  '(:repeat t
    :scale 980
    :frames (("arturo-walk-1.png" 4)
	     ("arturo-walk-2.png" 4)
	     ("arturo-walk-3.png" 4)
	     ("arturo-walk-4.png" 4))))

(defmethod walking-animation ((self arturo))
  *arturo-walk*)

(defparameter *arturo-stand*
  '(:scale 980
    :frames (("arturo-stand-1.png" 19)
	     ("arturo-stand-2.png" 16)
	     ("arturo-stand-3.png" 24))))

(defmethod standing-animation ((self arturo))
  *arturo-stand*)

(defmethod choose-target ((self arturo))
  (setf (field-value :next-target self)
	(let ((targets (find-instances (current-scene) 'silverwood)))
	  (when targets (random-choose targets)))))

(defmethod return-home ((self arturo))
  (multiple-value-bind (x y) (below (find-pentaquin-house))
    (walk-to self x y)))

(defmethod run ((self arturo))
  (with-fields (next-target met-player gump waypoints) self
    (call-next-method)
    (choose-target self)
    (let ((distance (distance-to-cursor self)))
      (cond 
	;; ((< distance 110)
	;;  (setf waypoints nil))
	;; ((< distance 220)
	;;  (walk-to-thing self (geoffrey)))
	((and next-target (null waypoints))
	     (percent-of-time 4 (walk-to-thing self next-target)))
	((and (null next-target)
	      (null waypoints))
	 (percent-of-time 4
			  (walk-to-thing self (find-pentaquin-house))))))))

(defmethod activate ((self arturo))
  (with-fields (met-player) self
    (if (not met-player)
	(progn (setf met-player t)
	       (discuss self :hello))
	(discuss self :chat))))
	
(define-topic hello arturo 
"Well, you do look rather unusual!
Those are most extraordinary robes. Oh,
but I'm being so rude! Greetings and
salutations, traveler. I am Arturo
Pentaquin, retired Lieutenant Commander
of the Paladins. And I can see you've
already met my grandson! So, Lucius, why
don't you introduce me to your friend?"
:traveler)

(define-topic traveler arturo 
"Oh Lord, this is absolutely incredible.
I never thought I would actually see a
Time Traveler. They say you can live a
thousand years without one passing
through your life."
:time-travel)

(define-topic time-travel arturo 
"It is recorded in the Histories that
certain people who vanished in one time
could reappear many years later, without
the traveler experiencing the time in
between. The magicks behind such events
are unknown to me, but I can see from
the design of your robes that you have
come to us from a distant year." 
:quine :garden :town)

(define-topic quine arturo 
"A second traveler, in one Age? I can
hardly believe it. But, the name doesn't
sound familiar at all. Perhaps he
stepped into a different year? I wish I
could help more."
:garden :town)

(define-topic garden arturo
"I used one of the ancient walls to
string up my silverwood garden. 
Silverwood arrows are the best way to
take down a wolf. Even Dire Wolves can be
felled by a single arrow, if your aim is
true." :wolves :town)

(define-topic wolves arturo
"Wolves can kill with ease, but are an
important source of food when traveling
in the wilderness. Sadly, the influx of
Dire Wolves threatens the food supply,
because the cursed flesh of the Dire
Wolf cannot be eaten." :town :flesh)

(define-topic flesh arturo 
"One can cure the meat of wolves and
other un-cursed animals by means of a
Curing spell. I'll teach it to you
before you leave town!" :town)
 
(define-topic town arturo
"Nothbehem is one of the few remaining
towns in the Vale---everything in the
North fell to ruins when the weather
grew cold. That was a hundred years
ago---and in fact, your robes are not
decked out with fur. I reckon you've
been gone for more than a century."
:cold)

(define-topic cold arturo
"You'll have to wear something warmer if
you want to keep looking for your
Dr. Quine, for it is rumored that the
sealed mountainside caves with the oddly
carven stones and the green copper
plating, are related somehow to the
appearance of Travelers. You'll have to
travel through snowy regions to reach
most of these caves."
:caves :clothing)

(define-topic clothing arturo
"I can give you something from my house
to wear, before you leave town." :caves)

(define-topic caves arturo
"The cave entrances are decorated with
stone carvings of a rounded, yet
irregular motif involving semi-circles.
Beyond these markings lay massive copper
doors, and strange copper plates whose
workings have never been deciphered.
What sleeps beyond the sealed doors,
none have ever found."
:plates)

(define-topic plates arturo
"Some caves have two plates, and some
have three or more. I believe that these
can be unlocked by a set of matching
copper keys. My brother Alonso found one
key on an archaeological dig, and I
found another while on an expedition to
the Northeast; but Alonso would not
share his key with me, for he greedily
desired whatever treasure may lay behind
the doors, all to himself. To this day,
we each hold one key, and are bitterly
estranged. If you want to find
Dr. Quine, you'd better find my
brother's key first. I'll give you the
other key when you return."
:alonso :expedition)

(define-topic expedition arturo
"We went in search of a woman of
Nothbehem, named Amalia, who had run
away from town. But we never found her.
By chance we found an ancient crypt with
a copper key buried inside." :alonso :amalia)

(define-topic amalia arturo
"I'll give you the written expedition
report before you leave for your
journey. I'm sure it will contain much
valuable help." :alonso)

(define-topic alonso arturo
"My brother became a hermit as he
aged. Last I heard, he built a cabin in
the Wilmont Woods, to the northwest.
Perhaps if you can find him, you'll find
the key." :woods)

(define-topic woods arturo
"Come to my house before you leave town,
and I'll give you enough supplies to get
started on your journey." :bye)




	  
			    
	
    
