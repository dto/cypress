(in-package :cypress)

;;; Arrows, the monk's main weapon

(defparameter *arrow-size* 25)
(defparameter *wooden-arrow-images* (image-set "wooden-arrow" 2))
(defparameter *silver-arrow-images* (image-set "silver-arrow" 2))
(defparameter *crystal-arrow-images* (image-set "crystal-arrow" 2))

(defthing (arrow sprite)
  :image-scale 700
  :height *arrow-size*
  :width *arrow-size*
  :sprite-height *arrow-size*
  :sprite-width *arrow-size*
  :clock 400
  :heading (/ pi 2)
  :images *wooden-arrow-images*
  :image (random-choose *wooden-arrow-images*))

(defmethod drop-object :after ((buffer buffer) (arrow arrow) &optional x y z )
  (layout arrow))

(defmethod initialize ((self arrow) &key heading rating)
  (when heading
    (setf (field-value :heading self) heading))
  (when rating
    (setf (field-value :rating self) rating)))

(defmethod initialize :after ((self arrow) &key heading)
  (resize self *arrow-size* *arrow-size*))

(defmethod run ((self arrow))
  (with-fields (clock image images) self
    (percent-of-time 13 (setf image (random-choose images)))
    (decf clock)
    (if (minusp clock)
	(destroy self)
	(forward self 18))))

(defmethod collide ((self arrow) (thing thing))
  (when (solidp thing) 
    (destroy self)))

(defmethod destroy :before ((self arrow))
  (play-sample "knock.wav"))

(defthing (wooden-arrow arrow))

(defmethod collide ((self wooden-arrow) (enemy enemy))
  (damage enemy (random-choose '(-3 -5 -7)))
  (destroy self))

(defthing (silver-arrow arrow)
  :images *silver-arrow-images*
  :image (random-choose *silver-arrow-images*))

(defmethod collide ((self silver-arrow) (enemy enemy))
  (damage enemy (random-choose '(-10 -12 -15)))
  (destroy self))

(defmethod collide ((arrow silver-arrow) (wolf wolf))
  (damage wolf (random-choose '(-20 -30)))
  (destroy arrow))

(defthing (crystal-arrow arrow)
  :images *crystal-arrow-images*
  :image (random-choose *crystal-arrow-images*))

(defmethod collide ((self crystal-arrow) (enemy enemy))
  (damage enemy (random-choose '(-30 -40)))
  (destroy self))

;;; A monk, either AI or human controlled

(defparameter *monk-cast*
  '(:scale 1400
    :frames (("monk-cast-1.png" 3)
	     ("monk-cast-2.png" 4)
	     ("monk-cast-3.png" 4)
	     ("monk-cast-4.png" 4)
	     ("monk-cast-5.png" 6))))

(defparameter *monk-stand*
  '(:scale 800
    :frames (("monk-stand-1.png" 19)
	     ("monk-stand-2.png" 24)
	     ("monk-stand-3.png" 18)
	     ("monk-stand-4.png" 20))))

(defparameter *monk-stand-bow*
  '(:scale 1500
    :frames (("monk-stand-bow-3.png" 19)
	     ("monk-stand-bow-2.png" 24)
	     ("monk-stand-bow-3.png" 32))))

(defparameter *monk-stand-bow-ready*
  '(:scale 1600
    :frames (("monk-stand-bow-ready-1.png" 19)
	     ("monk-stand-bow-ready-2.png" 24)
	     ("monk-stand-bow-ready-3.png" 32))))

(defparameter *monk-stand-images*
  '("monk-stand-1.png" "monk-stand-2.png" "monk-stand-3.png" "monk-stand-4.png"))

(defparameter *monk-walk* 
  '(:repeat t
    :scale 820
    :frames (("monk-walk-2.png" 4)
	     ("monk-walk-4.png" 4)
	     ("monk-walk-1.png" 4)
	     ("monk-walk-3.png" 1))))

(defparameter *monk-walk-bow* 
  '(:repeat t
    :scale 800
    :frames (("monk-walk-bow-3.png" 4)
	     ("monk-walk-bow-1.png" 4)
	     ("monk-walk-bow-2.png" 4)
	     ("monk-walk-bow-4.png" 4))))

(defparameter *monk-walk-bow-ready* 
  '(:repeat t
    :scale 750
    :frames (("monk-walk-bow-ready-1.png" 4)
	     ("monk-walk-bow-ready-2.png" 4)
	     ("monk-walk-bow-ready-3.png" 4)
	     ("monk-walk-bow-ready-4.png" 4))))

(defparameter *maximum-points* 100)

(defsprite monk
  (equipped-item :initform nil)
  (health :initform *maximum-points*)
  (magic :initform *maximum-points*)
  (hunger :initform 0)
  (fatigue :initform 0)
  (cold :initform 0)
  (inventory :initform nil)
  (sprite-height :initform (units 5))
  (sprite-width :initform (units 5))
  (image :initform (random-choose *monk-stand-images*))
  ;; weapon
  (load-time :initform (seconds->frames 0.85))
  (load-clock :initform 0)
  (reload-time :initform (seconds->frames 0.4))
  (reload-clock :initform 0)
  (aiming-bow :initform nil)
  (bow-ready :initform nil)
  (aim-heading :initform nil)
  (fire-direction :initform :up)
  (last-fire-time :initform 0)
  ;; human status
  (alive :initform t)
  (talking :initform nil)
  (walking :initform nil)
  (hearing-distance :initform 800)
  (tags :initform '(:monk :fixed :container))
  (direction :initform :up)
  ;; timers
  (walk-clock :initform 0))

(defmethod use ((monk monk) (arrow arrow))
  (let ((arrow (class-name (class-of arrow))))
    (equip monk (find-inventory-item monk arrow))
    (narrate "Equipped ~A." (fancy-description arrow))))

(defmethod walk-to :before ((monk monk) x y)
  (bring-to-front monk)
  (resume))

(defmethod bark-hunger ((monk monk))
  (with-fields (hunger) monk
    (cond ((> hunger 85)
	   (bark monk "I am starving to death!"))
	  ((> hunger 70)
	   (bark monk "I feel weak from hunger!"))
	  ((> hunger 45)
	   (bark monk "I am very hungry.")))))

(defmethod bark-cold ((monk monk))
  (with-fields (cold) monk
    (cond ((> cold 80)
	   (bark monk "I am freezing to death!"))
	  ((> cold 50)
	   (bark monk "I am beginning to freeze!"))
	  ((> cold 45)
	   (bark monk "I feel very cold.")))))

(defmethod drop-object :after ((buffer buffer) (monk monk) &optional x y z)
  (begin-animation monk (standing-animation monk))
  (setf (field-value :path monk) nil)
  (bark-hunger monk)
  (bark-cold monk))

(defmethod humanp ((self monk)) nil)

(defmethod equipped-item ((self monk))
  (field-value :equipped-item self))

(defmethod equip ((self monk) (item arrow))
  ;; equipping arrows works a little differently
  (setf (field-value :equipped-item self) item))

;;; Animating the monk as he walks

(defmethod draw ((self monk))
  (with-local-fields 
    (when %alive
      (draw-as-sprite self 
		      (or (current-animation-frame self) %image)
		      (if %aiming-bow %aim-heading %heading)))))

(defmethod begin-talking ((self monk) line)
  (setf (field-value :talking self) t))

(defmethod stop-talking ((self monk))
  (setf (field-value :talking self) nil))

;;; Footstep sounds

(defresource "left-foot.wav" :volume 15)
(defresource "right-foot.wav" :volume 15)

(defmethod footstep-sound ((self monk))
  (case (field-value :walk-clock self)
    ;; on first step
    (0 "left-foot.wav")
    ;; on 8th steps while looping 
    (7 "right-foot.wav")
;    (3 "right-foot.wav")
    (15 "left-foot.wav")
;    (7 "right-foot.wav")
    (23 "right-foot.wav")))
;    (11 "right-foot.wav")

;    (15 "right-foot.wav")))
    
(defparameter *footstep-sound-range* 300)

(defmethod make-footstep-sounds ((self monk))
  (let ((sound (footstep-sound self)))
    (when sound 
      (when (< (distance-to-cursor self) 400)
	(play-sound self sound)))))

(defmethod update-footsteps ((monk monk))
  (with-fields (walk-clock) monk
    (if (movement-heading monk)
	(progn 
	  (incf walk-clock)
	  (when (> walk-clock 32)
	    (setf walk-clock 0)))
	(setf walk-clock 0))))

;;; Default collision methods

(defmethod collide ((self monk) thing)
  (when (and (solidp thing) 
	     (null (field-value :waypoints self)))
    (restore-location self)
    (stop-walking self)))

(defresource "unh-1.wav" :volume 20)
(defresource "unh-2.wav" :volume 20)
(defresource "unh-3.wav" :volume 20)

(defmethod collide ((self monk) (enemy enemy))
  (when (field-value :alive self)
    (percent-of-time 10
      (damage self (- (random-choose '(2 3 3 5 7))))
      (play-sample (random-choose '("unh-1.wav" "unh-2.wav" "unh-3.wav"))))))

(defmethod collide ((self monk) (crack large-crack))
  (when (field-value :alive self)
    (percent-of-time 8
      (narrate "The ice cracks beneath your feet. You are splashed with frigid water.")
      (damage self (- (random-choose '(1 2))))
      (chill self +10)
      (play-sample (random-choose '("unh-1.wav" "unh-2.wav" "unh-3.wav"))))))

(defmethod collide ((self monk) (puddle puddle))
  (when (field-value :alive self)
    (percent-of-time 6
      (narrate "You step into the water. You are wet!")
      (damage self (- (random-choose '(5 7))))
      (chill self +20)
      (play-sample (random-choose '("unh-1.wav" "unh-2.wav" "unh-3.wav"))))))

(defmethod die ((self monk))
  (when (field-value :alive self)
    (when (humanp self) 
      (narrate "You died. Press Control-R to restart the game.")
      (change-image self (random-choose *remains-images*))
      (drop self (new 'remains))
      (drop self (new 'skull))
      (play-sample "death.wav")
      (send-to-back self)
      (setf (field-value :alive self) nil))))

;;; Control logic driven by the above (possibly overridden) methods.

(defparameter *monk-speed* 13)

(defmethod standing-animation ((self monk)) *monk-stand*)
(defmethod walking-animation ((self monk)) *monk-walk*)

(defmethod casting-animation ((self monk)) *monk-stand*)

(defmethod update-bow ((monk monk))
  (with-fields (aiming-bow load-clock load-time bow-ready reload-time reload-clock) monk
    (if (plusp reload-clock)
	;; we're reloading. 
	(when (plusp reload-clock)
	  (decf reload-clock))
	;; not reloading; ready to load
	(if (aiming-bow monk)
	    ;; ready to fire? 
	    (if (plusp load-clock)
		;; no, still reloading
		(progn 
		  (decf load-clock)
		  (setf bow-ready nil))
		;; yes
		(setf bow-ready t
		      aiming-bow nil))))))

(defparameter *camp-hint* 
"Place your magic tent on the ground and
cast the Ignite spell to start a fire
now, or you will freeze to death!")

(defmethod run ((self monk))
  (with-local-fields 
    (when %alive
      (update-footsteps self)
      (when (movement-heading self)
	(make-footstep-sounds self))
      (update-animation self)
      (update-bow self)
      (when (> %cold 80)
	(percent-of-time 1
	  (damage self (- (random-choose '(2 3 3 5 7))))
	  (play-sample (random-choose '("unh-1.wav" "unh-2.wav" "unh-3.wav")))
	  (narrate "You are freezing to death!")
	  (show-hint *camp-hint*)))
      (when (> %hunger 80)
	(percent-of-time 1
	  (damage self (- (random-choose '(2 3 3 5 7))))
	  (play-sample (random-choose '("unh-1.wav" "unh-2.wav" "unh-3.wav")))
	  (narrate "You are starving to death! You must eat something.")))
      (when (field-value :bow-ready self)
	(fire self (find-arrow self)))
      ;; find out what direction the AI or human wants to go
      (let ((heading 
	      ;; don't allow movement when aiming
	      (when (not (aiming-bow self))
		(movement-heading self))))
	(when (null heading)
	  (when (or (null %animation)
		    (not (eq %animation (casting-animation self))))
	    (begin-animation self (standing-animation self))))
	(when heading 
	  (unless (eq %animation (walking-animation self))
	    (begin-animation self (walking-animation self)))
	  ;; move in the movement direction
	  (move self heading (/ *monk-speed* 2))
	  (setf %heading heading))))))

;;; Firing arrows

(defmethod aim ((self monk) heading)
  (with-fields (aim-heading) self
    (setf aim-heading heading)))

(defmethod aim-heading ((self monk))
  (with-fields (heading aim-heading) self 
    (or aim-heading heading)))

(defmethod fire-location ((self monk))
  (multiple-value-bind (tx ty) 
      (step-toward-heading self (aim-heading self) (units 0.9))
    (values (- tx (* *arrow-size* 0.4))
	    (- ty (* *arrow-size* 0.4)))))

(defmethod aiming-bow ((self monk))
  (field-value :aiming-bow self))

(defmethod reloading-bow ((self monk))
  (plusp (field-value :reload-clock self)))

(defresource "bow.wav" :volume 20)

(defmethod fire ((monk monk) (arrow arrow))
  (with-fields (reload-time reload-clock bow-ready aiming-bow last-fire-time) monk
    (setf last-fire-time *updates*)
    (setf reload-clock reload-time)
    (setf aiming-bow nil bow-ready nil)
    (play-sound monk "bow.wav")
    (narrate "Fire!")
    (multiple-value-bind (x y) 
	(fire-location monk)
      (drop-object (current-buffer) 
		   (new (class-name (class-of arrow)) 
			:heading (aim-heading monk))
		   x y)
      (consume-single monk (class-name (class-of arrow))))))

(defmethod begin-firing ((monk monk))
  (stop-walking monk)
  (narrate "You take aim... Ready.... Set...")
  (with-fields (load-time aiming-bow load-clock bow-ready) monk
    (setf bow-ready nil)
    (setf aiming-bow t)
    (setf load-clock load-time)))

(defmethod equipped-arrow ((monk monk))
  (when (and (typep (equipped-item monk) 'arrow)
	     (and (find-inventory-item monk (class-name (class-of (equipped-item monk))))))
    (equipped-item monk)))

(defmethod find-arrow ((monk monk))
  (or 
   (equipped-arrow monk)
   (find-inventory-item monk 'wooden-arrow)
   (find-inventory-item monk 'silver-arrow)
   (find-inventory-item monk 'crystal-arrow)))

(defmethod attack ((monk monk) (enemy enemy))
  (if (not (find-arrow monk))
      (progn (show-error enemy)
	     (narrate "You don't have any arrows!"))
      (with-fields (bow-ready) monk
	(if (reloading-bow monk) 
	    (progn
	      (show-error enemy)
	      (narrate "Cannot fire while reloading."))
	    (progn 
	      (aim monk (heading-between monk enemy))
	      (begin-firing monk)
	      (modify-hunger monk 1))))))

(defmethod will-accept ((self monk) (item thing))
  (with-fields (inventory) self
    (if (field-value :stacking item)
	(or (search-inventory self (class-name (class-of item)))
	    (not (fullp inventory)))
	(not (fullp inventory)))))

;;; Monk food and potions

(defthing food)

(defmethod use ((monk monk) (food food))
  (eat monk food)
  (let ((container (find-container food)))
    (if container
	;; we're in a container. update the container's quantity
	(consume-single container (class-name (class-of food)))
	;; not in container.
	(destroy food))))

(defthing (white-bread food)
  :image "white-bread.png")

(defmethod eat ((monk monk) (bread white-bread))
  (modify-health monk +5)
  (modify-hunger monk -25))

(defthing (wheat-bread food)
  :image "wheat-bread.png")

(defmethod eat ((monk monk) (bread wheat-bread))
  (modify-health monk +10)
  (modify-hunger monk -35))

(defthing (jerky food)
  :image "beef-jerky.png")

(defmethod eat ((monk monk) (jerky jerky))
  (modify-health monk +15)
  (modify-hunger monk -50))

(defparameter *elixir-images* (image-set "red-potion" 2))

(defthing (elixir food)
  :scale 0.8
  :description "elixir (+50 hp)"
  :image (random-choose *elixir-images*))

(defmethod eat ((monk monk) (elixir elixir))
  (modify-health monk +50))

(defparameter *silver-elixir-images* (image-set "blue-potion" 2))

(defthing (silver-elixir food)
  :scale 0.8
  :description "elixir (+100 mp)"
  :image (random-choose *silver-elixir-images*))

(defmethod eat ((monk monk) (elixir silver-elixir))
  (narrate "You recover your magic fully.")
  (modify-magic monk +100))

(defparameter *green-elixir-images* (image-set "green-potion" 2))

(defthing (green-elixir food)
  :scale 0.8
  :description "elixir (-100 hunger)"
  :image (random-choose *green-elixir-images*))

(defmethod eat ((monk monk) (elixir green-elixir))
  (narrate "You feel full.")
  (modify-hunger monk -100))

(defmethod eat ((monk monk) (snowdrop snowdrop))
  (modify-magic monk +1))

(defmethod use ((monk monk) (snowdrop snowdrop))
  (eat monk snowdrop)
  (let ((container (find-container snowdrop)))
    (if container
	;; we're in a container. update the container's quantity
	(consume-single container (class-name (class-of snowdrop)))
	;; not in container.
	(destroy snowdrop))))

