(in-package :cypress)

;;; Arrows, the monk's main weapon

(defparameter *arrow-size* 25)
(defparameter *wooden-arrow-images* (image-set "wooden-arrow" 2))
(defparameter *silver-arrow-images* (image-set "silver-arrow" 2))
(defparameter *crystal-arrow-images* (image-set "crystal-arrow" 2))

(defthing (arrow sprite)
  :image-scale 40
  :clock 400
  :heading (/ pi 2)
  :images *wooden-arrow-images*
  :image (random-choose *wooden-arrow-images*))

(defmethod initialize ((self arrow) &key heading)
  (when heading
    (setf (field-value :heading self) heading)))

(defmethod run ((self arrow))
  (with-fields (clock image images) self
    (percent-of-time 13 (setf image (random-choose images)))
    (resize self *arrow-size* *arrow-size*)
    (decf clock)
    (if (minusp clock)
	(destroy self)
	(forward self 15))))

(defmethod collide ((self arrow) (thing thing))
  (when (solidp thing) 
    (destroy self)))

(defmethod destroy :before ((self arrow))
  (play-sample "knock.wav"))

(defthing (wooden-arrow arrow))

(defmethod collide ((self wooden-arrow) (enemy enemy))
  (modify-health enemy -5)
  (destroy self))

(defthing (silver-arrow arrow)
  :images *silver-arrow-images*
  :image (random-choose *silver-arrow-images*))

(defmethod collide ((self silver-arrow) (enemy enemy))
  (modify-health enemy -10)
  (destroy self))

(defthing (crystal-arrow arrow)
  :images *crystal-arrow-images*
  :image (random-choose *crystal-arrow-images*))

(defmethod collide ((self crystal-arrow) (enemy enemy))
  (modify-health enemy -15)
  (destroy self))

;;; A monk, either AI or human controlled

(defparameter *monk-cast*
  '(:scale 950
    :frames (("monk-cast-1.png" 3)
	     ("monk-cast-2.png" 4)
	     ("monk-cast-3.png" 4)
	     ("monk-cast-4.png" 4)
	     ("monk-cast-5.png" 6))))

(defparameter *monk-stand*
  '(:scale 900
    :frames (("monk-stand-1.png" 19)
	     ("monk-stand-2.png" 24)
	     ("monk-stand-3.png" 18)
	     ("monk-stand-4.png" 20))))

(defparameter *monk-stand-bow*
  '(:scale 1000
    :frames (("monk-stand-bow-1.png" 19)
	     ("monk-stand-bow-2.png" 24)
	     ("monk-stand-bow-3.png" 32))))

(defparameter *monk-stand-bow-ready*
  '(:scale 1000
    :frames (("monk-stand-bow-ready-1.png" 19)
	     ("monk-stand-bow-ready-2.png" 24)
	     ("monk-stand-bow-ready-3.png" 32))))

(defparameter *monk-stand-images*
  '("monk-stand-1.png" "monk-stand-2.png" "monk-stand-3.png" "monk-stand-4.png"))

(defparameter *monk-walk* 
  '(:repeat t
    :scale 900
    :frames (("monk-walk-1.png" 4)
	     ("monk-walk-3.png" 4)
	     ("monk-walk-2.png" 4)
	     ("monk-walk-4.png" 4))))

(defparameter *monk-walk-bow* 
  '(:repeat t
    :scale 600
    :frames (("monk-walk-bow-3.png" 4)
	     ("monk-walk-bow-1.png" 4)
	     ("monk-walk-bow-2.png" 4)
	     ("monk-walk-bow-4.png" 4))))

(defparameter *monk-walk-bow-ready* 
  '(:repeat t
    :scale 1000
    :frames (("monk-walk-bow-ready-1.png" 4)
	     ("monk-walk-bow-ready-2.png" 4)
	     ("monk-walk-bow-ready-3.png" 4)
	     ("monk-walk-bow-ready-4.png" 4))))

;;; Animations for monk 2

(defparameter *monk-2-walk* 
  '(:repeat t
    :scale 900
    :frames (("monk-2-walk-1.png" 4)
	     ("monk-2-walk-2.png" 4)
	     ("monk-2-walk-3.png" 4)
	     ("monk-2-walk-4.png" 4))))

(defparameter *monk-2-stand*
  '(:scale 900
    :frames (("monk-2-stand-1.png" 19)
	     ("monk-2-stand-2.png" 24))))

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
  (raising-bow :initform nil)
  (bow-ready :initform nil)
  (aim-heading :initform nil)
  (alive :initform t)
  (talking :initform nil)
  (walking :initform nil)
  (hearing-distance :initform 800)
  (tags :initform '(:monk :fixed :container))
  (direction :initform :up)
  (fire-direction :initform :up)
  ;; timers
  (retry-clock :initform (seconds->frames 5))
  (walk-clock :initform 0)
  (step-clock :initform 0)
  (fire-clock :initform 0))

(defmethod walk-to :before ((monk monk) x y)
  (bring-to-front monk))

(defmethod initialize :after ((monk monk) &key)
  (add-inventory-item monk (new 'jerky))
  (add-inventory-item monk (quantity-of 'wooden-arrow 60)))

(defmethod humanp ((self monk)) nil)

(defmethod equipped-item ((self monk))
  (or (field-value :equipped-item self)
      (find-arrow self)))

(defmethod equip ((self monk) (item thing))
  (setf (field-value :equipped-item self) item))

;;; Animating the monk as he walks

(defparameter *monk-reload-frames* 30)

(defmethod draw ((self monk))
  (with-local-fields 
    (draw-as-sprite self 
		    (or (current-animation-frame self) %image)
		    %heading)))

(defmethod cast-spell ((self monk))
  (begin-animation self *monk-cast*))

(defmethod begin-talking ((self monk) line)
  (setf (field-value :talking self) t))

(defmethod stop-talking ((self monk))
  (setf (field-value :ttalking self) nil))

;;; Footstep sounds

(defresource "left-foot.wav" :volume 20)
(defresource "right-foot.wav" :volume 20)

(defmethod footstep-sound ((self monk))
  (case (field-value :walk-clock self)
    ;; on first step
    (0 "left-foot.wav")
    ;; on 8th steps while looping 
    (1 "left-foot.wav")
;    (3 "right-foot.wav")
    (5 "left-foot.wav")
;    (7 "right-foot.wav")
    (9 "left-foot.wav")
;    (11 "right-foot.wav")
    (13 "left-foot.wav")))
;    (15 "right-foot.wav")))
    
(defparameter *footstep-sound-range* 300)

(defmethod make-footstep-sounds ((self monk))
  (let ((sound (footstep-sound self)))
    (when sound 
      (when (< (distance-to-cursor self) 400)
	(play-sound self sound)))))

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
  (percent-of-time 10
    (modify-health self -3)
    (play-sample (random-choose '("unh-1.wav" "unh-2.wav" "unh-3.wav")))))

(defmethod die ((self monk))
  (when (field-value :alive self)
    (when (humanp self) 
      (change-image self "skull.png")
      (setf (field-value :alive self) nil))))

;;; Control logic driven by the above (possibly overridden) methods.

(defparameter *monk-speed* (truncate (/ *unit* 1.3)))

(defmethod standing-animation ((self monk)) *monk-2-stand*)
(defmethod walking-animation ((self monk)) *monk-2-walk*)

(defmethod run ((self monk))
  (with-local-fields 
    (when %alive
      (update-animation self)
      (when (null %animation)
	(begin-animation self (standing-animation self)))
	;; find out what direction the AI or human wants to go
	(let ((heading (movement-heading self)))
	  (when (or (null heading)
		    (null %animation))
	    (begin-animation self (standing-animation self)))
	  (when heading 
	    (unless (eq %animation (walking-animation self))
	      (begin-animation self (walking-animation self)))
	    ;; move in the movement direction
	    (move self heading (/ *monk-speed* 2))
	    (setf %heading heading))))))

;;; Firing arrows

(defmethod aim ((self monk) heading)
  (setf (field-value :aim-heading self) heading))

(defmethod aim-heading ((self monk))
  (with-fields (heading aim-heading) self 
    (or aim-heading heading)))

(defmethod fire-location ((self monk))
  (multiple-value-bind (tx ty) 
      (step-toward-heading self (aim-heading self) (units 0.7))
    (values (- tx (* *arrow-size* 0.4))
	    (- ty (* *arrow-size* 0.4)))))

(defmethod fire ((monk monk) (arrow arrow))
  (multiple-value-bind (x y) 
      (fire-location monk)
    (drop-object (current-buffer) arrow x y)))

(defresource "bow.wav" :volume 20)

(defmethod use ((monk monk) (arrow arrow))
  (fire monk (new (class-name (class-of arrow))
		  :heading (aim-heading monk)))
  (play-sound monk "bow.wav")
  (modify-fatigue monk 1)
  (modify-quantity arrow -1))

(defmethod find-arrow ((monk monk))
  (or (find-inventory-item monk 'wooden-arrow)
      (find-inventory-item monk 'silver-arrow)
      (find-inventory-item monk 'crystal-arrow)))

(defmethod attack ((monk monk) (enemy enemy))
  (when (has-quantity monk 'arrow)
    (aim monk (heading-to-thing monk enemy))
    (use monk (or (equipped-item monk)
		  (find-arrow monk)))))

(defmethod can-accept ((self monk))
  (with-fields (inventory) self
    (not (fullp inventory))))

;;; As the monk Geoffrey, the player drives the action
  
(defthing (geoffrey monk) :description "Geoffrey")

(defmethod activate ((monk monk))
  (replace-gump monk (new 'browser :container monk)))

(defmethod standing-animation ((self geoffrey))
  *monk-stand*)

(defmethod walking-animation ((self geoffrey))
 *monk-walk*)

;; (defmethod standing-animation ((self geoffrey))
;;   (if %bow-ready 
;;       *monk-stand-bow-ready*
;;       *monk-stand-bow*))

;; (defmethod walking-animation ((self geoffrey))
;;   (if %bow-ready 
;;       *monk-walk-bow-ready*
;;       *monk-walk-bow*))

;;; Lucius 

(defthing (lucius monk) :clock 10 :description "Lucius")

(defmethod run ((self lucius))
  (call-next-method)
  (with-fields (clock) self
    (decf clock)
    (when (cursor)
      (cond  ((> (distance-to-cursor self) 150)
	      (unless (or (field-value :waypoints self) (plusp clock))
		(multiple-value-bind (x y) (at (cursor))
		  (walk-to self x y))))
	     ((> (distance-to-cursor self) 110)
	      (prog1 nil (stop-walking self) (setf clock 10)))))))

;;; Monk food

(defthing food)

(defmethod use ((monk monk) (food food))
  (consume monk food)
  (destroy food))

(defthing (white-bread food)
  :image "white-bread.png")

(defmethod consume ((monk monk) (bread white-bread))
  (modify-health monk +5)
  (modify-hunger monk -10))

(defthing (wheat-bread food)
  :image "wheat-bread.png")

(defmethod consume ((monk monk) (bread wheat-bread))
  (modify-health monk +10)
  (modify-hunger monk -15))

(defmethod consume :after ((monk geoffrey) (bread wheat-bread))
  (message "BURP!"))

(defthing (jerky food)
  :image "beef-jerky.png")

(defmethod consume ((monk monk) (jerky jerky))
  (modify-health monk +15)
  (modify-hunger monk -30))

(defparameter *elixir-images* (image-set "elixir" 2))

(defthing (elixir food)
  :image (random-choose *elixir-images*))

(defmethod consume ((monk monk) (elixir elixir))
  (modify-magic monk +40))

(defparameter *silver-elixir-images* (image-set "silver-elixir" 2))

(defthing (silver-elixir food)
  :image (random-choose *silver-elixir-images*))

(defmethod consume ((monk monk) (silver-elixir elixir))
  (modify-magic monk +100))



;; (defmethod activate ((self lucius))
;;   (discuss self :hello))

;; (define-topic hello lucius 
;;    "Good morning Geoffrey! A Raven just
;; delivered this letter for you."
;;    :letter :weather :name :job :bye)
	   
;; (define-topic name lucius 
;;   "I am your friend Lucius, of course.")

;; (define-topic job lucius 
;;   "You know perfectly well that I work
;; at the Nothbess Library. My duties
;; include dusting and organizing books.
;; And what else have you forgotten today?
;; Something must be wrong with you.")

;; (define-topic weather lucius 
;; "It's nice out today, but I feel as if
;; it's been a bit colder than usual."
;;   :colder :letter :name :job :bye)

;; (define-topic bye lucius () nil)

;; (define-topic colder lucius 
;; "Yes. The leaves seem to be turning early.")

;; (define-topic letter lucius 
;;   (drop self (new 'scroll) 0 (field-value :height self))
;;   (make-talk-gump self "I wonder what it says? It comes
;; straight from Dr. Quine at the
;; monastery. Here you go. I'm so curious
;; to know what it says. Open it, open it!" :bye))

    ;;   (when (not %raising-bow))
    ;;   (setf %raising-bow t))
    ;; (when (not (pressing-fire-p self))
    ;;   (setf %raising-bow nil))
    ;; (when (and %raising-bow (zerop %fire-clock))
    ;;   (setf %bow-ready t)
    ;;   (setf %raising-bow nil))
