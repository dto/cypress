(in-package :cypress)

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
  (hit-points :initform *maximum-points*)
  (magic-points :initform *maximum-points*)
  (hunger-points :initform 0)
  (fatigue-points :initform 0)
  (cold-points :initform 0)
  (inventory :initform (list (quantity-of 'arrow 10)))
  (sprite-height :initform (units 5))
  (sprite-width :initform (units 5))
  (image :initform (random-choose *monk-stand-images*))
  (raising-bow :initform nil)
  (bow-ready :initform nil)
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
  (fire-clock :initform 0)
  ;;
  (path :initform nil)
  (waypoints :initform nil)
  (goal-x :initform nil)
  (goal-y :initform nil))

(defmethod humanp ((self monk)) nil)

;;; Animating the monk as he walks

(defparameter *monk-step-frames* 3)

(defparameter *monk-reload-frames* 30)

(defparameter *walk-interval* 16)

(defmethod raise-bow ((monk monk))
  (when (has-quantity monk 'arrow)
    (setf (field-value :raising-bow monk) t)))

(defmethod update-walk ((self monk))
  (with-fields (walk-clock step-clock) self
    ;; only when moving
    (when (plusp step-clock)
      (when (plusp walk-clock)
	(decf walk-clock))
      (when (zerop walk-clock)
	(setf walk-clock *walk-interval*)))))

(defmethod draw ((self monk))
  (with-local-fields 
    (draw-as-sprite self 
		    (or (current-animation-frame self) %image)
		    %heading)))

(defmethod cast-spell ((self monk))
  (begin-animation self *monk-cast*))

(defmethod fire-location ((self monk))
  (with-fields (direction) self
    (multiple-value-bind (cx cy) (center-point self)
      (multiple-value-bind (tx ty) 
	  (step-in-direction cx cy direction (units 0.7))
	(values (- tx (* *arrow-size* 0.4))
		(- ty (* *arrow-size* 0.4)))))))
	  
(defmethod fire ((self monk) direction)
  (with-fields (fire-clock) self
    (when (zerop fire-clock)
      (setf fire-clock *monk-reload-frames*)
      (multiple-value-bind (x y) (fire-location self)
	(when (has-quantity self 'arrow)
	  (consume-quantity self 'arrow)
	  (let ((arrow (new 'arrow (direction-heading direction))))
	    (drop-object (current-buffer) arrow x y)))))))

(defmethod begin-talking ((self monk) line)
  (setf (field-value :talking self) t))

(defmethod stop-talking ((self monk))
  (setf (field-value :ttalking self) nil))

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

;; (defresource "fire.wav" :volume 23)
;; (defresource "serve.wav" :volume 23)

(defparameter *fire-sound* "fire.wav")

;;; Default AI methods. 

(defmethod movement-direction ((self monk)) nil)

(defvar *joystick-enabled* t)

(defmethod stick-heading ((self monk))
  (when (humanp self)
    (or (when (left-analog-stick-pressed-p)
	  (left-analog-stick-heading))
	(when (right-analog-stick-pressed-p)
	  (right-analog-stick-heading)))))

(defmethod collide ((self monk) thing)
  (when (and (solidp thing) 
	     (null (field-value :waypoints self)))
    (restore-location self)
    (stop-walking self))
  (when (enemyp thing)
    (percent-of-time 20 (play-sample (random-choose '("unh-1.wav" "unh-2.wav" "unh-3.wav"))))))

(defmethod die ((self monk))
  (when (field-value :alive self)
    (when (humanp self) 
      (change-image self "skull.png")
      (setf (field-value :alive self) nil))))

(defmethod damage ((self monk) points)
  (die self))

(defmethod pressing-fire-p ((self monk)) 
  nil)

(defmethod pressing-fire ((self monk))
  nil)

;;; Control logic driven by the above (possibly overridden) methods.

(defparameter *monk-speed* (truncate (/ *unit* 1.3)))

(defmethod standing-animation ((self monk)) *monk-2-stand*)
(defmethod walking-animation ((self monk)) *monk-2-walk*)

(defmethod next-waypoint ((self monk))
  (with-local-fields 
    (destructuring-bind (wx wy) (pop %waypoints)
      (setf %goal-x wx %goal-y wy))))

(defmethod movement-heading ((self monk))
  (with-fields (x y goal-x goal-y waypoints) self
      (if (and goal-x goal-y)
	  (if (< 4 (distance x y goal-x goal-y))
	      ;; keep walking 
	      (find-heading x y goal-x goal-y)
	      (if waypoints 
		  (progn (next-waypoint self)
			 (find-heading x y goal-x goal-y))
		  (setf goal-x nil goal-y nil)))
	  (when waypoints
	    (next-waypoint self)
	    (find-heading x y goal-x goal-y)))))

(defmethod walk-to ((self monk) x1 y1)
  (with-fields (x y waypoints path) self
    (when (null path) 
      (setf path (create-path self :buffer (current-buffer))))
    (setf waypoints (rest (rest (find-path-waypoints path x y x1 y1))))
    (when (null waypoints) (stop-walking self))))
  
(defmethod stop-walking ((self monk))
  (with-fields (waypoints goal-x goal-y) self
    (setf waypoints nil)
    (setf goal-x nil goal-y nil)))

(defmethod run ((self monk))
  (with-local-fields 
    (when (and (pressing-fire-p self) 
	       (not %raising-bow))
      (setf %raising-bow t))
    (when (not (pressing-fire-p self))
      (setf %raising-bow nil))
    (when (and %raising-bow (zerop %fire-clock))
      (setf %bow-ready t)
      (setf %raising-bow nil))
    (when %alive
      (update-animation self)
      (when (null %animation)
	(begin-animation self (standing-animation self)))
      (with-fields (step-clock fire-clock) self
	(when (plusp step-clock)
	  (decf step-clock))
	;; find out what direction the AI or human wants to go
	(let ((heading (movement-heading self))
	      (fire-button (pressing-fire-p self)))
	  (when (or (null heading)
		    (null %animation))
	    (begin-animation self (standing-animation self)))
	  (when heading 
	    (unless (eq %animation (walking-animation self))
	      (begin-animation self (walking-animation self)))
	    ;; move in the movement direction
	    (move self heading (/ *monk-speed* 2))
	    (setf %heading heading))
	  (if heading
	      ;; controller is pushing in a direction
	      (when (zerop step-clock)
		;; don't animate on every frame
		(setf step-clock *monk-step-frames*))
	      ;; possibly make footstep sounds
	      ;;	      (make-footstep-sounds self))
	      ;; not pushing. allow movement immediately
	      (setf step-clock 0 %walk-clock 0))
	  ;; update walk counters
	  (update-walk self)
	  ;; delay between fires
	  (when (plusp fire-clock)
	    (decf fire-clock))
	  ;; ready to fire?
	  (when (and (zerop fire-clock) %bow-ready)
	    ;; did you let go? 
	    (when (not fire-button)
	      ;; yes, do it
	      (setf %bow-ready nil)
	      (setf %raising-bow nil)
	      (fire self %fire-direction fire-button))))))))
  
;; As Geoffrey, the player drives the logic with the arrows/numpad
;; and the shift key

(defun holding-fire-button ()
  (or (holding-shift)
      (some #'joystick-button-pressed-p
	    '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20))))

(defun holding-down-arrow ()
  (or (joystick-button-pressed-p :down)
      (keyboard-down-p :kp2)
      (keyboard-down-p :down)))

(defun holding-up-arrow ()
  (or (joystick-button-pressed-p :up)
      (keyboard-down-p :kp8)
      (keyboard-down-p :up)))

(defun holding-left-arrow ()
  (or (joystick-button-pressed-p :left)
      (keyboard-down-p :kp4)
      (keyboard-down-p :left)))

(defun holding-right-arrow ()
  (or (joystick-button-pressed-p :right)
      (keyboard-down-p :kp6)
      (keyboard-down-p :right)))

(defthing (geoffrey monk))

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

(defmethod pressing-fire-p ((self geoffrey)) nil)

(defmethod humanp ((self geoffrey)) t)

(defmethod movement-direction ((self geoffrey))
  (or 
   (cond 
     ((and (holding-down-arrow) (holding-right-arrow)) :downright)
     ((and (holding-down-arrow) (holding-left-arrow)) :downleft)
     ((and (holding-up-arrow) (holding-right-arrow)) :upright)
     ((and (holding-up-arrow) (holding-left-arrow)) :upleft)
     ((holding-down-arrow) :down)
     ((holding-up-arrow) :up)
     ((holding-left-arrow) :left)
     ((holding-right-arrow) :right))
   (let ((heading (stick-heading self)))
     (when 
  	 (and *joystick-enabled*
  	      (or (left-analog-stick-pressed-p)
  		  (right-analog-stick-pressed-p)))
       (or (heading-direction heading) :left)))))
  
;;; Lucius 

(defthing (lucius monk) :clock 10)

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
  (modify-hit-points monk +5)
  (modify-hunger-points monk -10))

(defthing (wheat-bread food)
  :image "wheat-bread.png")

(defmethod consume ((monk monk) (bread wheat-bread))
  (modify-hit-points monk +10)
  (modify-hunger-points monk -15))

(defmethod consume :after ((monk geoffrey) (bread wheat-bread))
  (message "BURP!"))

(defthing (jerky food)
  :image "beef-jerky.png")

(defmethod consume ((monk monk) (jerky jerky))
  (modify-hit-points monk +15)
  (modify-hunger-points monk -30))




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
