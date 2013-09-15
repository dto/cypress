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

(defsprite monk
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
  (goal-x :initform nil)
  (goal-y :initform nil))

(defmacro defmonk (name &rest body)
  `(define-block (,name :super monk) ,@body))

(define-method walk-to monk (x y)
  (setf %goal-x x %goal-y y))

(define-method stop-walking monk ()
  (setf %goal-x nil %goal-y nil))
    
(define-method humanp monk () nil)

;;; Animating the monk as he walks

(defparameter *monk-step-frames* 3)

(defparameter *monk-reload-frames* 30)

(defparameter *walk-interval* 16)

(define-method raise-bow monk ()
  (setf %raising-bow t))

(define-method update-walk monk ()
  (with-fields (walk-clock step-clock) self
    ;; only when moving
    (when (plusp step-clock)
      (when (plusp walk-clock)
	(decf walk-clock))
      (when (zerop walk-clock)
	(setf walk-clock *walk-interval*)))))

(define-method draw monk ()
  (draw-as-sprite self 
		  (or (animation-frame self) %image)
		  %heading))

(define-method cast-spell monk ()
  (animate self *monk-cast*))

(define-method fire-location monk ()
  (with-fields (direction) self
    (multiple-value-bind (cx cy) (center-point self)
      (multiple-value-bind (tx ty) 
	  (step-in-direction cx cy direction (units 0.7))
	(values (- tx (* *arrow-size* 0.4))
		(- ty (* *arrow-size* 0.4)))))))
	  
(define-method fire monk (direction ignore)
  (when (zerop %fire-clock)
    (setf %fire-clock *monk-reload-frames*)
    (multiple-value-bind (x y) (fire-location self)
      (play-sample "bow.wav")
      (let ((arrow (new 'arrow (direction-heading direction))))
	(drop-object (current-buffer) arrow x y)))))

(define-method begin-talking monk (line)
  (setf %talking t))

(define-method stop-talking monk ()
  (setf %talking nil))

(defresource "left-foot.wav" :volume 20)
(defresource "right-foot.wav" :volume 20)

(define-method footstep-sound monk ()
  (case %walk-clock
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

(define-method make-footstep-sounds monk ()
  (let ((sound (footstep-sound self)))
    (when sound 
      (when (< (distance-to-cursor self) 400)
	(play-sound self sound)))))

;; (defresource "fire.wav" :volume 23)
;; (defresource "serve.wav" :volume 23)

(defparameter *fire-sound* "fire.wav")

;;; Default AI methods. 

(define-method movement-direction monk () nil)

(define-method movement-heading monk () nil)

(defvar *joystick-enabled* t)

(define-method stick-heading monk () 
  (when (humanp self)
    (or (when (left-analog-stick-pressed-p)
	  (left-analog-stick-heading))
	(when (right-analog-stick-pressed-p)
	  (right-analog-stick-heading)))))

;; (define-method bounding-box monk ()
;;   ;; shrink bounding box by a few px to make game more forgiving
;;   (with-field-values (x y height width) self
;;     (let ((margin 2))
;;       (values (+ margin y) (+ margin x)
;; 	      (+ (- margin) x width)
;; 	      (+ (- margin) y height)))))

(define-method collide monk (thing)
  (when (solidp thing)
    (restore-location self)
    (stop-walking self))
  (when (enemyp thing)
    (percent-of-time 20 (play-sample (random-choose '("unh-1.wav" "unh-2.wav" "unh-3.wav"))))))

(define-method die monk ()
  (when %alive
    (when (humanp self) 
      (change-image self "skull.png")
      (setf %alive nil))))

(define-method damage monk (points) (die self))

(define-method pressing-fire-p monk () nil)

(define-method pressing-fire monk ())

;;; Control logic driven by the above (possibly overridden) methods.

(defparameter *monk-speed* (truncate (/ *unit* 1.3)))

(define-method standing-animation monk () *monk-2-stand*)
(define-method walking-animation monk () *monk-2-walk*)

(define-method run monk ()
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
      (animate self (standing-animation self)))
    (with-fields (step-clock fire-clock) self
      (when (plusp step-clock)
	(decf step-clock))
      ;; find out what direction the AI or human wants to go
      (let ((heading (movement-heading self))
	    (fire-button (pressing-fire-p self)))
	(when (or (null heading)
		  (null %animation))
	  (animate self (standing-animation self)))
	(when heading 
	  (unless (eq %animation (walking-animation self))
	    (animate self (walking-animation self)))
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
	    (fire self %fire-direction fire-button)))))))

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

(defmonk geoffrey)

(define-method standing-animation geoffrey ()
  *monk-stand*)

(define-method walking-animation geoffrey ()
 *monk-walk*)

;; (define-method standing-animation geoffrey ()
;;   (if %bow-ready 
;;       *monk-stand-bow-ready*
;;       *monk-stand-bow*))

;; (define-method walking-animation geoffrey ()
;;   (if %bow-ready 
;;       *monk-walk-bow-ready*
;;       *monk-walk-bow*))

(define-method pressing-fire-p geoffrey () nil)
;  (holding-fire-button))

(define-method humanp geoffrey () t)

(define-method movement-heading geoffrey ()
  (with-fields (goal-x goal-y) self
    (when (and goal-x goal-y)
      (multiple-value-bind (x y) (center-point self)
	(when (< 15 (distance x y goal-x goal-y))
	  (find-heading x y goal-x goal-y))))))

(define-method movement-direction geoffrey ()
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

(defmonk lucius :clock 10)

(define-method run lucius ()
  (monk%run self)
  (decf %clock))

(define-method movement-heading lucius ()
  (with-fields (clock) self
    (when (cursor)
      (cond  ((> (distance-to-cursor self) 150)
	      (unless (plusp clock)
		(heading-to-cursor self)))
	     ((> (distance-to-cursor self) 110)
	      (prog1 nil (setf clock 10)))))))

(define-topic hello lucius 
   "Good morning Geoffrey! A Raven just
delivered this letter for you."
   :letter :weather :name :job :bye)
	   
(define-topic name lucius 
  "I am your friend Lucius, of course.")

(define-topic job lucius 
  "You know perfectly well that I work
at the Nothbess Library. My duties
include dusting and organizing books.
And else have you forgotten today?
Something must be wrong with you.")

(define-topic weather lucius 
"It's nice out today, but I feel as if
it's been a bit colder than usual.")

(define-topic letter lucius 
   "I wonder what it says? It comes
straight from Dr. Quine at the
monastery. Open it, open it!")

(define-method activate lucius ()
  (discuss self :hello))
