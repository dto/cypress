(in-package :valisade)

(defun targetp (thing)
  (and (blockyp thing)
       (has-tag thing :target)))

(defun enemyp (thing)
  (and (blockyp thing)
       (has-tag thing :enemy)))

(defun monkp (thing)
  (and (blockyp thing)
       (has-tag thing :monk)))
 
;;; A monk, either AI or human controlled

(defun animation-scale (a) (getf a :scale 1.0))
(defun animation-frames (a) (getf a :frames))
(defun frame-image (f) (first f))
(defun frame-delay (f) (or (second f) 1))

(defparameter *casting-animation*
  '(:scale 1.2 
    :frames (("monk-cast-1.png" 3)
	     ("monk-cast-2.png" 4)
	     ("monk-cast-3.png" 4)
	     ("monk-cast-4.png" 4))))
;	     ("monk-cast-0.png" 8))))

(define-block monk 
  ;; animation parameters
  (scale :initform nil)
  (frames :initform nil)
  (delay :initform 0)
  ;; 
  (leave-direction :initform nil)
  (alive :initform t)
  (talking :initform nil)
  (shielded :initform nil)
  (shield-clock :initform 0)
  (hearing-distance :initform 650)
  (body-color :initform "white")
  (color :initform "white")
  (carrying :initform nil)
  (tags :initform '(:monk :colored))
  (direction :initform :up)
  (kick-direction :initform :up)
  ;; timers
  (retry-clock :initform (seconds->frames 5))
  (walk-clock :initform 0)
  (step-clock :initform 0)
  (kick-clock :initform 0)
  ;; we want to catch the beginning of firing, even if the input
  ;; polling in `update' misses it. (see below)
  (default-events :initform '(((:space) (strong-kick)))))

(define-method animate monk (&optional animation)
  (setf %scale (when animation (animation-scale animation)))
  (setf %frames (when animation (animation-frames animation))))
  
(define-method animation-frame monk ()
  (with-fields (frames delay) self
    (when frames 
      (if (zerop delay)
	  (let ((frame (pop frames)))
	    (setf delay (frame-delay frame))
	    (frame-image frame))
	  (frame-image (first frames))))))

(define-method update-animation monk ()
  (with-fields (frames delay scale) self
    (if (null frames)
	(setf scale nil)
	(decf delay))))

(defparameter *monk-colors* '("gold" "olive drab" "RoyalBlue3" "dark orchid"))

(define-method initialize monk (&optional color)
  (initialize%super self)
  (when color (setf %body-color color)))

(define-method raise-shield monk ()
  ;; (play-sound self "go.wav")
  (setf %shielded t %shield-clock (seconds->frames 3)))

(define-method humanp monk () nil)

(defvar *ball-carrier* nil)

;;; Drawing the monk onscreen and animating his feet

(defparameter *monk-step-frames* 3)

(defparameter *monk-empty-color* "white")

(defparameter *monk-speed* (truncate (/ *unit* 1.3)))

(defparameter *monk-reload-frames* 10)

(defparameter *walk-interval* 16)

(define-method animate-walk monk ()
  (with-fields (walk-clock step-clock) self
    ;; only when moving
    (when (plusp step-clock)
      (when (plusp walk-clock)
	(decf walk-clock))
      (when (zerop walk-clock)
	(setf walk-clock *walk-interval*)))))

(defparameter *walking-right* 
  '("monk-right-1.png"
    "monk-right-lstep-1.png"
    "monk-right-lstep-2.png"
    "monk-right-2.png"
    "monk-right-rstep-1.png"
    "monk-right-rstep-2.png"))

(defparameter *walking-up* 
  '("monk-up-1.png"
    "monk-up-lstep-1.png"
    "monk-up-lstep-2.png"
    "monk-up-2.png"
    "monk-up-rstep-1.png"
    "monk-up-rstep-2.png"))
        
(defun monk-walking-image (direction clock)
  (let ((frames
	  (or 
	   (cond
	     ((member direction '(:up :down :upright :downleft))
	      *walking-up*)
	     ((member direction '(:left :right :upleft :downright))
	      *walking-right*))
	   *walking-up*)))
    (or (nth (truncate (* clock (/ 6 *walk-interval*)))
	     frames)
	(if (or (eq direction :left) (eq direction :right))
	    "monk-right-1.png"
	    "monk-up-1.png"))))

(define-method begin-talking monk (line)
  (setf %talking t))

(define-method stop-talking monk ()
  (setf %talking nil))

(define-method serve-location monk ()
  (with-fields (direction) self
    (multiple-value-bind (cx cy) (center-point self)
      (multiple-value-bind (tx ty) 
	  (step-in-direction cx cy direction (units 0.7))
	(values (- tx (* *ball-size* 0.4))
		(- ty (* *ball-size* 0.4)))))))

(define-method draw monk ()
  (let ((image 
	  (if %alive
	      (or
	       (animation-frame self)
	       (monk-walking-image %direction %walk-clock) 
	       "monk-up.png")
	      "skull.png")))
    (draw-textured-rectangle %x %y %z %width %height (find-texture image))
    (when %shielded 
      (multiple-value-bind (cx cy) (center-point self)
	(draw-circle cx cy 30 :color (random-choose '("deep pink" "yellow")))))))
	  
;; (define-method serve monk ()
;;   (multiple-value-bind (x y) (serve-location self)
;;     (make-ball %color)
;;     (drop-object (current-buffer) *ball* x y)))

;;; Cool vintage footstep and kick sounds

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

;; (defresource "kick.wav" :volume 23)
;; (defresource "serve.wav" :volume 23)

(defparameter *kick-sound* "kick.wav")

;;; Default AI methods. 

(define-method movement-direction monk () 
  (if %carrying
      (opposite-direction (direction-to-cursor self))
      (if (< (distance-to-cursor self) 400)
  	  (if *ball*
  	      (direction-to-thing self *ball*)
  	      (if (> (distance-to-cursor self) 250)
  		  (opposite-direction (direction-to-cursor self))
  		  (or (percent-of-time 3 (setf %direction (random-choose *directions*)))
  		      %direction))))))

(defvar *joystick-enabled* nil)

(define-method stick-heading monk () 
  (when (humanp self)
    (or (when (left-analog-stick-pressed-p)
	  (left-analog-stick-heading))
	(when (right-analog-stick-pressed-p)
	  (right-analog-stick-heading)))))

;; (define-method can-reach-ball monk ()
;;   (and *ball* (colliding-with self *ball*)))

(define-method bounding-box monk ()
  ;; shrink bounding box by a few px to make game more forgiving
  (with-field-values (x y height width) self
    (let ((margin 2))
      (values (+ margin y) (+ margin x)
	      (+ (- margin) x width)
	      (+ (- margin) y height)))))

(define-method collide monk (thing)
  (when (or (brickp thing) (enemyp thing) (holep thing) (cloudp thing))
    (restore-location self)))

;; (defresource "skull.png")

;; (defresource "analog-death.wav" :volume 70)

(define-method die monk ()
  (when (and %alive (not %shielded))
    (when (humanp self) 
      (change-image self "skull.png")
      (setf %alive nil))))

(define-method damage monk (points) (die self))

(define-method strong-kick-p monk () nil)

(define-method cast-spell monk ()
  (animate self *casting-animation*))

(define-method kick monk (&optional direction strong)
  (when %alive
    (when (and (null *ball*) (zerop %kick-clock))
      (cast-spell self)
      (setf %kick-clock *monk-reload-frames*))))

(define-method strong-kick monk ()
  (kick self nil t))

(define-method paint monk (color)
  (setf %color color))

;;; Control logic driven by the above (possibly overridden) methods.

(defparameter *monk-size* (* 7 *unit*))

(define-method update monk ()
  (when (dialogue-playing-p) (update-dialogue))
  (update-animation self)
  ;; possibly lower shields
  (when %shielded
    (decf %shield-clock)
    (unless (plusp %shield-clock)
      (setf %shielded nil)))
  ;; normal update
  (when %alive
    (resize self *monk-size* *monk-size*)
    (with-fields (step-clock kick-clock) self
      (when (plusp step-clock)
	(decf step-clock))
      ;; find out what direction the AI or human wants to go
      (let ((direction (movement-direction self))
	    (kick-button (strong-kick-p self)))
	(when direction 
	  ;; move in the movement direction
	  (move-toward self direction (/ *monk-speed* 2))
	  (setf %direction direction)
	  ;; possibly kick, lock firing dir when held
	  (unless (holding-fire)
	    (setf %kick-direction direction)))
	(if direction
	    ;; controller is pushing in a direction
	    (when (zerop step-clock)
	    ;; don't animate on every frame
	      (setf step-clock *monk-step-frames*)
	      ;; possibly make footstep sounds
	      (make-footstep-sounds self))
	    ;; not pushing. allow movement immediately
	    (setf step-clock 0 %walk-clock 0))
	;; update animation
	(animate-walk self)
	;; delay between kicks
	(when (plusp kick-clock)
	  (decf kick-clock))
	;; ready to kick?
	(when (zerop kick-clock)
	  (when kick-button
	    ;; yes, do it
	    (kick self %kick-direction kick-button)))))))

;; Player 1 drives the logic with the arrows/numpad and spacebar

(define-block (player-1-monk :super monk)
  (body-color :initform "white"))

(defun holding-space ()
  (keyboard-down-p :space))     

(defun holding-fire ()
  (or (holding-space)
      (holding-shift)
      (some #'joystick-button-pressed-p
	    '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20))))

(define-method collide player-1-monk (thing)
  (when (enemyp thing)
    (die self)))

(define-method strong-kick-p player-1-monk ()
  (holding-fire))

(define-method humanp player-1-monk () t)

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

(define-method movement-direction player-1-monk ()
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
  
