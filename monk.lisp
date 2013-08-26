(in-package :f0rest)

(defun simple-bounding-box (thing width &optional height)
  (let ((hw (/ width 2))
	(hh (/ (or height width 2))))
    (multiple-value-bind (x y) (center-point thing)
      (values (cfloat (- y hh))
	      (cfloat (- x hw))
	      (cfloat (+ x hw))
	      (cfloat (+ y hh))))))

(defconstant +animation-pixels-per-scale+ 500)

(defun sprite-bounding-box (thing image &optional (scale-base +animation-pixels-per-scale+))
  (multiple-value-bind (top left right bottom) (bounding-box thing)
    (let* ((image-height (image-height image))
	   (image-width (image-width image))
	   (height (- bottom top))
	   (width (- right left))
	   (scale (/ (min height width)
		     (min image-height image-width)))
	   (scaled-width (* scale scale-base))
	   (scaled-height (* scale scale-base))
	   (hw (/ scaled-width 2))
	   (hh (/ scaled-height 2)))
      (multiple-value-bind (x y) (center-point thing)
	(values (cfloat (- y hh))
		(cfloat (- x hw))
		(cfloat (+ x hw))
		(cfloat (+ y hh)))))))

(defun draw-as-sprite (thing image angle)
  (multiple-value-bind (top left right bottom)
      (sprite-bounding-box thing image)
    (draw-textured-rectangle-* left top 0
			       (- right left) (- bottom top)
			       (find-texture image)
			       :angle angle)))
      
(defparameter *monk-speed* (truncate (/ *unit* 1.3)))

(defun solidp (thing)
  (and (blockyp thing)
       (has-tag thing :solid)))

(defun targetp (thing)
  (and (blockyp thing)
       (has-tag thing :target)))

(defun enemyp (thing)
  (and (blockyp thing)
       (has-tag thing :enemy)))

(defun monkp (thing)
  (and (blockyp thing)
       (has-tag thing :monk)))

;;; Arrows, the main weapon

(define-block arrow :image (random-choose '("arrow-1.png" "arrow-2.png" "arrow-3.png")))

(define-method bounding-box arrow () 
  (simple-bounding-box self (units 4)))

(define-method collide arrow (thing)
  (when (or (enemyp thing) (solidp thing))
    (destroy self)))
 
;;; A monk, either AI or human controlled

(defun animation-scale (a) (getf a :scale 1.0))
(defun animation-repeat (a) (getf a :repeat))
(defun animation-frames (a) (getf a :frames))
(defun frame-image (f) (first f))
(defun frame-delay (f) (or (second f) 1))

(defparameter *monk-cast*
  '(:frames (("monk-cast-1.png" 3)
	     ("monk-cast-2.png" 4)
	     ("monk-cast-3.png" 4)
	     ("monk-cast-4.png" 4)
	     ("monk-cast-5.png" 8))))

(defparameter *monk-stand*
  '(:frames (("monk-stand-1.png" 19)
	     ("monk-stand-2.png" 24)
	     ("monk-stand-3.png" 13)
	     ("monk-stand-4.png" 20))))

(defparameter *monk-stand-bow*
  '(:frames (("monk-stand-bow-1.png" 19)
	     ("monk-stand-bow-2.png" 24)
	     ("monk-stand-bow-3.png" 13))))

(defparameter *monk-stand-bow-ready*
  '(:frames (("monk-stand-bow-ready-1.png" 19)
	     ("monk-stand-bow-ready-2.png" 24)
	     ("monk-stand-bow-ready-3.png" 13))))

(defparameter *monk-stand-images*
  '("monk-stand-1.png" "monk-stand-2.png" "monk-stand-3.png" "monk-stand-4.png"))

(defparameter *monk-walk* 
  '(:frames (("monk-walk-1.png" 4)
	     ("monk-walk-2.png" 4)
	     ("monk-walk-3.png" 4)
	     ("monk-walk-4.png" 4)
	     ("monk-walk-5.png" 4)
	     ("monk-walk-6.png" 4)
	     ("monk-walk-7.png" 4)
	     ("monk-walk-8.png" 4))))

(defparameter *monk-walk-bow* 
  '(:frames (("monk-walk-bow-1.png" 4)
	     ("monk-walk-bow-2.png" 4)
	     ("monk-walk-bow-3.png" 4)
	     ("monk-walk-bow-4.png" 4))))

(defparameter *monk-walk-bow-ready* 
  '(:frames (("monk-walk-bow-ready-1.png" 4)
	     ("monk-walk-bow-ready-2.png" 4)
	     ("monk-walk-bow-ready-3.png" 4)
	     ("monk-walk-bow-ready-4.png" 4))))

(define-block monk 
  ;; animation parameters
  (image :initform (random-choose *monk-stand-images*))
  (scale :initform nil)
  (frames :initform nil)
  (delay :initform 0)
  (repeat :initform nil)
  (animation :initform nil)
  ;; 
  (bow-ready :initform nil)
  (alive :initform t)
  (talking :initform nil)
  (walking :initform nil)
  (hearing-distance :initform 800)
  (tags :initform '(:monk))
  (direction :initform :up)
  (fire-direction :initform :up)
  ;; timers
  (retry-clock :initform (seconds->frames 5))
  (walk-clock :initform 0)
  (step-clock :initform 0)
  (fire-clock :initform 0)
  ;; we want to catch the beginning of firing, even if the input
  ;; polling in `update' misses it. (see below)
  (default-events :initform '(((:space) (strong-fire)))))

(define-method animation-frame monk ()
  (when %animation (frame-image (first %frames))))

(define-method animate monk (animation)
  (setf %scale (animation-scale animation))
  (setf %frames (animation-frames animation))
  (setf %repeat (animation-repeat animation))
  (setf %animation animation)
  (let ((image (animation-frame self)))
    (when image (setf %image image))))

(define-method update-animation monk ()
  (with-fields (animation frames delay scale repeat image) self
    (decf delay)
    (when (minusp delay)
      ;; done displaying current frame. show next, if any
      (let ((frame (pop frames)))
	(if frame
	    (setf delay (frame-delay frame))
	    ;; no more frames
	    (animate self (if repeat animation nil)))))))

(define-method humanp monk () nil)

;;; Animating the monk as he walks

(defparameter *monk-step-frames* 3)

(defparameter *monk-reload-frames* 10)

(defparameter *walk-interval* 16)

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
		  (+ (direction-degrees %direction) 90)))

  ;; (draw-textured-rectangle-* %x %y %z %width %height 

(define-method cast-spell monk ()
  (animate self *monk-cast*))

(defparameter *arrow-size* 25)

(define-method fire-location monk ()
  (with-fields (direction) self
    (multiple-value-bind (cx cy) (center-point self)
      (multiple-value-bind (tx ty) 
	  (step-in-direction cx cy direction (units 0.7))
	(values (- tx (* *arrow-size* 0.4))
		(- ty (* *arrow-size* 0.4)))))))
	  
(define-method fire monk ()
  (multiple-value-bind (x y) (fire-location self)
    (let ((arrow (new 'arrow (direction-heading %direction))))
      (drop-object (current-buffer) arrow x y))))

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

(define-method movement-direction monk () 
  (or (percent-of-time 3 (setf %direction (random-choose *directions*)))
      %direction))

(defvar *joystick-enabled* nil)

(define-method stick-heading monk () 
  (when (humanp self)
    (or (when (left-analog-stick-pressed-p)
	  (left-analog-stick-heading))
	(when (right-analog-stick-pressed-p)
	  (right-analog-stick-heading)))))

(define-method bounding-box monk ()
  ;; shrink bounding box by a few px to make game more forgiving
  (with-field-values (x y height width) self
    (let ((margin 2))
      (values (+ margin y) (+ margin x)
	      (+ (- margin) x width)
	      (+ (- margin) y height)))))

(define-method collide monk (thing)
  (when (solidp thing)
    (restore-location self)))

(define-method die monk ()
  (when %alive
    (when (humanp self) 
      (change-image self "skull.png")
      (setf %alive nil))))

(define-method damage monk (points) (die self))

(define-method strong-fire-p monk () nil)

(define-method fire monk (&optional direction strong)
  (when %alive
    (when (and (null *ball*) (zerop %fire-clock))
      (cast-spell self)
      (setf %fire-clock *monk-reload-frames*))))

(define-method strong-fire monk ()
  (fire self nil t))

;;; Control logic driven by the above (possibly overridden) methods.

(defparameter *monk-size* (* 8 *unit*))

(define-method update monk ()
  (when (dialogue-playing-p) (update-dialogue))
  ;; update standing/breathing anim
;  (when (null %animation) (setf %image "monk-stand-1.png"))
  (when (percent-of-time 5 t)
    (setf %image (random-choose *monk-stand-images*)))
  ;; possibly overwrite that image
  (update-animation self)
  ;; normal update
  (when %alive
    (resize self *monk-size* *monk-size*)
    (with-fields (step-clock fire-clock) self
      (when (plusp step-clock)
	(decf step-clock))
      ;; find out what direction the AI or human wants to go
      (let ((direction (movement-direction self))
	    (fire-button (strong-fire-p self)))
	(when (null direction)
	  (animate self nil))
	(when (and direction (null %animation)) (animate self *monk-walk*))
	(when direction 
	  ;; move in the movement direction
	  (move-toward self direction (/ *monk-speed* 2))
	  (setf %direction direction)
	  ;; possibly fire, lock firing dir when held
	  (unless (holding-fire)
	    (setf %fire-direction direction)))
	(if direction
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
	(when (zerop fire-clock)
	  (when fire-button
	    ;; yes, do it
	    (fire self %fire-direction fire-button)))))))



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

;; (define-method collide player-1-monk (thing)
;;   (when (enemyp thing)
;;     (restore

(define-method strong-fire-p player-1-monk ()
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
  
