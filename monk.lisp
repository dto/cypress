(in-package :cypress)

(defun image-set (name count &optional (start 1))
  (loop for n from start to count
	collect (format nil "~A-~S.png" name n)))

(defparameter *book-images* (image-set "book" 10))
(defparameter *skull-images* (image-set "skull" 3))
(defparameter *wolf-skull-images* (image-set "wolf-skull" 3))
(defparameter *scroll-images* (image-set "scroll" 5))
(defparameter *remains-images* (image-set "remains" 2))
(defparameter *notebook-images* (image-set "notebook" 3))
(defparameter *wood-images* (image-set "wood" 4))
(defparameter *wraith-images* (image-set "wraith" 3))
(defparameter *arrow-images* (image-set "arrow" 3))
(defparameter *fire-pit-images* (image-set "fire-pit" 3))

(defconstant +dots-per-inch+ 600)
(defparameter *unit* 14) 
(defun units (n) (* n *unit*))

(defun solidp (thing)
  (and (xelfp thing)
       (has-tag thing :solid)))

(defun targetp (thing)
  (and (xelfp thing)
       (has-tag thing :target)))

(defun enemyp (thing)
  (and (xelfp thing)
       (has-tag thing :enemy)))

(defun monkp (thing)
  (and (xelfp thing)
       (has-tag thing :monk)))

;;; Animation system

(defun animation-scale (a) (getf a :scale +dots-per-inch+))
(defun animation-repeat (a) (getf a :repeat))
(defun animation-frames (a) (getf a :frames))
(defun frame-image (f) (first f))
(defun frame-delay (f) (or (second f) 1))

(defun simple-bounding-box (thing width &optional height)
  (let ((hw (/ width 2))
	(hh (/ (or height width 2))))
    (multiple-value-bind (x y) (center-point thing)
      (values (cfloat (- y hh))
	      (cfloat (- x hw))
	      (cfloat (+ x hw))
	      (cfloat (+ y hh))))))

(defun sprite-image-bounding-box (thing image)
  (multiple-value-bind (top left right bottom) (bounding-box thing)
    (let* ((image-height (image-height image))
	   (image-width (image-width image))
	   (height (- bottom top))
	   (width (- right left))
	   (scale-base (or (field-value :image-scale thing) +dots-per-inch+))
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

(defun draw-as-sprite (thing image heading)
  (multiple-value-bind (top left right bottom)
      (sprite-image-bounding-box thing image)
    (draw-textured-rectangle-* left top 0
			       (- right left) (- bottom top)
			       (find-texture image)
			       ;; adjust angle to normalize for up-pointing sprites 
			       :angle (+ 90 (heading-degrees heading)))))

(define-block thing 
  ;; world parameters
  (weight :initform 0)
  (hit-points :initform nil)
  ;; animation parameters
  (image-scale :initform +dots-per-inch+)
  (frames :initform nil)
  (delay :initform 0)
  (repeat :initform nil)
  (animation :initform nil))

(defmacro defthing (name &body body)
  `(define-block (,name :super thing) ,@body))

(defparameter *default-thing-scale* (/ 1 (/ +dots-per-inch+ 100)))

(define-method layout thing ()
  (resize self 
	  (* (image-width %image) *default-thing-scale*)
	  (* (image-height %image) *default-thing-scale*)))
	  
;;; Sprites

(define-block (sprite :super thing))

(define-method draw sprite ()
  (draw-as-sprite self %image %heading))

(define-method layout sprite () nil)

(defmacro defsprite (name &body body)
  `(define-block (,name :super sprite) ,@body))

(defthing scroll :image "scroll.png")
(define-method collide scroll (thing)
  (when (monkp thing)
    (play-sample "wood.wav")
    (destroy self)))

(defthing skull :image (random-choose '("skull-1.png" "skull-2.png")))

(define-method collide skull (thing)
  (when (monkp thing)
    (play-sample "wood.wav")
    (destroy self)))
(defthing remains :image (random-choose '("remains-1.png" "remains-2.png")))

;;; Arrows, the main weapon

(defparameter *arrow-size* 25)

(defsprite arrow
  :image-scale 40
  :image (random-choose *arrow-images*))

(define-method initialize arrow (heading)
  (block%initialize self)
  (setf %clock 400)
  (setf %heading heading))

(define-method collide arrow (thing)
  (cond ((enemyp thing) (damage thing 1) (destroy self))
	((solidp thing) (destroy self))))

(define-method update arrow ()
  (percent-of-time 13 (setf %image (random-choose *arrow-images*)))
  (resize self *arrow-size* *arrow-size*)
  (decf %clock)
  (if (minusp %clock)
      (destroy self)
      (forward self 15)))

;;; Wraiths

(defsprite wraith
  :seen-player nil
  :image-scale 600
  :tags '(:enemy)
  :hp 3
  :image (random-choose *wraith-images*))

(define-method damage wraith (points)
  (play-sample "knock.wav")
  (decf %hp points)
  (unless (plusp %hp)
    (drop self (new 'remains))
    (drop self (new 'skull))
    (percent-of-time 20 (drop self (new 'scroll) 40 40))
    (play-sample "lichdie.wav")
    (destroy self)))

(define-method update wraith ()
    (resize self 130 130)
  (when (< (distance-to-cursor self) 500)
    (unless %seen-player
      (play-sample "lichscream.wav")
      (setf %seen-player t))
    (percent-of-time 16 (setf %image (random-choose *wraith-images*)))
    (let ((heading (heading-to-cursor self)))
      (percent-of-time 13 
	(setf %heading heading))
      (percent-of-time 30
	(percent-of-time 12 (play-sample (random-choose '("growl-1.wav" "growl-2.wav"))))
	(move self %heading 4)))))

;;; A monk, either AI or human controlled

(defparameter *monk-cast*
  '(:scale 950
    :frames (("monk-cast-1.png" 3)
	     ("monk-cast-2.png" 4)
	     ("monk-cast-3.png" 4)
	     ("monk-cast-4.png" 4)
	     ("monk-cast-5.png" 6))))

(defparameter *monk-stand*
  '(:scale 600
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
    :scale 600
    :frames (("monk-walk-1.png" 4)
	     ("monk-walk-2.png" 4)
	     ("monk-walk-3.png" 4)
	     ("monk-walk-4.png" 4)
	     ("monk-walk-5.png" 4)
	     ("monk-walk-6.png" 4)
	     ("monk-walk-7.png" 4)
	     ("monk-walk-8.png" 4))))

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
    :scale 600
    :frames (("monk-2-walk-1.png" 4)
	     ("monk-2-walk-2.png" 4)
	     ("monk-2-walk-3.png" 4)
	     ("monk-2-walk-4.png" 4))))

(defparameter *monk-2-stand*
  '(:scale 600
    :frames (("monk-2-stand-1.png" 19)
	     ("monk-2-stand-2.png" 24))))

(defsprite monk
  (image :initform (random-choose *monk-stand-images*))
  (raising-bow :initform nil)
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
  (fire-clock :initform 0))

(defmacro defmonk (name &rest body)
  `(define-block (,name :super monk) ,@body))

;; (define-method can-accept monk () t)
;; (define-method accept monk (thing) t)

(define-method animation-frame monk ()
  (when %animation (frame-image (first %frames))))

(define-method animate monk (animation &optional force)
  (when (or force (not (eq %animation animation)))
    (setf %image-scale (animation-scale animation))
    (setf %frames (animation-frames animation))
    (setf %repeat (animation-repeat animation))
    (setf %animation animation)
    (setf %delay (frame-delay (first %frames)))))
  ;; (let ((image (animation-frame self)))
  ;;   (when image (setf %image image))))

(define-method update-animation monk ()
  (with-fields (animation frames delay scale repeat image) self
    (when animation
      (decf delay)
      (when (minusp delay)
	;; done displaying current frame. show next, if any
	(let ((frame (pop frames)))
	  (if frame
	      (setf image (frame-image frame)
		    delay (frame-delay frame))
	      ;; no more frames
	      (animate self (if repeat animation nil) t)))))))
    
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
		  (direction-heading %direction)))

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
    (restore-location self))
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

(defparameter *monk-size* (* 8 *unit*))

(defparameter *monk-speed* (truncate (/ *unit* 1.3)))

(define-method standing-animation monk () *monk-2-stand*)
(define-method walking-animation monk () *monk-2-walk*)

(define-method update monk ()
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
    (resize self *monk-size* *monk-size*)
    (with-fields (step-clock fire-clock) self
      (when (plusp step-clock)
	(decf step-clock))
      ;; find out what direction the AI or human wants to go
      (let ((direction (movement-direction self))
	    (fire-button (pressing-fire-p self)))
	(when (or (null direction)
		  (null %animation))
	  (animate self (standing-animation self)))
	(when direction 
	  (unless (eq %animation (walking-animation self))
	    (animate self (walking-animation self)))
	  ;; move in the movement direction
	  (move-toward self direction (/ *monk-speed* 2))
	  (setf %direction direction)
	  ;; lock fire direction when holding fire button
	  (unless (pressing-fire-p self) (setf %fire-direction direction)))
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
  (if %bow-ready 
      *monk-stand-bow-ready*
      *monk-stand-bow*))

(define-method walking-animation geoffrey ()
  (if %bow-ready 
      *monk-walk-bow-ready*
      *monk-walk-bow*))

(define-method pressing-fire-p geoffrey ()
  (holding-fire-button))

(define-method humanp geoffrey () t)

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

(define-method update lucius ()
  (monk%update self)
  (decf %clock))

(define-method movement-direction lucius ()
  (with-fields (clock) self
    (when (cursor)
      (cond  ((> (distance-to-cursor self) 150)
	      (unless (plusp clock)
		(direction-to-cursor self)))
	     ((> (distance-to-cursor self) 110)
	      (prog1 nil (setf clock 10)))))))
	   
;;; Meadow

(define-buffer meadow 
  :background-image "meadow2.png"
  :quadtree-depth 6
  :default-events
  '(((:pause) :transport-toggle-play)
    ((:e :alt) :edit-word)
    ((:x :control) :exec)
    ((:d :control) :delete-word)
    ((:c :control) :copy-word)
    ((:x :alt) :command-prompt)
    ((:g :control) :cancel)
    ((:c :alt) :clear-stack)
    ((:s :alt) :show-stack)
    ((:m :alt) :show-messages)
    ((:p :control) :transport-toggle-play)
    ((:return) :enter)
    ((:escape) :cancel)
    ((:f1) :help)
    ((:h :control) :help)
    ((:x :control) :edit-cut)
    ((:c :control) :edit-copy)
    ((:v :control) :edit-paste)
    ((:v :control :shift) :paste-here)
    ((:f9) :toggle-minibuffer)
    ((:f12) :transport-toggle-play)
    ((:g :control) :escape)
    ((:d :control) :drop-selection)))

(defthing wood :image "wood-1.png")

(define-method collide wood (thing)
  (when (monkp thing)
    (play-sample "wood.wav")
    (destroy self)))

(defun make-wood (&optional (n 0))
  (let ((wood (new 'wood)))
    (prog1 wood
      (change-image wood (nth (mod n 4) '("wood-1.png" "wood-2.png" "wood-3.png" "wood-4.png"))))))

(defun make-meadow ()
    (let ((geoffrey (new 'geoffrey))
	  (lucius (new 'lucius))
	  (buffer (new 'meadow)))
      (add-object buffer geoffrey 320 120)
      (add-object buffer lucius 350 80)
      ;; adjust scrolling parameters 
      (setf (%window-scrolling-speed buffer) (/ *monk-speed* 2)
	    (%horizontal-scrolling-margin buffer) 2/5
	    (%vertical-scrolling-margin buffer) 4/7)
      ;;
      (set-cursor buffer geoffrey)
      (snap-window-to-cursor buffer)
      (glide-window-to-cursor buffer)
      (follow-with-camera buffer geoffrey)

      (resize buffer 1254 2000)

      (drop-object buffer (new 'wraith) 800 600)
      (drop-object buffer (new 'scroll) 600 600)
      (drop-object buffer (new 'scroll) 640 610)
      ;; (drop-object buffer (make-wood 3) 100 150)
      ;; (drop-object buffer (make-wood 0) 100 180)

      ;; allocate
       (install-quadtree buffer)
      buffer))
