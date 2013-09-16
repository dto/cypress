(in-package :cypress)

;;; Spatial parameters

(defconstant +dots-per-inch+ 600)
(defparameter *unit* 14) 
(defun units (n) (* n *unit*))

;;; Object predicates

(defun containerp (thing)
  (and (xelfp thing)
       (has-tag thing :container)))

(defun etherealp (thing)
  (and (xelfp thing)
       (has-tag thing :ethereal)))

(defun solidp (thing)
  (and (xelfp thing)
       (has-tag thing :solid)))

(defun fixedp (thing)
  (and (xelfp thing)
       (has-tag thing :fixed)))

(defun targetp (thing)
  (and (xelfp thing)
       (has-tag thing :target)))

(defun enemyp (thing)
  (and (xelfp thing)
       (has-tag thing :enemy)))

(defun monkp (thing)
  (and (xelfp thing)
       (has-tag thing :monk)))

(defun bubblep (thing)
  (and (xelfp thing)
       (has-tag thing :bubble)))

;;; Animation system

(defun image-set (name count &optional (start 1))
  (loop for n from start to count
	collect (format nil "~A-~S.png" name n)))

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

;;; Fundamental object attributes in the world of Cypress

(define-block thing 
  (last-tap-time :initform nil)
  (gump :initform nil)
  ;; world parameters
  (weight :initform 0)
  (inventory :initform nil)
  (hit-points :initform nil)
  (description :initform nil)
  (inscription :initform nil)
  ;; animation parameters
  (image-scale :initform +dots-per-inch+)
  (scale :initform 1)
  (frames :initform nil)
  (delay :initform 0)
  (repeat :initform nil)
  (animation :initform nil))

(define-method animation-frame thing ()
  (when %animation (frame-image (first %frames))))

(define-method animate thing (animation &optional force)
  (when (or force (not (eq %animation animation)))
    (setf %image-scale (animation-scale animation))
    (setf %frames (animation-frames animation))
    (setf %repeat (animation-repeat animation))
    (setf %animation animation)
    (setf %delay (frame-delay (first %frames)))))
  ;; (let ((image (animation-frame self)))
  ;;   (when image (setf %image image))))

(define-method update-animation thing ()
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

(define-method get-gump thing () %gump)

(define-method set-gump thing (gump) 
  (setf %gump gump)
  (drop self gump %width))

(define-method destroy-gump thing () 
  (when (xelfp %gump)
    (destroy %gump)
    (setf %gump nil)))

(define-method replace-gump thing (gump) 
  (when (xelfp %gump) (destroy %gump))
  (set-gump self gump))

(defun discussion-method (topic)
  (make-keyword (format nil "discuss/~A" (symbol-name topic))))

(defmacro define-topic (name super &body forms)
  `(define-method ,(make-non-keyword (discussion-method name)) ,super ()
     ,@(if (stringp (first forms))
	   (list (append 
		  (list 'make-talk-gump 'self (first forms))
		  (rest forms)))
	   forms)))
	  
(define-method discuss thing (topic) 
  (let ((method (discussion-method topic)))
    (let ((gump (send method self)))
      (if (xelfp gump)
	  (if (or (null %gump)
		  (%inputs (buttons gump)))
	      ;; replace whole gump 
	      (replace-gump self gump)
	      ;; just replace text in existing gump
	      (replace-gump-text %gump gump))
	  ;; no gump. quit conversation
	  (destroy-gump self)))))
    
(define-method can-pick thing ()
  (or (shell-open-p)
      (and (not (fixedp self))
	   (not (etherealp self)))))

(define-method pick thing () self)

(define-method can-accept thing () 
  (containerp self))

(define-method accept thing (thing)
  (push thing %inventory))

(define-method after-drag-hook thing ()
  (setf %z 
	(max %z
	     (1+ (maximum-z-value (current-buffer))))))

(defmacro defthing (name &body body)
  `(define-block (,name :super thing) ,@body))

(defparameter *default-thing-scale* (/ 1 (/ +dots-per-inch+ 130)))

(define-method layout thing ()
  (when %image 
    (resize self 
	    (* %scale (image-width %image) *default-thing-scale*)
	    (* %scale (image-height %image) *default-thing-scale*)))
  (arrange self))

(define-method create thing ())

(define-method initialize thing (&rest args)
  (block%initialize self)
  (apply #'xelf:send :create self args)
  (layout self))

(defun auto-describe (thing)
  (let ((name (object-name (find-object (find-super (find-object thing))))))
    (pretty-string (subseq name (1+ (position (character ":") name))))))

(define-method find-description thing ()
  (or %description (auto-describe self)))

(define-method look thing ()
  (drop self (new 'bubble (find-description self))
	%width 0))

(define-method activate thing ())

(define-method run thing ())

(define-method arrange thing ())

(defparameter *double-tap-time* 8)

(define-method tap thing (x y)
  (with-fields (last-tap-time) self
    (let* ((time *updates*)
	   (elapsed-time (- time (or last-tap-time 0))))
      (cond ((null last-tap-time)
	     (setf last-tap-time time))
	    ((<= elapsed-time *double-tap-time*)
	     (setf last-tap-time nil)
	     (activate self))))))

(define-method update thing ()
  (with-fields (last-tap-time) self
    ;; we actually catch the end of single-click here.
    (when (and last-tap-time
	       (> (- *updates* last-tap-time)
		  *double-tap-time*))
      (setf last-tap-time nil)
      (look self))
    ;; now run the object's in-gameworld update.
    (arrange self)
    (run self)))

;;; Simple temporary tooltip bubble

(defparameter *bubble-font* "oldania-bubble")

(defresource (:name "oldania-bubble" 
	      :type :ttf 
	      :file "OldaniaADFStd-Regular.otf" 
	      :properties (:size 18)))

(defthing bubble 
  (tags :initform '(:bubble :ethereal))
  (text :initform nil) 
  (font :initform *bubble-font*)
  (collision-type :initform nil))

(define-method create bubble (text &optional (font *bubble-font*))
  (setf %text text)
  (setf %font font)
  (later 4.0 (destroy self)))

(define-method draw bubble ()
  (with-field-values (x y text font) self
    (let ((margin 4))
      (draw-box x y
		(+ (font-text-width text font) margin margin)
		(+ (font-height font) margin margin)
		:color "cornsilk")
      (draw-string text 
		   (round (+ x margin))
		   (round (+ y margin))
		   :color "saddle brown"
		   :font font))))

;;; Sprites

(define-block (sprite :super thing)
  (sprite-height :initform nil)
  (sprite-width :initform nil))

(define-method draw sprite ()
  (draw-as-sprite self %image %heading))

(define-method layout sprite ()
  (setf %height %sprite-height)
  (setf %width %sprite-width)
  (arrange self))

(defmacro defsprite (name &body body)
  `(define-block (,name :super sprite) ,@body))

;;; Cypress

(define-buffer cypress 
  :background-image "meadow5.png"
  :quadtree-depth 8
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
    ;; ((:return) :enter)
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

(define-method alternate-tap cypress (x y)
  (walk-to (cursor) x y))

(define-method draw-object-layer cypress () 
  (multiple-value-bind (top left right bottom) (window-bounding-box self)
    (dolist (object (z-sort (get-objects self)))
      ;; only draw onscreen objects
      (when (colliding-with-bounding-box object top left right bottom)
	(draw object)
	(after-draw-object self object)))))

(defun make-meadow ()
    (let ((geoffrey (new 'geoffrey))
	  (lucius (new 'lucius))
	  (buffer (new 'cypress)))
      (add-object buffer geoffrey 320 120)
      (add-object buffer lucius 350 80)
      ;; adjust scrolling parameters 
      (setf (%window-scrolling-speed buffer) (cfloat (/ *monk-speed* 3))
	    (%horizontal-scrolling-margin buffer) 2/5
	    (%vertical-scrolling-margin buffer) 4/7)
      ;;
      (resize-to-background-image buffer)
      (set-cursor buffer geoffrey)
      (snap-window-to-cursor buffer)
      (glide-window-to-cursor buffer)
      (follow-with-camera buffer geoffrey)

      (drop-object buffer (new 'tent) 400 400)
      (drop-object buffer (new 'tent) 800 800)
      
      ;; (drop-object buffer (new 'circle-key) 420 700)
      ;; (drop-object buffer (new 'triangle-key) 420 850)
      ;; (drop-object buffer (new 'xalcium-leggings) 400 400)
      ;; (drop-object buffer (new 'xalcium-armor) 420 700)
      ;; (drop-object buffer (new 'xalcium-mail) 420 850)
      (dotimes (n 8)
      	(let ((x (+ 300 (random 1500)))
      	      (y (+ 300 (random 1000))))
	  (drop-object buffer (new 'gray-rock))) x y)


      ;; allocate
       (install-quadtree buffer)
      buffer))
