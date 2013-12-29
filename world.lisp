(in-package :cypress)

;;; Spatial parameters

(defconstant +dots-per-inch+ 600)
(defparameter *unit* 14) 
(defun units (n) (* n *unit*))

;;; Object predicates

(defun containerp (thing)
  (and (xelfp thing)
       (has-tag (find-object thing) :container)))

(defun etherealp (thing)
  (and (xelfp thing)
       (has-tag (find-object thing) :ethereal)))

(defun solidp (thing)
  (and (xelfp thing)
       (has-tag (find-object thing) :solid)))

(defun fixedp (thing)
  (and (xelfp thing)
       (has-tag (find-object thing) :fixed)))

(defun targetp (thing)
  (and (xelfp thing)
       (has-tag (find-object thing) :target)))

(defun enemyp (thing)
  (and (xelfp thing)
       (has-tag (find-object thing) :enemy)))

(defun monkp (thing)
  (and (xelfp thing)
       (has-tag (find-object thing) :monk)))

(defun bubblep (thing)
  (and (xelfp thing)
       (has-tag (find-object thing) :bubble)))

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
	   ;; TODO compute separate scales
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

(defblock thing 
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

(defmethod animation-frame ((self thing))
  (with-fields (animation frames) self
    (when animation (frame-image (first frames)))))

(defmethod animate ((self thing) animation &optional force)
  (with-fields (image-scale frames repeat delay) self
    (when (or force (not (eq (field-value :animation self)
			     animation)))
      (setf image-scale (animation-scale animation))
      (setf frames (animation-frames animation))
      (setf repeat (animation-repeat animation))
      (setf (field-value :animation self) animation)
      (setf delay (frame-delay (first frames))))))

(defmethod update-animation ((self thing))
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

(defmethod get-gump ((self thing)) 
  (field-value :gump self))

(defmethod set-gump ((self thing) gump)
  (setf gump (find-object gump))
  (drop self gump (field-value :width self)))

(defmethod destroy-gump ((self thing))
  (with-fields (gump) self
    (when (xelfp gump)
      (destroy gump)
      (setf gump nil))))

(defmethod replace-gump ((self thing) gump)
  (destroy-gump self)
  (set-gump self gump))

(defun discussion-method (topic)
  (make-keyword (format nil "discuss/~A" (symbol-name topic))))

(defmacro define-topic (name super &body forms)
  `(defmethod ,(make-non-keyword (discussion-method name)) ((self ,super))
     ,@(if (stringp (first forms))
	   (list (append 
		  (list 'make-talk-gump 'self (first forms))
		  (rest forms)))
	   forms)))
	  
(defmethod discuss ((self thing) topic)
  (let ((method (discussion-method topic)))
    (let ((gump (send method self)))
      (if (xelfp gump)
	  (if (or (null gump)
		  (inputs (field-value :buttons gump)))
	      ;; replace whole gump 
	      (replace-gump self gump)
	      ;; just replace text in existing gump
	      (replace-gump-text gump gump))
	  ;; no gump. quit conversation
	  (destroy-gump self)))))
    
(defmethod can-pick ((self thing))
  (or (shell-open-p)
      (and (not (fixedp self))
	   (not (etherealp self)))))

(defmethod pick ((self thing)) self)

(defmethod can-accept ((self thing)) () 
  (containerp self))

(defmethod accept ((self thing) thing)
  (push thing (field-value :inventory self)))

(defmethod after-drag-hook ((self thing))
  (with-fields (z) self
    (setf z (max z
		 (1+ (maximum-z-value (current-buffer)))))))

(defmacro defthing (name &body body)
  `(defblock (,name :super thing) ,@body))

(defparameter *default-thing-scale* (/ 1 (/ +dots-per-inch+ 130)))

(defmethod layout ((self thing))
  (with-fields (image scale) self
    (when image 
      (resize self 
	      (* scale (image-width image) *default-thing-scale*)
	      (* scale (image-height image) *default-thing-scale*)))))

(defmethod initialize-instance :after ((self thing) &key)
  (layout self))

(defun auto-describe (thing)
  (pretty-string (class-name (class-of (find-object thing)))))

(defmethod find-description ((self thing)) 
  (or (field-value :description self)
      (auto-describe self)))

(defmethod look ((self thing))
  (drop self 
	(new 'bubble :text (find-description self))
	;; just to the right of object
	(field-value :width self) 0))

(defmethod activate ((self thing)))

(defmethod run ((self thing)))

(defmethod arrange ((self thing)))

;;; Detecting click and double-click

(defparameter *double-tap-time* 8)

(defmethod tap ((self thing) x y)
  (with-fields (last-tap-time) self
    (let* ((time *updates*)
	   (elapsed-time (- time (or last-tap-time 0))))
      (cond ((null last-tap-time)
	     (setf last-tap-time time))
	    ((<= elapsed-time *double-tap-time*)
	     (setf last-tap-time nil)
	     (activate self))))))

;;; The system update function does its own work, then invokes the
;;; gameworld

(defmethod update ((self thing))
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

(defmethod initialize ((self bubble) &key text (font *bubble-font*))
  (with-local-fields 
    (setf (field-value :text self) text)
    (setf (field-value :font self) font)
    (later 4.0 (destroy self))))

(defmethod draw ((self bubble))
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

(defblock (sprite :super thing)
  (sprite-height :initform nil)
  (sprite-width :initform nil))

(defmethod draw ((self sprite))
  (with-field-values (image heading) self
    (draw-as-sprite self image heading)))

(defmethod layout ((self sprite))
  (with-local-fields 
    (setf %height %sprite-height)
    (setf %width %sprite-width)
    (arrange self)))

(defmacro defsprite (name &body body)
  `(defblock (,name :super sprite) ,@body))

;;; Cypress

(define-buffer cypress 
  :background-image "meadow5.png"
  :quadtree-depth 8
  :default-events nil)
  ;; '(((:pause) :transport-toggle-play)
  ;;   ((:e :alt) :edit-word)
  ;;   ((:x :control) :exec)
  ;;   ((:d :control) :delete-word)
  ;;   ((:c :control) :copy-word)
  ;;   ((:x :alt) :command-prompt)
  ;;   ((:g :control) :cancel)
  ;;   ((:c :alt) :clear-stack)
  ;;   ((:s :alt) :show-stack)
  ;;   ((:m :alt) :show-messages)
  ;;   ((:p :control) :transport-toggle-play)
  ;;   ;; ((:return) :enter)
  ;;   ((:escape) :cancel)
  ;;   ((:f1) :help)
  ;;   ((:h :control) :help)
  ;;   ((:x :control) :edit-cut)
  ;;   ((:c :control) :edit-copy)
  ;;   ((:v :control) :edit-paste)
  ;;   ((:v :control :shift) :paste-here)
  ;;   ((:f9) :toggle-minibuffer)
  ;;   ((:f12) :transport-toggle-play)
  ;;   ((:g :control) :escape)
  ;;   ((:d :control) :drop-selection)))

(defmethod alternate-tap ((self cypress) x y)
  (walk-to (cursor) x y))

(defmethod draw-object-layer ((self cypress))
  (multiple-value-bind (top left right bottom) (window-bounding-box self)
    (dolist (object (mapcar #'find-object (z-sort (get-objects self))))
      ;; only draw onscreen objects
      (when (colliding-with-bounding-box object top left right bottom)
	(draw object)
	(after-draw-object self object)))))

