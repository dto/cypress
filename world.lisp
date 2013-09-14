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

;;; Simple temporary tooltip bubble

(defparameter *bubble-font* "oldania-bubble")

(defresource (:name "oldania-bubble" 
	      :type :ttf 
	      :file "OldaniaADFStd-Regular.otf" 
	      :properties (:size 18)))

(define-block bubble 
  (tags :initform '(:bubble :ethereal))
  (text :initform nil) 
  (font :initform *bubble-font*)
  (collision-type :initform nil))

(define-method initialize bubble (text &optional (font *bubble-font*))
  (block%initialize self)
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

;;; Easily defining sets of images

(defun image-set (name count &optional (start 1))
  (loop for n from start to count
	collect (format nil "~A-~S.png" name n)))

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

;;; Fundamental object attributes in the world of Cypress

(define-block thing 
  (last-tap-time :initform nil)
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

(define-method can-pick thing ()
  (or (shell-open-p)
      (not (fixedp self))))

(define-method pick thing () self)

(defmacro defthing (name &body body)
  `(define-block (,name :super thing) ,@body))

(defparameter *default-thing-scale* (/ 1 (/ +dots-per-inch+ 130)))

(define-method layout thing ()
  (resize self 
	  (* %scale (image-width %image) *default-thing-scale*)
	  (* %scale (image-height %image) *default-thing-scale*)))

(define-method initialize thing ()
  (block%initialize self)
  (layout self))

(defun auto-describe (thing)
  (let ((name (object-name (find-object (find-super (find-object thing))))))
    (pretty-string (subseq name (1+ (position (character ":") name))))))

(define-method find-description thing ()
  (or %description
      (auto-describe self)))

(define-method look thing ()
  (drop self (new 'bubble (find-description self))
	%width 0))

(define-method use thing ())

(define-method run thing ())

(defparameter *double-tap-time* 8)

(define-method tap thing (x y)
  (with-fields (last-tap-time) self
    (let* ((time *updates*)
	   (elapsed-time (- time (or last-tap-time 0))))
      (cond ((null last-tap-time)
	     (setf last-tap-time time))
	    ((<= elapsed-time *double-tap-time*)
	     (setf last-tap-time nil)
	     (use self))))))

(define-method update thing ()
  (with-fields (last-tap-time) self
    (when (and last-tap-time
	       (> (- *updates* last-tap-time)
		  *double-tap-time*))
      (setf last-tap-time nil)
      (look self))
    (run self)))

;;; Sprites

(define-block (sprite :super thing)
  (sprite-height :initform nil)
  (sprite-width :initform nil))

(define-method draw sprite ()
  (draw-as-sprite self %image %heading))

(define-method layout sprite ()
  (setf %height %sprite-height)
  (setf %width %sprite-width))

(defmacro defsprite (name &body body)
  `(define-block (,name :super sprite) ,@body))

