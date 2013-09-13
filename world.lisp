(in-package :cypress)

;;; Spatial parameters

(defconstant +dots-per-inch+ 600)
(defparameter *unit* 14) 
(defun units (n) (* n *unit*))

;;; Object predicates

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

(defun bubblep (thing)
  (and (xelfp thing)
       (has-tag thing :bubble)))

;;; Simple temporary tooltip bubble

(defparameter *bubble-font* "oldania-bubble")

(defresource (:name "oldania-bubble" :type :ttf :file "OldaniaADFStd-Regular.otf" :properties (:size 18)))

(define-block bubble 
  (tags :initform '(:bubble))
  (text :initform nil) 
  (font :initform *bubble-font*)
  (collision-type :initform nil))

(define-method initialize bubble (text &optional (font *bubble-font*))
  (block%initialize self)
  (setf %text text)
  (setf %font font)
  ;; (set-buffer-bubble self)
  (later 5.0 (destroy self)))

;; (define-method destroy bubble ()
;;   ;; (set-buffer-bubble nil)
;;   (block%destroy self))

(define-method draw bubble ()
  (with-field-values (x y text font) self
    (let ((margin 2))
      (draw-box x y
		(+ (font-text-width text font) margin margin)
		(+ (font-height font) margin margin)
		:color "corns")
      (draw-string text 
		   (+ x margin)
		   (+ y margin)
		   :color "saddle brown"
		   :font font))))

;; (define-method update bubble ()
;;   (when (cursor)
;;     (multiple-value-bind (x y)
;; 	(right-of (cursor))
;;       (move-to self x y))))

;;; Easily defining sets of images

(defun image-set (name count &optional (start 1))
  (loop for n from start to count
	collect (format nil "~A-~S.png" name n)))

(defparameter *ruin-wall-images* (image-set "ruin-wall" 4))
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
  (frames :initform nil)
  (delay :initform 0)
  (repeat :initform nil)
  (animation :initform nil))

(defmacro defthing (name &body body)
  `(define-block (,name :super thing) ,@body))

(defparameter *default-thing-scale* (/ 1 (/ +dots-per-inch+ 130)))

(define-method layout thing ()
  (resize self 
	  (* (image-width %image) *default-thing-scale*)
	  (* (image-height %image) *default-thing-scale*)))

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

(define-block (sprite :super thing))

(define-method draw sprite ()
  (draw-as-sprite self %image %heading))

(define-method layout sprite () nil)

(defmacro defsprite (name &body body)
  `(define-block (,name :super sprite) ,@body))

;;; Now some objects

(defthing book :image (random-choose *book-images*))

(define-method collide book (thing)
  (when (monkp thing)
    (play-sample "wood.wav")
    (destroy self)))

(defthing scroll :image (random-choose *scroll-images*) :z 20)

(define-method use scroll ()
  (drop self (new 'scroll-gump *letter-text*)))

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

;;; ruin walls

(defthing ruin-wall 
  :image-scale 1000
  :image (random-choose *ruin-wall-images*)
  :tags '(:solid))

(defthing coverstone :image "coverstone.png" :z 10)
(defthing item-box :image "item-box.png" :z 1)

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
