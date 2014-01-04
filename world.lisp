(in-package :cypress)

;;; Spatial parameters

(defconstant +dots-per-inch+ 600)
(defparameter *unit* 14) 
(defun units (n) (* n *unit*))

;;; Fundamental object attributes in the world of Cypress

(defblock thing 
  ;; world fields
  (quantity :initform 1)
  (weight :initform nil)
  (inventory :initform nil)
  (health :initform 0)
  (magic :initform 0)
  (hunger :initform 0)
  (fatigue :initform 0)
  (cold :initform 0)
  (description :initform nil)
  (inscription :initform nil)
  (container :initform nil)
  ;; animation fields
  (image-scale :initform +dots-per-inch+)
  (scale :initform 1)
  (frames :initform nil)
  (delay :initform 0)
  (repeat :initform nil)
  (animation :initform nil)
  ;; system fields
  (last-tap-time :initform nil)
  (gump :initform nil)
  ;; pathfinding
  (path :initform nil)
  (waypoints :initform nil)
  (goal-x :initform nil)
  (goal-y :initform nil))

(defmacro defthing (name &body body)
  (if (symbolp name) 
      `(defblock (,name thing) ,@body)
      `(defblock ,name ,@body)))

;;; Default resizing to image

(defparameter *default-thing-scale* (/ 1 (/ +dots-per-inch+ 130)))

(defmethod layout ((self thing))
  (with-fields (image scale) self
    (when image 
      (resize self 
	      (* scale (image-width image) *default-thing-scale*)
	      (* scale (image-height image) *default-thing-scale*)))))

(defmethod die ((self thing))
  (destroy self))

(defmethod initialize-instance :after ((self thing) &key)
  (layout self))

;;; Animation system

(defun animation-scale (a) (getf a :scale +dots-per-inch+))
(defun animation-repeat (a) (getf a :repeat))
(defun animation-frames (a) (getf a :frames))
(defun frame-image (f) (first f))
(defun frame-delay (f) (or (second f) 1))

(defun image-set (name count &optional (start 1))
  (loop for n from start to count
	collect (format nil "~A-~S.png" name n)))

(defmethod current-animation-frame ((self thing))
  (with-fields (animation frames) self
    (when animation (frame-image (first frames)))))

(defmethod begin-animation ((self thing) animation &optional force)
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
	      (begin-animation self (if repeat animation nil) t)))))))

;;; Pathfinding 

(defmethod next-waypoint ((self thing))
  (with-local-fields 
    (destructuring-bind (wx wy) (pop %waypoints)
      (setf %goal-x wx %goal-y wy))))

(defmethod movement-heading ((self thing))
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

(defmethod walk-to ((self thing) x1 y1)
  (with-fields (x y waypoints path) self
    (when (null path) 
      (setf path (create-path self :buffer (current-buffer))))
    (setf waypoints (rest (rest (find-path-waypoints path x y x1 y1))))
    (when (null waypoints) (stop-walking self))))
  
(defmethod stop-walking ((self thing))
  (with-fields (waypoints goal-x goal-y) self
    (setf waypoints nil)
    (setf goal-x nil goal-y nil)))

;;; Inventory management

(defparameter *maximum-inventory-size* 16)

(defmethod inventory-items ((container thing))
  (field-value :inventory container))

(defmethod add-inventory-item ((container thing) (item thing))
  (with-fields (inventory) container
    (let ((the-item (find-object item)))
      (pushnew the-item inventory :test 'eq)
      (setf (field-value :container the-item) container))))

(defmethod remove-inventory-item ((container thing) (item thing))
  (with-fields (inventory) container
    (setf inventory (remove (find-object item) inventory :test 'eq))
    (setf (field-value :container (find-object item)) nil)))

(defmethod destroy :before ((self thing))
  (with-fields (container) self
    (when container
      (remove-inventory-item container self))))

(defmethod find-inventory-item ((container thing) &optional (class 'thing))
  (dolist (item (inventory-items container))
    (when (typep item class)
      (return item))))

(defmethod consume ((consumer thing) (consumed thing))) ;;; possibly nothing

(defmethod quantity ((self thing))
  (field-value :quantity self))

(defmethod modify-quantity ((self thing) points)
  (prog1 self 
    (with-fields (quantity) self
      (setf quantity 
	    (max 0 (+ quantity points))))))

(defmethod add-quantity ((container thing) item-class &optional (quantity 1))
  (let ((item (find-inventory-item container item-class)))
    (when item (modify-quantity item quantity))))

(defmethod merge-inventory-item ((container thing) item)
  (let* ((item-class (class-name (class-of item)))
	 (item (find-inventory-item container item-class)))
    (if item 
	(progn 
	  (add-quantity container item-class (quantity item))
	  (destroy item))
	(add-inventory-item container item))))

(defmethod remove-quantity ((container thing) item-class &optional (quantity 1))
  (let ((item (find-inventory-item container item-class)))
    (when item (modify-quantity item (- quantity)))))

(defmethod take-quantity ((container thing) item-class &optional (quantity 1))
  (assert (plusp quantity))
  (let ((item (find-inventory-item container item-class)))
    (when item 
      (when (plusp (quantity item))
	(modify-quantity item quantity)
	;; remove the ghosts of departed quantities
	(when (not (plusp (quantity item)))
	  (remove-inventory-item container item)
	  (destroy item))
	;; send back the separated quantities
	(let (items)
	  (dotimes (n quantity)
	    (push (new item-class) items))
	  items)))))

(defun quantity-of (item-class new-quantity)
  (assert (plusp new-quantity))
  (let ((item (new item-class)))
    (prog1 item
      (with-fields (quantity) item
	(setf quantity new-quantity)))))

(defmethod take-one ((container thing) item-class)
  (first (take-quantity container item-class 1)))

(defmethod has-quantity ((container thing) item-class &optional (quantity 1))
  (let ((item (find-inventory-item container item-class)))
    (when item
      (>= (quantity item) quantity))))

(defmethod consume-quantity ((container thing) item-class &optional (quantity 1))
  (if (has-quantity container item-class quantity)
      (dolist (item (take-quantity container item-class quantity))
	(consume container item))
      (error "Not enough.")))

(defmethod modify-health ((self thing) points)
  (with-fields (health) self
    (incf health points)
    (when (not (plusp health))
      (die self))))

(defmethod modify-magic ((self thing) points)
  (with-fields (magic) self
    (incf magic points)))

(defmethod modify-fatigue ((self thing) points)
  (with-fields (fatigue) self
    (incf fatigue points)))

(defmethod modify-hunger ((self thing) points)
  (with-fields (hunger) self
    (incf hunger points)))

(defmethod modify-cold ((self thing) points)
  (with-fields (cold) self
    (incf cold points)))

;;; Attaching gumps to things

(defmethod get-gump ((self thing)) 
  (field-value :gump self))

(defmethod set-gump ((self thing) gump)
  (setf (field-value :gump self) 
	(find-object gump))
  (drop self gump (field-value :width self)))

(defmethod destroy-gump ((self thing))
  (with-fields (gump) self
    (when (xelfp gump)
      (destroy gump)
      (setf gump nil))))

(defmethod replace-gump ((self thing) gump)
  (destroy-gump self)
  (set-gump self gump))

;;; Dragging objects to move them
    
(defmethod can-pick ((self thing))
  (or (shell-open-p)
      (and (not (fixedp self))
	   (not (etherealp self)))))

(defmethod pick ((self thing)) self)

(defmethod fullp (inventory)
  (>= (length inventory) 
      *maximum-inventory-size*))

(defmethod refresh ((self thing)) nil)

(defmethod can-accept ((self thing)) nil)

(defmethod will-accept ((container thing) (item thing))
  (can-accept container))

(defmethod accept ((container thing) (item thing))
  (prog1 t
    (with-fields (gump) container
      (destroy-gump item)
      (remove-object (current-buffer) item)
      (add-inventory-item container item)
      (if (null gump)
	  (message "No gump for ~S" container)
	  (progn (message "Refreshing ~S of ~S" gump container)
		 (refresh gump))))))

(defmethod bring-to-front ((self thing))
  (when (current-buffer)
    (with-fields (z) self
      (setf z (max (or z 1)
		   (1+ (maximum-z-value (current-buffer))))))))

(defmethod drop-object :after ((buffer buffer) (self thing) &optional x y z)
  (bring-to-front self))

;;; Describing and naming objects 

(defun fancy-description (thing)
  (pretty-string (class-name (class-of (find-object thing)))))

(defmethod find-description ((self thing)) 
  (or (field-value :description self)
      (fancy-description self)))

(defmethod look ((self thing))
  (drop self 
	(new 'bubble :text (find-description self))
	;; just to the right of object
	(field-value :width self) 0))

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

;;; Double-clicking an object causes it to activate.

(defmethod use ((self thing) (object thing)))

(defmethod activate ((self thing))
  (use (cursor) self))

;;; The system update function does its own work, then invokes the
;;; gameworld's RUN method.

(defmethod run ((self thing)))
(defmethod arrange ((self thing)))

(defmethod update ((self thing))
  (with-fields (last-tap-time) self
    ;; we actually catch the end of single-click here.
    (when (and last-tap-time
	       (> (- *updates* last-tap-time)
		  *double-tap-time*))
      (setf last-tap-time nil)
      ;; display object's class name (by default on single click)
      (look self))
    (arrange self)
    (run self)))

(defun discussion-method (topic)
  (make-keyword (format nil "discuss/~A" (symbol-name topic))))

(defmacro define-topic (name super &body forms)
  `(define-method ,(make-non-keyword (discussion-method name)) ,super ()
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
		  (%inputs (buttons gump)))
	      ;; replace whole gump 
	      (replace-gump self gump)
	      ;; just replace text in existing gump
	      (replace-gump-text gump gump))
	  ;; no gump. quit conversation
	  (destroy-gump self)))))

;;; Sprites 

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

(defblock (sprite thing)
  (sprite-height :initform nil)
  (sprite-width :initform nil))

(defmethod draw ((self sprite))
  (with-field-values (image heading) self
    (draw-as-sprite self image heading)))

(defmethod layout ((self sprite))
  (with-local-fields 
    (setf %height %sprite-height)
    (setf %width %sprite-width)))

(defmacro defsprite (name &body body)
  `(defblock (,name sprite) ,@body))

;;; Highlighting the object to be dropped upon

(defresource "check-button.png")
(defresource "x-button.png")

(defmethod draw-hover ((self thing))
  (with-fields (x y) self
    (with-fields (drag) (current-buffer)
      (when drag
	(when (will-accept self drag)
	  (draw-image "check-button.png" x y :height 25 :width 25))))))

;;; Simple temporary tooltip bubble

(defparameter *bubble-font* "oldania-bubble")

(defresource (:name "oldania-bubble" 
	      :type :ttf 
	      :file "OldaniaADFStd-Regular.otf" 
	      :properties (:size 18)))

(defblock (bubble thing)
  (tags :initform '(:bubble :ethereal))
  (text :initform nil) 
  (font :initform *bubble-font*)
  (collision-type :initform nil))

(defmethod initialize ((self bubble) &key text (font *bubble-font*))
  (with-local-fields 
    (setf (field-value :text self) text)
    (setf (field-value :font self) font)
    (later 4.0 (destroy self))))

(defmethod add-object :after ((buffer buffer) (self bubble) &optional x y z)
  (bring-to-front self))

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

;;; Object predicates

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

;; ;;; Actions

;; (defthing action)

;; (defmethod use ((target thing) (spell action)))

;; (defthing (fire action))

;; (defthing (spell action))

;; (defmethod use ((target thing) (spell spell))

;; (defthing (spark spell)
;; (defthing (light spell)



;;; Cypress

(defvar *status-line* nil)

(define-buffer cypress 
  :background-image (random-choose '("stone-road.png" "paynes-meadow.png"))
  :quadtree-depth 8
  :default-events
  '(((:pause) :transport-toggle-play)
    ((:r :control) :reset-game)
    ((:space) :transport-toggle-play)
    ((:p :control) :transport-toggle-play)))

(defmethod initialize :after ((buffer cypress) &key)
  (setf *status-line* (new 'status-line)))

(defmethod alternate-tap ((buffer cypress) x y)
  (multiple-value-bind (top left right bottom)
      (bounding-box (cursor))
    ;; walk the monk's center point to the destination point
    (let ((height (- bottom top))
	  (width (- right left)))
      (walk-to (cursor)
	       (- x (/ width 2))
	       (- y (/ height 2))))))

(defmethod draw-object-layer ((buffer cypress))
  (multiple-value-bind (top left right bottom) 
      (window-bounding-box buffer)
    (dolist (object (mapcar #'find-object (z-sort (get-objects buffer))))
      ;; only draw onscreen objects
      (when (colliding-with-bounding-box object top left right bottom)
	(draw object)
	(after-draw-object buffer object)))))

(defmethod update :after ((self cypress))
  (when (cursor)
    (layout *status-line*)
    (update *status-line*)))

(defmethod draw :after ((self cypress))
  (with-fields (drag hover) self
    (when drag (draw drag))
    (when hover (draw-hover (find-object hover)))))
  ;; (when (cursor)
  ;;   (draw *status-line*)))

(define-method reset-game cypress ()
  (switch-to-buffer (make-meadow)))


