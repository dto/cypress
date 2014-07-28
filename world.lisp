(in-package :cypress)

(defvar *current-objective* nil)
(defvar *objectives* nil)

(defun set-objective (string)
  (unless (find string *objectives* :test 'equal) 
    (narrate "New objective: ~A" string)
    (push string *objectives*)
    (magical-flourish)
    (setf *current-objective* string)))

(defparameter *paused* nil)

(defvar *events* nil)

(defun add-event (key)
  (pushnew key *events* :test 'eq))

(defun event-occurred-p (key)
  (find key *events* :test 'eq))

(defun pause () 
  (setf *paused* t))

(defun resume () ()
  (setf *paused* nil))

;;; Spatial parameters

(defconstant +dots-per-inch+ 600)
(defparameter *unit* 14) 
(defun units (n) (* n *unit*))

;;; Finding all objects of a given class in a buffer

(defun find-instances (buffer class-name)
  (when (typep buffer (find-class 'buffer))
    (with-fields (objects) buffer
      (when objects
	(loop for thing being the hash-values in objects
	      when (typep (find-object thing t) (find-class class-name))
		collect (find-object thing t))))))

;;; Fundamental object attributes in the world of Cypress

(defblock thing 
  (excluded-fields :initform '(:quadtree-node :path))
  (colliding-objects :initform nil)
  ;; world fields
  (quantity :initform 1)
  (stacking :initform t)
  (description :initform nil)
  (inscription :initform nil)
  (container :initform nil)
  (equipper :initform nil)
  (weight :initform nil)
  (inventory :initform nil)
  (spells :initform nil)
  ;; player attributes
  (health :initform 0)
  (magic :initform 0)
  (hunger :initform 0)
  (cold :initform 0)
  ;; modifiers
  (attack :initform 0)
  (defense :initform 0)
  (resistance :initform 0)
  ;; time stop
  (stasis :initform nil)
  ;; conversation and lore fields
  (topics :initform '(:name :job :bye))
  (lore :initform nil)
  ;; animation fields
  (image-scale :initform +dots-per-inch+)
  (contained-image :initform nil)
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
  (safe-x :initform nil)
  (safe-y :initform nil)
  (goal-x :initform nil)
  (goal-y :initform nil))

(defmacro defthing (name &body body)
  (if (symbolp name) 
      `(defblock (,name thing) ,@body)
      `(defblock ,name ,@body)))

(defmethod find-lore ((thing thing))
  (field-value :lore thing))

;;; Default resizing to image

(defparameter *default-thing-scale* (/ 1 (/ +dots-per-inch+ 130)))

(defmethod layout ((self thing))
  (with-fields (image scale) self
    (when image 
      (when (find-resource image :noerror)
	(resize self 
		(* scale (image-width image) *default-thing-scale*)
		(* scale (image-height image) *default-thing-scale*))))))

(defmethod die ((self thing))
  (destroy self))

;; (defmethod destroy :after ((self thing))
;;   (with-fields (container) self
;;     (when container
;;       (let ((browser (get-gump container)))
;; 	(when (typep browser (find-class 'browser))
;; 	  (refresh browser))))))

(defmethod initialize :after ((self thing) &key)
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

;;; Action to run upon entering/exiting a scene

(defmethod enter-scene ((self thing)) nil)

(defmethod exit-scene ((self thing))
  (when (current-scene) 
    (remove-object (current-scene) self))
  (setf (field-value :path self) nil)
  (setf (field-value :safe-x self) nil)
  (setf (field-value :safe-y self) nil)
  (setf (field-value :waypoints self) nil))

;;; Pathfinding 

(defmethod find-colliding-objects ((self thing))
  (loop for object being the hash-keys of (field-value :objects (current-scene))
	when (colliding-with self (find-object object))
	  collect (find-object object)))

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
    (when (null waypoints) 
      (stop-walking self))
    waypoints))

;; (defmethod return-to-safe-point ((self thing))
;;   (with-local-fields 
;;     (when (numberp %safe-x)
;;       (prog1 t (move-to self %safe-x %safe-y)))))

;; (defmethod save-safe-point-maybe ((self thing))
;;   (with-local-fields 
;;     (unless (find-colliding-objects self)
;;       (setf %safe-x %x %safe-y %y))))

;; (defmethod will-obstruct ((self thing) (other thing))
;;   (if (has-tag self :round)
;;       (with-fields (width) self
;; 	(< (distance-between self other)
;; 	   (+ (units 2) (/ width 2))))
;;       (has-tag self :solid)))

(defmethod can-walk-to ((self thing) x1 y1)
  (with-fields (x y waypoints path) self
    (when (null path) 
      (setf path (create-path self :buffer (current-buffer))))
    (find-path-waypoints path x y x1 y1)))

(defmethod walk-to-thing ((self thing) (destination thing))
  (multiple-value-bind (x y) (center-point destination)
    (walk-to self x y)))

(defmethod stop-walking ((self thing))
  (with-fields (waypoints goal-x goal-y) self
    (setf waypoints nil)
    (setf goal-x nil goal-y nil)))

;;; Inventory management

(defparameter *maximum-inventory-size* 16)

(defmethod inventory-items ((container thing))
  (field-value :inventory container))

(defmethod valid-container ((container thing))
  (every #'(lambda (item)
	     (and (plusp (quantity item))
		  (eq container (find-container item))))
	 (inventory-items container)))

;; (defmethod duplicate-safely :after ((container thing))
;;   (with-fields (inventory) container
;;     (setf inventory (mapcar #'duplicate-safely inventory))
;;     (dolist (item inventory)
;;       (setf (field-value :container item) container))))

(defmethod find-container ((item thing))
  (field-value :container item))

(defmethod find-inventory-item ((container thing) item-class)
  (assert (valid-container container))
  (dolist (item (inventory-items container))
    (prog1 nil
      (when (typep item (find-class item-class))
	(return item))
      ;; search in containers
      (when (inventory-items item)
	(let ((i2 (find-inventory-item item item-class)))
	  (when i2 (return i2)))))))

(defmethod merge-inventory-item ((container thing) (item thing))
  (assert (valid-container container))
  (let* ((item-class (class-name (class-of item)))
	 (existing-item (find-inventory-item container item-class)))
    (assert existing-item)
    (add-quantity container item-class (quantity item))
    (destroy item)))

(defmethod add-inventory-item ((container thing) (item thing) &optional (merge t))
  (assert (valid-container container))
  (with-fields (inventory) container
    (if (or (not merge)
	    ;; don't allow container items themselves to stack, ever
	    (field-value :inventory item)
	    ;; don't allow non-stackable items to stack
	    (not (field-value :stacking item))
	    ;; we don't already have something
	    (not (find-inventory-item container 
				      (class-name (class-of item)))))
	(progn 
	  (setf inventory (append inventory (list (find-object item))))
	  (setf (field-value :container item) container))
	(merge-inventory-item container (find-object item))))
  (assert (valid-container container)))

(defmethod add-inventory-item :after ((container thing) (item thing) &optional (merge t))
  (assert (valid-container container))
  (let ((gump (get-gump container)))
    (when gump (refresh gump))))
  
(defmethod remove-inventory-item ((container thing) (item thing))
  (with-fields (inventory) container
    (let ((count (length inventory)))
      (setf inventory (remove (find-object item) inventory :test 'eq))
      (assert (< (length inventory) count))
      (setf (field-value :container (find-object item)) nil)
      inventory))
  (assert (valid-container container)))

(defmethod destroy :before ((self thing))
  (with-fields (container) self
    (when container
      (remove-inventory-item container self))))

(defun make-container (class contents)
  (let ((bag (new class)))
    (dolist (item contents)
      (add-inventory-item bag item))
    (assert (valid-container bag))
    bag))

(defmethod consume ((consumer thing) (consumed thing))) ;;; possibly nothing

(defmethod quantity ((self thing))
  (field-value :quantity self))

(defmethod modify-quantity ((self thing) points)
  (with-fields (quantity) self
    (setf quantity 
	  (max 0 (+ quantity points)))))

(defmethod add-quantity ((container thing) item-class &optional (quantity 1))
  (let ((item (find-inventory-item container item-class)))
    (when item (modify-quantity item quantity)))
  (assert (valid-container container)))

(defmethod remove-quantity ((container thing) item-class &optional (quantity 1))
  (let ((item (find-inventory-item container item-class)))
    (when item (modify-quantity item (- quantity))))
  (assert (valid-container container)))

(defmethod consume-single ((character thing) item-class &optional source)
  (let ((item (or source (find-inventory-item character item-class))))
    (when item 
      (let ((container (field-value :container item)))
	(when (plusp (quantity item))
	  (modify-quantity item -1)
	  (let ((new-item (new item-class)))
	    (prog1 new-item
	      ;; ensure new item has quantity one
	      (setf (field-value :quantity new-item) 1)
	      (layout new-item)
	      ;; remove the ghosts of departed quantities
	      (when (not (plusp (quantity item)))
		(remove-inventory-item container item)
		(destroy item))
	      ;; check
	      (assert (valid-container container)))))))))

(defmethod consume-quantity ((container thing) item-class &optional (quantity 1))
  (dotimes (n quantity)
    (consume-single container item-class)))

(defun quantity-of (item-class new-quantity)
  (assert (plusp new-quantity))
  (let ((item (new item-class)))
    (prog1 item
      (with-fields (quantity) item
	(setf quantity new-quantity)))))

(defmethod search-inventory ((container thing) &optional (class 'thing))
  (labels ((match (item)
	     (if (typep item class)
		 (list item)
		 (search-inventory item class)))) 
    (mapcan #'match (inventory-items container))))

(defmethod compute-quantity ((container thing) item-class)
  (let ((items (search-inventory container item-class)))
    (if items 
	(reduce #'+ items :key #'quantity)
	0)))

(defmethod has-quantity ((container thing) item-class &optional (quantity 1))
  (>= (compute-quantity container item-class)
      quantity))

;;; Equipping things

(defmethod equip ((equipper thing) (self thing))
  (when (and (field-value :container self)
	     ;; can only equip from top level of own inventory
	     (eq equipper (field-value :container self)))
    (setf (field-value :equipper self) equipper)))

(defmethod equipped ((equipment thing))
  (field-value :equipper equipment))

(defmethod unequip ((equipper thing) (self thing))
  (setf (field-value :equipper self) nil))

(defmethod equipment ((self thing))
  (remove-if-not #'(lambda (x)
		     (field-value :equipper x))
		 (inventory-items self)))

(defmethod toggle-equipped ((equipper thing) (self thing))
  (if (equipped self)
      (unequip equipper self)
      (equip equipper self)))
		
(defmethod equipment-description ((self thing)) nil)
   
;;; Derived attack/defense/resistance scores 

(defmethod compute-rating ((self thing) stat)
  (apply #'+ 
	 (field-value stat self) 
	 (mapcar #'(lambda (item)
		     (field-value stat item))
		 (equipment self))))

(defconstant *rating-unit-percentage-points* 10)

(defmethod compute-modifier ((self thing) stat)
 (cfloat
   (/ (+ 100 (* (compute-rating self stat)
		*rating-unit-percentage-points*))
      100)))

(defmethod attack-rating ((self thing))
  (compute-rating self :attack))

(defmethod defense-rating ((self thing))
  (compute-rating self :defense))

(defmethod resistance-rating ((self thing))
  (compute-rating self :resistance))

(defmethod attack-modifier ((self thing))
  (compute-modifier self :attack))

(defmethod defense-modifier ((self thing))
  (- 1 (- (compute-modifier self :defense) 1)))

(defmethod resistance-modifier ((self thing))
  (- 1 (- (compute-modifier self :resistance) 1)))

;;; Vital attributes

(defparameter *maximum-points* 100)

(defmethod modify-points ((self thing) field points)
  (setf (field-value field self)
	(truncate 
	 (max 0
	      (min *maximum-points*
		   (+ points 
		      (field-value field self)))))))

(defmethod modify-health ((self thing) points)
  (modify-points self :health points))

(defmethod damage ((self thing) points)
  (modify-health self (* points (defense-modifier self))))
		 
(defmethod modify-health :after ((self thing) points)
  (when (not (plusp (field-value :health self)))
    (die self)))

(defmethod modify-cold ((self thing) points)
  (modify-points self :cold points))

(defmethod chill ((self thing) points)
  (modify-cold self (* points (resistance-modifier self))))

(defmethod modify-magic ((self thing) points)
  (modify-points self :magic points))

(defmethod modify-hunger ((self thing) points)
  (modify-points self :hunger points))

;;; Reagent management

(defmethod has-condition ((self thing) field points)
  (assert (keywordp field))
  (let ((p (field-value field self)))
    (assert (numberp p))
    (>= p points)))

(defmethod modify-condition ((self thing) field points)
  (assert (keywordp field))
  (incf (field-value field self) points))

(defmethod have-reagents ((self thing) reagents)
  (block checking
    (prog1 t
      (loop while reagents do
	(let* ((name (pop reagents))
	       (value (pop reagents)))
	  (assert (and name value))
	  (or
	   (etypecase name
	     (keyword (has-condition self name value))
	     (symbol (has-quantity self name value)))
	   (return-from checking nil)))))))

(defmethod expend-reagents ((self thing) reagents)
  (loop while reagents do
    (let* ((name (pop reagents))
	   (value (pop reagents)))
      (assert (and name value))
      (etypecase name
	(keyword (modify-condition self name (- value)))
	(symbol
	 (dotimes (n value)
	   (destroy (consume-single self name))))))))

;;; Attaching gumps to things

(defmethod get-gump ((self thing)) 
  (field-value :gump self))

(defmethod set-gump ((self thing) gump)
  (setf (field-value :gump self) 
	(find-object gump))
  ;; cascade
  (multiple-value-bind (x y)
      (gump-cascade-position)
    (drop-object (current-buffer) gump x y)))

(defmethod destroy-gump ((self thing))
  (with-fields (gump) self
    (when gump
      (when (xelfp gump)
	(destroy gump))
      (setf gump nil))))

(defmethod replace-gump ((self thing) gump)
  (with-local-fields
    (if %gump
	(multiple-value-bind (x y) (get-target-position %gump)
	  (destroy-gump self)
	  (set-gump self gump))
	(set-gump self gump))))

(defmethod talk ((self thing) &rest args)
  (replace-gump self (apply #'make-talk-gump self args)))

;;; Reaching objects

(defparameter *reach-distance* 400)

(defmethod can-reach ((target thing) (reacher thing))
  (or (field-value :container target)
      (< (distance-between reacher target)
	 *reach-distance*)))

;;; Dragging objects to move them
    
(defmethod can-pick ((self thing))
  (or (shell-open-p)
      (and
       (not (equipped self))
       (can-reach self (geoffrey))
       (not (fixedp self))
       (not (etherealp self)))))

(defmethod pick ((self thing)) self)

(defmethod fullp (inventory)
  (>= (length inventory) 
      *maximum-inventory-size*))

(defmethod refresh ((self thing)) nil)

(defmethod can-accept ((self thing)) nil)

(defmethod will-accept ((container thing) (item thing))
  (unless (eq container item)
    (when (can-reach container (geoffrey))
      (can-accept container))))

(defmethod accept ((container thing) (item thing))
  (prog1 t
    (with-fields (gump) container
      (destroy-gump item)
      (remove-object (current-buffer) item)
      (add-inventory-item container item))))

(defmethod accept :before ((container thing) (item thing))
  (play-sample "ok.wav"))

(defmethod take ((container thing) (item thing))
  ;; simple for now
  (accept container item))

(defmethod bring-to-front ((self thing))
  (when (xelfp (current-buffer))
    (with-fields (z) self
      (setf z (max (or z 1)
		   (+ 1 (maximum-z-value (current-buffer))))))))

(defmethod send-to-back ((self thing))
  (when (xelfp (current-buffer))
    (setf (field-value :z self) 1)))

;; (defmethod drop-object :after ((buffer buffer) (thing thing) &optional x y z)
;;   (bring-to-front thing))
 
(defmethod finish-drag ((self thing))
  (bring-to-front self)
  (play-sample "ok.wav"))

;;; Describing and naming objects 

(defun fancy-description (thing)
  (pretty-string (class-name (class-of (find-object thing)))))

(defmethod find-description ((self thing)) 
  (or (field-value :description self)
      (fancy-description self)))

(defun find-bubble ()
  (block finding
    (with-fields (objects) (current-buffer)
      (loop for thing being the hash-values in objects
	    do (when (typep (find-object thing t) (find-class 'bubble))
		 (return-from finding (find-object thing t)))))))

(defmethod replace-bubble ((self thing) text &optional anchor)
  (let ((old-bubble (find-bubble)))
    (when old-bubble 
      (destroy old-bubble))
    (multiple-value-bind (x y) (right-of self)
      (drop-object (current-buffer)
		   (new 'bubble :text text :anchor anchor)
		   x y))))

(defmethod look ((self thing))
  (replace-bubble self (find-description self)))

(defresource "talk.wav" :volume 20)
(defresource "talk-1.wav" :volume 20)
(defresource "talk-2.wav" :volume 20)
(defresource "talk-3.wav" :volume 20)
(defresource "npc-talk.wav" :volume 20)

(defmethod play-talk-sound ((self thing))
  (play-sample (random-choose '("talk-1.wav" "talk-2.wav" "talk-3.wav"))))

(defmethod bark ((self thing) string)
  (play-talk-sound self)
  (replace-bubble self string self))

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
	     (activate-maybe self))))))

;;; Double-clicking an object causes it to activate.

(defmethod use ((self thing) (object thing))
  (narrate "Nothing happens."))

(defmethod activate-maybe ((self thing))
  (if (or 
       (field-value :container self)
       (can-reach self (geoffrey)))
      (activate self)
      (prog1 nil 
	(show-error self)
	(narrate "You can't reach that from where you're standing."))))

(defmethod activate ((self thing))
  (use (geoffrey) self))

(defmethod activate :before ((thing thing))
  (destroy-gump thing)
  (play-sample "activate.wav"))

;;; movement-click through, for decals etc

(defmethod alternate-tap ((thing thing) x y)
  (alternate-tap (current-scene) x y))

;;; The system update function does its own work, then invokes the
;;; gameworld's RUN method.

(defmethod add-stasis ((thing thing) seconds)
  (setf (field-value :stasis thing) (seconds->frames seconds)))

(defmethod cancel-stasis ((thing thing))
  (setf (field-value :stasis thing) nil))

(defmethod run ((self thing)))
(defmethod arrange ((self thing)))

(defmethod update ((self thing))
  (with-fields (last-tap-time stasis) self
    ;; we actually catch the end of single-click here.
    (when (and last-tap-time
	       (> (- *updates* last-tap-time)
		  *double-tap-time*))
      (setf last-tap-time nil)
      ;; display object's class name (by default on single click)
      (look self))
    (arrange self)
    ;; handle stasis counter
    (when stasis 
      (decf stasis)
      (when (minusp stasis) 
	(setf stasis nil)))
    ;; possibly run world AI
    (unless (or stasis *paused*)
      ;;(run-tasks self)
      (run self))))

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

;;; Simple temporary tooltip bubble

(defparameter *bubble-font* "oldania-bubble")

(defresource (:name "oldania-bubble" 
	      :type :ttf 
	      :file "OldaniaADFStd-Regular.otf" 
	      :properties (:size 18)))

(defthing bubble
  (tags :initform '(:bubble :ethereal))
  (anchor :initform nil)
  (text :initform nil) 
  (font :initform *bubble-font*)
  (collision-type :initform nil))

(defmethod look ((bubble bubble)))

(defmethod initialize ((self bubble) &key text (font *bubble-font*) anchor)
  (with-local-fields 
    (setf (field-value :anchor self) anchor)
    (setf (field-value :text self) text)
    (setf (field-value :font self) font)
    (later 4.0 (destroy self))))

(defmethod drop-object :after ((buffer buffer) (self bubble) &optional x y z)
  (bring-to-front self))

(defmethod arrange ((self bubble))
  (with-field-values (width height text font anchor) self
    (when anchor
      (resize self  (+ 8 (font-text-width text font))  (+ 8 (font-height font)))
      (multiple-value-bind (x y) (right-of anchor)
	(move-to self 
		 (min x
		      (- (+ (window-x) *nominal-screen-width*)
			 width))
		 (min y 
		      (- (+ (window-y) *nominal-screen-height*)
			 height)))))))

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

;;; User interface audio/visual icons

(defresource "check-button.png")
(defresource "x-button.png")

(defresource "activate.wav" :volume 7)
(defresource "error.wav" :volume 8)
(defresource "ok.wav" :volume 7)
(defresource "close.wav" :volume 8)

;;; Highlighting the object to be dropped upon

(defparameter *indicator-size* 30)

(defmethod draw-hover ((self thing))
  (with-fields (x y) self
    (with-fields (drag) (current-scene)
      (when drag
	(when (will-accept self drag)
	  (draw-image "check-button.png" x y 
		      :height *indicator-size* :width *indicator-size*))))))

;;; Indicating that an action has failed

(defthing error-bubble
  :tags '(:ethereal)
  :image "x-button.png")

(defmethod look ((self error-bubble)) nil)

(defmethod initialize ((self error-bubble) &key)
  (resize self *indicator-size* *indicator-size*)
  (later 2.0 (destroy self)))

(defmethod show-error ((thing thing) &optional x0 y0)
  (with-field-values (x y) thing
    (let ((bubble (new 'error-bubble)))
      (play-sample "error.wav")
      (drop-object (current-buffer) bubble
		   (or x0 x) (or y0 y))
      (bring-to-front bubble))))

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

;;; Serialization

(defconstant +hash+ :%CYPRESS%1%HASH%)
(defconstant +xelf+ :%CYPRESS%1%XELF%)
(defconstant +uuid+ :%CYPRESS%1%UUID%)

(defmethod flatten (object)
  ;; use labels here so we can call #'flatten
  (with-standard-io-syntax
    (labels ((hash-to-list (hash)
	       (let (plist)
		 (labels ((collect (k v)
			    (push (flatten v) plist)
			    (push k plist)))
		   (maphash #'collect hash)
		   (cons +hash+ (cons (hash-table-test hash) plist))))))
      (typecase object 
	(hash-table (hash-to-list object))
	;; handle tree structure
	(cons 
	 (if (consp (cdr object)) ;; it's a list
	     (mapcar #'flatten object)
	     (cons (flatten (car object)) ;; it's a dotted pair
		   (flatten (cdr object)))))
	;; handle strings
	(string
	 (if (xelfp object)
	     (list +uuid+ (coerce (copy-tree object) 'simple-string))
	     (coerce (copy-tree object) 'simple-string)))
	;; pass other vectors
	(vector (map 'vector #'flatten object))
	;; UUID the xelf objects
	(xelf-object (list +uuid+ (xelf::uuid object)))
	;; pass through other Lisp entities
	(otherwise object)))))

(defmethod flatten-and-store (self)
  (let ((excluded-fields (when (has-field :excluded-fields self)
			   (field-value :excluded-fields self))))
    (let ((class-name (class-name (class-of self)))
	  (uuid (xelf::uuid self))
	  (fields (xelf::fields self))
	  (plist nil))
      (assert (and class-name (stringp uuid)))
      ;; just flatten
      (labels ((collect (field value)
		 (unless (member field excluded-fields)
		   (push (flatten value) plist)
		   (push field plist))))
	;; make flattened/cleaned plist 
	(etypecase fields
	  (hash-table (maphash #'collect fields))
	  (list (loop while fields
		      ;; dissect plist
		      do (let ((field (pop fields))
			       (value (pop fields)))
			   (collect field value)))))
	;; cons up the final flattened sexp
	(list +xelf+
	      :class class-name
	      :uuid uuid
	      :fields plist)))))

(defmethod flatten-database (&optional (database xelf::*database*))
  (let (results)
    (labels ((each (uuid object)
	       (push (flatten-and-store object) results)))
      (maphash #'each database))
    results))

(defparameter *quest-variables* '(*travel-direction* *geoffrey*
*lucius* *updates* *events* *current-objective*
*status-line* *previous-scene* *previous-x* *previous-y*
*status-messages* *status-message-time* *last-status-message-time*
*map-screen* *map-row* *map-column* *current-scene*))

(defun flatten-variable (sym)
  (list sym (flatten (symbol-value sym))))

(defmethod flatten-quest ()
  (list
   :variables (mapcar #'flatten-variable *quest-variables*)
   :database (flatten-database)))

(defun cypress-save-file () (xelf::database-file))

(defmethod save-quest ()
  (let ((quest (flatten-quest)))
    (prog1 quest
      (write-sexp-to-file (cypress-save-file) quest))))

(defun unflatten-hash (data test)
    (let ((plist data)
	  (hash (make-hash-table :test test)))
      (prog1 hash
	(loop while plist do
	  (let* ((key (pop plist))
		 (value (pop plist)))
	    (setf (gethash key hash) (unflatten value)))))))

(defun unflatten-fields (fields &optional (type :list) (test 'eq))
  (ecase type
    (:list (mapcar #'unflatten fields))
    (:hash (unflatten-hash fields test))))

(defun unflatten-uuid (sexp)
  (destructuring-bind (key uuid) sexp
    (assert (eq +uuid+ key))
    (gethash uuid xelf::*database*)))

(defun unflatten (data)
  (with-standard-io-syntax 
    (cond 
      ;; replace uuid's with direct references
      ((and (listp data) (eq +uuid+ (first data)))
       (unflatten-uuid data))
      ;; handle hashes
      ((and (listp data) (eq +hash+ (first data)))
       ;; pass hash table test key
       (unflatten-fields (rest (rest data)) :hash (second data)))
      ;; handle lists
      ((consp data)
       (if (consp (cdr data))
	   ;; it's a list
	   (mapcar #'unflatten data)
	   ;; it's a dotted pair
	   (cons (unflatten (car data))
		 (unflatten (cdr data)))))
      ;; passthru
      (t data))))

(defun expand-variable (symbol value)
  (assert (boundp symbol))
  (assert (not (eq '*quest-variables* symbol)))
  (assert (member symbol *quest-variables*))
  (setf (symbol-value symbol) 
	(unflatten value)))

(defun unflatten-object (sexp)
  (destructuring-bind (key &key class uuid fields) sexp
    (assert (eq +xelf+ key))
    (let ((instance (gethash uuid xelf::*database*)))
      (assert (not (null instance)))
      ;; (setf (uuid instance) uuid)
      (setf (xelf::super instance) (xelf::find-prototype class)) 
      (setf (xelf::fields instance) (unflatten-fields fields))
      instance)))
	    
(defun revive (sexp)
  (destructuring-bind (key &key class uuid fields) sexp
    (assert (eq +xelf+ key))
    (setf (gethash uuid xelf::*database*)
	  (make-instance class :uuid uuid))))

(defmethod after-revive ((thing thing)) nil)

(defun expand-quest (quest)
  (destructuring-bind (&key variables database) quest
    (mapc #'revive database)
    (let ((objects (mapcar #'unflatten-object database)))
      (dolist (variable variables)
	(expand-variable (first variable) 
			 (unflatten (second variable))))
      (mapc #'after-revive objects)))) 
	
(defun load-quest (&optional (file (cypress-save-file)))
  (expand-quest (first (read-sexp-from-file file)))
  (switch-to-buffer (current-scene)))

;; (defmethod walk-to :around ((self thing) x1 y1)
;;   (setf (field-value :colliding-objects self)
;; 	(find-colliding-objects self))
;;   (call-next-method))

;; (defmethod will-obstruct :around ((self leafy-tree) (other geoffrey)) 
;;   (let ((result (call-next-method)))
;;     (prog1 result
;;       (message "RESULT: ~S" result))))

;; (defmethod will-obstruct :around ((self thing) (other thing))
;;   ;; ignore objects that were colliding at the start of pathing
;;   (unless (member other (field-value :colliding-objects self) :test 'eq)
;;     (call-next-method)))
