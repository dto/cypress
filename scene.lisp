(in-package :cypress)

;;; Scene

(defvar *status-line* nil)

(defun status-line () *status-line*)

(defvar *current-scene* nil)

(defun current-scene () *current-scene*)

(defun switch-to-scene (buffer &optional previous-x previous-y)
  ;; (play-music (random-choose *soundtrack*) :loop nil)
  (stop-walking (geoffrey))
  (switch-to-buffer buffer)
  (xelf::delete-all-textures)
  (set-cursor buffer (geoffrey))
  (when (and (numberp previous-x) (numberp previous-y))
    (move-to (geoffrey) previous-x previous-y))
  (snap-window-to-cursor buffer)
  (follow-with-camera buffer (geoffrey))
  (setf *current-scene* buffer)
  (begin-scene buffer))

(defparameter *use-music* t)

(define-buffer scene 
  :background-image "stone-road.png"
  :darkness-image nil ;; "darkness.png"
  :darkness-boil 1.0
  :quadtree-depth 8
  :music nil
  :camped nil
  :time :day
  :cold 0
  :generated nil
  :default-events
  '(
    ;; ((:pause) transport-toggle-play)
    ((:r :control) reset-game)
    ;; ((:space) transport-toggle-play)
    ;; ((:p) transport-toggle-play)
    ((:m) open-map)
    ((:s) open-spellbook)
    ((:i) open-inventory)))

(defmethod drop-object :after ((scene scene) (thing thing) &optional x y z)
  (enter-scene thing))

(defmethod map-icon ((scene scene))
  (terrain-icon (class-name (class-of scene))))

(defmethod find-description ((scene scene))
  (pretty-string (class-name (class-of scene))))

(defmethod can-be-visited ((scene scene)) t)

(defthing (mountain-pass scene))
(defmethod can-be-visited ((scene mountain-pass)) nil)

(defthing (large-mountain scene))
(defmethod can-be-visited ((scene large-mountain)) nil)

(defthing (valisade scene))
(defmethod can-be-visited ((scene valisade)) nil)

(defmethod cue-music ((scene scene) music)
  (when *use-music*
    (play-music music)))

(defmethod drag-fail ((scene scene) (object thing) x y)
  (when (and (not (typep object 'enemy))
	     (not (can-reach object (geoffrey))))
    (show-error (geoffrey) x y)
    (narrate "You can't reach that from where you're standing.")))

(defmethod begin-region ((buffer scene)))

(defmethod mark-camped ((buffer scene))
  (setf (field-value :camped buffer) t))

(defmethod camped ((buffer scene)) 
  (field-value :camped buffer))

(defmethod make-terrain ((buffer scene)))

(defmethod starting-x ((buffer scene) direction)
  (with-fields (width) buffer 
     (ecase direction
       (:downright (units 5))
       (:downleft (- width (units 8)))
       (:down (/ width 2))
       (:right (units 8))
       (:upright (units 8))
       (:up (/ width 2))
       (:upleft (- width (units 8)))
       (:left (- width (units 8))))))

(defmethod starting-y ((buffer scene) direction)
  (with-fields (height) buffer 
     (ecase direction
       (:downright (units 5))
       (:downleft (units 5))
       (:down (units 5))
       (:right (/ height 2))
       (:upright (- height (units 8)))
       (:up (- height (units 8)))
       (:upleft (- height (units 8)))
       (:left (/ height 2)))))

(defmethod starting-location ((buffer scene) &optional (direction :downright))
  (values (starting-x buffer direction)
	  (starting-y buffer direction)))
  ;; (values (units 5) (/ (field-value :height buffer) 2)))

(defun find-spell (class)
  (find-inventory-item (find-spellbook) class))

(defmethod open-map ((buffer scene))
  (let ((travel (find-spell 'travel)))
    (when travel (use (geoffrey) travel))))

(defmethod open-spellbook ((buffer scene))
  (activate (find-spellbook)))

(defmethod open-inventory ((buffer scene))
  (activate (geoffrey)))

(defvar *travel-direction* :downright)

(defmethod begin-scene ((buffer scene))
  (with-buffer buffer
    (setf *status-line*
	  (find-object (new 'status-line)))
    (with-fields (generated) buffer
      (when (not generated)
	(setf generated t)
	(let ((terrain (make-terrain buffer)))
	  (when terrain
	    (paste-from buffer terrain)
	    (destroy terrain)
	    (resize buffer (field-value :width terrain) (field-value :height terrain))
	    (unless (field-value :quadtree buffer)
	      (install-quadtree buffer))))))
    ;; adjust scrolling parameters 
    (setf (%window-scrolling-speed buffer) (cfloat (/ *monk-speed* 2.8))
	  (%horizontal-scrolling-margin buffer) 3/5
	  (%vertical-scrolling-margin buffer) 4/7)
    ;;
    (multiple-value-bind (x y) (starting-location buffer *travel-direction*)
      (drop-object buffer (geoffrey) x y))
    (set-cursor buffer (geoffrey))
    (snap-window-to-cursor buffer)
    (follow-with-camera buffer (geoffrey))
    ;; drop player at start point
    ;; return buffer
    buffer))

(defmethod alternate-tap ((buffer scene) x y)
  (when (xelfp (geoffrey))
    (multiple-value-bind (top left right bottom)
	(bounding-box (geoffrey))
      ;; walk the monk's center point to the destination point
      (let ((height (- bottom top))
	    (width (- right left)))
	(walk-to (geoffrey)
		 (- x (/ width 2))
		 (- y (/ height 2)))))))

(defmethod scroll-tap ((buffer scene) x y)
  nil)

(defmethod draw-object-layer ((buffer scene))
  (multiple-value-bind (top left right bottom) 
      (window-bounding-box buffer)
    (dolist (object 
	     ;; don't draw gumps yet
	     (remove-if #'(lambda (x)
			    (typep x 'gump))
			(mapcar #'find-object (z-sort (get-objects buffer)))))
      ;; only draw onscreen objects
      (when (colliding-with-bounding-box object top left right bottom)
	(draw object)
	(after-draw-object buffer object)))))

(defparameter *darkness-scale* 1.0)

(defun random-boil-factor () (random-choose '(1.0 1.005 1.01 1.015 1.02)))

(defmethod draw-object-layer :after ((buffer scene))
  (with-fields (darkness-image darkness-boil) buffer
    (when darkness-image
      (percent-of-time 16 (setf darkness-boil (random-boil-factor)))
      (let ((height (* darkness-boil *darkness-scale* (image-height darkness-image)))
	    (width (* darkness-boil *darkness-scale* (image-width darkness-image))))
	(multiple-value-bind (x y) (center-point (cursor))
	  (draw-image darkness-image
		    (- x (/ width 2))
		    (- y (/ height 2))
		    :height height
		    :width width
		    :blend :multiply))))))

(defmethod drag-candidate ((scene scene) (drag thing) x y)
  (let ((objects (z-sorted-objects scene)))
    (block searching
      (dolist (object objects)
	(when (and (colliding-with drag object)
		   (not (eq drag object)))
	  (return-from searching object))))))

(defmethod update :after ((self scene))
  (when (xelfp (geoffrey))
    (layout (status-line))
    (update (status-line))))

(defmethod draw :after ((self scene))
  ;; draw gumps and bubbles
  (mapc #'draw (z-sort 
		(append (find-gumps self)
			(find-instances self 'bubble))))
  ;; draw any drags
  (with-fields (drag hover) self
    (when (xelfp drag) (draw drag))
    (when (xelfp hover) (draw-hover (find-object hover))))
  ;; draw status line
  (when (xelfp (geoffrey))
    (draw (status-line))))

(define-method reset-game scene ()
  (let ((buffer (current-buffer)))
    (at-next-update 
      (switch-to-scene (make-quest))
      (destroy buffer))))

(defun find-camp ()
  (dolist (object (get-objects (current-buffer)))
    (when (typep object (find-class 'camp))
      (return object))))

(defun find-enemies ()
  (let (enemies)
    (dolist (object (get-objects (current-buffer)))
      (when (typep object (find-class 'enemy))
	(push object enemies)))
    enemies))

(defparameter *enemy-near-distance* 800)

(defmethod near-geoffrey ((self thing))
  (< (distance-between self (geoffrey))
     *enemy-near-distance*))

(defun nearby-enemies-p ()
  (some #'near-geoffrey (find-enemies)))

;;; Camera control

(defmethod glide-follow ((self scene) object)
  (with-local-fields 
    (with-fields (window-x window-y width height) self
      (let ((margin-x (* %horizontal-scrolling-margin *gl-screen-width*))
	    (margin-y (* %vertical-scrolling-margin *gl-screen-height*)))
	(multiple-value-bind (object-x object-y)
	    (center-point object)
	  (multiple-value-bind (tx ty) 
	      (step-toward-heading object 
				   (field-value :heading object) 
				   (/ *monk-speed* 1.2))
		;; yes. recenter.
		(glide-window-to self
				 (truncate (max 0
						(min (- width *gl-screen-width*)
						     (- tx
							(truncate (/ *gl-screen-width* 2))))))
				 (truncate (max 0 
				      (min (- height *gl-screen-height*)
					   (- ty 
					      (truncate (/ *gl-screen-height* 2)))))))))))))

(defmethod update-window-glide ((self scene))
  (with-fields (window-x window-x0 window-y window-y0 window-scrolling-speed) self
    (labels ((nearby (a b)
	       (> window-scrolling-speed (abs (- a b))))
	     (jump (a b)
	       (if (< a b) window-scrolling-speed (- window-scrolling-speed))))
      (when (and window-x0 window-y0)
	(if (nearby window-x window-x0)
	    (setf window-x0 nil)
	    (incf window-x (jump window-x window-x0)))
	(if (nearby window-y window-y0)
	    (setf window-y0 nil)
	    (incf window-y (jump window-y window-y0)))))))

;;; Large background textures

(defparameter *grassy-meadow-images* '("golden-meadow.png" "stone-road.png" "meadow.png"))
(defparameter *snowy-meadow-images* '("cloudy-meadow.png" "paynes-meadow.png" "purple-meadow.png" "sky-meadow.png" "forgotten-meadow.png"))
(defparameter *frozen-meadow-images* (image-set "frozen-meadow" 3))

;;; Meadow

(defun reagents ()
  (spatter (random-choose *reagents*)
	 :count (+ 1 (random 5))
	 :trim t))

(defun enemy ()
  (singleton (new (random-choose '(wraith wraith wolf)))))

(defun lone-wolf ()
  (singleton (new 'wolf)))

(defun lone-wraith ()
  (singleton (new 'wraith)))

(defun pack-of-wolves ()
  (spray 'wolf :trim t :count 2))

(defparameter *flowers* '(violet forget-me-not snowdrop))
(defparameter *reagents* '(ginseng ginseng silverwood stone branch))

(defthing (meadow scene)
  :background-image (random-choose '("stone-road.png" "meadow.png")))

(defun meadow-debris () (spatter '(stone twig branch branch ginseng silverwood)
			       :trim t :count (+ 2 (random 4))))

(defun flowers () (spatter *flowers* :trim t :count (+ 1 (random 4))))
(defun dead-tree () (singleton (new 'dead-tree)))
(defun leafy-tree () (singleton (new 'leafy-tree)))
(defun wood-pile ()
  (spatter '(twig twig branch dead-tree)
	 :trim t :count (+ 2 (random 2))))

(defun clearing ()
  (with-border (units (random-choose '(10 15 20)))
    (if (percent-of-time 50 t)
	(meadow-debris)
	(some-trees))))

(defun some-trees ()
   (spray 'leafy-tree :trim nil :count (+ 1 (random 3))))

(defun dead-trees ()
   (spray 'dead-tree :trim nil :count (+ 2 (random 3))))

(defmethod make-terrain ((meadow meadow))
  (with-border (units 12)
    (lined-up-randomly 
     (stacked-up-randomly (dead-tree) (flowers) (some-trees))
     (stacked-up-randomly (wood-pile) (flowers) (clearing) (meadow-debris))
     (stacked-up-randomly (some-trees) (flowers) (flowers)))))

(defmethod begin-scene :after ((meadow meadow))
  (drop-object meadow (new 'lucius) 
	       (- (field-value :width meadow)
		  (units 8))
	       (- (field-value :height meadow)
		  (units 8))))

;;; Grassy meadow

(defthing (grassy-meadow scene)
  :background-image (random-choose *grassy-meadow-images*))

(defmethod make-terrain ((meadow grassy-meadow))
  (with-border (units 10)
    (lined-up-randomly 
     (stacked-up-randomly (meadow-debris) (flowers) (some-trees))
     (stacked-up-randomly (clearing) (pine-trees) (clearing))
     (stacked-up-randomly (flowers)
			  (lone-wolf)
			  (spatter '(silverwood stone) :count (+ 2 (random 3)))))))

;;; Forest

(defun dense-trees ()
  (randomly
   (spray '(leafy-tree leafy-tree pine-tree leafy-tree dead-tree)
	  :trim t :count (+ 4 (random 7)))
   (spatter '(silverwood thornweed) :count (+ 2 (random 3)))))

(defun tree-clearing ()
  (with-border (units (random-choose '(5 10 12)))
    (dense-trees)))

(defthing (forest scene)
  :background-image (random-choose '("stone-road.png" "meadow.png")))

(defmethod make-terrain ((forest forest))
  (with-border (units 10)
    (lined-up-randomly 
     (stacked-up-randomly (meadow-debris) (dense-trees) (lone-wraith) (some-trees))
     (stacked-up-randomly (some-trees) (some-trees) (flowers) (dead-trees))
     (stacked-up-randomly (flowers) (if (percent-of-time 50 t)
					(lone-wolf)
					(lone-wraith))
			  (dense-trees)))))

;;; Cold meadow

(defthing (cold-meadow scene)
  :background-image (random-choose *snowy-meadow-images*))

(defmethod make-terrain ((meadow cold-meadow))
  (with-border (units 10)
    (lined-up-randomly 
     (stacked-up-randomly (wood-pile) (pine-trees) (meadow-debris))
     (stacked-up-randomly (meadow-debris) (lone-wraith) (pine-trees))
     (stacked-up-randomly (dead-trees) (lone-wolf) (flowers)))))

(defmethod begin-scene :after ((meadow cold-meadow))
  (percent-of-time 50 (cue-music meadow (random-choose '("rain.ogg" "passageway.ogg")))))

(defmethod drop-object :after ((meadow cold-meadow) (monk geoffrey) &optional x y z)
  (chill monk +12))

;;; Ruins and basements

(defparameter *ruined-house-images* (image-set "ruin" 7))

(defthing ruined-house 
  :description "ruined house"
  :scale 2.0
  :tags '(:fixed)
  :image (random-choose *ruined-house-images*))

(defparameter *basement-images* (image-set "basement" 2))

(defthing (basement scene)
  :darkness-image "darkness.png"
  :background-image (random-choose *basement-images*))

(defmethod initialize :after ((scene basement) &key)
  (resize-to-background-image scene))

(defmethod make-terrain ((scene basement))
  (with-border (units 15)
    (or (percent-of-time 50 (spray '(corpse bone-dust bone-dust ) :count (+ 2 (random 4))))
	(percent-of-time 40 (singleton (new 'coverstone)))
	(spray 'ruin-wall :trim nil :count (1+ (random 3))))))
	
(defvar *previous-scene* nil)
(defvar *previous-x* nil)
(defvar *previous-y* nil)

(defthing stone-stairwell 
  :tags '(:fixed) 
  :scale 0.8
  :image (random-choose *gray-stairwell-images*) 
  :basement nil)

(defmethod activate ((self stone-stairwell))
  (narrate "You descend the stairs and enter a moldering basement.")
  (with-fields (basement) self
    (when (null basement)
      (setf basement (new 'basement)))
    (setf *previous-scene* (current-scene))
    (multiple-value-bind (x y) (at (cursor))
      (setf *previous-x* x *previous-y* y)
      (switch-to-scene basement))))

(defparameter *crumbling-stair-images* (image-set "crumbling-stair" 2))

(defthing crumbling-stairwell 
  :tags '(:fixed) 
  :image-scale 200
  :image (random-choose *crumbling-stair-images*))

(defmethod activate ((self crumbling-stairwell))
  (switch-to-scene *previous-scene* *previous-x* *previous-y*)
  (setf *previous-scene* nil))

(defthing (ruins scene)
  :background-image (random-choose '("forgotten-meadow.png" "paynes-meadow.png")))

(defmethod make-terrain ((ruins ruins))
  (with-border (units 10)
    (lined-up-randomly 
     (stacked-up-randomly (wood-pile) (dense-trees) (lone-wolf) (meadow-debris))
     (stacked-up-randomly (some-trees) (spray '(ruin-wall ruined-house dead-tree ruin-wall berry-bush thornweed) :count (+ 7 (random 5)))
			  (singleton (new 'stone-stairwell)))
     (stacked-up-randomly (dead-trees) (with-border (units 10) (singleton (new 'ruined-house))) (spray '(wraith wolf) :count 2) (flowers)))))

(defmethod drop-object :after ((ruins ruins) (monk geoffrey) &optional x y z)
  (chill monk +16))

;;; Cemetery

(defthing (cemetery scene)
  :background-image (random-choose '("forgotten-meadow.png" "paynes-meadow.png")))

(defun row-of-graves ()
  (with-border (units (+ 2 (random 3)))
    (apply #'lined-up-randomly (mapcar #'singleton (grab '(gravestone) (+ 2 (random 4)))))))

(defun some-graves ()
  (let (rows)
    (dotimes (n (+ 2 (random 2)))
      (push (row-of-graves) rows))
    (with-border (units 4) 
      (apply #'stacked-up-randomly rows))))

(defmethod make-terrain ((cemetery cemetery))
  (with-border (units 10)
    (lined-up-randomly 
     (stacked-up-randomly (wood-pile) (spatter 'bone-dust) (spray 'iron-fence :count (+ 2 (random 3))) (some-graves) (dense-trees) (singleton (new 'grave-hag)) (spatter '(nightshade ginseng)))
     (stacked-up-randomly (dead-trees) (spray 'iron-fence :count (+ 2 (random 3)))
			  ;; (singleton (new 'stone-stairwell)) 
			  (some-trees))
     (stacked-up-randomly (dead-trees) (spray 'iron-fence :count (+ 2 (random 3))) (some-graves) (spray 'iron-fence :count (+ 2 (random 3))) (singleton (new 'grave-hag)) (spray 'bone-dust) (singleton (new 'iron-fence)) (flowers)))))

(defmethod drop-object :after ((cemetery cemetery) (monk geoffrey) &optional x y z)
  (chill monk +15))

;;; Hidden cemetery

(defthing (hidden-cemetery scene)
  :background-image "stone-road.png")

(defmethod map-icon ((self hidden-cemetery)) (random-choose *forest-icons*))

(defmethod find-description ((self hidden-cemetery)) "forest")

(defun small-fence ()
  (with-border (units 3)
    (apply #'stacked-up (mapcar #'singleton
				(list 
				(new 'iron-fence)
				(new 'iron-fence)
				(new 'iron-fence)
				(new 'iron-fence)
				(new 'iron-fence))))))
(defun small-cemetery ()
  (lined-up (small-fence)
	    (stacked-up 
	     (flowers)
	     (some-graves)
	     (flowers))
	    (small-fence)))

(defmethod make-terrain ((self hidden-cemetery))
  (with-border (units 12)
    (stacked-up 
     (lined-up (some-trees) (some-trees) (some-trees))
     (lined-up (flowers) (small-cemetery) (flowers))
     (lined-up (some-trees) (some-trees) (some-trees)))))

;;; Frozen forest

(defthing (frozen-forest scene)
  :backgrounnd-image (random-choose *frozen-meadow-images*))

(defun pine-trees ()
  (randomly 
   (spatter '(silverwood snowdrop thornweed) :trim t :count (+ 2 (random 2)))
   (spray 'pine-tree
	  :trim nil
	  :count (random-choose '(3 4 4 5)))))

(defun dead-trees-and-puddles ()
  (spray '(dead-tree dead-tree puddle puddle silverwood) :trim nil :count (random-choose '(3 5 6))))

(defmethod make-terrain ((forest frozen-forest))
  (with-border (units 10)
    (lined-up-randomly 
     (stacked-up-randomly (rock-outcropping) (pine-trees) (pine-trees))
     (stacked-up-randomly (pine-trees) (lone-wraith) (pine-trees))
     (stacked-up-randomly (dead-trees-and-puddles) (pack-of-wolves) (wood-pile)))))

(defmethod drop-object :after ((forest frozen-forest) (monk geoffrey) &optional x y z)
  (chill monk +20))

;; dense pine trees and some dead trees
;; wood piles and twigs and branches
;; nightshade and bushes
;; wraiths, skeleton archers
;; ginseng

(defthing (river scene)
  :background-image (random-choose *frozen-meadow-images*))

(defun treacherous-trees ()
  (spray '(dead-tree dead-tree nightshade crack crack large-crack puddle)
	 :trim nil
	 :count 14))

(defmethod make-terrain ((river river))
  (with-border (units 10)
    (lined-up-randomly 
     (stacked-up-randomly (wood-pile) (treacherous-trees))
     (stacked-up-randomly (pine-trees) (lone-wraith) (pine-trees))
     (stacked-up-randomly (dead-trees-and-puddles) (lone-wraith) (wood-pile)))))

(defmethod drop-object :after ((river river) (monk geoffrey) &optional x y z)
  (chill monk +10))

(defthing (frozen-meadow river)
  :background-image (random-choose *frozen-meadow-images*))


;; pools of water
;; various ice cracks
;; twigs and branches
;; skeleton archers

(defthing (highway scene)
  :background-image (random-choose '("stone-road.png" "golden-meadow.png")))
;; generated roadway
;; stone chips
;; twigs
;; a few leafy trees
;; item boxes/ruins
;; wolves

(defun random-terrain ()
  (new 'map-screen))
  ;; (new (random-choose '(forest frozen-forest meadow grassy-meadow
  ;; 			cold-meadow frozen-meadow river ruins cemetery highway))))
