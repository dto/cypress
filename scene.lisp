(in-package :cypress)

(defvar *status-line* nil)
(defun status-line () *status-line*)

(defvar *current-scene* nil)
(defun current-scene () *current-scene*)

(defun lucius-in-party-p ()
  (and (lucius)
       (field-value :leader (lucius))))

(defun switch-to-scene (buffer)
  (exit-scene (geoffrey))
  (xelf::delete-all-textures)
  (setf *current-scene* buffer)
  (switch-to-buffer buffer)
  (add-object (current-scene) (geoffrey))
  (set-cursor (current-scene) (geoffrey))
  (snap-window-to-cursor (current-scene))
  (follow-with-camera (current-scene) (geoffrey))
  (begin-scene buffer)
  (enter-scene (geoffrey)))

(defvar *previous-scene* nil)
(defvar *previous-x* nil)
(defvar *previous-y* nil)

(defun save-excursion ()
  (setf *previous-scene* (current-scene))
  (multiple-value-bind (x y) (at (geoffrey))
    (setf *previous-x* x *previous-y* y)))

(defun restore-excursion ()
  (assert (and *previous-scene* *previous-x* *previous-y*))
  (switch-to-scene *previous-scene*)
  (move-to (geoffrey) *previous-x* *previous-y*)
  (snap-window-to-cursor (current-scene))
  (mark-traversed (current-scene))
  (setf *previous-scene* nil
	*previous-x* nil
	*previous-y* nil))

(defun restore-excursion-maybe ()
  (when (and *previous-scene* *previous-x* *previous-y*)
    (restore-excursion)))

(defun clear-excursion ()
  (setf *previous-y* nil *previous-x* nil *previous-scene* nil))
    
(defparameter *use-music* t)

(define-buffer scene 
  :background-image "stone-road.png"
  :darkness-image nil ;; "darkness.png"
  :darkness-boil 1.0
  :quadtree-depth 8
  :music nil
  :camped nil
  :traversed nil
  :barrier-y nil
  :time :day
  :cold 0
  :generated nil
  :default-events
  '(
    ((:pause) toggle-pause)
    ((:r :control) reset-game)
    ((:space) toggle-pause)
    ;; ((:f5) quicksave)
    ((:p) toggle-pause)
    ((:m) open-map)
    ((:s) open-spellbook)
    ((:i) open-inventory))
  ;;
  :excluded-fields '(:quadtree :click-start :click-start-block :drag-origin :drag-start :drag-offset :focused-block                      :shell :drag :hover :highlight :inputs))

(define-method quicksave scene ()
  (save-quest))
    
(define-method toggle-pause scene ()
  (setf *paused* (if *paused* nil t)))

(defmethod after-revive ((scene scene))
  ;; (unless (field-value :quadtree scene)
  ;;   (install-quadtree scene))
  (bind-any-default-events scene))

(defmethod initialize :after ((scene scene) &key)
  (bind-any-default-events scene))

(defmethod expend-travel-cost ((scene scene))
  (chill (geoffrey) (random-choose '(5 8 9)))
  (modify-hunger (geoffrey) (random-choose '(10 12 14))))

(defmethod add-object :after ((scene scene) (thing thing) &optional x y z)
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

(defmethod unmark-camped ((buffer scene))
  (setf (field-value :camped buffer) nil))

(defmethod camped ((buffer scene)) 
  (field-value :camped buffer))

(defmethod make-terrain ((buffer scene)))

(defmethod starting-x ((buffer scene) direction)
  (with-fields (width) buffer 
     (ecase direction 
       (:here *previous-x*)
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
       (:here *previous-y*)
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

(defmethod mark-traversed ((buffer scene))
  (setf (field-value :traversed buffer) t))

(defmethod unmark-traversed ((buffer scene))
  (setf (field-value :traversed buffer) nil))

(defmethod distance-from-starting-point ((thing thing))
  (multiple-value-bind (x y) (center-point thing)
    (multiple-value-bind (x0 y0) (starting-location (current-scene) *travel-direction*)
      (distance x y x0 y0))))

(defmethod traversed ((buffer scene))
  (or (field-value :traversed buffer)
      (if (null (starting-location (current-scene) *travel-direction*))
	  (mark-traversed buffer)
	  (with-fields (width height) (current-scene)
	    (when (> (distance-from-starting-point (geoffrey))
		     (/ (+ width height) 4))
	      (narrate "You have traversed the current scene, and may now travel.")
	      (mark-traversed buffer))))))
	 
;;; Utilities

(defun find-spell (class)
  (find-inventory-item (find-spellbook) class))

(defmethod open-map ((buffer scene))
  (let ((travel (find-spell 'travel)))
    (when travel (use (geoffrey) travel))))

(defmethod open-spellbook ((buffer scene))
  (resume)
  (activate (find-spellbook)))

(defmethod open-inventory ((buffer scene))
  (activate (geoffrey)))

(defvar *travel-direction* :downright)

(defmethod begin-scene ((buffer scene))
  (with-buffer buffer
    (unmark-traversed buffer)
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
  (traversed self)
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
      (when (or (typep object (find-class 'enemy))
		(typep object (find-class 'black-wizard)))
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

(defun meadow-debris () (spatter '(silverwood stone twig ginseng ginseng stone twig branch branch silverwood)
			       :trim t :count (+ 2 (random 4))))

(defun flowers () (spatter *flowers* :trim t :count (+ 1 (random 4))))
(defun dead-tree () (singleton (new 'dead-tree)))
(defun leafy-tree () (singleton (new 'leafy-tree)))
(defun wood-pile ()
  (spatter '(twig twig branch branch dead-tree)
	 :trim t :count (+ 2 (random 4))))

(defun clearing ()
  (with-border (units (random-choose '(10 15 20)))
    (if (percent-of-time 50 t)
	(or (percent-of-time 50 (meadow-debris)) 
	    (wood-pile))
	(some-trees))))

(defun some-trees ()
   (spray 'leafy-tree :trim nil :count (+ 1 (random 3))))

(defun dead-trees ()
   (spray 'dead-tree :trim nil :count (+ 2 (random 3))))

(defmethod make-terrain ((meadow meadow))
  (with-border (units 12)
    (lined-up-randomly 
     (stacked-up-randomly  (dead-tree) (flowers) (some-trees))
     (stacked-up-randomly (wood-pile) (flowers) (clearing) (meadow-debris))
     (stacked-up-randomly (some-trees) (flowers) (flowers)))))

(defmethod begin-scene :after ((meadow meadow))
  (let ((geoffrey (geoffrey)))
    (later 5.0 (show-movement-hint geoffrey))
    (later 19.0 (show-object-hint geoffrey)))
  (mark-traversed meadow)
  (drop-object meadow (new 'stone-of-remembrance)
	       (units 25)
	       (units 4))
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
     (stacked-up-randomly (flowers) (wood-pile)
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
     (stacked-up-randomly (meadow-debris) (lone-wolf) (dense-trees) (lone-wraith) (some-trees))
     (stacked-up-randomly (some-trees) (wood-pile) (some-trees) (flowers) (dead-trees))
     (stacked-up-randomly (flowers) (if (percent-of-time 50 t)
					(lone-wolf)
					(reagents))
			  (if (percent-of-time 50 t)
			      (lone-wolf)
			      (lone-wraith))
			  (dense-trees)))))

(defmethod begin-scene :after ((forest forest))
  (percent-of-time 8 (prog1 t (cue-music forest (random-choose '("path.ogg" "mountain.ogg" "lutey.ogg" "tumbling.ogg" "dusk.ogg" "believe-me2.ogg" "xolaros3.ogg"))))))

;;; Cold meadow

(defthing (cold-meadow scene)
  :background-image (random-choose *snowy-meadow-images*))

(defmethod make-terrain ((meadow cold-meadow))
  (with-border (units 10)
    (lined-up-randomly 
     (stacked-up-randomly (wood-pile) (pine-trees) (meadow-debris))
     (stacked-up-randomly (meadow-debris) (lone-wraith) (pine-trees))
     (stacked-up-randomly (dead-trees) (or (percent-of-time 80 (lone-wolf))
					   (singleton (new 'black-wolf))) (flowers)))))

(defmethod begin-scene :after ((meadow cold-meadow))
  (percent-of-time 50 (cue-music meadow (random-choose '("lutey.ogg" "passageway.ogg")))))

(defmethod expend-travel-cost ((meadow cold-meadow))
  (modify-hunger (geoffrey) +6)
  (chill (geoffrey) +20))

;;; Ruins and basements

(defparameter *ruined-house-images* (image-set "ruin" 7))

(defthing ruined-house 
  :description "ruined house"
  :scale 2.0
  :tags '(:fixed)
  :image (random-choose *ruined-house-images*))

(defmethod begin-scene :after ((house ruined-house))
  (send-to-back house))

(defparameter *basement-images* (image-set "basement" 2))

(defthing (basement scene)
  :darkness-image "darkness.png"
  :background-image (random-choose *basement-images*))

(defmethod begin-scene :after ((scene basement))
  (mark-traversed scene)
  (resize-to-background-image scene))

(defmethod make-terrain ((scene basement))
  (with-border (units 15)
    (or (percent-of-time 50 (spray '(corpse bone-dust bone-dust cryptghast) :count (+ 2 (random 4))))
	(percent-of-time 40 (singleton (new 'coverstone)))
	(spray 'ruin-wall :trim nil :count (1+ (random 3))))))
	
(defthing stone-stairwell 
  :tags '(:fixed) 
  :scale 0.8
  :image (random-choose *gray-stairwell-images*) 
  :basement nil)

(defmethod activate ((self stone-stairwell))
  (narrate "This stairway has crumbled beyond repair. You can't get in."))
  ;; (with-fields (basement) self
  ;;   (when (null basement)
  ;;     (setf basement (new 'basement)))
  ;;   (save-excursion)
  ;;   (switch-to-scene basement)))

(defparameter *crumbling-stair-images* (image-set "crumbling-stair" 2))

(defthing crumbling-stairwell 
  :tags '(:fixed) 
  :scale 2
  :image (random-choose *crumbling-stair-images*))

(defmethod activate ((self crumbling-stairwell))
  (if *previous-scene*
      (restore-excursion-maybe)
      (switch-to-map)))

(defthing (ruins scene)
  :background-image (random-choose '("forgotten-meadow.png" "paynes-meadow.png")))

(defmethod make-terrain ((ruins ruins))
  (with-border (units 10)
    (lined-up-randomly 
     (stacked-up-randomly (wood-pile) (some-trees) (singleton (new 'ruined-house)) (lone-wolf) (meadow-debris))
     (stacked-up-randomly (some-trees) (spray '(ruin-wall ruined-house dead-tree ruin-wall berry-bush thornweed) :count (+ 7 (random 5)))
			  (singleton (new 'stone-stairwell)))
     (stacked-up-randomly (dead-trees) (with-border (units 10) (singleton (new 'ruined-house))) (spray '(wraith wolf) :count 2) (flowers)))))

(defmethod expend-travel-cost ((ruins ruins))
  (modify-hunger (geoffrey) 14)
  (chill (geoffrey) +27))

;;; Frozen forest

(defthing (frozen-forest scene)
  :background-image (random-choose *frozen-meadow-images*))

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
    (stacked-up-randomly 
     (lined-up-randomly (rock-outcropping) (dead-trees-and-puddles) (pine-trees) (meadow-debris) (pine-trees))
     (lined-up-randomly (pine-trees) (or (percent-of-time 60 (lone-wraith))
					 (singleton (new 'black-wolf))) (pine-trees))
     (lined-up-randomly (dead-trees-and-puddles) (pack-of-wolves) (wood-pile)))))

(defmethod expend-travel-cost ((forest frozen-forest))
  (modify-hunger (geoffrey) 15)
  (chill (geoffrey) +50))

;; dense pine trees and some dead trees
;; wood piles and twigs and branches
;; nightshade and bushes
;; wraiths, skeleton archers
;; ginseng

(defthing (river scene)
  :background-image (random-choose *frozen-meadow-images*))

(defun treacherous-trees ()
  (spray '(dead-tree large-crack dead-tree nightshade crack crack large-crack puddle)
	 :trim nil
	 :count 14))

(defmethod make-terrain ((river river))
  (with-border (units 10)
    (lined-up-randomly 
     (stacked-up-randomly (wood-pile) (treacherous-trees))
     (stacked-up-randomly (pine-trees) (lone-wraith) (pine-trees))
     (stacked-up-randomly (treacherous-trees) (lone-wraith) (wood-pile)))))

(defmethod expend-travel-cost ((river river))  
  (modify-hunger (geoffrey) 14)
  (chill (geoffrey) +50))

(defthing (frozen-meadow river)
  :background-image (random-choose *frozen-meadow-images*))

(defmethod expend-travel-cost ((frozen-meadow frozen-meadow))  
  (modify-hunger (geoffrey) 25)
  (chill (geoffrey) +80))

;; pools of water
;; various ice cracks
;; twigs and branches
;; skeleton archers

(defthing (highway frozen-forest))

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
