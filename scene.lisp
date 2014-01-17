(in-package :cypress)

;;; Scene

(defvar *status-line* nil)

(defun status-line () *status-line*)

(defvar *current-scene* nil)

(defun current-scene () *current-scene*)

(defun switch-to-scene (buffer)
;;  (play-music (random-choose *soundtrack*) :loop t)
  (setf *current-scene* buffer)
  (switch-to-buffer buffer))

(define-buffer scene 
  :background-image "stone-road.png"
  :quadtree-depth 8
  :camped nil
  :time :day
  :cold 0
  :default-events
  '(((:pause) transport-toggle-play)
    ((:r :control) reset-game)
    ((:space) transport-toggle-play)
    ((:p) transport-toggle-play)
    ((:m) open-map)
    ((:s) open-spellbook)
    ((:i) open-inventory)))

(defmethod begin-region ((buffer scene)))

(defmethod mark-camped ((buffer scene))
  (setf (field-value :camped buffer) t))

(defmethod camped ((buffer scene)) 
  (field-value :camped buffer))

(defmethod make-terrain ((buffer scene)))

(defmethod starting-location ((buffer scene))
  (values (units 5) (units 5)))
  ;; (values (units 5) (/ (field-value :height buffer) 2)))

(defun find-spell (class)
  (find-inventory-item (find-spellbook) class))

(defmethod open-map ((buffer scene))
  (let ((travel (find-spell 'travel)))
    (when travel (cast (geoffrey) travel))))

(defmethod open-spellbook ((buffer scene))
  (activate (find-spellbook)))

(defmethod open-inventory ((buffer scene))
  (activate (geoffrey)))

(defmethod initialize :after ((buffer scene) &key (player (geoffrey)))
  (with-buffer buffer
    (setf *status-line*
	  (find-object (new 'status-line)))
    (let ((terrain (make-terrain buffer)))
      (when terrain
	(paste-from buffer terrain)
	(resize buffer (field-value :width terrain) (field-value :height terrain))))
    ;; adjust scrolling parameters 
    (setf (%window-scrolling-speed buffer) (cfloat (/ *monk-speed* 3))
	  (%horizontal-scrolling-margin buffer) 3/5
	  (%vertical-scrolling-margin buffer) 4/7)
    ;;
    (set-cursor buffer (geoffrey))
    ;;  (glide-window-to-cursor buffer)
    (follow-with-camera buffer (geoffrey))
    ;;  (snap-window-to-cursor buffer)
    ;; allocate
    (install-quadtree buffer)
    ;; drop player at start point
    (when player
      (multiple-value-bind (x y) (starting-location buffer)
	(drop-object buffer player x y)))
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
    (dolist (object (mapcar #'find-object (z-sort (get-objects buffer))))
      ;; only draw onscreen objects
      (when (colliding-with-bounding-box object top left right bottom)
	(draw object)
	(after-draw-object buffer object)))))

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
  (with-fields (drag hover) self
    (when drag (draw drag))
    (when hover (draw-hover (find-object hover))))
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

;;; Various scenes

(defparameter *grassy-meadow-images* '("golden-meadow.png" "stone-road.png" "meadow.png"))
(defparameter *snowy-meadow-images* '("cloudy-meadow.png" "paynes-meadow.png" "purple-meadow.png" "sky-meadow.png" "forgotten-meadow.png"))
(defparameter *frozen-meadow-images* (image-set "frozen-meadow" 3))

;;; Enemies in scenes

(defun lone-wolf ()
  (singleton (new 'wolf)))

(defun pack-of-wolves ()
  (with-border (units 5) 
    (randomly (lone-wolf) (lone-wolf))))

(defun lone-wraith () ()
  (singleton (new 'wraith)))

;;; Meadow

(defparameter *flowers* '(violet forget-me-not snowdrop))
(defparameter *reagents* '(ginseng ginseng silverwood stone branch))

(defthing (meadow scene)
  :background-image (random-choose '("stone-road.png" "meadow.png")))

(defun meadow-debris () (spray '(stone twig branch branch ginseng silverwood)
			       :trim t :count (+ 2 (random 4))))

(defun flowers () (spray *flowers* :trim t :count (+ 1 (random 4))))
(defun dead-tree () (singleton (new 'dead-tree)))
(defun leafy-tree () (singleton (new 'leafy-tree)))
(defun wood-pile ()
  (spray '(twig twig branch dead-tree)
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
  (with-border (units 10)
    (lined-up-randomly 
     (stacked-up-randomly (meadow-debris) (some-trees))
     (stacked-up-randomly (wood-pile) (clearing) (meadow-debris))
     (stacked-up-randomly (some-trees) (flowers)))))

;;; Grassy meadow

(defthing (grassy-meadow scene)
  :background-image (random-choose *grassy-meadow-images*))

(defun ginseng-garden ()
  (spray '(dead-tree ruin-wall ginseng) :trim nil :count (+ 2 (random 3))))

(defmethod make-terrain ((meadow grassy-meadow))
  (with-border (units 10)
    (lined-up-randomly 
     (stacked-up-randomly (meadow-debris) (flowers) (some-trees))
     (stacked-up-randomly (clearing) (ginseng-garden) (clearing))
     (stacked-up-randomly (flowers)
			  (lone-wolf)
			  (spray '(silverwood stone) :count (+ 2 (random 3)))))))

;;; Forest

(defun dense-trees ()
  (randomly
   (spray '(leafy-tree leafy-tree leafy-tree leafy-tree dead-tree)
	  :trim t :count (+ 4 (random 7)))
   (spray '(silverwood thornweed) :count (+ 2 (random 3)))))

(defun tree-clearing ()
  (with-border (units (random-choose '(5 10 12)))
    (dense-trees)))

(defthing (forest scene)
  :background-image (random-choose '("stone-road.png" "meadow.png")))

(defmethod make-terrain ((forest forest))
  (with-border (units 10)
    (lined-up-randomly 
     (stacked-up-randomly (meadow-debris) (dense-trees) (lone-wraith) (some-trees))
     (stacked-up-randomly (clearing) (tree-clearing) (clearing))
     (stacked-up-randomly (flowers) (if (percent-of-time 50 t)
					(pack-of-wolves)
					(lone-wraith))
			  (dense-trees)))))


;;; Cold meadow

(defthing (cold-meadow scene)
  :background-image (random-choose *snowy-meadow-images*))

(defmethod make-terrain ((meadow cold-meadow))
  (with-border (units 10)
    (lined-up-randomly 
     (stacked-up-randomly (wood-pile) (ginseng-garden) (meadow-debris))
     (stacked-up-randomly (meadow-debris) (lone-wraith) (some-trees))
     (stacked-up-randomly (dead-trees) (lone-wolf) (flowers)))))

;;; Ruins

(defthing (ruins scene)
  :background-image (random-choose '("forgotten-meadow.png" "paynes-meadow.png")))

(defmethod make-terrain ((ruins ruins))
  (with-border (units 10)
    (lined-up-randomly 
     (stacked-up-randomly (wood-pile) (dense-trees) (lone-wolf) (meadow-debris))
     (stacked-up-randomly (some-trees) (spray '(ruin-wall ruin-wall berry-bush thornweed) :count (+ 7 (random 5)))
			  (singleton (new 'stone-stairwell)))
     (stacked-up-randomly (dead-trees) (spray '(ancient-road) :trim t :count 8) (spray '(wraith wolf) :count 2) (flowers)))))

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
     (stacked-up-randomly (wood-pile) (spray 'bone-dust) (spray 'iron-fence :count (+ 2 (random 3))) (some-graves) (dense-trees) (singleton (new 'grave-hag)) (spray '(nightshade ginseng)))
     (stacked-up-randomly (dead-trees) (spray 'iron-fence :count (+ 2 (random 3)))
			  (singleton (new 'stone-stairwell)) 
			  (some-trees))
     (stacked-up-randomly (dead-trees) (spray 'iron-fence :count (+ 2 (random 3))) (some-graves) (spray 'iron-fence :count (+ 2 (random 3))) (singleton (new 'grave-hag)) (spray 'bone-dust) (singleton (new 'iron-fence)) (flowers)))))

;;; Frozen forest

(defthing (frozen-forest scene)
  :background-image (random-choose *frozen-meadow-images*))

(defun pine-trees ()
  (randomly 
   (spray '(silverwood snowdrop thornweed) :trim t :count (+ 2 (random 2)))
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
     (stacked-up-randomly (dead-trees-and-puddles) (lone-wolf) (wood-pile)))))

;; dense pine trees and some dead trees
;; wood piles and twigs and branches
;; nightshade and bushes
;; wraiths, skeleton archers
;; ginseng

(defthing (frozen-meadow scene)
  :background-image (random-choose *frozen-meadow-images*))

;; a few pine trees
;; wet pools and icy areas
;; ginseng
;; twigs, branches
;; black wolves
;; wood piles
;; dead trees

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
