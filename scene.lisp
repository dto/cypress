(in-package :cypress)

;;; Scene

(define-buffer scene 
  :status-line nil
  :background-image "stone-road.png"
  :quadtree-depth 8
  :time :day
  :cold 0
  :default-events
  '(((:pause) :transport-toggle-play)
    ((:r :control) :reset-game)
    ((:space) :transport-toggle-play)
    ((:p :control) :transport-toggle-play)))

(defun status-line () (field-value :status-line (current-buffer)))

(defmethod make-terrain ((buffer scene)))

(defmethod starting-location ((buffer scene))
  (values (units 5) (/ (field-value :height buffer) 2)))

(defmethod initialize :after ((buffer scene) &key (player (geoffrey)))
  (setf (field-value :status-line buffer) 
	(find-object (new 'status-line)))
  (let ((terrain (make-terrain buffer)))
    (when terrain
      (paste-from buffer terrain))
    (when player
      (multiple-value-bind (x y) (starting-location buffer)
	(drop-object buffer player x y)))))

(defmethod alternate-tap ((buffer scene) x y)
  (when (xelfp (cursor))
    (multiple-value-bind (top left right bottom)
	(bounding-box (cursor))
      ;; walk the monk's center point to the destination point
      (let ((height (- bottom top))
	    (width (- right left)))
	(walk-to (cursor)
		 (- x (/ width 2))
		 (- y (/ height 2)))))))

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
  (when (xelfp (cursor))
    (layout (status-line))
    (update (status-line))))

(defmethod draw :after ((self scene))
  (with-fields (drag hover) self
    (when drag (draw drag))
    (when hover (draw-hover (find-object hover))))
  (when (xelfp (cursor))
    (draw (status-line))))

(define-method reset-game scene ()
  (let ((buffer (current-buffer)))
    (at-next-update 
      (switch-to-buffer (make-meadow))
      (destroy buffer))))

(defun find-camp ()
  (dolist (object (get-objects (current-buffer)))
    (when (typep object (find-class 'camp))
      (return object))))

;;; Various scenes

(defparameter *grassy-meadow-images* '("golden-meadow.png" "stone-road.png" "meadow.png"))
(defparameter *snowy-meadow-images* '("cloudy-meadow.png" "paynes-meadow.png" "purple-meadow.png" "sky-meadow.png" "forgotten-meadow.png"))
(defparameter *frozen-meadow-images* (image-set "frozen-meadow" 3))

(defthing (meadow scene)
  :background-image (random-choose '("stone-road.png" "meadow.png")))
;; flowers
;; twigs, a branch
;; dead tree
;; a few leafy trees

(defthing (grassy-meadow scene)
  :background-image (random-choose *grassy-meadow-images*))
;; flowers
;; twigs and stones
;; some leafy trees
;; wolf
;; ginseng and/or thornweed near dead tree and ruin wall

(defthing (forest scene)
  :background-image (random-choose '("stone-road.png" "meadow.png")))
;; flowers, silverwood
;; dense leafy trees
;; wood piles and twigs
;; wraiths, wolves
;; stones

(defthing (cold-meadow scene)
  :background-image (random-choose *snowy-meadow-images*))
;; twigs and stones
;; dead trees
;; thornweed

(defthing (ruins scene)
  :background-image (random-choose '("forgotten-meadow.png" "paynes-meadow.png" "purple-meadow.png")))
;; ruin walls
;; item boxes
;; item corpses (ginseng)
;; silverwood
;; ginseng trees
;; stones
;; wraiths 
;; blocked stairwells (for now)

(defthing (cemetery scene)
  :background-image (random-choose '("forgotten-meadow.png" "paynes-meadow.png")))
;; fences
;; stones and twigs
;; a few pine trees
;; bone dust
;; nightshade and bushes
;; one or two dead trees
;; rows of gravestones
;; grave hags

(defthing (frozen-forest scene)
  :background-image (random-choose *snowy-meadow-images*))
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
