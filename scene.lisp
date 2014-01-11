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

(defmethod initialize :after ((buffer scene) &key)
  (setf (field-value :status-line buffer) 
	(find-object (new 'status-line)))
  (let ((terrain (make-terrain buffer)))
    (when terrain
      (paste-from buffer terrain))))

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
      (switch-to-buffer (random-terrain))
      (destroy buffer))))

;;; Various scenes

(defparameter *forest-debris-items* '(stone stone twig branch branch ginseng silverwood))
(defparameter *flowers* '(violet forget-me-not snowdrop))

(defthing (forest scene)
  :background-image (random-choose '("stone-road.png" "meadow.png")))

(defthing (frozen-forest scene)
  :background-image (random-choose *snowy-meadow-images*))

(defthing (meadow scene)
  :background-image (random-choose '("stone-road.png" "meadow.png")))

(defthing (grassy-meadow scene)
  :background-image (random-choose *grassy-meadow-images*))

(defthing (cold-meadow scene)
  :background-image (random-choose *snowy-meadow-images*))

(defthing (frozen-meadow scene)
  :background-image (random-choose *frozen-meadow-images*))

(defthing (river scene)
  :background-image (random-choose *frozen-meadow-images*))

(defthing (ruins scene)
  :background-image (random-choose '("forgotten-meadow.png" "paynes-meadow.png" "purple-meadow.png")))

(defthing (cemetery scene)
  :background-image (random-choose '("forgotten-meadow.png" "paynes-meadow.png")))

(defthing (highway scene)
  :background-image (random-choose '("stone-road.png" "golden-meadow.png")))

(defun random-terrain ()
  (new 'cemetery))
  ;; (new (random-choose '(forest frozen-forest meadow grassy-meadow
  ;; 			cold-meadow frozen-meadow river ruins cemetery highway))))
