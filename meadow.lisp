(in-package :valisade)

(defresource "meadow.png")

(defparameter *unit* 14)

(defun units (n) (* n *unit*))

(define-buffer meadow 
  :background-image "meadow.png"
  :quadtree-depth 6)

(defresource "wood-1.png")
(defresource "wood-2.png")
(defresource "wood-3.png")
(defresource "wood-4.png")

(defresource "wood.wav" :volume 40)

(define-block wood :image "wood-1.png")

(define-method collide wood (thing)
  (play-sample "wood.wav")
  (destroy self))

(defun make-wood (&optional (n 0))
  (let ((wood (new 'wood)))
    (prog1 wood
      (change-image wood (nth (mod n 4) '("wood-1.png" "wood-2.png" "wood-3.png" "wood-4.png"))))))

(defun make-meadow ()
    (let ((monk (new 'player-1-monk))
	  (buffer (new 'meadow)))
      (add-object buffer monk 400 400)
      ;; adjust scrolling parameters 
      (setf (%window-scrolling-speed buffer) (/ *monk-speed* 2)
	    (%horizontal-scrolling-margin buffer) 2/5
	    (%vertical-scrolling-margin buffer) 4/7)
      ;;
      (set-cursor buffer monk)
      (snap-window-to-cursor buffer)
      (glide-window-to-cursor buffer)

      (resize buffer 1533 2300)

      (drop-object buffer (make-wood 0) 100 1400)
      (drop-object buffer (make-wood 1) 1100 300)
      (drop-object buffer (make-wood 2) 1900 120)
      (drop-object buffer (make-wood 3) 500 1300)

      ;; allocate
       (install-quadtree buffer)
      buffer))
