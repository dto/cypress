(in-package :valisade)

(defparameter *unit* 14)

(defun units (n) (* n *unit*))

(define-buffer meadow 
  :background-image "meadow.png"
  :quadtree-depth 6)

(define-method initialize meadow ()
  (initialize%super self)
  (let ((monk (new 'player-1-monk)))
    (add-object self monk 400 400)
    ;; adjust scrolling parameters 
    (setf (%window-scrolling-speed buffer) (/ *monk-speed* 2)
	  (%horizontal-scrolling-margin buffer) 2/5
	  (%vertical-scrolling-margin buffer) 4/7)
    ;;
    (set-cursor self monk)
    (snap-window-to-cursor self)
    (glide-window-to-cursor self)
    ;; allocate
    (install-quadtree self)))
