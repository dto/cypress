(in-package :f0rest)

(defparameter *unit* 14)

(defun units (n) (* n *unit*))

(define-buffer meadow 
  :background-image "meadow.png"
  :quadtree-depth 6
  :default-events
  '(((:pause) :transport-toggle-play)
    ((:e :alt) :edit-word)
    ((:x :control) :exec)
    ((:d :control) :delete-word)
    ((:c :control) :copy-word)
    ((:x :alt) :command-prompt)
    ((:g :control) :cancel)
    ((:c :alt) :clear-stack)
    ((:s :alt) :show-stack)
    ((:m :alt) :show-messages)
    ((:p :control) :paste)
    ((:return) :enter)
    ((:escape) :cancel)
    ((:f1) :help)
    ((:h :control) :help)
    ((:x :control) :edit-cut)
    ((:c :control) :edit-copy)
    ((:v :control) :edit-paste)
    ((:v :control :shift) :paste-here)
    ((:f9) :toggle-minibuffer)
    ((:f12) :transport-toggle-play)
    ((:g :control) :escape)
    ((:d :control) :drop-selection)))

(defresource "wood.wav" :volume 40)

(define-block wood :image "wood-1.png")

(define-method collide wood (thing)
  (when (monkp thing)
    (play-sample "wood.wav")
    (destroy self)))

(defun make-wood (&optional (n 0))
  (let ((wood (new 'wood)))
    (prog1 wood
      (change-image wood (nth (mod n 4) '("wood-1.png" "wood-2.png" "wood-3.png" "wood-4.png"))))))

(define-block stone1 :image "coverstone.png")
(define-block stone2 :image "coverstone2.png")
(define-block scroll :image "scroll.png")
(define-block skull :image (random-choose '("skull-1.png" "skull-2.png")))
(define-block remains :image (random-choose '("remains-1.png" "remains-2.png")))

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

;
      (resize buffer 979 1600)

      (drop-object buffer (new 'scroll) 90 90)
      (drop-object buffer (new 'stone1) 60 60)
      (drop-object buffer (new 'stone2) 90 90)
      (drop-object buffer (make-wood 1) 100 110)
      (drop-object buffer (make-wood 2) 100 130)
      (drop-object buffer (make-wood 3) 100 150)
      (drop-object buffer (make-wood 0) 100 180)

      ;; allocate
       (install-quadtree buffer)
      buffer))
