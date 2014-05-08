(in-package :cypress)


(defresource "prologue.ogg" :volume 70)

(defparameter *prologue-height* 1578)

(define-buffer prologue 
  (start-time :initform *updates*)
  (quadtree-depth :initform 4)
  (background-color :initform "black"))

(defthing mountain-foreground
  :scale 2.2
  :image "mountain-foreground.png")

(defmethod run ((self mountain-foreground))
  (move-toward self :right 0.17))

(defthing mountain-background
  :scale 2.2
  :image "mountain-background.png")

(defmethod run ((self mountain-background))
  (move-toward self :left 0.4))

(defthing mountain-sky
  :scale 2.2
  :image "mountain-sky.png")

(defmethod run ((self mountain-sky))
  (move-toward self :right 0.10))

(defthing fine-map
  :scale 2.2
  :image "fine-map.png")

(defmethod run ((self fine-map))
  (move-toward self :upleft 0.14))

(defthing smoke-map
  :scale 2.1
  :image "smoke.png")

(defmethod run ((self smoke-map))
  (move-toward self :upleft 0.12)
  (with-local-fields
    (resize self (* %width 1.0003) (* %height 1.0003))))

(defthing shadows
  :scale 2.3
  :image "souls.png")

(defmethod run ((self shadows))
  (move-toward self :left 0.25))

(defthing destiny
  :scale 2.6
  :image "destiny.png")

(defmethod run ((self destiny))
  (move-toward self :left 0.25))

(defthing guiding
  :scale 2.4
  :image "guiding.png")

(defmethod run ((self guiding))
  (move-toward self :upleft 0.1)
  (with-local-fields
    (resize self (* %width 1.0003) (* %height 1.0003))))

(defthing amalia
  :opacity 0.0
  :scale 2.4
  :image "amalia.png")

(defmethod run ((self amalia))
  (with-local-fields
    (move-toward self :upleft 0.24)
    (resize self (* %width 1.0003) (* %height 1.0003))
    (setf %opacity (min 1.0 (+ %opacity 0.001)))))

(defmethod draw ((self amalia))
  (with-local-fields 
    (draw-textured-rectangle %x %y 0.0
			     %width %height
			     (find-texture %image)
			     :opacity 0.1)))

(defthing hero
  :scale 2.4
  :image "hero.png")

(defmethod run ((self hero))
  (move-toward self :upleft 0.24)
  (with-local-fields
    (resize self (* %width 1.0003) (* %height 1.0003))))

(defthing shade
  :scale 2.2
  :image "shade.png")

(defmethod run ((self shade))
  (move-toward self :upleft 0.2)
  (with-local-fields
    (resize self (* %width 1.0003) (* %height 1.0003))))

(defparameter *amalia* (seconds->frames 8))
(defparameter *guiding* (seconds->frames 18))
(defparameter *tell-thee-now* (seconds->frames 33))
(defparameter *famine* (seconds->frames 42))
(defparameter *souls* (seconds->frames 50.5))
(defparameter *map-time* (seconds->frames 58))
(defparameter *destinies* (seconds->frames 71))
(defparameter *shade* (seconds->frames 79))

(defmethod initialize :after ((self prologue) &key)
  (resize self 1280 720)
  (drop-object self (new 'amalia) -240 -240)
  (play-music "prologue.ogg" :loop nil))

(defmethod momentp ((self prologue) time)
  (= *updates* (+ time (field-value :start-time self))))

(defmethod clear-objects ((self prologue))
  (dolist (object (get-objects self))
    (destroy object)))

(defmethod update :after ((self prologue))
  (cond ((momentp self *guiding*)
	 (clear-objects self)
	 (drop-object self (new 'guiding) 0 -100))
	((momentp self *famine*)
	 (clear-objects self)
	 (drop-object self (new 'smoke-map) -50 -150))
	((momentp self *tell-thee-now*)
	 (clear-objects self)
	 ;; (drop-object self (new 'mountain-sky) -200 0)
	 ;; (drop-object self (new 'mountain-foreground) -250 100)
	 (drop-object self (new 'mountain-background) -100 0))
	((momentp self *souls*)
	 (clear-objects self)
	 (drop-object self (new 'shadows) -100 -100))
	((momentp self *destinies*)
	 (clear-objects self)
	 (drop-object self (new 'destiny) 0 0))
	((momentp self *map-time*)
	 (clear-objects self)
	 (drop-object self (new 'hero) -200 -300))
	((momentp self *shade*)
	 (clear-objects self)
	 (drop-object self (new 'shade) 0 0))))
	 

	   


