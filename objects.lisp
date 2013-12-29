(in-package :cypress)

(defparameter *ruin-wall-images* (image-set "ruin-wall" 4))
(defparameter *book-images* (image-set "book" 10))
(defparameter *skull-images* (image-set "skull" 3))
(defparameter *wolf-skull-images* (image-set "wolf-skull" 3))
(defparameter *scroll-images* (image-set "scroll" 5))
(defparameter *remains-images* (image-set "remains" 2))
(defparameter *notebook-images* (image-set "notebook" 3))
(defparameter *wood-images* (image-set "wood" 4))
(defparameter *wraith-images* (image-set "wraith" 3))
(defparameter *arrow-images* (image-set "arrow" 3))
(defparameter *fire-pit-images* (image-set "fire-pit" 3))
(defparameter *dead-tree-images* (image-set "dead-tree" 5))
(defparameter *gray-rock-images* (image-set "gray-rock" 8))
(defparameter *gray-stairwell-images* (image-set "gray-stairwell" 2))
(defparameter *copper-lock-images* (image-set "copper-lock" 5))

(defthing copper-lock :image (random-choose *copper-lock-images*))
(defthing gray-stairwell :tags '(:fixed) :image (random-choose *gray-stairwell-images*))
(defthing copper-stairwell  :tags '(:fixed) :image (random-choose '("copper-stairwell-1.png" "copper-stairwell-2.png")))
(defthing copper-plate :tags '(:fixed) :image (random-choose '("copper-plate-1.png" "copper-plate-2.png")))

(defthing campfire :image "fire-pit-3.png")
(defthing tent 
  :image (random-choose '("tent-1.png" "tent-2.png"))
  :tags '(:solid :fixed))
   
(defthing dead-tree 
  :tags '(:solid :fixed) 
  :image (random-choose *dead-tree-images*)
  :scale 2.5)

(defthing gray-rock 
  :tags '(:solid :fixed) 
  :image (random-choose *gray-rock-images*)
  :scale 1.7)

(defthing book :image (random-choose *book-images*))

(defthing scroll :image (random-choose *scroll-images*) :z 20)

(defmethod activate ((self scroll))
  (drop self (new 'scroll-gump *letter-text-2*)))

(defthing skull :image (random-choose '("skull-1.png" "skull-2.png")))

(defthing remains :image (random-choose '("remains-1.png" "remains-2.png")))

(defthing warrior-key :image "warrior-key.png")
(defthing triangle-key :image "triangle-key.png")
(defthing circle-key :image "circle-key.png")

(defthing xalcium-leggings :image "xalcium-leggings.png")
(defthing xalcium-armor :image "xalcium-armor.png")
(defthing xalcium-mail :image "xalcium-mail.png")

;;; Arrows, the main weapon

(defparameter *arrow-size* 25)

(defsprite arrow
  :image-scale 40
  :image (random-choose *arrow-images*))

(defmethod initialize ((self arrow) &key heading)
  (with-local-fields 
    (setf %clock 400)
    (setf %heading heading)))

(defmethod collide ((self arrow) thing)
  (cond ((enemyp thing) (damage thing 1) (destroy self))
	((solidp thing) (destroy self))))

(defmethod update ((self arrow))
  (with-local-fields 
    (percent-of-time 13 (setf %image (random-choose *arrow-images*)))
    (resize self *arrow-size* *arrow-size*)
    (decf %clock)
    (if (minusp %clock)
	(destroy self)
	(forward self 15))))

;;; ruin walls

(defthing ruin-wall 
  :image-scale 1000
  :image (random-choose *ruin-wall-images*)
  :tags '(:fixed :solid))

(defthing coverstone :image "coverstone.png" :z 10)
(defthing item-box :image "item-box.png" :z 1)

