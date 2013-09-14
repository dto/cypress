(in-package :cypress)

;;; Now some objects

(defthing book :image (random-choose *book-images*))

(defthing scroll :image (random-choose *scroll-images*) :z 20)

(define-method use scroll ()
  (drop self (new 'scroll-gump *letter-text*)))

(defthing skull :image (random-choose '("skull-1.png" "skull-2.png")))

(defthing remains :image (random-choose '("remains-1.png" "remains-2.png")))

;;; Arrows, the main weapon

(defparameter *arrow-size* 25)

(defsprite arrow
  :image-scale 40
  :image (random-choose *arrow-images*))

(define-method initialize arrow (heading)
  (block%initialize self)
  (setf %clock 400)
  (setf %heading heading))

(define-method collide arrow (thing)
  (cond ((enemyp thing) (damage thing 1) (destroy self))
	((solidp thing) (destroy self))))

(define-method update arrow ()
  (percent-of-time 13 (setf %image (random-choose *arrow-images*)))
  (resize self *arrow-size* *arrow-size*)
  (decf %clock)
  (if (minusp %clock)
      (destroy self)
      (forward self 15)))

;;; ruin walls

(defthing ruin-wall 
  :image-scale 1000
  :image (random-choose *ruin-wall-images*)
  :tags '(:fixed :solid))

(defthing coverstone :image "coverstone.png" :z 10)
(defthing item-box :image "item-box.png" :z 1)

