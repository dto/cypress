(in-package :cypress)

(defparameter *house-images* (image-set "house" 3))

(defthing house
  :scale 1.4
  :tags '(:solid :fixed)
  :image (random-choose *house-images*))

(define (nothbehem scene)
  :background-image "stone-road.png"
  :cold 0)

(define (pentaquin-house house)
  :scale 1.4
  :image "pentaquin-house.png")

(defun random-house ()
  (singleton (new 'house)))

(defun pentaquin-house ()
  (with-border (units 5) (singleton (new 'pentaquin-house))))

(defun find-pentaquin-house ()
  (find-instances (current-scene) 'pentaquin-house))

(defun nothbehem-p ()
  (typep (current-scene) (find-class 'nothbehem)))

(defmethod make-terrain ((self nothbehem))
  (with-border (units 12)
    (lined-up-randomly
     (stacked-up-randomly (flowers) (some-trees) (random-house) (wood-pile))
     (stacked-up-randomly (random-house) (some-trees) (flowers) (wood-pile))
     (stacked-up-randomly (random-house) (dead-trees) 
			  (stacked-up-randomly
			   (pentaquin-house)
			   (singleton (new 'arturo))
			   (singleton (new 'silverwood))
			   (spray '(ruin-wall silverwood silverwood) :trim t :count 10)
			   (flowers))))))
