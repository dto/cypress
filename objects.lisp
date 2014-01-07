(in-package :cypress)

(defparameter *ruin-wall-images* (image-set "ruin-wall" 4))
(defparameter *skull-images* (image-set "skull" 3))
(defparameter *wolf-skull-images* (image-set "wolf-skull" 3))

(defparameter *item-box-images* (image-set "item-box" 2))

(defthing item-box :image (random-choose *item-box-images*))

(defmethod can-accept ((box item-box)) t)

(defmethod activate ((item-box item-box))
  (replace-gump item-box (new 'browser :container item-box)))

(defparameter *chest-images* (image-set "chest" 2))

(defthing chest :image (random-choose *chest-images*))

(defmethod can-accept ((chest chest)) t)

(defparameter *bag-images* (image-set "bag" 3))

(defthing bag :scale 1.2 :image (random-choose *bag-images*))

(defmethod activate ((bag bag))
  (replace-gump bag (new 'browser :container bag)))

(defmethod can-accept ((bag bag)) t)

(defparameter *book-images* (image-set "book" 10))

(defthing book :image (random-choose *book-images*))
(defthing ruined-book :image (random-choose *book-images*))

(defparameter *notebook-images* (image-set "notebook" 3))

(defthing notebook :image (random-choose *notebook-images*))

(defthing atlas :image (random-choose *notebook-images*))

(defmethod activate ((atlas atlas))
  (show-old-map))

(defparameter *scroll-images* (image-set "scroll" 5))

(defthing scroll :image (random-choose *scroll-images*) :description "Scroll of Helping")

(defmethod activate ((self scroll))
  (drop self (new 'scroll-gump :text *help-text*)))

(defparameter *remains-images* (image-set "remains" 2))
(defparameter *wraith-images* (image-set "wraith" 3))
(defparameter *fire-pit-images* (image-set "fire-pit" 3))
(defparameter *dead-tree-images* (image-set "dead-tree" 5))
(defparameter *gray-rock-images* (image-set "gray-rock" 8))
(defparameter *gray-stairwell-images* (image-set "gray-stairwell" 2))
(defparameter *copper-lock-images* (image-set "copper-lock" 5))

(defparameter *nightshade-images* (image-set "nightshade" 6))
(defthing nightshade :scale 1.1 :image (random-choose *nightshade-images*)) 

(defparameter *ginseng-images* (image-set "ginseng" 4))
(defthing ginseng :image (random-choose *ginseng-images*)) 

(defparameter *thornweed-images* (image-set "thornweed" 8))
(defthing thornweed :scale 1.2 :image (random-choose *thornweed-images*)) 

(defparameter *snowdrop-images* (image-set "snowdrop" 3))
(defparameter *violet-images* (image-set "violet" 3))
(defparameter *forget-me-not-images* (image-set "forget-me-not" 3))
(defparameter *stone-images* (image-set "stone" 5))

(defthing snowdrop :image (random-choose *snowdrop-images*))
(defthing violet :image (random-choose *violet-images*))
(defthing forget-me-not :image (random-choose *forget-me-not-images*))
(defthing stone :scale 0.8 :image (random-choose *stone-images*))

(defthing copper-lock :image (random-choose *copper-lock-images*))
(defthing gray-stairwell :tags '(:fixed) :image (random-choose *gray-stairwell-images*))
(defthing copper-stairwell  :tags '(:fixed) :image (random-choose '("copper-stairwell-1.png" "copper-stairwell-2.png")))
(defthing copper-plate :tags '(:fixed) :image (random-choose '("copper-plate-1.png" "copper-plate-2.png")))

(defthing campfire :image "fire-pit-3.png")

(defmethod can-pick ((campfire campfire)) nil)

(defthing tent 
  :image (random-choose '("tent-1.png" "tent-2.png"))
  :tags '(:solid :fixed))
  
(defmethod activate ((tent tent))
  (replace-gump tent (new 'browser :container tent)))

(defmethod can-accept ((tent tent)) t)

(defthing dead-tree 
  :tags '(:solid :fixed) 
  :image (random-choose *dead-tree-images*)
  :scale 3.0)

(defparameter *leafy-tree-images* (image-set "leafy-tree" 12))

(defthing leafy-tree 
  :tags '(:solid :fixed) 
  :image (random-choose *leafy-tree-images*)
  :scale 2.0)

(defthing gray-rock 
  :tags '(:solid :fixed) 
  :image (random-choose *gray-rock-images*)
  :scale 1.7)

(defthing skull :image (random-choose '("skull-1.png" "skull-2.png")))
(defthing wolf-skull :image (random-choose '("wolf-skull-1.png" "wolf-skull-2.png")))

(defthing remains :tags '(:fixed) :image (random-choose '("remains-1.png" "remains-2.png")))

(defmethod activate ((remains remains))
  (replace-gump remains (new 'browser :container remains)))

(defthing warrior-key :image "warrior-key.png")
(defthing triangle-key :image "triangle-key.png")
(defthing circle-key :image "circle-key.png")

(defthing xalcium-leggings :image "xalcium-leggings.png")
(defthing xalcium-armor :image "xalcium-armor.png")
(defthing xalcium-mail :image "xalcium-mail.png")

;;; ruin walls

(defthing ruin-wall 
  :image-scale 1000
  :image (random-choose *ruin-wall-images*)
  :tags '(:fixed :solid))

(defthing coverstone :image "coverstone.png" :z 10)

