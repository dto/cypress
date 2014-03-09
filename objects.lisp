(in-package :cypress)

(defthing wood)

(defparameter *twig-images* (image-set "twig" 9))

(defthing (twig wood) 
  :scale 1.4
  :image (random-choose *twig-images*))

(defparameter *branch-images* (image-set "branch" 9))

(defthing (branch wood) 
  :quantity 3 
  :image (random-choose *branch-images*) 
  :scale 1.2)

(defparameter *silverwood-images* (image-set "silverwood" 9))

(defthing silverwood
  :scale 1.2 
  :image (random-choose *silverwood-images*))

(defparameter *iron-fence-images* (image-set "iron-fence" 7))

(defthing iron-fence 
  :tags '(:solid :fixed)
  :image (random-choose *iron-fence-images*)
  :scale 1.2)

(defparameter *berry-bush-images* (image-set "berry-bush" 5))

(defthing berry-bush 
  :tags '(:solid :fixed)
  :image (random-choose *berry-bush-images*)
  :scale 1.4)

(defparameter *bone-dust-images* (image-set "bone-dust" 4))

(defthing bone-dust :image (random-choose *bone-dust-images*))

(defparameter *ancient-road-images* (image-set "ancient-road" 10))

(defthing ancient-road
  :tags '(:fixed)
  :scale 2
  :image (random-choose *ancient-road-images*))

(defparameter *ancient-road-debris-images* (image-set "ancient-road-debris" 5))

(defthing ancient-road-debris
  :tags '(:fixed)
  :image (random-choose *ancient-road-debris-images*))

(defparameter *black-wolf-images* (image-set "black-wolf" 2))

(defparameter *campfire-image* "campfire.png")

(defthing campfire :tags '(:fixed) :image *campfire-image*)

(defparameter *crack-images* (image-set "crack" 6))

(defthing crack :tags '(:fixed) :image (random-choose *crack-images*))

(defparameter *large-crack-images* (image-set "large-crack" 3))

(defthing large-crack :tags '(:fixed) :image (random-choose *large-crack-images*))

(defparameter *gravestone-images* (image-set "gravestone" 11))

(defthing gravestone :tags '(:solid :fixed) :image (random-choose *gravestone-images*))

(defparameter *puddle-images* (image-set "puddle" 10))

(defthing puddle :tags '(:fixed) :scale 3 :image (random-choose *puddle-images*))

(defparameter *ruin-wall-images* (image-set "ruin-wall" 4))
(defparameter *skull-images* (image-set "skull" 3))
(defparameter *wolf-corpse-images* (image-set "wolf-skull" 3))

(defparameter *corpse-images* (image-set "corpse" 4))

(defthing corpse 
  :activated nil
  :image (random-choose *corpse-images*) 
  :tags '(:fixed)
  :stacking nil)

(defmethod can-accept ((corpse corpse)) t)

(defmethod activate ((corpse corpse))
  (with-fields (activated) corpse
    (unless activated
      (setf activated t)
      (percent-of-time 80
	(add-inventory-item corpse (quantity-of 'bone-dust (1+ (random 5))))
	 (percent-of-time 50 
	   (add-inventory-item corpse (new (grab-bag)))
	   (percent-of-time 30
	     (add-inventory-item corpse (new (make-box)))))))
    (replace-gump corpse (new 'browser :container corpse))))

(defparameter *item-box-images* (image-set "item-box" 2))

(defthing item-box :image (random-choose *item-box-images*) :stacking nil)

(defmethod can-accept ((box item-box)) t)

(defmethod activate ((item-box item-box))
  (replace-gump item-box (new 'browser :container item-box)))

(defparameter *chest-images* (image-set "chest" 2))

(defthing chest :image (random-choose *chest-images*))

(defmethod can-accept ((chest chest)) t)

(defparameter *bag-images* (image-set "bag" 3))

(defthing bag :stacking nil :scale 1.2 :image (random-choose *bag-images*))

(defmethod activate ((bag bag))
  (replace-gump bag (new 'browser :container bag)))

(defmethod can-accept ((bag bag)) t)

(defparameter *book-images* (image-set "book" 10))

(defthing book :stacking nil :image (random-choose *book-images*))
(defthing ruined-book :stacking nil :image (random-choose *book-images*))

(defparameter *notebook-images* (image-set "notebook" 3))

(defthing notebook :image (random-choose *notebook-images*))

(defthing atlas :image (random-choose *notebook-images*))

(defmethod activate ((atlas atlas))
  (show-old-map))

(defparameter *scroll-images* (image-set "scroll" 5))

(defthing scroll 
  :stacking nil
  :image (random-choose *scroll-images*) 
  :description "Scroll of Helping")

(defmethod activate ((self scroll))
  (let ((gump (new 'scroll-gump :text *help-text*)))
    (replace-gump self gump)
    (set-target-position gump (units 50) (units 1))))

(defthing wax-cylinder-letter
  :stacking nil
  :image (random-choose *scroll-images*) 
  :description "In re: wax cylinder recordings")

(defmethod activate ((self wax-cylinder-letter))
  (let ((gump (new 'scroll-gump :text *wax-cylinder-letter*)))
    (replace-gump self gump)))
	     
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

(defthing flower)

(defthing (snowdrop flower) :image (random-choose *snowdrop-images*))
(defthing (violet flower) :image (random-choose *violet-images*))
(defthing (forget-me-not flower) :image (random-choose *forget-me-not-images*))

(defthing stone :scale 0.8 :image (random-choose *stone-images*))

(defthing copper-lock :image (random-choose *copper-lock-images*))

(defthing copper-stairwell  :tags '(:fixed) :image (random-choose '("copper-stairwell-1.png" "copper-stairwell-2.png")))
(defthing copper-plate :tags '(:fixed) :image (random-choose '("copper-plate-1.png" "copper-plate-2.png")))

(defmethod can-pick ((campfire campfire)) nil)

(defthing dead-tree 
  :tags '(:solid :fixed) 
  :image (random-choose *dead-tree-images*)
  :scale 3.0)

(defparameter *leafy-tree-images* (image-set "leafy-tree" 12))

(defthing leafy-tree 
  :tags '(:solid :fixed) 
  :image (random-choose *leafy-tree-images*)
  :scale 2.0)

(defparameter *pine-tree-images* (image-set "pine-tree" 15))

(defthing pine-tree 
  :tags '(:solid :fixed) 
  :image (random-choose *pine-tree-images*)
  :scale 2.0)

(defthing gray-rock 
  :tags '(:solid :fixed) 
  :image (random-choose *gray-rock-images*)
  :scale 1.7)

(defthing skull 
  :image (random-choose '("skull-1.png" "skull-2.png" "skull-3.png")))

(defthing wolf-corpse :image (random-choose '("wolf-skull-1.png" "wolf-skull-2.png")))

(defthing remains :tags '(:fixed) :image (random-choose '("remains-1.png" "remains-2.png")))

(defmethod can-accept ((remains remains)) t)

(defmethod activate ((remains remains))
  (replace-gump remains (new 'browser :container remains)))

(defthing warrior-key :image "warrior-key.png")
(defthing triangle-key :image "triangle-key.png")
(defthing circle-key :image "circle-key.png")

;;; ruin walls

(defthing ruin-wall 
  :scale 2.0
  :image (random-choose *ruin-wall-images*)
  :tags '(:fixed :solid))

(defthing coverstone :tags '(:solid :fixed) :image (random-choose '("triangle-coverstone.png" "warrior-coverstone.png")))

;;; Various prizes

(defparameter *grab-bag-items* '(elixir skull wolf-corpse ruined-book silver-elixir
 stone stone silver-leggings thornweed nightshade white-bread wheat-bread))

(defparameter *silver-book-images* (image-set "silver-book" 3))

(defthing silver-book :image (random-choose *silver-book-images*))

(defmethod activate ((book silver-book))
  (bark (geoffrey) "I can't read this language."))

(defparameter *boxed-items* '(silver-elixir elixir silver-armor silver-leggings silver-bow silver-book))

(defun grab (bag &optional (count (+ 1 (random 3))))
  (let (items)
    (dotimes (n count)
      (push (new (random-choose bag)) items))
    items))

(defun grab-bag ()
  (make-container 'bag (grab *grab-bag-items*)))

(defun make-box ()
  (make-container 'item-box (grab *boxed-items*)))

(defun reagent-bag ()
  (make-container 'bag (grab *reagents*)))

