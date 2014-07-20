(in-package :cypress)

(defthing wood)

(defparameter *twig-images* (image-set "twig" 9))

(defthing (twig wood) 
  :scale 1.1
  :image (random-choose *twig-images*))

(defparameter *branch-images* (image-set "branch" 9))

(defthing (branch wood) 
  :quantity 3 
  :image (random-choose *branch-images*) 
  :scale 1.0)

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

(defmethod enter-scene ((bush berry-bush))
  (add-inventory-item bush (quantity-of 'nightshade (1+ (random 3)))))

(defmethod activate ((bush berry-bush))
  (replace-gump bush (new 'browser :container bush)))

(defparameter *bone-dust-images* (image-set "bone-dust" 4))

(defthing bone-dust :image (random-choose *bone-dust-images*))

(defparameter *ancient-road-images* (image-set "ancient-road" 10))

(defthing ancient-road
  :tags '(:fixed)
  :scale 3
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

(defmethod draw ((crack crack))
  (when (< (distance-between crack (geoffrey)) 200)
    (call-next-method)))

(defparameter *large-crack-images* (image-set "large-crack" 3))

(defthing large-crack :tags '(:fixed) :image (random-choose *large-crack-images*))

(defmethod draw ((crack large-crack))
  (when (< (distance-between crack (geoffrey)) 250)
    (call-next-method)))

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

(defmethod activate ((chest chest))
  (replace-gump chest (new 'browser :container chest)))

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

(defparameter *lorem-ipsum*
"Lorem ipsum dolor sit amet,
consectetuer adipiscing elit. Aenean
commodo ligula eget dolor. Aenean
massa. Cum sociis natoque penatibus et
magnis dis parturient montes, nascetur
ridiculus mus. Donec quam felis,
ultricies nec, pellentesque eu, pretium
quis, sem. Nulla consequat massa quis
enim. Donec pede justo, fringilla vel,
aliquet nec, vulputate eget, arcu. In
enim justo, rhoncus ut, imperdiet a,
venenatis vitae, justo. Nullam dictum
felis eu pede mollis pretium. Integer
tincidunt. Cras dapibus. Vivamus
elementum semper nisi. Aenean vulputate
eleifend tellus. Aenean leo ligula,
porttitor eu, consequat vitae, eleifend
ac, enim. Aliquam lorem ante, dapibus
in, viverra quis, feugiat a,
tellus. Phasellus viverra nulla ut metus
varius laoreet. Quisque rutrum. Aenean
imperdiet. Etiam ultricies nisi vel
augue. Curabitur ullamcorper ultricies
nisi. Nam eget dui.")

(defthing scroll
  :stacking nil
  :text *lorem-ipsum*
  :image (random-choose *scroll-images*)
  :description "Blank scroll")

(defmethod activate ((self scroll))
  (let ((gump (new 'scroll-gump :text (field-value :text self))))
    (replace-gump self gump)))

(defun make-scroll (description text)
  (let ((scroll (new 'scroll)))
    (setf (field-value :description scroll) description)
    (setf (field-value :text scroll) text)
    scroll))

(defparameter *help-text* 
"Welcome to the Adventurer's Guide. If
you click this scroll, you will advance
to the next page. Use the right mouse
button (or the Control key with the left
button) to close scrolls. You can also
position scrolls by left-clicking and
dragging them with the mouse. 

Right-click a destination to move
Geoffrey there. 

Right-click Geoffrey (or press Spacebar)
to pause and unpause the game. Issuing
any command resumes the game.

Click an object to show its name.
Drag objects to move them.
Drag objects onto Geoffrey to take them.
Double-click an object to activate it.
Double-click a monster to attack it.
Double click Geoffrey for his inventory.
Drag items into/out of inventory pop-ups.

Click spells in spellbook for
description. Double click spells in
spellbook to cast.

Double-click armor/arrows while in
inventory to equip/unequip them.

Use the Travel spell to explore the
land. Double-click land symbols to
explore regions.  If you die, press
Control-R for a new quest. You can
resume your last saved quest by
activating the Stone of Remembrance
after resetting the game.

You must eat. Traveling and combat cause
hunger. Double-click food to eat it.
Use the \"Cure meat\" spell to make
jerky from freshly killed wolves.

Geoffrey must stay warm. He will get
colder by progressing through the
terrain (or by stepping in water). You
have a magic tent and campfire which you
can use to heal and warm yourself up. To
use the tent, drag it out of your
inventory onto an open space on the
ground, and then cast Ignite. The tent
will automatically return to your
inventory when you travel away.

Geoffrey has three stats: Attack,
Defense, and Resistance. When Attack
power is raised by equipping a weapon or
casting a spell, Geoffrey's arrows do
more damage. When Defense power is
raised, Geoffrey is more resilient in
the face of attacks. When Resistance is
up, your resistance to cold is
increased. High resistance makes you
almost immune to cold.

Press \"I\" to see Geoffrey's inventory.
Press \"S\" to open the spellbook.
Press \"M\" to open the travel map.
")

(defthing (help-scroll scroll)
  :text *help-text*
  :stacking nil
  :image (random-choose *scroll-images*)
  :description "Adventurer's guide")

;; (defmethod activate :after ((self help-scroll))
;;   (let ((gump (get-gump self)))
;;     (when gump
;;       (set-target-position gump 
;; 			   (+ (window-x) (units 50))
;; 			   (+ (window-y) (units 1))))))

(defparameter *remains-images* (image-set "remains" 2))
(defparameter *wraith-images* (image-set "wraith" 3))
(defparameter *fire-pit-images* (image-set "fire-pit" 3))
(defparameter *dead-tree-images* (image-set "dead-tree" 5))
(defparameter *gray-rock-images* (image-set "gray-rock" 8))
(defparameter *gray-stairwell-images* (image-set "gray-stairwell" 2))

(defthing well :scale 1.1 :image "well.png" :description "old well" :tags '(:solid :fixed))

(defmethod activate ((well well)) 
  (narrate "You don't have a bucket or rope to draw water."))

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

(defthing tent :scale 0.9 :image (random-choose '("tent-1.png" "tent-2.png")) :tags '(:solid :fixed))

(defmethod can-pick ((campfire campfire)) nil)

(defthing dead-tree 
  :tags '(:solid :fixed :round) 
  :image (random-choose *dead-tree-images*)
  :scale 3.0)

(defparameter *leafy-tree-images* (image-set "leafy-tree" 12))

(defthing leafy-tree 
  :tags '(:solid :fixed :round) 
  :image (random-choose *leafy-tree-images*)
  :scale 2.0)

(defparameter *pine-tree-images* (image-set "pine-tree" 15))

(defthing pine-tree 
  :tags '(:solid :fixed :round) 
  :image (random-choose *pine-tree-images*)
  :scale 2.0)

(defthing gray-rock 
  :tags '(:solid :fixed) 
  :image (random-choose *gray-rock-images*)
  :scale 1.7)

(defthing skull 
  :image (random-choose '("skull-1.png" "skull-2.png" "skull-3.png")))

(defmethod find-lore ((skull skull))
  (random-choose *skull-lore*))

(defthing wolf-corpse :image (random-choose '("wolf-skull-1.png" "wolf-skull-2.png")))

(defthing remains :tags '(:fixed) :image (random-choose '("remains-1.png" "remains-2.png")))

(defmethod can-accept ((remains remains)) t)

(defmethod activate ((remains remains))
  (if (nearby-enemies-p)
      (narrate "Cannot search remains while enemies are near.")
      (replace-gump remains (new 'browser :container remains))))

(defthing warrior-key :image "warrior-key.png")
(defthing triangle-key :image "triangle-key.png")
(defthing circle-key :image "circle-key.png")

;;; ruin walls

(defthing ruin-wall 
  :scale 2.0
  :image (random-choose *ruin-wall-images*)
  :tags '(:fixed :solid))

(defthing coverstone :tags '(:solid :fixed) :image (random-choose
'("triangle-coverstone.png" "warrior-coverstone.png")))

;;; Various prizes

(defparameter *grab-bag-items* '(elixir elixir skull wolf-corpse
 ruined-book silver-elixir silver-elixir stone stone green-elixir green-elixir
 thornweed nightshade white-bread wheat-bread))

(defparameter *silver-book-images* (image-set "silver-book" 3))

(defthing silver-book :image (random-choose *silver-book-images*))

(defmethod activate ((book silver-book))
  (bark (geoffrey) "I can't read this language."))

(defparameter *boxed-items* '(silver-elixir elixir green-elixir ruined-book silver-book))

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

;;; The silver things and leather also

(defthing silver-bow :stacking nil :attack 3 :image "silver-bow.png")

(defmethod equipment-description ((self silver-bow))
  "Geoffrey wields a silver-strung longbow.")

(defmethod activate ((self silver-bow))
  (toggle-equipped (geoffrey) self))

(defthing silver-armor :stacking nil :defense 3 :resistance 1 :image "silver-armor.png")

(defmethod activate ((self silver-armor))
  (toggle-equipped (geoffrey) self))

(defmethod equipment-description ((self silver-armor))
  "Geoffrey is wearing silver armor.")

(defthing silverwool-leggings :stacking nil :image "woolen-leggings.png" :defense 1 :resistance 3)

(defmethod activate ((self silverwool-leggings))
  (toggle-equipped (geoffrey) self))

(defmethod equipment-description ((self silverwool-leggings))
  "Geoffrey is wearing silverwool leggings.")

(defthing silverwool-shirt :stacking nil :image "woolen-shirt.png" :defense 1 :resistance 3)

(defmethod equipment-description ((self silverwool-shirt))
  "Geoffrey is wearing a silverwool shirt.")

(defmethod activate ((self silverwool-shirt))
  (toggle-equipped (geoffrey) self))

(defthing leather-leggings :stacking nil :image "leather-leggings.png" :defense 1 :resistance 1)

(defmethod activate ((self leather-leggings))
  (toggle-equipped (geoffrey) self))

(defmethod equipment-description ((self leather-leggings))
  "Geoffrey is wearing leather leggings.")

(defthing leather-shirt :stacking nil :image "leather-shirt.png" :defense 1 :resistance 1)

(defmethod equipment-description ((self leather-shirt))
  "Geoffrey is wearing a leather shirt.")

(defmethod activate ((self leather-shirt))
  (toggle-equipped (geoffrey) self))
