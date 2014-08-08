(in-package :cypress)

;;; As the monk Geoffrey, the player drives the action

(defvar *geoffrey* nil)

(defun geoffrey () *geoffrey*)
  
(defthing (geoffrey monk) :description "Geoffrey" :hints nil :translation-timer 0)

(defmethod enable-translation ((self geoffrey))
  (setf (field-value :translation-timer self) (seconds->frames 120)))

(defmethod can-translate ((self geoffrey))
  (plusp (field-value :translation-timer self)))

(defmethod update-translation ((self geoffrey))
  (with-fields (translation-timer) self
    (when (plusp translation-timer)
      (decf translation-timer))))

(defmethod after-revive ((geoffrey geoffrey))
  (setf (field-value :path geoffrey) nil)
  (setf (field-value :tasks geoffrey) nil))

(defmethod stomach-full-p ((monk monk))
  (<= (field-value :hunger monk) 12))

(defmethod eat :after ((monk geoffrey) (food food))
  (let ((messages (if (stomach-full-p monk)
		      '("I'm full now." "My stomach is full." "I feel full.")
		      '("Very good!" "That's much better." "Delicious!"))))
    (bark monk (random-choose messages))))

(defparameter *hunger-hint* 
"Double-click Geoffrey to open the
inventory and find something to eat.
")

(defmethod alternate-tap ((self geoffrey) x y)
  (toggle-pause (current-scene)))

(defmethod initialize :after ((monk geoffrey) &key)
  (setf *geoffrey* monk)
  ;; temp
  ;; (add-inventory-item monk (quantity-of 'copper-gear 5))
  ;; (add-inventory-item monk (quantity-of 'nightshade 5))
  ;; (add-inventory-item monk (new 'bone-flute))
  ;; (add-inventory-item monk (quantity-of 'neume 3))
  ;; (add-inventory-item monk (quantity-of 'green-elixir 2))

  ;; (learn-spell (geoffrey) (new 'hold-creature))
  ;; (learn-spell (geoffrey) (new 'travel))
  ;; 
  (add-inventory-item monk (new 'spellbook))
  (add-inventory-item monk (new 'camp))
  (add-inventory-item monk (new 'bag))
  (add-inventory-item monk (quantity-of 'ginseng 2))
  (add-inventory-item monk (quantity-of 'stone 2))
  (add-inventory-item monk (quantity-of 'branch 6))
  (add-inventory-item monk (quantity-of 'white-bread 3))
  (add-inventory-item monk (quantity-of 'wooden-arrow 16))
  (add-inventory-item monk (make-scroll "Adventurer's guide" *help-text*))
  (add-inventory-item monk (make-scroll "Summons from Dr. Quine" *quine-summons*))
  (equip monk (find-arrow monk)))

(define-method show-movement-hint geoffrey ()
  (show-hint *movement-hint*))

(define-method show-object-hint geoffrey ()
  (show-hint *object-hint*))

(defmethod add-inventory-item :after ((monk geoffrey) (item thing) &optional merge)
  (resume))

(defmethod equip :after ((monk geoffrey) (self thing))
  (when (not (field-value :equipper self))
    (narrate "You cannot equip this item in its current location.")))

(defmethod eat :after ((monk geoffrey) (snowdrop snowdrop))
  (bark monk (random-choose '("I gained one Magic Point."))))

(defmethod humanp ((monk geoffrey)) t)

(defparameter *summons-hint* 
"Try reading the scrolls
in your inventory.")

(defmethod activate ((monk geoffrey))
  (resume)
  (show-hint *summons-hint*)
  (replace-gump monk (new 'browser :container monk)))

(defmethod collide :after ((monk geoffrey) (gump gump))
  (when (field-value :waypoints monk)
    (bring-to-front gump)))

(defmethod bleed :after ((monk geoffrey))
  (damage monk (- (random-choose '(2 4 6))))
  (bark monk "I'm bleeding!"))

(defmethod collide :after ((monk geoffrey) (wolf wolf)) 
  (when (and (not *paused*) (field-value :alive monk))
    (percent-of-time 1
      (narrate "You receive a deep gash!")
      (run-away wolf)
      (gash (geoffrey)))))

(defmethod collide :after ((monk geoffrey) (wolf black-wolf))
  (when (and (not *paused*) (field-value :alive monk))
    (percent-of-time 4
      (narrate "The wolf bites Geoffrey!")
      (gash (geoffrey))
      (bark (geoffrey) "Aaaaghh!")
      (run-away wolf)
      (damage monk (- (random-choose '(7 9))))
      (play-sample (random-choose '("unh-1.wav" "unh-2.wav" "unh-3.wav"))))))

(defmethod collide :after ((monk geoffrey) (enemy cryptghast))
  (when (and (not *paused*) (field-value :alive monk))
    (percent-of-time 10
      (narrate "The cryptghast bites Geoffrey!")
      (bark (geoffrey) "Aaaaghh!")
      (damage monk (- (random-choose '(2 3))))
      (play-sample (random-choose '("unh-1.wav" "unh-2.wav" "unh-3.wav"))))))

(defmethod modify-health :after ((monk geoffrey) points)
  (with-fields (alive health) monk
    (when alive
      (or (when (< health 40) 
	    (bark monk "I'm badly hurt!"))
	  (when (< health 20) 
	    (bark monk "I'm dying!"))))
    (when (and alive
	       (not (plusp health)))
      (die monk))))

(defmethod modify-cold :after ((monk geoffrey) points)
  (with-fields (cold) monk
    (bark-cold monk)
    (narrate "You feel colder. Currently at ~S percent." cold)))

(defparameter *monk-hide-weapon-time* (seconds->frames 10))

(defmethod standing-animation ((self geoffrey))
  (with-fields (aiming-bow last-fire-time) self
      (if aiming-bow
	  *monk-stand-bow-ready*
	  (if (> *monk-hide-weapon-time* 
		 (- *updates* last-fire-time))
	      *monk-stand-bow*
	      *monk-stand*))))

(defmethod walking-animation ((self geoffrey))
  (with-fields (aiming-bow last-fire-time) self
      (if aiming-bow
	  *monk-walk-bow-ready*
	  (if (> *monk-hide-weapon-time* 
		 (- *updates* last-fire-time))
	      *monk-walk-bow*
	      *monk-walk*))))

(defmethod walk-to :after ((monk geoffrey) x y)
  (with-fields (waypoints) monk
    (with-fields (barrier-y) (current-scene)
      ;; (when (null waypoints) (return-to-safe-point monk))
      (when (or
	     ;; pathfinding failed
	     (null waypoints)
	     ;; beyond invisible barrier
	     (and barrier-y (> y barrier-y)))
	(show-error monk x y)
	(stop-walking monk)
	(narrate "That destination is obstructed.")))))

;;; Learning new spells

(defmethod learn-spell ((self geoffrey) (spell spell))
  (add-spell (find-spellbook) spell))

(defmethod learn-spell :after ((self geoffrey) (spell spell))
  (narrate "You learned a new magic spell: ~A" (find-description spell))
  (magical-flourish))

(defthing tome
  :stacking nil
  :image (random-choose *book-images*)
  :spell nil)

(defmethod initialize :after ((tome tome) &key spell)
  (assert spell)
  (setf (field-value :spell tome) spell))

(defmethod activate ((tome tome))
  (learn-spell (geoffrey) (field-value :spell tome))
  (destroy tome))

(defmethod find-description ((tome tome))
  (format nil "Magic tome of ~A" 
	  (find-description (field-value :spell tome))))

(defun tome-of (spell-class)
  (new 'tome :spell (new spell-class)))

;;; Party members

(defmethod enter-scene ((self geoffrey))
  (when (lucius-in-party-p)
    (multiple-value-bind (x y) (left-of self)
      (add-object (current-scene) (lucius) (- x 10) (- y 10)))))

(defmethod exit-scene ((self geoffrey))
  (stop-walking self)
  (when (typep (current-scene)
	       (find-class 'scene))
    (unmark-camped (current-scene)))
  (when (lucius-in-party-p)
    (exit-scene (lucius)))
  (when (current-scene) 
    (remove-object (current-scene) self)))

;;; Keeping geoffrey on the map

(defmethod knock-toward-center ((geoffrey geoffrey))
  (stop-walking geoffrey)
  (multiple-value-bind (gx gy) (center-point (geoffrey))
    (multiple-value-bind (cx cy) (center-point (current-scene))
      (let ((jerk-distance (/ (distance cx cy gx gy) 16)))
	(move geoffrey (find-heading gx gy cx cy) jerk-distance)))))

(defmethod restrict-to-buffer ((geoffrey geoffrey))
  (unless (bounding-box-contains (multiple-value-list (bounding-box (current-scene)))
				 (multiple-value-list (bounding-box geoffrey)))
    (knock-toward-center geoffrey)))

;; (defmethod knock-backward ((geoffrey geoffrey) (thing thing))
;;   (multiple-value-bind (gx gy) (center-point (geoffrey))
;;     (multiple-value-bind (cx cy) (center-point thing)
;;       (let ((jerk-distance (/ (distance cx cy gx gy) 4)))
;; 	(move geoffrey (find-heading gx gy cx cy) jerk-distance)))))

;; (defmethod dig-out-maybe ((geoffrey geoffrey))
;;   (let ((embed (some #'(lambda (x) (when (has-tag x :round) x))
;; 		     (find-colliding-objects geoffrey))))
;;     (when embed 
;;       (knock-backward geoffrey embed))))

(defmethod run :after ((geoffrey geoffrey))
  (update-translation geoffrey)
  (restrict-to-buffer geoffrey))

;;; Geoffrey's magic tent

(defparameter *fire-images* (image-set "fire" 4))

(defthing fire :image (random-choose *fire-images*) :scale 1.1 :tags '(:ethereal))

(defmethod run ((fire fire))
  (percent-of-time 14 (setf (field-value :image fire) (random-choose *fire-images*))))

(defthing camp
  :description "Geoffrey's magic tent"
  :stacking nil
  :fire nil
  :timer nil
  :contained-image "tent-2.png"
  :image "tent-3.png"
  :tags '(:solid))

(defmethod activate ((camp camp)) nil)

(defmethod can-accept ((camp camp)) nil)

(defmethod will-accept ((thing thing) (camp camp)) nil)

(defmethod will-accept ((geoffrey geoffrey) (camp camp)) t)

(defmethod ignite ((camp camp))
  (with-fields (fire timer) camp
    (when (not fire)
      (mark-camped (current-scene))
      (setf fire (new 'fire))
      (setf timer (seconds->frames 15))
      (drop camp fire 25 125)
      (bring-to-front fire))))

(defmethod recover ((monk monk))
  (cancel-bleeding monk)
  (modify-health monk +12)
  (modify-magic monk +50)
  (modify-cold monk -75)
  (narrate "You rest at the campfire, and feel much better."))
  
(defmethod ignite :after ((camp camp))
  (recover (geoffrey)))

(defmethod douse ((camp camp))
  (with-fields (fire timer) camp
    (when fire
      (destroy fire)
      (setf fire nil)
      (setf timer nil))))

(defmethod can-pick ((camp camp)) 
  (not (null (field-value :container camp))))

(defmethod return-to-geoffrey ((camp camp))
  (with-fields (fire) camp
    (remove-object (current-scene) camp)
    (when fire
      (remove-object (current-scene) fire))
    (add-inventory-item (geoffrey) camp)))

(defmethod run ((camp camp))
  (with-fields (fire timer) camp
    (when (and fire timer)
      (decf timer)
      (unless (plusp timer)
	(douse camp)))))

