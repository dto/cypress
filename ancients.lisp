(in-package :cypress)

(defthing sextant :image "sextant.png" :scale 0.8)

(defparameter *megalith-images* (image-set "megalith" 2))

(defthing megalith 
  :scale 2.0
  :image (random-choose *megalith-images*)
  :tags '(:fixed)
  :description "giant stone block")

(defthing waystone 
  :scale 2.0
  :image (random-choose *megalith-images*)
  :tags '(:fixed)
  :description "ancient waystone")

(defmethod activate ((waystone waystone))
  (cue-music (current-scene) "flutism.ogg")
  (narrate "You feel the traces of an ancient memory, but nothing happens."))

(defparameter *copper-gear-images* (image-set "copper-lock" 5))

(defthing copper-gear 
  :scale 0.7
  :image (random-choose *copper-gear-images*))

(defparameter *copper-seal-images* (image-set "copper-seal" 4))

(defthing copper-seal
  :image (random-choose *copper-seal-images*)
  :touching nil
  :tags '(:fixed))

(defmethod collide ((seal copper-seal) (gear copper-gear))
  (setf (field-value :touching seal) t))

(defthing copper-plate 
  :tags '(:fixed) 
  :image (random-choose '("copper-plate-1.png" "copper-plate-2.png")))

(defthing copper-stairwell  :tags '(:fixed) :image (random-choose '("copper-stairwell-1.png" "copper-stairwell-2.png")))

(defparameter *copper-door-closed-images* (image-set "copper-door-closed" 2))
(defparameter *copper-door-opening-images* (image-set "copper-door-opening" 2))
(defparameter *copper-door-open-images* (image-set "copper-door-open" 2))

(defthing copper-door
  :image (random-choose *copper-door-closed-images*)
  :seal nil
  :open nil
  :tags '(:solid :fixed)
  :timer 0)

(defmethod lock ((door copper-door) (seal copper-seal))
  (setf (field-value :seal door) seal))

(defmethod door-image ((door copper-door) n)
  (cond ((> n 90) "copper-door-open-2")
	((> n 80) "copper-door-open-1")
	((> n 60) "copper-door-opening-1")
	((> n 30) "copper-door-opening-2")
	((> n 10) "copper-door-closed-1")
	((t "copper-door-closed-2"))))

(defmethod run ((door copper-door))
  (with-fields (timer open image seal) door
    (when seal
      (with-fields (touching) seal
	(setf timer 
	      (max 0
		   (min (+ timer (if touching 1 -1))
			100)))
	(setf image (door-image door timer))
	(if (plusp timer)
	    (progn (setf open t)
		   (remove-tag door :solid))
	    (progn (setf open nil)
		   (add-tag door :solid)))))))

;;; Ancient garden

(defthing (garden scene)
  :background-image (random-choose *grassy-meadow-images*))

(defmethod find-description ((scene garden)) "forest")
(defmethod map-icon ((scene garden)) (random-choose *forest-icons*))

(defun some-ginseng () (spatter '(ginseng stone twig twig) :trim t :count (+ 2 (random 5))))

(defun some-snowdrops () (spatter 'snowdrop :trim t :count (+ 1 (random 3))))

(defun ginseng-garden ()
  (stacked-up
   (spray '(ruin-wall) :trim nil :count (+ 2 (random 3)))
   (randomly (some-ginseng)
	     (with-border (units 5) (singleton (new 'waystone))))
   (spray '(ruin-wall copper-plate ginseng ruin-wall) :trim nil :count (+ 1 (random 4)))))

(defmethod make-terrain ((scene garden))
  (with-border (units 15)
    (stacked-up-randomly 
     (lined-up-randomly (some-snowdrops) (some-trees))
     (lined-up-randomly (some-snowdrops) (ginseng-garden)))))

(defmethod begin-scene :after ((scene garden))
  (or (percent-of-time 50 (cue-music scene (random-choose '("believe-me2.ogg" "kosmium.ogg"))) t)
      (percent-of-time 20 (cue-music scene (random-choose '("mountain.ogg" "xolaros3.ogg"))))))
      
;;; Ancient caves

(defparameter *ancient-cave-images* (image-set "ancient-cave" 3))

(defthing (cave scene)
  :darkness-image "darkness.png"
  :background-image (random-choose *ancient-cave-images*))

(defmethod make-terrain ((scene cave))
  (with-border (units 10)
    (stacked-up (singleton (new 'crumbling-stairwell))
		(spray '(ruin-wall copper-seal stone) :trim nil :count (random-choose '(2 3 4 5))))))

(defmethod initialize :after ((scene cave) &key)
  (with-fields (height width) scene
    (percent-of-time 80
      (dotimes (n (1+ (random 5)))
	(drop-object scene (new 'bone-dust) (random width) (random height))))))

(defmethod begin-scene :after ((scene cave))
  (with-fields (height width) scene
    (resize-to-background-image scene)
    (percent-of-time 40 (cue-music scene (random-choose '("monks.ogg" "dusk.ogg" "spiritus.ogg"))))
    (percent-of-time 20 (drop-object scene (new 'cryptghast) (random width) (random height)))	
    (percent-of-time 20 (drop-object scene (new 'cryptghast) (random width) (random height)))
    (percent-of-time 40 (drop-object scene (make-box) (- width 200) (- height 200)))))

(defthing (eastern-cave cave))
(defthing (southern-cave cave))

    
























