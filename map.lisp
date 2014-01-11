(in-package :cypress)

(defparameter *castle-icons* (image-set "castle" 2))
(defparameter *cold-meadow-icons* (image-set "cold-meadow" 2))
(defparameter *frozen-meadow-icons* (image-set "frozen" 2))
(defparameter *danger-icons* (image-set "danger" 2))
(defparameter *forest-icons* (image-set "forest" 6))
(defparameter *grassland-icons* (image-set "grassland" 3))
(defparameter *grassy-meadow-icons* (image-set "grassy-meadow" 2))
(defparameter *large-mountain-icons* (image-set "large-mountain" 3))
(defparameter *meadow-icons* (image-set "meadow" 2))
(defparameter *mountain-icons* (image-set "mountain" 5))
(defparameter *ruins-icons* (image-set "ruins" 2))
(defparameter *river-icons* (image-set "river" 2))
(defparameter *map-image* "map.png")
(defparameter *home-image* "home.png")

(defparameter *grassy-meadow-images* '("golden-meadow.png" "stone-road.png" "meadow.png"))
(defparameter *snowy-meadow-images* '("cloudy-meadow.png" "paynes-meadow.png" "purple-meadow.png" "sky-meadow.png" "forgotten-meadow.png"))
(defparameter *frozen-meadow-images* (image-set "frozen-meadow" 3))

(defthing sector
  :tags '(:fixed)
  :scale 1.1
  :image (random-choose *grassy-meadow-icons*))

(defparameter *terrain-icons* 
  (list :forest *forest-icons*
	:meadow *meadow-icons*
	:grassy-meadow *grassy-meadow-icons*
	:mountain *mountain-icons*
	:large-mountain *large-mountain-icons*
	:ruins *ruins-icons*
	:river *river-icons*
	:home (list *home-image*)
	:cold-meadow *cold-meadow-icons*
	:frozen-meadow *frozen-meadow-icons*
	:castle *castle-icons*
	:cemetery *danger-icons*))

(defun terrain-icon (terrain)
  (random-choose (getf *terrain-icons* terrain)))

(defmethod initialize ((sector sector) &key terrain)
  (change-image sector (terrain-icon terrain)))
  
(defun make-sectors (&rest keys)
  (mapcar #'(lambda (key)
	      (singleton (new 'sector :terrain key)))
	  keys))

(defun row-of-sectors (&rest keys)
  (apply #'lined-up (apply #'make-sectors keys)))

(defthing (map-screen scene)
  :background-image "parchment.png")

(defmethod make-terrain ((map map-screen))
  (stacked-up
   (row-of-sectors :grassy-meadow :meadow :forest :cold-meadow :forest :mountain :forest :large-mountain :frozen-meadow :river :large-mountain :mountain)
   (row-of-sectors :meadow :grassy-meadow :meadow :forest :forest :cold-meadow :ruins :frozen-meadow :cemetery :mountain :river :castle :mountain)
   (row-of-sectors :home :meadow :forest :forest :mountain :forest :cold-meadow :cemetery :ruins :river :frozen-meadow :river :mountain)))

(defmethod initialize :after ((map map-screen) &key)
  (resize map 1280 781))
  


  
