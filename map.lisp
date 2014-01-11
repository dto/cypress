(in-package :cypress)

(defparameter *castle-icons* (image-set "castle" 2))
(defparameter *cold-meadow-icons* (image-set "cold-meadow" 2))
(defparameter *frozen-meadow-icons* (image-set "frozen" 2))
(defparameter *danger-icons* (image-set "danger" 2))
(defparameter *forest-icons* (image-set "forest" 6))
(defparameter *frozen-forest-icons* (image-set "frozen-forest" 6))
(defparameter *grassland-icons* (image-set "grassland" 3))
(defparameter *grassy-meadow-icons* (image-set "grassy-meadow" 2))
(defparameter *large-mountain-icons* (image-set "large-mountain" 3))
(defparameter *meadow-icons* (image-set "meadow" 2))
(defparameter *mountain-icons* (image-set "mountain" 5))
(defparameter *ruins-icons* (image-set "ruins" 2))
(defparameter *river-icons* (image-set "river" 2))
(defparameter *map-image* "map.png")
(defparameter *home-image* "home.png")
(defparameter *road-image* "road.png")

(defparameter *sector-size* (units 5))

(defthing sector
  :tags '(:fixed)
  :terrain nil
  :scale 1.1
  :image (random-choose *grassy-meadow-icons*))

(defparameter *terrain-classes* '(forest frozen-forest meadow
  grassy-meadow cold-meadow frozen-meadow ruins river valisade highway
  cemetery))

(defparameter *terrain-icons* 
  (list 'forest *forest-icons*
	'frozen-forest *frozen-forest-icons*
	'meadow *meadow-icons*
	'grassy-meadow *grassy-meadow-icons*
	'cold-meadow *cold-meadow-icons*
	'frozen-meadow *frozen-meadow-icons*
	'ruins *ruins-icons*
	'river *river-icons*
	'valisade *castle-icons*
	'highway *road-image*
	'cemetery *danger-icons*
	;; can't visit these.
	'home (list *home-image*)
	'mountain *mountain-icons*
	'large-mountain *large-mountain-icons*))
  
(defun terrain-icon (terrain)
  (random-choose (getf *terrain-icons* terrain)))

(defmethod initialize ((sector sector) &key terrain)
  (change-image sector (terrain-icon terrain))
  (setf (field-value :terrain sector) terrain)
  (resize sector *sector-size* *sector-size*))

(defmethod find-description ((sector sector))
  (pretty-string (symbol-name (field-value :terrain sector))))

(defmethod draw ((sector sector))
  (with-fields (x y width height image) sector
    (let ((image-width (image-width image))
	  (image-height (image-height image)))
	;; (if (> image-width image-height)
	;;     (draw-image image x y 
	;; 		:width width
	;; 		:height (- height 
	;; 			   (* height (/ width image-width))))
	    (draw-image image x y
			:height height
			:width (- width (* width (/ height image-height)))))))

(defmethod activate ((sector sector))
  (with-fields (terrain) sector 
    (switch-to-buffer (new terrain))))

(defun make-sectors (&rest keys)
  (mapcar #'(lambda (key)
	      (singleton (new 'sector :terrain key)))
	  keys))

(defun row-of-sectors (&rest keys)
  (apply #'lined-up (apply #'make-sectors keys)))

(defthing (map-screen buffer)
  :background-image "parchment.png")

(defmethod make-terrain ((map map-screen))
  (stacked-up
   (row-of-sectors 'grassy-meadow 'meadow 'forest 'cold-meadow 'forest 'mountain 'forest 'large-mountain 'frozen-meadow 'river 'large-mountain 'mountain)
   (row-of-sectors 'meadow 'grassy-meadow 'meadow 'forest 'forest 'cold-meadow 'ruins 'frozen-meadow 'cemetery 'mountain 'river 'valisade 'mountain)
   (row-of-sectors 'home 'meadow 'forest 'forest 'mountain 'forest 'cold-meadow 'cemetery 'ruins 'river 'frozen-meadow 'river 'mountain)))

(defmethod initialize :after ((map map-screen) &key)
  (paste-from map (make-terrain map) 130 250)
  (resize map 1280 781))
  


  
