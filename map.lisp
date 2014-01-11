(in-package :cypress)

(defvar *map-screen* nil)

(defparameter *castle-icons* (image-set "castle" 2))
(defparameter *cold-meadow-icons* (image-set "cold-meadow" 2))
(defparameter *frozen-meadow-icons* (image-set "frozen" 2))
(defparameter *danger-icons* (image-set "danger" 2))
(defparameter *forest-icons* (image-set "forest" 6))
(defparameter *frozen-forest-icons* (image-set "frozen-forest" 2))
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

(defparameter *sector-size* (units 6))

(defthing sector
  :tags '(:fixed)
  :terrain nil
  :scale 1.1
  :row 0
  :column 0
  :height *sector-size*
  :width *sector-size*)

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
	'highway (list *road-image*)
	'cemetery *danger-icons*
	;; can't visit these.
	'home (list *home-image*)
	'mountain *mountain-icons*
	'large-mountain *large-mountain-icons*))
  
(defun terrain-icon (terrain)
  (random-choose (getf *terrain-icons* terrain)))

(defmethod initialize ((sector sector) &key terrain)
  (resize sector *sector-size* *sector-size*)
  (setf (field-value :image sector) (terrain-icon terrain))
  (setf (field-value :terrain sector) terrain))

(defmethod set-coordinates ((sector sector) row column)
  (setf (field-value :row sector) row)
  (setf (field-value :column sector) column))

(defmethod find-description ((sector sector))
  (pretty-string (symbol-name (field-value :terrain sector))))

(defvar *map-row* 0)

(defvar *map-column* 0)

(defun find-map-row ()
  *map-row*)

(defun find-map-column ()
  *map-column*)

(defmethod draw ((sector sector))
  (with-fields (x y width height image row column) sector
    (let ((draw-p (if (and (= row (find-map-row))
			   (= column (find-map-column)))
		      (plusp (- (mod *updates* 30) 15))
		      t)))
      (when draw-p
	(let ((image-width (image-width image))
	      (image-height (image-height image)))
	  (if (>= image-width image-height)
	      (draw-image image x y 
			  :width width
			  :height (- height 
				     (* height (/ height image-height))))
	      (draw-image image x y
			  :height height
			  :width (- width (* width (/ width image-width))))))))))

(defmethod can-travel-to ((sector sector))
  (with-fields (row column) sector
    (and (<= 1 (abs (- row *map-row*)))
	 (<= 1 (abs (- column *map-column*))))))

(defmethod activate ((sector sector))
  (if (can-travel-to sector)
      (with-fields (terrain row column) sector 
	(setf *map-row* row *map-column* column)
	(switch-to-buffer (new terrain)))
      (show-error sector)))

(defun make-sector (key)
  (new 'sector :terrain key))

(defun test-map-sectors ()
  (list
   (mapcar #'make-sector '(home meadow forest cold-meadow forest mountain
			   forest large-mountain frozen-meadow river large-mountain mountain))
   (mapcar #'make-sector '(meadow grassy-meadow meadow forest forest cold-meadow ruins
			   frozen-meadow cemetery mountain river frozen-forest mountain))
   (mapcar #'make-sector '(grassy-meadow meadow forest forest mountain
			   forest highway cemetery ruins river frozen-meadow river
			   valisade))
   (mapcar #'make-sector '(meadow grassy-meadow forest forest 
			   cold-meadow ruins highway frozen-meadow frozen-forest river river))))
			   
(defthing (map-screen buffer)
  :sectors nil
  :row 0
  :column 0
  :background-image "parchment.png")

(defparameter *map-screen-left-margin* 100)
(defparameter *map-screen-top-margin* 230)

(defmethod find-sector ((map map-screen) row column)
  (with-fields (sectors) map
    (when (<= row (length sectors))
      (let ((ss (nth row sectors)))
	(when (<= column (length ss))
	  (nth column ss))))))
  
(defmethod arrange ((map-screen map-screen))
  ;; lay out the items
  (let ((x0 *map-screen-left-margin*)
	(y0 *map-screen-top-margin*)
	(sector-rows (field-value :sectors map-screen)))
    (dotimes (row (length sector-rows))
      (let ((sectors (pop sector-rows)))
	(dotimes (column (length sectors))
	  (let ((sector (pop sectors)))
	    (resize sector *sector-size* *sector-size*)
	    (set-coordinates sector row column)
	    (move-to sector 
		     (+ x0 (* column *sector-size*))
		     (+ y0 (* row *sector-size*)))))))))

(defmethod initialize :after ((map map-screen) &key)
  (with-buffer map
    (setf *map-screen* map)
    (with-fields (sectors) map 
      (setf sectors (test-map-sectors))
      (dolist (row sectors)
	(dolist (sector row)
	  (drop-object map sector)))
      (resize map 1280 781)))
  map)

(defmethod update :after ((map map-screen)) 
  (arrange map))

(defmethod alternate-tap ((map map-screen) x y) nil)

