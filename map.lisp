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

(defparameter *sector-size* (units 5.2))

(defthing sector
  :tags '(:fixed)
  :terrain nil
  :scale 0.6
  :row 0
  :column 0
  :scene nil 
  :height *sector-size*
  :width *sector-size*)

(defparameter *terrain-classes* '(forest frozen-forest meadow cave
  grassy-meadow cold-meadow frozen-meadow ruins ruined-hamlet river
  retreat northern-ruins valisade highway alonso-ruins amalia-ruins
  nothbehem southern-cave eastern-cave wizard-ruins garden
  southeastern-cave cemetery owl-garden))

(defparameter *terrain-icons* 
  (list 'forest *forest-icons*
	'frozen-forest *frozen-forest-icons*
	'nothbehem (list *home-image*)
	'alonso-ruins *ruins-icons*
	'northern-ruins *ruins-icons*
	'amalia-ruins *frozen-forest-icons*
	'southern-cave (list *road-image*)
	'southeastern-cave (list *road-image*)
	'eastern-cave (list *road-image*)
	'owl-garden *forest-icons*
	'meadow *meadow-icons*
	'grassy-meadow *grassy-meadow-icons*
	'cold-meadow *cold-meadow-icons*
	'frozen-meadow *frozen-meadow-icons*
	'ruins *ruins-icons*
	'retreat *frozen-meadow-icons*
	'wizard-ruins *ruins-icons*
	'ruined-hamlet *ruins-icons*
	'river *river-icons*
	'valisade *castle-icons*
	'cave (list *road-image*)
	'cemetery *danger-icons*
	'highway *frozen-forest-icons*
	;; can't visit these.
	'mountain-pass *meadow-icons*
	'large-mountain *mountain-icons*))
  
(defun terrain-icon (terrain)
  (random-choose (getf *terrain-icons* terrain)))

(defmethod initialize ((sector sector) &key terrain)
  (resize sector *sector-size* *sector-size*)
  (setf (field-value :scene sector) (new terrain))
  (setf (field-value :image sector) 
	(map-icon (field-value :scene sector)))
  (setf (field-value :terrain sector) terrain))

(defmethod set-coordinates ((sector sector) row column)
  (setf (field-value :row sector) row)
  (setf (field-value :column sector) column))

(defmethod find-description ((sector sector))
  (find-description (field-value :scene sector)))

(defparameter *map-row* 10)

(defparameter *map-column* 2)

(defun map-row ()
  *map-row*)

(defun map-column ()
  *map-column*)

(defmethod draw :before ((sector sector))
  ;; silly hack
  (when (or (typep (field-value :scene sector) (find-class 'valisade))
	    (typep (field-value :scene sector) (find-class 'retreat))
	    (typep (field-value :scene sector) (find-class 'northern-ruins)))
    (setf (field-value :image sector)
	  (map-icon (field-value :scene sector)))))

(defmethod draw ((sector sector))
  (with-fields (x y width height image row column) sector
    (let ((draw-p (if (and (= row (map-row))
			   (= column (map-column)))
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

(defmethod draw :after ((sector sector))
  (with-fields (x y width height terrain scene) sector
    (when (and (find-inventory-item (geoffrey) 'sextant)
	       (find-instances scene 'waystone))
      (draw-string "w" 
		   (+ x width -22)
		   (+ y height -22)
		   :color "red"
		   :font "oldania-title"))))

(defparameter *unlimited-travel* nil)

(defmethod can-travel-to ((sector sector))
  (and (can-be-visited (field-value :scene sector))
       (with-fields (row column) sector
	 (and
	  ;; ;; don't allow return to same square
	  ;; (not (and (= row *map-row*)
	  ;; 	    (= column *map-column*)))
	  (>= 1 (abs (- row *map-row*)))
	  (>= 1 (abs (- column *map-column*)))))))

(defmethod travel-to ((sector sector))
  (with-fields (terrain row column scene) sector 
    ;; determine travel dir
    (setf *travel-direction* 
	  (direction-to *map-column* *map-row* column row)) 
    (unless (eq :here *travel-direction*)
      (expend-travel-cost scene))
    ;; go
    (setf *map-row* row *map-column* column)
    (load-scene scene)))

(defmethod activate-maybe ((sector sector))
  (activate sector))

(defmethod activate ((sector sector))
  (if (or *unlimited-travel*
	   (can-travel-to sector))
      (travel-to sector)
      (show-error sector)))

(defun make-sector (key)
  (new 'sector :terrain key))

(defparameter *ildron-map-data*
  '((large-mountain large-mountain large-mountain
     large-mountain large-mountain large-mountain 
     large-mountain large-mountain large-mountain
     large-mountain)

    (large-mountain retreat frozen-meadow
     northern-ruins frozen-forest frozen-meadow 
     frozen-forest large-mountain amalia-ruins
     large-mountain)

    (frozen-meadow frozen-forest frozen-forest
     frozen-meadow cold-meadow ruins 
     cemetery large-mountain river
     large-mountain)

    (alonso-ruins frozen-meadow frozen-forest
     cold-meadow owl-garden cold-meadow
     ruins large-mountain river
     large-mountain)

    (garden frozen-forest forest
     forest cold-meadow cold-meadow
     cold-meadow frozen-forest frozen-meadow
     large-mountain)

     (ruined-hamlet cold-meadow valisade
      forest grassy-meadow forest
      cold-meadow forest frozen-forest
      large-mountain)

     (forest cold-meadow forest
      grassy-meadow forest forest 
      forest garden wizard-ruins
      large-mountain)
    
    (forest grassy-meadow nothbehem
     forest garden large-mountain 
     large-mountain southeastern-cave large-mountain
     large-mountain)

    (grassy-meadow forest forest 
     grassy-meadow forest large-mountain 
     large-mountain large-mountain large-mountain
     large-mountain)

    (large-mountain forest grassy-meadow 
     garden southern-cave large-mountain
     large-mountain large-mountain large-mountain
     large-mountain)

    (large-mountain large-mountain mountain-pass
     forest large-mountain large-mountain 
     large-mountain large-mountain large-mountain
     large-mountain)))
         
(defun test-map-sectors ()
  (let (valley)
    (dolist (line *ildron-map-data*)
      (push (mapcar #'make-sector line)
	    valley))
    (nreverse valley)))

(defthing (map-screen buffer)
  :sectors nil
  :row 10
  :column 2
  :background-image "parchment.png")

(defparameter *map-screen-left-margin* 470)
(defparameter *map-screen-top-margin* 80)

(defmethod begin-scene ((self map-screen)))

(defmethod find-sector ((map map-screen) row column)
  (when (and (plusp row) (plusp column))
    (with-fields (sectors) map
      (when (<= row (length sectors))
	(let ((ss (nth row sectors)))
	  (when (<= column (length ss))
	    (nth column ss)))))))

(defmethod find-neighbors ((map map-screen) r c)
  (let (neighbors)
    (dolist (coords '((-1 -1) (-1 0) (-1 1) (0 -1) (1 1) (1 -1) (1 0) (0 1)))
      (destructuring-bind (dr dc) coords
	(push (find-sector map (+ r dr) (+ c dc)) neighbors)))
    (delete nil neighbors)))

(defmethod travel-to :after ((sector sector))
  (with-fields (row column) sector
    (dolist (neighbor (find-neighbors (ildron) row column))
      (when (can-be-visited (field-value :scene neighbor))
	(generate-maybe (field-value :scene sector))))))

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
    (setf *map-row* 10 *map-column* 2)
    (with-fields (sectors) map 
      (setf sectors (test-map-sectors))
      (dolist (row sectors)
	(dolist (sector row)
	  (drop-object map sector)))
      (resize map *nominal-screen-width* *nominal-screen-height*)))
  map)

(defmethod update :after ((map map-screen)) 
  (arrange map))

(defmethod draw :after ((map map-screen))
  (mapc #'draw (z-sort (find-instances map 'bubble)))
  (draw-string "Current terrain square blinks. Click to identify terrain; double-click adjacent squares to travel."
	       860 900
	       :color "burlywood4"
	       :font "oldania-bold")
  (when *current-objective*
    (draw-string (format nil "Current Objective: ~A" *current-objective*) 40 900 :color "burlywood4" :font "oldania-bold")))

(defmethod alternate-tap ((map map-screen) x y) nil)

(defun ildron ()
  (or *map-screen*
      (setf *map-screen* (new 'map-screen))))

(defun switch-to-map ()
  (at-next-update
    (remove-object (current-buffer) (geoffrey))
    (switch-to-buffer (ildron))))

