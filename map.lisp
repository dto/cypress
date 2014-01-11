(in-package :cypress)

(defparameter *castle-images* (image-set "castle" 2))
(defparameter *cold-meadow-images* (image-set "cold-meadow" 2))
(defparameter *danger-images* (image-set "danger" 2))
(defparameter *forest-images* (image-set "forest" 6))
(defparameter *grassland-images* (image-set "grassland" 3))
(defparameter *grassy-meadow-images* (image-set "grassy-meadow" 2))
(defparameter *large-mountain-images* (image-set "large-mountain" 3))
(defparameter *meadow-images* (image-set "meadow" 2))
(defparameter *mountain-images* (image-set "mountain" 5))
(defparameter *ruins-images* (image-set "ruins" 2))
(defparameter *river-images* (image-set "river" 2))
(defparameter *map-image* "map.png")
(defparameter *home-image* "home.png")
(defparameter *grassy-meadow-images*
'("golden-meadow.png" "stone-road.png" "bright-meadow.png"))
(defparameter *snowy-meadow-images* '("cloudy-meadow.png" "paynes-meadow.png" "purple-meadow.png" "forgotten-meadow.png"))
(defparameter *frozen-meadow-images* (image-set "frozen-meadow" 3))

(defthing (map scene)
  :background-image *map-image*)

(defmethod initialize :after ((map map) &key)
  (resize-to-background-image map))

(defthing sector
  :tags (:fixed)
  :image (random-choose *grassy-meadow-images*))

(defparameter *terrain-types* 
  '(:forest *forest-images*
    :meadow *meadow-images*
    :grassy-meadow *grassy-meadow-images*
    :mountain *mountain-images*
    :large-mountain *large-mountain-images*
    :ruins *ruins-images*
    :river *river-images*
    :home (list *home-image*)
    :cold-meadow *cold-meadow-images*
    :castle *castle-images*
    :cemetery *danger-images*))




(defmethod initialize ((sector sector) &key terrain)
  
  
  
