(in-package :cypress)

;;; The ground

(defparameter *ancient-road-images* (image-set "ancient-road" 10))

(defthing ancient-road
  :tags '(:fixed)
  :scale 2
  :image (random-choose *ancient-road-images*))

(defparameter *ancient-road-debris-images* (image-set "ancient-road-debris" 5))

(defthing ancient-road-debris
  :tags '(:fixed)
  :scale 2
  :image (random-choose *ancient-road-debris-images*))

;;; Ancient stairwells

(defparameter *ancient-stairwell-images* (image-set "ancient-stairwell" 3))

;;; Valisade ruins scene

(defparameter *valisade-background-image* "golden-meadow.png")

(defthing (valisade scene)
  :background-image *valisade-background-image*)

(defmethod find-description ((self valisade)) 
  (if (field-value :generated self)
      "ruined castle"
      "clearing"))

(defmethod map-icon ((self valisade))
  (if (field-value :generated self)
      "castle-1.png"
      "meadow-1.png"))

(defparameter *ruin-hint*
"You have discovered an
enormous stone ruin.")

(defmethod begin-scene :after ((self valisade))
  (percent-of-time 30 (cue-music self (random-choose '("ancient-fanfare.ogg" "kosmium.ogg" "monks.ogg"))))
  (show-hint *ruin-hint*))















