(in-package :cypress)

(defresource "knock.wav" :volume 20)

;;; Wraiths

(defsprite enemy)

(defmethod can-pick ((enemy enemy)) nil)

(defmethod activate ((enemy enemy))
  (attack (cursor) enemy))

(defthing (wraith enemy)
  :seen-player nil
  :image-scale 600
  :sprite-height 130
  :sprite-width 130
  :tags '(:enemy)
  :health 15
  :image (random-choose *wraith-images*))

(defmethod die ((self wraith))
  (drop self (new 'remains))
  (drop self (new 'skull))
  (play-sound self "lichscream.wav")
  (destroy self))

(defmethod run ((self wraith))
  (with-fields (image heading seen-player) self
  (when (< (distance-to-cursor self) 500)
    (unless seen-player
      (play-sample "lichscream.wav")
      (setf seen-player t))
    (percent-of-time 16 (setf image (random-choose *wraith-images*)))
    (let ((heading0 (heading-to-cursor self)))
      (percent-of-time 13 
	(setf heading heading0))
      (percent-of-time 30
	(percent-of-time 12 (play-sample (random-choose '("growl-1.wav" "growl-2.wav"))))
	(move self heading0 4))))))
