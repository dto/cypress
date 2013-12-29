(in-package :cypress)

;;; Wraiths

(defsprite enemy)

(defthing (wraith enemy)
  :seen-player nil
  :image-scale 600
  :sprite-height 130
  :sprite-width 130
  :tags '(:enemy)
  :hp 3
  :image (random-choose *wraith-images*))

(defmethod damage ((self wraith) points)
  (with-fields (hp) self 
    (play-sample "knock.wav")
    (decf hp points)
    (unless (plusp hp)
      (drop self (new 'remains))
      (drop self (new 'skull))
      (percent-of-time 20 (drop self (new 'scroll) 40 40))
      (play-sample "lichdie.wav")
      (destroy self))))

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
