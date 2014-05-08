(in-package :cypress)

;;; Arturo Pentaquin

(defthing (arturo monk) 
  :description "Arturo")

(defparameter *arturo-walk* 
  '(:repeat t
    :scale 820
    :frames (("arturo-walk-1.png" 4)
	     ("arturo-walk-2.png" 4)
	     ("arturo-walk-3.png" 4)
	     ("arturo-walk-4.png" 4))))

(defmethod walking-animation ((self arturo))
  *arturo-walk*)

(defparameter *arturo-stand*
  '(:scale 820
    :frames (("arturo-stand-1.png" 19)
	     ("arturo-stand-2.png" 16)
	     ("arturo-stand-3.png" 24))))

(defmethod standing-animation ((self arturo))
  *arturo-stand*)

