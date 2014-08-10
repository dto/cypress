(in-package :cypress)

(defthing subtitle :clock 200 :text "Subtitle text.")

(defmethod initialize ((subtitle subtitle) &key text)
  (setf (field-value :text subtitle) text)
  (move-to subtitle 0 -100))

(defmethod arrange ((subtitle subtitle))
  (with-fields (text) subtitle
    (let ((text-width (font-text-width text "oldania-subtitle")))
      (resize subtitle text-width (font-height "oldania-subtitle"))
      (move-to subtitle (- (/ *nominal-screen-width* 2)
			   (/ text-width 2))
	       (- *nominal-screen-height* (units 4.2))))))

(defmethod run ((subtitle subtitle))
  (with-fields (clock) subtitle
    (decf clock)
    (unless (plusp clock)
      (destroy subtitle))))

(defmethod draw ((subtitle subtitle))
  (with-fields (text x y) subtitle
    (draw-string text x y :color "white" :font "oldania-subtitle")))

(defun subtitle (text)
  (dolist (subtitle (find-instances (current-buffer) 'subtitle))
    (destroy subtitle))
  (drop-object (current-buffer) (new 'subtitle :text text)))

;;; Opening movie

(define mountain)

(defmethod initialize ((self mountain) &key)
  (with-fields (image) self 
    (resize self (image-width image) (image-height image))))

(defmethod update ((self mountain))
  (move-toward self :up (field-value :speed self)))

(define (m1 mountain) :image "mist-1.png" :speed 0.4)
(define (m2 mountain) :image "mist-2.png" :speed 0.5)
(define (m3 mountain) :image "mist-3.png" :speed 0.6)
(define (m4 mountain) :image "mist-4.png" :speed 0.7)

(define (movie buffer) :start-time *updates*)

(defmethod clear-objects ((self movie))
  (dolist (object (get-objects self))
      (destroy object)))

(defmethod show-mountains ((self movie))
  (drop-object self (new 'm1) 0 -300)
  (drop-object self (new 'm2) 0 700)
  (drop-object self (new 'm3) 0 1000)
  (drop-object self (new 'm4) 0 1300))

(defmethod initialize :after ((self movie) &key)
  (play-music "prologue.ogg" :loop nil)
  (resize self *nominal-screen-width* *nominal-screen-height*)
  (show-mountains self))

(defmethod draw :after ((buffer movie))
  (draw-box 0 (- *nominal-screen-height* (units 6))
	    *nominal-screen-width* (units 6)
	    :color "black")
  (dolist (subtitle (find-instances (current-buffer) 'subtitle))
    (draw subtitle)))

(defmethod momentp ((self movie) time)
  (= *updates* (+ time (field-value :start-time self))))

(defmethod elapsed-time ((self movie))
  (with-fields (start-time) self
    (let ((delta (- *updates* start-time)))
      (if (zerop (mod delta 30))
	  (truncate (/ delta 30))
	  (cfloat (/ delta 30))))))

(defmethod update :after ((self movie))
  (case (elapsed-time self)
    (10 (subtitle "In the land of Ildron there are Mindless wizards,"))
    (14 (subtitle "and machines with Minds;"))
    (20 (subtitle "there is a Spirit in the White Cypress,"))
    (25 (subtitle "and a river full of Souls."))
    (36 (subtitle "The hills and vales of Ildron hide a thousand lost Empires;"))
    (41 (subtitle "I have watched them crumble, one by one."))
    (47 (subtitle "But our hero, Geoffrey, knows nothing of these;"))
    (51 (subtitle "for he grew to manhood in the quietude of Monks."))
    (58 (subtitle "He carries an urgent letter from his friend and mentor, Dr. Quine;"))
    (71 (subtitle "A summons to the great monastery at Valisade."))
    (80 (subtitle "As he crossed into the valley where Valisade lay,"))
    (88 (subtitle "There was a brilliant flash of white light."))))

(defmethod release ((movie movie) x y &optional button))
(defmethod press ((movie movie) x y &optional button))




;; (defthing guiding
;;   :scale 3.1
;;   :image "guiding.png")

;; (defmethod run ((self guiding))
;;   (move-toward self :upleft 0.1)
;;   (with-local-fields
;;     (resize self (* %width 1.0003) (* %height 1.0003))))

