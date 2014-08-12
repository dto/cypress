(in-package :cypress)

(defthing subtitle :clock 240 :text "Subtitle text.")

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

(defmethod can-pick ((mountain mountain)) nil)
(defmethod tap ((mountain mountain) x y) (begin-quest))
(defmethod alternate-tap ((mountain mountain) x y) (begin-quest))
(defmethod scroll-tap ((mountain mountain) x y) (begin-quest))

(defmethod initialize ((self mountain) &key)
  (with-fields (image) self 
    (resize self (image-width image) (image-height image))))

(defmethod update ((self mountain))
  (move-toward self :up (field-value :speed self)))

(define (m1 mountain) :image "mist-1.png" :speed 0.6)
(define (m2 mountain) :image "mist-2.png" :speed 0.72)
(define (m3 mountain) :image "mist-3.png" :speed 0.85)
(define (m4 mountain) :image "mist-4.png" :speed 0.99)

(define ruin)

(defmethod can-pick ((ruin ruin)) nil)
(defmethod tap ((ruin ruin) x y) (begin-quest))
(defmethod alternate-tap ((ruin ruin) x y) (begin-quest))
(defmethod scroll-tap ((ruin ruin) x y) (begin-quest))

(defmethod initialize ((self ruin) &key)
  (with-fields (image) self 
    (resize self (image-width image) (image-height image))))

(defmethod update ((self ruin))
  (move-toward self :left (field-value :speed self)))

(define (r1 ruin) :image "mist-5.png" :speed 0.3)
(define (r2 ruin) :image "mist-6.png" :speed 0.4)
(define (r3 ruin) :image "mist-7.png" :speed 0.5)
(define (r4 ruin) :image "mist-8.png" :speed 0.6)

(define pisgah :image "mist-9.png" :speed 0.8)

(defmethod can-pick ((pisgah pisgah)) nil)
(defmethod tap ((pisgah pisgah) x y) (begin-quest))
(defmethod alternate-tap ((pisgah pisgah) x y) (begin-quest))
(defmethod scroll-tap ((pisgah pisgah) x y) (begin-quest))

(defmethod initialize ((self pisgah) &key)
  (with-fields (image) self 
    (resize self (image-width image) (image-height image))))

(defmethod update ((self pisgah))
  (move-toward self :down (field-value :speed self)))

(define (movie buffer) :start-time *updates*)

(defmethod clear-objects ((self movie))
  (dolist (object (get-objects self))
      (destroy object)))

(defmethod show-mountains ((self movie))
  (drop-object self (new 'm1) 0 -300)
  (drop-object self (new 'm2) 0 700)
  (drop-object self (new 'm3) 0 1000)
  (drop-object self (new 'm4) 0 1300))

(defmethod show-ruins ((self movie))
  (drop-object self (new 'r4) -200 300)
  (drop-object self (new 'r3) 800 350)
  (drop-object self (new 'r2) 0 300)
  (drop-object self (new 'r1) -500 -500))

(defmethod show-pisgah ((self movie))
  (drop-object self (new 'pisgah) 0 -1500))

(defresource "prologue.ogg" :volume 50)

(defmethod initialize :after ((self movie) &key)
  (mapc #'find-resource (image-set "mist" 9))
  (play-music "prologue.ogg" :loop nil)
  (resize self *nominal-screen-width* *nominal-screen-height*)
  (show-mountains self))

(defmethod draw :after ((buffer movie))
  (draw-box 0 (- *nominal-screen-height* (units 6))
	    *nominal-screen-width* (units 6)
	    :color "black")
  (dolist (subtitle (find-instances (current-buffer) 'subtitle))
    (draw subtitle)))

(defmethod tap ((movie movie) x y) (begin-quest))
(defmethod alternate-tap ((movie movie) x y) (begin-quest))
(defmethod scroll-tap ((movie movie) x y) (begin-quest))

;; (defmethod momentp ((self movie) time)
;;   (= *updates* (+ time (field-value :start-time self))))

(defmethod elapsed-time ((self movie))
  (with-fields (start-time) self
    (let ((delta (- *updates* start-time)))
      (if (zerop (mod delta 30))
	  (truncate (/ delta 30))
	  (cfloat (/ delta 30))))))

(defun begin-quest ()
  (magical-flourish)
  (halt-music 500)
  (load-scene (make-quest)))

(defmethod update :after ((self movie))
  (case (elapsed-time self)
    (10 (subtitle "In the land of Ildron there are Mindless wizards,"))
    (16 (subtitle "and machines with Minds;"))
    (22 (subtitle "there is a Spirit in the White Cypress, and a river full of Souls."))
    (34 (clear-objects self) 
     (show-ruins self))
    (38 (subtitle "The hills and vales of Ildron hide a thousand lost Empires;"))
    (48 (subtitle "I have watched them crumble, one by one; for ruin is the destiny of Kings."))
    (58 (subtitle "But humble Geoffrey knows nothing of these, being raised in the quietude of Monks."))
    (70 (clear-objects self)
     (show-pisgah self))
    (73 (subtitle "Not even I suspected, then, that he would be an agent of Prophecy,"))
    (82 (subtitle "and would pass into the history of Heroes."))
    (90 (subtitle "But I musn't jump ahead of the tale. Listen now! The Cypress is about to speak."))
    (104 (magical-flourish)
     (halt-music 15000)
     (load-scene (make-quest)))))

;; (defmethod release ((movie movie) x y &optional button))
;; (defmethod press ((movie movie) x y &optional button))




;; (defthing guiding
;;   :scale 3.1
;;   :image "guiding.png")

;; (defmethod run ((self guiding))
;;   (move-toward self :upleft 0.1)
;;   (with-local-fields
;;     (resize self (* %width 1.0003) (* %height 1.0003))))

