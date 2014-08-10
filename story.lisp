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

(defresource "prologue.ogg" :volume 70)

(defparameter *prologue-height* 1578)

(define-buffer prologue
  (start-time :initform *updates*)
  (quadtree-depth :initform 4)
  (background-color :initform "black"))

(defmethod draw :after ((buffer prologue))
  (draw-box 0 (- *nominal-screen-height* (units 6))
	    *nominal-screen-width* (units 6)
	    :color "black")
  (dolist (subtitle (find-instances (current-buffer) 'subtitle))
    (draw subtitle)))

(defthing mountain-foreground
  :scale 3
  :image "mountain-foreground.png")

(defmethod run ((self mountain-foreground))
  (move-toward self :right 0.17))

(defthing mountain-background
  :scale 3
  :image "mountain-background.png")

(defmethod run ((self mountain-background))
  (move-toward self :left 0.4))

(defthing mountain-sky
  :scale 3
  :image "mountain-sky.png")

(defmethod run ((self mountain-sky))
  (move-toward self :right 0.10))

(defthing fine-map
  :scale 3
  :image "fine-map.png")

(defmethod run ((self fine-map))
  (move-toward self :upleft 0.14))

(defthing smoke-map
  :scale 3
  :image "smoke.png")

(defmethod run ((self smoke-map))
  (move-toward self :upleft 0.12)
  (with-local-fields
    (resize self (* %width 1.0003) (* %height 1.0003))))

(defthing shadows
  :scale 3
  :image "souls.png")

(defmethod run ((self shadows))
  (move-toward self :left 0.25))

(defthing destiny
  :scale 3.3
  :image "destiny.png")

(defmethod run ((self destiny))
  (move-toward self :left 0.25))

(defthing elysium-1
  :scale 3.5
  :image "elysium-1.png")

(defmethod run ((self elysium-1))
  (move-toward self :up 0.25))

(defthing elysium-2
  :scale 3.5
  :image "elysium-2.png")

(defmethod run ((self elysium-2))
  (move-toward self :up 0.25))

(defthing elysium-3
  :image "continued.png")

(defmethod run ((self elysium-3))
  (resize self *nominal-screen-width* *nominal-screen-height*))

(defthing guiding
  :scale 3.1
  :image "guiding.png")

(defmethod run ((self guiding))
  (move-toward self :upleft 0.1)
  (with-local-fields
    (resize self (* %width 1.0003) (* %height 1.0003))))

(defthing amalia
  :opacity 0.0
  :scale 3
  :image "amalia.png")

(defmethod run ((self amalia))
  (with-local-fields
    (move-toward self :upleft 0.24)
    (resize self (* %width 1.0003) (* %height 1.0003))
    (setf %opacity (min 1.0 (+ %opacity 0.001)))))

(defmethod draw ((self amalia))
  (with-local-fields 
    (draw-textured-rectangle %x %y 0.0
			     %width %height
			     (find-texture %image)
			     :opacity 0.1)))

(defthing hero
  :scale 4
  :image "hero.png")

(defmethod run ((self hero))
  (move-toward self :upleft 0.24)
  (with-local-fields
    (resize self (* %width 1.0003) (* %height 1.0003))))

(defthing shade
  :scale 4
  :image "shade.png")

(defmethod run ((self shade))
  (move-toward self :upleft 0.2)
  (with-local-fields
    (resize self (* %width 1.0003) (* %height 1.0003))))

(defparameter *amalia* (seconds->frames 9.5))
(defparameter *spirit* (seconds->frames 14))
(defparameter *therein* (seconds->frames 20))
(defparameter *pass* (seconds->frames 25))

(defparameter *guiding* (seconds->frames 21))
(defparameter *tell-thee-now* (seconds->frames 33))

(defparameter *tell-subtitle* (seconds->frames 36))
(defparameter *so-long-subtitle* (seconds->frames 41))
(defparameter *when-fire* (seconds->frames 47))
(defparameter *when-famine* (seconds->frames 51))
(defparameter *when-cypress* (seconds->frames 58))

(defparameter *famine* (seconds->frames 45))
(defparameter *souls* (seconds->frames 55))
(defparameter *map-time* (seconds->frames 61))
(defparameter *destinies* (seconds->frames 65))
(defparameter *shade* (seconds->frames 79))

(defparameter *elysium-1* (seconds->frames 72))
(defparameter *elysium-2* (seconds->frames 83))
(defparameter *elysium-3* (seconds->frames 95))

(defparameter *souls-call* (seconds->frames 71))
(defparameter *sanctuary* (seconds->frames 80))
(defparameter *story* (seconds->frames 88))

(defmethod initialize :after ((self prologue) &key)
  (resize self *nominal-screen-width* *nominal-screen-height*)
  (drop-object self (new 'amalia) -240 -340)
  (play-music "prologue.ogg" :loop nil))

(defmethod momentp ((self prologue) time)
  (= *updates* (+ time (field-value :start-time self))))

(defmethod clear-objects ((self prologue))
  (xelf::delete-all-textures)
  (dolist (object (get-objects self))
    (unless (typep object (find-class 'subtitle))
      (destroy object))))

(defmethod update :after ((self prologue))
  (cond ((momentp self *amalia*)
	 (subtitle "My name is Amalia."))
	((momentp self *spirit*)
	 (subtitle "I am the spirit who lives in the white cypress."))
	((momentp self *therein*)
	 (subtitle "Like those who dwelt therein before, I guide the souls of Ildron"))
	((momentp self *pass*)
	 (subtitle "as they pass between worlds."))
	((momentp self *guiding*)
	 (clear-objects self)
	 (drop-object self (new 'guiding) 0 -100))
	((momentp self *tell-thee-now*)
	 (clear-objects self)
	 (drop-object self (new 'mountain-background) -100 0))
	((momentp self *tell-subtitle*)
	 (subtitle "I will tell thee now of an age whose very stones have crumbled,"))
	((momentp self *when-fire*)
	 (subtitle "When ash and smoke brought famine and despair through Ildron,"))
	;; ((momentp self *when-famine*)
	;;  (subtitle "When famine and despair swept through Ildron."))
	((momentp self *when-cypress*)
	 (subtitle "When the Cypress died, leaving souls to roam in Shadow."))
	((momentp self *so-long-subtitle*)
	 (subtitle "so long ago began this tapestry of sorrows."))
	((momentp self *famine*)
	 (clear-objects self)
	 (drop-object self (new 'smoke-map) -50 -150))
	((momentp self *souls*)
	 (clear-objects self)
	 (drop-object self (new 'shadows) -100 -100))
	((momentp self *destinies*)
	 (clear-objects self)
	 (drop-object self (new 'destiny) 0 0))
	((momentp self *elysium-1*)
	 (clear-objects self)
	 (drop-object self (new 'elysium-1) 0 0))
	((momentp self *souls-call*)
	 (subtitle "These souls call out for a place they call \"Aelezium\"."))
	((momentp self *sanctuary*)
	 (subtitle "This \"Aelezium\" is believed to be a place of sanctuary."))
	((momentp self *story*)
	 (subtitle "And it is in \"Aelezium\" that our true story shall begin."))
	;; ((momentp self *elysium-2*)
	;;  (clear-objects self)
	;;  (drop-object self (new 'elysium-2) 0 0))
	((momentp self *elysium-3*)
	 (clear-objects self)
	 (drop-object self (new 'elysium-3) 0 0))))
	;; ((momentp self *map-time*)
	;;  (clear-objects self)
	;;  (drop-object self (new 'hero) -200 -300))
	 
;;; trailer

(defvar *trailer* nil)

(defthing card :height *nominal-screen-height* :width *nominal-screen-width*)

(defmethod update ((card card))
  (resize card *nominal-screen-width* *nominal-screen-height*)
  (move-to card 0 0))

(defthing (card-1 card) :image "card1.png")
(defthing (card-2 card) :image "card2.png")
(defthing (card-3 card) :image "card3.png")
(defthing (card-4 card) :image "card4.png")
(defthing (card-5 card) :image "card5.png")
(defthing (card-6 card) :image "card6.png")
(defthing (card-7 card) :image "card7.png")

(define-buffer trailer
  (start-time :initform *updates*)
  (quadtree-depth :initform 4)
  (background-color :initform "black"))

(defmethod initialize :after ((self trailer) &key)
  (setf *trailer* self)
  (setf *use-music* nil)
  (resize self *nominal-screen-width* *nominal-screen-height*)
  (drop-object self (new 'card-1))
  (play-music "traveler2.ogg" :loop nil))

(defmethod momentp ((self trailer) time)
  (= *updates* (+ time (field-value :start-time self))))

(defmethod clear-objects ((self trailer))
  (dolist (object (get-objects self))
      (destroy object)))

(defmethod switch-to-card ((self trailer) card)
  (switch-to-buffer self)
  (clear-objects self)
  (let ((card-object (new card)))
    (drop-object self card-object)
    (update card-object)))

(defun snapshot-file (n)
  (xelf::find-project-file *project* (format nil "snapshot-~S.xelf" n)))

(defun restore-snapshot (n)
  (load-quest (snapshot-file n)))

(defmethod elapsed-time ((self trailer))
  (with-fields (start-time) self
    (let ((delta (- *updates* start-time)))
      (if (zerop (mod delta 30))
	  (truncate (/ delta 30))
	  (cfloat (/ delta 30))))))

(defmethod update :after ((self trailer))
  (case (elapsed-time self)
    (6 (restore-snapshot 1)) ;; meeting lucius
    (12 (restore-snapshot 3))  ;; making camp
    (22 (switch-to-card self 'card-2))
    (27 (restore-snapshot 2)) ;; wizards
    (34 (switch-to-card self 'card-3))
    (39 (restore-snapshot 5)) ;; wraiths
    (46 (restore-snapshot 4)) ;; wiz
    (51 (switch-to-card self 'card-4)    
     (play-music "home.ogg" :loop nil))
    (56 (restore-snapshot 6)) 
    (61 (restore-snapshot 7))
    (65 (switch-to-card self 'card-5))
    (70 (switch-to-card self 'card-6))
    (75 (switch-to-card self 'card-7))))

;; (defmethod update :after ((geoffrey geoffrey))
;;   (when *trailer*
;;     (update *trailer*)))

(defmethod release ((trailer trailer) x y &optional button))
(defmethod press ((trailer trailer) x y &optional button))

;;; Opening movie

(define mountain)

(defmethod initialize ((self mountain) &key)
  (with-fields (image) self 
    (resize self (image-width image) (image-height image))))

(defmethod update ((self mountain))
  (move-toward self :up (field-value :speed self)))

(defresource "mist-1.png" :wrap-r :clamp-to-border :wrap-s :clamp-to-border)
(defresource "mist-2.png" :wrap-r :clamp-to-border :wrap-s :clamp-to-border)
(defresource "mist-3.png" :wrap-r :clamp-to-border :wrap-s :clamp-to-border)
(defresource "mist-4.png" :wrap-r :clamp-to-border :wrap-s :clamp-to-border)

(define (m1 mountain) :image "mist-1.png" :speed 0.2)
(define (m2 mountain) :image "mist-2.png" :speed 0.23)
(define (m3 mountain) :image "mist-3.png" :speed 0.28)
(define (m4 mountain) :image "mist-4.png" :speed 0.31)

(define (movie buffer))

(defmethod initialize :after ((self movie) &key)
  (play-music "passageway.ogg" :loop t)
  (resize self *nominal-screen-width* *nominal-screen-height*)
  (drop-object self (new 'm1) 0 -400)
  (drop-object self (new 'm2) 0 500)
  (drop-object self (new 'm3) 0 700)
  (drop-object self (new 'm4) 0 1000))





