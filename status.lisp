(in-package :cypress)

(defparameter *paused-status-message* 
  "GAME IS PAUSED. Issue a command to continue, or press SPACEBAR.")

(defvar *status-messages* nil)

(defparameter *status-message-time* (seconds->frames 4.8))

(defparameter *last-status-message-time* 0)

;; (defun narrate (format-string &rest args)
;;   (let ((message (apply #'format nil format-string args)))
;;     (unless (find message *status-messages* :test 'equal)
;;       (setf *status-messages*
;; 	    (append *status-messages* (list message)))
;;       (setf *last-status-message-time* *updates*))))

(defun narrate (format-string &rest args)
  (let ((message (apply #'format nil format-string args)))
      (setf *status-messages* (list message))
      (setf *last-status-message-time* *updates*)))

(defun current-status-message ()
  (when *status-messages* 
    (if (< *status-message-time* 
	   (- *updates* *last-status-message-time*))
	(prog1 (pop *status-messages*)
	  (setf *last-status-message-time* *updates*))
	(first *status-messages*))))

;;; Status-Line

(defun-memo status-line-health-string (n)
    (:key #'identity :test 'equal :validator #'identity)
  (format nil "Health: ~S    " n))

(defun-memo status-line-magic-string (n)
    (:key #'identity :test 'equal :validator #'identity)
  (format nil "Magic: ~S     " n))

(defun-memo status-line-equipment-string (n)
    (:key #'identity :test 'equal :validator #'identity)
  (format nil "Quiver: ~A       " n))

(defun-memo status-line-lighter-string (n)
    (:key #'identity :test 'equal :validator #'identity)
  (format nil "Status: ~A       " n))

(defresource (:name "oldania-medium-bold" :type :ttf :file "OldaniaADFStd-Bold.otf" :properties (:size 18)))

(defparameter *status-line-font* "oldania-medium-bold")

(define-block-macro status-line
    (:super phrase
     :fields 
     ((orientation :initform :horizontal)
      (no-background :initform t))
     :inputs (:health (new 'label :read-only t :font *status-line-font*)
	      :magic (new 'label :read-only t :font *status-line-font*)
	      :equipment (new 'label :read-only t :font *status-line-font*)
	      :message (new 'label :read-only t :font *status-line-font*))))

(defmethod tap ((self status-line) x y)
  (replace-gump (geoffrey) (new 'scroll-gump :text (status-text))))

(define-method update status-line ()
  (mapc #'pin %inputs)
  (set-value %%health (status-line-health-string (field-value :health (geoffrey))))
  (set-value %%magic (status-line-magic-string (field-value :magic (geoffrey))))
  (set-value %%equipment 
	     (let ((item (equipped-item (geoffrey))))
	       (status-line-equipment-string 
		(if item
		    (format nil " ~A x ~A "
			    (fancy-description item)
			    (quantity item))
		  "None"))))
  (set-value %%message (if *paused*
			   *paused-status-message*
			   (or (current-status-message) 
			       (status-lighter-string)))))

(defparameter *status-line-height* (units 2))
(defparameter *status-line-background-color* "black")
(defparameter *status-line-foreground-color* "white")

(defmethod initialize :after ((self status-line) &key)
  (setf *status-messages* nil)
  (setf *last-status-message-time* 0)
  (dolist (input (field-value :inputs self))
    (setf (field-value :text-color input) *status-line-foreground-color*)))

(defun status-window-x () 
  (field-value :window-x (or (current-scene) (current-buffer))))

(defun status-window-y () 
  (field-value :window-y (or (current-scene) (current-buffer))))

(define-method layout status-line ()
  (with-fields (x y width height) self
    (setf x (status-window-x))
    (setf y (+ (status-window-y)
	       *gl-screen-height*
	       (- *status-line-height*)))
    (call-next-method)
    ;; manually adjust layout a bit to prevent jitter caused by
    ;; changing stat numbers in proportional font
    (move-to %%equipment 
	     (+ x (* 0.18 *gl-screen-width*))
	     (field-value :y %%message))
    (move-to %%message 
	     (+ x (* 0.43 *gl-screen-width*))
	     (field-value :y %%message))
    (setf height *status-line-height*)
    (setf width *gl-screen-width*)))

(define-method draw status-line ()
  (with-fields (x y width height) self
    (draw-box x y width height :color *status-line-background-color*)
    (mapc #'draw %inputs)))

(defun health-description (points)
  (format nil "Geoffrey is ~A."
	  (cond ((< points 10)
		 "close to death")
		((< points 25)
		 "badly wounded")
		((< points 50)
		 "seriously wounded")
		((< points 80)
		 "wounded")
		((< points 95)
		 "lightly wounded")
		((<= points 100)
		 "alive and well"))))

(defun health-lighter-string (points0)
  (let ((points points0))
    (cond ((< points 10)
	   "Dying")
	  ((< points 25)
	   "Crippled")
	  ((< points 50)
	   "Badly hurt")
	  ((< points 80)
	   "Injured")
	  ((< points 90)
	   "Slightly injured"))))

(defun hunger-description (points0)
  (let ((points (- 100 points0)))
    (format nil "Geoffrey is ~A."
	    (cond ((< points 10)
		   "on the verge of starvation")
		  ((< points 25)
		   "becoming weak from hunger")
		  ((< points 50)
		   "extremely hungry")
		  ((< points 80)
		   "hungry")
		  ((< points 87)
		   "slightly hungry")
		  ((<= points 100)
		   "not hungry")))))

(defun hunger-lighter-string (points0)
  (let ((points (- 100 points0)))
    (cond ((< points 10)
	   "Starving")
	  ((< points 25)
	   "Extremely hungry")
	  ((< points 50)
	   "Very hungry")
	  ((< points 80)
	   "Hungry")
	  ((< points 87)
	   "Slightly hungry"))))

(defun cold-description (points0)
  (let ((points (- 100 points0)))
    (format nil "Geoffrey is ~A."
	    (cond ((< points 10)
		   "freezing to death")
		  ((< points 25)
		   "shivering from extreme cold")
		  ((< points 50)
		   "very cold")
		  ((< points 75)
		   "cold")
		  ((< points 90)
		   "slightly cold")
		  ((< points 95)
		   "comfortable")
		  ((<= points 100)
		   "comfortable and warm")))))

(defun cold-lighter-string (points0)
  (let ((points (- 100 points0)))
    (cond ((< points 10)
	   "Freezing")
	  ((< points 25)
	   "Shivering")
	  ((< points 50)
	   "Very cold")
	  ((< points 80)
	   "Cold")
	  ((< points 87)
	   "Slightly cold"))))

(defun compass-direction (dir)
  (ecase dir
    (:here :here)
    (:up :north)
    (:down :south)
    (:upright :northeast)
    (:upleft :northwest)
    (:downright :southeast)
    (:downleft :southwest)
    (:left :west)
    (:right :east)))

(defun status-lighter-string ()
  (concatenate 'string 
	       (format nil "Attack +~S" (attack-rating (geoffrey)))
	       "  " 
	       (format nil "Defense +~S" (defense-rating (geoffrey)))
	       "  "
	       (format nil "Resist +~S" (resistance-rating (geoffrey)))
	       "      "
	       (if (or (null *travel-direction*)
		       (eq :here *travel-direction*))
		   ""
		   (format nil "Headed ~A" (string-downcase (symbol-name (compass-direction *travel-direction*)))))
	       (if (traversed (current-scene))
		   " * "
		   "   ")
	       "     "
	       (or (health-lighter-string (field-value :health (geoffrey))) "") 
	       "     "
	       (or (hunger-lighter-string (field-value :hunger (geoffrey))) "")
	       "     "
	       (or (cold-lighter-string (field-value :cold (geoffrey))) "")))

	       

(defun magic-description (points)
  (or (when (<= points 20)
	(format nil "Geoffrey's magic is very low."))
      (when (<= points 35)
	(format nil "Geoffrey's magic is low."))))
      	  
(defun attack-description (points)
  (when (plusp points)
    (format nil "Attack power is raised ~A points." points)))

(defun defense-description (points)
  (when (plusp points)
    (format nil "Defensive power is up ~A points." points)))

(defun resistance-description (points)
  (when (plusp points)
    (format nil "Resistance is up ~A points." points)))

(defun make-page (lines)
  (apply #'concatenate 'string
	 (mapcar #'(lambda (x)
		     (concatenate 'string x (string #\Newline)))
		 lines)))

(defun status-text ()
  (with-fields (health magic cold hunger) (geoffrey)
    (make-page
     (delete nil
	     (append 
	      (list 
	       (health-description health)
	       (magic-description magic)
	       (hunger-description hunger)
	       (cold-description cold))
	      (mapcar #'equipment-description
		      (equipment (geoffrey)))
	      (list
	       (attack-description (attack-rating (geoffrey)))
	       (defense-description (defense-rating (geoffrey)))
	       (resistance-description (resistance-rating (geoffrey)))))))))
