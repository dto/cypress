(in-package :cypress)

;;; Status-Line

(defun-memo status-line-health-string (n)
    (:key #'identity :test 'equal :validator #'identity)
  (format nil "Health: ~S    " n))

(defun-memo status-line-magic-string (n)
    (:key #'identity :test 'equal :validator #'identity)
  (format nil "Magic: ~S     " n))

(defun-memo status-line-equipment-string (n)
    (:key #'identity :test 'equal :validator #'identity)
  (format nil "Equipment: ~A       " n))

(defparameter *status-line-font* "oldania-bold")

(define-block-macro status-line
    (:super phrase
     :fields 
     ((orientation :initform :horizontal)
      (no-background :initform t))
     :inputs (:health (new 'label :read-only t :font *status-line-font*)
	      :magic (new 'label :read-only t :font *status-line-font*)
	      :equipment (new 'label :read-only t :font *status-line-font*)
	      :message (new 'label :read-only t :font *status-line-font*))))

(define-method update status-line ()
  (mapc #'pin %inputs)
  (set-value %%health (status-line-health-string (field-value :health (cursor))))
  (set-value %%magic (status-line-magic-string (field-value :magic (cursor))))
  (set-value %%equipment 
	     (status-line-equipment-string 
	      (if (equipped-item (cursor))
		  (fancy-description (equipped-item (cursor)))
		  "None")))
  (set-value %%message (if (field-value :paused (current-buffer))
			   "Game is paused."
			   " ")))

(defparameter *status-line-height* (units 1.8))
(defparameter *status-line-background-color* "black")
(defparameter *status-line-foreground-color* "white")

(defmethod initialize :after ((self status-line) &key)
  (dolist (input (field-value :inputs self))
    (setf (field-value :text-color input) *status-line-foreground-color*)))

(define-method layout status-line ()
  (with-fields (x y width height) self
    (setf x (window-x))
    (setf y (+ (window-y)
	       *gl-screen-height*
	       (- *status-line-height*)))
    (call-next-method)
    (setf height *status-line-height*)
    (setf width *gl-screen-width*)))

(define-method draw status-line ()
  (with-fields (x y width height) self
    (draw-box x y width height :color *status-line-background-color*)
    (mapc #'draw %inputs)))


