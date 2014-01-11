(in-package :cypress)

(defparameter *paused-status-message* 
  "GAME IS PAUSED. Press the spacebar (or issue a command) to continue...")

(defvar *status-messages* nil)

(defparameter *status-message-time* (seconds->frames 4.8))

(defparameter *last-status-message-time* 0)

(defun narrate (format-string &rest args)
  (let ((message (apply #'format nil format-string args)))
    (unless (find message *status-messages* :test 'equal)
      (setf *status-messages*
	    (append *status-messages* (list message)))
      (setf *last-status-message-time* *updates*))))

(defun narrate-now (format-string &rest args)
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
  (format nil "Equipment: ~A       " n))

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
  (set-value %%message (if (field-value :paused (current-buffer))
			   *paused-status-message*
			   (or (current-status-message) " "))))

(defparameter *status-line-height* (units 2))
(defparameter *status-line-background-color* "black")
(defparameter *status-line-foreground-color* "white")

(defmethod initialize :after ((self status-line) &key)
  (setf *status-messages* nil)
  (setf *last-status-message-time* 0)
  (dolist (input (field-value :inputs self))
    (setf (field-value :text-color input) *status-line-foreground-color*)))

(define-method layout status-line ()
  (with-fields (x y width height) self
    (setf x (window-x))
    (setf y (+ (window-y)
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


