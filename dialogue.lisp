(in-package :f0rest)

;;; Dialogue

;; (defresource "monk-talk-1.png")
;; (defresource "monk-talk-2.png")
;; (defresource "monk-talk-3.png")
;; (defresource "monk-talk-4.png")

(defparameter *talking-images* 
  '("monk-talk-1.png" "monk-talk-2.png" "monk-talk-3.png" "monk-talk-4.png")) 

;; (defresource "balloon.png")
;; (defresource "balloon2.png")

(defvar *ball* nil)

(defparameter *balloon-images* 
  '("balloon.png" "balloon2.png"))

(defvar *dialogue* nil)

(defvar *actor* nil)

(defvar *dialogue-channel* nil)

(defun talkingp (thing) 
  (and (monkp thing)
       (field-value :talking thing))) 

(defun monk-talk-image ()
  (random-choose *talking-images*))

(defun dialogue-playing-p () 
  (and *dialogue* (integerp *dialogue-channel*)))

(defun say (actor line) 
  (setf *dialogue*
	(append *dialogue* (list (list actor line)))))

(defun act (actor method) 
  (setf *dialogue*
	(append *dialogue* (list (list actor method)))))

(defun stop-dialogue ()
  (setf *dialogue* nil)
  (halt-sample *dialogue-channel*)
  (setf *dialogue-channel* nil)
  (setf *actor* nil))

(defun play-dialogue ()
  (if (null *dialogue*)
      (stop-dialogue)
      (destructuring-bind (actor line) (pop *dialogue*)
	(when (blockyp *actor*)
	  (stop-talking *actor*))
	(setf *actor* actor)
	;; is it an audio line or an action?
	(etypecase line
	  (string 
	   (begin-talking *actor* line)
	   (setf *dialogue-channel* 
		 (play-sample line)))
	  (keyword 
	   (send line *actor*))))))

(defun update-dialogue ()
  (when (and (dialogue-playing-p)
	     (not (sdl-mixer:sample-playing-p *dialogue-channel*)))
    (play-dialogue)))
