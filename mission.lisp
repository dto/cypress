(in-package :f0rest)

;;; Missions and Goals

(defstruct goal 
  name 
  description
  condition ;; either a symbol or a function (or nil)
  state ; one of nil, :achieved, :failed
  prerequisites)

(defun check-condition (goal)
  (etypecase goal
    (keyword (check-condition (mission-variable-value goal)))
    (goal (or (eq :achieved (goal-state goal))
	      (let ((condition (goal-condition goal))
		    (prerequisites (goal-prerequisites goal)))
		(when (and (etypecase condition
			     (symbol (symbol-value condition))
			     (function (funcall condition)))
			   (or (null prerequisites)
			       (every #'check-condition prerequisites)))
		  (setf (goal-state goal) :achieved)))))))

(defun achieve (goal &optional force)
  (let ((prerequisites (goal-prerequisites goal)))
    (when (or force (every #'check-condition prerequisites))
      (setf (goal-state goal) t))))

(defvar *mission* nil)

(define-block mission
  name 
  title
  description
  address
  universe
  variables)

(define-method set-variable mission (var value)
  (setf (gethash var %variables) value))

(define-method get-variable mission (var)
  (gethash var %variables))

(defun mission-variable-value (var-name)
  (get-variable *mission* var-name))

(defun set-mission-variable-value (var-name value)
  (set-variable *mission* var-name value))

(defsetf mission-variable-value set-mission-variable-value)

(defmacro with-mission-locals (vars &rest body)
  (labels ((make-clause (sym)
	     `(,sym (mission-variable-value ,(make-keyword sym)))))
    (let* ((symbols (mapcar #'make-non-keyword vars))
	   (clauses (mapcar #'make-clause symbols)))
      `(symbol-macrolet ,clauses ,@body))))

(define-method completedp mission ()
  "Return T if all goal-valued mission variables are achieved."
  (with-fields (variables) self
    (block checking 
      (labels ((check (name goal)
		 (when (and (goal-p goal) 
			    (null (check-condition goal)))
		   (return-from checking nil))))
	(maphash #'check variables)
	(return-from checking t)))))
	       
;; (define-method begin mission (player)
;;   (assert (object-p player))
;;   (with-fields (name description address universe variables) self
;;     (assert (listp address))
;;     (when (null universe)
;;       (setf universe (if (null *universe*)
;; 			 (new 'universe)
;; 			 *universe*)))
;;     ;; this probably works better if you have already set up a universe.
;;     (setf *mission* self)
;;     (play universe :player player :address address)
;;     (do-prologue self)))
      
(define-method do-prologue mission ())

(define-method win mission ())

(define-method lose mission ())

(define-method end mission ())

(define-method run mission ())

;; (defmacro defmission (name (&key title description address)
;; 		      &rest goals)
;;   (let ((hash (gensym)))
;;     (labels ((set-goal (entry)
;; 	       (destructuring-bind (var-name &rest goal-props) entry
;; 		 `(setf (gethash ,(make-keyword var-name) ,hash) (make-goal ,@goal-props)))))
;;       `(let ((,hash (make-hash-table)))
;; 	 (progn ,@(mapcar #'set-goal goals))
;; 	 (define-prototype ,name (:super "BLOCKY:MISSION")
;; 	   (name :initform ,(make-keyword name))
;; 	   (description :initform ,description)
;; 	   (address :initform ,address)
;; 	   (variables :initform ,hash)
;; 	 (title :initform ,title))))))

;; ;; The flow goes defmission, initialize, begin, win/lose, end

;; (defparameter *test-grammar* 
;;   '((mission >> (at location please goal+ in exchange for reward))
;;     (location >> mars zeta-base nebula-m corva-3)
;;     (goal+ >> goal (goal and goal+))
;;     (goal >> (defeat foe) (defend friend) (activate button) (retrieve documents)
;;      (collect mineral+))
;;     (mineral+ >> mineral (mineral and mineral+))
;;     (mineral >> endurium technetium molybdenum francium a-biosilicates)
;;     (foe >> scanner biclops unique)
;;     (friend >> transport skiff soldier scientist)
;;     (unique >> zx-90 xioblade)
;;     (reward >> money part)
;;     (money >> 10000 20000 30000 40000 50000)
;;     (part >> muon-pistol lepton-cannon ion-shield-belt)))
