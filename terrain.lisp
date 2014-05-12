(in-package :cypress) 

;;; Some utility functions for generating terrain

(defun random-border () (units (1+ (random 3))))

(defun bordered-randomly (x) 
  (with-border (random-border) x))

(defun derange (things)
  (let ((len (length things))
	(things2 (coerce things 'vector)))
    (dotimes (n len)
      (rotatef (aref things2 n)
	       (aref things2 (random len))))
    (coerce things2 'list)))

(defvar *extra-padding* 0)
  
(defun padded-horizontally (amount buffer)
  (with-field-values (height width) buffer
    (with-new-buffer 
      (paste-into (current-buffer) buffer amount 0) 
      (resize (current-buffer)
	      height
	      (+ width amount *extra-padding*)))))

(defun padded-vertically (amount buffer)
  (with-field-values (height width) buffer
    (with-new-buffer 
      (paste-into (current-buffer) buffer 0 amount) 
      (resize (current-buffer)
	      (+ height amount *extra-padding*)
	      width))))

(defun random-padding () (units (1+ (random 15))))

(defun horizontally (a b)
  (percent-of-time 50 (rotatef a b))
  (place-beside 
   (padded-horizontally (random-padding) a)
   (padded-horizontally (random-padding) b)))

(defun vertically (a b)
  (percent-of-time 50 (rotatef a b))
  (place-below 
   (padded-vertically (random-padding) a)
   (padded-vertically (random-padding) b)))

(defun horizontally-or-vertically (a b)
  (funcall (random-choose '(#'horizontally #'vertically))
	   a b))

(defun place-beside (a b)
  (arrange-beside a b))

(defun place-below (a b)
  (arrange-below a b))

(defun stacked-up (&rest things)
  (assert things)
  (if (= 1 (length things))
      (first things)
      (place-below (first things) 
	     (apply #'stacked-up (rest things)))))

(defun lined-up (&rest things)
  (assert things)
  (if (= 1 (length things))
      (first things)
      (place-beside (first things) 
	      (apply #'lined-up (rest things)))))

(defun stacked-up-randomly (&rest things)
  (bordered-randomly (apply #'funcall #'stacked-up (derange things))))

(defun lined-up-randomly (&rest things)
  (bordered-randomly (apply #'funcall #'lined-up (derange things))))

(defun randomly (&rest things)
  (apply #'funcall (or (percent-of-time 50 #'stacked-up-randomly)
		       #'lined-up-randomly) 
	 things))

(defun singleton (x) 
  (with-new-buffer
    (drop-object (current-buffer) (find-object x))
    (layout (find-object x))
    (trim (current-buffer))))

(defun bordered-singleton (x)
  (with-border (units (+ 4 (random 8)))
    (singleton x)))
  
(defun single-tree () 
   (singleton 
    (new (or (percent-of-time 70 'leafy-tree)
	     'dead-tree))))

(defun patch-of (&optional class (n (1+ (random 4))))
  (let ((patch (singleton (new class))))
    (dotimes (i n)
;      (trim patch)
      (setf patch (randomly patch (singleton (new class)))))
    patch))

(defun group-of (&optional class (n (1+ (random 5))))
  (let ((patch (singleton (new class))))
    (dotimes (i n)
      (trim patch)
      (setf patch (with-border (units (1+ (random 3)))
		   (randomly patch (singleton (new class))))))
    patch))

(defun spray (class-or-classes 
	      &key trim 
	      (count (1+ (random 5))))
  (labels ((get-class ()
	     (etypecase class-or-classes
	       (symbol class-or-classes)
	       (cons (random-choose class-or-classes)))))
    (let ((patch (bordered-singleton (new (get-class)))))
      (dotimes (i count)
	(when trim (trim patch))
	(let ((class (get-class)))
	  (setf patch 
		(randomly patch (singleton (new class))))))
      patch)))

(defun spatter (&rest args)
  (let ((spray (apply #'spray args)))
    (let ((objects (get-objects spray)))
      (dolist (object objects)
	(with-fields (x y) object
	  (move-to object (+ x (random (units 7))) (+ y (random (units 7))))))
      spray)))

;; (defun some-trees ()
;;   (randomly 
;;    (spray *forest-debris-items* :trim t :count (+ 2 (random 4)))
;;    (spray 'leafy-tree
;; 	  :trim nil
;; 	  :count (random-choose '(4 5)))))

;; (defun ginseng-garden ()
;;   (lined-up-randomly (singleton (new (random-choose '(stone-stairwell copper-plate))))
;; 		     (spray '(dead-tree ruin-wall ginseng) :trim nil :count (+ 5 (random 5)))))
  
;;; Forest pieces

(defparameter *flowers* '(violet forget-me-not snowdrop))
(defparameter *reagents* '(ginseng silverwood silverwood stone stone stone branch branch snowdrop violet))

;;; Enemies in scenes

(defun stuff ()
  (if (percent-of-time 60 t)
      (debris)
      (if (percent-of-time 20 t)
	  (reagents)
	  (enemy))))


(defun rock-outcropping ()
  (spray '(gray-rock stone stone)
	 :trim t :count (+ 5 (random 4))))


;; (defun dense-trees ()
;;   (with-border (units 8)
;;     (stacked-up-randomly
;;      (lined-up-randomly (enemy) (some-trees) (ginseng-garden) (enemy))
;;      (lined-up-randomly (some-trees) (wood-pile) (enemy) (enemy)))))

;; (defun stuff-cluster ()
;;   (randomly (rock-outcropping) (spray '(dead-tree ruin-wall ginseng) :count (+ 5 (random 5)))))

;; (defun make-forest ()
;;   (with-border (units 3)
;;     (dense-trees)))
