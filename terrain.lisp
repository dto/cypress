(in-package :cypress) 

;;; Various prizes

(defparameter *grab-bag-items* '(skull skull wolf-skull ruined-book ruined-book stone stone stone stone bag notebook forget-me-not forget-me-not snowdrop forget-me-not violet warrior-key triangle-key))

(defparameter *boxed-items* '(white-bread wheat-bread xalcium-armor skull ruined-book atlas))

(defun grab (bag &optional (count (1+ (random 4))))
  (let (items)
    (dotimes (n count)
      (push (new (random-choose bag)) items))
    items))

(defun grab-bag ()
  (make-container 'bag (grab *grab-bag-items*)))

(defun make-box ()
  (make-container 'item-box (grab *boxed-items*)))

(defparameter *forest-debris-items* '(stone stone stone twig twig twig branch branch thornweed silverwood silverwood nightshade snowdrop snowdrop))

(defparameter *flowers* '(violet forget-me-not snowdrop))

(defparameter *reagents* '(ginseng thornweed silverwood nightshade stone))

(defun reagent-bag ()
  (make-container 'bag (grab *reagents* (1+ (random 3)))))

;;; Context-free generation rules

(defun forest-border () (units 3))

(defun bordered (x) 
  (assert (xelfp x))
  (with-border (forest-border) x))

(defun random-border () (units (1+ (random 15))))

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
  
(defun single-tree () 
   (singleton 
    (new (or (percent-of-time 95 'leafy-tree)
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
    (let ((patch (singleton (new (get-class)))))
      (dotimes (i count)
	(when trim (trim patch))
	(let ((class (get-class)))
	  (setf patch 
		(with-border (units (1+ (random 3)))
		  (randomly patch (singleton (new class)))))))
      patch)))

(defun lone-wolf ()
  (singleton (new 'wolf)))

(defun pack-of-wolves ()
  (with-border (units 5) 
    (randomly (lone-wolf) (lone-wolf))))

(defun debris () 
  (spray *forest-debris-items*
	 :count (1+ (random 4))
	 :trim nil))

(defun reagents ()
  (spray (random-choose *reagents*)
	 :count 3
	 :trim nil))

(defun enemy ()
  (singleton (new (random-choose '(wraith wraith wolf)))))

(defun stuff ()
  (if (percent-of-time 60 t)
      (debris)
      (if (percent-of-time 20 t)
	  (reagents)
	  (enemy))))

(defun trees-or-clearing ()
   (if (percent-of-time 50 t)
       (some-trees)
       (if (percent-of-time 70 t)
	   (with-border (units (+ 2 (random 3)))
	     (if (percent-of-time 40 t)
		 (single-tree)
		 (if (percent-of-time 50 t)
		     (debris)
		     (reagents))))
	   (patch-of 'gray-rock))))

(defun some-trees ()
  (spray (list 'leafy-tree 'leafy-tree (random-choose *forest-debris-items*))
	 :trim t
	 :count (random-choose '(2 2 3 4))))
    
(defun dense-trees ()
  (with-border (units (+ 1 (random 7)))
    (vertically 
     (horizontally (some-trees) (horizontally (stuff) (trees-or-clearing)))
     (horizontally (stuff) (horizontally (trees-or-clearing) (some-trees))))))

(defun make-forest ()
  (with-border (units 3)
    (dense-trees)))

;; (defun trees.lisp (color &optional (size 6))
;;   (bordered
;;    (with-new-buffer

;;      (trim (current-buffer)))))

