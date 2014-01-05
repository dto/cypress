(in-package :cypress) 

(defun stacked-up (&rest things)
  (assert things)
  (if (= 1 (length things))
      (first things)
      (arrange-below (first things) (apply #'stacked-up (rest things)))))

(defun lined-up (&rest things)
  (assert things)
  (if (= 1 (length things))
      (first things)
      (arrange-beside (first things) (apply #'lined-up (rest things)))))

(defun stacked-up-randomly (&rest things)
  (bordered (apply #'funcall #'stacked-up (derange things))))

(defun lined-up-randomly (&rest things)
  (bordered (apply #'funcall #'lined-up (derange things))))

(defun randomly (&rest things)
  (apply #'funcall (or (percent-of-time 50 #'stacked-up-randomly)
		       #'lined-up-randomly) 
	 things))

(defun forest-border () (units 1))

(defun derange (things)
  (let ((len (length things))
	(things2 (coerce things 'vector)))
    (dotimes (n len)
      (rotatef (aref things2 n)
	       (aref things2 (random len))))
    (coerce things2 'list)))

(defvar *extra-padding* 0)
  
(defun with-hpadding (amount buffer)
  (with-field-values (height width) buffer
    (with-new-buffer 
      (paste-into (current-buffer) buffer amount 0) 
      (resize (current-buffer)
	      height
	      (+ width amount *extra-padding*)))))


(defun with-vpadding (amount buffer)
  (with-field-values (height width) buffer
    (with-new-buffer 
      (paste-into (current-buffer) buffer 0 amount) 
      (resize (current-buffer)
	      (+ height amount *extra-padding*)
	      width))))

(defun random-padding () (units (1+ (random 15))))

(defun horizontally (a b)
  (percent-of-time 50 (rotatef a b))
  (arrange-beside 
   (with-border (random-padding) a)
   (with-border (random-padding) b)))

(defun vertically (a b)
  (percent-of-time 50 (rotatef a b))
  (arrange-below 
   (with-border (random-padding) a)
   (with-border (random-padding) b)))

(defun either-way (a b)
  (funcall (or (percent-of-time 50 #'horizontally) #'vertically)
	   a b))

(defun bordered (x) 
  (assert (xelfp x))
  (with-border (forest-border) x))

(defun singleton (x) 
  (assert (xelfp x))
  (bordered (with-new-buffer (drop-object (current-buffer) (find-object x)) (trim (current-buffer)))))

(defun make-container (class contents)
  (let ((bag (new class)))
    (dolist (item contents)
      (add-inventory-item bag item))
    bag))

(defparameter *grab-bag-items* '(skull skull wolf-skull ruined-book ruined-book stone stone stone stone bag notebook forget-me-not forget-me-not forget-me-not violet warrior-key triangle-key))

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

(defun single-tree () 
   (singleton 
    (new (or (percent-of-time 95 'leafy-tree)
	     'dead-tree))))

(defun patch-of (&optional (class (random-choose '(snowdrop violet forget-me-not)))
			   (n (1+ (random 4))))
  (let ((patch (singleton (new class))))
    (dotimes (i n)
      (trim patch)
      (setf patch (randomly patch (singleton (new class)))))
    patch))

(defun lone-wolf ()
  (singleton (new 'wolf)))

(defun pack-of-wolves ()
  (with-border (units 5) 
    (either-way (lone-wolf)
		(if (percent-of-time 40 t)
		    (singleton (new 'wraith))
		    (lone-wolf)))))

(defun two-trees ()
  (with-border (units 2)
    (randomly (single-tree) 
	      (with-border (units (1+ (random 4)))
		(if (percent-of-time 80 t)
		    (single-tree)
		    (if (percent-of-time 50 t)
			(lone-wolf)
			(singleton (new 'wraith))))))))

(defun one-or-two-trees ()
  (trim 
   (if (percent-of-time 50 t)
       (two-trees)
       (if (percent-of-time 70 t)
	   (single-tree)
	   (if (percent-of-time 70 t)
	       (patch-of 'snowdrop)
	       (patch-of (random-choose '(stone stone forget-me-not))))))))

(defun some-trees ()
  (with-border (units 1)
    (randomly (one-or-two-trees)
		(if (percent-of-time 95 t)
		    (one-or-two-trees)
		    (singleton 
		     (new (random-choose '(dead-tree gray-rock))))))))

(defun dense-trees ()
  (with-border (units 2)
    (either-way 
       (some-trees)
       (with-border (random-padding) 
	 (some-trees)))))

(defun make-forest ()
  (with-border (units 3)
    (arrange-beside
     (with-border (units 5) (arrange-below (dense-trees) (dense-trees)))
     (with-border (units 5) (arrange-below (dense-trees) 
					   (if (percent-of-time 50 t)
					       (pack-of-wolves)
					       (singleton (new 'wraith))))))))

;; (defun trees.lisp (color &optional (size 6))
;;   (bordered
;;    (with-new-buffer

;;      (trim (current-buffer)))))

