(in-package :cypress)

(defresource (:name "oldania" :type :ttf :file "OldaniaADFStd-Regular.otf" :properties (:size 16)))
(defresource (:name "oldania-title" :type :ttf :file "OldaniaADFStd-Regular.otf" :properties (:size 20)))

(defparameter *gump-font* "oldania")
(defparameter *gump-title-font* "oldania-title")
(defparameter *gump-color* '(#x52 #x2e #x00)) 

(defthing gump
  (target-x :initform 0)
  (target-y :initform 0))

(define-method set-target gump (x y)
  (with-fields (target-x target-y) self
    (setf target-x (- x (window-x)))
    (setf target-y (- y (window-y)))))

(define-method move-to-target gump ()
  (with-fields (target-x target-y) self
    (move-to self 
	     (+ target-x (window-x))
	     (+ target-y (window-y)))))

(define-method arrange scroll-gump ()
  (resize self 
	  (* (image-width %image) *scroll-scale*)
	  (* (image-height %image) *scroll-scale*))
  (move-to-target self))

(define-method after-add-hook gump ()
  (set-target self %x %y))

(define-method drag gump (x y)
  (set-target self x y)
  (move-to self x y))
      
(defmacro defgump (name &body body)
  `(define-block (,name :super gump) ,@body))

;;; The scroll gump is for reading pages of text.

(defun split-into-pages (text lines-per-page)
  (let ((lines (split-string-on-lines text))
	(pages nil))
    (loop while lines do
      (if (<= (length lines) lines-per-page)
	  (progn 
	    (push lines pages)
	    (setf lines nil))
	  (progn 
	    (push (subseq lines 0 lines-per-page) pages)
	    (setf lines (subseq lines lines-per-page)))))
    (reverse pages)))

(defgump scroll-gump 
  :image "scroll-gump.png"
  :image-scale 300
  :pages nil
  :page-number 0)

(defparameter *scroll-scale* (/ 1 3))

(defparameter *lines-per-scroll* 14)

(define-method flip scroll-gump (&optional p)
  (with-fields (pages page-number) self
    (setf page-number 
	  (mod (or p (1+ page-number))
	       (length pages)))))

(define-method create scroll-gump (text)
  (setf %pages (split-into-pages text *lines-per-scroll*))
  (flip self 0))

(define-method tap scroll-gump (x y)
  (flip self))

(define-method alternate-tap scroll-gump (x y)
  (destroy self))
  
(define-method draw scroll-gump ()
  (draw%super self)
  (with-fields (x y z height width) self
    (let ((x0 (+ x (* 0.1 width)))
	  (y0 (+ y (* 0.18 height))))
      (let ((text-lines (nth %page-number %pages)))
	(loop while text-lines do
	  (draw-string (let ((line (pop text-lines)))
			 (if (plusp (length line))
			     line
			     " "))
		       x0 y0 :z z
			   :color *gump-color*
			   :font *gump-font*)
	  (incf y0 (font-height *gump-font*)))))))

;;; The talk gump 

(defparameter *button-margin* 2)

(defthing button
  ((method :initform nil)
   (target :initform nil)
   (label :initform "foo")
   (arguments :initform nil)))

(define-method create button (&key label method target arguments font)
  (setf %label label %method method %target target %arguments arguments))

(define-method arrange button ()
  (with-fields (label) self
    (let ((font *gump-font*))
      (resize self 
	      (+ (* 2 *button-margin*)
		 (font-text-width label font))
	      (font-height font)))))

(define-method draw button ()
  (draw-string %label (+ %x *button-margin*) %y :color *gump-color* :font *gump-font*))

(define-method tap button ()
  (when (xelfp %target)
    (apply #'xelf:send %method %target %arguments)))

(define-block-macro talk-gump 
    (:super gump
     :fields ((





