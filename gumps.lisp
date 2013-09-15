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

(define-method arrange scroll-gump ()
  (resize self 
	  (* (image-width %image) *scroll-scale*)
	  (* (image-height %image) *scroll-scale*))
  (move-to-target self))

;;; The talk gump 

(defparameter *button-margin* 2)

(defthing button
  (method :initform nil)
  (target :initform nil)
  (label :initform "foo")
  (arguments :initform nil))

(define-method create button (&key label method target arguments font)
  (setf %label (format nil "* ~A " label)
	%method method 
	%target target 
	%arguments arguments))

(define-method arrange button ()
  (with-fields (label) self
    (let ((font *gump-font*))
      (resize self 
	      (+ (* 2 *button-margin*)
		 (font-text-width label font))
	      (font-height font)))))

(define-method can-pick button () nil)
(define-method pick button () nil)

(define-method draw button ()
  (draw-string %label (+ %x *button-margin*) %y :color *gump-color* :font *gump-font*))

(define-method tap button (x y)
  (when (xelfp %target)
    (apply #'xelf:send %method %target %arguments)))

(defun make-topic-button (target topic)
  (new 'button :label (pretty-string (make-keyword topic))
	       :method (discussion-method topic)
	       :target target))

(defun make-talk-gump-text (data)
  (let ((text (new 'text data)))
    (prog1 text
      (set-font text *gump-font*)
      (set-background-color text nil)
      (set-read-only text t))))

(defparameter *lines-per-talk-gump* 3)

(defgump talk-gump 
  (topic :initform nil)
   (pages :initform nil)
   (page-number :initform 0))

(defparameter *talk-gump-scale* (/ 1 2.3))

(define-method create talk-gump ()
  (setf %inputs (list 
		 (make-talk-gump-text "hello")
		 (make-sentence nil)))
  (update-parent-links self))

(define-method flip talk-gump (&optional p)
  (with-fields (inputs pages page-number) self
    (setf page-number 
	  (mod (or p (1+ page-number))
	       (length pages)))
    (setf (first inputs)
	  (make-talk-gump-text (nth page-number pages)))
    (update-parent-links self)))

(define-method can-pick talk-gump () t)
(define-method pick talk-gump () self)

(define-method tap talk-gump (x y)
  (flip self))

(define-method alternate-tap talk-gump (x y)
  (destroy self))
  
(define-method buttons talk-gump ()
  (second %inputs))

(define-method text talk-gump ()
  (first %inputs))

(define-method insert-button talk-gump (item)
  (unfreeze (buttons self))
  (accept (buttons self) item)
  (freeze (buttons self)))

(define-method destroy-buttons talk-gump ()
  (mapc #'destroy (%inputs (buttons self)))
  (setf (%inputs (buttons self)) nil))

(define-method replace-buttons talk-gump (items)
  (destroy-buttons self)
  (dolist (item items)
    (insert-button self item)))

(define-method number-of-buttons talk-gump ()
  (length (%inputs (buttons self))))

(define-method draw talk-gump ()
  (draw-image "talk-scroll.png" %x %y :width %width :height %height)
  (mapc #'draw %inputs))

(define-method arrange talk-gump ()
  (move-to-target self)
  (when (xelfp (text self))
    (let ((x0 (+ %x (* 0.13 %width)))
	  (y0 (+ %y (* 0.14 %height))))
      (resize-to-fit (text self))
      (move-to (text self) x0 y0)
      (move-to (buttons self) x0 (+ y0 (%height (text self))))
      (layout (buttons self))))
  (resize self 
	  (* (image-width "talk-scroll.png") *talk-gump-scale*)
	  (* (image-height "talk-scroll.png") *talk-gump-scale*)))

(define-method configure talk-gump (text buttons)
  (setf %pages (split-into-pages text *lines-per-talk-gump*))
  (when buttons (replace-buttons self buttons))
  (flip self 0))

(defun make-talk-gump (thing text &rest button-keys)
  (let ((buttons (mapcar #'(lambda (k)
			     (make-topic-button thing k))
			 button-keys))
	(gump (new 'talk-gump)))
      (prog1 gump (configure gump text buttons))))



