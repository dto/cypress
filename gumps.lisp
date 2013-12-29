(in-package :cypress)

(defresource (:name "oldania" :type :ttf :file "OldaniaADFStd-Regular.otf" :properties (:size 16)))
(defresource (:name "oldania-title" :type :ttf :file "OldaniaADFStd-Regular.otf" :properties (:size 20)))

(defparameter *gump-font* "oldania")
(defparameter *gump-title-font* "oldania-title")
(defparameter *gump-color* '(#x52 #x2e #x00)) 

(defthing gump
  (target-x :initform 0)
  (target-y :initform 0))

(defmethod set-target ((self gump) x y)
  (with-fields (target-x target-y) self
    (setf target-x (- x (window-x)))
    (setf target-y (- y (window-y)))))

(defmethod move-to-target ((self gump))
  (with-fields (target-x target-y) self
    (move-to self 
	     (+ target-x (window-x))
	     (+ target-y (window-y)))))

(defmethod after-add-hook ((self gump))
  (set-target self (field-value :x self) (field-value :y self)))

(defmethod drag ((self gump) x y)
  (set-target self x y)
  (move-to self x y))

(defmethod run ((self gump))
  (arrange self))
      
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

(defthing (scroll-gump gump) 
  :image "scroll-gump.png"
  :image-scale 300
  :pages nil
  :page-number 0)

(defparameter *scroll-scale* (/ 1 3))

(defparameter *lines-per-scroll* 14)

(defmethod flip ((self scroll-gump) &optional p)
  (with-fields (pages page-number) self
    (setf page-number 
	  (mod (or p (1+ page-number))
	       (length pages)))))

(defmethod initialize ((self scroll-gump) &key text)
  (setf (field-value :pages self) 
	(split-into-pages text *lines-per-scroll*))
  (flip self 0))

(defmethod tap ((self scroll-gump) x y)
  (flip self))

(defmethod alternate-tap ((self scroll-gump) x y)
  (destroy self))
  
(defmethod draw ((self scroll-gump))
  (call-next-method)
  (with-fields (x y z height width page-number pages) self
    (let ((x0 (+ x (* 0.1 width)))
	  (y0 (+ y (* 0.18 height))))
      (let ((text-lines (nth page-number pages)))
	(loop while text-lines do
	  (draw-string (let ((line (pop text-lines)))
			 (if (plusp (length line))
			     line
			     " "))
		       x0 y0 :z z
			   :color *gump-color*
			   :font *gump-font*)
	  (incf y0 (font-height *gump-font*)))))))

(defmethod arrange ((self scroll-gump)) ()
  (with-fields (image) self
    (resize self 
	    (* (image-width image) *scroll-scale*)
	    (* (image-height image) *scroll-scale*))
    (move-to-target self)))

;;; The talk gump 

(defparameter *button-margin* 2)

(defthing button
  (method :initform nil)
  (target :initform nil)
  (label :initform "foo")
  (arguments :initform nil))

(defmethod initialize ((self button) &key label method target arguments font)
  (with-local-fields 
    (setf %label (format nil "* ~A " label)
	  %method method 
	  %target target 
	  %arguments arguments)))

(defmethod arrange ((self button))
  (with-fields (label) self
    (let ((font *gump-font*))
      (resize self 
	      (+ (* 2 *button-margin*)
		 (font-text-width label font))
	      (font-height font)))))

(defmethod can-pick ((self button)))
(defmethod pick ((self button)))

(defmethod draw ((self button))
  (with-fields (x y label) self
    (draw-string label (+ x *button-margin*) y :color *gump-color* :font *gump-font*)))

(defmethod tap ((self button) x y)
  (with-fields (target arguments method) self
    (when (xelfp target)
      (apply #'xelf:send method target arguments))))

(defun make-topic-button (target topic)
  (new 'button :label (pretty-string (make-keyword topic))
	       :method :discuss
	       :target target
	       :arguments (list topic)))

(defblock (scroll-text text))

(defmethod tap ((self scroll-text) x y)
  (with-fields (parent) self
    (flip parent)))

(defmethod alternate-tap ((self scroll-text) x y)
  (with-fields (parent) self
    (destroy parent)))

(defun make-talk-gump-text (data)
  (let ((text (new 'scroll-text :text data)))
    (prog1 text
      (set-font text *gump-font*)
      (set-background-color text nil)
      (set-read-only text t))))

(defparameter *lines-per-talk-gump* 3)

(defthing (talk-gump gump)
  (topic :initform nil)
   (pages :initform nil)
   (page-number :initform 0))

(defparameter *talk-gump-scale* (/ 1 2.3))

(defmethod initialize ((self talk-gump) &key)
  (with-local-fields
    (setf %inputs (list 
		   (make-talk-gump-text "hello")
		   (make-sentence nil)))
    (update-parent-links self)))

(defmethod flip ((self talk-gump) &optional p)
  (with-fields (inputs pages page-number) self
    (setf page-number 
	  (mod (or p (1+ page-number))
	       (length pages)))
    (setf (first inputs)
	  (make-talk-gump-text (nth page-number pages)))
    (update-parent-links self)))

(defmethod can-pick ((self talk-gump)) 
  t)

(defmethod pick ((self talk-gump))
  self)

(defmethod tap ((self talk-gump) x y)
  (flip self))

(defmethod alternate-tap ((self talk-gump) x y)
  (destroy self))
  
(defmethod buttons ((self talk-gump))
  (second (field-value :inputs self)))

(defmethod text ((self talk-gump))
  (first (field-value :inputs self)))

(defmethod replace-gump-text ((self talk-gump) (gump gump))
  (with-fields (pages) self 
    (setf pages (field-value :pages gump))
    (flip self 0)))

(defmethod insert-button ((self talk-gump) item)
  (unfreeze (buttons self))
  (accept (buttons self) item)
  (freeze (buttons self)))

(defmethod destroy-buttons ((self talk-gump))
  (with-local-fields 
    (mapc #'destroy (%inputs (buttons self)))
    (setf (%inputs (buttons self)) nil)))

(defmethod replace-buttons ((self talk-gump) items)
  (destroy-buttons self)
  (dolist (item items)
    (insert-button self item)))

(defmethod number-of-buttons ((self talk-gump))
  (length (%inputs (buttons self))))

(defmethod draw ((self talk-gump))
  (with-fields (x y width height inputs) self
    (draw-image "talk-scroll.png" x y :width width :height height)
    (mapc #'draw inputs)))

(defmethod arrange ((self talk-gump))
  (with-local-fields
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
	    (* (image-height "talk-scroll.png") *talk-gump-scale*))))

(defmethod configure ((self talk-gump) text buttons)
  (with-local-fields
    (setf %pages (split-into-pages text *lines-per-talk-gump*))
    (when buttons (replace-buttons self buttons))
    (flip self 0)))

(defun make-talk-gump (thing text &rest button-keys)
  (let ((buttons (mapcar #'(lambda (k)
			     (make-topic-button thing k))
			 button-keys))
	(gump (new 'talk-gump)))
      (prog1 gump (configure gump text buttons))))



