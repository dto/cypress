(in-package :cypress)

(defresource (:name "oldania" :type :ttf :file "OldaniaADFStd-Regular.otf" :properties (:size 16)))
(defresource (:name "oldania-bold" :type :ttf :file "OldaniaADFStd-Bold.otf" :properties (:size 16)))
(defresource (:name "oldania-title" :type :ttf :file "OldaniaADFStd-Regular.otf" :properties (:size 20)))

(defparameter *gump-font* "oldania")
(defparameter *gump-title-font* "oldania-title")
(defparameter *gump-color* '(#x52 #x2e #x00)) 

(defthing gump
  (target-x :initform 0)
  (target-y :initform 0))

(defmethod set-target-position ((self gump) x y)
  (with-fields (target-x target-y) self
    (setf target-x (- x (window-x)))
    (setf target-y (- y (window-y)))))

(defmethod move-to-target-position ((self gump))
  (with-fields (target-x target-y) self
    (move-to self 
	     (+ target-x (window-x))
	     (+ target-y (window-y)))))

(defmethod drop-object :after ((buffer buffer) (gump gump) &optional x y z)
  (set-target-position gump (field-value :x gump) (field-value :y gump))
  (bring-to-front gump))

(defmethod drag ((self gump) x y)
  (set-target-position self x y)
  (move-to self x y)
  (bring-to-front self))

(defmethod run ((self gump))
  (arrange self))

;;; Right click to close gumps

(defmethod alternate-tap ((self gump) x y)
  (play-sample "close.wav")
  (destroy self))
       
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
    (move-to-target-position self)))

;;; Generic object icons representing items in inventories.

(defparameter *icon-height* 60)
(defparameter *icon-width* 60)

(defthing icon target)

(defmethod initialize ((icon icon) &key target)
  (with-fields (height width) icon
    (setf height *icon-height*)
    (setf width *icon-width*)
    (setf (field-value :target icon) target)))

(defmethod can-pick ((icon icon))
  (can-pick (field-value :target icon)))

(defmethod draw ((icon icon))
  (with-fields (x y height width target) icon
    (with-fields (image) target
      (let ((image-width (image-width image))
	    (image-height (image-height image)))
      ;; fit to square without distorting
	(if (> image-width image-height)
	    (draw-image image x y 
			:width width
			:height (- height (/ 1 (/ width image-width))))
	    (draw-image image x y
			:height height
			:width (- width (/ 1 (/ height image-height)))))))))

(defparameter *icon-spacing* (units 1))

;;; Container browser gump

(defparameter *browser-top-margin* (units 5.5))

(defparameter *browser-left-margin* (units 2.0))

(defthing (browser gump) 
  :image "scroll-gump.png"
  :image-scale 300
  :target nil
  :icons nil
  :rows 4
  :columns 4)

;; Don't allow browsers to be dropped into other inventories

(defmethod accept ((container thing) (gump gump)) nil)

(defmethod will-accept ((container thing) (gump gump)) nil)

(defparameter *browser-scale* (/ 1 3))

(defmethod run ((browser browser))
  (arrange browser))

(defmethod clear ((browser browser))
  (with-fields (icons) browser
    (mapc #'destroy icons)
    (setf icons nil)))

(defmethod can-accept ((browser browser))
  (can-accept (field-value :target browser)))

(defmethod accept ((browser browser) thing)
  (accept (field-value :target browser) thing)
  (refresh browser))

(defmethod arrange ((browser browser))
  ;; stay in same spot onscreen
  (with-fields (image) browser
    (resize browser 
	    (* (image-width image) *browser-scale*)
	    (* (image-height image) *browser-scale*))
    (move-to-target-position browser))
  ;; lay out the items
  (with-fields (x y rows columns) browser
    (let ((x0 (+ x *browser-left-margin*))
	  (y0 (+ y *browser-top-margin*))
	  (icons (field-value :icons browser)))
      (block spacing
	(dotimes (row rows)
	  (dotimes (column columns)
	    (let ((icon (pop icons)))
	      (if icon
		  (move-to icon 
			   (+ x0 
			      (* column (+ *icon-width* *icon-spacing*)))
			   (+ y0 
			      (* row (+ *icon-height* *icon-spacing*))))
		  (return-from spacing)))))))))

(defmethod draw ((self browser))
  (call-next-method)
  (with-fields (x y icons target) self
    (mapc #'draw icons)
    (draw-string (find-description target)
		 (+ x (units 7))
		 (+ y (units 1.5))
		 :color "saddle brown" 
		 :font "oldania-title")))

(defmethod destroy :before ((self browser))
  (mapc #'destroy (field-value :icons self)))

(defun item-icon (item)
  (new 'icon :target item))

(defmethod refresh ((browser browser))
  (clear browser)
  (with-fields (target icons rows columns) browser
    (with-fields (inventory spells) target
      (let ((things (append spells inventory)))
	;; only take max items
	(let ((items (subseq things
			     0 (min *maximum-inventory-size* (length things)))))
	  (setf icons (mapcar #'item-icon items)))))
    (layout browser)))

(defmethod initialize ((browser browser) &key container)
  (with-fields (target) browser
    (setf target container)
    (refresh browser)))

(defmethod hit-icons ((browser browser) x y)
  (with-fields (icons) browser
    (labels ((try (icon)
	       (hit icon x y)))
      (some #'try icons))))

(defmethod tap :after ((browser browser) x y)
  (let ((icon (hit-icons browser x y)))
    (when icon 
      (drop-object (current-buffer)
		   (new 'bubble 
			:text (find-description 
			       (field-value :target icon)))
		   (+ (units 2) x)
		   y))))

(defmethod look ((browser browser)))

(defmethod activate ((browser browser))
  (let ((x (window-pointer-x))
	(y (window-pointer-y)))
    (let ((icon (hit-icons browser x y)))
      (when icon 
	(activate (field-value :target icon))
	(refresh browser)))))

(defmethod can-pick ((browser browser))
  (let ((x (window-pointer-x))
	(y (window-pointer-y)))
    (let ((icon (hit-icons browser x y)))
      (if icon (can-pick icon) browser))))

(defmethod pick ((browser browser))
  (with-fields (target) browser
    (let ((x (window-pointer-x))
	  (y (window-pointer-y)))
      (let ((icon (hit-icons browser x y)))
	(if icon
	    (let ((item (field-value :target icon)))
	      (prog1 item
		(move-to item x y)
		(remove-inventory-item target item)
		(refresh browser)))
	    browser)))))

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
    (move-to-target-position self)
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

