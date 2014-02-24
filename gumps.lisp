(in-package :cypress)

(defresource (:name "oldania" :type :ttf :file "OldaniaADFStd-Regular.otf" :properties (:size 16)))
(defresource (:name "oldania-bold" :type :ttf :file "OldaniaADFStd-Bold.otf" :properties (:size 16)))
(defresource (:name "oldania-title" :type :ttf :file "OldaniaADFStd-Regular.otf" :properties (:size 20)))

(defparameter *gump-font* "oldania")
(defparameter *gump-button-font* "oldania")
(defparameter *gump-title-font* "oldania-title")
(defparameter *gump-color* '(#x52 #x2e #x00)) 

(defthing gump
  (target-x :initform 0)
  (target-y :initform 0))

(defmethod get-target-position ((self gump))
  (with-local-fields (values %target-x %target-y)))

(defmethod set-target-position ((self gump) x y)
  (with-fields (target-x target-y) self
    (setf target-x (- x (window-x)))
    (setf target-y (- y (window-y)))))

(defmethod move-to-target-position ((self gump))
  (with-fields (target-x target-y) self
    (move-to self 
	     (+ target-x (window-x))
	     (+ target-y (window-y)))))

(defmethod drag ((self gump) x y)
  (set-target-position self x y)
  (move-to self x y)
  (bring-to-front self))

(defmethod run ((self gump))
  (arrange self))

;;; Cascading the gumps 

(defun find-gumps (&optional (buffer (current-buffer)))
  (with-fields (objects) buffer
    (loop for thing being the hash-values in objects
	  when (typep (find-object thing) (find-class 'gump))
	    collect (find-object thing))))

(defparameter *gump-cascade-offset* (units 1))
(defparameter *gump-cascade-size* (units 4))

(defun gump-cascade-position ()
  (let ((delta (* *gump-cascade-size* (length (find-gumps)))))
    (values
     (+ (window-x) *gump-cascade-offset* delta)
     (+ (window-y) (units 1) delta))))

(defmethod drop-object :after ((buffer buffer) (gump gump) &optional x y z)
  (set-target-position gump (field-value :x gump) (field-value :y gump))
  (bring-to-front gump))

(defmethod add-object :after ((buffer buffer) (self gump) &optional x y z)
  (bring-to-front self))

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
  (play-sample "ok.wav")
  (flip self))

(defmethod can-pick ((self scroll-gump)) 
  t)

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
    (with-fields (quantity) target
      (let ((image (or (field-value :contained-image target)
		       (field-value :image target))))
	(let ((image-width (image-width image))
	      (image-height (image-height image)))
	  ;; fit to square without distorting
	  (if (> image-width image-height)
	      (draw-image image x y 
			  :width width
			  :height (- height 
				     (* height (/ width image-width))))
	      (draw-image image x y
			  :height height
			  :width (- width (* width (/ height image-height)))))
	  ;; now possibly draw quantity
	  (when (> quantity 1)
	    (draw-string (format nil "~S" quantity)
			 (+ x *icon-width* (- (units 1)))
			 (+ y *icon-width* (- (units 0.5)))
			 :color "saddle brown"
			 :font "oldania-bold")))))))

(defparameter *icon-spacing* (units 1))

(defmethod icon-drop ((icon icon))
  (with-fields (target) icon
    (with-fields (container inventory stacking) target
      (if (or inventory (not stacking))
	  ;; don't split up containers or non-stackable items.
	  (prog1 target 
	    (remove-inventory-item container target))
	  ;; consume single quantity, splitting if needed
	  (let ((class (class-name (class-of target))))
	    (consume-single container class))))))

;;; Container browser gump

(defparameter *browser-top-margin* (units 5.2))

(defparameter *browser-left-margin* (units 2.3))

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
  (with-fields (target) browser
    (can-accept target)))

(defmethod will-accept ((browser browser) (thing thing))
  (with-fields (target) browser
    (if (eq thing target)
	nil
	(will-accept target thing))))

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
    (with-fields (inventory) target
      ;; only take max items
      (let ((items (subseq inventory
			   0 (min *maximum-inventory-size* (length inventory)))))
	(setf icons (mapcar #'item-icon items)))
      (layout browser))))

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
      (let ((old-bubble (find-bubble)))
	(when old-bubble 
	  (destroy old-bubble))
	(drop-object (current-buffer)
		     (new 'bubble 
			  :text (find-description 
				 (field-value :target icon)))
		     (+ (units 2) x)
		     y)))))

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
  (block checking
    (with-fields (target) browser
      (let ((x (window-pointer-x))
	    (y (window-pointer-y)))
	(let ((icon (hit-icons browser x y)))
	  (if icon
	      (let ((drop (icon-drop icon)))
		(refresh browser)
		(move-to drop x y)
		drop)
	      browser))))))

;;; Conversation gump 

(defparameter *button-margin* 2)

(defthing button
  (target :initform nil)
  (label :initform "foo")
  (arguments :initform nil))

(defmethod initialize ((self button) &key label method target arguments font)
  (with-local-fields 
    (setf %label (format nil "* ~A  " label)
	  %target target 
	  %arguments arguments)))

(defmethod arrange ((self button))
  (with-fields (label) self
    (when label
      (let ((font *gump-button-font*))
	(resize self 
		(+ (* 2 *button-margin*)
		   (font-text-width label font))
		(font-height font))))))

(defmethod layout ((self button))
  (arrange self))

(defmethod can-pick ((self button)))
(defmethod pick ((self button)))

(defmethod draw ((self button))
  (with-fields (x y label) self
    (draw-string label (+ x *button-margin*) y :color *gump-color* :font *gump-button-font*)))

(defmethod tap ((self button) x y)
  (with-fields (target arguments method) self
    (when (xelfp target)
      (discuss target (first arguments)))))

(defun make-topic-button (target topic)
  (new 'button :label (pretty-string (make-keyword topic))
	       :target target
	       :arguments (list topic)))

(defblock (scroll-text text))

(defmethod tap ((self scroll-text) x y)
  (with-fields (parent) self
    (flip (find-object parent))))

(defmethod alternate-tap ((self scroll-text) x y)
  (with-fields (parent) self
    (destroy (find-object parent))))

(defun make-talk-gump-text (data)
  (let ((text (new 'scroll-text :text (list data))))
    (prog1 text
      (set-font text *gump-font*)
      (set-background-color text nil)
      (set-read-only text t))))

(defparameter *lines-per-talk-gump* 4)

(defthing (talk-gump gump)
  (image :initform "talk-scroll.png")
  (target :initform nil)
  (topic :initform nil)
  (pages :initform nil)
  (page-number :initform 0))

(defparameter *talk-gump-scale* (/ 1 2))

(defmethod initialize ((self talk-gump) &key)
  (with-local-fields
    ;; dummy contents
    (setf %inputs (list 
		   (make-talk-gump-text "hello")
		   (make-sentence nil)))
    (update-parent-links self)))

(defmethod flip ((self talk-gump) &optional p)
  (with-fields (inputs pages page-number) self
    (setf page-number 
	  (mod (or p (1+ page-number))
	       (length pages)))
    (message "page number ~S" page-number)
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
  (with-fields (x y target image width height inputs) self
    (draw-image image x y :width width :height height)
    (mapc #'draw inputs)))

(defmethod arrange ((self talk-gump))
  (with-local-fields
    (move-to-target-position self)
    (when (xelfp (text self))
      (let ((x0 (+ %x (* 0.14 %width)))
	    (y0 (+ %y (* 0.18 %height))))
	(resize (text self) %width (units 6))
	(move-to (text self) x0 y0)
	(move-to (buttons self) x0 (+ y0 (units 7.5)))
	(layout (buttons self))))
    (resize self 
	    (* (image-width %image) *talk-gump-scale*)
	    (* (image-height %image) *talk-gump-scale*))))

(defmethod configure ((self talk-gump) text buttons target)
  (with-local-fields
    (setf %target target)
    (setf %pages (split-into-pages text *lines-per-talk-gump*))
    (when buttons (replace-buttons self buttons))
    (flip self 0)))

;;; Conversation

(defun make-talk-gump (thing text &rest button-keys)
  (let ((buttons (mapcar #'(lambda (k)
			     (make-topic-button thing k))
			 button-keys))
	(gump (new 'talk-gump)))
      (prog1 gump (configure gump text buttons thing))))

(defmethod topic-content ((self thing) topic))

(defmacro define-topic (name super &body forms)
  `(defmethod topic-content ((self ,super) (topic (eql ,(make-keyword name))))
     (list ,@forms)))

(defmethod get-topic-gump ((self thing) topic)
  (let ((content (topic-content self topic)))
    (when (first content)
      (destructuring-bind (text &rest keys) content
	(apply #'make-talk-gump self text keys)))))

(defmethod discuss ((self thing) topic)
  (with-fields (gump width x y) self
    (let ((new-gump (get-topic-gump self topic)))
      (if new-gump 
	  (progn (replace-gump self new-gump)
		 (set-target-position gump (+ x width 2) y))
	  (destroy-gump self)))))
		
	
      

