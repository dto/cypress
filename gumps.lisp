(in-package :cypress)

(defresource (:name "oldania" :type :ttf :file "OldaniaADFStd-Regular.otf" :properties (:size 20)))
(defresource (:name "oldania-bold" :type :ttf :file "OldaniaADFStd-Bold.otf" :properties (:size 20)))
(defresource (:name "oldania-subtitle" :type :ttf :file "OldaniaADFStd-Bold.otf" :properties (:size 26)))
(defresource (:name "oldania-italic" :type :ttf :file "OldaniaADFStd-Italic.otf" :properties (:size 16)))
(defresource (:name "oldania-title" :type :ttf :file "OldaniaADFStd-Regular.otf" :properties (:size 22)))

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
  (find-instances buffer 'gump))

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

;;; Utility function

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

;;; Displaying tooltip hints

(defthing (hint gump)
  :image "talk-scroll.png"
  :image-scale 600
  :timer (seconds->frames 10)
  :lines nil)

(defresource "hint.wav" :volume 6)

(defmethod initialize ((self hint) &key text)
  (setf (field-value :lines self) 
	(split-string-on-lines text)))
  
(defparameter *hint-scale* (/ 1 1.8))

(defmethod arrange :after ((self hint))
  (with-fields (image width) self
    (resize self 
	    (* (image-width image) *hint-scale*)
	    (* (image-height image) *hint-scale*))
    (move-to self 
	     (+ (window-x) (units 70))
	     (+ (window-y) (units 1)))))

(defmethod run ((self hint))
  (with-fields (timer) self
    (decf timer)
    (unless (plusp timer)
      (destroy self))))

(defmethod draw ((self hint))
  (call-next-method)
  (with-fields (x y width height image lines) self
    (let ((x0 (+ x (* 0.15 width)))
	  (y0 (+ y (* 0.19 height))))
      (let ((text-lines lines))
	(loop while text-lines do
	  (draw-string (let ((line (pop text-lines)))
			 (if (plusp (length line))
			     line
			     " "))
		       x0 y0 
			   :color *gump-color*
			   :font *gump-font*)
	  (incf y0 (font-height *gump-font*)))))))

(defun show-hint (text)
  (with-fields (hints) (geoffrey)
    (unless (find text hints :test 'equal)
      (play-sample "hint.wav")
      (push text hints)
      (drop-object (current-buffer)
		   (new 'hint :text text)))))

(defmethod tap ((hint hint) x y)
  (destroy hint))

;;; The scroll gump is for reading pages of text.

(defthing (scroll-gump gump) 
  :image "scroll-gump.png"
  :image-scale 300
  :pages nil
  :page-number 0)

(defparameter *scroll-scale* (/ 1 2.2))

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

(defmethod draw :after ((self scroll-gump))
  (with-fields (x y page-number pages) self
    (unless (= page-number (- (length pages) 1))
      (draw-string "continued..."
		   (+ x (units 20))
		   (+ y (units 32))
		   :color *gump-color*
		   :font "oldania-italic"))))

(defmethod arrange ((self scroll-gump)) ()
  (with-fields (image) self
    (resize self 
	    (* (image-width image) *scroll-scale*)
	    (* (image-height image) *scroll-scale*))
    (move-to-target-position self)))

;;; Generic object icons representing items in inventories.

(defparameter *icon-height* 85)
(defparameter *icon-width* 85)

(defthing icon target bag)

(defmethod initialize ((icon icon) &key target container)
  (with-fields (height width bag) icon
    (setf bag container)
    (setf height *icon-height*)
    (setf width *icon-width*)
    (setf (field-value :target icon) target)))

(defmethod can-pick ((icon icon))
  (can-pick (field-value :target icon)))

(defmethod can-accept ((icon icon))
  (with-fields (target bag) icon
    (or	(can-accept target)
	(can-accept bag))))

(defmethod accept ((icon icon) (thing thing))
  (with-fields (target bag) icon
    (if	(can-accept target)
	(accept target thing)
	(accept bag thing))))

(defmethod accept :after ((icon icon) thing)
  (mapc #'refresh (find-gumps)))

(defmethod will-accept ((icon icon) (thing thing))
  (with-fields (target bag) icon
    (or	(will-accept target thing)
	(will-accept bag thing))))

(defmethod draw ((icon icon))
  (with-fields (x y height width target) icon
    (with-fields (quantity equipper) target
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
	  ;; now possibly draw quantity or Equipped indicator
	  (let (label-string)
	    (when (> quantity 1)
	      (setf label-string (format nil "~S" quantity)))
	    (when equipper 
	      (setf label-string "Eq"))
	    (when label-string
	      (draw-string label-string
			   (+ x *icon-width* (- (units 1)))
			   (+ y *icon-width* (- (units 0.5)))
			   :color "saddle brown"
			   :font "oldania-bold"))))))))

(defparameter *icon-spacing* (units 1))

(defmethod icon-drop ((icon icon))
  (with-fields (target) icon
    (with-fields (container inventory stacking) target
      (if (null container)
	  (prog1 nil (message "Warning; attempt to get icon drop for non-contained object ~A" target))
	  (if (or inventory (not stacking))
	      ;; don't split up containers or non-stackable items.
	      (prog1 target 
		(remove-inventory-item container target))
	      ;; consume single quantity, splitting if needed
	      (let ((class (class-name (class-of target))))
		(consume-single container class target)))))))

;;; Container browser gump

(defparameter *browser-top-margin* (units 6.4))

(defparameter *browser-left-margin* (units 2.6))

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

(defparameter *browser-scale* (/ 1 2.2))

(defmethod run ((browser browser))
  (arrange browser))

(defmethod clear ((browser browser))
  (with-fields (icons) browser
    (mapc #'destroy icons)
    (setf icons nil)))

;; (defmethod can-accept ((browser browser))
;;   (with-fields (target) browser
;;     (let ((icon (hit-icons browser (window-pointer-x) (window-pointer-y))))
;;       (if icon 
;; 	  (or (can-accept icon)
;; 	      (can-accept target))
;; 	  (can-accept target)))))

(defmethod will-accept ((browser browser) (thing thing))
  (with-fields (target) browser
    (let ((icon (hit-icons browser (window-pointer-x) (window-pointer-y))))
      (if icon
	  (will-accept icon thing)
	  (when (can-reach target (geoffrey))
	    (if (eq thing target)
		nil
		(will-accept target thing)))))))

(defmethod accept ((browser browser) thing)
  (with-fields (target) browser
    (let ((icon (hit-icons browser (window-pointer-x) (window-pointer-y))))
      (if (and icon (will-accept icon thing))
	  (accept icon thing)
	  (accept target thing))
      (refresh browser))))

(defmethod accept :after ((browser browser) thing)
  (mapc #'refresh (find-gumps)))

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
		 (+ x (units 7.7))
		 (+ y (units 2.2))
		 :color "saddle brown" 
		 :font "oldania-title")))

(defmethod draw :after ((self browser))
  (with-fields (x y target) self
    (when (> (length (inventory-items target)) 16)
      (draw-string "overfull; some items hidden."
		   (+ x (units 15.5))
		   (+ y (units 2.3))
		   :color "saddle brown"
		   :font "oldania-bold"))))

(defmethod destroy :before ((self browser))
  (mapc #'destroy (field-value :icons self)))

(defun item-icon (item container)
  (new 'icon :target item :container container))

(defmethod refresh ((browser browser))
  (clear browser)
  (with-fields (target icons rows columns) browser
    (with-fields (inventory) target
      ;; only take max items
      (let ((items (subseq inventory
			   0 (min *maximum-inventory-size* (length inventory)))))
	(setf icons (mapcar #'(lambda (x) (item-icon x target)) items)))
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

(defmethod activate-maybe ((browser browser))
  (activate browser))

(defmethod activate ((browser browser))
  (let ((x (window-pointer-x))
	(y (window-pointer-y)))
    (let ((icon (hit-icons browser x y)))
      (when icon 
	(activate (field-value :target icon))
	(refresh browser)))))

(defmethod can-pick ((browser browser))
  (let ((x (window-pointer-x))
	(y (window-pointer-y))
	(target (field-value :target browser)))
    (let ((icon (hit-icons browser x y)))
      (if icon 
	  (if 
	   (or (eq target (geoffrey))
	       (can-reach target (geoffrey)))
	   (can-pick icon)
	   (prog1 nil (show-error browser)))
	  ;; you should always be able to move the gump
	  browser))))

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

(defmethod handle-text-event ((self scroll-text) event) nil)

(defmethod tap ((self scroll-text) x y)
  (with-fields (parent) self
    (flip (find-object parent))))

(defmethod alternate-tap ((self scroll-text) x y)
  (with-fields (parent) self
    (with-fields (target) parent
      (destroy-gump target))))

(defun make-talk-gump-text (data)
  (let ((text (new 'scroll-text :text (list data))))
    (prog1 text
      (set-font text *gump-font*)
      (set-foreground-color text "saddle brown")
      (set-background-color text nil)
      (set-read-only text t))))

(defparameter *lines-per-talk-gump* 10)

(defthing (talk-gump gump)
  (image :initform "scroll-gump.png")
  (target :initform nil)
  (topic :initform nil)
  (pages :initform nil)
  (page-number :initform 0))

(defparameter *talk-gump-scale* (/ 1 2.2))

(defmethod initialize ((self talk-gump) &key)
  (with-local-fields
    ;; dummy contents
    (setf %inputs (list 
		   (make-talk-gump-text "hello")
		   (make-sentence nil)))
    (update-parent-links self)))

(defmethod destroy :before ((self talk-gump))
  (with-fields (target) self
    (setf (field-value :gump target) nil)))

(defmethod flip ((self talk-gump) &optional p)
  (with-fields (inputs pages page-number) self
    (setf page-number 
	  (mod (or p (1+ page-number))
	       (length pages)))
    (setf (first inputs)
	  (make-talk-gump-text (nth page-number pages)))
    (update-parent-links self)))

(defmethod more-p ((self talk-gump))
  (with-fields (pages page-number) self
    (< page-number (1- (length pages)))))

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
  (mapc #'destroy (field-value :inputs (buttons self)))
  (setf (field-value :inputs (buttons self)) nil))

(defmethod replace-buttons ((self talk-gump) items)
  (destroy-buttons self)
  (dolist (item items)
    (insert-button self item)))

(defmethod number-of-buttons ((self talk-gump))
  (length (%inputs (buttons self))))

(defmethod draw ((self talk-gump))
  (with-fields (x y target image width height inputs) self
    (draw-image image x y :width width :height height)
    (when (more-p self)
      (draw-string "(continued...)" 
		   (+ x (units 20))
		   (+ y (units 25))
		   :color *gump-color*
		   :font *gump-font*))
    (draw (buttons self))
    (draw (text self))))

(defmethod arrange ((self talk-gump))
  (with-local-fields
    (move-to-target-position self)
    (when (xelfp (text self))
      (let ((x0 (+ %x (* 0.08 %width)))
	    (y0 (+ %y (* 0.17 %height))))
	(resize (text self) %width (units 10))
	(move-to (text self) x0 y0)
	(move-to (buttons self) x0 (+ y0 (units 24)))
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
			 ;;(append (field-value :topics thing)
				 button-keys))
	(gump (new 'talk-gump)))
      (prog1 gump (configure gump text buttons thing))))

(defun configure-talk-gump (gump thing content)
  (destructuring-bind (text &rest button-keys) content
    (let ((buttons (mapcar #'(lambda (k)
			     (make-topic-button thing k))
			   ;; (append (field-value :topics thing)
				 button-keys)))
      (prog1 gump (configure gump text buttons thing)))))

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
  (with-fields (width x y) self
    (let ((gump (get-gump self))
	  (content (topic-content self topic)))
      (if (and (first content)
	       (typep gump 'talk-gump))
	  ;; configure in place
	  (configure-talk-gump gump self content)
	  ;; possibly replace other gump
	  (let ((new-gump (get-topic-gump self topic)))
	    (destroy-gump self)
	    (setf (field-value :gump self) nil)
	    (when new-gump 
	      ;; we got a new gump for this topic.
	      (replace-gump self new-gump)))))))

(defmethod discuss :after ((self thing) topic)
  (resume))

(defmethod after-revive ((gump gump))
  (destroy gump))
