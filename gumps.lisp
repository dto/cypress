(in-package :cypress)

(defresource (:name "oldania" :type :ttf :file "OldaniaADFStd-Regular.otf" :properties (:size 16)))

(defparameter *gump-font* "oldania")
(defparameter *gump-color* "saddle brown")

(defthing scroll-gump 
  :image "scroll-gump.png"
  :image-scale 300
  :text-lines nil
  :target nil)

(defparameter *scroll-scale* (/ 1 3))

(defparameter *lines-per-scroll* 14)

(define-method initialize scroll-gump (text)
  (initialize%super self)
  (setf %text-lines (split-string-on-lines text)))

(define-method layout scroll-gump ()
  (resize self 
	  (* (image-width %image) *scroll-scale*)
	  (* (image-height %image) *scroll-scale*)))
  
(define-method draw scroll-gump ()
  (draw%super self)
  (with-fields (x y z height width) self
    (let ((x0 (+ x (* 0.1 width)))
	  (y0 (+ y (* 0.18 height))))
      (let ((text-lines %text-lines))
	(loop while text-lines do
	  (draw-string (let ((line (pop text-lines)))
			 (if (plusp (length line))
			     line
			     " "))
		       x0 y0 :z z
			   :color *gump-color*
			   :font *gump-font*)
	  (incf y0 (font-height *gump-font*)))))))

(defparameter *letter-text* 
"Dear Geoffrey,

Our fondest hope is that this letter
finds you, and in good health. For the
message it contains will seem
predestined to break your Spirit.

If we understand this situation aright,
then by the time you read these words,
your brother and I will have been dead
for more than five thousand years. For
when we crossed into Valisade, each of
us fell prey to magicks involving
Time. You must now be realizing...")

