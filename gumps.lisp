(in-package :cypress)

(defresource (:name "oldania" :type :ttf :file "OldaniaADFStd-Regular.otf" :properties (:size 16)))

(defparameter *gump-font* "oldania")
(defparameter *gump-color* "saddle brown")

(defthing scroll-gump 
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

(defun split-into-pages (text)
  (let ((lines (split-string-on-lines text))
	(pages nil))
    (loop while lines do
      (if (<= (length lines) *lines-per-scroll*)
	  (progn 
	    (push lines pages)
	    (setf lines nil))
	  (progn 
	    (push (subseq lines 0 *lines-per-scroll*) pages)
	    (setf lines (subseq lines *lines-per-scroll*)))))
    (reverse pages)))

(define-method initialize scroll-gump (text)
  (initialize%super self)
  (setf %pages (split-into-pages text))
  (flip self 0))

(define-method tap scroll-gump (x y)
  (flip self))

(define-method alternate-tap scroll-gump (x y)
  (destroy self))

(define-method layout scroll-gump ()
  (resize self 
	  (* (image-width %image) *scroll-scale*)
	  (* (image-height %image) *scroll-scale*)))
  
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

(defparameter *letter-text* 
"Dear Geoffrey,

Our fondest hope is that this letter
finds you, and in good health. For the
message it contains will seem
predestined to break your Spirit.

Surely you noticed that the weather had
suddenly grown cold, and that the
vegetation had withered as if touched by
some sort of plague?

We failed to understand this ourselves,
until nightfall came and we saw that the
positions of the stars had changed so
much as to render several constellations
nearly unrecognizable. As Francis had
studied the ancients' commentaries on
Rama's maps of the Cosmos, we drew
charts of our own and found that ages
had passed since our departure from
Nothbess.

If we read these charts aright, then by
the time you read these words your
brother and I will have been dead for
more than five thousand years. For when
we crossed into Valisade, each of us
fell prey to magicks involving Time,
such that a delay in one day's departure
meant the passage of five millennia.

You stand on the soil of of a continent
ten thousand years older than the land
you left.
")


