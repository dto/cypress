;;; cypress.lisp --- the further evolution of dto rpg's

;; Copyright (C) 2010, 2011, 2012, 2013  David O'Toole

;; Author: David O'Toole <dto@ioforms.org>
;; Keywords: games

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :cypress)

(defresource "ancient-fanfare.ogg" :volume 10)
(defresource "presence.ogg" :volume 11)
(defresource "constellation.ogg" :volume 20)
(defresource "passageway.ogg" :volume 20)
(defresource "procession4.ogg" :volume 20)
(defresource "home.ogg" :volume 08)
(defresource "kosmium.ogg" :volume 10)
(defresource "battle-1.ogg" :volume 20) 
(defresource "believe-me2.ogg" :volume 20)
(defresource "3-against-2.ogg" :volume 20)
(defresource "xolaros3.ogg" :volume 20)
(defresource "dusk.ogg" :volume 20)
(defresource "monks.ogg" :volume 14)
(defresource "traveler2.ogg" :volume 13)
(defresource "lutey.ogg" :volume 20)
(defresource "tumbling.ogg" :volume 18)
(defresource "ruins.ogg" :volume 17)
(defresource "mountain.ogg" :volume 12)
(defresource "standing-by-the-river.ogg" :volume 10)
(defresource "spiritus.ogg" :volume 20)
(defresource "path.ogg" :volume 20)
(defresource "rain.ogg" :volume 20)
(defresource "crickets.ogg" :volume 10)
(defresource "xmrio.ogg" :volume 20)
(defresource "drum.ogg" :volume 20)
(defresource "flutism.ogg" :volume 20)

(defparameter *skip-meadow* nil)

(defparameter *soundtrack*
'("passageway.ogg" "home.ogg" "kosmium.ogg" "believe-me2.ogg" "xolaros3.ogg" "path.ogg"
  "3-against-2.ogg" "dusk.ogg" "ruins.ogg" "standing-by-the-river.ogg" "spiritus.ogg"))

(defparameter *movement-hint*  
"Right-click to move Geoffrey.")

(defparameter *object-hint*
"Left-click objects to identify them.
To take objects, left-click and drag
them onto Geoffrey.")

(defun make-quest (&optional (terrain-class 'meadow))
  (let ((geoffrey (new 'geoffrey))
	(buffer (new terrain-class)))
    (setf *lucius* nil)
    (setf *map-row* 0)
    (setf *map-column* 0)
    (setf *map-screen* nil)
    (setf *travel-direction* :downright)
    (with-buffer buffer
      (if *skip-meadow* 
	  (ildron)
	  (current-buffer)))))

;;; Opening story card 

(defthing opening-card :image "opening.png")

(defmethod initialize :after ((opening-card opening-card) &key)
  (resize opening-card *nominal-screen-width* *nominal-screen-height*)
  (move-to opening-card 0 (- *nominal-screen-height* 100)))
  
(defmethod update ((card opening-card))
  (with-fields (x y) card
    (when (plusp y)
      (move-to card x (- y 0.8)))))

(defmethod tap ((opening-card opening-card) x y)
  (magical-flourish)
  (load-scene (make-quest)))

(define-buffer opening 
  (quadtree-depth :initform 4))

(defmethod initialize :after ((opening opening) &key)
  (drop-object opening (new 'opening-card))
  (resize opening *nominal-screen-width* *nominal-screen-height*))

(defmethod tap ((opening opening) x y)
  (magical-flourish)
  (load-scene (make-quest)))

(defmethod scroll-tap ((opening opening) x y) nil)
(defmethod alternate-tap ((opening opening) x y) nil)

;;; Title screen

(defresource "title-sbcl.png")
(defresource "title-ccl.png")
;; (defresource "title-jp.png")

(defun title-screen-image () 
  #+sbcl "title-sbcl.png"
  #+ccl "title-ccl.png")

(define-buffer title 
  (quadtree-depth :initform 4)
  (background-image :initform (title-screen-image)))

(defmethod initialize :after ((title title) &key)
  (play-music "ancient-fanfare.ogg" :loop nil)
  (resize title *nominal-screen-width* *nominal-screen-height*))

(defmethod tap ((title title) x y)
  (magical-flourish)
  (at-next-update (switch-to-buffer (new 'movie))))

(defmethod scroll-tap ((title title) x y) nil)
(defmethod alternate-tap ((title title) x y) nil)

;;; Main game

(defun cypress (&optional (level 1))
  (setf *window-title* "Cypress v2.0")
  (setf *screen-width* 1280)
  (setf *screen-height* 720)
  (setf *nominal-screen-width* 1680)
  (setf *nominal-screen-height* 945)

  ;; (setf *nominal-screen-width* (* 1280 4))
  ;; (setf *nominal-screen-height* (* 720 4))

  (setf *scale-output-to-window* t) 
  (setf *use-antialiased-text* t)
  (setf *font-texture-filter* :mipmap)
  (setf *font-texture-scale* 2)
  (setf *default-texture-filter* :mipmap)
  (setf *frame-rate* 30)
  (setf *current-objective* nil)
  (setf *objectives* nil)
  (setf *events* nil)
  
  ;;  (setf *font* "sans-mono-bold-9") 
  (with-session 
    (load-project "cypress" '(:with-database nil))
    (index-all-images)
    (index-all-samples)
    (index-pending-resources)
    ;; (preload-resources)
    (switch-to-buffer (new 'title))
    ;; (switch-to-buffer (new 'movie))
    ;; (switch-to-buffer (new 'prologue))
    ;; (switch-to-buffer (new 'trailer))
    (setf *font* "oldania")
    (start-session)))

;;; cypress.lisp ends here
