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

;;; Title screen

(defresource "title-sbcl.png")
(defresource "title-ccl.png")
(defresource "title-jp.png")

(defun title-screen-image () 
  #+sbcl "title-sbcl.png"
  #+ccl "title-ccl.png")

(define-buffer title 
  (quadtree-depth :initform 4)
  (background-image :initform (title-screen-image)))

(defmethod initialize :after ((title title) &key)
  (resize title 1280 720))

(defmethod tap ((title title) x y)
  (switch-to-scene (make-quest)))

(defmethod scroll-tap ((title title) x y) nil)
(defmethod alternate-tap ((title title) x y) nil)

;;; Main game

(defun cypress (&optional (level 1))
  (setf *window-title* "cypress v0.6") 
  (setf *screen-width* 1280)
  (setf *screen-height* 720)
  (setf *nominal-screen-width* 1280)
  (setf *nominal-screen-height* 720)

  ;; (setf *nominal-screen-width* (* 1280 4))
  ;; (setf *nominal-screen-height* (* 720 4))

  (setf *scale-output-to-window* t) 
  (setf *use-antialiased-text* t)
  (setf *font-texture-filter* :linear)
  (setf *font-texture-scale* 2)
  (setf *default-texture-filter* :mipmap)
  (setf *frame-rate* 30)
  
  ;;  (setf *font* "sans-mono-bold-11") 
  (with-session 
    (load-project "cypress" '(:with-database nil))
    (index-all-images)
    (index-all-samples)
    (index-pending-resources)
    ;; (preload-resources)
    (switch-to-buffer (new 'title))
    (setf *font* "oldania")
    (start-session)))

;;; cypress.lisp ends here
