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
;; (defresource "title-ccl.png")
;; (defresource "title-jp.png")

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
  (setf *window-title* "cypress v0.8") 
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
    ;; (switch-to-buffer (new 'prologue))
    (setf *font* "oldania")
    (start-session)))

(defparameter *help-text* 
"Welcome to Cypress v0.8 (alpha) 

This is the Scroll of Helping. If you
click the scroll, you will advance to
the next page. Use the right mouse
button (or the Control key with the left
button) to close scrolls.

Right-click a destination to move
Geoffrey there.

Click an object to show its name.
Drag objects to move them.
Drag objects onto Geoffrey to take them.
 (Take and keep this scroll 
  for easy reference.)
Double-click an object to activate it.
Double-click a monster to attack it.
Double click Geoffrey for his inventory.
Drag items into/out of inventory scrolls.

Click spells in spellbook for
description.  Double click spells in
spellbook to cast.

Use the Travel spell to explore the
land.  Double-click land symbols to
explore regions.  If you die, press
Control-R for a new quest.

You must eat. Traveling and combat cause
hunger. Double-click food to eat it.
Use the \"Cure meat\" spell to make
jerky from freshly killed wolves.

Geoffrey must stay warm. He will get
colder by progressing through the
terrain (or by touching certain
objects). You have a magic tent and
campfire which you can use to heal and
warm yourself up. To use the tent, drag
it out of your inventory onto an open
space on the ground, and then cast
Spark.

There are several new keyboard shortcuts:

Press \"I\" to see Geoffrey's inventory.
Press \"S\" to open the spellbook.
Press \"M\" to open the travel map.

I hope you enjoy this work-in-progress
demonstration of Cypress.  Please submit
bug reports and feedback to me at
dto@blocky.io

-- David O'Toole
")

;;; cypress.lisp ends here
