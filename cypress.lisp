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

(eval-when (:load-toplevel) 
  (setf *window-title* "cypress v0.1")
  (setf *default-texture-filter* :nearest)
  (setf *use-antialiased-text* nil)
  (setf *current-directory*
	(make-pathname
	 :directory (pathname-directory #.#P"./"))))

;;; Main program

(defun cypress (&optional (level 1))
  (setf *window-title* "cypress v0.1")
  (setf *screen-width* 1280)
  (setf *screen-height* 720)
  (setf *nominal-screen-width* 1280)
  (setf *nominal-screen-height* 720)
  ;; zoomout 
;;  (setf *nominal-screen-width* (* 1280 5))
;;  (setf *nominal-screen-height* (* 720 5))
  ;;
  (setf *scale-output-to-window* t) 
  (setf *default-texture-filter* :mipmap)
  (setf *use-antialiased-text* t)

  (setf *frame-rate* 30)
  
  (disable-key-repeat) 
  
  (setf *font* "sans-mono-bold-11") 
  (with-session 
      (load-project "cypress" '(:with-database nil))

    (index-all-images)
    (index-all-samples)
    (preload-resources)
    (index-pending-resources)

    (switch-to-buffer (make-meadow))
    (start-session)))

(define-buffer cypress
  (bubble :initform nil)
  (retrying :initform nil))
  ;; (default-events 
  ;;    :initform
  ;;    '(((:r :control) :reset-game)
  ;;      ((:q :control) :quit-game)
  ;;      ;; ((:y :control) :show-waypoint)
  ;;      ((:h :control) :help)
  ;;      ;; ((:m :control) :toggle-music) 
  ;;      ;; ((:p :control) :toggle-pause)
  ;;      ((:leftbracket) :toggle-red-green-color-blindness)
  ;;      ((:j :control) :toggle-joystick)
  ;;      ;;       ((:f8) :cheat)
  ;;      ((:n :control) :next-joystick)
  ;;      ;;
  ;;      ;; ((:x :alt) :command-prompt)
  ;;      ;; ((:g :control) :escape)
  ;;      ((:f6 :control) :regenerate))))
  ;;      ;;
  ;;      ;; ((:x :alt) :command-prompt)
  ;;      ;; ((:x :control) :edit-cut)
  ;;      ;; ((:c :control) :edit-copy)
  ;;      ;; ((:v :control) :edit-paste)
  ;;      ;; ((:v :control :shift) :paste-here)
  ;;      ;; ((:f9) :toggle-minibuffer)
  ;;      ;; ((:f12) :transport-toggle-play)
  ;;      ;; ((:g :control) :escape)
  ;;      ;; ((:d :control) :drop-selection))))

;; ;;; Disable mouse editing

;; (defun set-buffer-bubble (bubble)
;;   (setf (field-value :bubble (current-buffer)) bubble))

;; (define-method handle-point-motion cypress (x y))
;; (define-method press cypress (x y &optional button))
;; (define-method release cypress (x y &optional button))
;; (define-method tap cypress (x y))
;; (define-method alternate-tap cypress (x y))
      
;;; cypress.lisp ends here
