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

(defresource "wood.wav" :volume 40)
(defresource "growl-1.wav" :volume 60)
(defresource "growl-2.wav" :volume 60)
(defresource "unh-1.wav" :volume 20)
(defresource "unh-2.wav" :volume 20)
(defresource "unh-3.wav" :volume 20)
(defresource "howl.wav" :volume 50)
(defresource "knock.wav" :volume 50)
(defresource "bow.wav" :volume 40)
(defresource "death.wav" :volume 40)
(defresource "lichscream.wav" :volume 60)
(defresource "lichdie.wav" :volume 60)
(defresource "dead.wav" :volume 40)

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
  (setf *font-texture-filter* :linear)
  (setf *font-texture-scale* 2)
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

;;; cypress.lisp ends here
