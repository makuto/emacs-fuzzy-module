;;; macoyFuzzy.el --- fuzzy module integration for ido

;; Copyright © 2018 Macoy Madson

;; Author: Macoy Madson
;; Maintainer: Macoy Madson
;; Description: Module fuzzy search integration for ido
;; Version: 0.1
;; URL: https://github.com/makuto/emacs-fuzzy-modulel

;; This file is NOT part of GNU Emacs.

;;; License

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; This package provides a native alternative to `ido-mode''s
;; built-in flex matching.

;;; Acknowledgments

;; Forrest Smith wrote a fuzzy matching algorithm. I took some inspiration from his implementation
;; flx-ido.el is what I looked at to know how to integrate my module into ido.

;;; Installation:

;; You must have compiled your Emacs with --with-modules for it to support modules.
;; Add the following code to your init file:
;;     (setq macoy-fuzzy-library-location "path/to/macoyFuzzy[.dll|.so]")
;;     (require 'macoy-fuzzy-ido)
;;     (ido-mode 1)
;;     (ido-everywhere 1)
;;     (macoy-fuzzy-ido-mode 1)

;;; Code:

(when macoy-fuzzy-library-location
  (module-load macoy-fuzzy-library-location)
  )

(require 'macoy-fuzzy)
;; TODO duplicate and remove
(require 'flx-ido)
(require 'flx)

(defcustom macoy-flx-ido-use-faces t
  "Use `flx-highlight-face' to indicate characters contributing to best score."
  :type 'boolean
  :group 'ido)

(define-minor-mode macoy-fuzzy-ido-mode
  "Toggle Macoy fuzzy mode"
  :init-value nil
  :lighter "MacoyFuzzy"
  :group 'ido
  :global t)

;; TODO Remove flx-ido dependency
(defadvice ido-exit-minibuffer (around macoy-fuzzy-ido-reset activate)
  "Remove flx properties after."
  (let* ((obj (car ido-matches))
         (str (if (consp obj)
                  (car obj)
                obj)))
    (when (and macoy-fuzzy-ido-mode str)
      (remove-text-properties 0 (length str)
                              '(face flx-highlight-face) str)))

  ad-do-it)

;; (defun macoy-flx-ido-decorate (things &optional clear)
;;   "Add ido text properties to THINGS.
;; If CLEAR is specified, clear them instead."
;;   (if macoy-flx-ido-use-faces
;;       (let ((decorate-count (min ido-max-prospects
;;                                  (length things))))
;;         (nconc
;;          (cl-loop for thing in things
;;                for i from 0 below decorate-count
;;                collect (if clear
;;                            (flx-propertize thing nil)
;; 						 (flx-propertize thing 1
;; 						 )
;; 						 ;; (message thing)
;; 						 )
;; 			   )
;;          (if clear
;;              (nthcdr decorate-count things)
;;         (mapcar 'car (nthcdr decorate-count things)))))
;;     (if clear
;;         things
;;       (mapcar 'car things)))
;;   )


;; (defun macoy-flx-ido-decorate (things &optional clear)
;;   "Add ido text properties to THINGS.
;; If CLEAR is specified, clear them instead."
;;   (cl-loop for thing in things
;;            for i from 0 below 10
;;            do (
;; 			   flx-propertize (car (list thing 1)) 1
;; 			   ;; message thing
;; 			   )
;;         )
;;   )

(defun macoy-filter-list-fuzzy-ido (query items)
  (if (zerop (length query))
      (nreverse items) ;; Reverse because the history is in reverse
	;; By setting t we're telling macoy-filter-list-fuzzy this list
	;;  is in reverse order of recently selected

	;; LEFT OFF: Need to tack on scores for flx-propertize just like in flx-ido-match-internal. Save a copy for the normal return though

	
    (let ((filtered-list
		   (macoy-filter-list-fuzzy query (vconcat original-items) t)))
	  ;; (let* ((matches (cl-loop for item in filtered-list
      ;;                            for string = (ido-name item)
      ;;                            collect item
      ;;                            into matches
      ;;                            finally return matches)))
      ;;   (macoy-flx-ido-decorate matches)
	  ;; 	)
	  filtered-list
    )
  ))

;; When set, use this cached list instead of whatever ido is searching
;; You should set this value, then unset it after the ido search, otherwise every
;; ido-completing-read will look for this value
(setq macoy-fuzzy-use-cache-list-name "")

(defadvice ido-set-matches-1 (around macoy-fuzzy-ido-set-matches-1 activate compile)
  "Choose between the regular ido-set-matches-1 and macoy-fuzzy-ido-match"
  (if (not macoy-fuzzy-ido-mode)
      ad-do-it
    (let* ((query ido-text)
           (original-items (ad-get-arg 0)))
      (setq ad-return-value
			(if macoy-fuzzy-use-cache-list-name
				(macoy-fuzzy-filter-cached-list macoy-fuzzy-use-cache-list-name query)
			  (macoy-filter-list-fuzzy-ido query original-items)
			  )
			)
	  )
    ))

(provide 'macoy-fuzzy-ido)

;;
;; Reference
;; (setq macoy-fuzzy-library-location "d:/Development/code/repositories/emacs-fuzzy-module/macoyFuzzy.dll")
;; (load-file "d:/Development/code/repositories/emacs-fuzzy-module/macoyFuzzy.el")
;; (require 'macoy-fuzzy-ido)
;; (macoy-fuzzy-ido-mode 1)

;;; macoyFuzzy.el ends here
