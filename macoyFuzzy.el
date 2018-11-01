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

;; Forrest Smith wrote the fuzzy matching algorithm.
;; flx-ido.el is what I looked at to know how to integrate my module into ido.

;;; Installation:

;; You must have compiled your Emacs with --with-modules for it to support modules.
;; Add the following code to your init file:
;;
;;     (require 'macoy-fuzzy-ido)
;;     (ido-mode 1)
;;     (ido-everywhere 1)
;;     (macoy-fuzzy-ido-mode 1)

;;; Code:

;; TODO: Make relative
(module-load "/home/macoy/Development/code/repositories/emacs-fuzzy-module/macoyFuzzy.so")
(require 'macoy-fuzzy)
;; TODO duplicate and remove
(require 'flx-ido)

(define-minor-mode macoy-fuzzy-ido-mode
  "Toggle Macoy fuzzy mode"
  :init-value nil
  :lighter "MacoyFuzzy"
  :group 'ido
  :global t)


;; (defadvice ido-exit-minibuffer (around flx-ido-reset activate)
;;   "Remove flx properties after."
;;   (let* ((obj (car ido-matches))
;;          (str (if (consp obj)
;;                   (car obj)
;;                 obj)))
;;     (when (and macoy-fuzzy-ido-mode str)
;;       (remove-text-properties 0 (length str)
;;                               '(face flx-highlight-face) str)))

;;   ad-do-it)

;; TODO: Sort list by last used? (duplicate whatever behavior the normal stuff does)
(defun macoy-filter-list-fuzzy-ido (query items)
  (if (zerop (length query))
      (nreverse items) ;; Reverse because the history is in reverse
    (macoy-filter-list-fuzzy query (vconcat original-items))
    )
  ;; TODO duplicate and remove (remember remove properties advice!)
  ;; (flx-ido-decorate)
  )

(defadvice ido-set-matches-1 (around macoy-fuzzy-ido-set-matches-1 activate compile)
  "Choose between the regular ido-set-matches-1 and macoy-fuzzy-ido-match"
  (if (not macoy-fuzzy-ido-mode)
      ad-do-it
    (let* ((query ido-text)
           (original-items (ad-get-arg 0)))
      (setq ad-return-value (macoy-filter-list-fuzzy-ido query original-items)))
    ))

(provide 'macoy-fuzzy-ido)

;;; macoyFuzzy.el ends here
