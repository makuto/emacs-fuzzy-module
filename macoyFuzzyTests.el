;; Init
(module-load
"/home/macoy/Development/code/3rdParty/repositories/emacs/modules/macoyFuzzy/macoyFuzzy.so")
(require 'macoy-fuzzy)

;; Score
(message "%s" (macoy-filter-fuzzy-score "test" "blah-tsdfj-sfdasjes-st"))

;; Filter
(message "%s" (macoy-filter-list-fuzzy "test" ["blah-tsdfj-sfdasjes-st" "test" "nomatch" "whew"]))

;; Mode
(define-minor-mode macoy-fuzzy-ido-mode
  "Toggle Macoy fuzzy mode"
  :init-value nil
  :lighter "MacoyFuzzy"
  :group 'ido
  :global t)

(defun macoy-filter-list-fuzzy-ido (query items)
  (if (zerop (length query))
      items
    (macoy-filter-list-fuzzy query (vconcat original-items))
    )
  )

(defadvice ido-set-matches-1 (around flx-ido-set-matches-1 activate compile)
  "Choose between the regular ido-set-matches-1 and macoy-fuzzy-ido-match"
  (if (not macoy-fuzzy-ido-mode)
      ad-do-it
    (let* ((query ido-text)
           (original-items (ad-get-arg 0)))
      (setq ad-return-value (macoy-filter-list-fuzzy-ido query original-items)))
    ))

(macoy-fuzzy-ido-mode 1)


(message "%s" (macoy-filter-fuzzy-score "mac" "emacs.org"))
(message "%s" (macoy-filter-fuzzy-score "mac" "macoyFuzzy.el"))

(macoy-fuzzy-cache-list "testList" ["fdjaf" "test" "balls" "testest"])
(message "%s" (macoy-fuzzy-filter-cached-list "testList" "test"))

(macoy-fuzzy-cache-list "tags" (vconcat macoy-tag-names))
(message "%s" (macoy-fuzzy-filter-cached-list "tags" "test"))


(set macoy-fuzzy-use-cache-list-name "")

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

(setq macoy-fuzzy-use-cache-list-name "tags")
