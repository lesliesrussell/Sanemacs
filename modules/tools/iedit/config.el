;; moved from README.org
(defun search-and-start-iedit (search-term)
  "Search for SEARCH-TERM and start iedit-mode on its occurrences."
  (interactive "sEnter search term: ")
  (let ((case-fold-search t)) ; Set this to nil if you want case-sensitive search
    (goto-char (point-min))   ; Start from the beginning of the buffer
    (search-forward search-term nil t)
    (backward-word)
    (iedit-mode)))          ; Activate iedit-mode on the current occurrence

(use-package iedit
  :vc (:fetcher github :repo victorhge/iedit)
  :bind ("M-e" . iedit-mode)
  :custom
  (iedit-toggle-key-default (kbd "M-e")))
