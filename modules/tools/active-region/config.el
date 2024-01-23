;;; sane-region-mode.el --- Enhance region-related functionalities in Emacs

;; Author: Leslie S. Russell <lesliesrussell@gmail.com>
;; Version: 1.0
;; Keywords: region, convenience
;; URL: https://github.com/lesliesrussell/Sanemacs

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The `sane-region-mode` is an Emacs Lisp package designed to augment
;; the Emacs editing experience by enhancing region-based actions. This
;; mode provides a suite of utilities for working with text regions in
;; Emacs, making common tasks like commenting, capitalization, text
;; manipulation, and other region-specific actions more accessible and
;; efficient.

;; Key features of `sane-region-mode`:
;; - Quick commenting/uncommenting of regions.
;; - Easy capitalization and other text transformations.
;; - Seamless integration with Emacs's existing region functionality.
;; - Customizable key bindings for enhanced workflow integration.

;; This package is ideal for Emacs users who frequently perform text
;; editing and manipulation, offering streamlined shortcuts and
;; commands that are easily triggered when a text region is selected.

;;; Code:

(defun sane-kill-comment-and-yank ()
  "If the region is active, kill it, else kill the whole line.
Comment the line, then open a new line and yank the killed text."
  (interactive)
  (if (use-region-p)
      (kill-ring-save (region-beginning) (region-end))
    (kill-whole-line))
  (comment-line 1)
  (open-line 1)
  (forward-line 1)
  (yank))

;; You can bind this function to a key, for example, F5
;; (global-set-key (kbd "C-c y") 'sane-kill-comment-and-yank)

(defun sane-apropos-region ()
  "Use the current region as the input for `apropos-command`."
  (interactive)
  (if (use-region-p)
      (let ((region-text (buffer-substring (region-beginning) (region-end))))
	(apropos-command region-text))
    (message "No region selected!")))

(defvar sane-region-mode-map (make-sparse-keymap)
  "Keymap for `sane-region-mode'.")

(define-minor-mode sane-region-mode
  "A minor mode that is activated whenever a region is marked."
  :lighter " Я"
  :keymap sane-region-mode-map
  ;; Here you can add any additional setup or teardown code needed when the mode is toggled
  )

(defun sane-region-mode-toggle ()
  "Toggle `sane-region-mode' based on whether a region is active."
  (if (use-region-p)
      (unless sane-region-mode (sane-region-mode 1))
    (when sane-region-mode (sane-region-mode 0))))

(add-hook 'post-command-hook 'sane-region-mode-toggle)

;; Define your key bindings for this mode
;; (define-key sane-region-mode-map (kbd "<your-key>") 'your-command)
;; Replace <Your-Key> With The Key You Want To Use,
;; And 'Your-Command With The Command You Want To Execute.
(define-key sane-region-mode-map (kbd ";") #'comment-dwim)
;; (define-key sane-region-mode-map (kbd "C") #'capitalize-region)
(define-key sane-region-mode-map (kbd "w") #'kill-region)
(define-key sane-region-mode-map (kbd "s") #'kill-ring-save)
(define-key sane-region-mode-map (kbd "?") #'sane-apropos-region)
(define-key sane-region-mode-map (kbd "y") #'sane-kill-comment-and-yank)

(provide 'sane-region-mode)
;;; sane-region-mode.el ends here
