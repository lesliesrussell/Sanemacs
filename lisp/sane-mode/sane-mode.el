;;; sane-mode.el --- Custom keybindings for Emacs -*- lexical-binding: t; -*-

;; Author: Your Name <your.email@example.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.3"))
;; Keywords: convenience
;; URL: http://github.com/yourusername/sane-mode

;;; Commentary:

;; This Emacs Lisp code defines a custom minor mode called `sane-mode`, which aims to provide an alternative keybinding scheme for Emacs.

;; 1. **Keymap Definitions (`sane-g-prefix`, `sane-ctrl-x-prefix`, `sane-mode-map`)**:
;;    - These sections create sparse keymaps for different prefix keys (`g` and `Ctrl-x`) and for the main `sane-mode`.
;;    - It redefines common Emacs keybindings to more familiar ones (possibly inspired by other editors).
;;    - For instance, `g` prefix is used for navigation commands like `beginning-of-buffer`, `end-of-buffer`, etc.
;;    - The `Ctrl-x` prefix contains commands for window management, file operations, and more.

;; 2. **Custom Mode Definition (`sane-mode`)**:
;;    - `sane-mode` is a minor mode with its own keymap (`sane-mode-map`).
;;    - The mode changes the cursor type depending on its state (enabled/disabled).
;;    - It automatically disables itself in certain modes, defined in `sane-mode-exempt-modes`.

;; 3. **Toggle Function (`toggle-sane-mode`)**:
;;    - This function toggles `sane-mode` on and off.
;;    - It's bound globally to the `<escape>` key, allowing quick toggling of the mode.

;; 4. **Commented Global Key Binding**:
;;    - There's a commented line for setting a global keybinding (`C-c s`) for `toggle-sane-mode`. Uncommenting this would enable this global shortcut.

;;; Code:
(defun kill-thing-at-point (thing)
  "Kill the THING at point and prepare for replacement."
  (let ((bounds (bounds-of-thing-at-point thing)))
    (when bounds
      (kill-region (car bounds) (cdr bounds)))))

(defvar sane-g-prefix (let ((map (make-sparse-keymap)))
                        ;; Add bindings that start with "g" here, for example:
                        ;; (define-key map (kbd "c") 'some-command)
			(define-key map (kbd "J") 'join-line)
			(define-key map (kbd "g") 'beginning-of-buffer)
			(define-key map (kbd "i") 'iedit-mode)
			(define-key map (kbd "l") 'goto-line)
			(define-key map (kbd "m") 'back-to-indentation)
			(define-key map (kbd "s o") 'occur)
			(define-key map (kbd "e") 'end-of-buffer)
                        ;; Suppress other keys
                        (suppress-keymap map)
                        map)
  "Keymap for commands prefixed with 'g'.")

(defvar sane-ctrl-x-prefix (let ((map (make-sparse-keymap)))
                             ;; Add bindings that start with "x" here
			     (define-key map (kbd "'") 'expand-abbrev)
			     (define-key map (kbd "1") 'delete-other-windows)
			     (define-key map (kbd "2") 'split-window-below)
			     (define-key map (kbd "3") 'split-window-right)
			     (define-key map (kbd "b") 'switch-to-buffer)
			     (define-key map (kbd "B") 'ibuffer)
			     (define-key map (kbd "c") 'save-buffers-kill-terminal)
			     (define-key map (kbd "o") 'other-window)
			     (define-key map (kbd "s") 'save-buffer)
			     (define-key map (kbd "w 0") 'delete-windows-on)
			     (define-key map (kbd "x") 'exchange-point-and-mark)
                             (define-key map (kbd "f") 'find-file)
                             ;; Suppress other keys
                             (suppress-keymap map)
                             map)
  "Keymap for commands prefixed with 'x'.")

(defvar sane-mode-map (let ((map (make-sparse-keymap)))
                        ;; Bind the sane-ctrl-x-prefix map to "x"
                        (define-key map (kbd "x") sane-ctrl-x-prefix)
			(define-key map (kbd "g") sane-g-prefix)
			(define-key map (kbd ".") 'repeat)
			(define-key map (kbd ":") 'repeat-complex-command)
			(define-key map (kbd ";") 'execute-extended-command)
			(define-key map (kbd "H") 'backward-delete-char)
			(define-key map (kbd "V") 'scroll-down-command)
			(define-key map (kbd "a") 'beginning-of-visual-line)
			(define-key map (kbd "b") 'backward-word)
			(define-key map (kbd "c l") '(lambda () (interactive) (kill-and-replace-thing-at-point 'line) (toggle-sane-mode)))
			(define-key map (kbd "c s") '(lambda () (interactive) (kill-and-replace-thing-at-point 'symbol) (toggle-sane-mode)))
			(define-key map (kbd "c w") '(lambda () (interactive) (kill-and-replace-thing-at-point 'word) (toggle-sane-mode)))
			(define-key map (kbd "c x") '(lambda () (interactive) (kill-and-replace-thing-at-point 'sexp) (toggle-sane-mode)))
			(define-key map (kbd "d c") 'delete-char)
			(define-key map (kbd "d w") 'kill-word)
			(define-key map (kbd "e") 'end-of-visual-line)
			(define-key map (kbd "f f") 'forward-char)
			(define-key map (kbd "f h") 'forward-paragraph)
			(define-key map (kbd "f x") 'forward-sexp)
			(define-key map (kbd "h") 'backward-char)
			(define-key map (kbd "j") 'electric-newline-and-maybe-indent)
			(define-key map (kbd "J") 'join-line)
			(define-key map (kbd "k b") 'kill-buffer)
			(define-key map (kbd "k k") 'kill-ring-save)
			(define-key map (kbd "k l") 'kill-line)
			(define-key map (kbd "k p") 'kill-paragraph)
			(define-key map (kbd "k p") 'kill-paragraph)
			(define-key map (kbd "k r") 'kill-region)
			(define-key map (kbd "k s") 'kill-sentence)
			(define-key map (kbd "k s") 'kill-sentence)
			(define-key map (kbd "k v") 'kill-visual-line)
			(define-key map (kbd "k w") 'kill-word)
			(define-key map (kbd "k x") 'kill-sexp)
			(define-key map (kbd "l") 'recenter-top-bottom)
			(define-key map (kbd "m m") 'set-mark-command)
			(define-key map (kbd "m p") 'mark-paragraph)
			(define-key map (kbd "n") 'next-line)
			(define-key map (kbd "o") '(lambda () (interactive) (open-line 0) (electric-newline-and-maybe-indent) (toggle-sane-mode)))
			(define-key map (kbd "p") 'previous-line)
			(define-key map (kbd "s r") 'isearch-backward-regexp)
			(define-key map (kbd "s s") 'isearch-forward-regexp)
			(define-key map (kbd "v") 'scroll-up-command)
			(define-key map (kbd "w") 'forward-word)
			(define-key map (kbd "y p") 'yank-pop)
			(define-key map (kbd "y y") 'yank)
			(define-key map (kbd "z") 'rectangle-mark-mode)
                        ;; Suppress other keys
                        (suppress-keymap map)
                        map)
  "Keymap for `sane-mode'.")

(defcustom sane-mode-exempt-modes '(minibuffer-mode dired-mode help-mode)
  "List of modes where `sane-mode` should not be activated."
  :type '(repeat symbol)
  :group 'sane)

(define-minor-mode sane-mode
  "A custom mode to remap keys."
  :lighter " S"
  ;; :global t
  :keymap sane-mode-map
  (if sane-mode
      (if (member major-mode sane-mode-exempt-modes)
          (sane-mode -1)  ; Deactivate sane-mode if the current mode is exempt
        (setq cursor-type 'box))  ; Actions when the mode is enabled
    (setq cursor-type 'bar)))  ; Actions when the mode is disabled

(defun toggle-sane-mode ()
  "Toggle `sane-mode'."
  (interactive)
  (sane-mode (if sane-mode -1 1)))

;; Global key binding for toggle-sane-mode
;; (global-set-key (kbd "C-c s") 'toggle-sane-mode)

;; Key binding for toggling the mode
(define-key sane-mode-map (kbd "<escape>") 'toggle-sane-mode)
(global-set-key (kbd "<escape>") 'toggle-sane-mode)
;; (toggle-sane-mode)
(sane-mode 1)


(provide 'sane-mode)

;;; sane-mode.el ends here
