;;; Do not edit this file!
;;; Make changes in README.org

(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))
(require 'vc-use-package)

(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(defconst emacs-tmp-dir (expand-file-name (format "emacs%d" (user-uid)) temporary-file-directory))

(use-package emacs
  :ensure nil
  :commands (reload-config
	     sanemacs/backward-kill-word)
  :bind (("C-<" . indent-rigidly-right-to-tab-stop)
	 ("C->" . indent-rigidly-left-to-tab-stop)
	 ("C-x C-b" . ibuffer))
  :custom ((inhibit-startup-screen t)
	   (initial-scratch-message "")
	   (use-short-answers t)
	   (ring-bell-function 'ignore)
	   (cursor-type 'bar)
	   (frame-title-format '("%b"))
	   (linum-format "%4d ")
	   (custom-file (expand-file-name "custom.el" user-emacs-directory))
	   (backup-by-copying t)
	   (delete-old-versions t)
	   (kept-new-versions 6)
	   (kept-old-versions 2)
	   (version-control t)
	   (auto-save-list-file-prefix emacs-tmp-dir)
	   (auto-save-file-name-transforms `((".*" ,emacs-tmp-dir t)))
	   (backup-directory-alist `((".*" . ,emacs-tmp-dir)))
	   (create-lockfiles nil)))

(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))

(defun reload-config ()
  (interactive)
  (load-file (expand-file-name "init.el" user-emacs-directory)))

(defun search-and-start-iedit (search-term)
  "Search for SEARCH-TERM and start iedit-mode on its occurrences."
  (interactive "sEnter search term: ")
  (let ((case-fold-search t)) ; Set this to nil if you want case-sensitive search
    (goto-char (point-min))   ; Start from the beginning of the buffer
    (search-forward search-term nil t)
    (backward-word)
    (iedit-mode)))          ; Activate iedit-mode on the current occurrence

(defun kill-and-replace-thing-at-point (thing)
  "Kill the THING at point and prepare for replacement."
  (let ((bounds (bounds-of-thing-at-point thing)))
    (when bounds
      (kill-region (car bounds) (cdr bounds)))))

;; Usage example:
;; (kill-and-replace-thing-at-point 'word)  ; Kills the word at point
;; (global-set-key (kbd "C-c k") (lambda () (interactive) (kill-and-replace-thing-at-point 'word)))

;; -*- lexical-binding: t -*-

(defun git-clone-clipboard-url ()
  "Clone git URL in clipboard asynchronously and open in dired when finished."
  (interactive)
  (cl-assert (string-match-p "^\\(http\\|https\\|ssh\\)://" (current-kill 0)) nil "No URL in clipboard")
  (let* ((url (current-kill 0))
	 (download-dir (expand-file-name "~/Downloads/"))
	 (project-dir (concat (file-name-as-directory download-dir)
			      (file-name-base url)))
	 (default-directory download-dir)
	 (command (format "git clone %s" url))
	 (buffer (generate-new-buffer (format "*%s*" command)))
	 (proc))
    (when (file-exists-p project-dir)
      (if (y-or-n-p (format "%s exists. delete?" (file-name-base url)))
	  (delete-directory project-dir t)
	(user-error "Bailed")))
    (switch-to-buffer buffer)
    (setq proc (start-process-shell-command (nth 0 (split-string command)) buffer command))
    (with-current-buffer buffer
      (setq default-directory download-dir)
      (shell-command-save-pos-or-erase)
      (require 'shell)
      (shell-mode)
      (view-mode +1))
    (set-process-sentinel proc (lambda (process state)
				 (let ((output (with-current-buffer (process-buffer process)
						 (buffer-string))))
				   (kill-buffer (process-buffer process))
				   (if (= (process-exit-status process) 0)
				       (progn
					 (message "finished: %s" command)
					 (dired project-dir))
				     (user-error (format "%s\n%s" command output))))))
    (set-process-filter proc #'comint-output-filter)))

(defun vc-git-status ()
  "Run git status on the current directory and display the results."
  (interactive)
  (shell-command "git status"))

(global-set-key (kbd "C-x v .") #'vc-git-status)

(add-hook 'after-init-hook #'global-visual-line-mode)

(add-hook 'before-save-hook
	  #'delete-trailing-whitespace)    ; Delete trailing

(add-hook 'prog-mode-hook
	  (if (or
	       (not (fboundp 'linum-mode))
	       (and (fboundp 'display-line-numbers-mode) (display-graphic-p)))
	      #'display-line-numbers-mode
	    #'linum-mode))

(setq lisp-dir (expand-file-name "lisp" user-emacs-directory))
(setq sane-dir (concat lisp-dir "/sanemodules"))

(add-to-list 'load-path lisp-dir)
(add-to-list 'load-path sane-dir)

(use-package sane-modules
  :ensure nil
  :load-path sane-dir
  :config
  (setq sane-module-base-directory (concat user-emacs-directory "modules/"))
  (setq sane-module-module-list '(;;; Sanemaces extended configuration
				  ;;; ui
				  ui/mct
				  ui/popper
				  ui/org-modern
				  ui/rainbow-mode
				  ;; ui/sane-mode
				  ;; ui/ef-themes
				  ui/golden
				  ;;; tools
				  tools/edit-indirect
				  ;; tools/emacs-dashboard
				  tools/expand-region
				  tools/helpful
				  tools/iedit
				  tools/hydra
				  tools/magit
				  tools/vundo
				  tools/visual-regexp
				  tools/denote
				  tools/active-region
				  tools/perspective
				  ;;; prose
				  writing/fountain
				  writing/darkroom
				  ;;; testing
				  testing/embark
				 )))
  (sane-modules-load-modules
   sane-module-base-directory
   sane-module-module-list t)
