(use-package perspective
  :vc t
  :commands (persp-switch-to-buffer
	     persp-mode)
  :custom
  (persp-mode-prefix-key (kbd "C-c p"))
  :bind (;; ("C-c p b" . persp-list-buffers)
	 ;; ("C-c p n" . persp-switch-to-buffer)
	 :map perspective-map
	 ("n" . nil)
	 ("N" . persp-next))
  :init
  (add-hook 'emacs-startup-hook 'persp-mode))
