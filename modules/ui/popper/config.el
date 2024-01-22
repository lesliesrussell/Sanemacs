(use-package popper
  :vc (:fetcher github :repo karthink/popper)
  :commands (popper-toggle
	     popper-cycle
	     popper-toggle-type
	     popper-mode
	     popper-echo-mode)
  :bind (("C-`" . popper-toggle)
	 ("M-`" . popper-cycle)
	 ("C-M-`" . popper-toggle-type))

  :hook (emacs-startup . (lambda ()
			   (popper-mode +1)
			   (popper-echo-mode +1)
			   ))
  :init
  (setq popper-reference-buffers
	'(
	  ("Output\\*$" . hide)
	  ("\\*Async Shell Command\\*" . hide)
	  ("\\*Messages\\*" . hide)
	  ("^\\*Occur.*\\*$")
	  ("^\\*Open Recent.*\\*$" recentf-dialog-mode)
	  ("^\\*Warn.*\\*$" special-mode . hide)
	  ("^\\*eshell.*\\*$" eshell-mode . hide)
	  ("^\\*grep.*\\*$")
	  ("^\\*shell.*\\*$"  shell-mode . hide)
	  ("^\\*term.*\\*$"   term-mode . hide)
	  ("^\\*vterm.*\\*$"  vterm-mode . hide)
	  ("^\\Buffer List.*\\*$")
	  ("^\\EKG Capture.*\\*$")
	  ("^\\Electric.*\\*$")
	  ("^\\Embark Actions.*\\*$")
	  ("^\\Geiser Guile REPL.*\\*$")
	  ("^\\helpful.*\\*$")
	  (compilation-mode . hide)
	  (elpaca-log-mode)
	  (help-mode)
	  (inferior-python-mode)
	  (rec-mode)
	  (use-package-statistics-mode)
	  )))
