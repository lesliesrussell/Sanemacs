(use-package dashboard
  :vc t
  :ensure t
  :custom ((dashboard-banner-logo-title "Sanemacs v4.0")
	   (dashboard-startup-banner '("~/.repos/Sanemacs/logo.png" . "~/.repos/Sanemacs/sanemacs.txt"))
	   (dashboard-center-content t)
	   (dashboard-show-shortcuts nil)
	   (dashboard-items '((recents  . 5)
                              (bookmarks . 5)
                              (agenda . 5)
                              (registers . 5))))
  :config
  (dashboard-setup-startup-hook))
