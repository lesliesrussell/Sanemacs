(use-package iedit
  :vc (:fetcher github :repo victorhge/iedit)
  :bind ("M-e" . iedit-mode)
  :custom
  (iedit-toggle-key-default (kbd "M-e")))
