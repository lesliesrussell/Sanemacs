(use-package ef-themes
  :vc (:fetcher github :repo protesilaos/ef-themes)
  :defer t
  :commands (ef-themes-select)
  :hook (emacs-startup . (lambda ()
			   (ef-themes-select 'ef-tritanopia-dark t)
			   ))
  :bind ("C-c t t" . ef-themes-toggle)
  :custom
  (ef-themes-to-toggle '(ef-melissa-dark ef-tritanopia-dark))
  :config
  ;; Do not extend `region' background past the end of the line.
  (custom-set-faces
   '(region ((t :extend nil))))

  (set-face-attribute 'default nil :family "Fira Code" :height 110)
  (set-face-attribute 'italic nil :family "Hack"))
