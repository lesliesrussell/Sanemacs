;; I sacrificed "M-g g" because it is redundant, and I don't use it
(use-package magit
  :vc t
  :commands (magit)
  :bind ("M-g g" . magit)
  :defer t)
