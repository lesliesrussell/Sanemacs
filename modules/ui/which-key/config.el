(use-package which-key
  :vc (:fetcher github :repo justbur/emacs-which-key)
    :defer t
    :custom
    (which-key-idle-delay 0.5)
    (which-key-max-description-length 27)
    (which-key-add-column-padding 0)
    (which-key-max-display-columns nil)
    (which-key-separator " → " )
    (which-key-unicode-correction 3)
    (which-key-prefix-prefix "+" )
    (which-key-special-keys '("SPC" "TAB" "RET" "ESC" "DEL"))
    (which-key-show-prefix 'left)
    (which-key-show-remaining-keys t)
    :init
    (which-key-mode 1))
