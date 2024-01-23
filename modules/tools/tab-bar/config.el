(global-set-key (kbd "C-c t p") 'tab-bar-switch-to-prev-tab)
(global-set-key (kbd "C-c t n") 'tab-bar-switch-to-next-tab)
(global-set-key (kbd "C-c t w") 'tab-bar-new-tab)
(global-set-key (kbd "C-c t x") 'tab-bar-close-tab)

(when (< 26 emacs-major-version)
  (tab-bar-mode 1)                           ;; enable tab bar
  (setq tab-bar-show 1)                      ;; hide bar if <= 1 tabs open
  (setq tab-bar-close-button-show nil)       ;; hide tab close / X button
  (setq tab-bar-new-tab-choice "*dashboard*");; buffer to show in new tabs
  (setq tab-bar-tab-hints t)                 ;; show tab numbers
  (setq tab-bar-format '(tab-bar-format-tabs tab-bar-separator)))
