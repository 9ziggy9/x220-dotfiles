(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
      doom-themes-enable-italic t)
  (load-theme 'doom-ephemeral t))

;; RAINBOW PARENS
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; CUSTOM MODE LINE
(use-package powerline-evil
  :ensure t
  :init 
  (setq powerline-height 25
        powerline-default-separator 'contour)
  :config (powerline-default-theme))
