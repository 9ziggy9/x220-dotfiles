(use-package helpful
  :ensure t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command]  . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key]      . helpful-key))

(use-package which-key
  :ensure t
  :init
  (which-key-mode)
  :config
  ;; (setq which-key-show-early-on-C-h-delay 0.8)
  (setq which-key-idle-delay 0.1)  ; Adjust the delay before which-key popup appears
  (setq which-key-popup-type 'side-window)  ; Display in a side window
  (setq which-key-side-window-location 'bottom)  ; Display at the bottom
  (setq which-key-side-window-max-width 0.33)  ; Maximum width of the which-key window
  (setq which-key-side-window-max-height 0.25)  ; Maximum height of the which-key window
  (setq which-key-add-column-padding 1)  ; Add padding between columns
  (setq which-key-max-description-length 32)  ; Truncate descriptions
  (setq which-key-allow-evil-operators t)  ; If you're using Evil mode
  (which-key-setup-side-window-bottom)  ; Setup side window at the bottom
  :diminish which-key-mode)  ; Hide which-key mode from the mode line

;; particularly useful for switching themes: M-x counsel themes
(use-package swiper
  :ensure t
  :bind (("M-s M-d" . swiper)) ; Bind swiper to C-s, replacing default isearch
  :config
  ;; Optional customization
  (setq swiper-action-recenter t)  ; Re-centers the screen after selection
  (setq swiper-goto-start-of-match t))  ; Start of the match is aligned with the cursor

(use-package avy
  :ensure t
  :config
  (setq avy-all-windows 'all-frames)
  (setq avy-all-windows-alt t)
  (setq avy-background t)
  (setq avy-style 'words)
  ;; (setq avy-keys '(?j ?k ?f ?s))

  (set-face-attribute 'avy-lead-face-0 nil :foreground "#181e26" :background "#f1fa8c")
  (set-face-attribute 'avy-lead-face-1 nil :foreground "#181e26" :background "#f1fa8c")
  (set-face-attribute 'avy-lead-face-2 nil :foreground "#181e26" :background "#f1fa8c")
  (set-face-attribute 'avy-lead-face   nil :foreground "#181e26" :background "#f1fa8c")
  (set-face-attribute 'avy-background-face nil :foreground "#77818f" :background "#181e26")
  (set-face-attribute 'avy-goto-char-timer-face nil :foreground "#53e2ae" :background "black")

  (define-key evil-normal-state-map (kbd "SPC") 'evil-avy-goto-char-timer)
  (define-key evil-normal-state-map (kbd "K")   'evil-avy-goto-line-above)
  (define-key evil-normal-state-map (kbd "J")   'evil-avy-goto-line-below))
