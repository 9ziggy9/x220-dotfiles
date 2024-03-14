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

(defun my/counsel-find-file-new-frame (x)
  "Open the selected file from `counsel-find-file` in a new frame."
  (select-frame (make-frame))
  (find-file x))

;; particularly useful for switching themes: M-x counsel themes
(use-package counsel
  :ensure t
  :diminish counsel-mode
  :bind (("M-x"       . counsel-M-x)             ; Bind M-x to counsel-M-x
         ("M-s M-b"   . counsel-switch-buffer) ; Enhance file finding
         ("M-s M-f"   . counsel-find-file)     ; Enhance file finding
         ("M-s M-x"   . counsel-M-x-history)   ; Search through recent files
         ("M-s M-G"   . counsel-git)           ; Search for files in git repo
         ("M-s M-g"   . counsel-rg)            ; Search with ripgrep
         ("M-s M-c"   . counsel-locate)        ; Use locate to find files
         ("M-s M-!"   . (lambda () (interactive)
                        "Pick unicode char"
                        (let ((ivy-height 25))
                          (counsel-unicode-char)))))
  :config
  (setq ivy-initial-inputs-alist nil))

(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-mode 1))

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
  (define-key evil-normal-state-map (kbd "J")   'evil-avy-goto-line-below)
  (define-key evil-normal-state-map (kbd "W")   'evil-avy-goto-char-in-line))


(use-package ivy
  :ensure t
  :diminish ivy-mode
  :config
  (ivy-mode 1)
  (setq ivy-wrap t)
  (setq ivy-height 18))
  ;; frame-height initialization bug, very curious, not sure why
  ;; see hooks at top of init.el for demonstration.
  ;; (setq ivy-height (round (* 0.40 (frame-height)))))

(use-package ivy-rich
  :ensure t
  :after ivy
  :config
  (ivy-rich-mode 1)
  (setq ivy-rich-path-style 'abbrev)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))
