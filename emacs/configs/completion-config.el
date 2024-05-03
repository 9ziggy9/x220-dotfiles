;; Tree sitter uses an AST to parse code in real time for better syntax
;; highlighting and so forth.
(use-package tree-sitter
  :ensure t
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter)

;; autocompletion
(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t) ;; Enable cycling through completion candidates
  (corfu-auto t) ;; Enable automatic completion
  :init
  (global-corfu-mode))

;; more detailed buffers
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t)     ; Cycle through completions
  (vertico-resize nil)) ; Don't resize minibuffer

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless)))

(use-package counsel
  :ensure t
  :diminish counsel-mode
  :bind (("M-x"       . counsel-M-x)           ; Bind M-x to counsel-M-x
         ("M-s M-b"   . counsel-switch-buffer) ; Enhance file finding
         ("M-s M-f"   . counsel-find-file)     ; Enhance file finding
         ("M-s M-x"   . counsel-M-x-history)   ; Search through recent files
         ("M-s M-G"   . counsel-git)           ; Search for files in git repo
         ("M-s M-g"   . counsel-rg)            ; Search with ripgrep
         ("M-s M-c"   . counsel-locate)        ; Use locate to find files
         ("M-s M-!"   . (lambda () (interactive) (let ((ivy-height 25))
                                                   (counsel-unicode-char)))))
  :config
  (setq ivy-initial-inputs-alist nil))

(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-mode 1))
