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
(use-package company
  :ensure t
  :init (global-company-mode)
  :config
  (setq company-tooltip-align-annotations t)
  (define-key company-active-map (kbd "H-s") 'company-select-next)
  (define-key company-active-map (kbd "H-w") 'company-select-previous)
  (setq company-selection-wrap-around t)
  (setq company-idle-delay 0)  ;; Start immediately without delay
  (setq company-minimum-prefix-length 4)  ;; Start completion after one char
  (add-to-list 'company-backends 'company-capf)
  (setq company-auto-complete-chars (quote (32 41 46))))

(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode))

;; more detailed buffers
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(use-package consult
  :ensure t
  :bind (("M-s M-d" . consult-line)))

(use-package embark
  :ensure t
  :bind (("M-!"   . embark-act)
         ("C-;"   . embark-dwim)
         ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (setq embark-prompter 'embark-completing-read-prompter)
  )

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :bind (
        ("M-s M-b"   . consult-buffer)
        ("M-s M-b"   . consult-buffer-other-window)
        ("M-s M-f"   . find-file)
        ("M-s M-g"   . consult-ripgrep)
        ("M-s M-c"   . locate))
  :custom
  (vertico-count 40))

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless)))
