;; testing org tangle
;; (use-package flycheck
;;   :ensure t
;;   :init (global-flycheck-mode)
;;   :hook
;;   (emacs-lisp-mode . (lambda ()
;;                        (setq-local flycheck-disabled-checkers
;;                                    '(emacs-lisp-checkdoc))))
;;   :config
;;   (setq flycheck-highlighting-mode 'symbols)
;;   (set-face-attribute 'flycheck-warning nil :foreground "yellow" :background nil :underline nil)
;;   (set-face-attribute 'flycheck-error nil :foreground "red" :background nil :underline nil))


;; access docs
(use-package eldoc-box
  :bind
  (("M-h" . eldoc-box-help-at-point)))

;; No more LSP-mode!!!
(use-package eglot
  :ensure t
  :commands eglot eglot-ensure
  :bind (:map eglot-mode-map ("M-H" . eglot-code-actions))
  :config 
  (setq eglot-ignored-server-capabilities
    '(:inlayHintProvider :hoverProvider ))
  (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-mode t)
  (add-to-list 'eglot-server-programs '(c-mode . ("clangd"))))

(use-package cc-mode
  :ensure t
  :hook (c-mode . eglot-ensure))

(use-package go-mode :ensure t)    ;; golang

(use-package rust-mode
  :ensure t
  :hook (rust-mode . eglot-ensure))

;; BEGIN PYTHON
;; pipenv support included
(use-package python-mode
  :ensure t
  :config
  (setq python-indent-offset 4))

(use-package pipenv
  :ensure t
  :commands (pipenv-mode pipenv-activate pipenv-run)
  :hook (python-mode . pipenv-mode))
;; END PYTHON

;; BEGIN TYPESCRIPT
;; Note my typescript is using tide directly, rather than eglot
(use-package typescript-mode
  :ensure t
  :mode (("\\.tsx?\\'" . typescript-mode))
  :hook (typescript-mode . eglot-ensure)
  :config
  (setq typescript-indent-level 2)
  (add-hook 'typescript-mode-hook (lambda ()
                (add-hook 'post-command-hook 'eglot-help-at-point nil t))))
;; END TYPESCRIPT

;; BEGIN JS (only gs at moment)
(use-package js2-mode
  :ensure t
  :mode (("\\.js\\'" . js2-mode)
         ("\\.gs\\'" . js2-mode))
  :config
  (setq js2-basic-offset 2))
(use-package rjsx-mode
  :ensure t
  :mode ("\\.jsx?\\'" . rjsx-mode)) ;; for .js and jsx files (?)
;; END JS

;; BEGIN RACKET
(use-package racket-mode
  :ensure t
  :mode "\\.rkt\\'"
  :config
  (add-hook 'racket-mode-hook
            (lambda ()
              (define-key racket-mode-map (kbd "C-c r") 'racket-run))))
;; END RACKET

;; BEGIN HASKELL
;; Haskell mode
(use-package haskell-mode
  :ensure t
  :hook ((haskell-mode . interactive-haskell-mode)))
;; END HASKELL

;; BEGIN OCAML
;; Tuareg mode for OCaml
(use-package tuareg
  :ensure t)
  ;; :config
  ;; (add-hook 'tuareg-mode-hook
  ;;           (lambda ()
  ;;             ;; Enable automatic indentation
  ;;             (setq indent-line-function 'tuareg-indent-line))))

;; Merlin for enhanced OCaml tooling support (type-checking, completion, etc.)
(use-package merlin
  :ensure t
  :config
  (add-hook 'tuareg-mode-hook 'merlin-mode)
  (setq merlin-command "/home/ziggy/.opam/default/bin/ocamlmerlin")
  (add-hook 'merlin-mode-hook 'company-mode))

;; e.g., dune for project management
(use-package dune
  :ensure t)
;; END OCAML

;; BEGIN GRAPHVIZ
(use-package graphviz-dot-mode
  :ensure t
  :mode "\\.dot\\'"
  :config
  (setq graphviz-dot-indent-width 4))
;; END GRAPHVIZ

;; MISC
(use-package i3wm-config-mode
  :mode (("\\.i3/config\\'" . i3wm-config-mode)
          ("i3_config\\'" . i3wm-config-mode)))

(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile\\'")
;; END MISC
