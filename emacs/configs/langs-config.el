(use-package ccls :ensure t)       ;; c/c++
(use-package go-mode :ensure t)    ;; golang

;; BEGIN: RUST (I really dislike all of this, fix later)
(use-package rust-mode :ensure t)
(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook (rust-mode . lsp)
  :init
  (setq lsp-rust-server 'rust-analyzer)
  :config
  (setq lsp-rust-analyzer-server-command '("~/.cargo/bin/rust-analyzer")))
(use-package flycheck-rust
  :ensure t
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
;; END: RUST

;; BEGIN PYTHON
;; pipenv support included
(use-package python-mode :ensure t)
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
  :config
  (setq typescript-indent-level 2))
(use-package tide
  :ensure t
  :after typescript-mode
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode))
  :config
  (setq tide-completion-ignore-case t
        tide-always-show-documentation t
        tide-server-max-response-length 102400))
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
  :config
  (add-hook 'haskell-mode-hook 'haskell-indentation-mode))
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
