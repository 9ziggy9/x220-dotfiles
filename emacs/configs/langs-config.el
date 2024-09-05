(use-package eglot
  :ensure t
  :commands eglot eglot-ensure
  :bind (:map eglot-mode-map ("M-H" . eglot-code-actions))
  :config 
  (setq eglot-extend-to-xref t)
  (setq eglot-ignored-server-capabilities '(:inlayHintProvider))
  (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-mode t)
  (add-to-list 'eglot-server-programs
               '( (c-mode          . ("Clangd" "--std=c17"))
                  (c++-mode        . ("clangd"))
                  (go-mode         . ("gopls"))
                  (typescript-mode . ("typescript-language-server" "--stdio"))
                ))
  (setq eglot-server-programs
        '( (c-mode          . ("clangd"))
           (c++-mode        . ("clangd"))
           (go-mode         . ("gopls"))
           (typescript-mode . ("typescript-language-server" "--stdio"))
         ))
)

(use-package eldoc
  :ensure t
  :hook (prog-mode . eldoc-mode))

(use-package eldoc-box
  :bind (("M-h" . eldoc-box-help-at-point))
  :hook (eldoc-mode . eldoc-box-hover-mode))

(use-package go-eldoc
  :ensure t
  :hook (go-mode . go-eldoc-setup))

(use-package markdown-mode)

(use-package cc-mode
  :ensure t
  :hook ((c-mode   . (lambda () (eglot-ensure) (flymake-mode -1)))
         (c++-mode . (lambda () (eglot-ensure) (flymake-mode -1)))))

(use-package glsl-mode
  :ensure t
  :mode (("\\.vs\\'" . glsl-mode)   ("\\.fs\\'" . glsl-mode)
         ("\\.gs\\'" . glsl-mode)   ("\\.vert\\'" . glsl-mode)
         ("\\.frag\\'" . glsl-mode) ("\\.geo\\'" . glsl-mode)
         ("\\.glsl\\'" . glsl-mode)))

(use-package go-mode
  :ensure t
  :hook ((go-mode . eglot-ensure)))

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

;; BEGIN JS/TS
(use-package js2-mode
  :ensure t
  :mode (("\\.gs\\'" . js2-mode))
  :config
  (setq js2-basic-offset 2))

(use-package rjsx-mode
  :ensure t
  :mode ("\\.jsx?\\'" . rjsx-mode))

(use-package typescript-mode
  :ensure t
  :mode (("\\.tsx?\\'" . typescript-mode))
  :hook (typescript-mode . eglot-ensure)
  :config
  (setq typescript-indent-level 2))
;; END JS/TS

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


;; BEGIN MISC
(use-package i3wm-config-mode
  :mode (("\\.i3/config\\'" . i3wm-config-mode)
          ("i3_config\\'" . i3wm-config-mode)))

(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile\\'")

(use-package yaml-mode
  :ensure t
  :mode "\\.yml\\'"
  :hook (yaml-mode . (lambda ()
                       (setq-local indent-tabs-mode nil)
                       (setq-local tab-width 2)))
  :config
  ;; Additional configuration (optional)
  (setq yaml-indent-offset 2))

(use-package web-mode
  :ensure t
  :mode ("\\.html?\\'" . web-mode)
  :config
  (setq web-mode-enable-auto-pairing t
        web-mode-enable-auto-closing t
        web-mode-enable-auto-indentation nil
        web-mode-enable-auto-expanding t
        web-mode-enable-auto-quoting t
        web-mode-auto-close-style 2
        web-mode-tag-auto-close-style 2))
;; END MISC
