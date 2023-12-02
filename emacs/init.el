;; BEGIN PACKAGE MANAGEMENT
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			("org" . "https://orgmode.org/elpa/")
			("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Grab use-package if not on platform
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
;; END USE-PACKAGE

(use-package emacs
  :ensure t
  :config
  ;; CUSTOM FUNCTIONS
  (load-file "~/.config/emacs/functions-shell.el")
  (load-file "~/.config/emacs/functions-find.el")

  ;; GLOBAL BINDINGS
  (load-file "~/.config/emacs/bindings-global.el")

  ;; BUG: leads to collision with company-quickhelp popups!
  ;; FRAMES
  (load-file "~/.config/emacs/frames.el")
  (setq-default inhibit-startup-screen t
                make-backup-files nil
                tab-width 2
                visible-bell t
                auto-save-default nil
                create-lockfiles nil
                indent-tabs-mode nil
                use-dialog-box nil
                compilation-scroll-output t)
  ;; Inhibit bars
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode -1)
  (set-fringe-mode 10)

  ;; Rather than find-file, you can find recently edited files
  (recentf-mode 1)

  ;; Automatic switching to compilation window after done
  (add-hook 'compilation-finish-functions
            'switch-to-buffer-other-window 'compilation)
  ;; Automatic switching to help windows
  (setq help-window-select t)


  ;; stop the backup files damnit
  (setq backup-directory-alist
    `((".*" . ,temporary-file-directory)))
  (setq auto-save-file-name-transforms
    `((".*" ,temporary-file-directory t)))
  (setq auto-save-file-name-transforms
    `((".*" ,temporary-file-directory t)))
  (setq undo-tree-history-directory-alist
        `(("." . "~/.config/emacs/.undo-tree-history")))

  ;; Lines
  (column-number-mode)
  (setq display-line-numbers-type 'relative)
  (global-display-line-numbers-mode t)

  ;; Command history
  (setq history-length 100)
  (savehist-mode 1)
  (save-place-mode 1)

  ;; Prevents customization varibles being put into out init.el!
  ;; Keep a nice clean init.el, with automatic code generated from
  ;; things such as use-package.
  (setq custom-file (locate-user-emacs-file "custom-vars.el"))
  (load custom-file 'noerror 'nomessage)

  ;; Refersh buffers when underlying files have changed.
  ;; i.e., suppose we change branches on git
  (global-auto-revert-mode 1)
  (setq auto-revert-ask-before-discard nil)
  (setq global-auto-revert-non-file-buffers t)

  ;; FONT
  (set-face-attribute 'default nil :font "Iosevka Nerd Font-14")
  (custom-set-faces
  '(echo-area ((t (:height 120))))
  '(minibuffer-prompt ((t (:height 150))))
  '(mode-line ((t (:height 120)))))

  ;; AUTOPAIRS
  (electric-pair-mode t)

  ;; USE SYSTEM CLIPBOARD
  (setq x-select-enable-clipboard t))

;; UNDO TREE
(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode))

;; EVIL MODE
(use-package evil
  :ensure t
  :demand t
  :bind (("<escape>" . keyboard-escape-quit))
  :init
  (setq evil-want-keybinding nil)
  :config
  (load-file "~/.config/emacs/bindings-vim.el")
  (setq evil-escape-key-sequence "C")
  (setq evil-shift-width 2)
  (setq evil-jump-cross-buffers t)
  (evil-set-undo-system 'undo-tree)
  (evil-mode 1)
  (add-hook 'evil-normal-state-exit-hook
            #'(lambda () (interactive)
                "Custom function to undo all evil-mc cursors."
                (when (and (bound-and-true-p evil-mc-mode)
                           (evil-mc-has-cursors-p))
                  (evil-mc-undo-all-cursors)))))
(use-package evil-collection
  :ensure t
  :after evil
  :config
  (setq evil-want-integration t)
  (setq avy-all-windows 'all-frames)
  (setq avy-all-windows-alt t)
  (setq avy-background t)
  (evil-collection-init))
(use-package evil-mc
  :ensure t
  :config
  (global-evil-mc-mode 1)
  (define-key evil-normal-state-map (kbd "L") 'evil-multiedit-match-and-next)
  (define-key evil-normal-state-map (kbd "J") 'evil-mc-make-cursor-move-next-line)
  (define-key evil-normal-state-map (kbd "K") 'evil-mc-undo-last-added-cursor))
(use-package evil-multiedit :ensure t)
(evil-multiedit-mode 1)

(use-package avy :ensure t)

;; Expand election in delimiters
(use-package expand-region :ensure t)

;; Helpful for examining bindings on the fly
(use-package command-log-mode :ensure t)

(use-package helpful
  :ensure t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; SELF-DISCOVERABILITY FEATURE. SHow commands that follow
;; the currently invoked binding
(use-package which-key
  :ensure t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.1)
  (setq which-key-show-early-on-C-h-delay 0.5)
  (define-key which-key-mode-map (kbd "C-x n") 'which-key-C-h-dispatch))

;; RAINBOW PARENS
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; THEME
(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
      doom-themes-enable-italic t)
  (load-theme 'doom-gruvbox t))

;; COUNSEL
;; particularly useful for switching themes: M-x counsel themes
(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
	 ("C-b" . counsel-ibuffer)
	 ("C-f" . counsel-find-file)
	 ("C-q" . counsel-descbinds)
   ("M-$" . (lambda () (interactive)
              (let ((ivy-height 25))
                (counsel-unicode-char))))))

(use-package counsel-projectile :ensure t)

;; SWIPER -- search through document
(use-package swiper :ensure t)

;; IVY completion framework in find files, etc.
;; More minimalisitic than helm.
(use-package ivy
  :ensure t
  :diminish
  :bind (("C-s" . (lambda () (interactive)
                    (let ((ivy-height 20))
                      (swiper))))
	:map ivy-minibuffer-map
	("TAB" . ivy-alt-done)
	("C-l" . ivy-alt-done)
	("C-j" . ivy-next-line)
	("C-k" . ivy-previous-line)
	("C-<return>" . find/open-in-new-frame)
	:map ivy-switch-buffer-map
	("C-k" . ivy-previous-line)
	("C-l" . ivy-done)
	("C-d" . ivy-switch-buffer-kill)
	("C-<return>" . find/open-in-new-frame)
	:map ivy-reverse-i-search-map
	("C-k" . ivy-previous-line)
	("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))
(use-package ivy-rich
  :ensure t
  :init
  (ivy-rich-mode 1))

(use-package company
  :ensure t
  :defer t
  :custom
  ;; Search other buffers with the same modes for completion instead of
  ;; searching all other buffers.
  (company-dabbrev-other-buffers t)
  (company-dabbrev-code-other-buffers t)
  ;; Only 2 letters required for completion to activate.
  (company-minimum-prefix-length 3)
  ;; Do not downcase completions by default.
  (company-dabbrev-downcase nil)
  ;; Even if I write something with the wrong case, provide the correct casing.
  (company-dabbrev-ignore-case t)
  ;; company completion wait
  (company-idle-delay 0.1)
  ;; No company-mode in shell & eshell
  (company-global-modes '(not eshell-mode shell-mode))
  ;; Use company with text and programming modes.
  :hook ((text-mode . company-mode)
         (prog-mode . company-mode))
  :config
  (setq company-tooltip-limit 20)
  (setq company-tooltip-maximum-width 50)
  (setq company-tooltip-maximum-height 12)
  (custom-set-faces
    '(company-tooltip ((t (:height 120))))
    '(company-tooltip-common ((t (:height 120))))))

(use-package company-quickhelp
  :ensure t
  :after company
  :config
  (company-quickhelp-mode 1)
  (setq company-quickhelp-max-lines 5)
  (setq company-quickhelp-delay 0.0))

(use-package eglot
  :ensure t
  :defer t
  :hook ((eglot-managed-mode . (lambda () (eglot-inlay-hints-mode -1)))
         (go-mode     . eglot-ensure)
         (c-mode      . eglot-ensure)
         (pipenv-mode . (lambda () (pipenv-activate) (eglot-ensure)))))
(use-package eldoc-box
  :ensure t
  :hook ((eldoc-mode . eldoc-box-hover-mode))
  :config (setq eldoc-box-max-pixel-width 400)
          (setq eldoc-box-max-pixel-height 200))

;; BEGIN LANGUAGES
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
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-position 'top-right-angle)
  (setq lsp-ui-doc-enable t))
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
  :mode ("\\.gs\\'" . js2-mode)
  :config
  (setq js2-basic-offset 2))
;; END JS

;; BEGIN OCAML
(use-package tuareg
  :ensure t
  :mode ("\\.ml[ily]?$" . tuareg-mode)
  :config
  (setq tuareg-prettify-symbols-full t)
  (add-hook 'tuareg-mode-hook 'merlin-mode)
  (add-hook 'caml-mode-hook 'merlin-mode))
(use-package merlin
  :ensure t
  :after tuareg
  :hook (tuareg-mode . merlin-mode)
  :config
  (setq merlin-command 'opam))
(use-package ocp-indent
  :ensure t
  :after tuareg)
(use-package utop
  :ensure t
  :config
  (setq utop-command "opam config exec -- utop -emacs")
  :hook (tuareg-mode . utop-minor-mode))
;; END OCAML
;; END LANGUAGES

;; CUSTOM MODE LINE
(use-package powerline-evil
  :ensure t
  :init 
  (setq powerline-height 25
        powerline-default-separator 'contour)
  :config (powerline-default-theme))

;; BETTER DIRED
(use-package dired-sidebar
  :ensure t
  :bind (("C-<tab>" . dired-sidebar-toggle-sidebar))
  :commands (dired-sidebar-toggle-sidebar)
  :config
  (setq dired-sidebar-theme 'nerd)
  (setq dired-sidebar-width 30)
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-modeline t)
  (setq dired-sidebar-custom-modeline-format "Dired Sidebar")
  (setq dired-sidebar-use-magit-integration t))
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-<return>")
    #'my-dired-find-file-other-frame))
(defun my-dired-find-file-other-frame ()
  "Open the file at point in a new frame."
  (interactive)
  (select-frame (make-frame))
  (dired-find-file))
