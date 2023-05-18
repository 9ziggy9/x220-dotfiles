;; CUSTOM FUNCTIONS
(load-file "~/.config/emacs/functions-shell.el")
(load-file "~/.config/emacs/functions-find.el")

;; GLOBAL BINDINGS
(load-file "~/.config/emacs/bindings-global.el")

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

;; AUTOPAIRS
(electric-pair-mode t)

;; USE SYSTEM CLIPBOARD
(setq x-select-enable-clipboard t)

;; PACKAGE MANAGEMENT
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

;; UNDO TREE
(use-package undo-tree
  :config
  (global-undo-tree-mode))

;; EVIL MODE
(use-package evil
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
  (evil-mode 1))
(use-package evil-collection
  :after evil
  :config
  (setq evil-want-integration t)
  (setq avy-all-windows 'all-frames)
  (setq avy-all-windows-alt t)
  (setq avy-background t)
  (evil-collection-init))
(use-package evil-mc
  :ensure t)
(global-evil-mc-mode 1)
(use-package evil-multiedit
  :ensure t)
(evil-multiedit-mode 1)

(use-package avy
  :ensure t)

;; Expand election in delimiters
(use-package expand-region
  :ensure t)

;; Helpful for examining bindings on the fly
(use-package command-log-mode
  :ensure t)

(use-package helpful
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
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.1)
  (setq which-key-show-early-on-C-h-delay 0.5)
  (define-key which-key-mode-map (kbd "C-x n") 'which-key-C-h-dispatch))

;; RAINBOW PARENS
(use-package rainbow-delimiters
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
  :bind (("M-x" . counsel-M-x)
	 ("C-b" . counsel-ibuffer)
	 ("C-f" . counsel-find-file)))
(use-package counsel-projectile)

;; SWIPER -- search through document
(use-package swiper)

;; IVY completion framework in find files, etc.
;; More minimalisitic than helm.
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
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
	:map ivy-reverse-i-search-map
	("C-k" . ivy-previous-line)
	("C-d" . ivy-reverse-i-search-kill))
    :config
    (ivy-mode 1))
(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package company
  :ensure t
  :defer t
  :init 
  (global-company-mode)
  (setq company-idle-delay 0.25)
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

;; LANGUAGES
;; Note, I do NOT like starting LSP by default. Must start lsp-mode manually.
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init (setq lsp-keymap-prefix "C-c l")
  :hook (go-mode . lsp-deferred)
  :config
  (setq lsp-prefer-flymake nil)
  (lsp-enable-which-key-integration t)
  (add-hook 'lsp-mode-hook
            (lambda () (remove-hook 'before-save-hook #'lsp-format-buffer))))

(use-package ccls
  :hook ((c-mode) . (lambda () (require 'ccls) (lsp))))

(use-package go-mode
  :mode "\\.go\\'"
  :config
  (add-hook 'go-mode-hook #'lsp-deferred))

(use-package python-mode
  :ensure t
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :config
  (setq python-indent-offset 4
        python-shell-interpreter-args "-i --simple-prompt")
  (add-hook 'python-mode-hook 'company-mode))

(use-package typescript-mode
  :ensure t
  :mode (("\\.tsx?\\'" . typescript-mode))
  :config
  ;; Set indentation style
  (setq typescript-indent-level 2))
