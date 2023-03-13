;; CUSTOM FUNCTIONS
(load-file "~/.config/emacs/functions-shell.el")
(load-file "~/.config/emacs/functions-find.el")

;; GLOBAL BINDINGS
(load-file "~/.config/emacs/bindings-global.el")

;; FRAMES
(load-file "~/.config/emacs/frames.el")

(setq inhibit-startup-message t)
(setq visible-bell t)

;; Inhibit bars
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(set-fringe-mode 10)

;; Rather than find-file, you can find recently edited files
(recentf-mode 1)

;; stop the backup files damnit
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)
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

;; Don't pop up UI dialog boxes
(setq use-dialog-box nil)

;; Refersh buffers when underlying files have changed.
;; i.e., suppose we change branches on git
(global-auto-revert-mode 1)
(setq auto-revert-ask-before-discard nil)
(setq global-auto-revert-non-file-buffers t)

;; FONT
(set-face-attribute 'default nil :font "Iosevka Nerd Font-12")

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
  (evil-set-undo-system 'undo-tree)
  (evil-mode 1))
(use-package evil-collection
  :after evil
  :config
  (setq evil-want-integration t)
  (evil-collection-init))
(use-package evil-mc
  :ensure t)
(global-evil-mc-mode 1)
(use-package evil-multiedit
  :ensure t)
(evil-multiedit-mode 1)

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
