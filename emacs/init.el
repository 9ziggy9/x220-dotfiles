(setq inhibit-startup-message t)
(setq visible-bell t)

;; Rice
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

;; EVIL MODE
(use-package evil
  :ensure t
  :config
  (evil-ex-define-cmd "q" (lambda () (interactive)
			    (kill-this-buffer) (delete-frame)))
  (evil-mode 1))

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
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history)))

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
	 :map ivy-switch-buffer-map
	 ("C-k" . ivy-previous-line)
	 ("C-l" . ivy-done)
	 ("C-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
	 ("C-k" . ivy-previous-line)
	 ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

;; FRAMES / TILES
;; This code tells Emacs to open all new windows in new frames,
;; and to reuse existing frames if possible.
;; NOTE: this configuration can cause Emacs to create a large number
;; of frames if you frequntly open and close windows.
(setq display-buffer-alist
      '(;; Open all new windows in new frames
        ("." . ((display-buffer-reuse-window display-buffer-pop-up-frame)
                (inhibit-same-window . t)
                (frame . nil)))))

;;;;;;;;;;;;;;
;; BINDINGS ;;
;;;;;;;;;;;;;;

;; BEGIN GLOBAL ;;

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;; ZOOM IN
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C-\-") 'text-scale-decrease)

;; END GLOBAL ;;
