(use-package emacs
  :init
  (load (expand-file-name "configs/packman.el" user-emacs-directory))
  (defvar org/master-org-file "~/org/MASTER.org"
    "Path to the master org file used as a persistent scratch pad.")
  :preface
  (defun org/open-master-org (f) "Open the persistent scratch file."
         (find-file f))
  (defun zig/enable-line-numbers () "Enable line numbers in programming modes."
    (when (derived-mode-p 'prog-mode) (display-line-numbers-mode 1)))


  :hook (prog-mode . zig/enable-line-numbers)
  :config
  (setq-default inhibit-startup-screen t
                make-backup-files      nil
                tab-width              2
                visible-bell           t
                auto-save-default      nil
                create-lockfiles       nil
                indent-tabs-mode       nil
                use-dialog-box         nil
                compilation-scroll-output t)

  (setq help-window-select t
        backup-directory-alist `((".*" . ,temporary-file-directory))
        auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
        display-line-numbers 'relative
        select-enable-clipboard t
        custom-file (locate-user-emacs-file "custom-vars.el"))

  (load custom-file 'noerror 'nomessage)

  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode -1)
  (column-number-mode)
  (global-hl-line-mode 1)
  (save-place-mode 1)
  (set-fringe-mode 10)
  (electric-pair-mode t)
  (global-auto-revert-mode t)

  (set-face-attribute 'default nil :font "Iosevka Nerd Font-14")
  (custom-set-faces
   '(hl-line
     ((t (:background nil :box (:line-width 1 :color "#103052" :style nil)))))
   '(line-number ((t (:foreground "#8090A0" :weight normal))))
   '(line-number-current-line ((t (:foreground "#4C772B" :weight bold))))
   '(echo-area ((t (:height 120))))
   '(mode-line ((t (:height 120)))))

  (load (expand-file-name "bindings.el"                    user-emacs-directory))
  (load (expand-file-name "configs/minibuffer-config.el"   user-emacs-directory))
  (load (expand-file-name "configs/misc-config.el"         user-emacs-directory))
  (load (expand-file-name "configs/org-config.el"          user-emacs-directory))
  (load (expand-file-name "configs/aesthetic-config.el"    user-emacs-directory))
  (load (expand-file-name "configs/evil-config.el"         user-emacs-directory))
  (load (expand-file-name "configs/discovery-config.el"    user-emacs-directory))
  (load (expand-file-name "configs/completion-config.el"   user-emacs-directory))
  (load (expand-file-name "configs/file-manager-config.el" user-emacs-directory))
  (load (expand-file-name "configs/langs-config.el"        user-emacs-directory))
  (load (expand-file-name "configs/tex-config.el"          user-emacs-directory))
  )

;; use compile (window behavior)
(use-package compile
  :bind (("H-c" . compile)
         ("H-r" . recompile)
         ("H-n" . next-error)))

;; use tab-bar (tabbing buffers)
(use-package tab-bar
  :ensure nil ; its built-in
  :config
  (tab-bar-mode 1)
  (setq tab-bar-format '(tab-bar-format-history
                          tab-bar-format-tabs
                          tab-bar-format-align-right))
  :bind
  (("H-2"  . tab-next)
   ("H-1"   . tab-previous)
   ("H-q"   . tab-close)
   ("H-SPC" . tab-new)))

;; use ziggy-mode (custom binding override)
(use-package ziggy-mode
  :ensure nil
  :load-path "modes/"
  :config
  (global-ziggy-mode 1)
  (bind-keys :map ziggy-mode-map
             ;; Window operations
             ("C-l" . windmove-right)
             ("C-h" . windmove-left)
             ("C-k" . windmove-up)
             ("C-j" . windmove-down)
             
             ;; Buffer operations
             ("C-." . next-buffer)
             ("C-," . previous-buffer)
             
             ;; Discovery operations (using helpful)
             ("C-/ 1" . helpful-key)
             ("C-/ 2" . helpful-function)
             ("C-/ 3" . helpful-variable)
             ("C-/ 4" . helpful-command)
             ("C-/ q" . helpful-macro)
             ("C-/ w" . helpful-symbol)
             ("C-/ e" . helpful-at-point)
             ("C-/ r" . helpful-callable)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(warning-suppress-types '((use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
