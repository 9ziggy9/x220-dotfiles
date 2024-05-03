;; package management
(load (expand-file-name "configs/packman.el" user-emacs-directory))

;; some custom functions
(defvar org/master-org-file "~/org/MASTER.org"
  "Path to the master org file used as a persistent scratch pad.")
(defun org/open-master-org (f)
  "Open the persistent scratch file." (find-file f))
(defun my/log-debug-msg (s)
  "Logging for testing hooks and so forth"
  (message s))

;; use emacs
(use-package emacs ;; note that emacs is a built-in package
  ;; custom fns
  :preface
  (defun zig/shell-command (command)
    (interactive "sShell command: ")
    (let* ((output-buffer (get-buffer-create "*Shell Command Output*"))
          (command-output (shell-command-to-string command)))
      (with-current-buffer output-buffer
        (erase-buffer)
        (insert command-output))
      (display-buffer-pop-up-frame output-buffer
                                  '((width . 80) ; Adjust the width as needed
                                    (height . 20) ; Adjust the height as needed
                                    (left . center)
                                    (top . center)))))
  (defun zig/shell-command-on-region (start end command)
    (interactive "r\nsShell command on region: ")
    (let* ((input (buffer-substring-no-properties start end))
          (output-buffer (get-buffer-create "*Shell Command Output*"))
          (command-output (with-temp-buffer
                            (insert input)
                            (shell-command-on-region (point-min) (point-max)
                                                     command output-buffer nil)
                            (buffer-string))))
      (with-current-buffer output-buffer
        (erase-buffer)
        (insert command-output))
      (display-buffer-pop-up-frame output-buffer
                                  '((width . 80)  ; Adjust the width as needed
                                    (height . 20) ; Adjust the height as needed
                                    (left . center)
                                    (top . center)))))
  (defun zig/balance-windows-horizontally ()
    (let ((windows (window-list))
          (frame-width (frame-width)))
      (dolist (window windows)
        (let ((window-width (floor (/ frame-width (length windows)))))
          (window-resize window (- window-width (window-width window)) t)))))

  ;; shortcut to config
  :bind
  ("<f12>" . (lambda () (interactive)
               (let ((config-dir (expand-file-name "~/.config/emacs/")))
                 (find-file (read-file-name "configs: " config-dir)))))
  ;; cleaner ui
  :config
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode -1)
  ;; sane defaults
  (setq-default inhibit-startup-screen t
                make-backup-files nil
                tab-width 2
                visible-bell t
                auto-save-default nil
                create-lockfiles nil
                indent-tabs-mode nil
                use-dialog-box nil
                compilation-scroll-output t)

  ;; start up hooks
  (add-hook 'emacs-startup-hook
            (lambda () (my/log-debug-msg "Welcome to emacs, ziggy!")))

  ;; small margins
  (set-fringe-mode 10)
  ;; auto focus new windows
  (setq help-window-select t)
  ;; prevent save file clutter
  (setq backup-directory-alist
    `((".*" . ,temporary-file-directory)))
  (setq auto-save-file-name-transforms
    `((".*" ,temporary-file-directory t)))
  ;; display buffer alist
  (setq-default display-buffer-alist
                '(("\\*compilation\\*"         . ((display-buffer-reuse-window
                                                   display-buffer-same-window)
                                                  (reusable-frames . visible)))
                  ("\\*Help\\*"                . ((display-buffer-reuse-window
                                                   display-buffer-pop-up-frame) 
                                                  (reusable-frames . t)))
                  ("\\*Backtrace\\*"           . ((display-buffer-reuse-window
                                                   display-buffer-pop-up-frame) 
                                                  (reusable-frames . t)))
                  ("\\*Warnings\\*"            . ((display-buffer-reuse-window
                                                   display-buffer-pop-up-frame) 
                                                  (reusable-frames . t)))
                  ("\\*Compile-Log\\*"         . ((display-buffer-reuse-window
                                                   display-buffer-pop-up-frame) 
                                                  (reusable-frames . t)))
                  ("\\*Flycheck errors\\*"     . ((display-buffer-reuse-window
                                                   display-buffer-pop-up-frame) 
                                                  (reusable-frames . t)))
                  ("\\*Calendar\\*"            . ((display-buffer-reuse-window
                                                   display-buffer-pop-up-frame) 
                                                  (reusable-frames . t)))
                  ("\\*scratch\\*"             . ((display-buffer-reuse-window
                                                   display-buffer-pop-up-frame) 
                                                  (reusable-frames . t)))
                  ("\\*Org Agenda\\*"          . ((display-buffer-reuse-window
                                                   display-buffer-pop-up-frame) 
                                                  (reusable-frames . t)))
                  ("\\*info\\*"                . ((display-buffer-reuse-window
                                                   display-buffer-pop-up-frame) 
                                                  (reusable-frames . t)))
                  ("\\*Man .*\\*"              . ((display-buffer-reuse-window
                                                   display-buffer-pop-up-frame) 
                                                  (reusable-frames . t)))
                  ("\\*shell\\*"               . ((display-buffer-reuse-window
                                                   display-buffer-pop-up-frame) 
                                                  (reusable-frames . t)))
                  ("\\*grep\\*"                . ((display-buffer-reuse-window
                                                   display-buffer-pop-up-frame) 
                                                  (reusable-frames . t)))
                  ("\\*Occur\\*"               . ((display-buffer-reuse-window
                                                   display-buffer-pop-up-frame) 
                                                  (reusable-frames . t)))
                  ("\\*Shell Command Output\\*" . ((display-buffer-reuse-window
                                                    display-buffer-pop-up-frame) 
                                                   (reusable-frames . t)))
                  ("\\*Async Shell Command\\*" . ((display-buffer-reuse-window
                                                   display-buffer-pop-up-frame) 
                                                  (reusable-frames . t)))
                  ("\\*Helpful .*\\*"          . ((display-buffer-reuse-window
                                                   display-buffer-pop-up-frame)
                                                  (reusable-frames . t)))))
  ;; lines
  (column-number-mode)
  (setq display-line-numbers 'relative)
  (global-display-line-numbers-mode t)
  (global-hl-line-mode 1)
  ;; command history
  (setq history-length 100)
  (savehist-mode 1)
  (save-place-mode 1)
  ;; custom variable cleanliness
  ;; prevents customization varibles being put into out init.el!
  ;; keeps a nice clean init.el, with automatic code generated from
  ;; things such as use-package.
  (setq custom-file (locate-user-emacs-file "custom-vars.el"))
  (load custom-file 'noerror 'nomessage)
  ;; font
  ;; TODO: broken because env, should be fine when made default
  ;; (set-face-attribute 'default nil :font "Iosevka Nerd Font-14")
  (set-face-attribute 'default nil :font "Iosevka Nerd Font-14")
  (custom-set-faces
   '(hl-line ((t (:background nil
                              :box
                              (:line-width 1 :color "#00FF77" :style nil)))))
   '(line-number ((t (:foreground "#8090A0" :weight normal))))
   '(line-number-current-line ((t (:foreground "#ebedef" :weight bold))))
   '(echo-area ((t (:height 120))))
   '(minibuffer-prompt ((t (:height 150))))
   '(mode-line ((t (:height 120)))))
  ;; autopairs
  (electric-pair-mode t)
  ;; automatically refresh buffers when files change on disk
  (global-auto-revert-mode t)
  ;; use system clipboard
  (setq select-enable-clipboard t)
  ;; shell cmd
  (global-set-key (kbd "M-!") 'zig/shell-command)
  (global-set-key (kbd "M-|") 'zig/shell-command-on-region)
  ;; windows
  (global-set-key (kbd "M-q") 'delete-window)
  (global-set-key (kbd "C-<return>") '(lambda () (interactive)
                                        (split-window-right)
                                        (other-window 1)
                                        (zig/balance-windows-horizontally)))
  (global-set-key (kbd "H-<tab>") 'previous-window-any-frame)

  ;; zoom
  (global-set-key (kbd "C-=") 'text-scale-increase)
  (global-set-key (kbd "C-\-") 'text-scale-decrease))

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
  (("H-2"            . tab-next)
  ("H-1"            . tab-previous)
  ("H-q"            . tab-close)
  ("H-SPC"     . tab-new)))

;; imports
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

;; use ziggy-mode (my code)
(use-package ziggy-mode
  :ensure nil
  :load-path "modes/"
  :config
  (ziggy-mode 1))
