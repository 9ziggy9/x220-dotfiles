;; package management
(load (expand-file-name "configs/packman.el" user-emacs-directory))

;; PKG: EMACS
;; base configuration
(use-package emacs ;; note that emacs is a built-in package
  :ensure t
  :config
  ;; disable ugly GUI elements
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode -1)

  (setq-default inhibit-startup-screen t
                make-backup-files nil
                tab-width 2
                visible-bell t
                auto-save-default nil
                create-lockfiles nil
                indent-tabs-mode nil
                use-dialog-box nil
                compilation-scroll-output t)

  (set-fringe-mode 10) ;; add small margins in editor.
  (setq help-window-select t) ;; auto focus help window wwhen opened

  ;; prevent save file clutter
  (setq backup-directory-alist
    `((".*" . ,temporary-file-directory)))
  (setq auto-save-file-name-transforms
    `((".*" ,temporary-file-directory t)))
  (setq undo-tree-history-directory-alist
        `(("." . "~/.config/emacs/.undo-tree-history")))

  ;; windows to frame
  (setq-default display-buffer-alist
                '(("\\*compilation\\*" . (display-buffer-pop-up-frame . nil))
                  ("\\*Messages\\*" . (display-buffer-pop-up-frame . nil))
                  ("\\*Help\\*" . (display-buffer-pop-up-frame . nil))
                  ("\\*Backtrace\\*" . (display-buffer-pop-up-frame . nil))
                  ("\\*Warnings\\*" . (display-buffer-pop-up-frame . nil))
                  ("\\*Compile-Log\\*" . (display-buffer-pop-up-frame . nil))
                  ("\\*Flycheck errors\\*" . (display-buffer-pop-up-frame . nil))
                  ("\\*Calendar\\*" . (display-buffer-pop-up-frame . nil))
                  ("\\*scratch\\*" . (display-buffer-pop-up-frame . nil))
                  ("\\*Org Agenda\\*" . (display-buffer-pop-up-frame . nil))
                  ("\\*info\\*" . (display-buffer-pop-up-frame . nil))
                  ;; Additional entries for helpful command buffers
                  ("\\*Man .*\\*" . (display-buffer-pop-up-frame . nil)) ;; Man pages
                  ("\\*shell\\*" . (display-buffer-pop-up-frame . nil)) ;; Shell buffer
                  ("\\*grep\\*" . (display-buffer-pop-up-frame . nil)) ;; Grep results
                  ("\\*Occur\\*" . (display-buffer-pop-up-frame . nil)) ;; Occur results
                  ("\\*Async Shell Command\\*" . (display-buffer-pop-up-frame . nil)) ;; Async commands
                  ("\\*Helpful .*\\*" . (display-buffer-pop-up-frame . nil)))) ;; Helpful buffer (if you use the `helpful` package)

  ;; lines
  (column-number-mode)
  (setq display-line-numbers-type 'relative)
  (global-display-line-numbers-mode t)

  ;; command history
  (setq history-length 100)
  (savehist-mode 1)
  (save-place-mode 1)

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
  '(echo-area ((t (:height 120))))
  '(minibuffer-prompt ((t (:height 150))))
  '(mode-line ((t (:height 120)))))

  ;; autopairs
  (electric-pair-mode t)

  ;; automatically refresh buffers when files change on disk
  (global-auto-revert-mode t)

  ;; use system clipboard
  (setq x-select-enable-clipboard t)

  ;; make new frame
  (global-set-key (kbd "C-s-<return>") 'make-frame-command)

  ;; shell cmd
  (global-set-key (kbd "M-!") 'my-shell-command)
  (global-set-key (kbd "M-|") 'my-shell-command-on-region)


  ;; zoom
  (global-set-key (kbd "C-=") 'text-scale-increase)
  (global-set-key (kbd "C-\-") 'text-scale-decrease))

;; compile window behavior
(use-package compile
  :bind ("C-c C-c" . compile))

(use-package tab-bar
  :ensure nil ; its built-in
  :config
  (tab-bar-mode 1)
  (setq tab-bar-format '(tab-bar-format-history
                          tab-bar-format-tabs
                          tab-bar-format-align-right))
  :bind
  (("C-0"            . tab-next)
   ("C-9"            . tab-previous)
   ("C-q"            . tab-close)
   ("C-<return>"     . tab-new)))
;; END: EMACS

(load (expand-file-name "configs/aesthetic-config.el"    user-emacs-directory))
(load (expand-file-name "configs/evil-config.el"         user-emacs-directory))
(load (expand-file-name "configs/discovery-config.el"    user-emacs-directory))
(load (expand-file-name "configs/completion-config.el"   user-emacs-directory))
(load (expand-file-name "configs/file-manager-config.el" user-emacs-directory))
(load (expand-file-name "configs/langs-config.el"        user-emacs-directory))

;; CUSTOM BINDINGS
(use-package ziggy-mode
  :ensure nil
  :load-path "modes/"
  :config
  (ziggy-mode 1))

(defun my-shell-command (command)
  "Execute a shell command COMMAND and display the output in a new centered frame."
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

(defun my-shell-command-on-region (start end command)
  "Execute a shell command on the selected region and display the output in a new centered frame."
  (interactive "r\nsShell command on region: ")
  (let* ((input (buffer-substring-no-properties start end))
         (output-buffer (get-buffer-create "*Shell Command Output*"))
         (command-output (with-temp-buffer
                           (insert input)
                           (shell-command-on-region (point-min) (point-max) command output-buffer nil)
                           (buffer-string))))
    (with-current-buffer output-buffer
      (erase-buffer)
      (insert command-output))
    (display-buffer-pop-up-frame output-buffer
                                 '((width . 80)  ; Adjust the width as needed
                                   (height . 20) ; Adjust the height as needed
                                   (left . center)
                                   (top . center)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;             use-package keyword synopsis               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The `use-package` macro in Emacs Lisp is used for concise and organized
;; management of package configurations. Below are some of its common keywords:

;; :ensure
;; Ensures that the package is installed. If the package is not available,
;; use-package will attempt to install it from a configured repository.
;; Example: (use-package some-package :ensure t)

;; :init
;; Code in this section is executed before the package is loaded.
;; It's used for setting variables or running code that needs to be in place
;; before the package's own initialization.
;; Example: (use-package some-package :init (setq some-var t))

;; :config
;; Code in this section is executed after the package is loaded.
;; This is where you put the bulk of your configuration code.
;; Example: (use-package some-package :config (some-function-setting))

;; :bind
;; Defines keybindings for the package.
;; It can be used globally or within specific modes.
;; Example: (use-package some-package :bind (("C-c a" . some-action)))

;; :hook
;; Attaches functions to hooks.
;; Useful for running code when certain modes or events are activated.
;; Example: (use-package some-package :hook (some-mode . some-function))

;; :mode
;; Associates file extensions with a mode provided by the package.
;; Example: (use-package some-package :mode ("\\.ext\\'" . some-mode))

;; :defer
;; Delays loading of the package until it's needed.
;; Useful for reducing startup time.
;; Example: (use-package some-package :defer t)

;; :demand
;; Use :demand for essential packages that you want loaded
;; immediately and are critical to your Emacs setup.
;; By default, use-package will defer loading of a package
;; until it is needed.
;; Example: (use-package some-package :demand t)

;; :commands
;; Declares commands as autoloads.
;; Like :defer, this is for delaying loading of the package.
;; It makes specific functions available as autoloads and loads the package
;; when they are invoked.
;; Example: (use-package some-package :commands (some-command))

;; :disabled
;; Disables loading of the package.
;; Useful for temporarily turning off a configuration.
;; Example: (use-package some-package :disabled t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Note: This is not an exhaustive list. use-package has many more
;; keywords for advanced configurations and use cases.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
