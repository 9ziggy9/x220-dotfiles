(use-package dirvish
  :ensure t
  :init
  (dirvish-override-dired-mode)
  :config
  (global-set-key (kbd "H-f") 'dirvish))

(use-package general
  :ensure t
  :config
  (general-evil-setup t))

(general-define-key
 :states    'normal
 :keymaps   'dirvish-mode-map
 "h"        'dired-up-directory
 "<escape>" 'dirvish-quit)

(general-define-key
 :states '(normal insert visual emacs)
 :keymaps 'override
 "M-x" 'execute-extended-command)

(use-package all-the-icons
  :ensure t)

(use-package image-dired
  :ensure nil) ;; image-dired is part of Emacs, no need to install

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install))
