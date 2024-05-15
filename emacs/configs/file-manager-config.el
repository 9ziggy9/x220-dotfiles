(use-package general
  :ensure t
  :config
  (general-evil-setup t))

(use-package all-the-icons :ensure t)

(use-package image-dired :ensure nil)
(use-package dirvish
  :ensure t
  :preface
  (defun zig/disable-line-numbers ()
    (display-line-numbers-mode -1))
  :init
  (dirvish-override-dired-mode)
  :bind (("H-f" . dirvish))
  :custom
  (dirvish-attributes '(all-the-icons
                        file-size
                        subtree-state)
                      )
  (dirvish-reuse-session t))

(general-define-key
 :states    'normal
 :keymaps   'dirvish-mode-map
 "h"        'dired-up-directory
 "<escape>" 'dirvish-quit)

(general-define-key
 :states '(normal insert visual emacs)
 :keymaps 'override
 "M-x" 'execute-extended-command)
