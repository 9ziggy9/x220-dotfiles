(defun org/font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•")))))))

(use-package org
  :ensure t  ; Make sure org is installed from ELPA/MELPA
  :pin org   ; Prefer Org mode from org's package repository
  :mode ("\\.org\\'" . org-mode)  ; Automatically use org-mode for .org files
  :hook
  (org-mode . (lambda ()
                (org-indent-mode)
                (local-unset-key (kbd "C-<return>"))))
  :bind (:map org-mode-map
              ("<tab>" . org-cycle)
              ("C-c c" . org-edit-special))
  :config
  (with-eval-after-load 'org

    (org-babel-do-load-languages
        'org-babel-load-languages
        '((emacs-lisp . t)
          (python     . t)))

    (push '("conf-unix" . conf-unix) org-src-lang-modes))

  (setq org-log-done t
        org-ellipsis " ▾"
        org-src-tab-acts-natively t
        org-startup-folded 'fold
        org-src-fontify-natively t
        org-src-preserve-indentation t
        org-edit-src-content-indentation 0
        org-src-window-setup 'other-frame
        org-confirm-babel-evaluate nil)
  (org/font-setup))

(use-package org-bullets
  :ensure t
  :after org
  :hook (org-mode . org-bullets-mode)
  :config
  (setq org-bullets-bullet-list
        '("◉" "○" "●" "○" "●" "○" "●")))

(use-package toc-org
  :ensure t
  :after org
  :hook (org-mode . toc-org-enable))
