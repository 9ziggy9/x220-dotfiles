(defun org/save-top-level-heading()
  "Save all top-level headings in the current Org file to individual Org files."
  (interactive)
  (let ((output-dir "~/org/"))
    (unless (file-exists-p output-dir) (make-directory output-dir t))
    (org-map-entries (lambda ()
      (let* ((heading (org-get-heading t t t t))
             (filename
              (concat output-dir (replace-regexp-in-string "[/\\?%*:|\"<>]" "_" heading) ".org")))
        (org-mark-subtree)
        (write-region (point) (mark) filename)
        (deactivate-mark))) "LEVEL=1" 'file)))

(defun org/save-master ()
  "Check if current file is MASTER.org and save"
  (when (string= (buffer-file-name) (expand-file-name org/master-org-file))
        (let ((org-confirm-babel-evaluate nil))
          (org-babel-tangle))))

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

  (org/font-setup)

  (add-hook 'org-mode-hook (lambda ()
                             (add-hook 'after-save-hook #'org/save-master))))

(use-package org-bullets
  :ensure t
  :after org
  :hook (org-mode . org-bullets-mode)
  :config
  (setq org-bullets-bullet-list
        '("◉" "○" "●" "○" "●" "○" "●")))
