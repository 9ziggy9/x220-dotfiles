(defun yas/start-insert ()
  "Function to run before expanding a snippet."
  (when (evil-normal-state-p)
    (evil-insert-state)))

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1)
  :bind (:map yas-minor-mode-map ("C-c s" . yas-insert-snippet))
  :config
  (add-to-list 'yas-snippet-dirs "~/.config/emacs/snippets")
  (add-hook 'yas-before-expand-snippet-hook 'yas/start-insert)
  (with-eval-after-load 'yasnippet
      (define-key yas-keymap (kbd "<return>") 'yas-next-field-or-maybe-expand))
  (yas-reload-all))

(use-package command-log-mode
  :ensure t
  :init
  (setq command-log-mode-open-log-turns-on-mode t
        command-log-mode-auto-show t))
