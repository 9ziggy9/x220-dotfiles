(use-package evil
  :ensure t
  :diminish evil-mode
  :hook (after-init . evil-mode)
  :bind (("<escape>" . keyboard-escape-quit))
  :preface
  (defun ian/save-and-kill-this-buffer ()
    (interactive)
    (save-buffer)
    (kill-this-buffer))
  :config
  (setq evil-shift-width 2)
  (with-eval-after-load 'evil-maps ;; avoid conflcits with tooltips
    (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
    (define-key evil-normal-state-map (kbd "-")   'evil-join)
    (define-key evil-normal-state-map (kbd "C")   'comment-line)
    (define-key evil-visual-state-map (kbd "C")   'comment-or-uncomment-region)
    (define-key evil-visual-state-map (kbd "a")   'align-regexp)
    (define-key evil-normal-state-map (kbd "u")   'undo-fu-only-undo)
    (define-key evil-normal-state-map (kbd "C-r") 'undo-fu-only-redo))
  (evil-ex-define-cmd "q" #'kill-this-buffer)
  (evil-ex-define-cmd "wq" #'ian/save-and-kill-this-buffer))

(use-package vimish-fold
  :ensure t
  :after evil
  :preface
  (defun zig/vimish-fold-region ()
    (interactive)
    (when (region-active-p)
      (let ((beg (region-beginning)) (end (region-end)))
        (if (= (char-before end) ?\n) (setq end (1- end)))
        (vimish-fold beg end)
        (evil-normal-state))))
  :config
  (vimish-fold-global-mode 1)
  (setq vimish-fold-persist-on-saving t
        vimish-fold-include-last-empty-line t)
  (define-key evil-visual-state-map (kbd "<tab>")   'zig/vimish-fold-region)
  (define-key evil-normal-state-map (kbd "<tab>")   'vimish-fold-toggle)
  (define-key evil-normal-state-map (kbd "M-<tab>") 'vimish-fold-delete))

(use-package evil-vimish-fold
  :ensure t
  :after (evil vimish-fold)
  :config
  (evil-vimish-fold-mode 1))

(use-package evil-mc
  :ensure t
  :diminish evil-mc-mode
  :config
  (global-evil-mc-mode 1)
  (define-key evil-visual-state-map
              (kbd "<return>") 'evil-mc-make-cursor-in-visual-selection-beg)
  (add-hook 'evil-visual-state-entry-hook 'zig/clear-all-cursors))

(use-package evil-multiedit
  :ensure t
  :diminish evil-multiedit-mode
  :config
  (evil-multiedit-mode 1)
  (define-key evil-motion-state-map (kbd "L") nil)
  (define-key evil-normal-state-map (kbd "L") 'evil-multiedit-match-and-next))

(use-package undo-fu
  :ensure t
  :after evil
  :config
  (setq undo-fu-ignore-keyboard-quit t)
  (setq undo-fu-allow-undo-in-region t))

;; user defined functions
(defun zig/clear-all-cursors () (interactive)
  (when (and (bound-and-true-p evil-mc-mode) (evil-mc-has-cursors-p))
    (evil-mc-undo-all-cursors)))
