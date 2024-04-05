(use-package vimish-fold
  :ensure t
  :after evil
  :config
  (vimish-fold-global-mode 1)
  (evil-define-key 'normal global-map
    "zf" 'vimish-fold
    "zd" 'vimish-fold-delete
    "zo" 'vimish-fold-unfold
    "zO" 'vimish-fold-unfold-all
    "zc" 'vimish-fold-refold
    "zC" 'vimish-fold-refold-all
    "za" 'vimish-fold-toggle
    "zA" 'vimish-fold-toggle-all))

(use-package evil
  :ensure t
  :demand t
  :diminish evil-mode
  :bind (("<escape>" . keyboard-escape-quit))
  :init
  ;; (setq evil-want-keybinding nil)
  :config
  (add-to-list 'evil-fold-list
               '((vimish-fold-mode)
                 :open-all vimish-fold-unfold-all
                 :close-all vimish-fold-refold-all
                 :toggle vimish-fold-toggle
                 :open vimish-fold-unfold
                 :close vimish-fold-refold))
  ;; (setq evil-escape-key-sequence "C")
  (setq evil-shift-width 2)
  ;; (setq evil-jump-cross-buffers t)
  ;; (evil-set-undo-system 'undo-tree)
  (evil-mode 1)
  (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
  (define-key evil-normal-state-map (kbd "-")   'evil-join)
  (define-key evil-normal-state-map (kbd "C")   'comment-line)
  (define-key evil-visual-state-map (kbd "C")   'comment-or-uncomment-region)
  (define-key evil-visual-state-map (kbd "a")   'align-regexp)
  (define-key evil-normal-state-map (kbd "u")   'undo-fu-only-undo)
  (define-key evil-normal-state-map (kbd "C-r") 'undo-fu-only-redo))
  (dolist (state '(normal insert visual motion operator))
    (let ((map (intern (format "evil-%s-state-map" state))))
      (define-key (eval map) (kbd "C-e") nil)))

(use-package evil-mc
  :ensure t
  :diminish evil-mc-mode
  :config
  (global-evil-mc-mode 1)
  (define-key evil-visual-state-map (kbd "<return>") 'evil-mc-make-cursor-in-visual-selection-beg)
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
  (when (and (bound-and-true-p evil-mc-mode)
             (evil-mc-has-cursors-p))
    (evil-mc-undo-all-cursors)))
