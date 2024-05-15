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

(use-package expand-region
  :ensure t
  :bind (("H-e" . er/expand-region))
  :config)

(use-package centered-cursor-mode
  :demand
  :config
  (global-centered-cursor-mode))

;; (use-package posframe
;;   :ensure t
;;   :preface
;;   (defun zig/display-in-posframe (buffer &optional _alist)
;;     "Display the compilation BUFFER in a posframe."
;;     (let ((posframe (posframe-show buffer
;;                                    :position (point)
;;                                    :poshandler 'posframe-poshandler-frame-center
;;                                    :width 140
;;                                    :height 40
;;                                    :border-width 3
;;                                    :border-color "#003300"
;;                                    :accept-focus t)))
;;       (select-frame-set-input-focus posframe)))
;;   :config
;;   (global-set-key (kbd "H-q") 'posframe-hide-all)
;;   (setq display-buffer-alist
;;         '(("\\*compilation\\*" (zig/display-in-posframe))
;;           ("\\*Help\\*"        (zig/display-in-posframe))
;;           ("\\*Warnings\\*"    (zig/display-in-posframe))
;;           ("\\*Calendar\\*"    (zig/display-in-posframe))
;;           ("\\*Man .*\\*"      (zig/display-in-posframe))
;;           ("\\*Helpful .*\\*"  (zig/display-in-posframe)))))
(use-package posframe
  :ensure t
  :config
  (defun zig/in-posframe (buffer &optional _alist)
    "Display the manpage BUFFER in a posframe."
    (posframe-show buffer
                   :position (point)
                   :poshandler 'posframe-poshandler-frame-center
                   :width 120
                   :height 40
                   :border-width 2
                   :border-color "#003300"
                   :background-color (face-attribute 'default :background)
                   :accept-focus t)
    (select-frame-set-input-focus (window-frame (get-buffer-window buffer)))
    buffer)
  (setq display-buffer-alist
        '(
          ("\\*Man .*\\*"      (zig/in-posframe . nil)
                               (display-buffer-no-window . nil))
          ("\\*Calendar\\*"    (zig/in-posframe . nil)
                               (display-buffer-no-window . nil))
          ("\\*compilation\\*" (zig/in-posframe . nil)
                               (display-buffer-no-window . nil))
          ("\\*Warnings\\*"    (zig/in-posframe . nil)
                               (display-buffer-no-window . nil))
          ("\\*Helpful .*\\*"  (zig/in-posframe . nil)
                               (display-buffer-no-window . nil))
          ))

  (global-set-key (kbd "H-q") 'posframe-hide-all))


(use-package vertico-posframe
  :after vertico
  :ensure t
  :preface
  :config
  (vertico-posframe-mode 1)
  (setq vertico-posframe-poshandler 'posframe-poshandler-frame-center
        vertico-posframe-width 140
        vertico-posframe-height 40
        vertico-posframe-border-width 2
        vertico-posframe-parameters '((left-fringe . 8) (right-fringe . 8)))
  )
