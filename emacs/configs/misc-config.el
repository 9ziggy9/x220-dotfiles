(use-package command-log-mode
  :ensure t
  :init
  (setq command-log-mode-open-log-turns-on-mode t
        command-log-mode-auto-show t))

(use-package expand-region :ensure t)

(use-package centered-cursor-mode :demand :config (global-centered-cursor-mode))

(use-package posframe
  :ensure t
  :preface
  (defun zig/in-posframe (buffer &optional _alist)
    "Display the BUFFER in a posframe and focus it for interaction."
    (posframe-show buffer
                   :position (point)
                   :poshandler 'posframe-poshandler-frame-center
                   :width 120
                   :height 40
                   :border-width 2
                   :border-color "#003300"
                   :background-color (face-attribute 'default :background)
                   :accept-focus t)
    (when-let ((win (get-buffer-window buffer)))
      (select-window win)
      (select-frame-set-input-focus (window-frame win))) buffer)


  :config
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
  )


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
