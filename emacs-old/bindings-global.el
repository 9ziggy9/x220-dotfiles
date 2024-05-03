;; Global unset
(global-unset-key (kbd "C-<return>"))
(global-unset-key (kbd "C-j"))
(global-unset-key (kbd "C-k"))
(global-unset-key (kbd "C-n"))
(global-unset-key (kbd "C-u"))
(global-unset-key (kbd "M-c"))
(global-unset-key (kbd "C-h"))
(global-unset-key (kbd "C-l"))
(global-unset-key (kbd "M-d"))
(global-unset-key (kbd "M-$"))

;; Describe/help functions
(global-set-key (kbd "M-d f") 'describe-function)
(global-set-key (kbd "M-d v") 'describe-variable)
(global-set-key (kbd "M-d c") 'describe-command)
(global-set-key (kbd "M-d m") 'describe-mode)
(global-set-key (kbd "M-d b") 'describe-bindings)

;; Make ESC quit prompt
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; ZOOM IN
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C-\-") 'text-scale-decrease)

;; BUFFERS
(global-set-key (kbd "M-<tab>") 'mode-line-other-buffer)
(global-set-key (kbd "M-q") 'kill-buffer-and-window)
(global-set-key (kbd "M-e") 'eval-buffer)
(global-set-key (kbd "M-c") 'compile)
(global-set-key (kbd "C-h") 'windmove-left)
(global-set-key (kbd "C-l") 'windmove-right)
(global-set-key (kbd "C-j") 'windmove-down)
(global-set-key (kbd "C-k") 'windmove-up)
(global-set-key (kbd "C-d") 'evil-scroll-down)
(global-set-key (kbd "C-u") 'evil-scroll-up)
(global-set-key (kbd "C-<return>") #'(lambda () (interactive)
                                      (split-window-right)
                                      (other-window 1)
                                      (zig/balance-windows-horizontally)))
(global-set-key (kbd "H-<tab>") 'previous-window-any-frame)

;; SHELL
(global-set-key (kbd "C-\\") 'shell/ex-on-reg-and-frame)

;; ZOOM
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C-\-") 'text-scale-decrease)

(defun zig/balance-windows-horizontally ()
  "Balance window widths equally."
  (let ((windows (window-list))
        (frame-width (frame-width)))
    (dolist (window windows)
      (let ((window-width (floor (/ frame-width (length windows)))))
        (window-resize window (- window-width (window-width window)) t)))))
