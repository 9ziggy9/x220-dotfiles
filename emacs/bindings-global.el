;; Global unset
(global-unset-key (kbd "C-j"))
(global-unset-key (kbd "C-k"))
(global-unset-key (kbd "C-n"))
(global-unset-key (kbd "C-u"))
(global-unset-key (kbd "M-c"))
(global-unset-key (kbd "C-h"))
(global-unset-key (kbd "C-l"))

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

;; SHELL
(global-set-key (kbd "C-\\") 'shell/ex-on-reg-and-frame)
