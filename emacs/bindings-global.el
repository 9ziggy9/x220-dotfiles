;; Global unset
(global-unset-key (kbd "C-j"))
(global-unset-key (kbd "C-k"))
(global-unset-key (kbd "C-n"))

;; Make ESC quit prompt
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; ZOOM IN
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C-\-") 'text-scale-decrease)

;; FIND FILE
(global-set-key (kbd "C-f") 'counsel-find-file)

;; BUFFERS
(global-set-key (kbd "M-<tab>") 'mode-line-other-buffer)
(global-set-key (kbd "M-k") 'kill-buffer-and-window)
(global-set-key (kbd "C-j") 'windmove-down)
(global-set-key (kbd "C-k") 'windmove-up)