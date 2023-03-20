;; EXAMPLE of binding to lambda expression
;;(define-key evil-normal-state-map (kbd "C-p") (lambda ()
;;						(interactive)
;;						(message "Hello, world!")))

;; Unbind
(define-key evil-motion-state-map (kbd "C-f") nil)
(define-key evil-motion-state-map (kbd "C-p") nil)
(define-key evil-motion-state-map (kbd "!") nil)
(define-key evil-motion-state-map (kbd "K") nil)
(define-key evil-motion-state-map (kbd "L") nil)
(define-key evil-motion-state-map (kbd "H") nil)
(define-key evil-motion-state-map (kbd "SPC") nil)
(define-key evil-normal-state-map (kbd "J") nil)
(define-key evil-normal-state-map (kbd "D") nil)
(define-key evil-normal-state-map (kbd "C-g") nil)

;; normal MODE
;; Bind to normal emacs shell command. It is quite useful that shell
;; output can be piped to a buffer for text editing!
(define-key evil-normal-state-map (kbd "!") 'shell/ex-and-frame)
(define-key evil-normal-state-map (kbd "C") 'comment-line)
(define-key evil-normal-state-map (kbd "-") 'evil-join)
(define-key evil-normal-state-map (kbd "C-G") 'evil-goto-line)
(define-key evil-normal-state-map (kbd "C-b") 'counsel-switch-buffer)
(define-key evil-normal-state-map (kbd "L") 'evil-multiedit-match-and-next)
(define-key evil-normal-state-map (kbd "J")
  'evil-mc-make-cursor-move-next-line)
(define-key evil-normal-state-map (kbd "K") 'evil-mc-undo-last-added-cursor)
(define-key evil-normal-state-map (kbd "H") 'evil-mc-undo-all-cursors)
(define-key evil-normal-state-map (kbd "D") 'er/expand-region)
(define-key evil-normal-state-map (kbd "SPC") 'avy-goto-char-timer)
(define-key evil-normal-state-map (kbd "C-g") 'counsel-projectile-ag)

(evil-ex-define-cmd "q" (lambda () (interactive)
			(kill-this-buffer) (delete-frame)))
