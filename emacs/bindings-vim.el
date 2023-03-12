;; EXAMPLE of binding to lambda expression
;;(define-key evil-normal-state-map (kbd "C-p") (lambda ()
;;						(interactive)
;;						(message "Hello, world!")))

;; Unbind
(define-key evil-motion-state-map (kbd "C-f") nil)
(define-key evil-motion-state-map (kbd "C-p") nil)
(define-key evil-motion-state-map (kbd "!") nil)

;; NORMAL MODE
;; Bind to normal emacs shell command. It is quite useful that shell
;; output can be piped to a buffer for text editing!
(define-key evil-normal-state-map (kbd "!") 'shell/ex-and-frame)
(define-key evil-normal-state-map (kbd "C") 'comment-line)
(define-key evil-normal-state-map (kbd "-") 'evil-join)
(define-key evil-normal-state-map (kbd "C-G") 'evil-goto-line)
(define-key evil-normal-state-map (kbd "C-b") 'counsel-switch-buffer)

(evil-ex-define-cmd "q" (lambda () (interactive)
			(kill-this-buffer) (delete-frame)))
