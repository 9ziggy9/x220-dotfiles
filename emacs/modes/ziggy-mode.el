;; be aware; this minor mode will take precedence over all global bindings
(defvar ziggy-mode-map (make-sparse-keymap)
  "Keymap for 'ziggy-mode'.")

(define-minor-mode ziggy-mode
  "9ziggy9 minor mode of custom binding."
  :global t
  :lighter " 9ziggy9"
  :keymap ziggy-mode-map
  ;; window ops
  (define-key ziggy-mode-map (kbd "C-x C-l") 'split-window-right)
  (define-key ziggy-mode-map (kbd "C-x C-j") 'split-window-below)
  (define-key ziggy-mode-map (kbd "C-x C-x") 'delete-window)
  (define-key ziggy-mode-map (kbd "C-l")     'windmove-right)
  (define-key ziggy-mode-map (kbd "C-h")     'windmove-left)
  (define-key ziggy-mode-map (kbd "C-k")     'windmove-up)
  (define-key ziggy-mode-map (kbd "C-j")     'windmove-down)
  ;; buffer ops
  (define-key ziggy-mode-map (kbd "M-<backspace>") 'kill-buffer-and-window)
  (define-key ziggy-mode-map (kbd "M-]")           'next-buffer)
  (define-key ziggy-mode-map (kbd "M-[")           'previous-buffer)
  ;; discovery ops
  (define-key ziggy-mode-map (kbd "C-/ 1") 'helpful-key)
  (define-key ziggy-mode-map (kbd "C-/ 2") 'helpful-function)
  (define-key ziggy-mode-map (kbd "C-/ 3") 'helpful-variable)
  (define-key ziggy-mode-map (kbd "C-/ 4") 'helpful-command)
  (define-key ziggy-mode-map (kbd "C-/ q") 'helpful-macro)
  (define-key ziggy-mode-map (kbd "C-/ w") 'helpful-symbol)
  (define-key ziggy-mode-map (kbd "C-/ e") 'helpful-at-point)
  (define-key ziggy-mode-map (kbd "C-/ r") 'helpful-callable))
    

(defun ziggy/test-log ()
  "Test function for my ziggy mode."
  (interactive)
  (message "9ziggy9 is in business!"))

(provide 'ziggy-mode)
