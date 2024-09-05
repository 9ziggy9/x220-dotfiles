(defvar ziggy-mode-map (make-sparse-keymap) "keymap for `ziggy-mode'")
(define-minor-mode ziggy-mode
  "9ziggy9 minor mode for custom bindings"
  :lighter " 9ziggy9"
  :keymap ziggy-mode-map
  (if ziggy-mode
      (progn
        (make-local-variable 'emulation-mode-map-alists)
        (push (list (cons 'ziggy-mode ziggy-mode-map))
              emulation-mode-map-alists))
    (setq emulation-mode-map-alists
          (delete (assq 'ziggy-mode emulation-mode-map-alists)
                  emulation-mode-map-alists))))

(define-globalized-minor-mode global-ziggy-mode ziggy-mode
  (lambda () (ziggy-mode 1)))

(defun ziggy/test-log () "test function for ziggy mode"
  (interactive)
  (message "9ziggy9 is in business!"))

(with-eval-after-load 'evil
  (evil-make-overriding-map ziggy-mode-map 'normal)
  (add-hook 'ziggy-mode-hook #'evil-normalize-keymaps))

(provide 'ziggy-mode)
