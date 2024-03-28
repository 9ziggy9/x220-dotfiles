(use-package whitespace
  :ensure t
  :config
  (setq whitespace-line-column 80  ; Set the column after which to highlight
        whitespace-style '(face lines-tail))  ; Highlight only the tail of lines
  :hook ((prog-mode . whitespace-mode)
         (text-mode . whitespace-mode)))

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
      doom-themes-enable-italic t)
  (load-theme 'doom-ephemeral t))

;; RAINBOW PARENS
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; Show colors when encountering hex color codes
(use-package rainbow-mode
  :ensure t
  :config
  ;; Optionally, enable rainbow-mode in certain modes automatically
  (add-hook 'css-mode-hook #'rainbow-mode)
  (add-hook 'emacs-lisp-mode-hook #'rainbow-mode)
  (add-hook 'html-mode-hook #'rainbow-mode))

(defun my-powerline-theme ()
  "A powerline theme that removes git info and adds line and column numbers."
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                    (let* ((active (powerline-selected-window-active))
                          (mode-line-buffer-id (if active 'mode-line-buffer-id 'mode-line-buffer-id-inactive))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face1 (if active 'powerline-active1 'powerline-inactive1))
                          (face2 (if active 'powerline-active2 'powerline-inactive2))
                          (separator-left (intern (format "powerline-%s-%s"
                                                            (powerline-current-separator)
                                                            (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                            (powerline-current-separator)
                                                            (cdr powerline-default-separator-dir))))
                          (lhs (list (powerline-raw "%*" nil 'l)
                                      (powerline-buffer-size nil 'l)
                                      (powerline-raw mode-line-mule-info nil 'l)
                                      (powerline-buffer-id `(mode-line-buffer-id ,mode-line) 'l)
                                      (when (and (boundp 'which-func-mode) which-func-mode)
                                        (powerline-raw which-func-format nil 'l))
                                      (powerline-raw " ")
                                      (powerline-raw "%4l:%3c" nil 'l) ; Line and column numbers
                                      (powerline-raw " ")
                                      (funcall separator-left mode-line face1)
                                      (when (bound-and-true-p nyan-mode)
                                        (powerline-raw (list (nyan-create)) face1 'l))
                                      (powerline-major-mode face1 'l)
                                      (powerline-process face1)
                                      (powerline-minor-modes face1 'l)
                                      (powerline-narrow face1 'l)
                                      (powerline-raw " " face1)))
                          (rhs (list (powerline-raw global-mode-string face1 'r)
                                      (funcall separator-right face1 mode-line)
                                      (powerline-raw "%4p" nil 'r)
                                      (powerline-hud face2 face1))))
                      (concat (powerline-render lhs)
                              (powerline-fill face1 (powerline-width rhs))
                              (powerline-render rhs)))))))
(use-package powerline-evil
  :ensure t
  :init
  (setq powerline-height 25
        powerline-default-separator 'contour)
  :config
  (my-powerline-theme))
