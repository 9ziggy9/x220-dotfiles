(use-package company
  :ensure t
  :init
  (global-company-mode)
  :config
  ;; Increase the number of displayed completion items
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.2)  ;; Decrease delay before suggestions pop up
  ;; Enable company in all buffers
  (add-hook 'after-init-hook 'global-company-mode))

;; Using company-box for a more visually appealing dropdown
(use-package company-box
  :hook (company-mode . company-box-mode)
  :ensure t
  :config
  (setq company-box-enable-icon nil))

(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook ((rust-mode . lsp)))
