(use-package auctex
  :ensure t
  :defer t
  :mode ("\\.tex\\'" . latex-mode)
  :hook (LaTeX-mode . (lambda ()
                        (set (make-local-variable 'TeX-auto-save) t)
                        (set (make-local-variable 'TeX-parse-self) t)
                        (set (make-local-variable 'TeX-master) nil)
                        (set (make-local-variable 'TeX-PDF-mode) t)
                        (TeX-source-correlate-mode t)))
  :config
  (add-to-list 'TeX-command-list
                '("pdflatex" "pdflatex -synctex=1 -interaction=nonstopmode %t"
                  TeX-run-command nil t :help "Run pdflatex with SyncTeX"))
  (setq TeX-source-correlate-method 'synctex)
  (setq TeX-source-correlate-start-server t)
  
  ;; Set Zathura as the default PDF viewer
  (setq TeX-view-program-selection '((output-pdf "Zathura"))
        TeX-view-program-list '(("Zathura"
                                 "zathura --synctex-forward %n:1:%b %o"))))

