(use-package general
  :ensure t

  :preface
  (defconst gen/hgro 10)
  (defconst gen/vgro 5)

  (defun gen/test-msg (s) (message (concat "TEST: " s)))
  (defun gen/find-configs () (interactive)
    (let ((config-dir (expand-file-name "~/.config/emacs/")))
      (find-file (read-file-name "configs: " config-dir))))
  (defun gen/split (split-fn) (funcall split-fn) (other-window 1))
  (defun gen/split/bal (split-fn) (funcall split-fn)
         (balance-windows) (other-window 1))

  :config
  ;; general config
  (general-evil-setup t)

  ;; simple
  (general-define-key
   "C-=" 'text-scale-increase
   "C--" 'text-scale-decrease
   "C-<up>"    '(lambda () (interactive) (enlarge-window gen/vgro))
   "C-<down>"  '(lambda () (interactive) (shrink-window  gen/vgro))
   "C-<left>"  '(lambda () (interactive) (shrink-window-horizontally gen/hgro))
   "C-<right>" '(lambda () (interactive) (enlarge-window-horizontally gen/hgro))
   )

  ;; maps
  (general-create-definer leader/spc/normal :keymaps '(normal) :prefix "SPC")
  (general-create-definer leader/spc/visual :keymaps '(visual) :prefix "SPC")

  (leader/spc/normal
    "c"   '(:ignore t :which-key "configuration")
    "c c" 'gen/find-configs

    ;; eval
    "e"   '(:ignore t :which-key "windowing operations")
    "e e" '(eval-buffer :which-key "eval buffer")

    ;; windowing
    "w"   '(:ignore t :which-key "windowing operations")
    "w q" '((lambda () (interactive) (delete-window))
            :which-key "delete current window")
    "w w" '((lambda () (interactive) (gen/split/bal 'split-window-right))
            :which-key "split window right")
    "w l" '((lambda () (interactive) (gen/split/bal 'split-window-right))
            :which-key "split window right")
    "w j" '((lambda () (interactive) (gen/split 'split-window-below))
            :which-key "split window below")
    "w d" '((lambda () (interactive)
              (windmove-swap-states-left) (windmove-right))
            :which-key "swap with left window")
    "w a" '((lambda () (interactive)
              (windmove-swap-states-right) (windmove-left))
            :which-key "swap with right window")
  )

  (leader/spc/visual
   "e r" '(eval-region :which-key "eval region")
  )
)

