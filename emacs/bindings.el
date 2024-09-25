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
   ;; text scale
   "C-=" 'text-scale-increase
   "C--" 'text-scale-decrease

   ;; minibuffer/general line movement
   "H-w" 'previous-line
   "H-s" 'next-line

   ;; expand selection
   "H-e" 'er/expand-region

   ;; hide pop-up frame
   "H-q" 'posframe-hide-all
   )

  (general-define-key
    :keymaps 'ziggy-mode-map
    ;; window operations
    "C-l" 'windmove-right
    "C-h" 'windmove-left
    "C-k" 'windmove-up
    "C-j" 'windmove-down
    "C-S-H" '(lambda () (interactive) (shrink-window-horizontally gen/hgro))
    "C-S-L" '(lambda () (interactive) (enlarge-window-horizontally gen/hgro))
    "C-S-K" '(lambda () (interactive) (enlarge-window gen/vgro))
    "C-S-J" '(lambda () (interactive) (shrink-window  gen/vgro))
    "C-<return>" '(lambda () (interactive) (gen/split/bal 'split-window-right))
    "C-<backspace>" '(lambda () (interactive) (delete-window))

    ;; buffer operations
    "C-0" 'next-buffer
    "C-9" 'previous-buffer

    ;; discovery operations (using helpful)
    "C-/ 1" '(helpful-key      :which-key "key")
    "C-/ 2" '(helpful-function :which-key "fn")
    "C-/ 3" '(helpful-variable :which-key "var")
    "C-/ 4" '(helpful-command  :which-key "cmd")
    "C-/ q" '(helpful-macro    :which-key "macro")
    "C-/ w" '(helpful-symbol   :which-key "symbol")
    "C-/ e" '(helpful-at-point :which-key "help @ point")
    "C-/ r" '(helpful-callable :which-key "callable")
    )

  ;; maps
  (general-create-definer leader/spc/normal :keymaps '(normal) :prefix "SPC")
  (general-create-definer leader/spc/visual :keymaps '(visual) :prefix "SPC")

  (leader/spc/normal
    ;; compile
    "c"   '(:ignore t  :which-key "compile")
    "c c" '(compile    :which-key "compile command")
    "c r" '(recompile  :which-key "recompile (last command)")
    "c n" '(next-error :which-key "next error")

    ;; quick config
    "C"   '(:ignore t :which-key "configuration")
    "C C" 'gen/find-configs

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

