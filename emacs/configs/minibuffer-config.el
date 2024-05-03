(use-package minibuffer
  :ensure nil
  :bind
  ("H-w" . previous-line)
  ("H-s" . next-line)
  ("H-p" . yank)
  ("H-y" . evil-yank-line))
