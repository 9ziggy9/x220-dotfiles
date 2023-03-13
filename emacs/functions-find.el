(defun find/open-in-new-frame ()
  (interactive)
  (find-file-other-frame (ivy-state-current ivy-last))
  (keyboard-escape-quit))
