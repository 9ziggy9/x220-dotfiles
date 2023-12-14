;; BUG: Doesn't pull in absolute path
;; (defun find/open-in-new-frame ()
;;   (interactive)
;;   (find-file-other-frame (ivy-state-current ivy-last))
;;   (keyboard-escape-quit))

(defun find/open-in-new-frame ()
  (interactive)
  (let ((file-path (expand-file-name (ivy-state-current ivy-last) ivy--directory)))
    (find-file-other-frame file-path)
    (keyboard-escape-quit)))
