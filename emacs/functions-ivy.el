(defun ivy/open-in-new-frame ()
  "Open the selected file in a new frame."
  (interactive)
  (let ((file (ivy-state-current ivy-last)))
    (when (file-exists-p file)
      (select-frame (make-frame))
      (find-file file))))
