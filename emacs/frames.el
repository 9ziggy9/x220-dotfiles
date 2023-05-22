;; FRAMES / TILES
;; This code tells Emacs to open all new windows in new frames,
;; and to reuse existing frames if possible.
;; NOTE: this configuration can cause Emacs to create a large number
;; of frames if you frequntly open and close windows.
(setq display-buffer-alist
      '(("^*Man.*" .
	 ((display-buffer-reuse-window display-buffer-pop-up-frame)
	  (inhibit-same-window . t)
	  (frame . nil)))))
