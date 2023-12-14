(use-package dired
  :ensure nil  ; dired is part of Emacs, no need to install
  :hook (dired-mode . dired-auto-refresh)
  :config
  (defun dired-auto-refresh ()
    "Automatically refresh dired on open."
    (auto-revert-mode))
  (setq dired-listing-switches "-alh"  ; Human-readable sizes, all files
        dired-dwim-target t))           ; Guess target directory

(use-package hydra
  :ensure t
  :config
  (defhydra hydra-dired (:hint nil :color pink)
    "
  ^Mark^            ^Actions^          ^Navigation^        ^View^
  ^^^^^^^^----------------------------------------------------------
  _m_: mark         _C_: copy          _n_: next line      _v_: view file
  _u_: unmark       _D_: delete        _p_: previous line  _o_: open other window
  _t_: toggle       _R_: rename        _g_: refresh        _i_: insert subdir
  _*_: specific     _+_: mkdir         _s_: sort           _I_: image toggle
  _A_: regex        _Z_: compress      ^ ^                 _h_: toggle hidden
  ^ ^               _M_: chmod         ^ ^                 _l_: redisplay
  ^ ^               _G_: chgrp         ^ ^                 _._: hide subdir
  ^ ^               _O_: chown         ^ ^                 _q_: quit
  "
    ("m" dired-mark)
    ("u" dired-unmark)
    ("t" dired-toggle-marks)
    ("*" dired-mark-files-regexp)
    ("A" dired-mark-files-containing-regexp)
    ("C" dired-do-copy)
    ("D" dired-do-delete)
    ("R" dired-do-rename)
    ("+" dired-create-directory)
    ("Z" dired-do-compress)
    ("M" dired-do-chmod)
    ("G" dired-do-chgrp)
    ("O" dired-do-chown)
    ("n" dired-next-line)
    ("p" dired-previous-line)
    ("g" revert-buffer)
    ("s" dired-sort-toggle-or-edit)
    ("v" dired-view-file)
    ("o" dired-find-file-other-window)
    ("i" dired-maybe-insert-subdir)
    ("I" dired-toggle-image-preview)
    ("h" dired-omit-mode)
    ("l" dired-do-redisplay)
    ("." dired-hide-subdir)
    ("q" nil))
    (define-key dired-mode-map "?" 'hydra-dired/body))
