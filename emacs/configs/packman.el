(require 'package)
;; Set the package repositories from which Emacs can download and install packages.
;; - MELPA: A large repository of community-maintained packages.
;; - Org: The repository for Org-mode and related packages.
;; - ELPA: The official GNU Emacs package repository.
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; Check if the package archive contents are available.
;; If not, refresh the package contents to retrieve the latest package lists.
(unless package-archive-contents
  (package-refresh-contents))

;; Check if the 'use-package' package is installed.
;; If not, install 'use-package' which is a declarative configuration tool
;; for managing Emacs packages.
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Load and configure 'use-package'.
(eval-when-compile (require 'use-package))

;; Ensure that packages are automatically installed if they are not already present.
;; This is a convenient default that makes sure declared packages are available.
(setq use-package-always-ensure t)
