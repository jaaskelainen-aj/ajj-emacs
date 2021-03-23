;; execute: emacs --batch -l install-base.el -q
(defvar base-pkg-list
  '(cff clang-format company fill-column-indicator gnu-elpa-keyring-update js2-mode json-mode magit-find-file markdown-mode php-mode projectile s web-mode yasnippet)
  )

(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))
(dolist (pkg base-pkg-list)
  (unless (package-installed-p pkg)
    (package-install pkg)))
