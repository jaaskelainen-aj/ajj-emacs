;; execute: emacs --batch -l install-base.el -q
(defvar base-pkg-list
  '(bind-key use-package))
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(dolist (pkg base-pkg-list)
  (unless (package-installed-p pkg)
    (package-install pkg)))
