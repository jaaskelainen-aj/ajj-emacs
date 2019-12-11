;; execute: emacs --batch -l install-base.el -q
(defvar base-pkg-list
  '(cc-mode which-key helm rtags magit helm-rtags helm-ag projectile helm-projectile swiper-helm cff))
;; possilbe: swiper-helm 

(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))
(dolist (pkg base-pkg-list)
  (unless (package-installed-p pkg)
    (package-install pkg)))
