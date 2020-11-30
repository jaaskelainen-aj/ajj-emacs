(eval-when-compile
  (add-to-list 'load-path "~/ajj-emacs/ext")
  (require 'package)
  (package-initialize)
  )

(require 'align)
(require 'paren)
(require 'cc-mode)
(require 'robot-mode)

;; ------------------------------
;; RTAGS
(require 'rtags)
(define-key global-map (kbd "M-.") 'rtags-find-symbol-at-point)
(define-key global-map (kbd "M-,") 'rtags-find-references-at-point)
(define-key global-map (kbd "M-;") 'rtags-find-file)
(define-key global-map (kbd "C-.") 'rtags-find-symbol)
(define-key global-map (kbd "C-,") 'rtags-find-references)

;; ------------------------------

;; ------------------------------
;; MAGIT
(require 'magit)
(setq magit-diff-use-overlays nil)

;; --
;; AG
(require 'ag)
(setq ag-highlight-search t)
(setq ag-reuse-buffers 't)
(define-key global-map (kbd "s-a") 'ag)

;; ------------------------------
;; PROJECTILE
(require 'projectile)
;;-- (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
;;-- (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq projectile-indexing-method 'alien
      projectile-enable-caching t
      projectile-switch-project-action 'projectile-find-file
      )
;; (setq projectile-project-search-path '("~/projects/" "~/work/"))
(define-key global-map (kbd "S-<f2>") 'projectile-ag)

;; ------------------------------
;; ORG mode
(require 'org)
(setq org-log-done t)
(define-key global-map (kbd "M-s-l") 'org-store-link)

;; ------------------------------
;; JS..HTML/CSS/PHP
(require 'js2-mode)
(require 'web-mode)
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 4)
(setq web-mode-enable-auto-pairing t)
(setq web-mode-enable-css-colorization t)
(setq web-mode-enable-current-element-highlight t)

;; ------------------------------
;; MISC
(require 'cff)

(require 'whitespace)
(setq whitespace-line-column nil)

;; ------------------------------
;;? (load-file "~/.emacs.d/mylib/org-recipes.el")
;;? (setq org-recipes-file-list (list
;;? 			       "~/.emacs.d/myrecipes/cpp_recipes.org"
;;? 			       "~/.emacs.d/myrecipes/elisp_recipes.org"
;;? 			       "~/.emacs.d/myrecipes/org_recipes.org"
;;? 			       "~/.emacs.d/myrecipes/python_recipes.org"))
;;? (global-set-key [(f7)] 'org-recipes)

