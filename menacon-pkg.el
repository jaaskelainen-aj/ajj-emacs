(require 'align)
(require 'paren)
(require 'cc-mode)
(require 'which-key)

;;(require 'robot-mode)

;; ------------------------------
;; HELM
(require 'helm)
(setq helm-split-window-inside-p            t
      helm-move-to-line-cycle-in-source     t
      helm-ff-search-library-in-sexp        t
      helm-scroll-amount                    8
      helm-ff-file-name-history-use-recentf t
      helm-candidate-number-limit	    150
      helm-autoresize-mode                  t
      helm-autoresize-max-height            75
      helm-autoresize-min-height            40
      helm-semantic-fuzzy-match             t
      helm-imenu-fuzzy-match                t
      helm-allow-mouse                      nil
      )

(define-key mc-prefix (kbd "r") 'helm-resume)
(define-key global-map (kbd "M-s-x")   'helm-M-x)
(define-key global-map (kbd "M-y")     'helm-show-kill-ring)
(define-key global-map (kbd "C-x C-f") 'helm-find-files)
(define-key global-map (kbd "C-x b")   'helm-buffers-list)
(define-key global-map (kbd "C-x C-b") 'ibuffer)

(define-key global-map (kbd "C-x c") nil)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-i")   'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z")   'helm-select-action)

;; ------------------------------
;; RTAGS
(require 'rtags)
(define-key global-map (kbd "M-.") 'rtags-find-symbol-at-point)
(define-key global-map (kbd "M-,") 'rtags-find-references-at-point)
(define-key global-map (kbd "M-;") 'rtags-find-file)
(define-key global-map (kbd "C-.") 'rtags-find-symbol)
(define-key global-map (kbd "C-,") 'rtags-find-references)

;; ------------------------------
;; after: helm, rtags
(require 'helm-rtags)

;; ------------------------------
;; MAGIT
(require 'magit)
(setq magit-diff-use-overlays nil)

;; ------------------------------
;; AG silver search; after: helm, magit
(require 'helm-ag)
(setq helm-grep-ag-command "ag --line-numbers -S --hidden --color --color-match '31;43' --nogroup %s %s %s")
(setq helm-grep-ag-pipe-cmd-switches '("--color-match '31;43'"))
(setq helm-ag-use-agignore t)
(make-variable-buffer-local 'helm-ag-command-option)

(define-key global-map (kbd "C-f") 'helm-ag-this-file)
(define-key global-map (kbd "M-f") 'helm-do-ag)
(define-key global-map (kbd "M-s-f") 'helm-do-ag-project-root)

;; ------------------------------
;; PROJECTILE
(require 'projectile)
(require 'helm-projectile)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
;;(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq projectile-indexing-method 'alien
      projectile-enable-caching t
      projectile-completion-system 'helm)
;; (setq projectile-project-search-path '("~/projects/" "~/work/"))

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
(require 'swiper-helm)
(require 'cff)

;; ------------------------------
;;? (load-file "~/.emacs.d/mylib/org-recipes.el")
;;? (setq org-recipes-file-list (list
;;? 			       "~/.emacs.d/myrecipes/cpp_recipes.org"
;;? 			       "~/.emacs.d/myrecipes/elisp_recipes.org"
;;? 			       "~/.emacs.d/myrecipes/org_recipes.org"
;;? 			       "~/.emacs.d/myrecipes/python_recipes.org"))
;;? (global-set-key [(f7)] 'org-recipes)

