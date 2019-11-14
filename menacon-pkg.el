(defun mc-packages-init()
  (add-to-list 'load-path "~/ajj-emacs/ext")
  (require 'bind-key)
  (require 'helm)
  (require 'magit)
  (require 'robot-mode)
  
  (use-package which-key :ensure t)
  (use-package find-file-in-repository
    :ensure t
    :bind
    (("C-x f" . find-file-in-repository)) )

  (use-package rtags
    :ensure t
    :bind
    (
     ("M-." . (function rtags-find-symbol-at-point))
     ("M-," . (function rtags-find-references-at-point))
     ("M-;" . (function rtags-find-file))
     ("C-." . (function rtags-find-symbol))
     ("C-," . (function rtags-find-references)) )
     )

  ;;? (load-file "~/.emacs.d/mylib/org-recipes.el")
  ;;? (setq org-recipes-file-list (list
  ;;? 			       "~/.emacs.d/myrecipes/cpp_recipes.org"
  ;;? 			       "~/.emacs.d/myrecipes/elisp_recipes.org"
  ;;? 			       "~/.emacs.d/myrecipes/org_recipes.org"
  ;;? 			       "~/.emacs.d/myrecipes/python_recipes.org"))
  ;;? (global-set-key [(f7)] 'org-recipes)
  (use-package magit
    :ensure t
    :config
    (setq magit-diff-use-overlays nil)
    )

  (use-package helm-rtags
    :ensure t
    :after (helm rtags))

  (use-package helm-ag
    :ensure t
    :init
    :bind
    (
     :map global-map
	  ("M-p" . helm-ag-project-root)
	  )
    :config
    (setq helm-grep-ag-command "ag --line-numbers -S --hidden --color --color-match '31;43' --nogroup %s %s %s")
    (setq helm-grep-ag-pipe-cmd-switches '("--color-match '31;43'"))
  
    :after (helm magit)
    )
  (load "pkg-helm-config")
  
  ;; CFF
  (require 'cff)
  (add-hook 'c++-mode-hook
            '(lambda ()
               (define-key c-mode-base-map (kbd "M-o") 'cff-find-other-file)))
  (add-hook 'c-mode-hook
            '(lambda ()
               (define-key c-mode-base-map (kbd "M-o") 'cff-find-other-file)))

  (use-package find-file-in-repository
    :ensure t
    :bind
    (("C-x f" . find-file-in-repository)))
  
  ) ;; mc-packages-init

(defun mc-packages-update()
  (interactive)
  (use-package gnu-elpa-keyring-update
    :ensure t)
  )

