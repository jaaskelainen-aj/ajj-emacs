(defun mc-packages-init()
  (add-to-list 'load-path "~/ajj-emacs/ext")
  (require 'bind-key)
  (require 'helm)
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

  ;; CFF
  (require 'cff)
  (add-hook 'c++-mode-hook
            '(lambda ()
               (define-key c-mode-base-map (kbd "M-o") 'cff-find-other-file)))
  (add-hook 'c-mode-hook
            '(lambda ()
               (define-key c-mode-base-map (kbd "M-o") 'cff-find-other-file)))

  )

(defun mc-packages-update()
  (interactive)
  (use-package gnu-elpa-keyring-update
    :ensure t)
  )

