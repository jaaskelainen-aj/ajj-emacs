(use-package helm
  :ensure t
  :init
  (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
	helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
	helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
	helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
	helm-ff-file-name-history-use-recentf t)
  
  :bind
  (
   ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
   ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
   ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
   ("C-e"     . helm-command-prefix)
   ("M-x"     . helm-M-x)
   ("M-y"     . helm-show-kill-ring)
   ("C-x C-f" . helm-find-files)
   ("C-x b"   . helm-buffers-list)
   ("C-x C-b" . ibuffer) 
   :map helm-map
   ("<tab>" . helm-execute-persistent-action) ; rebind tab to run persistent action
   ("C-i"   . helm-execute-persistent-action) ; make TAB works in terminal
   ("C-z"   . helm-select-action) ; list actions using C-z
   )
  
  :config
  ;;COLORS
  (set-face-attribute 'helm-selection nil 
                      :background "#252555"
                      :foreground nil)
  ;; OTHER
  (unbind-key "C-x c" global-map)
  (helm-autoresize-mode t)
  (setq helm-autoresize-max-height 50)
  (setq helm-autoresize-min-height 20)
  (setq helm-semantic-fuzzy-match t
	helm-imenu-fuzzy-match    t)
  ;; MAN
  (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)

  )
