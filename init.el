 ;AJJ emacs initialization file

;; ===============================================================
;; Basic environmentm, packages and modes
;; ===============================================================

(when (eq system-type 'gnu/linux)
  (when window-system (progn
    (setq server-socket-dir "~/.emacs.d/")
    (server-start)
    )))

(when (eq system-type 'darwin)
  (define-key global-map [home] 'beginning-of-line)
  (define-key global-map [end] 'end-of-line)
  (define-key global-map [C-home] 'beginning-of-buffer)
  (define-key global-map [C-end] 'end-of-buffer)
  (setq shell-file-name "/bin/bash")
  (setq shell-command-switch "-ic")
  (if window-system (progn
      (define-key global-map [ns-drag-file] 'ns-find-file-in-frame)
      (define-key global-map [s-tab] 'next-multiframe-window)
      ;; (global-set-key (kbd "C->") 'previous-multiframe-window)
      (tool-bar-mode 0)
      (setq mac-command-modifier (quote control))
      (setq mac-control-modifier (quote super))
      (setq mac-option-modifier (quote meta))
      (setq mouse-wheel-progressive-speed nil)
      (setq server-socket-dir "~/.emacs.d/")
      (server-start))
    ((menu-bar-mode -1))
    ))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(require 'cc-mode)
(require 'align)

(load "menacon-pkg")
(load "menacon-modes")
(load "menacon")
(mc-packages-init)
(mc-modes-init)

(setq custom-theme-directory "~/ajj-emacs/themes/")
(load-theme 'ajj-light t)

(add-hook 'speedbar-load-hook (lambda () (require 'semantic/sb)))

(prefer-coding-system 'utf-8-unix)
;(setq auto-coding-alist (cons '("\\.\\(html\\|utf8\\|sql\\|java\\|php\\|sh\\)\\'" . utf-8-unix) auto-coding-alist))
(add-to-list 'auto-coding-regexp-alist '("^\xFF\xFE" . utf-16-le) t)
(add-to-list 'auto-coding-regexp-alist '("^\xEF\xBB\xBF" . utf-8-unix) t)

(defun utf-16-le-pre-write-conversion 
  (start end) nil)

;; ===============================================================
;; MAC
;; ===============================================================
(defun ns-find-file-in-frame ()
  "Do a `find-file' with the `ns-input-file' as argument; staying in frame."
  (interactive)
  (let ((ns-pop-up-frames nil))
    (ns-find-file)))

;; ===============================================================
;; GLOBAL KEYBOARD DEFINITIONS 
;; ===============================================================
; General keys
(global-set-key [backtab] 'other-window)
(global-set-key [C-delete] 'mc-remove-right-wspace)
(global-set-key [M-delete] '(lambda () (interactive) (kill-word 1)))
(global-set-key [M-backspace] 'backward-kill-word)
(global-set-key (kbd "C-<right>") 'move-end-of-line)
(global-set-key (kbd "C-<left>") 'move-beginning-of-line)
(global-set-key (kbd "s-<up>") 'beginning-of-buffer)
(global-set-key (kbd "s-<down>") 'end-of-buffer)
(global-set-key (kbd "M-<up>") 'c-beginning-of-defun)
(global-set-key (kbd "M-<down>") 'c-end-of-defun)
(global-set-key (kbd "s-r") 'copy-to-register)
(global-set-key (kbd "<s-kp-enter>") 'overwrite-mode)
(global-set-key (kbd "M-s-a") 'helm-ag)
(global-set-key (kbd "M-s-s") 'swiper-helm)

(global-set-key "\M-n" 'other-window)
(global-set-key "\C-\M-\\" 'indent-region)
(global-set-key "\C-\M-n" '(lambda () (interactive) (insert "<<'\\n'")))
(global-set-key (kbd "C-M-'") '(lambda () (interactive) (insert "\\\"")))
(global-set-key "\C-h\C-c" 'describe-face)
(global-set-key "§" '(lambda() (interactive)(insert "´")))
(global-set-key (kbd "<f14>") '(lambda() (interactive)(insert "_«V»")))

;Function keys
(global-set-key [f2] 'mc-match-paren)
(global-set-key [C-f2] 'query-replace)
(global-set-key [M-f2] 'query-replace-regexp)

(global-set-key [f3] 'mc-prev-buf)
(global-set-key [C-f3] 'mc-save-kill)

(global-set-key [f4] 'mc-replace)
(global-set-key [C-f4] 'mc-replace-clear)
(global-set-key [M-f4] 'mc-replace-swap)

(global-set-key [f5] 'dabbrev-expand)

; f6 - f8 reserved for modes
(global-set-key [f9] '(lambda() (interactive)(insert-register ?1 t)))
(global-set-key [C-f9] 'copy-to-register)
(global-set-key [M-f9] 'insert-register)

(global-set-key [f10] 'mc-point-to-reg1)
(global-set-key [S-f10] 'mc-copy-sel-to-point1)
(global-set-key [C-f10] 'mc-move-sel-to-point1)
(global-set-key [S-C-f10] 'mc-insert-reg2)

(global-set-key [C-f11] 'mc-search-clear)
(global-set-key [M-f11] 'mc-search-this-word)

(global-set-key [M-f12] 'mc-line-copy-char)
(global-set-key [S-M-f12] '(lambda () (interactive)(mc-line-copy-char nil)))

; Unset the reserved mac keys
; ??

;; ===============================================================
;; MENUS
;; ===============================================================
(define-key-after (lookup-key global-map [menu-bar tools]) 
  [speedbar] '("Speedbar" . speedbar-frame-mode) [calendar])

;; ===============================================================
;; GLOBAL SETTINGS
;; ===============================================================
(setq frame-title-format "%f ( %I )")

(setq 
  line-number-mode t
  column-number-mode t
  default-tab-width 4
  show-paren-mode t
  delete-key-deletes-forward t
  make-backup-files nil
  split-height-threshold nil
  split-width-threshold nil
  buffers-menu-buffer-name-length 40
  buffers-menu-max-size 20
  inhibit-startup-screen t
  initial-scratch-message nil
  ;;
  custom-theme-directory "~/ajj-emacs/themes/"
  grep-command "grep  -n "
  grep-find-command
   (quote
    ("find . -name '*.cpp' -or -name '*.h' | xargs grep -n"))
  grep-highlight-matches t
  ;;
  compilation-scroll-output t
  )

(setq-default fill-column 120)