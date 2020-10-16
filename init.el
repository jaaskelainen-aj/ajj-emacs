 ;AJJ emacs initialization file

;; ===============================================================
;; Basic environmentm, packages and modes
;; ===============================================================

(add-to-list 'load-path "~/ajj-emacs/ext")
(add-to-list 'load-path "~/ajj-emacs/themes")
(when (eq system-type 'gnu/linux)
  (when window-system
    (tool-bar-mode 0)
    (set-face-font 'default "-*-Hack-normal-normal-normal-*-12-*-*-*-m-0-iso10646-1")
    (setq server-socket-dir "~/.emacs.d/")
    (server-start)
    ))

(when (eq system-type 'darwin)
  (define-key global-map [home] 'beginning-of-line)
  (define-key global-map [end] 'end-of-line)
  (define-key global-map [C-home] 'beginning-of-buffer)
  (define-key global-map [C-end] 'end-of-buffer)
  (setq shell-file-name "/bin/bash")
  (setq shell-command-switch "-ic")
  (if window-system (progn
      (set-face-font 'default "-*-Hack-normal-normal-normal-*-13-*-*-*-m-0-iso10646-1")
      (define-key global-map [ns-drag-file] 'ns-find-file-in-frame)
      (define-key global-map [s-tab] 'next-multiframe-window)
      ;; (global-set-key (kbd "C->") 'previous-multiframe-window)
      (tool-bar-mode 0)
      (setq mac-command-modifier (quote control))
      (setq mac-control-modifier (quote super))
      (setq mac-option-modifier (quote meta))
      (setq mouse-wheel-progressive-speed nil)
      (setq server-socket-dir "~/.emacs.d/")
      (server-start)
      )
    ;;else
    (
     ;;(menu-bar-mode -1)
     )
    ))

;; Our own prefix, needed by our init packages
(define-prefix-command 'mc-prefix)
(global-set-key (kbd "s-m") 'mc-prefix)

;; See install-base.el also
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(load "menacon-pkg")
(load "menacon-modes")
(load "menacon")
(load "kone")
(mc-modes-init)

(setq custom-theme-directory "~/ajj-emacs/themes/")
(load-theme 'ajj-light t)

;;(add-hook 'speedbar-load-hook (lambda () (require 'semantic/sb)))

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
;; Note: f5 and f6 do not work in current MacOs/Emacs
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
(global-set-key (kbd "<s-return>") 'overwrite-mode)
(global-set-key (kbd "M-s-s") 'swiper-helm)
(global-set-key (kbd "C-=") 'mc-select-this-word)

(global-set-key "\M-n" 'other-window)
(global-set-key "\C-\M-\\" 'indent-region)
(global-set-key "\C-\M-n" '(lambda () (interactive) (insert "<<'\\n'")))
(global-set-key (kbd "C-M-'") '(lambda () (interactive) (insert "\\\"")))
(global-set-key "\C-h\C-c" 'describe-face)
(global-set-key "§" '(lambda() (interactive)(insert "´")))
(global-set-key (kbd "s->") '(lambda() (interactive)(insert "»")))
(global-set-key (kbd "s-<") '(lambda() (interactive)(insert "«")))

;Function keys
(global-set-key [f2] 'mc-match-paren)
(global-set-key [C-f2] 'helm-projectile-find-file-in-known-projects)
(global-set-key [M-f2] 'query-replace-regexp)
; S-f2 reserved for helm-ag

(global-set-key [f3] 'mc-prev-buf)
(global-set-key [C-f3] 'mc-save-kill)
(global-set-key [M-f3] '(lambda() (interactive) (revert-buffer nil t t)))

(global-set-key [f4] 'dabbrev-expand)
;S-F4 used in programmin modes
;(global-set-key [C-f4] 'mc-replace-clear)
;(global-set-key [M-f4] 'mc-replace-swap)

; f6 - f8 reserved for modes
(global-set-key [f9] '(lambda() (interactive)(insert-register ?1 t)))
(global-set-key [C-f9] 'copy-to-register)
(global-set-key [M-f9] 'insert-register)

(global-set-key [M-f10] 'mc-line-copy-char)
(global-set-key [S-M-f10] '(lambda () (interactive)(mc-line-copy-char nil)))

(global-set-key [C-f11] 'mc-search-clear)
(global-set-key [M-f11] 'mc-search-this-word)

(global-set-key [f12] 'mc-point-to-reg1)
(global-set-key [S-f12] 'mc-copy-sel-to-point1)
(global-set-key [C-f12] 'mc-move-sel-to-point1)
(global-set-key [S-C-f12] 'mc-insert-reg2)

; Unset the reserved mac keys
; ??

;; ===============================================================
;; MENUS
;; ===============================================================

;; ===============================================================
;; GLOBAL SETTINGS
;; ===============================================================
(setq frame-title-format "%f ( %I )")

(setq 
  line-number-mode t
  column-number-mode t
  default-tab-width 4
  delete-key-deletes-forward t
  make-backup-files nil
  split-height-threshold nil
  split-width-threshold nil
  buffers-menu-buffer-name-length 40
  buffers-menu-max-size 20
  inhibit-startup-screen t
  initial-scratch-message nil
  ;;
  grep-command "grep  -n "
  grep-find-command
   (quote
    ("find . -name '*.cpp' -or -name '*.h' | xargs grep -n"))
  grep-highlight-matches t
  ;;
  compilation-scroll-output t
  )

(custom-set-variables
 '(speedbar-default-position (quote right))
 '(speedbar-directory-button-trim-method (quote trim))
 '(speedbar-frame-parameters
   (quote
    ((minibuffer)
     (width . 30)
     (border-width . 0)
     (menu-bar-lines . 0)
     (tool-bar-lines . 0)
     (unsplittable . t)
     (left-fringe . 0))))
 '(speedbar-indentation-width 2)
 '(speedbar-obj-do-check nil)
 '(speedbar-select-frame-method (quote attached))
 '(speedbar-show-unknown-files t)
 '(speedbar-sort-tags nil)
 '(speedbar-track-mouse-flag nil)
 '(speedbar-use-tool-tips-flag nil)
 '(speedbar-vc-do-check t)
 )

(setq-default fill-column 120)
(setq-default show-paren-mode 1)
