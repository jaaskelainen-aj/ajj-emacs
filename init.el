 ;AJJ emacs initialization file

;; ===============================================================
;; Basic environmentm, packages and modes
;; ===============================================================

(when (eq system-type 'gnu/linux)
  (set-default-font "DejaVu Sans-10")
  (when window-system (progn
    (setq x-select-enable-clipboard t)
    (setq server-socket-dir "~/.emacs.d/")
    (server-start)
    )))

(when (eq system-type 'darwin)
  (define-key global-map [home] 'beginning-of-line)
  (define-key global-map [end] 'end-of-line)
  (define-key global-map [C-home] 'beginning-of-buffer)
  (define-key global-map [C-end] 'end-of-buffer)
  (if window-system (progn
      (define-key global-map [ns-drag-file] 'ns-find-file-in-frame)
      (define-key global-map [s-tab] 'other-window)
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
;; MODES
;; ===============================================================
(add-hook 'c-mode-hook     'mc-set-c-mode)
(add-hook 'cc-mode-hook	   'mc-set-cc-mode)
(add-hook 'c++-mode-hook   'mc-set-c++-mode)
(add-hook 'java-mode-hook  'mc-set-java-mode)
(add-hook 'sql-mode-hook   'mc-set-sql-mode)
(add-hook 'php-mode-hook   'mc-set-php-mode)
(add-hook 'nxml-mode-hook  'mc-set-nxml-mode)
(add-hook 'sh-mode-hook    'mc-set-sh-mode)
(add-hook 'python-mode-hook 'mc-set-py-mode)
(add-hook 'js-mode-hook    'mc-set-js-mode)
(add-hook 'web-mode-hook   'mc-set-js-mode)
(add-hook 'json-mode-hook  'mc-set-json-mode)

(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'js-indent-level 'tab-width)

(electric-pair-mode)
;; ===============================================================
;; FONT LOCKS
;; ===============================================================

;; ===============================================================
;; CUSTOM VARIABLES
;; ===============================================================

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ack-command "ack -H --nocolour --nofilter --cpp")
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(buffers-menu-buffer-name-length 40)
 '(buffers-menu-max-size 20)
 '(column-number-mode t)
 '(compilation-message-face (quote default))
 '(compilation-scroll-output t)
 '(custom-enabled-themes (quote (github-modern)))
 '(custom-safe-themes
   (quote
    ("696171d66b2f152e9d71755be171a6d9ac3be0b0d3deb7d8cbf2dd6132306638" "b9f16b449dd896d7f2bb87bbbb59968491dc3ac96c006951e68338365b53cdaa" "707227acad0cf8d4db55dcf1e574b3644b68eab8aca4a8ce6635c8830bc72144" "3629b62a41f2e5f84006ff14a2247e679745896b5eaa1d5bcfbc904a3441b0cd" "60d4556ebff0dc94849f177b85dcb6956fe9bd394c18a37e339c0fcd7c83e4a9" "20e23cba00cf376ea6f20049022241c02a315547fc86df007544852c94ab44cb" default)))
 '(custom-theme-directory "~/ajj-emacs/themes/")
 '(fci-rule-color "#3C3D37")
 '(grep-command "grep  -n ")
 '(grep-find-command
   (quote
    ("find . -name '*.cpp' -or -name '*.h' | xargs grep -n")))
 '(grep-highlight-matches t)
 '(grep-template "grep <X> <C> -nH <R> <F>")
 '(grep-use-null-device nil)
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(line-move-visual nil)
 '(magit-diff-use-overlays nil)
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 3) ((control)))))
 '(package-selected-packages
   (quote
    (scons robot rtags cff helm find-file-in-repository gnu-elpa-keyring-update which-key use-package csharp-mode ack json-mode monokai-theme avk-emacs-themes elmacro web-mode php-mode)))
 '(pos-tip-background-color "#FFFACE")
 '(pos-tip-foreground-color "#272822")
 '(show-paren-mode t)
 '(speedbar-default-position (quote right))
 '(speedbar-directory-button-trim-method (quote trim))
 '(speedbar-indentation-width 2)
 '(speedbar-obj-do-check nil)
 '(speedbar-select-frame-method (quote attached))
 '(speedbar-show-unknown-files t)
 '(speedbar-sort-tags nil)
 '(speedbar-track-mouse-flag nil)
 '(speedbar-use-tool-tips-flag nil)
 '(speedbar-vc-do-check nil)
 '(tool-bar-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF"))))
 '(vc-annotate-very-old-color nil)
 '(web-mode-enable-current-column-highlight t)
 '(weechat-color-list
   (unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0")))

;; ===============================================================
;; COLORS
;; ===============================================================

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-string-face ((t (:foreground "#bc791f"))))
 '(highlight ((t (:background "#486BAF" :foreground "#272822"))))
 '(isearch ((t (:inherit region :background "#A6E22E"))))
 '(lazy-highlight ((t (:background "#708EC9" :foreground "black"))))
 '(mode-line ((t (:background "systemYellowColor" :foreground "black" :box (:line-width -1 :style released-button)))))
 '(region ((t (:background "#AFAD99"))))
 '(trailing-whitespace ((t (:background "light gray")))))

;; ===============================================================
;; MANUAL CUSTOMIZATION
;; ===============================================================
(setq uniquify-buffer-name-style 'forward)
(setq line-number-mode t)
(setq column-number-mode t)
(setq default-tab-width 4)
(setq delete-key-deletes-forward t)
(setq make-backup-files nil)
(setq-default fill-column 120)
(setq split-height-threshold nil)
(setq split-width-threshold nil)

;; Customized variables: Programming
(setq c-continued-statement-offset 4)
(setq c-basic-offset 4)
(setq c-indent-level 4)
(setq c-auto-newline nil)
(c-set-offset 'substatement-open 0)
(setq compilation-window-height 20)
(setq compilation-read-command t)
(setq compilation-ask-about-save nil)
(setq compilation-scroll_output t)
(setq compile-command "c4s-build -deb")
(setq smerge-command-prefix (kbd "M-s"))
