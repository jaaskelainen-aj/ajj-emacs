;; HOOKS

(defun mc-modes-init()
  (add-hook 'web-mode-hook     'mc-set-html-mode)
  (add-hook 'nxml-mode-hook    'mc-set-nxml-mode)
  (add-hook 'text-mode-hook    'mc-set-txt-mode)
  (add-hook 'org-mode-hook     'mc-set-org-mode)
  (add-hook 'conf-mode-hook    'mc-set-conf-mode)
  ;; Programmin modes
  (add-hook 'c-mode-common-hook     'mc-set-cc-mode)
  (add-hook 'java-mode-hook    'mc-set-java-mode)
  (add-hook 'sql-mode-hook     'mc-set-sql-mode)
  (add-hook 'php-mode-hook     'mc-set-php-mode)
  (add-hook 'nxml-mode-hook    'mc-set-nxml-mode)
  (add-hook 'sh-mode-hook      'mc-set-sh-mode)
  (add-hook 'python-mode-hook  'mc-set-py-mode)
  (add-hook 'js2-mode-hook     'mc-set-js-mode)
  (add-hook 'json-mode-hook    'mc-set-json-mode)
  (add-hook 'conf-unix-mode-hook 'mc-set-conf-mode)
  (add-hook 'robot-mode-hook   'mc-set-robot-mode)
  (add-hook 'logview-mode-hook 'mc-set-log-mode)

  ;;(defvaralias 'c-basic-offset 'tab-width)
  ;;(defvaralias 'js-indent-level 'tab-width)

  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))
  (add-to-list 'auto-mode-alist '("\\.?css\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.robot\\'" . robot-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-to-list 'auto-mode-alist '("\\.?json?\\'" . json-mode))
  (add-to-list 'auto-mode-alist '("\\.txt?\\'" . text-mode))

  (add-to-list 'auto-mode-alist '("\\.\\(h||c4s\\)\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.mak$" . makefile-mode))

  ;(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))
  ;(autoload 'csharp-mode "csharp-mod;; e" "Major mode for editing C# code." t)
  ;(setq auto-mode-alist (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))
  ;(setq auto-mode-alist (cons '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\|dita\\)\\'" . nxml-mode) auto-mode-alist))
  ;(setq interpreter-mode-alist (cons '("python" . python-mode) interpreter-mode-alist))
  ;(setq auto-mode-alist (cons '("\\.\\(py\\|cal\\)\\'" . python-mode) auto-mode-alist))
  ;(autoload 'python-mode "python-mode" "Python editing mode." t)
					; WebMode lists

  ;; (require 'scons-mode)
  ;;- (add-to-list 'auto-mode-alist '("\\SConscript" . scons-mode))
  ;;- (add-to-list 'auto-mode-alist '("\\SConstruct" . scons-mode))

  ;; Set the styles
  (c-add-style "KoneCPP" kone-c-style)
  ;; (setq c-default-style "KoneCPP")

  ;; Turn global modes on
  (show-paren-mode)
  (global-hl-line-mode)
  (electric-pair-mode)
  (projectile-mode +1)

  ;; PHP
  ;;(when (file-directory-p "~/ajj-emacs/ext/php")
  ;;  (load "~/ajj-emacs/ext/php/php-mode-autoloads.el"))  
)

;; --------------------------------------------------------------------------------
;; UTILITY FUNCTIONS
(defun konecpp-arglist-indent(elem)
  ;; Indents argument list first line with 8 spaces
  (save-excursion
    (goto-char (cdr elem))
    (vector (+ 8 (current-column)))
    ))

(defconst kone-c-style
  '("stroustrup"
    (c-tab-always-indent        . t)
    (show-trailing-whitespace	. t)
    (indent-tabs-mode           . nil)
    (tab-width                  . 4)
    (c-offsets-alist            . ((innamespace . 0)
                                   (inline-open . 0)
                                   (arglist-intro konecpp-arglist-indent)
                                   (inher-intro . ++)
                                   (member-init-intro . ++)
				   (substatement-open 0)))
    ))
  
;; Note: f5 and f6 do not work in current MacOs/Emacs
(defun mc-set-programming-mode (map)
  "Sets various variables that are useful with programming languages"
  (setq case-fold-search nil)
  (setq compilation-always-kill t)
  (setq compilation-ask-about-save nil)
  (fci-mode)
  ;; Offsets
  ;; (c-set-offset 'arglist-intro 'konecc-arglist-indent)
  ;; (c-set-offset 'inher-intro ++)
  ;; (c-set-offset 'substatement-open 0)
  ;; Keys
  (define-key map [S-f4] 'next-error)
  (define-key map [f7]   'compile)
  (define-key map [S-f7] 'prj-compile-c4s)
  (define-key map [C-f7] 'mc-file-header)
  (define-key map [M-f7] 'mc-remove-compilation-window)
  (define-key map [f8]   'mc-toggle-source)
  (define-key map [S-f8] 'projectile-find-other-file)
  (define-key map [C-f8] 'mc-narrow-to-function)
  (define-key map [C-delete] 'mc-remove-right-wspace)
  (define-key map (kbd "s-\"") '(lambda() (interactive) (insert "\"<<x<<\"")))
  ;; web server dev macros
  ;; (define-key map (kbd "M-g s") '(lambda() (interactive) (insert "\"<<GETS(0x0000)<<\"")))
  ;; (define-key map (kbd "M-g p") '(lambda() (interactive) (insert "\"<p>\"<<GETS(0x0000)<<\"</p>\"")))  
  (define-key mc-prefix (kbd "p 1")  '(lambda() (interactive) (mc-log-prt 1)))
  (define-key mc-prefix (kbd "p 2")  '(lambda() (interactive) (mc-log-prt 2)))
  (define-key mc-prefix (kbd "p 3")  '(lambda() (interactive) (mc-log-prt 3)))
  (define-key mc-prefix (kbd "p 4")  '(lambda() (interactive) (mc-log-prt 4)))
  (define-key mc-prefix (kbd "p 5")  '(lambda() (interactive) (mc-log-prt 5)))
  (define-key mc-prefix (kbd "p 6")  '(lambda() (interactive) (mc-log-prt 6)))
  (define-key mc-prefix (kbd "p 7")  '(lambda() (interactive) (mc-log-prt 7)))
  (define-key mc-prefix (kbd "v 1")  '(lambda() (interactive) (mc-log-vap 1)))
  (define-key mc-prefix (kbd "v 2")  '(lambda() (interactive) (mc-log-vap 2)))
  (define-key mc-prefix (kbd "v 3")  '(lambda() (interactive) (mc-log-vap 3)))
  (define-key mc-prefix (kbd "v 4")  '(lambda() (interactive) (mc-log-vap 4)))
  (define-key mc-prefix (kbd "v 5")  '(lambda() (interactive) (mc-log-vap 5)))
  (define-key mc-prefix (kbd "v 6")  '(lambda() (interactive) (mc-log-vap 6)))
  (define-key mc-prefix (kbd "v 7")  '(lambda() (interactive) (mc-log-vap 7)))
  )

;; --------------------------------------------------------------------------------
(defun mc-set-sh-mode ()
  (setq fill-column 120)
  (setq indent-tabs-mode nil)
  (setq tab-width 2)
  )

(defun mc-set-txt-mode ()
  (setq fill-column 120)
  (setq tab-width 4)
  )

(defun mc-set-org-mode ()
  (interactive)
  (define-key org-mode-map (kbd "C-c c") (kbd "C-c C-x C-b"))
  (define-key org-mode-map (kbd "C-c v") (kbd "C-u C-u C-c C-x C-b"))
  )

(defun mc-set-conf-mode ()
  (setq tab-width 4)
  )

(defun mc-set-cc-mode ()
  (c-set-style "KoneCPP")
  (setq fill-column 101)
  (mc-set-programming-mode c-mode-base-map)
  (define-key c-mode-base-map [C-M-tab] 'clang-format-region)
  (setq compile-command "./remote-build.sh")
  (setq ag-file-type "--cpp")
  ;;(message "Menacon cc-mode enabled.")
  )

(defun mc-set-java-mode ()
  (mc-set-programming-mode java-mode-map))

(defun mc-set-lisp-mode ()
  (mc-set-programming-mode lisp-mode-map)
  )

(defun mc-set-js-mode()
  (interactive)
  (setq indent-tabs-mode nil)
  (setq case-fold-search nil)
  (setq fill-column 120)
  (setq js-indent-level 2)
  (setq js2-mode-assume-strict t)
  ;; js2-highlight-level (0-3) default 2
  (define-key js2-mode-map (kbd "M-g d") '(lambda() (interactive) (insert "document.getElementById();")))
  (message "Menacon js2-mode initialized.")
  )

(defun mc-set-json-mode()
  (setq indent-tabs-mode nil)
  (setq case-fold-search nil)
  (setq tab-width 4)
  (setq js-indent-level 4)
  (setq fill-column 120)
  (setq show-trailing-whitespace t)
  )

(defun mc-set-sql-mode ()
  "Menacon sql mode settings"
  (setq indent-tabs-mode nil)
  (setq tab-width 4)
  )

(defun mc-set-yaml-mode ()
  "Menacon yaml (Swagger) mode settings"
  (setq indent-tabs-mode nil)
  (setq tab-width 2)
  )

(defun mc-set-html-mode()
  "Menacon html mode extras"
  (setq indent-tabs-mode nil)
  (setq fill-column 110)
  (define-key web-mode-map (kbd "C-M-p") '(lambda () (interactive) (insert "<p></p>")))
  (define-key web-mode-map (kbd "C-M-a") '(lambda () (interactive) (insert "<a href=\"\"></a>")))
  ;(fci-mode)
  ;(define-key html-helper-mode-map [C-delete] 'mc-remove-right-wspace)
  ;(define-key html-helper-mode-map "\C-c\C-d" 'mc-sgml-del-tag-contents)
  )

(defun mc-set-nxml-mode()
  "Menacon nXml mode extras"
  (setq indent-tabs-mode nil)
  (setq fill-column 110)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (define-key nxml-mode-map [M-delete] 'mc-sgml-del-tag-contents)
  )

(defun mc-set-py-mode()
  "Menacon Python mode extras"
  (setq indent-tabs-mode nil)
  (setq tab-width 4)
  (setq fill-column 120)
  ;;(setq whitespace-style (quote (face space-before-tab indentation-tab space-after-tab )))
  (whitespace-mode)
  )

(defun mc-set-php-mode()
  "Menacon php-mode"
  (setq indent-tabs-mode nil)
  (setq fill-column 105)
  (fci-mode)
  (setq tab-width 4)
  (setq php-template-compatibility nil)
  (c-set-offset 'case-label -)
  )

(defun mc-set-conf-mode()
  "Menacon conf mode"
  (setq truncate-lines t)
  )

(defun mc-set-robot-mode()
  (setq tab-width 4)
  (define-key robot-mode-map (kbd "<backtab>") '(lambda() (interactive) (insert "    ")))
  (setq fill-column 140)
  (setq truncate-lines t)
  (whitespace-mode)
  ;;(message "Menacon robot mode set.")
  )

(defun mc-set-log-mode()
  (setq tab-width 4)
  (setq truncate-lines t)
  )  
;; 
