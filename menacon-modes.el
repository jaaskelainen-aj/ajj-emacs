;; HOOKS

(defun mc-modes-init()
  (add-hook 'web-mode-hook     'mc-set-html-mode)
  (add-hook 'nxml-mode-hook    'mc-set-nxml-mode)
  (add-hook 'text-mode-hook    'mc-set-txt-mode)
  ;; Programmin modes
  (add-hook 'c-mode-common-hook     'mc-set-cc-mode)
  ;;(add-hook 'c++-mode-hook   'mc-set-c++-mode)
  (add-hook 'java-mode-hook    'mc-set-java-mode)
  (add-hook 'sql-mode-hook     'mc-set-sql-mode)
  (add-hook 'php-mode-hook     'mc-set-php-mode)
  (add-hook 'nxml-mode-hook    'mc-set-nxml-mode)
  (add-hook 'sh-mode-hook      'mc-set-sh-mode)
  (add-hook 'python-mode-hook  'mc-set-py-mode)
  (add-hook 'js-mode-hook      'mc-set-js-mode)
  (add-hook 'json-mode-hook    'mc-set-json-mode)

  (defvaralias 'c-basic-offset 'tab-width)
  (defvaralias 'js-indent-level 'tab-width)

  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js[x]$\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.json5?$\\'" . json-mode))
  (add-to-list 'auto-mode-alist '("\\.txt?\\'" . text-mode))

  (add-to-list 'auto-mode-alist '("\\.\\(h||c4s\\)\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.mak$" . makefile-mode))

  ;(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))
  ;(autoload 'csharp-mode "csharp-mod;; e" "Major mode for editing C# code." t)
  ;(setq auto-mode-alist (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))
  ;(setq auto-mode-alist (cons '("\\.jsx$" . javascript-mode) auto-mode-alist))
  ;(setq auto-mode-alist (cons '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\|dita\\)\\'" . nxml-mode) auto-mode-alist))
  ;(setq interpreter-mode-alist (cons '("python" . python-mode) interpreter-mode-alist))
  ;(setq auto-mode-alist (cons '("\\.\\(py\\|cal\\)\\'" . python-mode) auto-mode-alist))
  ;(autoload 'python-mode "python-mode" "Python editing mode." t)
					; WebMode lists
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.?css\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.robot\\'" . robot-mode))

  ;; (require 'scons-mode)
  ;;- (add-to-list 'auto-mode-alist '("\\SConscript" . scons-mode))
  ;;- (add-to-list 'auto-mode-alist '("\\SConstruct" . scons-mode))

  ;; Set the styles
  (c-add-style "KoneCPP" kone-c-style)
  ;; (setq c-default-style "KoneCPP")

  ;; Turn global modes on
  (helm-mode 1)
  (global-hl-line-mode)
  (electric-pair-mode)
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
    (c-basic-offset             . 4)
    (fill-column		. 85)
    (c-offsets-alist            . ((innamespace . 0)
                                   (inline-open . 0)
                                   (arglist-intro konecpp-arglist-indent)
                                   (inher-intro . ++)
                                   (member-init-intro . ++)
				   (substatement-open 0)))
    ))
  
(defun mc-set-programming-mode (map)
  "Sets various variables that are useful with programming languages"
  (setq case-fold-search nil)
  ;; Offsets
  ;; (c-set-offset 'arglist-intro 'konecc-arglist-indent)
  ;; (c-set-offset 'inher-intro ++)
  ;; (c-set-offset 'substatement-open 0)
  ;; Keys
  (define-key map [f6]   'next-error)
  (define-key map [C-f6] 'mc-function-header)
  (define-key map [f7]   'compile)
  (define-key map [S-f7] 'prj-compile-c4s)
  (define-key map [C-f7] 'mc-file-header)
  (define-key map [M-f7] 'mc-remove-compilation-window)
  (define-key map [f8]   'mc-toggle-source)
  (define-key map [C-f8] 'mc-narrow-to-function)
  (define-key map [C-delete] 'mc-remove-right-wspace)
  ;; web server dev macros
  ;;? (define-key map (kbd "M-g s") '(lambda() (interactive) (insert "\"<<GETS(0x0000)<<\"")))
  ;;? (define-key map (kbd "M-g p") '(lambda() (interactive) (insert "\"<p>\"<<GETS(0x0000)<<\"</p>\"")))  
  ;;? (define-key map "\C-cp1"  '(lambda() (interactive) (mc-log-prt 1)))
  ;;? (define-key map "\C-cp2"  '(lambda() (interactive) (mc-log-prt 2)))
  ;;? (define-key map "\C-cp3"  '(lambda() (interactive) (mc-log-prt 3)))
  ;;? (define-key map "\C-cp4"  '(lambda() (interactive) (mc-log-prt 4)))
  ;;? (define-key map "\C-cp5"  '(lambda() (interactive) (mc-log-prt 5)))
  ;;? (define-key map "\C-cp6"  '(lambda() (interactive) (mc-log-prt 6)))
  ;;? (define-key map "\C-cp7"  '(lambda() (interactive) (mc-log-prt 7)))
  ;;? (define-key map "\C-cv1"  '(lambda() (interactive) (mc-log-vap 1)))
  ;;? (define-key map "\C-cv2"  '(lambda() (interactive) (mc-log-vap 2)))
  ;;? (define-key map "\C-cv3"  '(lambda() (interactive) (mc-log-vap 3)))
  ;;? (define-key map "\C-cv4"  '(lambda() (interactive) (mc-log-vap 4)))
  ;;? (define-key map "\C-cv5"  '(lambda() (interactive) (mc-log-vap 5)))
  ;;? (define-key map "\C-cv6"  '(lambda() (interactive) (mc-log-vap 6)))
  ;;? (define-key map "\C-cv7"  '(lambda() (interactive) (mc-log-vap 7)))
  )

;; --------------------------------------------------------------------------------
(defun mc-set-txt-mode ()
  (setq fill-column 120)
  (setq tab-width 4)
  )

(defun mc-set-cc-mode ()
  (c-set-style "KoneCPP")
  (mc-set-programming-mode c-mode-base-map)
  )

(defun mc-set-java-mode ()
  (mc-set-programming-mode java-mode-map))

(defun mc-set-lisp-mode ()
  (mc-set-programming-mode lisp-mode-map))

(defun mc-set-js-mode()
  (setq indent-tabs-mode nil)
  (setq case-fold-search nil)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq fill-column 120)
  (message "mc-set-js-mode completed.")
  )

(defun mc-set-json-mode()
  (setq indent-tabs-mode nil)
  (setq case-fold-search nil)
  (setq tab-width 2)
  (setq fill-column 120)
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
  (interactive)
  (setq indent-tabs-mode t)
  (setq tab-width 4)
  (setq fill-column 120)  
  (setq whitespace-style (quote
						  (face space-before-tab indentation-tab space-after-tab )))
  (whitespace-mode)
  (message "Menacon Python mode completed")
  )

(defun mc-set-php-mode()
  "Menacon php-mode"
  (interactive)
  (setq indent-tabs-mode nil)
  (setq fill-column 110)  
  (setq tab-width 4)
  (setq php-template-compatibility nil)  
  )
