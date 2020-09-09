(require 'generic-x)

(defvar mc-logmode-map
  (let ((map (make-sparce-keymap)))
    (define-key [C-f8] 'logmode-dox)
    map)
  "Keymap for `mc-logmode'.")

(defvar mc-logmode-stxtbl
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?# "<" st)
    (modify-syntax-entry ?\n ">" st)
    st)
  "Syntax table for `mc-logmode'.")

(defvar mc-logmode-fl-keywords
  '(("function \\(\\sw+\\)" (1 font-lock-name-face)))
  "Keyword highlighting for `mc-logmode'.")

(defun logmode-dox()
  (interactive)
  (message "Menacon logmode dox")
  )

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.log\\'" . mc-logmode))

(define-derived-mode mc-logmode fundamental-mode "Text"
  "Major mode for menacon log files"
  :syntax-table mc-logmode-stxtbl
  (setq-local comment-start "# ")
  (setq-local font-lock-defaults 'mc-logmode-fl-keywords)
  )

(provide 'mc-logmode)
;;; end here
