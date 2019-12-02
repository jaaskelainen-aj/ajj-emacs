(defvar mc-replace-original nil "Menacon replacement function orginal text")
(defvar mc-replace-text nil "Menacon replacement function replacement text")

(defun mc-replace ()
  "Replaces text. First call copies the original text from current selection
  Second call copies text to replace. Third call starts the query-replace process"
  (interactive)
  (if (eq mc-replace-original nil)
      (progn 
        (message "Original copied")
        (setq mc-replace-original (buffer-substring (region-beginning) (region-end))))
    (if (eq mc-replace-text nil)
        (progn
          (message "Replace text copied")
          (setq mc-replace-text (buffer-substring (region-beginning) (region-end))))
      (progn
        (perform-replace mc-replace-original mc-replace-text t nil nil)
        )
      )
    )
  )

(defun mc-replace-clear ()
  "Clears replace variables"
  (interactive)
  (setq mc-replace-text nil)
  (if (use-region-p)
      (progn 
        (message "Original copied")
        (setq mc-replace-original (buffer-substring (region-beginning) (region-end))))
	(setq mc-replace-original nil)
	(message "Menacon replace variables cleared")
	)
  )

(defun mc-replace-swap ()
  "Swaps the original and replacement strings"
  (interactive)
  (let (tmpStr)
    (setq tmpStr mc-replace-original)
    (setq mc-replace-original mc-replace-text)
    (setq mc-replace-text tmpStr))
  (message "Menacon replace variables swapped")
  )

(defun mc-menacon-el()
  "Opens menacon.el"
  (interactive)
  (find-file "/Users/anttim/Documents/lisp/menacon.el"))

(defun mc-cp-filename()
  (interactive)
  (kill-new (buffer-file-name)))

;; =================================================================================================
(defun mc-log-prt (level)
  "Inserts log-print message to point"
  (interactive "nLevel:")
  (cond ((eq level 1) (insert "CS_PRINT_TRCE(\"\");"))
		((eq level 2) (insert "CS_PRINT_DEBU(\"\");"))
		((eq level 3) (insert "CS_PRINT_INFO(\"\");"))
		((eq level 4) (insert "CS_PRINT_NOTE(\"\");"))
		((eq level 5) (insert "CS_PRINT_WARN(\"\");"))
		((eq level 6) (insert "CS_PRINT_ERRO(\"\");"))
		((eq level 7) (insert "CS_PRINT_CRIT(\"\");"))
		(t (message "Unknown level"))
		))

(defun mc-log-vap (level)
  "Inserts log-vaprt message to point"
  (interactive "nLevel:")
  (cond ((eq level 1) (insert "CS_VAPRT_TRCE(\"\",nn);"))
		((eq level 2) (insert "CS_VAPRT_DEBU(\"\",nn);"))
		((eq level 3) (insert "CS_VAPRT_INFO(\"\",nn);"))
		((eq level 4) (insert "CS_VAPRT_NOTE(\"\",nn);"))
		((eq level 5) (insert "CS_VAPRT_WARN(\"\",nn);"))
		((eq level 6) (insert "CS_VAPRT_ERRO(\"\",nn);"))
		((eq level 7) (insert "CS_VAPRT_CRIT(\"\",nn);"))
		(t (message "Unknown level"))
		))


;; =================================================================================================
(defun mc-prev-buf ()
  "Switches to previous buffer"
  (interactive)
  (switch-to-buffer (other-buffer))
  )

;; ..................................................................................................
(defun mc-remove-right-wspace()
  (interactive)
  (while (looking-at "[^a-z0-9A-Z()/=\&\*<]")
	(delete-char 1)
	)
  )

;; ..................................................................................................
(defun mc-remove-trailing-M ()
  "Removes the trailing ^M from a file."
  (interactive)
  (save-excursion
	(goto-char (point-min))
	(perform-replace "\^M" "" nil nil nil)
	(pop-mark))
  )

;; ..................................................................................................
(defun mc-save-kill ()
  "Normal buffers: Saves the current buffer and kills it. 
   Temprary buffers are simply killed and
   Scratch buffer is changed to previous buffer."
  (interactive)
  (if (string= (buffer-name) "*scratch*")
	  (switch-to-buffer (other-buffer))
	(if (char-equal ?* (aref (buffer-name) 0))
		(progn
		  (kill-buffer nil)
		  (delete-window)
		  )
	  (save-buffer)
	  (kill-buffer nil)
	  )
	)
  )
;; ..................................................................................................
(defun mc-insert-vtag()
  (interactive)
  (insert "_«V»")
  )
;; ..................................................................................................
(defun mc-remove-compilation-window()
  "Removes the compilatin window and deletes the compilation buffer"
  (interactive)
  (let (compBuffer)
	(setq compBuffer (get-buffer "*compilation*")) 
	(when compBuffer
	  (delete-windows-on compBuffer)
	  (kill-buffer compBuffer))))

;; ..................................................................................................
(defun mc-trim-eol()
  "Trims the end of line from spaces"
  (interactive)
  (save-excursion 
    (end-of-line)
    (backward-char)
    (while (looking-at " ")
      (delete-char 1)
      (backward-char))
    ))

;; ..................................................................................................
(defun mc-line-copy-char (&optional b)
  "Copy a character exactly below or above the point
to the current point of the cursor (default is above)."
  (interactive "p")
  (let (p col s)
    (setq p (point))
    (setq col (current-column))
    (forward-line (if b -1 1))
    (move-to-column col)
    (setq s (buffer-substring (point) (+ (point) 1)))
    (goto-char p)
    (insert s))
  )

;; ..................................................................................................
(defun mc-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (message "Match not found!"))))

;; ..................................................................................................
(defun mc-toggle-source ()
  "Switches to the source buffer if currently in the header buffer and vice versa."
  (interactive)
  (let ((buf (current-buffer))
        (name (file-name-nondirectory (buffer-file-name)))
        (c++-header-ext-regexp "\.\\(hpp\\|hh\\|h\\)$")
        (c++-source-ext-regexp "\.\\(cpp\\|cc\\|c\\)$")
        (c++-header-extension-list '("hpp" "hh" "h"))
        (c++-source-extension-list '("cpp" "cc" "c"))
        file
        offs)
    (setq offs (string-match c++-header-ext-regexp name))
    (if offs
        (let ((lst c++-source-extension-list)
              (ok nil)
              ext)
          (setq file (substring name 0 offs))
          (while (and lst (not ok))
            (setq ext (car lst))
            (if (file-exists-p (concat file "." ext))
                (setq ok t))
            (setq lst (cdr lst)))
          (if ok
              (find-file (concat file "." ext))))
      (let ()
        (setq offs (string-match c++-source-ext-regexp name))
        (if offs
            (let ((lst c++-header-extension-list)
                  (ok nil)
                  ext)
              (setq file (substring name 0 offs))
              (while (and lst (not ok))
                (setq ext (car lst))
                (if (file-exists-p (concat file "." ext))
                    (setq ok t))
                (setq lst (cdr lst)))
              (if ok
                  (find-file (concat file "." ext))))))
      ) ))

;; ..................................................................................................
(defun mc-attr2cpp (opt)
  "Creates a line of copy constructor. Supposes that the orinal is named 'original'"
  (interactive "N(1=Zero; 2=CopyConst.) ")
  (let (max bol varname)
	(beginning-of-line)
	(setq bol (point))
	(forward-line 1)
	(setq max (point))
	(forward-line -1)
	(when (search-forward ";" max t)
	  (backward-char 1)
	  (setq max (point))
	  (search-backward " " bol t)
	  (setq varname (buffer-substring (+ (point) 1) max))
	  (delete-region bol (point))
	  (search-forward ";" max t)
	  (backward-char 1)
	  (if (= opt 1)
		  (insert " = 0")
		(progn
		  (insert " = original.")
		  (insert varname))
		)
	  (forward-char 1)
	  (when (not (eolp))
		(kill-line)
		)
	  )
	)
  (c-indent-command)
  (forward-line 1)
  )

;; ..................................................................................................
(defun mc-ods-insert()
  "Inserts OutputDebugString -function to the beginning of each function"
  (interactive)
  (save-excursion
	(goto-char (point-min))
	(let (lcount)
	  (setq lcount 1)
	  (while (not (eq (point) (point-max)))
		(when (eq (char-after) ?\{)
		  (forward-char 1)
		  (insert "\n    OutputDebugString(\"" (buffer-name) " -- ")
		  (insert (format "%d\\n\");" lcount))
		  (setq lcount (1+ lcount)))
		(beginning-of-line 2))
	  (message (format "%d ODS commands added" lcount)))
	)
  )

;; ..................................................................................................
(defun mc-ods-remove()
  (interactive)
  (save-excursion
	(goto-char (point-min))
	(let (bol lcount)
	  (setq lcount 1)
	  (while (search-forward "OutputDebugString" nil t)
		(beginning-of-line)
		(setq bol (point))
		(forward-line 1)
		(delete-region bol (point))
		(setq lcount (1+ lcount)))
	  (message (format "%d ODS commands removed" lcount))
	  )
	)
  )

;; ..................................................................................................
(defun mc-path2lines()
  "Adds line feeds into a long path string separated with semicolons."
  (interactive)
  (beginning-of-line)
  (save-excursion 
	(while (not (= (char-after) ?\n)) ; new line
	  (when (= (char-after) ?\;) ; semicolon
		(forward-char 1)
		(insert "\n"))
	  (forward-char 1))
	)
  )

;; ..................................................................................................
(defun mc-lines2path()
  "Connects following lines of text into one long line. Stops at first empty line"
  (interactive)
  (save-excursion
	(let (contFlag)
	  (setq contFlag t)
	  (while contFlag
		(when (= (char-after) 10)
		  (delete-char 1)
		  (when (= (char-after) 10)
			  (setq contFlag nil)))
		(forward-char 1))
	  )))

;; ..................................................................................................
(defun mc-narrow-to-function()
  "Narrows the edit to current C/C++/Java function. Function is determined by searching back to { in position 1."
  (interactive)
  (save-excursion
	(let (beginPos)
	  (re-search-backward "^{")
	  (setq beginPos (point))
	  (re-search-forward "^}")
	  (narrow-to-region beginPos (point))
	  )))

;; ..................................................................................................
(defun mc-esc-quotes()
  "Escapes all quotation marks with in region (selection)"
  (interactive)
  (save-excursion
	(let (beg end)
	  (setq end (region-end))
	  (setq beg (region-beginning))
	  (goto-char beg)
	  (while (not (equal end (point)))
		(when (equal (char-after) ?\")
		  (insert "\\")
		  (setq end (+ end 1))
		  )
		(forward-char 1)
		)
	)))

;; ..................................................................................................
(defun mc-toggle-long-lines()
  "Toggles the line break = truncation of long lines in ecb-mode."
  (interactive)
  (if truncate-partial-width-windows
	  (setq truncate-partial-width-windows nil)
	(setq truncate-partial-width-windows t))
  (redraw-display)
  )

;; ..................................................................................................
(defun mc-tab2spaces(count)
  "Replace tabs with spaces."
  (interactive "n(number of spaces) ")
  (setq spaceStr (make-string count ? ))
  (save-excursion
	(while (search-forward "\t") 
	  (delete-char 1)
	  (insert spaceStr)
	  )
  ))
;; ..................................................................................................
(defun mc-mkbp()
  "Makes a gdb breakpoint to current source code line."
  (interactive)
  (setq bpstr (concat (buffer-name) ":" (number-to-string (line-number-at-pos))))
  (message bpstr)
  (kill-new bpstr)
  )
;; ..................................................................................................
(defun mc-count-chars()
  "Counts the characters within the reguion taking in consideration the escapes."
  (interactive)
  (save-excursion
	(let (end cntr)
	  (setq end (region-end))
	  (setq cntr 0)
	  (goto-char (region-beginning))
	  (while (not (equal end (point)))
		(when (not (equal (char-after) ?\\))
		  (setq cntr (+ cntr 1)))
		(forward-char 1))
	  (message "Number of chars: %d" cntr)))
  )

;; =================================================================================================
;;  WORD FUNCTIONS
;; =================================================================================================
(defun mc-test-cw()
  (interactive)
  (save-excursion
	(let (word)
	  (setq word (buffer-substring (mc-current-word) (point)))
	  (message (format "Found: %s" word))
	)))

(defun mc-current-word()
  "Returns a point to the beginning of the word leaving the cursor at the end of the word"
  (let (wordMark exp)
	(setq exp "[] /=&%*!?|,.#()}{^\'\"\+\[]")
	(re-search-backward (concat exp "\\|^"))
	(if (looking-at exp)
		(forward-char 1)
	  (when (not (bolp))
		(forward-char 1)
		))
    (setq wordMark (point))
	(forward-char 1)
	(re-search-forward (concat "$\\|" exp))
	(backward-char 1)
	(when (not (looking-at exp))
	  (forward-char 1)
	  )
	wordMark)
  )

(defun mc-search-this-word ()
  "Searches the word under the cursor. Uses register 4"
  (interactive)
  (let (searchWord start)
	(setq start (point))
	(setq searchWord (get-register 4))
	(when (not searchWord)		
	  (setq searchWord (buffer-substring (mc-current-word) (point)) ))
    (if (search-forward searchWord nil t 1)
		(set-register 4 searchWord)
	  (progn 
        (goto-char (point-min))
        (search-forward searchWord nil t 1)
        ;(backward-word 1)
        (if (> (point) start)
			(goto-char start)
		  (set-register 4 searchWord)
          ))
      )
    )
  )

(defun mc-search-clear ()
  "Clear the last searched word"
  (interactive)
  (set-register 4 nil)
  (message "Register 4 cleared")
  )
  
;; =================================================================================================
(defvar mc-mark1 nil "Marker for the following functions")

(defun mc-point-to-reg1()
  "This puts a current point to register one"
  (interactive)
  (setq mc-mark1 (make-marker))
  (set-marker mc-mark1 (point))
  )

(defun mc-copy-sel-to-point1()
  "This copies current selection to the point in register 2"
  (interactive)
  (let (msize)
	(if (< (mark) (point))
		(setq msize (- (point) (mark)))
	  (setq msize (- (mark) (point))))
	(copy-to-register 2 (mark) (point))
	(goto-char (marker-position mc-mark1))
	(insert-register 2 t)
	(set-marker mc-mark1 nil)
	(message "Copied %d chars" msize)
	)
  )

(defun mc-move-sel-to-point1()
  "This moves current selection to the point in register 2"
  (interactive)
  (let (msize)
	(if (< (mark) (point))
		(setq msize (- (point) (mark)))
	  (setq msize (- (mark) (point))))
	(copy-to-register 2 (mark) (point))
	(delete-region (mark) (point))
	(goto-char (marker-position mc-mark1))
	(insert-register 2 t)
	(set-marker mc-mark1 nil)
	(message "Moved %d chars" msize)
	)
  )

(defun mc-insert-reg2 ()
  "Inserts previously filled register 2"
  (interactive)
  (insert-register 2 t)
  )

;; =================================================================================================
;  SGML MODE FUNCTIONS
;; =================================================================================================
(defun mc-sgml-mode ()
  (interactive)
  (setq fill-column 100)
  (define-key sgml-mode-map [C-delete] 'mc-sgml-del-tag-contents)
  )

(defun mc-sgml-search-tag()
  "Search for the previous open tag. Returns position of the start tag and stores closing tag to register 4."
  (save-excursion
	(let (orig ndx startPos tagName)
	  (setq orig (point) ndx 0)
	  (catch 'break
		(while (search-backward "<" nil t)
		  (if (looking-at "</")
			  (incf ndx)
			(if (> ndx 0)
				(decf ndx)
			  (throw 'break ndx))
			)
		  ))
	  (if (looking-at "<")
		  (progn
			(setq startPos (+ (point) 1))
			(re-search-forward "[> ]" orig t)
			(setq tagName (concat "</" (buffer-substring startPos (- (point) 1)) ">"))
			(set-register 4 tagName)
			(- startPos 2))
		(- 0 1))
	  )
  ))

(defun mc-sgml-del-tag-contents ()
  "Function deletes tag contents"
  (interactive)
  (let (pos cp)
	(setq pos (mc-sgml-search-tag) cp (point))
	(if (< pos 0)
		(message "Open tag not found!")
	  (if (not (search-forward (get-register 4) nil t))
		  (message "Closing tag %s not found" (get-register 4))
		(if (y-or-n-p (format "Delete tag %s with %d characters? " (get-register 4) (- (point) pos)))
			(delete-region (point) pos)
		  (goto-char cp))
		))
	))

(defun mc-sgml-close-tag()
  "Closes the currently open tag."
  (interactive)
  (let (pos)
	(setq pos (mc-sgml-search-tag))
	(if (< pos 0)
		(message "Open tag not found!")
	  (insert-register 4))
	  ))

(defun mc-sgml-match-div()
  "Finds matching div"
  (interactive)
  (let (nested found mypos)
	(setq nested 0)
	(setq found 0)
	(setq mypos (point))
	(while (= found 0)
	  (if (re-search-forward "<div\\|</div" nil t)
		  (if (= (char-after (- (point) 4)) 47) ; 47 = /
			  (if (> nested 0)
				  (decf nested)
				(setq found 1))
			(incf nested))
		; div not found
		(setq found -1))
	  
	  )
	(if (= found 1)
		(message "Match FOUND")
	  (goto-char mypos)
	  (message "NO match"))
	))

;; =================================================================================================
;; FILE/FUNCTION HEADER FUNCTIONS
;; =================================================================================================
(defun mc-file-header ()
  "Menacon: insert comment to beginning of file"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (insert "/*! \\file ")
	(insert (buffer-name))
	(insert "\n * \\brief [short description] */\n")
    (insert "// Copyright (c) Menacon Oy\n/********************************************************************************/\n")
    )
  )

;; --------------------------------------------------------------------------------------------------
(defun mc-is-nul-param()
  "Returns true if the parameter name is available. Nil if it not.
   Use only from the mc-function-header function."
  (let (ch pnt)
	(setq pnt (- (point) 1))
	(while (eq (char-before pnt) 32)
	  (setq pnt (- pnt 1)))
	(if (or (eq (char-before pnt) ?\&) (eq (char-before pnt) ?\*))
		nil
	  t)
	)
  )

;; --------------------------------------------------------------------------------------------------
(defun mc-function-header()
  "Menacon: Insert function comment"
  (interactive)
  (beginning-of-line)
  (insert "// ==================================================================================================\n")
  (let (paramRemain paramList cmark editPoint voidFlag boolFlag ctorFlag)
    (setq boolFlag nil)
    (setq voidFlag nil)
    (setq ctorFlag nil)
    (mc-trim-eol)
    (end-of-line)
    (setq cmark (point))
	(when (eq (point) (point-max))
	  (progn
		(insert "\n{\n}\n")
		(goto-char cmark)))
    (beginning-of-line)
    ; Check for boolean, void and constructor functions
    (if (re-search-forward "^bool" cmark t)
        (setq boolFlag t)
      (if (re-search-forward "^void" cmark t)
          (setq voidFlag t)
        (progn
          (beginning-of-line)
          (forward-word 1)
          (if (looking-at ":")
              (setq ctorFlag t)
            ())))
      )
    ; Check for the empty parameter list. 
    (setq paramList (list))
    (if (re-search-forward "()$" cmark t)
        (setq paramRemain nil)
      (setq paramRemain t))
    ; Make a list of the parameter names
    (while paramRemain
      (if (eq nil (re-search-forward "[,)]" (point-max) t))
          (progn
            (message "Syntax error. ',' or ')' not found!")
            (setq paramRemain nil))
        (progn
          (when (eq (char-before) 41)
			(setq paramRemain nil))
		  (when (mc-is-nul-param)
			  (progn
				(backward-word 1)
				(setq cmark (point))
				(forward-word 1)
				(setq paramList (cons (buffer-substring cmark (point)) paramList))))
		  (forward-char))
        )
      )
    ;Add the parameters and other comments
    (setq paramList (reverse paramList))
	(backward-char)
    (beginning-of-line)
    (forward-line 1)
    (insert "/*!\n   ")
    (setq editPoint (point))
    ;(insert "\n")
    (while (car paramList)
      (insert "\n")
      (insert "   \\param ")
      (insert (car paramList))
      (insert " ...")
      (setq paramList (cdr paramList))
      )
    (if boolFlag
        (insert "\n   \\retval bool True on succes, false on error.\n")
      (if voidFlag
          (insert "\n")
        (if (not ctorFlag)
            (insert "\n   \\retval ...\n")
          ())))
    (insert "*/\n")
    (goto-char editPoint)
    )
  )

;----------------------------------------------------------------------------------------------------
(defun mc-h2cpp()
  "Takes the current line from the header file and makes a function frame from it into corresponding cpp file."
  (interactive)
  (let (offset filename hline classname)
    (setq offset (string-match "\\.h" (buffer-file-name)))
    (if (and offset (> offset 0))
        (progn 
          ;Copy the current line in header file into a hline variable
          (let ((start (save-excursion
                         (beginning-of-line)
                         (skip-chars-forward " \t")
                         (point)))
                (end (save-excursion
                       (end-of-line)
                       (skip-chars-backward "; \t")
                       (point))))
            (setq hline (buffer-substring start end)))

          ;Get the class name
          (setq classname (substring (buffer-name) 0 (string-match "\\.h" (buffer-name))))

          ;Change to cpp file
          (setq filename (concat (substring buffer-file-name 0 offset) ".cpp"))
          (find-file filename)

          ;Make the function frame 
          (goto-char (point-max))
          (insert "\n")
          (insert hline)
          (beginning-of-line)
          (save-excursion 
            (forward-word 1)
            (re-search-forward "[^\*&\ ]")
            ;(backward-char)
            (insert classname)
            (insert "::")
            (end-of-line)
            (insert "\n{\n}\n"))
          t
          )
      (message "This is not valid header file")
      nil
      )
    )
  )

;----------------------------------------------------------------------------------------------------
(defun mc-make-function()
  "Combines the actions from mc-h2cpp and mc-function-header thereby providing a way to create function frame with appropriate comments from current hearer file line. "
  (interactive)
  (when (mc-h2cpp)
      (mc-function-header))
  )

;; =================================================================================================
(defun mc-db-insert ()
  "Creates insert statement from create table script. "
  (interactive)
  (let (tblname namepoint delpoint (case-fold-search nil))
	(beginning-of-line)
	(setq delpoint (point))
	(search-forward "CREATE TABLE")
	(forward-char 1)
	(setq namepoint (point))
	(forward-word 1)
	(while (looking-at "_")
	  (forward-word 1))
	(setq tblname (buffer-substring namepoint (point)))
	(forward-line 2)
	(beginning-of-line)
	(delete-region delpoint (point))

	(insert "INSERT INTO ")
	(insert tblname)
	(insert "(")
	(while (not (char-equal (following-char) ?\n ))
	  (re-search-forward "[ ^I]")
	  (delete-char 1)
	  (insert ",")
	  (kill-line)
	  (delete-char 1)
	  )
	(delete-char 1)
	(insert ") VALUES ()\n")
	)
  )

;; =================================================================================================
(defun mc-tcm1()
  "Replace "
  (interactive)
  (save-excursion
	(let (beg end)
	  (goto-char (point-min))
	  (while (search-forward "REQUEST_" (point-max) t)
		(setq beg (mc-current-word))
		(setq end (point))
		(insert "\"")
		(goto-char beg)
		(insert "\"")
		(goto-char (+ end 1))
		)
	  (goto-char (point-min))
	  (while (search-forward "RECEIVE_" (point-max) t)
		(setq beg (mc-current-word))	  
		(setq end (point))
		(insert "\"")
		(goto-char beg)
		(insert "\"")	  
		(goto-char (+ end 1))
	 	)
	  )
	))

(defun mc-util1()
  "Replace "
  (interactive)
  (save-excursion
	(let (beg end)
	  (setq end (region-end)) ; (region-end)
	  (setq beg (region-beginning)) ; (region-beginning)
	  (goto-char beg)
	  (while (search-forward "Naisten telinevoimistelu" end t)
		(setq end (- end 21))
		(replace-match "NTV" t))
	  (goto-char beg)
	  (while (search-forward "Näytösvoimistelu" end t)
		(setq end (- end 14))
		(replace-match "NV" t))
	  (goto-char beg)
	  (while (search-forward "Rytminen voimistelu" end t)
		(setq end (- end 17))
		(replace-match "RV" t))
	  (goto-char beg)
	  (while (search-forward "luokka" end t)
		(setq end (- end 4))
		(replace-match "lk" t))
	  (goto-char beg)
	  (while (search-forward "Trampoliinivoimistelu" end t)
		(setq end (- end 4))
		(replace-match "TRA" t))
	  (goto-char beg)
	  (while (search-forward "\n" end t)
		(setq end (- end 4))
		(replace-match " + " t))
	  )
	))

(defun mc-port-mclib()
  "Port known mclib changes "
  (interactive)
  (save-excursion
	(let (beg end)
	  (setq end (point-max)) ; (region-end)
	  (setq beg (point-min)) ; (region-beginning)
	  (goto-char beg)
	  (while (search-forward "MCLinkedObject" end t)
		(replace-match "mclib::LinkedObject" t))
	  (goto-char beg)
	  (while (search-forward "MCLinkedList" end t)
		(replace-match "mclib::LinkedList" t))
	)))

(defun mc-c4s-frame()
  "Create c4s program frame"
  (interactive)
  (goto-char (point-min))
  (insert "#include <iostream>\n")
  (insert "using namespace std;\n")
  (insert "#include <cpp4scripts.hpp>\n")
  (insert "using namespace c4s;\n\n")
  (insert "program_arguments args;\n\n")
  (insert "int main(int argc, char **argv)\n{\n")
  (insert "    args += argument(\"-l\",  true, \"Sets VALUE as log level [TRACE|DEBUG|INFO|NOTICE|WARNING|ERROR|CRIT] for application.\");\n")
  (insert "    cout << \"[Program title]\\n\";\n")
  (insert "    try{\n")
  (insert "        args.initialize(argc,argv);\n")
  (insert "        LOG_LEVEL ll = args.is_set(\"-l\") ? logbase::str2level(args.get_value(\"-l\").c_str()) : LL_INFO;\n")
  (insert "        logger::init_log(ll, new stderr_sink());\n")
  (insert "    }catch(c4s_exception ce){\n")
  (insert "        cout << \"Initialization failed: \"<<ce.what()<<'\\n';\n")
  (insert "        args.usage();\n")
  (insert "        return 1;\n")
  (insert "    }\n")
  (insert "    cout << \"Done.\\n\";\n");
  (insert "    return 0;\n")
  (insert "}\n"))


(defun mc-cmd-condence()
  "Condenses everything from point to next ; into single line with single space in between the words."
  (interactive)
  (while (and (< (point) (point-max)) (not (looking-at ";")))
	(when (looking-at " ")
	  (just-one-space))
	(when (looking-at "\n")
	  (join-line t)
	  (insert " ")
	  )
	(forward-char 1)
	)
  )

(defun officetime()
  "Cleans up OfficeTime report"
  (interactive)
  (if (use-region-p)
	  (let (beg end)
		(setq end (region-end)) ; (region-end)
		(setq beg (region-beginning)) ; (region-beginning)
		(goto-char beg)
		(while(> (- end beg) 20)
		  (end-of-line)
		  (search-backward "\t")
		  (forward-char 1)
		  (setq end (- end (- (point) beg)))
		  (kill-region beg (point))
		  (end-of-line)
		  (delete-char 1)
		  (insert "; ")
		  (setq beg (point))
		))
	(message "Region not defined")
	))

  
;(defun mc-util3 ()
;  "Replace the old file header."
;  (interactive)
;  (goto-char (point-min))
;  (kill-line 5)
;  (insert "/*******************************************************************************\n")
;  (insert (buffer-name))
;  (insert "\nCopyright (c) Antti Merenluoto\n")
;  (insert "*******************************************************************************/\n")
;  (save-buffer)
;  )
