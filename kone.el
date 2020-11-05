(defun kone-desktop-save()
  "Saves desktop to /Volumes/KONE"
  (interactive)
  (desktop-save "/Volumes/KONE/" t)
  )

(defun kone-desktop-read()
  "Saves desktop to /Volumes/KONE"
  (interactive)
  (desktop-read "/Volumes/KONE/")
  )

(defun kone-gcparams(ws)
  "Open gcparams from particular workspace a-d"
  (interactive "cWorkspace (a-d)")
  (let (fname)
    (setq fname (format "/Volumes/KONE/ws-%c/kcegc/src/gcparams/gcparams.json" ws))
    (find-file fname)
    )
  )

(defun kone-sitesnippet(ws)
  "Open gcparams from particular workspace a-d"
  (interactive "cWorkspace (a-d)")
  (let (fname)
    (setq fname (format "/Volumes/KONE/ws-%c/kcegc/config/pnb118/sitesnippet.conf" ws))
    (find-file fname)
    )
  )

(defun kone-wsnotes(ws)
  "Open notes from workspace a-d"
  (interactive "cWorkspace (a-d)")
  (let (fname)
    (setq fname (format "/Volumes/KONE/ws-%c/kcegc/kone-ajjnotes.txt" ws))
    (find-file fname)
    )
  )

(defun kone-desktop()
  "Open Kone desktop"
  (interactive)
  (desktop-change-dir "/Volumes/KONE/" )
  )

(defun kone-jsonpath()
  "Finds current path in json file."
  (interactive)
  (defconst ST_LEVEL 1)
  (defconst ST_LABEL 2)
  (end-of-line)
  (let (pt level ch path state lbl_end str)
    (setq pt (point))
    (setq state ST_LABEL)
    (setq level 1)
    (setq path '())
    (while (> pt 0)
      (setq ch (char-after pt))
      (cond
       ;; -------------------------------
       ((= state ST_LEVEL)
	(cond
	 ((= ch ?} )
	  (setq level (1+ level)))
	 ((= ch ?{ )
	  (setq level (1- level))
	  (when (eq level 0)
	    (setq level 1)
	    (setq state ST_LABEL))
	  )) ;; cond
	) ;; LEVEL
       ;; -------------------------------
       ((= state ST_LABEL)
	(when (= ch ?\")
	  ;; if first "
	  (if (= level 1)
	      (progn
		(setq level 0)
		(setq lbl_end pt))
	    ;; else second "
	    ;;(setq str (buffer-substring (1+ pt) lbl_end))
	    ;;(with-current-buffer (get-buffer "*scratch*") (insert str) (insert "\n"))
	    (push (buffer-substring (1+ pt) lbl_end) path)
	    (setq state ST_LEVEL)
	    (setq level 1)
	    )
	  )
	)
       )
      (setq pt (1- pt))
      );; while pt
    
    (dolist (elem path str)
      (setq str (concat str (concat "/" elem))) )
    (kill-new str)
    (message "JSON path: %s" str)
    )
  )
