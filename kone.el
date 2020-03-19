(defun kone-gcparams(ws)
  "Open gcparams from particular workspace a-c"
  (interactive "cWorkspace (a-c)")
  (let (fname)
    (setq fname (format "/Volumes/KONE/ws-%c/kcegc/src/gcparams/gcparams.json" ws))
    (find-file fname)
    )
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
