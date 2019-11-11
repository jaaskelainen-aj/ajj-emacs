(defun logindent(elem)
  (let ((log (get-buffer-create "*log*"))
	(cc (c-lineup-arglist-intro-after-paren elem)) )
    (print (format "ref=%d cc=%d" (cdr elem) (seq-elt cc 0)) log)
    )
  (vector 10)
  )

