
(defun konecc-arglist-indent(elem)
  ;; Indents argument list first line with 8 spaces
  (save-excursion
    (goto-char (cdr elem))
    (vector (+ 8 (current-column)))
    ))

