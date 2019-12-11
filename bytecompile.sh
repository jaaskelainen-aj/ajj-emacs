#!/bin/bash
EMACSLOADPATH=/Users/ajj/ajj-emacs/themes:/Users/ajj/ajj-emacs/ext:$EMACSLOADPATH
emacs --batch --eval '(byte-recompile-directory (expand-file-name "~/ajj-emacs") 0)'
