#!/bin/zsh
echo 'Packages base install'
emacs --batch -l install-base.el
echo 'Make local installation'
make
