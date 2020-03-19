EMACS ?= emacs
ELS = auctex.el init.el install-base.el menacon-modes.el menacon-pkg.el menacon.el misc-snippets.el kone.el
ELCS = $(ELS:.el=.elc)

%.elc: %.el
	$(EMACS) -Q -batch -L . -l package -f package-initialize -f batch-byte-compile $<

all: $(ELCS) 

.PHONY: all
