EMACS ?= emacs
ELS = auctex.el init.el install-base.el kone-cc-mode.el menacon-modes.el menacon-pkg.el menacon.el misc-snippets.el
ELCS = $(ELS:.el=.elc)

%.elc: %.el
	$(EMACS) -Q -batch -L . -f batch-byte-compile $<

all: $(ELCS)

.PHONY: all
