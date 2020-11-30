VPATH = ext
EMACS ?= emacs
ELS = auctex.el init.el install-base.el menacon-modes.el menacon-pkg.el menacon.el misc-snippets.el kone.el
EXT = ag.el robot-mode.el

ELCS = $(ELS:.el=.elc)
EXTS = $(EXT:.el=.elc)

%.elc: %.el
	$(EMACS) -Q -batch -L . -l package -f package-initialize -f batch-byte-compile $<

ext/%.elc: %.el
	$(EMACS) -Q -batch -L . -l package -f package-initialize -f batch-byte-compile $<

all: $(ELCS) $(EXT)

.PHONY: all
