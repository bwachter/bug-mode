ALL: lisp

.PHONY: lisp

ELC = $(SOURCES:.el=.elc)

%.elc: %.el
	emacs -batch -Q -L . -f batch-byte-compile $<

lisp:
	@$(MAKE) -C lisp
