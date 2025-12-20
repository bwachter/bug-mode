ALL: lisp
MAKEFLAGS += --print-directory

.PHONY: clean lisp tests compile autoloads

clean:
	@$(MAKE) -C lisp clean

%.elc: %.el
	emacs -batch -Q -L . -f batch-byte-compile $<

compile:
	@$(MAKE) -C lisp compile

autoloads: lisp/bug-autoloads.el

lisp/bug-autoloads.el:
	@$(MAKE) -C lisp bug-autoloads.el

lisp:
	@$(MAKE) -C lisp

test: lisp
	@$(MAKE) -C t
