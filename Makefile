ALL: lisp

.PHONY: clean lisp tests

clean:
	@$(MAKE) -C lisp clean

%.elc: %.el
	emacs -batch -Q -L . -f batch-byte-compile $<

compile:
	@$(MAKE) -C lisp compile

lisp:
	@$(MAKE) -C lisp

test: lisp
	@$(MAKE) -C t
