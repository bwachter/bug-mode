ALL: lisp

.PHONY: clean lisp tests

clean:
	@$(MAKE) -C lisp clean

%.elc: %.el
	emacs -batch -Q -L . -f batch-byte-compile $<

lisp:
	@$(MAKE) -C lisp

test: lisp
	@emacs -batch -Q --eval "(progn\
	(load-file \"t/bug-tests.el\")\
	(ert-run-tests-batch-and-exit))"
