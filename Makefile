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

# ---------------------------------------------------------------------------
# Interactive Emacs debugging targets
# ---------------------------------------------------------------------------

.PHONY: test-emacs test-emacs-gui test-emacs-preconfigured test-emacs-preconfigured-gui tep tepg

EMACS_PRECONFIG := --init-directory=conf/preconfigured
EMACS_EMPTY     := --init-directory=conf/empty

test-emacs:
	@emacs -nw $(EMACS_EMPTY)

test-emacs-gui:
	@emacs $(EMACS_EMPTY)

test-emacs-preconfigured:
	@emacs -nw $(EMACS_PRECONFIG)

test-emacs-preconfigured-gui:
	@emacs $(EMACS_PRECONFIG)

tep: test-emacs-preconfigured
tepg: test-emacs-preconfigured-gui
