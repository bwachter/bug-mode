EMACS=emacs
AUTOLOADS_FILE=bug-autoloads.el
ALL: $(AUTOLOADS_FILE)

.PHONY: clean

SOURCES = bug-auth.el bug-mode.el bug-comment-mode.el
SOURCES += bug-list-mode.el bug-rpc.el bug-search.el
SOURCES += bug-search-common.el bug-common-functions.el
SOURCES += bug-rally-subscription-mode.el
SOURCES += bug-rally-iteration.el
SOURCES += bug-format.el bug-custom.el
SOURCES += bug-debug.el bug-persistent-data.el
SOURCES += bug-backend-bz-rpc.el
SOURCES += bug-backend-rally.el
SOURCES += org-bug.el
ELC = $(SOURCES:.el=.elc)

# the load-file is required to compile macros properly
%.elc: %.el
	emacs -batch -Q -L . -eval "(load-file \"$<\")" -f batch-byte-compile $<

compile: $(AUTOLOADS_FILE) $(ELC)

clean:
	@$(RM) *.elc
	@$(RM) bug-autoloads.el

bug-autoloads.el: $(SOURCES)
	@$(EMACS) -Q --batch --eval "\
	 (loaddefs-generate '(\".\") \"$(AUTOLOADS_FILE)\"))"
