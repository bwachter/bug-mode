ALL: bug-autoloads.el

.PHONY: clean

SOURCES = bug-auth.el bug-mode.el bug-comment-mode.el
SOURCES += bug-list-mode.el bug-rpc.el bug-search.el
SOURCES += bug-search-common.el bug-common-functions.el
SOURCES += bug-rpc-rally.el
SOURCES += bug-search-rally.el
SOURCES += bug-mode-rally.el
SOURCES += bug-rally-subscription-mode.el
SOURCES += bug-format.el bug-custom.el
SOURCES += bug-debug.el bug-persistent-data.el
SOURCES += bug-backend-bz-rpc.el
SOURCES += org-bug.el
ELC = $(SOURCES:.el=.elc)

# the load-file is required to compile macros properly
%.elc: %.el
	emacs -batch -Q -L . -eval "(load-file \"$<\")" -f batch-byte-compile $<

compile: bug-autoloads.el $(ELC)

clean:
	@$(RM) *.elc
	@$(RM) bug-autoloads.el

bug-autoloads.el: $(SOURCES)
	emacs -batch -Q -L . -eval "(progn\
	(let ((generated-autoload-file \"$(CURDIR)/bug-autoloads.el\")\
	      (make-backup-files nil))\
	  (update-directory-autoloads \".\")))"
