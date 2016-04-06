ALL: bz-autoloads.el

SOURCES = bz.el
SOURCES = bz-auth.el bz-bug-mode.el bz-comment-mode.el 
SOURCES += bz-list-mode.el bz-rpc.el bz-search.el
SOURCES += bz-search-common.el bz-common-functions.el
SOURCES += bz-rpc-bz.el
ELC = $(SOURCES:.el=.elc)

%.elc: %.el
	emacs -batch -Q -L . -f batch-byte-compile $<

bz-autoloads.el: $(SOURCES)
	emacs -batch -Q -L . -eval "(progn\
	(let ((generated-autoload-file \"$(CURDIR)/bz-autoloads.el\")\
	      (make-backup-files nil))\
	  (update-directory-autoloads \".\")))"

