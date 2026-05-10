EMACS=emacs
ALL: lisp
MAKEFLAGS += --print-directory
GIT_TAG := $(shell git describe --tags --always --dirty 2>/dev/null)
TAG ?= $(if $(GIT_TAG),$(GIT_TAG),snapshot)

.PHONY: clean lisp tests compile autoloads CHANGELOG-current.md

clean:
	@rm -f *.elc
	@$(MAKE) -C lisp clean

# the load-file is required to compile macros properly
%.elc: %.el
	@$(EMACS) -batch -Q -L . -eval "(load-file \"$<\")" -f batch-byte-compile $<

compile: autoloads bug.elc
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
	@$(EMACS) -nw $(EMACS_EMPTY)

test-emacs-gui:
	@(EMACS) $(EMACS_EMPTY)

test-emacs-preconfigured:
	@(EMACS) -nw $(EMACS_PRECONFIG)

test-emacs-preconfigured-gui:
	@(EMACS) $(EMACS_PRECONFIG)

tep: test-emacs-preconfigured
tepg: test-emacs-preconfigured-gui

tar: clean autoloads
	@echo "-> bug-mode-$(TAG).tar.gz"
	@tar czf ../bug-mode-$(TAG).tar.gz --exclude '.git*' --exclude '*.md' --exclude '*.tar.gz' --exclude '*.elc' --exclude 'doc' --exclude 't' --exclude 'conf/' .

CHANGELOG.md: CHANGELOG.org
	@echo "-> CHANGELOG.md"
	@$(EMACS) --batch -Q $< --eval "(require 'ox-md)" \
				--eval "(setq org-export-with-toc nil)" \
				--funcall org-md-export-to-markdown

# let's see if github is happy with that for releases, or if we need to add some headers
CHANGELOG-current.md: CHANGELOG.org
	@$(EMACS) --batch -Q $< \
		--eval "(require 'ox-md)" \
		--eval "(setq org-export-with-toc nil)" \
		--eval "(progn \
			(goto-char (point-min)) \
			(let ((found nil)) \
				(org-element-map (org-element-parse-buffer) 'headline \
					(lambda (hl) \
						(when (and (not found) (= (org-element-property :level hl) 2)) \
							(setq found t) \
							(goto-char (org-element-property :begin hl)) \
							(org-narrow-to-subtree) \
							(org-export-to-file 'md \"$@\" nil t))))))"
