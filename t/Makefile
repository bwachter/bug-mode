ALL: module-tests functional-tests

.PHONY: module-tests test

TESTS =
TESTS += test-bug-auth.el
#TESTS += test-bug-comment-mode.el
#TESTS += test-bug-common-functions.el
#TESTS += test-bug-format.el
#TESTS += test-bug-list-mode.el
#TESTS += test-bug-mode-bz.el
#TESTS += test-bug-mode.el
#TESTS += test-bug-mode-rally.el
#TESTS += test-bug-rally-subscription-mode.el
#TESTS += test-bug-rpc-bz.el
#TESTS += test-bug-rpc.el
TESTS += test-bug-rpc-rally.el
#TESTS += test-bug-search-bz.el
#TESTS += test-bug-search-common.el
#TESTS += test-bug-search.el
#TESTS += test-bug-search-rally.e

functional-tests:
	@emacs -batch -Q --eval "(progn\
	(load-file \"test-init.el\")\
	(load-file \"bug-tests.el\")\
	(ert-run-tests-batch-and-exit))"

module-tests: $(TESTS)
	@for test in $^; do\
	  echo ">> Running tests from $$test";\
	  emacs -batch -Q --eval "(progn \
	    (load-file \"test-init.el\")\
	    (load-file \"$$test\")\
	    (ert-run-tests-batch-and-exit))";\
	done
