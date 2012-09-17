TESTS = $(wildcard *-test.el)
EMACS = emacs

test : $(patsubst %.el,%,$(TESTS))

%-test : %-test.el
	$(EMACS) -Q -batch -L .. -l $< -f ert-run-tests-batch

.PHONY : test
