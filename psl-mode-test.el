;; Usage:
;;   emacs -batch -L . -l psl-mode-test.el -f ert-run-tests-batch

(require 'ert)
(require 'psl-mode)

(ert-deftest psl-indent-line-test ()
  "Test automatic indentation."
  (with-temp-buffer
    (psl-mode)
    (insert "deffun f()\ntrue")
    (psl-indent-line)
    (should (= (current-indentation) psl-indent-width)))
  (with-temp-buffer
    (psl-mode)
    (insert "# deffun\n10")
    (psl-indent-line)
    (should (= (current-indentation) 0)))
  (with-temp-buffer
    (psl-mode)
    (insert "deffvar v = true in {\n    # defvar\n10")
    (psl-indent-line)
    (should (= (current-indentation) psl-indent-width))))
