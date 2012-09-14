;; Usage:
;;   emacs -batch -L . -l psl-compile-test.el -f ert-run-tests-batch

(require 'ert)
(require 'cl)
(require 'psl-compile)

(defun psl-obj (&rest pairs)
  "Create a test ParselTongue object."
  (cons 'object (pairlis (loop for s in pairs by #'cddr collect s)
                         (loop for s in (cdr pairs) by #'cddr collect s))))

(ert-deftest psl-equality ()
  (should (psl-== (psl-obj :a 1 :b 2) (psl-obj :b 2 :a 1)))
  (should (psl-== (psl-obj :a 1 :b 2) (psl-obj :a 1 :b 2 :a 2)))
  (should-not (psl-== (psl-obj :a 1) (psl-obj :a 2)))
  (should (psl-== 4 4))
  (should (psl-== "foo" "foo"))
  (should-not (psl-== "foo" "foo2")))
