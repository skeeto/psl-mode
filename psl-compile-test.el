;; Usage:
;;   emacs -batch -L . -l psl-compile-test.el -f ert-run-tests-batch

(require 'ert)
(require 'cl)
(require 'psl-compile)

(defun psl-obj (&rest pairs)
  "Create a test ParselTongue object."
  (cons 'object (pairlis (loop for s in pairs by #'cddr collect s)
                         (loop for s in (cdr pairs) by #'cddr collect s))))

(defun psl-test-runner (program expected error)
  "Capture all program output for testing."
  (let ((actual "")
        (actual-error ""))
    (with-temp-buffer
      (insert program)
      (condition-case err
          (flet ((princ (str place) (setq actual (concat actual str))))
            (psl-eval-wrapper (psl-compile-to-elisp)))
        (error (setq actual-error (cadr err)))))
    (should (equal expected actual))
    (should (equal error actual-error))))

(ert-deftest psl-equality ()
  (should (psl-== (psl-obj :a 1 :b 2) (psl-obj :b 2 :a 1)))
  (should (psl-== (psl-obj :a 1 :b 2) (psl-obj :a 1 :b 2 :a 2)))
  (should-not (psl-== (psl-obj :a 1) (psl-obj :a 2)))
  (should (psl-== 4 4))
  (should (psl-== "foo" "foo"))
  (should-not (psl-== "foo" "foo2")))

(ert-deftest psl-printer ()
  (psl-test-runner "10" "10" "")
  (psl-test-runner "{}" "object" "")
  (psl-test-runner "lambda(x) {x}" "function" "")
  (psl-test-runner "\"hello, world\"" "hello, world" ""))

(ert-deftest psl-errors ()
  (psl-test-runner "+()" "" "Empty list for prim op")
  (psl-test-runner "-(\"foo\")" "" "Bad arguments to -"))
