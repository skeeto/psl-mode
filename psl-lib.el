;;; psl-lib.el --- ParselTongue library functions for compiled code

;; This is free and unencumbered software released into the public domain.

;;; Commentary:

;; This provides an environment in which to run compiled ParselTongue.

;;; Code:

;; Evaluation functions

(eval-when-compile (require 'psl-compile))

(defun psl-eval-buffer ()
  "Evaluate the ParselTongue program in the buffer."
  (interactive)
  (require 'psl-compile)
  (psl-eval-wrapper (psl-compile-to-elisp)))

(defun psl-batch-eval ()
  "Run a ParselTongue program from the command line. It would be
used like this:
    emacs -Q --batch -l psl-lib.el -f psl-batch-eval script.psl"
  (setq vc-handled-backends nil)        ; disable spurious messages
  (let ((filename (car (last command-line-args))))
    (with-temp-buffer
      (insert-file-contents-literally filename)
      (rename-buffer filename)
      (psl-eval-buffer)
      (psl-print "\n"))))

(defun psl-eval-wrapper (sexp)
  "Evaluate a compiled ParselTongue program inside a wrapper that
handles errors properly."
  (require 'cl)
  (condition-case err
      (psl-print (eval sexp))
    (wrong-number-of-arguments
     (error "Application failed with arity mismatch"))
    (void-variable
     (error (format "Unbound identifier: %s" (cadr err))))))

;; ParselTongue environment

(defun psl-+ (&rest args)
  "Implement ParselTongue's + function."
  (cond ((null args) (error "Empty list for prim op"))
        ((not (memq nil (mapcar #'stringp args))) (apply #'concat args))
        ((not (memq nil (mapcar #'numberp args))) (apply #'+ args))
        (t (error "Bad arguments to +"))))

(defun psl-- (&rest args)
  "Implement ParselTongue's - function."
  (cond ((null args) (error "Empty list for prim op"))
        ((not (memq nil (mapcar #'numberp args)))
         (if (= 1 (length args)) (car args) (apply #'- args)))
        (t (error "Bad arguments to -"))))

(defun psl-== (a b)
  "Implement ParselTongue's == function."
  (labels ((pair< (a b) (string< (symbol-name (car a)) (symbol-name (car b))))
           (clean (a) (unless (null a)
                        (cons (car a) (clean (if (eq (caar a) (caadr a))
                                                 (cddr a)
                                               (cdr a)))))))
    (cond
     ((and (psl-object-p a) (psl-object-p b))
      (equal (clean (sort (copy-sequence (cdr a)) #'pair<))
             (clean (sort (copy-sequence (cdr b)) #'pair<))))
     (t (equal a b)))))

(defun psl-< (&rest args)
  "Implement ParselTongue's < functions."
  (if (not (= 2 (length args)))
      (error "Bad primop")
    (destructuring-bind (a b) args
      (if (and (numberp a) (numberp b))
          (< a b)
        (error (format "Bad arguments for <:\n%s\n%s"
                       (psl-print-to-string a) (psl-print-to-string b)))))))

(defun psl-> (&rest args)
  "Implement ParselTongue's > functions."
  (if (not (= 2 (length args)))
      (error "Bad primop")
    (destructuring-bind (a b) args
      (if (and (numberp a) (numberp b))
          (> a b)
        (error (format "Bad arguments for >:\n%s\n%s"
                       (psl-print-to-string a) (psl-print-to-string b)))))))

(defun psl-object-p (o)
  "Return t if argument is ParselTongue object."
  (and (listp o) (eq (car o) 'object)))

(defun psl-print-to-string (o)
  "Print a ParselTongue value to a string."
  (cond ((eq o t)          "true")
        ((null o)          "false")
        ((functionp o)     "function")
        ((psl-object-p o)  "object")
        (t (format "%s" o))))

(defun psl-print (o &rest rest)
  "Implement ParselTongue's print function."
  (if rest
      (error "Bad primop")
    (princ (psl-print-to-string o) t))
  o)

(provide 'psl-lib)

;;; psl-lib.el ends here
