;;; psl-mode.el -- major mode for ParselTongue

;; This is free and unencumbered software released into the public domain.

;; Author: Christopher Wellons <mosquitopsu@gmail.com>
;; URL: https://github.com/skeeto/psl-mode
;; Version: 0.1

;;; Commentary:

;; Automatic indentation is a little flaky. It assumes the coding
;; style is going to be fairly reasonable. Currently, multiline if
;; statements won't indent properly without curly brackets.

;; The indentation size can be set with `psl-indent-width'.

;; The ParselTongue specification:
;;  http://www.cs.brown.edu/courses/cs173/2012/Assignments/ParselTest/spec.html

;;; Code:

(defvar psl-mode-hook nil
  "Hook for ParselTongue mode.")

(defvar psl-mode-map (make-sparse-keymap)
  "Keymap for ParselTongue mode.")

(defvar psl--keywords '("deffun" "defvar" "in"))
(defvar psl--functions '("<" ">" "+" "-" "==" "print"))
(defvar psl--builtin '("for" "while" "lambda"))

(defconst psl-font-lock-keywords
  (list
   `(,(regexp-opt psl--keywords 'words) . font-lock-keyword-face)
   `(,(regexp-opt psl--functions 'words) . font-lock-function-name-face)
   `(,(regexp-opt psl--builtin 'words) . font-lock-builtin-face)
   '("\\('\\w*'\\)" . font-lock-variable-name-face))
  "Minimal highlighting expressions for ParselTongue mode.")

(defvar psl-indent-width 4
  "Indent width for ParselTongue mode.")

(defvar psl-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?# "< b" st)
    (modify-syntax-entry ?\n "> b" st)
    st)
  "Syntax table for ParselTongue mode.")

(defun psl-indent-line ()
  "Indent current line as ParselTongue code."
  (interactive)
  (beginning-of-line)
  (if (bobp)
      (indent-line-to 0)
    (let (cur-indent)
      (if (looking-at "^[ \t]*\\(?:}\\|in\\)")
          (save-excursion
            (forward-line -1)
            (setq cur-indent (- (current-indentation) psl-indent-width)))
        (save-excursion
          (while (not cur-indent)
            (forward-line -1)
            (cond
             ((looking-at "^[ \t]*}")
              (setq cur-indent (current-indentation)))
             ((looking-at "\\(^.*}[^{]*$\\)")
              (setq cur-indent (- (current-indentation) psl-indent-width)))
             ((looking-at "\\(^.*\\(?:{\\|deffun\\|defvar\\)[^}]*$\\)")
              (setq cur-indent (+ (current-indentation) psl-indent-width)))
             ((bobp) (setq cur-indent (current-indentation)))))))
      (indent-line-to (max 0 (or cur-indent 0))))))

;;;###autoload
(defun psl-mode ()
  "Mode for editing ParselTongue code."
  (interactive)
  (kill-all-local-variables)
  (use-local-map psl-mode-map)
  (set-syntax-table psl-mode-syntax-table)
  (set (make-local-variable 'font-lock-defaults) '(psl-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'psl-indent-line)
  (set (make-local-variable 'comment-start) "#")
  (setq major-mode 'psl-mode)
  (setq mode-name "ParselTongue")
  (run-hooks 'psl-mode-hook))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.psl\\'" . psl-mode))

(provide 'psl-mode)

;;; psl-mode.el ends here
