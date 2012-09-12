;;; psl-mode.el --- major mode for ParselTongue

;; This is free and unencumbered software released into the public domain.

;; Author: Christopher Wellons <mosquitopsu@gmail.com>
;; URL: https://github.com/skeeto/psl-mode
;; Version: 0.1

;;; Commentary:

;; Automatic indentation is a little flaky. It assumes the coding
;; style is going to be fairly reasonable. Currently, multiline if
;; statements won't indent properly without curly brackets.

;; The indentation size can be set with `psl-indent-width'.

;; After setting up `psl-program-name' the current buffer can be run
;; by the interpreter with C-c C-r (`psl-run-buffer').

;; The ParselTongue specification:
;;  http://www.cs.brown.edu/courses/cs173/2012/Assignments/ParselTest/spec.html

;;; Code:

(eval-when-compile (require 'cl))

(defvar psl-mode-hook nil
  "Hook for ParselTongue mode.")

(defvar psl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-r") 'psl-run-buffer)
    map)
  "Keymap for ParselTongue mode.")

(defvar psl-program-name "psl"
  "The path to an interpreter which accepts a ParselTongue
program filename as its first argument.")

(defvar psl--keywords
  '("deffun" "defvar" "in" "if" "then" "else" "while" "lambda"))
(defvar psl--builtin
  '("<" ">" "+" "-" "==" "print"))
(defvar psl--constants
  '("true" "false"))

(defconst psl-font-lock-keywords
  (list
   `(,(regexp-opt psl--keywords 'words) . font-lock-keyword-face)
   `(,(regexp-opt psl--constants 'words) . font-lock-constant-face)
   `(,(regexp-opt psl--builtin 'words) . font-lock-builtin-face)
   '("\\('\\w*'\\)" . font-lock-variable-name-face))
  "Minimal highlighting expressions for ParselTongue mode.")

(defvar psl-indent-width 4
  "Indent width for ParselTongue mode.")

(defvar psl-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?+ "w" st)
    (modify-syntax-entry ?- "w" st)
    (modify-syntax-entry ?< "w" st)
    (modify-syntax-entry ?> "w" st)
    (modify-syntax-entry ?= "w" st)
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
             ((looking-at "\\(^[^#\n{]*}[^{]*$\\)")
              (setq cur-indent (- (current-indentation) psl-indent-width)))
             ((looking-at "\\(^[^#\n}]*\\(?:{\\|deffun\\|defvar\\)[^}]*$\\)")
              (setq cur-indent (+ (current-indentation) psl-indent-width)))
             ((bobp) (setq cur-indent (current-indentation)))))))
      (indent-line-to (max 0 (or cur-indent 0))))))

;;;###autoload
(defun psl-mode ()
  "Mode for editing ParselTongue code. \\{psl-mode-map}"
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

(define-derived-mode psl-output-mode special-mode "psl-output"
  "Output from ParselTongue interpreter.")

(defun psl-run-buffer ()
  "Run the interpreter (`psl-program-name') on the current buffer
displaying its output in *psl-output*."
  (interactive)
  (lexical-let ((out (get-buffer-create "*psl-output*"))
                (program (buffer-string))
                (tmpfile (make-temp-file "psl-")))
    (with-temp-buffer
      (insert program)
      (write-file tmpfile))
    (with-current-buffer out
      (psl-output-mode)
      (setq buffer-read-only nil)
      (erase-buffer))
    (set-process-sentinel (start-process "psl" out psl-program-name tmpfile)
                          (lambda (proc state)
                            (delete-file tmpfile)))
    (with-current-buffer out (setq buffer-read-only t))
    (pop-to-buffer out 'other-window)))

(provide 'psl-mode)

;;; psl-mode.el ends here
