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
;; by the interpreter with C-c C-r (`psl-run-buffer'). Alternatively,
;; the code can be evaluated within Emacs itself with a built-in
;; ParselTongue compiler with C-c C-e (`psl-eval-buffer').

;; The ParselTongue specification:
;;  http://www.cs.brown.edu/courses/cs173/2012/Assignments/ParselTest/spec.html

;;; Code:

(require 'psl-compile)

(defgroup psl-mode nil
  "Options for editing ParselTongue code."
  :tag "ParselTongue")

(defcustom psl-program-name "psl"
  "Path to the ParselTongue interpreter.

Program must accept a ParselTongue program filename as its first
argument."
  :group 'psl-mode
  :type 'file)

(defcustom psl-indent-width 4
  "Indent width for ParselTongue mode."
  :group 'psl-mode
  :type 'integer)

(defvar psl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-r") 'psl-run-buffer)
    (define-key map (kbd "C-c C-e") 'psl-eval-buffer)
    map)
  "Keymap for ParselTongue mode.")

(defconst psl--keywords
  '("deffun" "defvar" "in" "if" "then" "else" "while" "lambda" "for"))

(defconst psl--builtin
  '("<" ">" "+" "-" "==" "print"))

(defconst psl--constants
  '("true" "false"))

(defconst psl-font-lock-keywords
  `((,(regexp-opt psl--keywords 'words) . font-lock-keyword-face)
    (,(regexp-opt psl--constants 'words) . font-lock-constant-face)
    (,(regexp-opt psl--builtin 'words) . font-lock-builtin-face)
    ("\\('\\w*'\\)" . font-lock-variable-name-face))
  "Minimal highlighting expressions for ParselTongue mode.")

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

(defvar psl-indent-tokens '(defvar deffun then else while for object)
  "Parser tokens that cause indentation.")

(defun psl-count-indent (stack)
  "Compute the indentation level from the token stack."
  (cond ((null stack) 0)
        ((eq (car stack) 'block)
         (+ (if (member (cadr stack) psl-indent-tokens) 0 1)
            (psl-count-indent (cdr stack))))
        ((member (car stack) psl-indent-tokens)
         (+ 1 (psl-count-indent (cdr stack))))
        (t (psl-count-indent (cdr stack)))))

(defun psl-indent-line-to (n)
  "Indent the current line, maintaining cursor position like
other modes do."
  (let ((col (current-column))
        (indent (current-indentation)))
    (if (<= col indent)
        (indent-line-to n)
      (indent-line-to n)
      (forward-char (- col indent)))))

(defun psl-indent-line ()
  "Indent current line as ParselTongue code."
  (interactive)
  (condition-case err
      (psl-compile-to-elisp)
    (error nil))
  (let ((indent (psl-count-indent (delete 'expr (reverse mpd-point-stack)))))
    (save-excursion
      (back-to-indentation)
      (if (looking-at "}") (setq indent (1- indent))))
    (psl-indent-line-to (max 0 (* psl-indent-width indent)))))

(psl-count-indent '(defvar expr if expr))

;;;###autoload
(define-derived-mode psl-mode prog-mode "Parsel"
  :group 'psl-mode
  (set (make-local-variable 'font-lock-defaults) '(psl-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'psl-indent-line)
  (set (make-local-variable 'comment-start) "#"))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.psl\\'" . psl-mode))

(define-derived-mode psl-output-mode special-mode "psl-output"
  "Output from ParselTongue interpreter.")

(defun psl-run-buffer ()
  "Run the interpreter (`psl-program-name') on the current buffer
displaying its output in *psl-output*."
  (interactive)
  (let ((program-txt (buffer-string))
        (tmpfile (make-temp-file "psl-")))
    (with-temp-file tmpfile (insert program-txt))
    (pop-to-buffer (get-buffer-create "*psl-output*") 'other-window)
    (psl-output-mode)
    (let (buffer-read-only)
      (erase-buffer)
      (set-process-sentinel
       (start-process "psl" (current-buffer) psl-program-name tmpfile)
       (lambda (proc state) (delete-file (cadr (process-command proc))))))))

(provide 'psl-mode)

;;; psl-mode.el ends here
