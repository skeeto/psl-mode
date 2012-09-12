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
       (start-process "psl"
                      (current-buffer)
                      psl-program-name
                      tmpfile)
       `(lambda (proc state) (delete-file ,tmpfile))))))

(provide 'psl-mode)

;;; psl-mode.el ends here
