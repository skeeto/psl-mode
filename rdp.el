;;; rdp.el --- Recursive Descent Parser library

;; This is free and unencumbered software released into the public domain.

;; Author: Christopher Wellons <mosquitopsu@gmail.com>
;; URL: https://github.com/skeeto/psl-mode
;; Version: 0.1

;;; Commentary:

;; This library provides a recursive descent parser for parsing
;; languages in buffers. Some support is provided for implementing
;; automatic indentation based on the parser.

;;; Code:

(defvar rdp-best 0
  "The furthest most point that parsing reached. This information
can be used to determine where parsing failed.")

(defvar rdp-start 0
  "Position of point in original source buffer. The purpose is
for auto-indentation.")

(defvar rdp-point-stack ()
  "The token stack that contains the point. This is used for
auto-indentation.")

(defvar rdp-token-stack ()
  "Stack of tokens at this point.")

(defun rdp-box (value)
  "Box a parse return value, allowing nil to be a valid return."
  (vector value))

(defun rdp-unbox (box)
  "Unbox a parse return value."
  (aref box 0))

(defun rdp-get-token-func (token funcs)
  "Get the manipulation function for the given token."
  (cdr (assq token funcs)))

(defun rdp-parse (tokens &optional funcs pattern)
  "Return the next item in the current buffer."
  (setq rdp-best 0)
  (setq rdp-token-stack ())
  (if pattern
      (rdp-unbox (rdp-match pattern tokens funcs))
    (dolist (token tokens)
      (let ((result (rdp-match (car token) tokens funcs)))
        (if result (return (rdp-unbox result)))))))

(defun rdp-parse-string (string tokens &optional funcs pattern)
  "Like `rdp-parse' but operates on a string."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (rdp-parse tokens funcs pattern)))

(defun rdp-match-list (list tokens funcs)
  "Match all patterns in a list."
  (let ((result (rdp-match (car list) tokens funcs)))
    (when result
      (if (null (cdr list))
          (rdp-box (list (rdp-unbox result)))
        (let ((rest (rdp-match-list (cdr list) tokens funcs)))
          (when rest
            (rdp-box (cons (rdp-unbox result) (rdp-unbox rest)))))))))

(defun rdp-match-regex (regex tokens funcs)
  "Match a regex."
  (when (looking-at regex)
    (prog1 (rdp-box (buffer-substring-no-properties (point) (match-end 0)))
      (goto-char (match-end 0)))))

(defun rdp-match-token (token tokens funcs)
  "Match a token by name (symbol)."
  (push token rdp-token-stack)
  (let* ((pattern (cdr (assq token tokens)))
         (match (rdp-match pattern tokens funcs)))
    (pop rdp-token-stack)
    (when match
      (let ((macro (rdp-get-token-func token funcs)))
        (rdp-box (if macro
                     (funcall macro (rdp-unbox match))
                   (cons token (rdp-unbox match))))))))

(defun rdp-match-or (vec tokens funcs)
  "Match at least one pattern in the vector."
  (dolist (option (mapcar 'identity vec))
    (let ((match (rdp-match option tokens funcs)))
      (when match (return match)))))

(defun rdp-skip-whitespace ()
  "Skip over all whitespace."
  (search-forward-regexp "[[:space:]]*"))

(defun rdp-match (pattern tokens &optional funcs)
  "Match the given pattern object of any type (toplevel)."
  (rdp-skip-whitespace)
  (let ((start (point))
        (result (etypecase pattern
                  (string (rdp-match-regex pattern tokens funcs))
                  (list   (rdp-match-list  pattern tokens funcs))
                  (symbol (rdp-match-token pattern tokens funcs))
                  (vector (rdp-match-or    pattern tokens funcs)))))
    (when (and (<= (length rdp-point-stack) (length rdp-token-stack))
               (> rdp-start start)
               (> (point) rdp-start))
      (setq rdp-point-stack (reverse rdp-token-stack)))
    (unless result
      (setq rdp-best (max rdp-best (point)))
      (goto-char start))
    result))

(provide 'rdp)

;;; rdp.el ends here
