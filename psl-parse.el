(eval-when-compile (require 'cl))

;;; ParselTongue grammar

(with-current-buffer (get-buffer "*example*")
  (goto-char (point-min))
  (mpd-match 'expr psl-tokens psl-token-funcs))

(defvar psl-tokens
  '((expr      [block defvar deffun number object lambda funcapp string id])
    (defvar    "defvar" id "=" expr "in" expr)
    (deffun    "deffun" id params expr "in" expr)
    (lambda    "lambda" params expr)
    (number  . "[0-9]+")
    (string  . "\"\\(?:[^\"\\\\]\\|\\\\.\\)*\"")
    (block     "{" exprs "}")
    (exprs     expr [(";" exprs) ""])

    ;; Identifiers
    (id      . "[a-zA-Z]+")
    (ids       id [("," id) ""])
    (params    "(" ids ")")

    ;; Function application
    (funcapp   id "(" exprs ")")

    ;; Objects
    (pairs     pair [("," pairs) ""])
    (pair      id ":" expr)
    (object    "{" ["" pairs] "}"))
  "The ParselTongue grammar.")

(defvar psl-token-funcs
  `((number . ,(lambda (token num)  (string-to-number num)))
    (id     . ,(lambda (token name) (intern name)))
    (deffun . ,(lambda (token list)
                 (destructuring-bind (deffun id params expr in inexpr) list
                   (list token id params expr inexpr))))
    (string . ,(lambda (token string) (read string))))
  "Syntax tree manipulation functions.")

;;; Parser functions

(defun mpd-get-token-func (token funcs)
  "Get the manipulation function for the given token."
  (or (cdr (assoc token funcs)) #'cons))

(defun mpd-parse (tokens &optional funcs)
  "Return the next item in the current buffer."
  (dolist (token tokens)
    (let ((result (mpd-match (car token) tokens funcs)))
      (if result (return result)))))

(defun mpd-match-list (list tokens funcs)
  "Match all patterns in a list."
  (let ((result (mpd-match (car list) tokens funcs)))
    (when result
      (if (null (cdr list))
          (list result)
        (let ((rest (mpd-match-list (cdr list) tokens funcs)))
          (when rest
            (cons result rest)))))))

(defun mpd-match-regex (regex tokens funcs)
  "Match a regex."
  (when (looking-at regex)
    (prog1 (buffer-substring-no-properties (point) (match-end 0))
      (goto-char (match-end 0)))))

(defun mpd-match-token (token tokens funcs)
  "Match a token by name (symbol)."
  (let* ((pattern (cdr (assoc token tokens)))
         (match (mpd-match pattern tokens funcs)))
    (when match (funcall (mpd-get-token-func token funcs) token match))))

(defun mpd-match-or (vec tokens funcs)
  "Match at least one pattern in the vector."
  (dolist (option (coerce vec 'list))
    (let ((match (mpd-match option tokens funcs)))
      (when match (return match)))))

(defun mpd-match (pattern tokens &optional funcs)
  "Match the given pattern object of any type (toplevel)."
  (search-forward-regexp "[[:space:]]*")
  (let ((start (point))
        (result (etypecase pattern
                  (string (mpd-match-regex pattern tokens funcs))
                  (list   (mpd-match-list  pattern tokens funcs))
                  (symbol (mpd-match-token pattern tokens funcs))
                  (vector (mpd-match-or    pattern tokens funcs)))))
    (unless result
      (goto-char start))
    result))
