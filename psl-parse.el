(setq lexical-binding t)

(with-current-buffer (get-buffer "*example*")
  (goto-char (point-min))
  ;(mpd-parse psl-tokens)
  (mpd-match 'expr psl-tokens))

(defvar psl-tokens
  '((expr      [block defvar deffun number object lambda funcapp id])
    (defvar    "defvar" id "=" expr "in" expr)
    (deffun    "deffun" id params expr "in" expr)
    (lambda    "lambda" params expr)
    (number    "[0-9]+")
    (block     "{" exprs "}")
    (exprs     expr [(";" exprs) ""])

    ;; Identifiers
    (id        "[a-zA-Z]+")
    (ids       id [("," id) ""])
    (params    "(" ids ")")

    ;; Function application
    (funcapp   id "(" exprs ")")

    ;; Objects
    (pairs     pair [("," pairs) ""])
    (pair      id ":" expr)
    (object    "{" ["" pairs] "}")))

(defvar psl-token-funcs
  `((number . ,(lambda (expr) (string-to-number (nth 1 expr))))))

(defun mpd-parse (tokens &optional funcs)
  "Return the next item in the current buffer."
  (dolist (token tokens)
    (let ((result (mpd-match (car token) tokens funcs)))
      (if result (return result)))))

(defun mpd--cons-if (head tail)
  "Cons only if tail is not nil."
  (if (mpd-match-p tail)
      (cons head tail)))

(defun mpd-match-list (list tokens funcs)
  (let ((result (mpd-match (car list) tokens funcs)))
    (when result
      (if (null (cdr list))
          (list result)
        (let ((rest (mpd-match-list (cdr list) tokens funcs)))
          (when rest
            (cons result rest)))))))

(defun mpd-match-regex (regex tokens funcs)
  (when (looking-at regex)
    (prog1 (buffer-substring-no-properties (point) (match-end 0))
      (goto-char (match-end 0)))))

(defun mpd-match-token (token tokens funcs)
  (let* ((pattern (cdr (assoc token tokens)))
         (match (mpd-match pattern tokens funcs)))
    (when match (cons token match))))

(defun mpd-match-or (vec tokens funcs)
  (dolist (option (coerce vec 'list))
    (let ((match (mpd-match option tokens funcs)))
      (when match (return match)))))

(defun mpd-match (pattern tokens &optional funcs)
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
