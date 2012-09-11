(setq lexical-binding t)

(with-current-buffer (get-buffer "*example*")
  (goto-char (point-min))
  (mpd-parse psl-tokens))

(defvar psl-tokens
  '((expr      (block defvar deffun number object lambda id))
    (defvar    "defvar" id "=" expr "in" expr)
    (deffun    "deffun" id params expr "in" expr)
    (lambda    "lambda" params expr)
    (number    "[0-9]+")
    (block     "{" exprs "}")
    (exprs     expr ((";" exprs) ""))

    ;; Identifiers
    (id        "[a-zA-Z]+")
    (ids       id (("," id) ""))
    (params    "(" ids ")")

    ;; Function application
    (funcapp   "(" exprs ")")

    ;; Objects
    (pairs     pair (("," pairs) ""))
    (pair      id ":" expr)
    (object    "{" ("" pairs) "}")))

(defun mpd-parse (tokens)
  "Return the next item in the current buffer."
  (let ((start (point)))
    (dolist (token tokens)
      (message (format "Trying %s" (car token)))
      (let ((result (mpd-match tokens (cdr token))))
        (if (eq result 'no-match)
            (goto-char start)
          (return (cons (car token) result)))))))

(defun mpd-skip-space ()
  "Skip over whitespace."
  (search-forward-regexp "[[:space:]]*"))

(defun mpd--cons-if (head tail)
  "Cons only if tail is no `no-match'."
  (if (not (eq tail 'no-match))
      (cons head tail)
    'no-match))

(defun mpd-match (tokens expr)
  "Return the matching tokens if expr matches, otherwise return
the symbol `no-match'."
  (if (null expr)
      ()
    (mpd-skip-space)
    (let ((this (car expr))
          (rest (cdr expr)))
      (etypecase this
        (string
         (if (looking-at this)
           (let* ((end (match-end 0))
                  (start (point))
                  (capture (buffer-substring (point) end)))
             (goto-char end)
             (let ((result (mpd-match tokens rest)))
               (if (not (eq result 'no-match))
                   (cons capture result)
                 (goto-char start)
                 'no-match)))
           'no-match))
        (symbol
         (let ((result (mpd-match tokens (cdr (assoc this tokens)))))
           (if (not (eq result 'no-match))
               (mpd--cons-if (cons this result) (mpd-match tokens rest))
             'no-match)))
        (list
         (dolist (item this 'no-match)
           (let ((res1 (mpd-match tokens (if (listp item) item (list item)))))
             (if (not (eq res1 'no-match))
                 (let ((res2 (mpd-match tokens rest)))
                   (if (not (eq res2 'no-match))
                       (return (append res1 res2))))))))))))
