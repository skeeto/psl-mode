(eval-when-compile (require 'cl))

(defun psl-eval-buffer ()
  "Evaluate the ParselTongue program in the buffer."
  (interactive)
  (let ((buffer (current-buffer)))
    (with-temp-buffer
      (insert-buffer-substring buffer) ; lose the text properties
      (goto-char (point-min))
      (princ (eval (mpd-match 'expr psl-tokens psl-token-funcs)) t))))

;;; ParselTongue grammar

(with-temp-buffer
  (insert-buffer-substring (get-buffer-create "*example*"))
  (goto-char (point-min))
  (mpd-match 'expr psl-tokens psl-token-funcs))

(defvar psl-tokens
  '((expr      [block defvar deffun number object lambda funcall string
                true false id])
    (defvar    "defvar" id "=" expr "in" expr)
    (deffun    "deffun" id params expr "in" expr)
    (lambda    "lambda" params expr)
    (true      "true")
    (false     "false")
    (number  . "[0-9]+")
    (string  . "\"\\(?:[^\"\\\\]\\|\\\\.\\)*\"")
    (block     "{" exprs "}")
    (exprs     expr [(";" exprs) ""])

    ;; Identifiers
    (id      . "[a-zA-Z<>=+-]+")
    (ids       id [("," ids) ""])
    (params    "(" ids ")")

    ;; Function application
    (funcall   id "(" aexprs ")")
    (aexprs    expr [("," aexprs) ""])

    ;; Objects
    (pairs     pair [("," pairs) ""])
    (pair      id ":" expr)
    (object    "{" [pairs ""] "}"))
  "The ParselTongue grammar.")

(defvar psl-token-funcs
  `((expr     . ,(lambda (token expr) (car expr)))
    (number   . ,(lambda (token num)  (string-to-number num)))
    (id       . ,(lambda (token name) (intern name)))
    (deffun   . ,(lambda (token list)
                 (destructuring-bind (deffun id params expr in inexpr) list
                   `(flet ((,id ,params ,expr)) ,inexpr))))
    (defvar   . ,(lambda (token list)
                 (destructuring-bind (defvar id eq expr in inexpr) list
                   `(let ((,id ,expr)) ,inexpr))))
    (string   . ,(lambda (token string) (read string)))
    (params   . ,(lambda (token params) (nth 1 params)))
    (ids      . ,#'psl--tuck)
    (exprs    . ,(lambda (token exprs) (psl--tuck token exprs)))
    (funcall  . ,(lambda (token call) (cons (nth 0 call) (nth 2 call))))
    (aexprs   . ,(lambda (token exprs) (psl--tuck token exprs)))
    (block    . ,(lambda (token exprs) (cons 'progn (nth 1 exprs))))
    (true     . ,(lambda (token true) t))
    (false    . ,(lambda (token false) '(not t))) ; can't return nil
    (pair     . ,(lambda (token pair)
                   `(cons (quote ,(nth 0 pair)) ,(nth 2 pair))))
    (pairs    . ,#'psl--tuck)
    (object   . ,(lambda (token obj)
                   (cons 'list (nth 1 obj)))))
  "Syntax tree manipulation functions.")

(defun psl--tuck (token names)
  (if (stringp (nth 1 names))
      (list (car names))
    (cons (car names) (cadadr names))))

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
