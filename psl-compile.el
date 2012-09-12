;;; psl-compile.el --- ParselTongue parser and compiler

;; This is free and unencumbered software released into the public domain.

;; Author: Christopher Wellons <mosquitopsu@gmail.com>
;; URL: https://github.com/skeeto/psl-mode
;; Version: 0.1

;;; Commentary:

;; This code parses ParselTongue into s-expressions and compiles those
;; s-expressions into Emacs Lisp, which can be evaluated directly. You
;; can use `psl-eval-buffer' to compile the current buffer and
;; evaluate the result. Compilation output (mainly for compiler
;; debugging) can be viewed with `psl-show-elisp-compilation'.

;;; Known bugs:

;; * Assignment is an issue right now
;;   * Objects are mutable (object assignment semantics are wrong)
;;   * Operators += and -= not yet implemented
;; * Equality tests are incomplete
;; * The empty object is currently (), so it evaluates to false
;; * A big pile of semantic issues that I don't know about yet

;;; Code:

(require 'cl)
(require 'pp)

(defun psl-compile-to-elisp ()
  "Compile the current buffer into an Emacs Lisp s-expression."
  (let ((buffer (current-buffer)))
    (with-temp-buffer
      (insert-buffer-substring buffer) ; lose the text properties
      (psl-remove-comments)
      (goto-char (point-min))
      (mpd-match 'expr psl-tokens psl-token-funcs))))

(defun psl-eval-buffer ()
  "Evaluate the ParselTongue program in the buffer."
  (interactive)
  (psl-print (eval (psl-compile-to-elisp))))

(defun psl-show-elisp-compilation ()
  "Show the Emacs Lisp compilation in a buffer."
  (interactive)
  (pp-display-expression (psl-compile-to-elisp) "*Pp Eval Output*"))

(defun psl-+ (a &rest rest)
  "Implement ParselTongue's + function."
  (if (stringp a)
      (apply #'concat (cons a rest))
    (apply #'+ (cons a rest))))

(defun psl-- (a &rest rest)
  "Implement ParselTongue's - function."
  (if (null rest)
      a
    (apply #'- (cons a rest))))

(defun psl-== (a b)
  "Implement ParselTongue's == function."
  (equal a b))

(defun psl-print (o)
  "Implement ParselTongue's print function."
  (typecase o
    (function (princ "function" t))
    (list     (princ "object" t))
    (t (princ o t)))
  o)

(defun psl-remove-comments ()
  "Remove ParselTongue comments from the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "#[^\n]*\n" nil t)
      (replace-match "" nil nil))))

;;; ParselTongue grammar

(defvar psl-tokens
  '((expr      [block defvar deffun number object lambda string
                true false if while for inc-pre inc-post dec-pre dec-post
                assign + - == < > print funcall index message id])
    (pexpr     "(" expr ")")
    (defvar    "defvar" id "=" expr "in" expr)
    (deffun    "deffun" id params expr "in" expr)
    (lambda    "lambda" params expr)
    (true      "true")
    (false     "false")
    (number  . "[0-9]+")
    (string  . "\"\\(?:[^\"\\\\]\\|\\\\.\\)*\"")
    (block     "{" exprs ";?" "}")
    (exprs     expr [(";" exprs) ""])

    ;; Control structures
    (if        "if" expr "then" expr "else" expr)
    (while     "while" "(?" expr ")?" expr)
    (for       "for" "(" expr ";" expr ";" expr ")" expr)

    ;; Identifiers
    (id      . "[a-zA-Z<>=]+")
    (ids       id [("," ids) ""])
    (params    "(" [ids ""] ")")

    ;; Function application
    (funcall   [block index pexpr id] args)
    (args      "(" [aexprs ""] ")")
    (aexprs    expr [("," aexprs) ""])

    ;; Builtins
    (+         "+" args)
    (-         "-" args)
    (==        "==" args)
    (<         "<" args)
    (>         ">" args)
    (print     "print" args)

    ;; Operators
    (inc-pre   "++" id)
    (inc-post  id "++")
    (dec-pre   "--" id)
    (dec-post  id "--")
    (assign    [index id] "=" expr)

    ;; Objects
    (pairs     pair [("," pairs) ""])
    (pair      id ":" expr)
    (object    "{" [pairs ""] "}")
    (index     [pexpr id] [by-id index-str])
    (message   [pexpr id] "@" [index-str field] args)
    (by-id     "\\." field)
    (field   . id)
    (index-str "\\[" expr "\\]"))
  "The ParselTongue grammar.")

(defvar psl-token-funcs
  `((expr     . ,(lambda (token expr) (car expr)))
    (pexpr    . ,(lambda (token expr)  (nth 1 expr)))
    (number   . ,(lambda (token num)  (string-to-number num)))
    (id       . ,(lambda (token name) (intern name)))
    (deffun   . ,(lambda (token list)
                   (destructuring-bind (deffun id params expr in body) list
                     `(let ((,id (lambda ,(if (stringp params) () params)
                                   ,expr)))
                        ,body))))
    (defvar   . ,(lambda (token list)
                   (destructuring-bind (defvar id eq expr in inexpr) list
                     `(let ((,id ,expr)) ,inexpr))))
    (string   . ,(lambda (token string) (read string)))
    (params   . ,(lambda (token params) (nth 1 params)))
    (args     . ,(lambda (token args) (nth 1 args)))
    (ids      . ,#'psl--tuck)
    (exprs    . ,(lambda (token exprs) (psl--tuck token exprs)))
    (funcall  . ,(lambda (token call) (psl--funcall (nth 0 call) (nth 1 call))))
    (aexprs   . ,(lambda (token exprs) (psl--tuck token exprs)))
    (block    . ,(lambda (token exprs) (cons 'progn (nth 1 exprs))))
    (true     . ,(lambda (token true) t))
    (false    . ,(lambda (token false) '(not t))) ; can't return nil
    (pair     . ,(lambda (token pair)
                   `(cons (quote ,(nth 0 pair)) ,(nth 2 pair))))
    (pairs    . ,#'psl--tuck)
    (object   . ,(lambda (token obj)
                   (let ((fields (nth 1 obj)))
                     (if (stringp fields)
                         (list 'list)
                       (cons 'list fields)))))
    (if       . ,(lambda (token expr)
                   (destructuring-bind (if cond then expra else exprb) expr
                     `(if ,cond ,expra ,exprb))))
    (while    . ,(lambda (token expr)
                   (destructuring-bind (while ps cond pe body) expr
                     `(while ,cond ,body))))
    (for      . ,(lambda (token e)
                   (destructuring-bind (for ps init b1 cond b2 inc pe body) e
                     `(progn ,init (while ,cond ,body ,inc)))))
    (lambda   . ,(lambda (token lm)
                   (destructuring-bind (fn params body) lm
                     `(lambda ,(if (stringp params) () params) ,body))))
    (inc-pre  . ,(lambda (token expr)
                   (let ((id (cadr expr)))
                     `(setq ,id (1+ ,id)))))
    (dec-pre  . ,(lambda (token expr)
                   (let ((id (cadr expr)))
                     `(setq ,id (1- ,id)))))
    (inc-post  . ,(lambda (token expr)
                    (let ((id (car expr)))
                      `(prog1 ,id (setq ,id (1+ ,id))))))
    (dec-post  . ,(lambda (token expr)
                    (let ((id (car expr)))
                      `(prog1 ,id (setq ,id (1- ,id))))))
    (assign    . ,(lambda (token assign)
                    (destructuring-bind (lhs eq expr) assign
                      `(setf ,lhs ,expr))))
    (+         . ,(lambda (token op) (psl--funcall 'psl-+ (nth 2 op))))
    (-         . ,(lambda (token op) (psl--funcall 'psl-- (nth 2 op))))
    (<         . ,(lambda (token op) (psl--funcall '< (nth 2 op))))
    (>         . ,(lambda (token op) (psl--funcall '> (nth 2 op))))
    (==        . ,(lambda (token op) (psl--funcall 'psl-== (nth 2 op))))
    (print     . ,(lambda (token op) (psl--funcall 'psl-print (nth 2 op))))
    (index     . ,(lambda (token index)
                    (let ((obj (nth 0 index)))
                      `(cdr (assq ,(nth 1 index) ,obj)))))
    (message   . ,(lambda (token msg)
                    (destructuring-bind (obj at f args) msg
                      `(funcall (cdr (assq ,f ,obj))
                                ,@(psl--apply o args)))))
    (field     . ,(lambda (token field) `(quote ,field)))
    (index-str . ,(lambda (token index) `(intern ,(nth 1 index))))
    (by-id     . ,(lambda (token id) (cadr id))))
  "Syntax tree manipulation functions.")

(defun psl--apply (f args)
  (if (stringp args)
      (list f)
    (cons f args)))

(defun psl--funcall (f args)
  `(funcall ,f ,@(if (stringp args) () args)))

(defun psl--tuck (token names)
  (if (stringp (nth 1 names))
      (list (car names))
    (cons (car names) (cadadr names))))

;;; Parser functions

(defun mpd-get-token-func (token funcs)
  "Get the manipulation function for the given token."
  (or (cdr (assq token funcs)) #'cons))

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
  (let* ((pattern (cdr (assq token tokens)))
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

(provide 'psl-compile)

;;; psl-compile.el ends here
