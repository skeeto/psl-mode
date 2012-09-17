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

;; Parsing actually requires *a lot* of stack space and there's not
;; much that can be done about it. Fortunately Emacs is capable of
;; providing plenty of stack for parsing but the default limit set by
;; `max-lisp-eval-depth' (600) is too low. To work around this,
;; temporarily while parsing the parser will increase the stack size
;; by a factor of `psl-stack-multiplier'.

;;; Known bugs:

;; * Loops return the wrong value
;; * Probably some evaluation order mistakes

;;; Planned features:

;; * Support for calling Elisp functions

;;; Won't fix:

;; * Function equality is fundamentally broken
;;   - It's broken in the reference implementation and was
;;     acknowledged as a bad design choice.
;; * Semicolon chaining not supported outside of blocks ({ ... }).
;;   - The spec is completely ambiguous about it and the reference
;;     implementation follows complex, unpredictable precedence
;;     rules. For example, if expressions (usually) chain on the if
;;     expression as a whole rather than the trailing else
;;     expression. Why does it do this?

;;; Code:

(eval-when-compile (require 'cl))
(require 'rdp)

(defvar psl-stack-multiplier 4
  "Increase `max-lisp-eval-depth' by this factor when parsing.")

(defun psl-compile-to-elisp ()
  "Compile the current buffer into an Emacs Lisp s-expression."
  (let ((buffer (current-buffer)))
    (setq rdp-start (save-excursion (beginning-of-line) (point)))
    (setq rdp-point-stack ())
    (with-temp-buffer
      (insert-buffer-substring buffer) ; lose the text properties
      (psl-remove-comments)
      (goto-char (point-min))
      (let ((sexp
             (let ((max-lisp-eval-depth
                    (floor (* psl-stack-multiplier max-lisp-eval-depth))))
               (rdp-parse psl-tokens psl-token-funcs 'expr))))
        (rdp-skip-whitespace)
        (if (= (point) (point-max))
            sexp
          (error (format "%s:%d:%d: Encountered error while parsing"
                         (buffer-name buffer)
                         (line-number-at-pos rdp-best)
                         (save-excursion (goto-char rdp-best)
                                         (current-column)))))))))

(defun psl-show-elisp-compilation ()
  "Show the Emacs Lisp compilation in a buffer."
  (interactive)
  (require 'pp)
  (let ((print-circle t)
        (print-gensym t))
    (pp-display-expression (psl-compile-to-elisp) "*Pp Eval Output*")))

(defun psl-remove-comments ()
  "Remove ParselTongue comments from the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "#[^\n]*\n" nil t)
      (replace-match
       (concat (make-string (- (match-end 0) (match-beginning 0) 1) ? ) "\n")
       nil nil))))

;;; ParselTongue grammar

(defvar psl-tokens
  '((expr    . [block defvar deffun number object lambda string true false
                if while for assign assignm inc-pre inc-post dec-pre dec-post
                + - == < > print funcall index message pexpr id])
    (empty   . "")
    (pexpr     "(" expr ")")
    (defvar    "defvar" id "=" expr in)
    (deffun    "deffun" id params expr in)
    (in        "in" expr)
    (lambda    "lambda" params expr)
    (true      "true")
    (false     "false")
    (number  . "[0-9]+")
    (string  . "\"\\(?:[^\"\\\\]\\|\\\\.\\)*\"")
    (block     "{" exprs ";?" "}")
    (exprs     expr [(";" exprs) empty])

    ;; Control structures
    (if        "if" expr then else)
    (then      "then" expr)
    (else      "else" expr)
    (while     "while" expr expr)
    (for       "for" "(" expr ";" expr ";" expr ")" expr)

    ;; Identifiers
    (id      . "[a-zA-Z_][a-zA-Z0-9_]*")
    (ids       id [("," ids) empty])
    (params    "(" [ids empty] ")")

    ;; Function application
    (funcall   [block index pexpr id] args)
    (args      "(" [aexprs empty] ")")
    (aexprs    expr [("," aexprs) empty])

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
    (assign    [index= id] "=" expr)
    (assignm   [index= id] "[+-]=" expr)

    ;; Objects
    (pairs     pair [("," pairs) empty])
    (pair      id ":" expr)
    (object    "{" [pairs empty] "}")
    (index     [pexpr message id] [by-id index-str])
    (index=    [pexpr message id] [by-id index-str])
    (message   [pexpr id] "@" [index-str field] args)
    (by-id     "\\." field)
    (field   . id)
    (index-str "\\[" expr "\\]"))
  "The ParselTongue grammar.")

(defvar psl-token-funcs
  `((expr     . ,#'identity)
    (empty    . ,(lambda (empty) ()))
    (pexpr    . ,(apply-partially 'nth 1))
    (number   . ,#'string-to-number)
    (id       . ,#'intern)
    (deffun   . ,(lambda (list)
                   (destructuring-bind (deffun id params expr body) list
                     `(let ((,id (lambda ,params ,expr)))
                        ,body))))
    (defvar   . ,(lambda (list)
                   (destructuring-bind (defvar id eq expr body) list
                     `(lexical-let ((,id ,expr)) ,body))))
    (in       . ,(apply-partially 'nth 1))
    (string   . ,#'read)
    (params   . ,(apply-partially 'nth 1))
    (args     . ,(apply-partially 'nth 1))
    (ids      . ,#'psl--tuck)
    (exprs    . ,#'psl--tuck)
    (funcall  . ,(lambda (call)
                   `(funcall ,(nth 0 call) ,@(nth 1 call))))
    (aexprs   . ,#'psl--tuck)
    (block    . ,(lambda (exprs) (cons 'progn (nth 1 exprs))))
    (true     . ,(lambda (true) t))
    (false    . ,(lambda (false) nil))
    (pair     . ,(lambda (pair) (cons (nth 0 pair) (nth 2 pair))))
    (pairs    . ,(lambda (pairs) (cons (car pairs) (cadadr pairs))))
    (object   . ,(lambda (obj)
                   (let ((fields (nth 1 obj)))
                     `(list 'object ,@(psl--make-object fields)))))
    (if       . ,(lambda (expr)
                   (destructuring-bind (if cond then else) expr
                     `(if ,cond ,then ,else))))
    (then     . ,#'cadr)
    (else     . ,#'cadr)
    (while    . ,(lambda (expr)
                   (destructuring-bind (while cond body) expr
                     `(while ,cond ,body))))
    (for      . ,(lambda (e)
                   (destructuring-bind (for ps init b1 cond b2 inc pe body) e
                     `(progn ,init (while ,cond ,body ,inc)))))
    (lambda   . ,(lambda (lm)
                   (destructuring-bind (fn params body) lm
                     `(lambda ,params ,body))))
    (inc-pre  . ,(lambda (expr)
                   (let ((id (cadr expr)))
                     `(setq ,id (1+ ,id)))))
    (dec-pre  . ,(lambda (expr)
                   (let ((id (cadr expr)))
                     `(setq ,id (1- ,id)))))
    (inc-post  . ,(lambda (expr)
                    (let ((id (car expr)))
                      `(prog1 ,id (setq ,id (1+ ,id))))))
    (dec-post  . ,(lambda (expr)
                    (let ((id (car expr)))
                      `(prog1 ,id (setq ,id (1- ,id))))))
    (assign    . ,(lambda (assign)
                    (destructuring-bind (lhs eq expr) assign
                      (if (symbolp lhs)
                          `(setq ,lhs ,expr)
                        (destructuring-bind (obj field) lhs
                          `(cons 'object (acons ,field ,expr (cdr ,obj))))))))
    (assignm   . ,(lambda (assign)
                    (destructuring-bind (lhs opstr expr) assign
                      (let ((op (if (equal opstr "+=") '+ '-)))
                        (if (symbolp lhs)
                            `(setq ,lhs (,op ,lhs ,expr))
                          (destructuring-bind (obj field) lhs
                            `(cons 'object
                              (acons ,field
                               (,op (cdr (or (assq ,field (cdr ,obj))
                                         (error "Field not found: %s" ,field)))
                                    ,expr) ,obj))))))))
    (+         . ,(lambda (op) (cons 'psl-+ (nth 1 op))))
    (-         . ,(lambda (op) (cons 'psl-- (nth 1 op))))
    (<         . ,(lambda (op) (cons 'psl-< (nth 1 op))))
    (>         . ,(lambda (op) (cons 'psl-> (nth 1 op))))
    (==        . ,(lambda (op) (cons 'psl-== (nth 1 op))))
    (print     . ,(lambda (op) (cons 'psl-print (nth 1 op))))
    (index     . ,(lambda (index)
                    `(cdr (or (assq ,(nth 1 index) (cdr,(nth 0 index)))
                              (error "Field not found: %s" ,(nth 1 index))))))
    (index=    . ,#'identity)
    (message   . ,(lambda (msg)
                    (destructuring-bind (obj at f args) msg
                      `(funcall (cdr (assq ,f (cdr ,obj)))
                                ,@(cons obj args)))))
    (field     . ,(lambda (field) `(quote ,field)))
    (index-str . ,(lambda (index) `(intern ,(nth 1 index))))
    (by-id     . ,#'cadr))
  "Syntax tree manipulation functions.")

(defun psl--tuck (names)
  "Turn nested chains of things into a flat list."
  (cons (car names) (cadadr names)))

(defun psl--make-object (fields)
  "Create part a new object from a parsed s-exp."
  (if (null fields)
      ()
    (if (assq (caar fields) (cdr fields))
        '((error "Multiply-defined fields"))
      (cons `(cons (quote ,(caar fields)) ,(cdar fields))
            (psl--make-object (cdr fields))))))

(provide 'psl-compile)

;;; psl-compile.el ends here
