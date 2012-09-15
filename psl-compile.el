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

;; * Semicolon chaining not supported (outside of { } that is)
;; * Loops return the wrong value
;; * Probably some evaluation order mistakes
;; * Environment not quite right: defvar functions can recurse

;;; Planned features:

;; * Support for calling Elisp functions

;;; Won't fix:

;; * Function equality is fundamentally broken

;;; Code:

(eval-when-compile (require 'cl))
(require 'pp)

(defvar psl-stack-multiplier 4
  "Increase `max-lisp-eval-depth' by this factor when parsing.")

(defun psl-compile-to-elisp ()
  "Compile the current buffer into an Emacs Lisp s-expression."
  (let ((buffer (current-buffer)))
    (setq mpd-start (save-excursion (beginning-of-line) (point)))
    (setq mpd-point-stack ())
    (with-temp-buffer
      (insert-buffer-substring buffer) ; lose the text properties
      (psl-remove-comments)
      (goto-char (point-min))
      (let ((sexp
             (let ((max-lisp-eval-depth
                    (floor (* psl-stack-multiplier max-lisp-eval-depth))))
               (mpd-parse psl-tokens psl-token-funcs 'expr))))
        (mpd-skip-whitespace)
        (if (= (point) (point-max))
            sexp
          (error (format "%s:%d:%d: Encountered error while parsing"
                         (buffer-name buffer)
                         (line-number-at-pos mpd-best)
                         (save-excursion (goto-char mpd-best)
                                         (current-column)))))))))

(defun psl-eval-wrapper (sexp)
  "Evaluate a compiled ParselTongue program inside a wrapper that
handles errors properly."
  (condition-case err
      (psl-print (eval sexp))
    (wrong-number-of-arguments
     (error "Application failed with arity mismatch"))))

(defun psl-eval-buffer ()
  "Evaluate the ParselTongue program in the buffer."
  (interactive)
  (psl-eval-wrapper (psl-compile-to-elisp)))

(defun psl-show-elisp-compilation ()
  "Show the Emacs Lisp compilation in a buffer."
  (interactive)
  (pp-display-expression (psl-compile-to-elisp) "*Pp Eval Output*"))

(defun psl-batch-eval ()
  "Run a ParselTongue program from the command line. It would be
used like this:
    emacs -Q --batch -l psl-compile.el -f psl-batch-eval script.psl"
  (setq vc-handled-backends nil)        ; disable spurious messages
  (let ((filename (car (last command-line-args))))
    (with-temp-buffer
      (insert-file-contents-literally filename)
      (rename-buffer filename)
      (psl-eval-buffer)
      (psl-print "\n"))))

;;; ParselTongue environment

(defun psl-+ (&rest args)
  "Implement ParselTongue's + function."
  (cond ((null args) (error "Empty list for prim op"))
        ((not (memq nil (mapcar #'stringp args))) (apply #'concat args))
        ((not (memq nil (mapcar #'numberp args))) (apply #'+ args))
        (t (error "Bad arguments to +"))))

(defun psl-- (&rest args)
  "Implement ParselTongue's - function."
  (cond ((null args) (error "Empty list for prim op"))
        ((not (memq nil (mapcar #'numberp args)))
         (if (= 1 (length args)) (car args) (apply #'- args)))
        (t (error "Bad arguments to -"))))

(defun psl-== (a b)
  "Implement ParselTongue's == function."
  (labels ((pair< (a b) (string< (symbol-name (car a)) (symbol-name (car b))))
           (clean (a) (unless (null a)
                        (cons (car a) (clean (if (eq (caar a) (caadr a))
                                                 (cddr a)
                                               (cdr a)))))))
    (cond
     ((and (psl-object-p a) (psl-object-p b))
      (equal (clean (sort (cdr a) #'pair<)) (clean (sort (cdr b) #'pair<))))
     (t (equal a b)))))

(defun psl-< (&rest args)
  "Implement ParselTongue's < functions."
  (if (not (= 2 (length args)))
      (error "Bad primop")
    (destructuring-bind (a b) args
      (if (and (numberp a) (numberp b))
          (< a b)
        (error (format "Bad arguments for <:\n%s\n%s"
                       (psl-print-to-string a) (psl-print-to-string b)))))))

(defun psl-> (&rest args)
  "Implement ParselTongue's > functions."
  (if (not (= 2 (length args)))
      (error "Bad primop")
    (destructuring-bind (a b) args
      (if (and (numberp a) (numberp b))
          (> a b)
        (error (format "Bad arguments for >:\n%s\n%s"
                       (psl-print-to-string a) (psl-print-to-string b)))))))

(defun psl-object-p (o)
  "Return t if argument is ParselTongue object."
  (and (listp o) (eq (car o) 'object)))

(defun psl-print-to-string (o)
  "Print a ParselTongue value to a string."
  (cond ((eq o t)          "true")
        ((null o)          "false")
        ((functionp o)     "function")
        ((psl-object-p o)  "object")
        (t (format "%s" o))))

(defun psl-print (o &rest rest)
  "Implement ParselTongue's print function."
  (if rest
      (error "Bad primop")
    (princ (psl-print-to-string o) t))
  o)

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
    (if        "if" expr then else)
    (then      "then" expr)
    (else      "else" expr)
    (while     "while" expr expr)
    (for       "for" "(" expr ";" expr ";" expr ")" expr)

    ;; Identifiers
    (id      . "[a-zA-Z]+")
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
    (assign    [index= id] "=" expr)
    (assignm   [index= id] "[+-]=" expr)

    ;; Objects
    (pairs     pair [("," pairs) ""])
    (pair      id ":" expr)
    (object    "{" [pairs ""] "}")
    (index     [pexpr id] [by-id index-str])
    (index=    [pexpr id] [by-id index-str])
    (message   [pexpr id] "@" [index-str field] args)
    (by-id     "\\." field)
    (field   . id)
    (index-str "\\[" expr "\\]"))
  "The ParselTongue grammar.")

(defvar psl-token-funcs
  `((expr     . ,(lambda (token expr) expr))
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
    (pair     . ,(lambda (token pair) (cons (nth 0 pair) (nth 2 pair))))
    (pairs    . ,(lambda (token pairs)
                   (if (stringp (nth 1 pairs))
                       (list (car pairs))
                     (cons (car pairs) (cadadr pairs)))))
    (object   . ,(lambda (token obj)
                   (let ((fields (nth 1 obj)))
                     (if (stringp fields)
                         '(list (quote object))
                       `(list 'object ,@(psl-make-object fields))))))
    (if       . ,(lambda (token expr)
                   (destructuring-bind (if cond then else) expr
                     `(if ,cond ,then ,else))))
    (then     . ,(lambda (token expr) (cadr expr)))
    (else     . ,(lambda (token expr) (cadr expr)))
    (while    . ,(lambda (token expr)
                   (destructuring-bind (while cond body) expr
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
                      (if (symbolp lhs)
                          `(setq ,lhs ,expr)
                        (destructuring-bind (obj field) lhs
                          `(cons 'object (acons ,field ,expr (cdr ,obj))))))))
    (assignm   . ,(lambda (token assign)
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
    (+         . ,(lambda (token op) (psl--apply 'psl-+ (nth 1 op))))
    (-         . ,(lambda (token op) (psl--apply 'psl-- (nth 1 op))))
    (<         . ,(lambda (token op) (psl--apply 'psl-< (nth 1 op))))
    (>         . ,(lambda (token op) (psl--apply 'psl-> (nth 1 op))))
    (==        . ,(lambda (token op) (psl--apply 'psl-== (nth 1 op))))
    (print     . ,(lambda (token op) (psl--apply 'psl-print (nth 1 op))))
    (index     . ,(lambda (token index)
                    `(cdr (or (assq ,(nth 1 index) (cdr,(nth 0 index)))
                              (error "Field not found: %s" ,(nth 1 index))))))
    (index=    . ,(lambda (token index) index))
    (message   . ,(lambda (token msg)
                    (destructuring-bind (obj at f args) msg
                      `(funcall (cdr (assq ,f (cdr ,obj)))
                                ,@(psl--apply obj args)))))
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

(defun psl-make-object (fields)
  (if (null fields)
      ()
    (if (assq (caar fields) (cdr fields))
        '((error "Multiply-defined fields"))
      (cons `(cons (quote ,(caar fields)) ,(cdar fields))
            (psl-make-object (cdr fields))))))

;;; Parser functions

(defvar mpd-best 0
  "The furthest most point that parsing reached. This information
can be used to determine where parsing failed.")

(defvar mpd-start 0
  "Position of point in original source buffer. The purpose is
for auto-indentation.")

(defvar mpd-point-stack ()
  "The token stack that contains the point. This is used for
auto-indentation.")

(defvar mpd-token-stack ()
  "Stack of tokens at this point.")

(defun mpd-get-token-func (token funcs)
  "Get the manipulation function for the given token."
  (or (cdr (assq token funcs)) #'cons))

(defun mpd-parse (tokens &optional funcs pattern)
  "Return the next item in the current buffer."
  (setq mpd-best 0)
  (setq mpd-token-stack ())
  (if pattern
      (mpd-match pattern tokens funcs)
    (dolist (token tokens)
      (let ((result (mpd-match (car token) tokens funcs)))
        (if result (return result))))))

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
  (push token mpd-token-stack)
  (let* ((pattern (cdr (assq token tokens)))
         (match (mpd-match pattern tokens funcs)))
    (pop mpd-token-stack)
    (when match (funcall (mpd-get-token-func token funcs) token match))))

(defun mpd-match-or (vec tokens funcs)
  "Match at least one pattern in the vector."
  (dolist (option (mapcar 'identity vec))
    (let ((match (mpd-match option tokens funcs)))
      (when match (return match)))))

(defun mpd-skip-whitespace ()
  "Skip over all whitespace."
  (search-forward-regexp "[[:space:]]*"))

(defun mpd-match (pattern tokens &optional funcs)
  "Match the given pattern object of any type (toplevel)."
  (mpd-skip-whitespace)
  (let ((start (point))
        (result (etypecase pattern
                  (string (mpd-match-regex pattern tokens funcs))
                  (list   (mpd-match-list  pattern tokens funcs))
                  (symbol (mpd-match-token pattern tokens funcs))
                  (vector (mpd-match-or    pattern tokens funcs)))))
    (when (and (not mpd-point-stack)
               (> mpd-start start)
               (> (point) mpd-start))
      (setq mpd-point-stack (reverse mpd-token-stack)))
    (unless result
      (setq mpd-best (max mpd-best (point)))
      (goto-char start))
    result))

(provide 'psl-compile)

;;; psl-compile.el ends here
