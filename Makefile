all :
	emacs -Q -batch -L . --eval '(setq generated-autoload-file "$(PWD)/psl-mode-autoloads.el")' \
			     --eval '(update-directory-autoloads ".")'
	emacs -Q -batch -L . --eval '(byte-recompile-directory "." 0)'
	$(RM) *~

clean :
	$(RM) *.elc *autoloads.el *~
