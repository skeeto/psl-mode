# ParselTongue mode (`psl-mode`)

It looks like this,

![](http://i.imgur.com/BXXE8.png)

Automatic indentation is supported by a full language parser. If
`psl-program-name` is setup the current buffer can be executed by it
with `psl-run-buffer` (`C-c C-r`).

## ParselTongue Compiler

Also included with psl-mode is a ParselTongue compiler which compiles
a ParselTongue program into Elisp. To compile and evaluate a
ParselTongue program *inside*of Emacs (no spawning other processes),
use `psl-eval-buffer` (`C-c C-e`). This compiler can be accessed
standalone with the provided `psl` script.

### References

 * [ParselTongue specification](http://www.cs.brown.edu/courses/cs173/2012/Assignments/ParselTest/spec.html)
