# el-doctest

Run tests in Elisp docstring.

For example, with this Emacs Lisp file, `M-x el-doctest-check-feature foo` will check if `(foo-add 1 2)` equals to `3`.

```emacs-lisp
;;; foo.el --- Foo  -*- lexical-binding: t; -*-

(defun foo-add (x y)
  "Add X to Y.

  (foo-add 1 2)
  ;; => 3
"
  (+ x y))

(provide 'foo)
```

The test is considered if two adjacent lines matches the regexp:

``` emacs-lisp
;; first line matches
(rx bol (* space) "(")
;; second line matches
(rx bol (* space) ";; => ")
```
