# el-doctest

Run tests in Elisp docstring.

## Example

For example, with this Emacs Lisp file, `M-x el-doctest-check-feature foo` will check if `(foo-add 1 2)` equals to `3`.

```emacs-lisp
;;; foo.el --- Foo  -*- lexical-binding: t; -*-

(defun foo-add (x y)
  "Add X to Y.

  (foo-add 1 2)
  ;; => 3"
  (+ x y))

(provide 'foo)
```

## How to write test?

Use this format:

    (func arg1 arg2 ...)
    ;; => RESULT

The test is considered if two adjacent lines matches the regexp:

``` emacs-lisp
;; first line matches
(rx bol (* space) "(")
;; second line matches
(rx bol (* space) ";; => ")
```

Unfortunately, we can't (or shouldn't) put `(` at the beginning of a line (see
the following quote), and because I don't like `\(`, el-doctest.el recognizes
whitespaces before `(`, not `\`.

> If a line in a documentation string begins with an
> open-parenthesis, write a backslash before the open-parenthesis,
> like this:
>
>      The argument FOO can be either a number
>      \(a buffer position) or a string (a file name).
>
> This prevents the open-parenthesis from being treated as the start
> of a defun (*note Defuns: (emacs)Defuns.).
>
>
> [(elisp) Documentation Tips](https://www.gnu.org/software/emacs/manual/html_node/elisp/Documentation-Tips.html)
