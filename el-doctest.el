;;; el-doctest.el --- Run tests in Elisp docstring  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Xu Chunyang

;; Author: Xu Chunyang
;; Homepage: https://github.com/xuchunyang/el-doctest
;; Package-Requires: ((emacs "25"))
;; Keywords: lisp, tools
;; Version: 0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Run tests in Elisp docstring

;;; Code:

(require 'cl-lib)
(require 'loadhist)

;; NOT used in code, for testing
(defun el-doctest--length (lst)
  "Return the length of list LST.

  (el-doctest--length '())
  ;; => 0

  (el-doctest--length '(a))
  ;; => 1

  (el-doctest--length '(a b))
  ;; => 2
"
  (pcase (length lst)
    (1 456)
    (x x)))

(defun el-doctest--eval (expr)
  (let* ((err nil)
         (got (condition-case e
                  (eval expr 'lexical)
                (error (setq err e)))))
    (list err got)))

(defun el-doctest--function-tests (function)
  "Return tests in FUNCTION's docstring."
  (let* ((docstring (documentation function 'raw))
         (lines (and docstring (split-string docstring "\n" t)))
         (i 0)
         tests)
    (while (< i (1- (length lines)))
      (let ((line-1 (nth i lines))
            (line-2 (nth (1+ i) lines)))
        (cond
         ((and (string-match (rx bol (* space) "(") line-1)
               (string-match (rx bol (* space) ";; => ") line-2))
          (push (cons
                 ;; XXX handle read error?
                 (read line-1)
                 (read
                  (substring line-2 (length (match-string 0 line-2)))))
                tests)
          (cl-incf i 2))
         (t
          (cl-incf i 1)))))
    (nreverse tests)))

(defun el-doctest--feature-functions (feature)
  (let (funcs)
    (dolist (entry (feature-symbols feature))
      (pcase entry
        (`(defun . ,func)
         (push func funcs))))
    (nreverse funcs)))

(defun el-doctest--feature-tests (feature)
  (cl-mapcan #'el-doctest--function-tests
             (el-doctest--feature-functions feature)))

;; XXX batch
;; XXX better report
(defun el-doctest-check-feature (feature)
  (interactive (list (read-feature "Check feature: ")))
  (let* ((tests (el-doctest--feature-tests feature))
         (total (length tests))
         passes)
    (cl-loop for test in tests
             for i from 1
             for progress = (format "[%d/%d]" i total)
             do
             (pcase test
               (`(,expr . ,want)
                (pcase (el-doctest--eval expr)
                  (`(nil ,got)
                   (cond
                    ((equal got want)
                     (push test passes)
                     (message "%s %s pass" progress (car expr)))
                    (t
                     (let ((print-quoted t))
                       (message "%s %s got: %s want: %s" progress expr got want)))))
                  (`(,err ,_)
                   (let ((print-quoted t))
                     (message "%s %s got error: %s"
                              progress
                              expr (error-message-string err))))))))
    (message "Total: %d, Pass: %d, Fail: %d"
             total
             (length passes)
             (- total (length passes)))))

(provide 'el-doctest)
;;; el-doctest.el ends here
