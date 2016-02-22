;;; julia-mode-tests.el --- Tests for julia-mode.el

;; Copyright (C) 2009-2014 Julia contributors
;; URL: https://github.com/JuliaLang/julia
;; Version: 0.3
;; Keywords: languages

;;; Usage:

;; From command line:
;;
;; emacs -batch -L . -l ert -l julia-mode-tests.el -f  ert-run-tests-batch-and-exit

;;; Commentary:
;; Contains ert tests for julia-mode.el

;;; License:
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Code:

;; We can't use cl-lib whilst supporting Emacs 23 users who don't use
;; ELPA.
(with-no-warnings
  (require 'cl)) ;; incf, decf, plusp

(require 'julia-mode)
(require 'ert)

(defmacro julia--should-indent (from to)
  "Assert that we indent text FROM producing text TO in `julia-mode'."
  `(with-temp-buffer
     (let ((julia-indent-offset 4))
       (julia-mode)
       (insert ,from)
       (indent-region (point-min) (point-max))
       (should (equal (buffer-substring-no-properties (point-min) (point-max))
                      ,to)))))


(ert-deftest julia--test-indent-if ()
  "We should indent inside if bodies."
  (julia--should-indent
     "
if foo
bar
end"
     "
if foo
    bar
end"))

(ert-deftest julia--test-indent-else ()
  "We should indent inside else bodies."
  (julia--should-indent
     "
if foo
    bar
else
baz
end"
     "
if foo
    bar
else
    baz
end"))

(ert-deftest julia--test-indent-toplevel ()
  "We should not indent toplevel expressions. "
  (julia--should-indent
     "
foo()
bar()"
     "
foo()
bar()"))

(ert-deftest julia--test-indent-nested-if ()
  "We should indent for each level of indentation."
  (julia--should-indent
     "
if foo
    if bar
bar
    end
end"
     "
if foo
    if bar
        bar
    end
end"))

(ert-deftest julia--test-indent-module-keyword ()
  "Module should not increase indentation at any level."
  (julia--should-indent
   "
module
begin
    a = 1
end
end"
   "
module
begin
    a = 1
end
end")
  (julia--should-indent
   "
begin
module
foo
end
end"
   "
begin
    module
    foo
    end
end"))

(ert-deftest julia--test-indent-function ()
  "We should indent function bodies."
  (julia--should-indent
     "
function foo()
bar
end"
     "
function foo()
    bar
end"))

(ert-deftest julia--test-indent-begin ()
  "We should indent after a begin keyword."
  (julia--should-indent
     "
@async begin
bar
end"
     "
@async begin
    bar
end"))

(ert-deftest julia--test-indent-paren ()
  "We should indent to line up with the text after an open paren."
  (julia--should-indent
     "
foobar(bar,
baz)"
     "
foobar(bar,
       baz)"))

(ert-deftest julia--test-indent-paren-space ()
  "We should indent to line up with the text after an open
paren, even if there are additional spaces."
  (julia--should-indent
     "
foobar( bar,
baz )"
     "
foobar( bar,
        baz )"))

(ert-deftest julia--test-indent-equals ()
  "We should increase indent on a trailing =."
  (julia--should-indent
     "
foo() =
bar"
     "
foo() =
    bar"))

(ert-deftest julia--test-indent-operator ()
  "We should increase indent after the first trailing operator
but not again after that."
  (julia--should-indent
   "
foo() |>
bar |>
baz
qux"
   "
foo() |>
    bar |>
    baz
qux"))

(ert-deftest julia--test-indent-ignores-blank-lines ()
  "Blank lines should not affect indentation of non-blank lines."
  (julia--should-indent
     "
if foo
        
bar
end"
     "
if foo
    
    bar
end"))

(ert-deftest julia--test-indent-comment-equal ()
  "`=` at the end of comment should not increase indent level."
  (julia--should-indent
     "
# a =
# b =
c"
     "
# a =
# b =
c"))

(ert-deftest julia--test-indent-leading-paren ()
  "`(` at the beginning of a line should not affect indentation."
  (julia--should-indent
     "
\(1)"
     "
\(1)"))

(ert-deftest julia--test-top-level-following-paren-indent ()
  "`At the top level, a previous line indented due to parens should not affect indentation."
  (julia--should-indent
     "y1 = f(x,
       z)
y2 = g(x)"
     "y1 = f(x,
       z)
y2 = g(x)"))

(ert-deftest julia--test-indentation-of-multi-line-strings ()
  "Indentation should only affect the first line of a multi-line string."
    (julia--should-indent
     "   a = \"\"\"
    description
begin
    foo
bar
end
\"\"\""
     "a = \"\"\"
    description
begin
    foo
bar
end
\"\"\""))

(defun julia--run-tests ()
  (interactive)
  (if (featurep 'ert)
      (ert-run-tests-interactively "julia--test")
    (message "Can't run julia-mode-tests because ert is not available.")))

(provide 'julia-mode-tests)
;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (not obsolete)
;; End:
;;; julia-mode-tests.el ends here
