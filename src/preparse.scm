(load "jlfrontend.scm")

(with-bindings
 ((*print-pretty* #f))
 (print
  (jl-parse-file (cadr *argv*))))

(newline)
