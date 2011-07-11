(load "jlfrontend.scm")

(with-bindings
 ((*print-pretty* #f))
 (print
  (jl-parse-source (cadr *argv*))))

(newline)
