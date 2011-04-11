(load "jlfrontend.scm")

(with-bindings
 ((*print-pretty* #f))
 (print
  (jl-parse-named-stream "stdin" *input-stream*)))

(newline)
