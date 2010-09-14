(load "jlfrontend.scm")

(with-bindings
 ((*print-pretty* #f))
 (print
  (parser-wrap (lambda ()
		 (cons 'file
		       (map toplevel-expr
			    (julia-parse-file "stdin" *input-stream*)))))))
(newline)
