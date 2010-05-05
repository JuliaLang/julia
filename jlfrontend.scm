(define *julia-interpreter* #f)
(load "flisp/aliases.scm")
(load "utils.scm")
(load "match.scm")
(load "julia-parser.scm")
(load "julia-syntax.scm")

(define (parser-wrap thk)
  (with-exception-catcher
   (lambda (e)
     (prn e)
     (if (and (pair? e) (eq? (car e) 'error))
	 (let ((msg (cadr e)))
	   (if (equal? "incomplete:"
	               (substring msg 0 (string-length "incomplete:")))
	       `(continue)
	       `(error ,msg)))
	 #f))
   thk))

; return a lambda expression representing a thunk for a top-level expression
(define (toplevel-expr e)
  (if (or (boolean? e) (eof-object? e) (and (pair? e) (eq? (car e) 'line)))
      e
      (caddr (cadr (julia-expand `(lambda () ,e))))))

(define (jl-parse-string s)
  (parser-wrap (lambda ()
		 (toplevel-expr (julia-parse s)))))

(define (jl-parse-file s)
  (parser-wrap (lambda ()
		 (cons 'file (map toplevel-expr (julia-parse-file s))))))

(make-system-image "julia_flisp.boot")
