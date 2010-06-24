(load "flisp/aliases.scm")
(load "utils.scm")
(load "match.scm")
(load "julia-parser.scm")
(load "julia-syntax.scm")

(define (parser-wrap thk)
  (with-exception-catcher
   (lambda (e)
     (if (and (pair? e) (eq? (car e) 'error))
	 (let ((msg (cadr e)))
	   (if (equal? "incomplete:"
	               (substring msg 0 (string-length "incomplete:")))
	       `(continue)
	       `(error ,msg)))
	 (begin
	   (newline)
	   (display "unexpected error: ")
	   (prn e)
	   (print-stack-trace (stacktrace))
	   #f)))
   thk))

; return a lambda expression representing a thunk for a top-level expression
(define (toplevel-expr e)
  (if (or (boolean? e) (eof-object? e) (and (pair? e) (eq? (car e) 'line)))
      e
      (cadr (julia-expand `(lambda () ,e)))))

(define (jl-parse-string s)
  (parser-wrap (lambda ()
		 (toplevel-expr (julia-parse s)))))

(define (jl-parse-file s)
  (parser-wrap (lambda ()
		 (cons 'file (map toplevel-expr (julia-parse-file s))))))

; expand a piece of raw surface syntax to an executable thunk
(define (jl-expand-to-thunk expr)
  (parser-wrap (lambda ()
		 (toplevel-expr expr))))

;(load "profile.scm")

(make-system-image "julia_flisp.boot")
