(load "flisp/aliases.scm")
(load "utils.scm")
(load "match.scm")
(load "julia-parser.scm")
(load "julia-syntax.scm")

(define (parser-wrap thk)
  (with-exception-catcher
   (lambda (e)
     (if (and (pair? e) (eq? (car e) 'error))
	 (let ((msg (cadr e))
	       (pfx "incomplete:"))
	   (if (and (>= (string-length msg) (string-length pfx))
		    (equal? pfx
			    (substring msg 0 (string-length pfx))))
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
  (let ((preparsed-fname (string (string.sub s 0 (- (length s) 2))
				 ".jp")))
    (let ((ppmt  (file-mod-time preparsed-fname))
	  (srcmt (file-mod-time s)))
      (if (and (equal? (string.sub s (- (length s) 2) (length s)) ".j")
	       ppmt srcmt
	       (> ppmt srcmt))
	  (let ((fl (open-input-file preparsed-fname)))
	    (begin0 (read fl) (io.close fl)))
	  (let* ((infile (open-input-file s))
		 (ast
		  (parser-wrap (lambda ()
				 (cons 'file (map toplevel-expr
						  (julia-parse-file
						   s infile)))))))
	    (io.close infile)
	    (with-bindings
	     ((*print-pretty* #f))
	     (let ((outfl (file preparsed-fname :write :create :truncate)))
	       (write ast outfl)
	       (io.close outfl)))
	    ast)))))

; expand a piece of raw surface syntax to an executable thunk
(define (jl-expand-to-thunk expr)
  (parser-wrap (lambda ()
		 (toplevel-expr expr))))

;(load "profile.scm")
