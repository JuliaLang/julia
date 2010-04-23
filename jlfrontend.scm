(declare
 (standard-bindings)
 (extended-bindings)
 (not run-time-bindings)
 (not safe)
 (not interrupts-enabled)
)
(define *julia-interpreter* #f)
(include "utils.scm")
(include "match.scm")
(include "julia-parser.scm")
(include "julia-syntax.scm")

(define (parser-wrap thk)
  (with-exception-catcher
   (lambda (e)
     (if (error-exception? e)
	 (let ((msg (error-exception-message e)))
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

(gambit-only
 (c-define (jl-parse-string s) (char-string) scheme-object
   "jl_scm_parse_string" ""
   (parser-wrap (lambda () (toplevel-expr (julia-parse s)))))
 
 (c-define (jl-parse-file s) (char-string) scheme-object
   "jl_scm_parse_file" ""
    (parser-wrap (lambda ()
		   (cons 'file (map toplevel-expr (julia-parse-file s))))))
 
 (c-define (get-integer n) (scheme-object) unsigned-int64 "jl_scm_uint64" ""
   n)
 (c-define (get-float64 x) (scheme-object) float64 "jl_scm_float64" ""
   x)
 
 (c-define (is-integer x) (scheme-object) int "jl_scm_integerp" ""
   (if (not (flonum? x)) 1 0))
 
 )
