(define *julia-interpreter* #f)
(include "utils.scm")
(include "match.scm")
(include "julia-parser.scm")
(include "julia-syntax.scm")

(define (parser-wrap thk)
  (with-exception-catcher
   (lambda (e)
     (if (error-exception? e)
	 `(error ,(error-exception-message e))
	 #f))
   thk))

; return a lambda expression representing a thunk for a top-level expression
(define (toplevel-expr e)
  (if (or (boolean? e) (eof-object? e))
      e
      (caddr (cadr (julia-expand `(lambda () ,e))))))

; make symbol names easier to access from C by converting to strings
(define (unsymbol e)
  (cond ((symbol? e) (symbol->string e))
	((string? e) `(string ,e))
	((atom? e) e)
	(else (map unsymbol e))))

(gambit-only
 (c-define (jl-parse-string s) (char-string) scheme-object
   "jl_scm_parse_string" ""
   (unsymbol (parser-wrap (lambda () (toplevel-expr (julia-parse s))))))
 
 (c-define (jl-parse-file s) (char-string) scheme-object
   "jl_scm_parse_file" ""
   (unsymbol
    (parser-wrap (lambda ()
		   (cons 'file (map toplevel-expr (julia-parse-file s)))))))
 
 (c-define (get-sym s) (char-string) scheme-object "jl_get_sym" ""
   (string->symbol s))
 
 (c-define (get-string s) (scheme-object) char-string "jl_scm_str" ""
   (begin
     (if (not (string? s)) (error "jl_scm_str: string expected"))
     s))
 (c-define (get-integer n) (scheme-object) unsigned-int64 "jl_scm_uint64" ""
   n)
 (c-define (get-float64 x) (scheme-object) float64 "jl_scm_float64" ""
   x)
 
 (c-define (is-integer x) (scheme-object) int "jl_scm_integerp" ""
   (if (not (flonum? x)) 1 0))
 
 )
