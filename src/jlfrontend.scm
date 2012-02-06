(load "./flisp/aliases.scm")
(load "utils.scm")
(load "match.scm")
(load "julia-parser.scm")
(load "julia-syntax.scm")

;; exception handler for parser. turns known errors into special expressions,
;; and prevents throwing an exception past a C caller.
(define (parser-wrap thk)
  (with-exception-catcher
   (lambda (e)
     (if (and (pair? e) (eq? (car e) 'error))
	 (let ((msg (cadr e))
	       (pfx "incomplete:"))
	   (if (and (>= (string-length msg) (string-length pfx))
		    (equal? pfx
			    (substring msg 0 (string-length pfx))))
	       `(continue ,msg)
	       `(error ,msg)))
	 (begin
	   (newline)
	   (display "unexpected error: ")
	   (prn e)
	   (print-stack-trace (stacktrace))
	   #f)))
   thk))

;; assigned variables except those marked local or inside inner functions
(define (find-possible-globals e)
  (cond ((atom? e)   '())
	((quoted? e) '())
	(else (case (car e)
		((=)            (list (decl-var (cadr e))))
		((lambda)       '())
		((local local!) '())
		((break-block)  (find-possible-globals (caddr e)))
		(else
		 (delete-duplicates
		  (apply append!
			 (map find-possible-globals (cdr e)))))))))

;; this is overwritten when we run in actual julia
(define (defined-julia-global v) #f)

;; find variables that should be forced to be global in a toplevel expr
(define (toplevel-expr-globals e)
  (delete-duplicates
   (append
    ;; vars assigned at the outer level
    (filter (lambda (x) (not (gensym? x))) (find-assigned-vars e '()))
    ;; vars assigned anywhere, if they have been defined as global
    (filter defined-julia-global (find-possible-globals e)))))

;; return a lambda expression representing a thunk for a top-level expression
(define (expand-toplevel-expr- e)
  (if (or (boolean? e) (eof-object? e) (and (pair? e) (eq? (car e) 'line)))
      e
      (let* ((ex (julia-expand0 e))
	     (gv (toplevel-expr-globals ex))
	     (th (julia-expand1
		  `(lambda ()
		     (scope-block
		      (block ,@(map (lambda (v) `(global ,v)) gv)
			     ,ex))))))
	(if (null? (car (caddr th)))
	    ;; if no locals, return just body of function
	    (cadddr th)
	    `(thunk ,th)))))

;; (body (= v _) (return v)) => (= v _)
(define (simple-assignment? e)
  (and (length= e 3) (eq? (car e) 'body)
       (pair? (cadr e)) (eq? (caadr e) '=) (symbol? (cadadr e))
       (eq? (cadr (caddr e)) (cadadr e))))

(define (lambda-ex? e)
  (and (pair? e) (eq? (car e) 'lambda)))

(define (expand-toplevel-expr e)
  (let ((ex (expand-toplevel-expr- e)))
    (cond ((simple-assignment? ex)  (cadr ex))
	  ((and (length= ex 2) (eq? (car ex) 'body)
		(not (lambda-ex? (cadadr ex))))
	   ;; (body (return x)) => x
	   ;; if x is not a lambda expr, so we don't think it is a thunk
	   ;; to be called immediately.
	   (cadadr ex))
	  (else ex))))

(define (has-macrocalls? e)
  (or (and (pair? e) (eq? (car e) 'macrocall))
      (and (not (and (pair? e) (eq? (car e) 'quote)))
	   (any has-macrocalls? e))))

;; expand expression right after parsing if it's OK to do so
(define (pre-expand-toplevel-expr e)
  (if (or (and (pair? e) (eq? (car e) 'module))
	  (has-macrocalls? e))
      e
      (expand-toplevel-expr e)))

(define (jl-parse-one-string s pos0 greedy)
  (set! current-filename 'string)
  (let ((inp (open-input-string s)))
    (io.seek inp pos0)
    (let ((expr
	   (parser-wrap (lambda ()
			  (if greedy
			      (julia-parse inp)
			      (julia-parse inp parse-atom))))))
      (cons expr (io.pos inp)))))

(define (jl-parse-string s)
  (set! current-filename 'prompt)
  (parser-wrap (lambda ()
		 (let* ((inp  (make-token-stream (open-input-string s)))
			(expr (julia-parse inp)))
		   (if (not (eof-object? (julia-parse inp)))
		       (error "extra input after end of expression")
		       (pre-expand-toplevel-expr expr))))))

(define (jl-parse-named-stream name stream)
  (parser-wrap (lambda ()
		 (cons 'file (map pre-expand-toplevel-expr
				  (julia-parse-stream name stream))))))

;; parse file-in-a-string
(define (jl-parse-string-stream str)
  (jl-parse-named-stream "string" (open-input-string str)))

(define (jl-parse-file s)
  (let ((infile (open-input-file s)))
    (begin0
     (jl-parse-named-stream s infile)
     (io.close infile))))

; expand a piece of raw surface syntax to an executable thunk
(define (jl-expand-to-thunk expr)
  (parser-wrap (lambda ()
		 (expand-toplevel-expr expr))))

; run whole frontend on a string. useful for testing.
(define (fe str)
  (expand-toplevel-expr (julia-parse str)))
