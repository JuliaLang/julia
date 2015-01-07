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
	   (if (and (string? msg) (>= (string-length msg) (string-length pfx))
		    (equal? pfx
			    (substring msg 0 (string-length pfx))))
	       `(incomplete ,msg)
	       e))
	 (begin
	   (newline)
	   (display "unexpected error: ")
	   (prn e)
	   (print-stack-trace (stacktrace))
	   '(error "malformed expression"))))
   thk))

;; assigned variables except those marked local or inside inner functions
(define (find-possible-globals- e tab)
  (cond ((atom? e)   tab)
	((quoted? e) tab)
	(else (case (car e)
		((=)            (put! tab (decl-var (cadr e)) #t))
		((method)       (let ((n (method-expr-name e)))
				  (if (symbol? n)
				      (put! tab n #t)
				      tab)))
		((lambda)       tab)
		((local local!) tab)
		((break-block)  (find-possible-globals- (caddr e) tab))
		(else
		 (for-each (lambda (x) (find-possible-globals- x tab))
			   (cdr e))
		 tab)))))

(define (find-possible-globals e)
  (table.keys (find-possible-globals- e (table))))

;; this is overwritten when we run in actual julia
(define (defined-julia-global v) #f)

(define (some-gensym? x)
  (or (gensym? x) (memq x *gensyms*)))

;; find variables that should be forced to be global in a toplevel expr
(define (toplevel-expr-globals e)
  (diff
   (delete-duplicates
    (append
     ;; vars assigned at the outer level
     (filter (lambda (x) (not (some-gensym? x))) (find-assigned-vars e '()))
     ;; vars declared const outside any scope block
     (find-decls 'const e '())
     ;; vars assigned anywhere, if they have been defined as global
     (filter defined-julia-global (find-possible-globals e))))
   (append
    (find-decls 'local  e '())
    (find-decls 'local! e '()))))

;; return a lambda expression representing a thunk for a top-level expression
;; note: expansion of stuff inside module is delayed, so the contents obey
;; toplevel expansion order (don't expand until stuff before is evaluated).
(define (expand-toplevel-expr-- e)
  (cond ((or (boolean? e) (eof-object? e)
	     ;; special top-level expressions left alone
	     (and (pair? e) (or (eq? (car e) 'line) (eq? (car e) 'module))))
	 e)
	((and (pair? e) (memq (car e) '(import importall using export)))
	 e)
	((and (pair? e) (eq? (car e) 'global) (every symbol? (cdr e)))
	 e)
	(else
	 (let ((ex0 (julia-expand-macros e)))
	   (if (and (pair? ex0) (eq? (car ex0) 'toplevel))
	       `(toplevel ,@(map expand-toplevel-expr (cdr ex0)))
	       (let* ((ex (julia-expand01 ex0))
		      (gv (toplevel-expr-globals ex))
		      (th (julia-expand1
			   `(lambda ()
			      (scope-block
			       (block ,@(map (lambda (v) `(global ,v)) gv)
				      ,ex))))))
		 (if (null? (car (caddr th)))
		     ;; if no locals, return just body of function
		     (cadddr th)
		     `(thunk ,th))))))))

;; (body (= v _) (return v)) => (= v _)
(define (simple-assignment? e)
  (and (length= e 3) (eq? (car e) 'body)
       (pair? (cadr e)) (eq? (caadr e) '=) (symbol? (cadadr e))
       (eq? (cadr (caddr e)) (cadadr e))))

(define (lambda-ex? e)
  (and (pair? e) (eq? (car e) 'lambda)))

(define (expand-toplevel-expr- e)
  (let ((ex (expand-toplevel-expr-- e)))
    (cond ((contains (lambda (x) (equal? x '(top ccall))) ex) ex)
          ((simple-assignment? ex)  (cadr ex))
	  ((and (length= ex 2) (eq? (car ex) 'body)
		(not (lambda-ex? (cadadr ex))))
	   ;; (body (return x)) => x
	   ;; if x is not a lambda expr, so we don't think it is a thunk
	   ;; to be called immediately.
	   (cadadr ex))
	  (else ex))))

(define *in-expand* #f)

(define (expand-toplevel-expr e)
  (if (and (pair? e) (eq? (car e) 'toplevel))
      ;;`(toplevel ,@(map expand-toplevel-expr (cdr e)))
      ;; delay expansion so defined global variables take effect for later
      ;; toplevel expressions.
      e
      (let ((last *in-expand*))
	(if (not last)
	    (begin (reset-gensyms)
		   (set! *in-expand* #t)))
	(let ((ex (expand-toplevel-expr- e)))
	  (set! *in-expand* last)
	  ex))))

;; parse only, returning end position, no expansion.
(define (jl-parse-one-string s pos0 greedy)
  (let ((inp (open-input-string s)))
    (io.seek inp pos0)
    (let ((expr
	   (parser-wrap (lambda ()
			  (if greedy
			      (julia-parse inp)
			      (julia-parse inp parse-atom))))))
      (cons expr (io.pos inp)))))

(define (jl-parse-string s)
  (parser-wrap (lambda ()
		 (let ((inp  (make-token-stream (open-input-string s))))
		   ;; parse all exprs into a (toplevel ...) form
		   (let loop ((exprs '()))
		     ;; delay expansion so macros run in the Task executing
		     ;; the input, not the task parsing it (issue #2378)
		     ;; used to be (expand-toplevel-expr expr)
		     (let ((expr (julia-parse inp)))
		       (if (eof-object? expr)
			   (cond ((null? exprs)     expr)
				 ((length= exprs 1) (car exprs))
				 (else (cons 'toplevel (reverse! exprs))))
			   (if (and (pair? expr) (eq? (car expr) 'toplevel))
			       (loop (nreconc (cdr expr) exprs))
			       (loop (cons expr exprs))))))))))

;; parse file-in-a-string
(define (jl-parse-string-stream str filename)
  (jl-parser-set-stream filename (open-input-string str)))

(define (jl-parse-file s)
  (trycatch
   (begin (jl-parser-set-stream s (open-input-file s))
	  #t)
   (lambda (e) #f)))

(define *filename-stack* '())
(define *ts-stack* '())
(define current-token-stream #())

(define (jl-parser-set-stream name stream)
  (set! *filename-stack* (cons current-filename *filename-stack*))
  (set! *ts-stack* (cons current-token-stream *ts-stack*))
  (set! current-filename (symbol name))
  (set! current-token-stream (make-token-stream stream)))

(define (jl-parser-close-stream)
  (io.close (ts:port current-token-stream))
  (set! current-filename (car *filename-stack*))
  (set! current-token-stream (car *ts-stack*))
  (set! *filename-stack* (cdr *filename-stack*))
  (set! *ts-stack* (cdr *ts-stack*)))

(define *depwarn* #t)
(define (jl-parser-depwarn w)
  (let ((prev *depwarn*))
    (set! *depwarn* (eq? w #t))
    prev))

(define (jl-parser-next)
  (let* ((err (parser-wrap
	       (lambda ()
		 (skip-ws-and-comments (ts:port current-token-stream)))))
	 (lineno (input-port-line (ts:port current-token-stream))))
    (cons lineno
	  (if (pair? err)
	      err
	      (parser-wrap
	       (lambda ()
		 (let ((e (julia-parse current-token-stream)))
		   (if (eof-object? e)
		       e
		       (if (and (pair? e) (or (eq? (car e) 'error)
					      (eq? (car e) 'continue)))
			   e
			   (expand-toplevel-expr e))))))))))

(define (jl-parser-current-lineno)
  (input-port-line (ts:port current-token-stream)))

; expand a piece of raw surface syntax to an executable thunk
(define (jl-expand-to-thunk expr)
  (parser-wrap (lambda ()
		 (expand-toplevel-expr expr))))

; macroexpand only
(define (jl-macroexpand expr)
  (reset-gensyms)
  (parser-wrap (lambda ()
		 (julia-expand-macros expr))))

; run whole frontend on a string. useful for testing.
(define (fe str)
  (expand-toplevel-expr (julia-parse str)))
