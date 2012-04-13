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
;; note: expansion of stuff inside module is delayed, so the contents obey
;; toplevel expansion order (don't expand until stuff before is evaluated).
(define (expand-toplevel-expr- e)
  (if (or (boolean? e) (eof-object? e)
	  (and (pair? e) (or (eq? (car e) 'line) (eq? (car e) 'module))))
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
		 (let* ((inp  (make-token-stream (open-input-string s)))
			(expr (julia-parse inp)))
		   (if (not (eof-object? (julia-parse inp)))
		       (error "extra input after end of expression")
		       (expand-toplevel-expr expr))))))

;; parse file-in-a-string
(define (jl-parse-string-stream str)
  (jl-parser-set-stream "string" (open-input-string str)))

(define (jl-parse-file s)
  (jl-parser-set-stream s (open-input-file s)))

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

(define (jl-parser-next)
  (parser-wrap
   (lambda ()
     (with-exception-catcher
      (lambda (e)
	(if (and (pair? e) (eq? (car e) 'error))
	    (let ((msg (cadr e)))
	      (raise `(error ,(string msg " at " current-filename ":" 
				      (input-port-line
				       (ts:port current-token-stream))))))
	    (raise e)))
      (lambda ()
	(skip-ws-and-comments (ts:port current-token-stream))
	(let ((ln (input-port-line (ts:port current-token-stream))))
	  (let ((e (if #f #;(eq? (peek-token current-token-stream) 'end)
		       (take-token current-token-stream)
		       (julia-parse current-token-stream))))
	    (if (eof-object? e)
		#f
		(cons ln (expand-toplevel-expr e))))))))))

; expand a piece of raw surface syntax to an executable thunk
(define (jl-expand-to-thunk expr)
  (parser-wrap (lambda ()
		 (expand-toplevel-expr expr))))

; run whole frontend on a string. useful for testing.
(define (fe str)
  (expand-toplevel-expr (julia-parse str)))
