(load "flisp/aliases.scm")
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
	       `(continue)
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

; return a lambda expression representing a thunk for a top-level expression
(define (expand-toplevel-expr e)
  (if (or (boolean? e) (eof-object? e) (and (pair? e) (eq? (car e) 'line)))
      e
      (let* ((ex (julia-expand0 e))
	     (gv (toplevel-expr-globals ex))
	     (th (julia-expand1
		  `(lambda ()
		     (scope-block
		      (block ,@(map (lambda (v) `(global ,v)) gv)
			     ,ex))))))
	(if (null? (cdr (cadr (caddr th))))
	    ;; if no locals, return just body of function
	    (let ((body (cadddr th)))
	      body)
	    th))))

(define (jl-just-parse-string s pos0 greedy)
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
		 (expand-toplevel-expr (julia-parse s)))))

(define (has-macrocalls? e)
  (or (and (pair? e) (eq? (car e) 'macrocall))
      (and (not (and (pair? e) (eq? (car e) 'quote)))
	   (any has-macrocalls? e))))

;; (body (= v _) (return v)) => (= v _)
(define (simple-assignment? e)
  (and (length= e 3) (eq? (car e) 'body)
       (pair? (cadr e)) (eq? (caadr e) '=) (symbol? (cadadr e))
       (eq? (cadr (caddr e)) (cadadr e))))

(define (lambda-ex? e)
  (and (pair? e) (eq? (car e) 'lambda)))

;; in a file, we want to expand in advance as many expressions as possible.
;; we can't do this for expressions with macro calls, because the needed
;; macros might not have been defined yet (since that is done by the
;; evaluator). or, in the case of saving pre-lowered code, macros might be
;; defined in other files, creating implicit dependencies.
(define (file-toplevel-expr e)
  (if (has-macrocalls? e)
      `(unexpanded ,e)
      (let ((ex (expand-toplevel-expr e)))
	(cond ((simple-assignment? ex)  (cadr ex))
	      ((and (length= ex 2) (eq? (car ex) 'body)
		    (not (lambda-ex? (cadadr ex))))
	       (cadadr ex))
	      (else ex)))))

(define (jl-parse-named-stream name stream)
  (parser-wrap (lambda ()
		 (cons 'file (map file-toplevel-expr
				  (julia-parse-file name stream))))))

(define (jl-parse-source-string str)
  (jl-parse-named-stream "string" (open-input-string str)))

(define (jl-parse-source s)
  (let ((infile (open-input-file s)))
    (begin0
     (jl-parse-named-stream s infile)
     (io.close infile))))

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
	  (let ((ast (jl-parse-source s)))
	    (with-bindings
	     ((*print-pretty* #f))
	     (let ((outfl (file preparsed-fname :write :create :truncate)))
	       (write ast outfl)
	       (io.close outfl)))
	    ast)))))

; expand a piece of raw surface syntax to an executable thunk
(define (jl-expand-to-thunk expr)
  (parser-wrap (lambda ()
		 (expand-toplevel-expr expr))))

; run whole frontend on a string. useful for testing.
(define (fe str)
  (expand-toplevel-expr (julia-parse str)))

;(load "profile.scm")
