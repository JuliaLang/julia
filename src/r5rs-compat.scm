;; definitions for running the front-end code under R5RS

;; library imports for Chicken
(use srfi-1)
(use srfi-13)
(use srfi-12)
(use srfi-69)
(use srfi-60)

(define-syntax define-mac
  (syntax-rules ()
    [(define-mac (id expr) body ...)
     (define-syntax id (lambda (expr id-rename id=) body ...))]
    [(define-mac id xform)
     (define-mac (id expr) (xform expr))]))

(define (to-string x)
  (cond ((string? x) x)
	((char? x) (string x))
	((symbol? x) (symbol->string x))
	((number? x) (number->string x))
	(else
	 (call-with-output-string
	  (lambda (p) (display x p))))))

(define (str . xs)
  (apply string-append (map to-string xs)))

(define symbol string->symbol)

(define (length= lst n)
  (cond ((< n 0)     #f)
	((= n 0)     (atom? lst))
	((atom? lst) (= n 0))
	(else        (length= (cdr lst) (- n 1)))))

(define (length> lst n)
  (cond ((< n 0)     lst)
	((= n 0)     (and (pair? lst) lst))
	((atom? lst) (< n 0))
	(else        (length> (cdr lst) (- n 1)))))

(define (to-proper l)
  (cond ((null? l) l)
	((atom? l) (list l))
	(else (cons (car l) (to-proper (cdr l))))))

(define (compare-strs s1 s2)
  (if (string<? s1 s2)
      -1
      (if (string>? s1 s2)
	  1 0)))

(define (compare-nums x y)
  (if (< x y) -1 (if (> x y) 1 0)))

(define list* cons*)
(define nconc append!)

(define raise signal)

(define-mac (begin0 form) ;expr . body)
  (let ((v (gensym)))
    `(let ((,v ,(cadr form)))
       ,@(cddr form)
       ,v)))

(define-mac (unwind-protect form) ;expr finally)
  (let ((e   (gensym))
	(thk (gensym)))
    `(let ((,thk (lambda () ,(caddr form))))
       (begin0 (with-exception-handler
		(lambda (,e) (begin (,thk) (raise ,e)))
		(lambda () ,(cadr form)))
	       (,thk)))))

(define-mac (with-bindings form) ;binds . body)
  (let ((binds (cadr form))
	(body  (cddr form)))
    (let ((vars (map car binds))
	  (vals (map cadr binds))
	  (olds (map (lambda (x) (gensym)) binds)))
      `(let ,(map list olds vars)
	 ,@(map (lambda (v val) `(set! ,v ,val)) vars vals)
	 (unwind-protect
	  (begin ,@body)
	  (begin ,@(map (lambda (v old) `(set! ,v ,old)) vars olds)))))))

(define-mac (receive form) ;formals expr . body)
  (let ((formals (cadr form))
	(expr (caddr form))
	(body (cdddr form)))
    `(call-with-values (lambda () ,expr)
       (lambda ,formals ,@body))))

(define (separate pred lst)
  (let loop ((lst lst)  (yes '())  (no '()))
    (if (pair? lst)
	(if (pred (car lst))
	    (loop (cdr lst) (cons (car lst) yes) no)
	    (loop (cdr lst) yes (cons (car lst) no)))
	(values (reverse! yes) (reverse! no)))))

(define (skip-ws port newlines)
  (let loop ((c       (peek-char port))
	     (skipped #f))
    (if (and (char? c) (char-whitespace? c) (or newlines (not (newline? c))))
	(begin (read-char port)
	       (loop (peek-char port) #t))
	skipped)))

(define (identifier-start-char? c)
  (or (and (char>=? c #\a) (char<=? c #\z))
      (and (char>=? c #\A) (char<=? c #\Z))
      (eqv? c #\_)))

(define (identifier-char? c)
  (or (identifier-start-char? c)
      (and (char>=? c #\0) (char<=? c #\9))
      (eqv? c #\!)))

;; TODO handle "x!="
(define (accum-julia-symbol c port)
  (let loop ((sym '())
	     (c   c))
    (if (and (char? c) (identifier-char? c))
	(loop (cons (read-char port) sym) (peek-char port))
	(string->symbol (list->string (reverse! sym))))))

;; TODO
(define (input-port-line port) 0)

(define (eof-object) '#!eof)

(define (table . kv)
  (let ((h (make-hash-table)))
    (let loop ((kv kv))
      (if (null? kv)
	  h
	  (begin (hash-table-set! h (car kv) (cadr kv))
		 (loop (cddr kv)))))))

(define list-head take)

(define (nreconc l1 l2) (append! (reverse! l1) l2))

(define (jl-parse-file f)
  (let ((inp  (make-token-stream (open-input-file f))))
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
		(loop (cons expr exprs))))))))

(include "utils.scm")
(include "match.scm")
(include "julia-parser.scm")
(include "julia-syntax.scm")
