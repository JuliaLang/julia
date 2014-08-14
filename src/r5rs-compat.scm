;; definitions for running the front-end code under R5RS

;; library imports for Chicken
(use srfi-1)
(use srfi-13)
(use srfi-12)
(use srfi-69)

(require-library bindings)
(import-for-syntax (only bindings macro-rules)
                   (only macro-helpers once-only))
(import bindings bindings macro-helpers)


(define (to-string x)
  (cond ((string? x) x)
	((char? x) (string x))
	((symbol? x) (symbol->string x))
	(else
	 (with-output-to-string (lambda () (display x))))))

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

(define-macro (begin0 expr . body)
  (let ((v (gensym)))
    `(let ((,v ,expr))
       ,@body
       ,v)))

(define-macro (unwind-protect expr finally)
  (let ((e   (gensym))
	(thk (gensym)))
    `(let ((,thk (lambda () ,finally)))
       (begin0 (with-exception-handler
		(lambda (,e) (begin (,thk) (raise ,e)))
		(lambda () ,expr))
	       (,thk)))))

(define-macro (with-bindings binds . body)
  (let ((vars (map car binds))
	(vals (map cadr binds))
	(olds (map (lambda (x) (gensym)) binds)))
    `(let ,(map list olds vars)
       ,@(map (lambda (v val) `(set! ,v ,val)) vars vals)
       (unwind-protect
	(begin ,@body)
	(begin ,@(map (lambda (v old) `(set! ,v ,old)) vars olds))))))

(define-macro (receive formals expr . body)
  `(call-with-values (lambda () ,expr)
     (lambda ,formals ,@body)))

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

(load "utils.scm")
(load "match.scm")
(load "julia-parser.scm")
#;(load "julia-syntax.scm")
