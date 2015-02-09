; -*- scheme -*-
; femtoLisp standard library
; by Jeff Bezanson (C) 2009
; Distributed under the BSD License

(define (void) #t)  ; the unspecified value

(define *builtins*
  (vector
   0 0 0 0 0 0 0 0 0 0 0 0
   (lambda (x y) (eq? x y))          (lambda (x y) (eqv? x y))
   (lambda (x y) (equal? x y))       (lambda (x) (atom? x))
   (lambda (x) (not x))              (lambda (x) (null? x))
   (lambda (x) (boolean? x))         (lambda (x) (symbol? x))
   (lambda (x) (number? x))          (lambda (x) (bound? x))
   (lambda (x) (pair? x))            (lambda (x) (builtin? x))
   (lambda (x) (vector? x))          (lambda (x) (fixnum? x))
   (lambda (x) (function? x))        (lambda (x y) (cons x y))
   (lambda rest (apply list rest))   (lambda (x) (car x))
   (lambda (x) (cdr x))              (lambda (x y) (set-car! x y))
   (lambda (x y) (set-cdr! x y))     (lambda rest (apply apply rest))
   (lambda rest (apply + rest))      (lambda rest (apply - rest))
   (lambda rest (apply * rest))      (lambda rest (apply / rest))
   (lambda rest (apply div0 rest))   (lambda (x y) (= x y))
   (lambda (x y) (< x y))            (lambda (x y) (compare x y))
   (lambda rest (apply vector rest)) (lambda (x y) (aref x y))
   (lambda (x y z) (aset! x y z))))

(if (not (bound? '*syntax-environment*))
    (define *syntax-environment* (table)))

(define (set-syntax! s v) (put! *syntax-environment* s v))
(define (symbol-syntax s) (get *syntax-environment* s #f))

(define-macro (define-macro form . body)
  `(set-syntax! ',(car form)
		(lambda ,(cdr form) ,@body)))

(define-macro (letrec binds . body)
  `((lambda ,(map car binds)
      ,.(map (lambda (b) `(set! ,@b)) binds)
      ,@body)
    ,.(map (lambda (x) (void)) binds)))

(define-macro (let binds . body)
  (let ((lname #f))
    (if (symbol? binds)
	(begin (set! lname binds)
	       (set! binds (car body))
	       (set! body (cdr body))))
    (let ((thelambda
	   `(lambda ,(map (lambda (c) (if (pair? c) (car c) c))
			  binds)
	      ,@body))
	  (theargs
	   (map (lambda (c) (if (pair? c) (cadr c) (void))) binds)))
      (cons (if lname
		`(letrec ((,lname ,thelambda)) ,lname)
		thelambda)
	    theargs))))

(define-macro (cond . clauses)
  (define (cond-clauses->if lst)
    (if (atom? lst)
	#f
	(let ((clause (car lst)))
	  (if (or (eq? (car clause) 'else)
		  (eq? (car clause) #t))
	      (if (null? (cdr clause))
		  (car clause)
		  (cons 'begin (cdr clause)))
	      (if (null? (cdr clause))
		  ; test by itself
		  (list 'or
			(car clause)
			(cond-clauses->if (cdr lst)))
		  ; test => expression
		  (if (eq? (cadr clause) '=>)
		      (if (1arg-lambda? (caddr clause))
			  ; test => (lambda (x) ...)
			  (let ((var (caadr (caddr clause))))
			    `(let ((,var ,(car clause)))
			       (if ,var ,(cons 'begin (cddr (caddr clause)))
				   ,(cond-clauses->if (cdr lst)))))
			  ; test => proc
			  (let ((b (gensym)))
			    `(let ((,b ,(car clause)))
			       (if ,b
				   (,(caddr clause) ,b)
				   ,(cond-clauses->if (cdr lst))))))
		      (list 'if
			    (car clause)
			    (cons 'begin (cdr clause))
			    (cond-clauses->if (cdr lst)))))))))
  (cond-clauses->if clauses))

; standard procedures ---------------------------------------------------------

(define (member item lst)
  (cond ((atom? lst) #f)
        ((equal?     (car lst) item) lst)
        (#t          (member item (cdr lst)))))
(define (memv item lst)
  (cond ((atom? lst) #f)
        ((eqv?       (car lst) item) lst)
        (#t          (memv item (cdr lst)))))

(define (assoc item lst)
  (cond ((atom? lst) #f)
	((equal?     (caar lst) item) (car lst))
	(#t          (assoc item (cdr lst)))))
(define (assv item lst)
  (cond ((atom? lst) #f)
	((eqv?       (caar lst) item) (car lst))
	(#t          (assv item (cdr lst)))))

(define (>  a b) (< b a))
(define (<= a b) (or (< a b) (= a b)))
(define (>= a b) (or (< b a) (= a b)))
(define (negative? x) (< x 0))
(define (zero? x)     (= x 0))
(define (positive? x) (> x 0))
(define (even? x) (= (logand x 1) 0))
(define (odd? x) (not (even? x)))
(define (identity x) x)
(define (1+ n) (+ n 1))
(define (1- n) (- n 1))
(define (mod0 x y) (- x (* (div0 x y) y)))
(define (div x y) (+ (div0 x y)
		     (or (and (< x 0)
			      (or (and (< y 0) 1)
				  -1))
			 0)))
(define (mod x y) (- x (* (div x y) y)))
(define (abs x)   (if (< x 0) (- x) x))
(define (max x0 . xs)
  (if (null? xs) x0
      (foldl (lambda (a b) (if (< a b) b a)) x0 xs)))
(define (min x0 . xs)
  (if (null? xs) x0
      (foldl (lambda (a b) (if (< a b) a b)) x0 xs)))
(define (char? x) (eq? (typeof x) 'wchar))
(define (array? x) (or (vector? x)
		       (let ((t (typeof x)))
			 (and (pair? t) (eq? (car t) 'array)))))
(define (closure? x) (and (function? x) (not (builtin? x))))

(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))
(define (caaar x) (car (car (car x))))
(define (caadr x) (car (car (cdr x))))
(define (cadar x) (car (cdr (car x))))
(define (caddr x) (car (cdr (cdr x))))
(define (cdaar x) (cdr (car (car x))))
(define (cdadr x) (cdr (car (cdr x))))
(define (cddar x) (cdr (cdr (car x))))
(define (cdddr x) (cdr (cdr (cdr x))))
(define (caaaar x) (car (car (car (car x)))))
(define (caaadr x) (car (car (car (cdr x)))))
(define (caadar x) (car (car (cdr (car x)))))
(define (caaddr x) (car (car (cdr (cdr x)))))
(define (cadaar x) (car (cdr (car (car x)))))
(define (cadadr x) (car (cdr (car (cdr x)))))
(define (caddar x) (car (cdr (cdr (car x)))))
(define (cadddr x) (car (cdr (cdr (cdr x)))))
(define (cdaaar x) (cdr (car (car (car x)))))
(define (cdaadr x) (cdr (car (car (cdr x)))))
(define (cdadar x) (cdr (car (cdr (car x)))))
(define (cdaddr x) (cdr (car (cdr (cdr x)))))
(define (cddaar x) (cdr (cdr (car (car x)))))
(define (cddadr x) (cdr (cdr (car (cdr x)))))
(define (cdddar x) (cdr (cdr (cdr (car x)))))
(define (cddddr x) (cdr (cdr (cdr (cdr x)))))

(let ((*values* (list '*values*)))
  (set! values
	(lambda vs
	  (if (and (pair? vs) (null? (cdr vs)))
	      (car vs)
	      (cons *values* vs))))
  (set! call-with-values
	(lambda (producer consumer)
	  (let ((res (producer)))
	    (if (and (pair? res) (eq? *values* (car res)))
		(apply consumer (cdr res))
		(consumer res))))))

; list utilities --------------------------------------------------------------

(define (every pred lst)
  (or (atom? lst)
      (and (pred (car lst))
           (every pred (cdr lst)))))

(define (any pred lst)
  (and (pair? lst)
       (or (pred (car lst))
           (any pred (cdr lst)))))

(define (list? a) (or (null? a) (and (pair? a) (list? (cdr a)))))

(define (list-tail lst n)
  (if (<= n 0) lst
      (list-tail (cdr lst) (- n 1))))

(define (list-head lst n)
  (if (<= n 0) ()
      (cons (car lst)
	    (list-head (cdr lst) (- n 1)))))

(define (list-ref lst n)
  (car (list-tail lst n)))

; bounded length test
; use this instead of (= (length lst) n), since it avoids unnecessary
; work and always terminates.
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

(define (last-pair l)
  (if (atom? (cdr l))
      l
      (last-pair (cdr l))))

(define (lastcdr l)
  (if (atom? l)
      l
      (cdr (last-pair l))))

(define (to-proper l)
  (cond ((null? l) l)
	((atom? l) (list l))
	(else (cons (car l) (to-proper (cdr l))))))

#;(define (map! f lst)
  (prog1 lst
	 (while (pair? lst)
		(set-car! lst (f (car lst)))
		(set! lst (cdr lst)))))

(define (filter pred lst)
  (define (filter- f lst acc)
    (cdr
     (prog1 acc
      (while (pair? lst)
	     (begin (if (pred (car lst))
			(set! acc
			      (cdr (set-cdr! acc (cons (car lst) ())))))
		    (set! lst (cdr lst)))))))
  (filter- pred lst (list ())))

(define (separate pred lst)
  (define (separate- pred lst yes no)
    (let ((vals
	   (prog1
	    (cons yes no)
	    (while (pair? lst)
		   (begin (if (pred (car lst))
			      (set! yes
				    (cdr (set-cdr! yes (cons (car lst) ()))))
			      (set! no
				    (cdr (set-cdr! no  (cons (car lst) ())))))
			  (set! lst (cdr lst)))))))
      (values (cdr (car vals)) (cdr (cdr vals)))))
  (separate- pred lst (list ()) (list ())))

(define (count f l)
  (define (count- f l n)
    (if (null? l)
	n
	(count- f (cdr l) (if (f (car l))
			      (+ n 1)
			      n))))
  (count- f l 0))

#;(define (foldr f zero lst)
  (if (null? lst) zero
      (f (car lst) (foldr f zero (cdr lst)))))

(define (foldl f zero lst)
  (if (null? lst) zero
      (foldl f (f (car lst) zero) (cdr lst))))

(define (reverse- zero lst)
  (if (null? lst) zero
      (reverse- (cons (car lst) zero) (cdr lst))))

(define (reverse lst) (reverse- () lst))

(define (reverse!- prev l)
  (while (pair? l)
	 (set! l (prog1 (cdr l)
			(set-cdr! l (prog1 prev
					   (set! prev l))))))
  prev)

(define (reverse! l) (reverse!- () l))

(define (delete-duplicates lst)
  (if (length> lst 20)
      (let ((t (table)))
	(let loop ((l lst) (acc '()))
	  (if (atom? l)
	      (reverse! acc)
	      (if (has? t (car l))
		  (loop (cdr l) acc)
		  (begin
		    (put! t (car l) #t)
		    (loop (cdr l) (cons (car l) acc)))))))
      (if (atom? lst)
	  lst
	  (let ((elt  (car lst))
		(tail (cdr lst)))
	    (if (member elt tail)
		(delete-duplicates tail)
		(cons elt
		      (delete-duplicates tail)))))))

; backquote -------------------------------------------------------------------

(define (revappend l1 l2) (reverse-  l2 l1))
(define (nreconc   l1 l2) (reverse!- l2 l1))

(define (self-evaluating? x)
  (or (and (atom? x)
           (not (symbol? x)))
      (and (constant? x)
	   (symbol? x)
           (eq? x (top-level-value x)))))

(define-macro (quasiquote x) (bq-process x))

(define (bq-process x)
  (define (splice-form? x)
    (or (and (pair? x) (or (eq? (car x) 'unquote-splicing)
			   (eq? (car x) 'unquote-nsplicing)))
	(eq? x 'unquote)))
  ; bracket without splicing
  (define (bq-bracket1 x)
    (if (and (pair? x) (eq? (car x) 'unquote))
	(cadr x)
	(bq-process x)))
  (cond ((self-evaluating? x)
         (if (vector? x)
             (let ((body (bq-process (vector->list x))))
               (if (eq? (car body) 'list)
                   (cons vector (cdr body))
		   (list apply vector body)))
	     x))
        ((atom? x)                    (list 'quote x))
        ((eq? (car x) 'quasiquote)    (bq-process (bq-process (cadr x))))
        ((eq? (car x) 'unquote)       (cadr x))
        ((not (any splice-form? x))
         (let ((lc    (lastcdr x))
               (forms (map bq-bracket1 x)))
           (if (null? lc)
               (cons 'list forms)
	       (if (null? (cdr forms))
		   (list cons (car forms) (bq-process lc))
		   (nconc (cons 'list* forms) (list (bq-process lc)))))))
        (#t (let ((p x) (q ()))
	      (while (and (pair? p)
			  (not (eq? (car p) 'unquote)))
		     (set! q (cons (bq-bracket (car p)) q))
		     (set! p (cdr p)))
	      (let ((forms
		     (cond ((pair? p) (nreconc q (list (cadr p))))
			   ((null? p)  (reverse! q))
			   (#t        (nreconc q (list (bq-process p)))))))
		(if (null? (cdr forms))
		    (car forms)
		    (if (and (length= forms 2)
			     (length= (car forms) 2)
			     (eq? list (caar forms)))
			(list cons (cadar forms) (cadr forms))
			(cons 'nconc forms))))))))

(define (bq-bracket x)
  (cond ((atom? x)                        (list list (bq-process x)))
        ((eq? (car x) 'unquote)           (list list (cadr x)))
        ((eq? (car x) 'unquote-splicing)  (list 'copy-list (cadr x)))
        ((eq? (car x) 'unquote-nsplicing) (cadr x))
        (#t                               (list list (bq-process x)))))

; standard macros -------------------------------------------------------------

(define (quote-value v)
  (if (self-evaluating? v)
      v
      (list 'quote v)))

(define-macro (let* binds . body)
  (if (atom? binds) `((lambda () ,@body))
      `((lambda (,(caar binds))
	  ,@(if (pair? (cdr binds))
		`((let* ,(cdr binds) ,@body))
		body))
	,(cadar binds))))

(define-macro (when   c . body) (list 'if c (cons 'begin body) #f))
(define-macro (unless c . body) (list 'if c #f (cons 'begin body)))

(define-macro (case key . clauses)
  (define (vals->cond key v)
    (cond ((eq? v 'else)   'else)
	  ((null? v)       #f)
	  ((symbol? v)     `(eq?  ,key ,(quote-value v)))
          ((atom? v)       `(eqv? ,key ,(quote-value v)))
	  ((null? (cdr v)) `(eqv? ,key ,(quote-value (car v))))
	  ((every symbol? v)
	                   `(memq ,key ',v))
	  (else            `(memv ,key ',v))))
  (let ((g (gensym)))
    `(let ((,g ,key))
       (cond ,.(map (lambda (clause)
		      (cons (vals->cond g (car clause))
			    (cdr clause)))
		    clauses)))))

#;(define-macro (do vars test-spec . commands)
  (let ((loop (gensym))
	(test-expr (car test-spec))
	(vars  (map car  vars))
	(inits (map cadr vars))
	(steps (map (lambda (x)
		      (if (pair? (cddr x))
			  (caddr x)
			  (car x)))
		    vars)))
    `(letrec ((,loop (lambda ,vars
		       (if ,test-expr
			   (begin
			     ,@(cdr test-spec))
			   (begin
			     ,@commands
			     (,loop ,.steps))))))
       (,loop ,.inits))))

; SRFI 8
(define-macro (receive formals expr . body)
  `(call-with-values (lambda () ,expr)
     (lambda ,formals ,@body)))

(define-macro (dotimes var . body)
  (let ((v (car var))
        (cnt (cadr var)))
    `(for 0 (- ,cnt 1)
          (lambda (,v) ,@body))))

(define (map-int f n)
  (if (<= n 0)
      ()
    (let ((first (cons (f 0) ()))
          (acc ()))
      (set! acc first)
      (for 1 (- n 1)
           (lambda (i)
             (begin (set-cdr! acc (cons (f i) ()))
                    (set! acc (cdr acc)))))
      first)))

(define (iota n) (map-int identity n))

(define (for-each f l . lsts)
  (define (for-each-n f lsts)
    (if (pair? (car lsts))
	(begin (apply f (map car lsts))
	       (for-each-n f (map cdr lsts)))))
  (if (null? lsts)
      (while (pair? l)
	     (begin (f (car l))
		    (set! l (cdr l))))
      (for-each-n f (cons l lsts)))
  #t)

(define-macro (with-bindings binds . body)
  (let ((vars (map car binds))
	(vals (map cadr binds))
	(olds (map (lambda (x) (gensym)) binds)))
    `(let ,(map list olds vars)
       ,@(map (lambda (v val) `(set! ,v ,val)) vars vals)
       (unwind-protect
	(begin ,@body)
	(begin ,@(map (lambda (v old) `(set! ,v ,old)) vars olds))))))

; exceptions ------------------------------------------------------------------

(define (error . args) (raise (cons 'error args)))

(define-macro (throw tag value) `(raise (list 'thrown-value ,tag ,value)))
(define-macro (catch tag expr)
  (let ((e (gensym)))
    `(trycatch ,expr
               (lambda (,e) (if (and (pair? ,e)
                                     (eq? (car  ,e) 'thrown-value)
                                     (eq? (cadr ,e) ,tag))
                                (caddr ,e)
				(raise ,e))))))

(define-macro (unwind-protect expr finally)
  (let ((e   (gensym))
	(thk (gensym)))
    `(let ((,thk (lambda () ,finally)))
       (prog1 (trycatch ,expr
			(lambda (,e) (begin (,thk) (raise ,e))))
	      (,thk)))))

; debugging utilities ---------------------------------------------------------

(define-macro (assert expr) `(if ,expr #t (raise '(assert-failed ,expr))))

#;(define traced?
  (letrec ((sample-traced-lambda (lambda args (begin (write (cons 'x args))
						     (newline)
						     (apply #.apply args)))))
    (lambda (f)
      (and (closure? f)
	   (equal? (function:code f)
		   (function:code sample-traced-lambda))))))

#;(define (trace sym)
  (let* ((func (top-level-value sym))
	 (args (gensym)))
    (if (not (traced? func))
	(set-top-level-value! sym
			      (eval
			       `(lambda ,args
				  (begin (write (cons ',sym ,args))
					 (newline)
					 (apply ',func ,args)))))))
  'ok)

#;(define (untrace sym)
  (let ((func (top-level-value sym)))
    (if (traced? func)
	(set-top-level-value! sym
			      (aref (function:vals func) 2)))))

(define-macro (time expr)
  (let ((t0 (gensym)))
    `(let ((,t0 (time.now)))
       (prog1
	,expr
	(princ "Elapsed time: " (- (time.now) ,t0) " seconds\n")))))

; text I/O --------------------------------------------------------------------

(define (print . args) (for-each write args))
(define (princ . args)
  (with-bindings ((*print-readably* #f))
		 (for-each write args)))

(define (newline (port *output-stream*))
  (io.write port *linefeed*)
  #t)

(define (io.readline s) (io.readuntil s #\linefeed))

; call f on a stream until the stream runs out of data
(define (read-all-of f s)
  (let loop ((lines ())
	     (curr  (f s)))
    (if (io.eof? s)
	(reverse! lines)
	(loop (cons curr lines) (f s)))))

(define (io.readlines s) (read-all-of io.readline s))
(define (read-all s) (read-all-of read s))

(define (io.readall s)
  (let ((b (buffer)))
    (io.copy b s)
    (let ((str (io.tostring! b)))
      (if (and (equal? str "") (io.eof? s))
	  (eof-object)
	  str))))

(define-macro (with-output-to stream . body)
  `(with-bindings ((*output-stream* ,stream))
		  ,@body))
(define-macro (with-input-from stream . body)
  `(with-bindings ((*input-stream* ,stream))
		  ,@body))

; vector functions ------------------------------------------------------------

(define (list->vector l) (apply vector l))
(define (vector->list v)
  (let ((n (length v))
        (l ()))
    (for 1 n
         (lambda (i)
           (set! l (cons (aref v (- n i)) l))))
    l))

#;(define (vector.map f v)
  (let* ((n (length v))
         (nv (vector.alloc n)))
    (for 0 (- n 1)
         (lambda (i)
           (aset! nv i (f (aref v i)))))
    nv))

; table functions -------------------------------------------------------------

(define (table.pairs t)
  (table.foldl (lambda (k v z) (cons (cons k v) z))
               () t))
(define (table.keys t)
  (table.foldl (lambda (k v z) (cons k z))
               () t))
#;(define (table.values t)
  (table.foldl (lambda (k v z) (cons v z))
               () t))
#;(define (table.clone t)
  (let ((nt (table)))
    (table.foldl (lambda (k v z) (put! nt k v))
                 () t)
    nt))
#;(define (table.invert t)
  (let ((nt (table)))
    (table.foldl (lambda (k v z) (put! nt v k))
		 () t)
    nt))
(define (table.foreach f t)
  (table.foldl (lambda (k v z) (begin (f k v) #t)) () t))

; string functions ------------------------------------------------------------

(define (string.tail s n) (string.sub s (string.inc s 0 n)))

(define *whitespace*
  (string.encode #array(wchar 9 10 11 12 13 32 133 160 5760 6158 8192
			      8193 8194 8195 8196 8197 8198 8199 8200
			      8201 8202 8232 8233 8239 8287 12288)))

#;(define (string.trim s at-start at-end)
  (define (trim-start s chars i L)
    (if (and (< i L)
	     (string.find chars (string.char s i)))
	(trim-start s chars (string.inc s i) L)
	i))
  (define (trim-end s chars i)
    (if (and (> i 0)
	     (string.find chars (string.char s (string.dec s i))))
	(trim-end s chars (string.dec s i))
	i))
  (let ((L (length s)))
    (string.sub s
		(trim-start s at-start 0 L)
		(trim-end   s at-end   L))))

(define (string.map f s)
  (let ((b (buffer))
	(n (length s)))
    (let ((i 0))
      (while (< i n)
	     (begin (io.putc b (f (string.char s i)))
		    (set! i (string.inc s i)))))
    (io.tostring! b)))

#;(define (string.rep s k)
  (cond ((< k 4)
	 (cond ((<= k 0) "")
	       ((=  k 1) (string s))
	       ((=  k 2) (string s s))
	       (else     (string s s s))))
	((odd? k) (string s (string.rep s (- k 1))))
	(else     (string.rep (string s s) (/ k 2)))))

#;(define (string.lpad s n c) (string (string.rep c (- n (string.count s))) s))
#;(define (string.rpad s n c) (string s (string.rep c (- n (string.count s)))))

(define (print-to-string v)
  (let ((b (buffer)))
    (write v b)
    (io.tostring! b)))

(define (string.join strlist sep)
  (if (null? strlist) ""
      (let ((b (buffer)))
	(io.write b (car strlist))
	(for-each (lambda (s) (begin (io.write b sep)
				     (io.write b s)))
		  (cdr strlist))
	(io.tostring! b))))

; toplevel --------------------------------------------------------------------

(define (macrocall? e) (and (symbol? (car e))
			    (symbol-syntax (car e))))

#;(define (macroexpand-1 e)
  (if (atom? e) e
      (let ((f (macrocall? e)))
	(if f (apply f (cdr e))
	    e))))

(define (expand e)
  ; symbol resolves to toplevel; i.e. has no shadowing definition
  (define (top? s env) (not (or (bound? s) (assq s env))))

  (define (splice-begin body)
    (cond ((atom? body) body)
	  ((equal? body '((begin)))
	   body)
	  ((and (pair? (car body))
		(eq? (caar body) 'begin))
	   (append (splice-begin (cdar body)) (splice-begin (cdr body))))
	  (else
	   (cons (car body) (splice-begin (cdr body))))))

  (define *expanded* (list '*expanded*))

  (define (expand-body body env)
    (if (atom? body) body
	(let* ((body  (if (top? 'begin env)
			  (splice-begin body)
			  body))
	       (def?  (top? 'define env))
	       (dvars (if def? (get-defined-vars body) ()))
	       (env   (nconc (map list dvars) env)))
	  (if (not def?)
	      (map (lambda (x) (expand-in x env)) body)
	      (let* ((ex-nondefs    ; expand non-definitions
		      (let loop ((body body))
			(cond ((atom? body) body)
			      ((and (pair? (car body))
				    (eq? 'define (caar body)))
			       (cons (car body) (loop (cdr body))))
			      (else
			       (let ((form (expand-in (car body) env)))
				 (set! env (nconc
					    (map list (get-defined-vars form))
					    env))
				 (cons
				  (cons *expanded* form)
				  (loop (cdr body))))))))
		     (body ex-nondefs))
		(while (pair? body) ; now expand deferred definitions
		       (if (not (eq? *expanded* (caar body)))
			   (set-car! body (expand-in (car body) env))
			   (set-car! body (cdar body)))
		       (set! body (cdr body)))
		ex-nondefs)))))

  (define (expand-lambda-list l env)
    (if (atom? l) l
	(cons (if (and (pair? (car l)) (pair? (cdr (car l))))
		  (list (caar l) (expand-in (cadar l) env))
		  (car l))
	      (expand-lambda-list (cdr l) env))))

  (define (l-vars l)
    (cond ((atom? l)       (list l))
	  ((pair? (car l)) (cons (caar l) (l-vars (cdr l))))
	  (else            (cons (car l)  (l-vars (cdr l))))))

  (define (expand-lambda e env)
    (let ((formals (cadr e))
	  (name    (lastcdr e))
	  (body    (cddr e))
	  (vars    (l-vars (cadr e))))
      (let ((env   (nconc (map list vars) env)))
	`(lambda ,(expand-lambda-list formals env)
	   ,.(expand-body body env)
	   . ,name))))

  (define (expand-define e env)
    (if (or (null? (cdr e)) (atom? (cadr e)))
	(if (null? (cddr e))
	    e
	    `(define ,(cadr e) ,(expand-in (caddr e) env)))
	(let ((formals (cdadr e))
	      (name    (caadr e))
	      (body    (cddr e))
	      (vars    (l-vars (cdadr e))))
	  (let ((env   (nconc (map list vars) env)))
	    `(define ,(cons name (expand-lambda-list formals env))
	       ,.(expand-body body env))))))

  (define (expand-let-syntax e env)
    (let ((binds (cadr e)))
      (cons 'begin
	    (expand-body (cddr e)
			 (nconc
			  (map (lambda (bind)
				 (list (car bind)
				       ((compile-thunk
					 (expand-in (cadr bind) env)))
				       env))
			       binds)
			  env)))))

  ; given let-syntax definition environment (menv) and environment
  ; at the point of the macro use (lenv), return the environment to
  ; expand the macro use in. TODO
  (define (local-expansion-env menv lenv) menv)

  (define (expand-in e env)
    (if (atom? e) e
	(let* ((head (car e))
	       (bnd  (assq head env))
	       (default (lambda ()
			  (let loop ((e e))
			    (if (atom? e) e
				(cons (if (atom? (car e))
					  (car e)
					  (expand-in (car e) env))
				      (loop (cdr e))))))))
	  (cond ((and bnd (pair? (cdr bnd)))  ; local macro
		 (expand-in (apply (cadr bnd) (cdr e))
			    (local-expansion-env (caddr bnd) env)))
		((or bnd                      ; bound lexical or toplevel var
		     (not (symbol? head))
		     (bound? head))
		 (default))
		((macrocall? e) =>      (lambda (f)
				          (expand-in (apply f (cdr e)) env)))
		((eq? head 'quote)      e)
		((eq? head 'lambda)     (expand-lambda e env))
		((eq? head 'define)     (expand-define e env))
		((eq? head 'let-syntax) (expand-let-syntax e env))
		(else                   (default))))))
  (expand-in e ()))

(define (eval x) ((compile-thunk (expand x))))

(define (load-process x) (eval x))

(define (load filename)
  (let ((F (file filename :read)))
    (trycatch
     (let next (prev E v)
       (if (not (io.eof? F))
	   (next (read F)
                 prev
		 (load-process E))
	   (begin (io.close F)
		  ; evaluate last form in almost-tail position
		  (load-process E))))
     (lambda (e)
       (begin
	 (io.close F)
	 (raise `(load-error ,filename ,e)))))))

(define *banner* (string.tail "
;  _
; |_ _ _ |_ _ |  . _ _
; | (-||||_(_)|__|_)|_)
;-------------------|----------------------------------------------------------

" 1))

(define (repl)
  (define (prompt)
    (princ "> ") (io.flush *output-stream*)
    (let ((v (trycatch (read)
		       (lambda (e) (begin (io.discardbuffer *input-stream*)
					  (raise e))))))
      (and (not (io.eof? *input-stream*))
	   (let ((V (load-process v)))
	     (print V)
	     (set! that V)
	     #t))))
  (define (reploop)
    (when (trycatch (and (prompt) (newline))
		    (lambda (e)
		      (top-level-exception-handler e)
		      #t))
	  (begin (newline)
		 (reploop))))
  (reploop)
  (newline))

(define (top-level-exception-handler e)
  (with-output-to *stderr*
		  (print-exception e)
		  (print-stack-trace (stacktrace))))

(define (print-stack-trace st)
  (define (find-in-f f tgt path)
    (let ((path (cons (function:name f) path)))
      (if (eq? (function:code f) (function:code tgt))
	  (throw 'ffound path)
	  (let ((v (function:vals f)))
	    (for 0 (1- (length v))
		 (lambda (i) (if (closure? (aref v i))
				 (find-in-f (aref v i) tgt path))))))))
  (define (fn-name f e)
    (let ((p (catch 'ffound
		    (begin
		      (for-each (lambda (topfun)
				  (find-in-f topfun f ()))
				e)
		      #f))))
      (if p
	  (symbol (string.join (map string (reverse! p)) "/"))
	  'lambda)))
  (let ((st (reverse! (list-tail st (if *interactive* 5 4))))
	(e (filter closure? (map (lambda (s) (and (bound? s)
						  (top-level-value s)))
				 (environment))))
	(n 0))
    (for-each
     (lambda (f)
       (princ "#" n " ")
       (print (cons (fn-name (aref f 0) e)
		    (cdr (vector->list f))))
       (newline)
       (set! n (+ n 1)))
     st)))

(define (print-exception e)
  (cond ((and (pair? e)
	      (eq? (car e) 'type-error)
	      (length= e 4))
	 (princ "type error: " (cadr e) ": expected " (caddr e) ", got ")
	 (print (cadddr e)))

	((and (pair? e)
	      (eq? (car e) 'bounds-error)
	      (length= e 4))
	 (princ (cadr e) ": index " (cadddr e) " out of bounds for ")
	 (print (caddr e)))

	((and (pair? e)
	      (eq? (car e) 'unbound-error)
	      (pair? (cdr e)))
	 (princ "eval: variable " (cadr e) " has no value"))

	((and (pair? e)
	      (eq? (car e) 'error))
	 (princ "error: ")
	 (apply princ (cdr e)))

	((and (pair? e)
	      (eq? (car e) 'load-error))
	 (print-exception (caddr e))
	 (princ "in file " (cadr e)))

	((and (list? e)
	      (length= e 2))
	 (print (car e))
	 (princ ": ")
	 (let ((msg (cadr e)))
	   ((if (or (string? msg) (symbol? msg))
		princ print)
	    msg)))

	(else (princ "*** Unhandled exception: ")
	      (print e)))

  (princ *linefeed*))

(define (simple-sort l)
  (if (or (null? l) (null? (cdr l))) l
      (let ((piv (car l)))
	(receive (less grtr)
		 (separate (lambda (x) (< x piv)) (cdr l))
		 (nconc (simple-sort less)
			(list piv)
			(simple-sort grtr))))))

(define (make-system-image fname)
  (let ((f (file fname :write :create :truncate))
	(excludes '(*linefeed* *directory-separator* *argv* that
			       *print-pretty* *print-width* *print-readably*
			       *print-level* *print-length* *os-name*)))
    (with-bindings ((*print-pretty* #t)
		    (*print-readably* #t))
      (let ((syms
	     (filter (lambda (s)
		       (and (bound? s)
			    (not (constant? s))
			    (or (not (builtin? (top-level-value s)))
				(not (equal? (string s) ; alias of builtin
					     (string (top-level-value s)))))
			    (not (memq s excludes))
			    (not (iostream? (top-level-value s)))))
		     (simple-sort (environment)))))
	(write (apply nconc (map list syms (map top-level-value syms))) f)
	(io.write f *linefeed*))
      (io.close f))))

; initialize globals that need to be set at load time
(define (__init_globals)
  (if (or (eq? *os-name* 'win32)
	  (eq? *os-name* 'win64)
	  (eq? *os-name* 'windows))
      (begin (set! *directory-separator* "\\")
	     (set! *linefeed* "\r\n"))
      (begin (set! *directory-separator* "/")
	     (set! *linefeed* "\n")))
  (set! *output-stream* *stdout*)
  (set! *input-stream*  *stdin*)
  (set! *error-stream*  *stderr*))

(define (__script fname)
  (trycatch (load fname)
	    (lambda (e) (begin (top-level-exception-handler e)
			       (exit 1)))))

(define (__start argv)
  (__init_globals)
  (if (pair? (cdr argv))
      (begin (set! *argv* (cdr argv))
	     (set! *interactive* #f)
	     (__script (cadr argv)))
      (begin (set! *argv* argv)
	     (set! *interactive* #t)
	     (princ *banner*)
	     (repl)))
  (exit 0))
