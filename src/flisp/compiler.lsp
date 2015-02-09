; -*- scheme -*-

(define Instructions
  (let ((e (table))
	(keys
	 [nop dup pop call tcall jmp brf brt jmp.l brf.l brt.l ret

	  eq? eqv? equal? atom? not null? boolean? symbol?
	  number? bound? pair? builtin? vector? fixnum? function?

	  cons list car cdr set-car! set-cdr!
	  apply

	  + - * / div0 = < compare

	  vector aref aset!

	  loadt loadf loadnil load0 load1 loadi8
	  loadv loadv.l
	  loadg loadg.l
	  loada loada.l loadc loadc.l
	  setg setg.l
	  seta seta.l removed-setc removed-setc.l

	  closure argc vargc trycatch for tapply
	  add2 sub2 neg largc lvargc
	  loada0 loada1 loadc0 loadc1 call.l tcall.l
	  brne brne.l cadr brnn brnn.l brn brn.l
	  optargs brbound keyargs box box.l shift

	  dummy_t dummy_f dummy_nil]))
    (for 0 (1- (length keys))
	 (lambda (i)
	   (put! e (aref keys i) i)))))

(define arg-counts
  (table eq?      2      eqv?     2
	 equal?   2      atom?    1
	 not      1      null?    1
	 boolean? 1      symbol?  1
	 number?  1      bound?   1
	 pair?    1      builtin? 1
	 vector?  1      fixnum?  1
	 cons     2      car      1
	 cdr      1      set-car! 2
	 set-cdr! 2      =        2
         <        2      compare  2
         aref     2      aset!    3
	 div0     2))

;; code generation state, constant tables, bytecode encoding

(define (make-code-emitter) (vector () (table) 0 () 0))
(define (bcode:code   b) (aref b 0))
(define (bcode:ctable b) (aref b 1))
(define (bcode:nconst b) (aref b 2))
(define (bcode:cenv b)   (aref b 3))
(define (bcode:sp b)     (aref b 4))
(define (bcode:stack b n) (aset! b 4 (+ (aref b 4) n)))

;; get an index for a referenced value in a bytecode object
(define (bcode:indexfor b v)
  (let ((const-to-idx (bcode:ctable b))
	(nconst       (bcode:nconst b)))
    (if (has? const-to-idx v)
	(get const-to-idx v)
	(begin (put! const-to-idx v nconst)
	       (prog1 nconst
		      (aset! b 2 (+ nconst 1)))))))

(define (emit e inst . args)
  (if (null? args)
      (if (and (eq? inst 'car) (pair? (aref e 0))
	       (eq? (car (aref e 0)) 'cdr))
	  (set-car! (aref e 0) 'cadr)
	  (aset! e 0 (cons inst (aref e 0))))
      (begin
	(if (memq inst '(loadv loadg setg))
	    (set! args (list (bcode:indexfor e (car args)))))
	(let ((longform
	       (assq inst '((loadv loadv.l) (loadg loadg.l) (setg setg.l)
			    (loada loada.l) (seta  seta.l) (box box.l)))))
	  (if (and longform
		   (> (car args) 255))
	      (set! inst (cadr longform))))
	(let ((longform
	       (assq inst '((loadc loadc.l)))))
	  (if (and longform
		   (> (car  args) 255))
	      (set! inst (cadr longform))))
	(if (eq? inst 'loada)
	    (cond ((equal? args '(0))
		   (set! inst 'loada0)
		   (set! args ()))
		  ((equal? args '(1))
		   (set! inst 'loada1)
		   (set! args ()))))
	(if (eq? inst 'loadc)
	    (cond ((equal? args '(0))
		   (set! inst 'loadc0)
		   (set! args ()))
		  ((equal? args '(1))
		   (set! inst 'loadc1)
		   (set! args ()))))

	(let ((lasti (if (pair? (aref e 0))
			 (car (aref e 0)) ()))
	      (bc (aref e 0)))
	  (cond ((and
		  (eq? inst 'brf)
		  (cond ((and (eq? lasti 'not)
			      (eq? (cadr bc) 'null?))
			 (aset! e 0 (cons (car args) (cons 'brn (cddr bc)))))
			((eq? lasti 'not)
			 (aset! e 0 (cons (car args) (cons 'brt (cdr bc)))))
			((eq? lasti 'eq?)
			 (aset! e 0 (cons (car args) (cons 'brne (cdr bc)))))
			((eq? lasti 'null?)
			 (aset! e 0 (cons (car args) (cons 'brnn (cdr bc)))))
			(else #f))))
		((and (eq? inst 'brt) (eq? lasti 'null?))
		 (aset! e 0 (cons (car args) (cons 'brn (cdr bc)))))
		(else
		 (aset! e 0 (nreconc (cons inst args) bc)))))))
  e)

(define (make-label e)   (gensym))
(define (mark-label e l) (emit e 'label l))

;; convert symbolic bytecode representation to a byte array.
;; labels are fixed-up.
(define (encode-byte-code e)
  (let* ((cl (reverse! e))
	 (v  (list->vector cl))
	 (long? (>= (+ (length v)  ;; 1 byte for each entry, plus...
		       ;; at most half the entries in this vector can be
		       ;; instructions accepting 32-bit arguments
		       (* 3 (div0 (length v) 2)))
		    65536)))
    (let ((n              (length v))
	  (i              0)
	  (label-to-loc   (table))
	  (fixup-to-label (table))
	  (bcode          (buffer))
	  (vi             #f)
	  (nxt            #f))
      (io.write bcode #int32(0))
      (while (< i n)
	(begin
	  (set! vi (aref v i))
	  (if (eq? vi 'label)
	      (begin (put! label-to-loc (aref v (+ i 1)) (sizeof bcode))
		     (set! i (+ i 2)))
	      (begin
		(io.write bcode
			  (byte
			   (get Instructions
				(if long?
				    (case vi
				      (jmp  'jmp.l)
				      (brt  'brt.l)
				      (brf  'brf.l)
				      (brne 'brne.l)
				      (brnn 'brnn.l)
				      (brn  'brn.l)
				      (else vi))
				    vi))))
		(set! i (+ i 1))
		(set! nxt (if (< i n) (aref v i) #f))
		(cond ((memq vi '(jmp brf brt brne brnn brn))
		       (put! fixup-to-label (sizeof bcode) nxt)
		       (io.write bcode ((if long? int32 int16) 0))
		       (set! i (+ i 1)))
		      ((eq? vi 'brbound)
		       (io.write bcode (int32 nxt))
		       (set! i (+ i 1)))
		      ((number? nxt)
		       (case vi
			 ((loadv.l loadg.l setg.l loada.l seta.l loadc.l
			   largc lvargc call.l tcall.l box.l)
			  (io.write bcode (int32 nxt))
			  (set! i (+ i 1)))

			 ((optargs keyargs)  ; 2 int32 args
			  (io.write bcode (int32 nxt))
			  (set! i (+ i 1))
			  (io.write bcode (int32 (aref v i)))
			  (set! i (+ i 1))
			  (if (eq? vi 'keyargs)
			      (begin (io.write bcode (int32 (aref v i)))
				     (set! i (+ i 1)))))

			 (else
			  ; other number arguments are always uint8
			  (io.write bcode (uint8 nxt))
			  (set! i (+ i 1)))))
		      (else #f))))))

      (table.foreach
       (lambda (addr labl)
	 (begin (io.seek bcode addr)
		(io.write bcode ((if long? int32 int16)
				 (- (get label-to-loc labl)
				    addr)))))
       fixup-to-label)
      (io.tostring! bcode))))

(define (const-to-idx-vec e)
  (let ((cvec (vector.alloc (bcode:nconst e))))
    (table.foreach (lambda (val idx) (aset! cvec idx val))
		   (bcode:ctable e))
    cvec))

;; variables

(define (vinfo sym heap? index) (list sym heap? index))
(define vinfo:sym car)
(define vinfo:heap? cadr)
(define vinfo:index caddr)

(define (quoted? e) (eq? (car e) 'quote))

(define (index-of item lst start)
  (cond ((null? lst) #f)
	((eq? item (car lst)) start)
	(else (index-of item (cdr lst) (+ start 1)))))

(define (capture-var! g sym)
  (let ((ce (bcode:cenv g)))
    (let ((n (index-of sym ce 0)))
      (or n
	  (prog1 (length ce)
		 (aset! g 3 (append! ce (list sym))))))))

(define (in-env? s env)
  (and (pair? env)
       (or (assq s (car env))
	   (in-env? s (cdr env)))))

(define (lookup-sym s env lev)
  (if (null? env)
      'global
      (let* ((curr (car env))
	     (vi   (assq s curr)))
	(if vi
	    (cons lev vi)
	    (lookup-sym s
			(cdr env)
			(+ lev 1))))))

(define (printable? x) (not (or (iostream? x)
				(eof-object? x))))

(define (compile-sym g env s deref)
  (let ((loc (lookup-sym s env 0)))
    (cond ((eq? loc 'global)
	   (if (and (constant? s)
		    (printable? (top-level-value s)))
	       (emit g 'loadv (top-level-value s))
	       (emit g 'loadg s)))

	  ((= (car loc) 0)
	   (emit g 'loada (vinfo:index (cdr loc)))
	   (if (and deref (vinfo:heap? (cdr loc)))
	       (emit g 'car)))

	  (else
	   (emit g 'loadc (capture-var! g s))
	   (if (and deref (vinfo:heap? (cdr loc)))
	       (emit g 'car))))))

(define (compile-set! g env s rhs)
  (let ((loc (lookup-sym s env 0)))
    (if (eq? loc 'global)
	(begin (compile-in g env #f rhs)
	       (emit g 'setg s))
	(let ((arg?   (= (car loc) 0)))
	  (let ((h?   (vinfo:heap? (cdr loc)))
		(idx  (if arg?
			  (vinfo:index (cdr loc))
			  (capture-var! g s))))
	    (if h?
		(begin (emit g (if arg? 'loada 'loadc) idx)
		       (bcode:stack g 1)
		       (compile-in g env #f rhs)
		       (bcode:stack g -1)
		       (emit g 'set-car!))

		(begin (compile-in g env #f rhs)
		       (if (not arg?) (error (string "internal error: misallocated var " s)))
		       (emit g 'seta idx))))))))

(define (box-vars g env)
  (let loop ((e env))
    (if (pair? e)
	(begin (if (cadr (car e))
		   (emit g 'box (caddr (car e))))
	       (loop (cdr e))))))

;; control flow

(define (compile-if g env tail? x)
  (let ((elsel (make-label g))
	(endl  (make-label g))
	(test  (cadr x))
	(then  (caddr x))
	(else  (if (pair? (cdddr x))
		   (cadddr x)
		   (void))))
    (cond ((eq? test #t)
	   (compile-in g env tail? then))
	  ((eq? test #f)
	   (compile-in g env tail? else))
	  (else
	   (compile-in g env #f test)
	   (emit g 'brf elsel)
	   (compile-in g env tail? then)
	   (if tail?
	       (emit g 'ret)
	       (emit g 'jmp endl))
	   (mark-label g elsel)
	   (compile-in g env tail? else)
	   (mark-label g endl)))))

(define (compile-begin g env tail? forms)
  (cond ((atom? forms) (compile-in g env tail? (void)))
	((atom? (cdr forms))
	 (compile-in g env tail? (car forms)))
	(else
	 (compile-in g env #f (car forms))
	 (emit g 'pop)
	 (compile-begin g env tail? (cdr forms)))))

(define (compile-prog1 g env x)
  (compile-in g env #f (cadr x))
  (if (pair? (cddr x))
      (begin (bcode:stack g 1)
	     (compile-begin g env #f (cddr x))
	     (emit g 'pop)
	     (bcode:stack g -1))))

(define (compile-while g env cond body)
  (let ((top  (make-label g))
	(end  (make-label g)))
    (compile-in g env #f (void))
    (bcode:stack g 1)
    (mark-label g top)
    (compile-in g env #f cond)
    (emit g 'brf end)
    (emit g 'pop)
    (bcode:stack g -1)
    (compile-in g env #f body)
    (emit g 'jmp top)
    (mark-label g end)))

(define (1arg-lambda? func)
  (and (pair? func)
       (eq? (car func) 'lambda)
       (pair? (cdr func))
       (pair? (cadr func))
       (length= (cadr func) 1)))

(define (compile-for g env lo hi func)
  (if (1arg-lambda? func)
      (begin (compile-in g env #f lo)
	     (bcode:stack g 1)
	     (compile-in g env #f hi)
	     (bcode:stack g 1)
	     (compile-in g env #f func)
	     (emit g 'for)
	     (bcode:stack g -2))
      (error "for: third form must be a 1-argument lambda")))

(define (compile-short-circuit g env tail? forms default branch)
  (cond ((atom? forms)        (compile-in g env tail? default))
	((atom? (cdr forms))  (compile-in g env tail? (car forms)))
	(else
	 (let ((end  (make-label g)))
	   (compile-in g env #f (car forms))
	   (bcode:stack g 1)
	   (emit g 'dup)
	   (emit g branch end)
	   (bcode:stack g -1)
	   (emit g 'pop)
	   (compile-short-circuit g env tail? (cdr forms) default branch)
	   (mark-label g end)))))

(define (compile-and g env tail? forms)
  (compile-short-circuit g env tail? forms #t 'brf))
(define (compile-or g env tail? forms)
  (compile-short-circuit g env tail? forms #f 'brt))

;; calls

(define (compile-arglist g env lst)
  (for-each (lambda (a)
	      (compile-in g env #f a)
	      (bcode:stack g 1))
	    lst)
  (length lst))

(define (argc-error head count)
  (error "compile error: " head " expects " count
	 (if (= count 1)
	     " argument."
	     " arguments.")))

(define builtin->instruction
  (let ((b2i (table number? 'number?  cons 'cons
		    fixnum? 'fixnum?  equal? 'equal?
		    eq? 'eq?  symbol? 'symbol?
		    div0 'div0  builtin? 'builtin?
		    aset! 'aset!  - '-  boolean? 'boolean?  not 'not
		    apply 'apply  atom? 'atom?
		    set-cdr! 'set-cdr!  / '/
		    function? 'function?  vector 'vector
		    list 'list  bound? 'bound?
		    < '<  * '* cdr 'cdr  null? 'null?
		    + '+  eqv? 'eqv? compare 'compare  aref 'aref
		    set-car! 'set-car!  car 'car
		    pair? 'pair?  = '=  vector? 'vector?)))
    (lambda (b)
      (get b2i b #f))))

(define (compile-builtin-call g env tail? x head b nargs)
  (let ((count (get arg-counts head #f)))
    (if (and count
	     (not (length= (cdr x) count)))
	(argc-error b count))
    (case b  ; handle special cases of vararg builtins
      (list (if (= nargs 0) (emit g 'loadnil) (emit g b nargs)))
      (+    (cond ((= nargs 0) (emit g 'load0))
		  ((= nargs 2) (emit g 'add2))
		  (else (emit g b nargs))))
      (-    (cond ((= nargs 0) (argc-error b 1))
		  ((= nargs 1) (emit g 'neg))
		  ((= nargs 2) (emit g 'sub2))
		  (else (emit g b nargs))))
      (*    (if (= nargs 0) (emit g 'load1)
		(emit g b nargs)))
      (/    (if (= nargs 0)
		(argc-error b 1)
		(emit g b nargs)))
      (vector   (if (= nargs 0)
		    (emit g 'loadv [])
		    (emit g b nargs)))
      (apply    (if (< nargs 2)
		    (argc-error b 2)
		    (emit g (if tail? 'tapply 'apply) nargs)))
      (else      (emit g b)))))

(define (inlineable? form)
  (let ((lam (car form)))
    (and (pair? lam)
	 (eq? (car lam) 'lambda)
	 (list? (cadr lam))
	 (every symbol? (cadr lam))
	 (not (length> (cadr lam) 255))
	 (length= (cadr lam) (length (cdr form))))))

;; compile call to lambda in head position, inlined
(define (compile-let g env tail? form)
  (let ((lam  (car form))
	(args (cdr form))
	(sp   (bcode:sp g)))
    (let ((vars (cadr lam))
	  (n    (compile-arglist g env args)))
      (let ((newvars
	     (vars-to-env vars (complex-bindings (caddr lam) vars) sp)))
	(box-vars g newvars)
	(let ((newenv
	       (cons (append! newvars (car env))
		     (cdr env))))
	  (compile-in g newenv tail? (caddr lam))
	  (bcode:stack g (- n))
	  (if (and (> n 0) (not tail?))
	      (emit g 'shift n)))))))

(define (compile-app g env tail? x)
  (let ((head  (car x)))
    (let ((head
	   (if (and (symbol? head)
		    (not (in-env? head env))
		    (bound? head)
		    (constant? head)
		    (builtin? (top-level-value head)))
	       (top-level-value head)
	       head)))
      (if (length> (cdr x) 255)
	  ;; more than 255 arguments, need long versions of instructions
	  (begin (compile-in g env #f head)
		 (bcode:stack g 1)
		 (let ((nargs (compile-arglist g env (cdr x))))
		   (bcode:stack g (- nargs))
		   (emit g (if tail? 'tcall.l 'call.l) nargs)))
	  (let ((b (and (builtin? head)
			(builtin->instruction head))))
	    (if (and (eq? head 'cadr)
		     (not (in-env? head env))
		     (equal? (top-level-value 'cadr) cadr)
		     (length= x 2))
		(begin (compile-in g env #f (cadr x))
		       (emit g 'cadr))
		(if (and (pair? head) (eq? (car head) 'lambda)
			 (inlineable? x))
		    (compile-let g env tail? x)
		    (begin
		      (if (not b)
			  (begin (compile-in g env #f head)
				 (bcode:stack g 1)))
		      (let ((nargs (compile-arglist g env (cdr x))))
			(bcode:stack g (- nargs))
			(if (not b) (bcode:stack g -1))
			(if b
			    (compile-builtin-call g env tail? x head b nargs)
			    (emit g (if tail? 'tcall 'call) nargs)))))))))))

;; lambda, main compilation loop

(define (fits-i8 x) (and (fixnum? x) (>= x -128) (<= x 127)))

(define (compile-in g env tail? x)
  (cond ((symbol? x) (compile-sym g env x #t))
	((atom? x)
	 (cond ((eq? x 0)   (emit g 'load0))
	       ((eq? x 1)   (emit g 'load1))
	       ((eq? x #t)  (emit g 'loadt))
	       ((eq? x #f)  (emit g 'loadf))
	       ((eq? x ())  (emit g 'loadnil))
	       ((fits-i8 x) (emit g 'loadi8 x))
	       ((eof-object? x)
		(compile-in g env tail? (list (top-level-value 'eof-object))))
	       (else        (emit g 'loadv x))))
	((or (not (symbol? (car x))) (bound? (car x)) (in-env? (car x) env))
	 (compile-app g env tail? x))
	(else
	 (case (car x)
	   (quote    (if (self-evaluating? (cadr x))
			 (compile-in g env tail? (cadr x))
			 (emit g 'loadv (cadr x))))
	   (if       (compile-if g env tail? x))
	   (begin    (compile-begin g env tail? (cdr x)))
	   (prog1    (compile-prog1 g env x))
	   (lambda   (receive (the-f cenv) (compile-f- env x)
		       (begin (emit g 'loadv the-f)
			      (if (not (null? cenv))
				  (begin
				    (for-each (lambda (var)
						(compile-sym g env var #f))
					      cenv)
				    (emit g 'closure (length cenv)))))))
	   (and      (compile-and g env tail? (cdr x)))
	   (or       (compile-or  g env tail? (cdr x)))
	   (while    (compile-while g env (cadr x) (cons 'begin (cddr x))))
	   (for      (compile-for   g env (cadr x) (caddr x) (cadddr x)))
	   (return   (compile-in g env #t (cadr x))
		     (emit g 'ret))
	   (set!     (compile-set! g env (cadr x) (caddr x)))
	   (trycatch (compile-in g env #f `(lambda () ,(cadr x)))
		     (unless (1arg-lambda? (caddr x))
			     (error "trycatch: second form must be a 1-argument lambda"))
		     (compile-in g env #f (caddr x))
		     (emit g 'trycatch))
	   (else   (compile-app g env tail? x))))))

;; optional and keyword args

(define (keyword-arg? x) (and (pair? x) (keyword? (car x))))
(define (keyword->symbol k)
  (if (keyword? k)
      (symbol (let ((s (string k)))
		(string.sub s 0 (string.dec s (length s)))))
      k))

(define (lambda-vars l)
  (define (check-formals l o opt kw)
    (cond ((or (null? l) (symbol? l)) #t)
	  ((and (pair? l) (symbol? (car l)))
	   (if (or opt kw)
	       (error "compile error: invalid argument list "
		      o ". optional arguments must come after required.")
	       (check-formals (cdr l) o opt kw)))
	  ((and (pair? l) (pair? (car l)))
	   (unless (and (length= (car l) 2)
			(symbol? (caar l)))
		   (error "compile error: invalid optional argument " (car l)
			  " in list " o))
	   (if (keyword? (caar l))
	       (check-formals (cdr l) o opt #t)
	       (if kw
		   (error "compile error: invalid argument list "
			  o ". keyword arguments must come last.")
		   (check-formals (cdr l) o #t kw))))
	  ((pair? l)
	   (error "compile error: invalid formal argument " (car l)
		  " in list " o))
	  (else
	   (if (eq? l o)
	       (error "compile error: invalid argument list " o)
	       (error "compile error: invalid formal argument " l
		      " in list " o)))))
  (check-formals l l #f #f)
  (map (lambda (s) (if (pair? s) (keyword->symbol (car s)) s))
       (to-proper l)))

(define (emit-optional-arg-inits g env opta vars i)
  ; i is the lexical var index of the opt arg to process next
  (if (pair? opta)
      (let ((nxt (make-label g)))
	(emit g 'brbound i)
	(emit g 'brt nxt)
	(compile-in g (extend-env env (list-head vars i) '()) #f (cadar opta))
	(emit g 'seta i)
	(emit g 'pop)
	(mark-label g nxt)
	(emit-optional-arg-inits g env (cdr opta) vars (+ i 1)))))

;; define

(define (expand-define x)
  ;; expand a single `define` expression to `set!`
  (let ((form (cadr x))
	(body (if (pair? (cddr x))
		  (cddr x)
		  (if (symbol? (cadr x))
		      `(,(void))
		      (error "compile error: invalid syntax "
			     (print-to-string x))))))
    (if (symbol? form)
	`(set! ,form ,(car body))
	`(set! ,(car form)
	       (lambda ,(cdr form) ,@body . ,(car form))))))

(define get-defined-vars
  (letrec ((get-defined-vars-
	    (lambda (expr)
	      (cond ((atom? expr) ())
		    ((and (eq? (car expr) 'define)
			  (pair? (cdr expr)))
		     (or (and (symbol? (cadr expr))
			      (list (cadr expr)))
			 (and (pair? (cadr expr))
			      (symbol? (caadr expr))
			      (list (caadr expr)))
			 ()))
		    ((eq? (car expr) 'begin)
		     (apply nconc (map get-defined-vars- (cdr expr))))
		    (else ())))))
    (lambda (expr) (delete-duplicates (get-defined-vars- expr)))))

(define (lower-define e)
  ;; convert lambda to one body expression and process internal defines
  (define (lambda-body e)
    (let ((B (if (pair? (cddr e))
		 (if (pair? (cdddr e))
		     (cons 'begin (cddr e))
		     (caddr e))
		 (void))))
      (let ((V     (get-defined-vars B))
	    (new-B (lower-define B)))
	(if (null? V)
	    new-B
	    (cons `(lambda ,V ,new-B)
		  (map (lambda (x) (void)) V))))))
  (cond ((or (atom? e) (quoted? e))
	 e)
	((eq? (car e) 'define)
	 (lower-define (expand-define e)))
	((eq? (car e) 'lambda)
	 `(lambda ,(cadr e) ,(lambda-body e) . ,(lastcdr e)))
	(else
	 (map lower-define e))))

;; closure analysis

(define (lambda:body e) (caddr e))
(define (lambda:vars e) (lambda-vars (cadr e)))

(define (diff s1 s2)
  (cond ((null? s1)         '())
	((memq (car s1) s2) (diff (cdr s1) s2))
	(else               (cons (car s1) (diff (cdr s1) s2)))))

;; bindings that are both captured and set!'d
(define (complex-bindings- e vars head nested capt setd)
  (cond ((null? vars) #f)
	((symbol? e)
	 (if (and nested (memq e vars))
	     (put! capt e #t)))
	((or (atom? e) (quoted? e)) #f)
	((eq? (car e) 'set!)
	 (if (memq (cadr e) vars)
	     (begin (put! setd (cadr e) #t)
		    (if nested (put! capt (cadr e) #t))))
	 (complex-bindings- (caddr e) vars #f nested capt setd))
	((eq? (car e) 'lambda)
	 (complex-bindings- (lambda:body e)
			    (diff vars (lambda:vars e))
			    #f
			    (or (not head) nested)
			    capt setd))
	(else
	 (cons (complex-bindings- (car e) vars (inlineable? e) nested capt setd)
	       (map (lambda (x)
		      (complex-bindings- x vars #f nested capt setd))
		    (cdr e))))))

(define (complex-bindings e vars)
  (let ((capt (table))
	(setd (table)))
    (complex-bindings- e vars #f #f capt setd)
    (filter (lambda (x) (has? capt x))
	    (table.keys setd))))

(define (vars-to-env vars cb offs)
  (map (lambda (var i) (vinfo var (not (not (memq var cb))) (+ i offs)))
       vars (iota (length vars))))

(define (extend-env env vars cb)
  (cons (vars-to-env vars cb 0)
	env))

;; main entry points

(define (compile f) (compile-f () (lower-define f)))

(define (compile-thunk expr)
  ;; to eval a top-level expression we need to avoid internal define
  (compile-f () `(lambda () ,(lower-define expr))))

(define (compile-f env f)
  (receive (ff ignore)
	   (compile-f- env f)
	   ff))

(define (compile-f- env f)
  ;; compile lambda expression, assuming defines already lowered
  (let ((g     (make-code-emitter))
	(args  (cadr f))
	(atail (lastcdr (cadr f)))
	(vars  (lambda:vars f))
	(opta  (filter pair? (cadr f)))
	(last  (lastcdr f)))
    (let* ((name  (if (null? last) 'lambda last))
	   (nargs (if (atom? args) 0 (length args)))
	   (nreq  (- nargs (length opta)))
	   (kwa   (filter keyword-arg? opta)))

      ;; emit argument checking prologue
      (if (not (null? opta))
	  (begin
	    (if (null? kwa)
		(emit g 'optargs nreq
		      (if (null? atail) nargs (- nargs)))
		(begin
		  (bcode:indexfor g (make-perfect-hash-table
				     (map cons
					  (map car kwa)
					  (iota (length kwa)))))
		  (emit g 'keyargs nreq (length kwa)
			(if (null? atail) nargs (- nargs)))))
	    (emit-optional-arg-inits g env opta vars nreq)))

      (cond ((> nargs 255)           (emit g (if (null? atail)
						 'largc 'lvargc)
					   nargs))
	    ((not (null? atail))     (emit g 'vargc nargs))
	    ((null? opta)            (emit g 'argc  nargs)))

      (let ((newenv (extend-env env vars (complex-bindings (lambda:body f) vars))))
	(box-vars g (car newenv))
	;; set initial stack pointer
	(aset! g 4 (+ (length vars) 4))
	;; compile body and return
	(compile-in g newenv #t (lambda:body f))
	(emit g 'ret)
	(values (function (encode-byte-code (bcode:code g))
			  (const-to-idx-vec g) name)
		(bcode:cenv g))))))

;; disassembler

#;(define (ref-int32-LE a i)
  (int32 (+ (ash (aref a (+ i 0)) 0)
	    (ash (aref a (+ i 1)) 8)
	    (ash (aref a (+ i 2)) 16)
	    (ash (aref a (+ i 3)) 24))))

#;(define (ref-int16-LE a i)
  (int16 (+ (ash (aref a (+ i 0)) 0)
	    (ash (aref a (+ i 1)) 8))))

#;(define (hex5 n)
  (string.lpad (number->string n 16) 5 #\0))

#;(define (disassemble f . lev?)
  (if (null? lev?)
      (begin (disassemble f 0)
	     (newline)
	     (return #t)))
  (let ((lev (car lev?))
	(code (function:code f))
	(vals (function:vals f)))
    (define (print-val v)
      (if (and (function? v) (not (builtin? v)))
	  (begin (princ "\n")
		 (disassemble v (+ lev 1)))
	  (print v)))
    (dotimes (xx lev) (princ "\t"))
    (princ "maxstack " (ref-int32-LE code 0) "\n")
    (let ((i 4)
	  (N (length code)))
      (while (< i N)
	     ; find key whose value matches the current byte
	     (let ((inst (table.foldl (lambda (k v z)
					(or z (and (eq? v (aref code i))
						   k)))
				      #f Instructions)))
	       (if (> i 4) (newline))
	       (dotimes (xx lev) (princ "\t"))
	       (princ (hex5 (- i 4)) ":  "
		      (string inst) "\t")
	       (set! i (+ i 1))
	       (case inst
		 ((loadv.l loadg.l setg.l)
		  (print-val (aref vals (ref-int32-LE code i)))
		  (set! i (+ i 4)))

		 ((loadv loadg setg)
		  (print-val (aref vals (aref code i)))
		  (set! i (+ i 1)))

		 ((loada seta loadc call tcall list + - * / vector
		   argc vargc loadi8 apply tapply closure box shift)
		  (princ (number->string (aref code i)))
		  (set! i (+ i 1)))

		 ((loada.l seta.l loadc.l largc lvargc call.l tcall.l box.l)
		  (princ (number->string (ref-int32-LE code i)))
		  (set! i (+ i 4)))

		 ((optargs keyargs)
		  (princ (number->string (ref-int32-LE code i)) " ")
		  (set! i (+ i 4))
		  (princ (number->string (ref-int32-LE code i)))
		  (set! i (+ i 4))
		  (if (eq? inst 'keyargs)
		      (begin
			(princ " ")
			(princ (number->string (ref-int32-LE code i)) " ")
			(set! i (+ i 4)))))

		 ((brbound)
		  (princ (number->string (ref-int32-LE code i)) " ")
		  (set! i (+ i 4)))

		 ((jmp brf brt brne brnn brn)
		  (princ "@" (hex5 (+ i -4 (ref-int16-LE code i))))
		  (set! i (+ i 2)))

		 ((jmp.l brf.l brt.l brne.l brnn.l brn.l)
		  (princ "@" (hex5 (+ i -4 (ref-int32-LE code i))))
		  (set! i (+ i 4)))

		 (else #f)))))))

; From SRFI 89 by Marc Feeley (http://srfi.schemers.org/srfi-89/srfi-89.html)
; Copyright (C) Marc Feeley 2006. All Rights Reserved.
;
; "alist" is a list of pairs of the form "(keyword . value)"
; The result is a perfect hash-table represented as a vector of
; length 2*N, where N is the hash modulus.  If the keyword K is in
; the hash-table it is at index
;
;   X = (* 2 ($hash-keyword K N))
;
; and the associated value is at index X+1.
(define (make-perfect-hash-table alist)
  (define ($hash-keyword key n) (mod0 (abs (hash key)) n))
  (let loop1 ((n (length alist)))
    (let ((v (vector.alloc (* 2 n) #f)))
      (let loop2 ((lst alist))
        (if (pair? lst)
            (let ((key (caar lst)))
              (let ((x (* 2 ($hash-keyword key n))))
                (if (aref v x)
                    (loop1 (+ n 1))
                    (begin
                      (aset! v x key)
                      (aset! v (+ x 1) (cdar lst))
                      (loop2 (cdr lst))))))
            v)))))

#t
