#|
TODO:
* local variable identification pass
- varargs and keywords
- apply, splat
- type builder like maketype(...)
- more builtin functions (shifts, bitwise ops, conversions, primitive i/o)
- quote, expr, and symbol types, user-level macros
- modules
- more type checks
- try/catch
|#

(define julia-types (make-table))
(define julia-globals (make-table))

; --- tuples ---

; note: every julia value is represented as a vector whose first
;       element is the type of the value

(define (julia-tuple . args) (apply vector 'tuple args))

(define (tuple->list t) (cdr (vector->list t)))
(define (list->tuple l) (apply julia-tuple l))
(define (tuples->alist t)
  (map tuple->list (tuple->list t)))
(define (alist->tuples l)
  (list->tuple (map list->tuple l)))

(define (tuple-append t1 t2)
  (list->tuple (append (tuple->list t1)
		       (tuple->list t2))))

(define (tuple-ref t i) (vector-ref t (+ i 1)))
(define (tuple-length t) (- (vector-length t) 1))

(define (tuple? x) (and (vector? x)
			(or (eq? (vector-ref x 0) 'tuple)
			    (eq? (type-name (type-of x)) 'tuple))))

; --- singleton null value ---

(define julia-null (julia-tuple))

; --- type objects, reflection ---

; in any-type and Type-type we take pains to avoid circular references
; because gambit can't deal with them well, so there are symbols where
; type objects should go.
(define any-type (vector 'Type 'any 'any julia-null julia-null #t))

(define Type-type (vector 'Type 'Type any-type julia-null
			  (julia-tuple
			   (julia-tuple 'name 'symbol)
			   (julia-tuple 'super 'Type)
			   (julia-tuple 'parameters 'tuple)
			   (julia-tuple 'fields 'tuple)
			   (julia-tuple 'abstract 'boolean))
			  #f))

(define (make-type name super params fields)
  (vector Type-type name super params fields #f))

(define (make-abstract-type name super params fields)
  (vector Type-type name super params fields #t))

(define (type-name t) (vector-ref t 1))
(define (type-super t)
  (if (eq? t any-type)
      any-type
      (vector-ref t 2)))
(define (type-params t) (vector-ref t 3))
(define (type-params-list t) (tuple->list (type-params t)))
(define (type-fields t) (vector-ref t 4))

(define (make-tuple-type typelist)
  (make-type 'tuple tuple-type (list->tuple typelist) julia-null))

; get the type of a value
(define (type-of v)
  (cond ((eq? v Type-type)
	 Type-type)
	((eq? v any-type)
	 Type-type)
	((eq? v julia-null)
	 empty-tuple-type) ; again to avoid circular references
	; for now, allow scheme symbols to act as julia symbols
	((symbol? v)
	 symbol-type)
	((string? v)  ; temporary
	 any-type)
	((eq? (vector-ref v 0) 'tuple)
	 (let ((tt (make-tuple-type (map type-of (tuple->list v)))))
	   (vector-set! v 0 tt)
	   tt))
	(else
	 (vector-ref v 0))))

(define (type? v) (and (vector? v) (eq? (type-of v) Type-type)))

; --- define primitive types ---

(define tuple-type (make-abstract-type 'Tuple any-type julia-null julia-null))
(table-set! julia-types 'Tuple tuple-type)

(define sequence-type (make-abstract-type '... any-type (julia-tuple 'T)
					  julia-null))
(table-set! julia-types '... sequence-type)

(define empty-tuple-type
  (make-type 'tuple tuple-type julia-null julia-null))

(define bottom-type (make-abstract-type 'bottom '* julia-null julia-null))
(table-set! julia-types 'bottom bottom-type)

(define Tensor-type (make-abstract-type 'Tensor any-type
					(julia-tuple 'T 'n)
					julia-null))
(table-set! julia-types 'Tensor Tensor-type)

; --- type functions ---

(define (instantiate-type type params)
  (define (copy-type type super params fields)
    (if (vector-ref type 5)
	(make-abstract-type (type-name type)
			    super
			    params fields)
	(make-type (type-name type)
		   super
		   params fields)))
  
  ; create parameter list for the instantiated type
  ; tp - parameters of the incoming type
  ; sp - type template arguments ('params' argument)
  ; this fills in values from sp into slots in tp that are currently
  ; open (i.e. are symbols). concrete types already in tp are left
  ; in place. this allows partial specialization, for example:
  ; type foo(x,y,z,t) end
  ; typealias bar foo(int,y,int,t)
  ; bar(double,double)  fills in remaining y and t parameters
  (define (new-params-list tp sp)
    (cond ((null? tp)
	   (if (null? sp) tp
	       (error "Too many parameters for type" (type-name type))))
	  ((symbol? (car tp))
	   (if (null? sp)
	       (error "Too few parameters for type" (type-name type)))
	   (cons (car sp) (new-params-list (cdr tp) (cdr sp))))
	  (else
	   (new-params-list (cdr tp) sp))))
  
  (let* ((tp  (type-params-list type))
	 (ts  (filter symbol? tp)))
    (cond ((null? params)
	   type)
	  (else
	   (let* ((flds (tuples->alist (type-fields type)))
		  (env  (map cons ts params))
		  ; the following function fills in type parameters
		  ; for field types and our super type, by supplying
		  ; values that correspond to parameters with the same
		  ; names as ours.
		  (instantiate-inner-type
		   (lambda (t)
		     (instantiate-type
		      t
		      (map (lambda (p)
			     (let ((u (assq p env)))
			       (if u (cdr u) p)))
			   (filter symbol?
				   (type-params-list t)))))))
	     (copy-type type
			(instantiate-inner-type (type-super type))
		      
			(list->tuple (new-params-list tp params))
		      
			(let ((fnames (map car flds))
			      (ftypes (map cadr flds)))
			  ; replace type parameters with their values
			  ; in the types of all fields
			  (let ((mapped-ftypes
				 (map instantiate-inner-type
				      ftypes)))
			    (alist->tuples
			     (map list fnames mapped-ftypes))))))))))

(define (type-generic? t)
  (and (type? t)
       (any symbol? (type-params-list t))))

(define (type-abstract? t)
  (or (vector-ref t 5)
      (type-generic? t)))

(define (sequence-type? t)
  (and (type? t)
       (eq? (type-name t) '...)))

(define (type-param0 t)
  (tuple-ref (type-params t) 0))

(define (type-equal? a b)
  (and (subtype? a b)
       (subtype? b a)))

; tells whether child is assignable to parent
; returns #t, #f, or an assoc list witness showing the assignment of
; type parameters that makes the relation hold
(define (subtype? child parent)
  (cond ; see if the answer is obvious
        ((eq? child parent)       #t)
	((eq? parent any-type)    #t)
	((eq? child any-type)     #f)
	((eq? child bottom-type)  #t)
	((eq? parent bottom-type) #f)

	; see if the child is an instantiation of the parent
	((eq? parent (table-ref julia-types (type-name child) (list)))
	 (map cons (type-params-list parent) (type-params-list child)))

	; recursively handle union types
	((eq? (type-name child) 'union)
	 (every (lambda (t) (subtype? t parent))
		(type-params-list child)))
	((eq? (type-name parent) 'union)
	 (any   (lambda (t) (subtype? child t))
	        (type-params-list parent)))

	; handle tuple types, or any sibling instantiations of the same
	; generic type. parameters must be consistent.
        ; examples:
        ; (a, a)  subtype  (b, c)  YES
	; (a, b)  subtype  (c, c)  NO
        ; (int8, int8)  subtype  (integer, integer)  YES
        ; (a, a)  subtype  (int8, int8)  NO
	((eq? (type-name child)
	      (type-name parent))
	 (let loop ((cp (type-params-list child))  ; child parameters
		    (pp (type-params-list parent)) ; parent parameters
		    (env '()))
	   (let ((seq (and (pair? pp) (sequence-type? (car pp)))))
	     (cond
	      ((null? cp) (if (or (null? pp)
				  seq)
			      (if (null? env) #t env)
			      #f))
	      ((null? pp) #f)
	      (else
	       (let ((cp0 (if (sequence-type? (car cp))
			      (type-param0 (car cp))
			      (car cp)))
		     (pp0 (if seq
			      (type-param0 (car pp))
			      (car pp)))
		     (crest (if (sequence-type? (car cp))
				cp
				(cdr cp)))
		     (prest (if seq
				pp
				(cdr pp))))
		 (let ((continue
			(lambda (env)
			  (if (and seq
				   (sequence-type? (car cp)))
			      ; both ended up on sequence types, and
			      ; parameter matched. stop with "yes" now,
			      ; otherwise we'd start looping forever
			      env
			      (loop crest prest env)))))
		   (cond ((symbol? pp0)
			  (let ((u (assq pp0 env)))
			    (if u
				(and (or (eq? (cdr u) cp0)
					 (and (not (symbol? cp0))
					      (type-equal? cp0 (cdr u))))
				     env)
				(continue (cons (cons pp0 cp0) env)))))
			 ((symbol? cp0) #f)
			 ((or (number? cp0)
			      (number? pp0))
			  (if (eqv? cp0 pp0)
			      (continue env)
			      #f))
			 ((subtype? cp0 pp0) =>
			  (lambda (w)
			    (continue (if (pair? w)
					  (append w env)
					  env))))
			 (else #f)))))))))
	   
	; otherwise walk up the type hierarchy
	(else (subtype? (type-super child) parent))))

; --- define some key builtin types ---

(define scalar-type (make-abstract-type
		     'scalar
		     (instantiate-type Tensor-type
				       (list bottom-type 0))
		     julia-null julia-null))
(table-set! julia-types 'scalar scalar-type)

(define real-type (make-abstract-type 'real scalar-type julia-null julia-null))
(table-set! julia-types 'real real-type)
(define int-type (make-abstract-type 'int real-type julia-null julia-null))
(table-set! julia-types 'int int-type)

(define boolean-type (make-type 'boolean scalar-type julia-null julia-null))
(define int8-type (make-type 'int8 int-type julia-null julia-null))
(define uint8-type (make-type 'uint8 int-type julia-null julia-null))
(define int16-type (make-type 'int16 int-type julia-null julia-null))
(define uint16-type (make-type 'uint16 int-type julia-null julia-null))
(define int32-type (make-type 'int32 int-type julia-null julia-null))
(define uint32-type (make-type 'uint32 int-type julia-null julia-null))
(define int64-type (make-type 'int64 int-type julia-null julia-null))
(define uint64-type (make-type 'uint64 int-type julia-null julia-null))
(define size-type (make-type 'size int-type julia-null julia-null))
(define float-type (make-type 'float real-type julia-null julia-null))
(define double-type (make-type 'double real-type julia-null julia-null))

(define symbol-type (make-type 'symbol any-type julia-null julia-null))
(define buffer-type (make-type 'buffer any-type (julia-tuple 'T)
			       (julia-tuple
				(julia-tuple 'length size-type))))

(table-set! julia-types 'any any-type)
(table-set! julia-types 'boolean boolean-type)
(table-set! julia-types 'int8 int8-type)
(table-set! julia-types 'uint8 uint8-type)
(table-set! julia-types 'int16 int16-type)
(table-set! julia-types 'uint16 uint16-type)
(table-set! julia-types 'int32 int32-type)
(table-set! julia-types 'uint32 uint32-type)
(table-set! julia-types 'int64 int64-type)
(table-set! julia-types 'uint64 uint64-type)
(table-set! julia-types 'float float-type)
(table-set! julia-types 'double double-type)
(table-set! julia-types 'size size-type)
(table-set! julia-types 'Type Type-type)

(table-set! julia-types 'symbol symbol-type)
(table-set! julia-types 'buffer buffer-type)

; --- singleton true and false values ---

(define julia-true (vector boolean-type 1))
(define julia-false (vector boolean-type 0))

; --- processing type-related syntax ---

(define (type-ex-name t)
  (cond ((not (pair? t)) t)
	((eq? (car t) 'tuple) 'tuple)
	((eq? (car t) '...)   '...)
	((eq? (car t) 'call)  (cadr t))
	(else (error "Invalid type expression" t))))
(define (type-ex-params t)
  (cond ((not (pair? t)) '())
	((eq? (car t) 'tuple) (cdr t))
	((eq? (car t) '...)   (cdr t))
	((eq? (car t) 'call)  (cddr t))
	(else (error "Invalid type expression" t))))

; handles form (typename name . params), giving a type object
(define (resolve-type name params . nested?)
  (define (resolve-params p)
    (map (lambda (t)
	   (resolve-type (type-ex-name t)
			 (type-ex-params t) #t))
	 p))
  (cond ((eq? name 'tuple)
	 (make-type name tuple-type
		    (list->tuple (resolve-params params))
		    julia-null))
	((eq? name 'union)
	 (make-abstract-type name any-type
			     (list->tuple (resolve-params params))
			     julia-null))
	#;((assq name type-env) => (lambda (x)
				   (instantiate-type (cdr x) params)))
	((table-ref julia-types name #f) => (lambda (x)
					      (instantiate-type
					       x
					       (resolve-params params))))
	((pair? nested?) name)
	(else
	 (error "Unknown type" name))))

(define (resolve-type-ex e)
  (resolve-type (type-ex-name e)
		(type-ex-params e)))

; look for type < supertype
(define (type-def signature fields)
  (let ((parented?  (and (pair? signature) (eq? (car signature) 'call)
			 (pair? (cdr signature)) (eq? (cadr signature) '<))))
    (let ((sig (if parented?
		   (caddr signature)
		   signature))
	  (super (if parented?
		     (resolve-type-ex (cadddr signature))
		     any-type)))
      (type-def- sig fields super))))

(define (type-def- signature fields super)
  (let ((tname (type-ex-name signature))
	(tpara (type-ex-params signature))
	; fields looks like (block (: n t) (: n t) ...)
	(fnames (map cadr (cdr fields)))
	(ftypes (map (lambda (fld)
		       (resolve-type-ex (caddr fld)))
		     (cdr fields))))
    (let ((T
	   (make-type tname super (list->tuple tpara)
		      ; TODO: check for duplicate fields
		      (tuple-append
		       (type-fields super)
		       (alist->tuples
			(map list fnames ftypes))))))
      (table-set! julia-types tname T)
      T)))

(define (type-alias name type)
  (if (not (symbol? name))
      (error "typealias: type name must be a symbol"))
  (table-set! julia-types name (resolve-type-ex type)))

; --- function objects ---

(define (make-closure formals body env)
  (vector 'closure formals body env))

(define (j-closure? x) (and (vector? x)
			    (eq? (vector-ref x 0) 'closure)))

(define (make-generic-function name)
  (vector 'generic-function '() name))

(define (generic-function? x) (and (vector? x)
				   (eq? (vector-ref x 0) 'generic-function)))

(define (best-method gf argtype)
  (let loop ((m (vector-ref gf 1)))
    (if (null? m)
	#f
	(if (subtype? argtype (caar m))
	    (car m)
	    (loop (cdr m))))))

(define (cons-in-order item lst key <)
  (if (null? lst)
      (list item)
      (if (< (key item) (key (car lst)))
	  (cons item lst)
	  (cons (car lst) (cons-in-order item (cdr lst) key <)))))

(define (add-method gf argtype m)
  (let ((matches (filter (lambda (x)
			   (type-equal? (car x) argtype))
			 (vector-ref gf 1))))
    (if (null? matches)
	(vector-set! gf 1
		     (cons-in-order (cons argtype m) (vector-ref gf 1)
				    car subtype?))
	(if (null? (cdr matches))
	  ; overwrite existing exactly matching method
	  (begin (set-car! (car matches) argtype)
		 (set-cdr! (car matches) m))
	  (error "Function had multiple methods with the same type"))))
  #t)

; --- builtin functions ---

(define (field-offset obj fld)
  (let ((fl (type-fields (type-of obj))))
    (let loop ((i 0)
	       (L (tuple-length fl)))
      (if (< i L)
	  (if (eq? fld (tuple-ref (tuple-ref fl i) 0))
	      (+ i 1)
	      (loop (+ i 1) L))
	  (error "Type" (type-name (type-of obj)) "has no field" fld)))))

(define (to-symbol x)
  (if (symbol? x) x
      (if (not (eq? (type-of x) symbol-type))
	  (error "Expected symbol")
	  (vector-ref x 1))))

(define (j-get-field obj fld)
  (vector-ref obj (field-offset obj (to-symbol fld))))

(define (j-set-field obj fld v)
  ; TODO: type check
  (vector-set! obj (field-offset obj (to-symbol fld)) v))

(define (j-tupleref v i) (if (eq? i 0) (raise e) (vector-ref v i)))

(define (j-buffer-length v) (j-unbox (j-get-field v 'length)))

(define (buffer-data v) (vector-ref v 2))

(define (j-buffer-ref v i)
  (vector-ref (buffer-data v) (- i 1)))

(define (j-buffer-set v i rhs)
  ; TODO: type check/convert
  (vector-set! (buffer-data v) (- i 1) (j-unbox rhs)))

(define (j-not x) (if (eq? x julia-false) julia-true julia-false))

(define (j-new type args)
  (if (type-abstract? type)
      (error "Cannot instantiate abstract type" (type-name type)))
  (let* ((tn (type-name type))
	 (L (if (eq? tn 'tuple)
		(length args)
		(if (eq? tn 'buffer)
		    3
		    (tuple-length (type-fields type)))))
	 (v (make-vector (+ 1 L))))
    (vector-set! v 0 type)
    (if (eq? tn 'buffer)
	(begin (j-set-field v 'length (car args))
	       (vector-set! v 2 (make-vector (j-unbox (car args)) 0))
	       v)
	(let loop ((i 0)
		   (args args))
	  (if (< i L)
	      (if (null? args)
		  (error "Too few arguments to type constructor" tn)
		  (begin (vector-set! v (+ 1 i) (car args))
			 (loop (+ i 1) (cdr args))))
	      (if (pair? args)
		  (error "Too many arguments to type constructor" tn)
		  v))))))

(define (j-typeof x) (type-of x))

(define (j-is x y) (eq? x y))

(define (make-builtin name impl)
  (table-set! julia-globals name impl))

(define (j-box type v) (vector type v))
(define (j-unbox v) (vector-ref v 1))

(make-builtin 'getfield j-get-field)
(make-builtin 'setfield j-set-field)
(make-builtin 'bufferref j-buffer-ref)
(make-builtin 'tupleref j-tupleref)
(make-builtin 'tuplelen tuple-length)
(make-builtin 'bufferset j-buffer-set)
(make-builtin 'not j-not)
(make-builtin 'typeof j-typeof)
(make-builtin 'is j-is)
(make-builtin 'tuple julia-tuple)
(make-builtin 'box j-box)
(make-builtin 'unbox j-unbox)
(make-builtin 'istype (lambda (x y) (if (subtype? (type-of x) y)
					julia-true julia-false)))

(make-builtin 'add_int32 +)
(make-builtin 'add_double +)
(make-builtin 'sub_int32 -)
(make-builtin 'sub_double -)
(make-builtin 'neg_int32 -)
(make-builtin 'neg_double -)
(make-builtin 'mul_int32 *)
(make-builtin 'mul_double *)
(define (div-int32 x y)
  (let ((q (/ x y)))
    (if (< q 0)
	(ceiling q)
	(floor q))))
(make-builtin 'div_int32 div-int32)
(make-builtin 'mod_int32 (lambda (x y)
			   (- x (* (div-int32 x y) y))))
(make-builtin 'div_double (lambda (x y) (exact->inexact (/ x y))))
(make-builtin 'eq_int32 (lambda (x y) (if (= x y)
					  julia-true
					  julia-false)))
(make-builtin 'eq_double (lambda (x y) (if (= x y)
					  julia-true
					  julia-false)))
(make-builtin 'lt_int32 (lambda (x y) (if (< x y)
					  julia-true
					  julia-false)))
(make-builtin 'lt_double (lambda (x y) (if (< x y)
					   julia-true
					   julia-false)))
(make-builtin 'ne_int32 (lambda (x y) (if (not (= x y))
					  julia-true
					  julia-false)))
(make-builtin 'ne_double (lambda (x y) (if (and (= x x)
						(= y y)
						(not (= x y)))
					   julia-true
					   julia-false)))
(make-builtin 'isnan (lambda (x) (if (not (= x x))
				     julia-true
				     julia-false)))
(make-builtin 'isinf (lambda (x) (if (or (equal? x +inf.0)
					 (equal? x -inf.0))
				     julia-true
				     julia-false)))


; --- evaluator ---

(define (scm->julia x)
  (if (symbol? x)
      (vector symbol-type x)
      x))

(define (eval-sym s env)
  (let ((a (assq s env)))
    (if a (cdr a)
	(or (table-ref julia-globals s #f)
	    (error "Undefined variable" s)))))

(define (j-bound? s env)
  (or (assq s env) (table-ref julia-globals s #f)))

(define (j-eval e env)
  (cond ((number? e)
	 (if (integer? e)
	     (j-box int32-type e)
	     (j-box double-type e)))
	((eq? e 'false) julia-false)
	((eq? e 'true) julia-true)
	((symbol? e)
	 (eval-sym e env))
	((string? e)
	 e)
	((not (pair? e))
	 (error "I don't know how to handle" e))
	(else
	 (case (car e)
	   ((quote)   (scm->julia (cadr e)))
	   ((if)      (let ((c (j-eval (cadr e) env)))
			(if (not (eq? c julia-false))
			    (j-eval (caddr e) env)
			    (if (pair? (cdddr e))
				(j-eval (cadddr e) env)
				julia-false))))
	   ((_while)   (let loop ()
			 (if (not (eq? (j-eval (cadr e) env) julia-false))
			     (begin (j-eval (caddr e) env)
				    (loop)))
			 julia-null))
	   ((return)  (raise `(julia-return . ,(if (pair? (cdr e))
						   (j-eval (cadr e) env)
						   julia-null))))
	   ((&&)      (let loop ((clauses (cdr e)))
			(cond ((null? clauses) julia-true)
			      ((null? (cdr clauses)) (j-eval (car clauses) env))
			      ((not (eq? (j-eval (car clauses) env)
					 julia-false))
			       (loop (cdr clauses)))
			      (else julia-false))))
	   ((|\|\||)  (let loop ((clauses (cdr e)))
			(cond ((null? clauses) julia-false)
			      ((null? (cdr clauses)) (j-eval (car clauses) env))
			      (else (let ((v (j-eval (car clauses) env)))
				      (if (eq? v julia-false)
					  (loop (cdr clauses))
					  v))))))
	   ((block)   (let loop ((v julia-null)
				 (x (cdr e)))
			(if (null? x)
			    v
			    (if (null? (cdr x))
				(j-eval (car x) env)
				(loop (j-eval (car x) env) (cdr x))))))
	   ((break-block)  (let ((bname (cadr e)))
			     (with-exception-catcher
			      (lambda (e)
				(if (and (pair? e)
					 (eq? (car e) bname))
				    julia-null
				    (raise e)))
			      (lambda ()
				(j-eval (caddr e) env)))))
	   ((scope-block)
	    (let loop ((v julia-null)
		       (x (cdr e)))
	      (if (null? x)
		  v
		  (cond ((null? (cdr x))
			 (j-eval (car x) env))
			((and (pair? (car x))
			      (eq? (caar x) 'local))
			 (set! env (cons (cons (cadr (car x)) julia-null) env))
			 (loop v (cdr x)))
			(else
			 (loop (j-eval (car x) env) (cdr x)))))))

	   ((break)  (raise (cdr e)))
	   ((time)   (time (j-eval (cadr e) env)))

	   ((type)
	    (type-def (cadr e) (caddr e)))
	   ((typename)
	    (resolve-type-ex (cadr e)))
	   ((typealias)
	    (type-alias (cadr e) (caddr e)))
	   ((tuple)
	    (if (null? (cdr e)) julia-null
		(apply julia-tuple (map (lambda (x) (j-eval x env))
					(cdr e)))))
	   ((new)
	    (j-new (resolve-type-ex (cadr e))
		   (map (lambda (x) (j-eval x env))
			(cddr e))))
	   ((lambda)
	    (make-closure (cadr e) (caddr e) env))
	   ((addmethod)
	    (let* ((name (cadr e))
		   (gf (if (j-bound? name env)
			   (eval-sym name env)
			   (make-generic-function name))))
	      (if (not (generic-function? gf))
		  (error "Variable" name "does not name a function")
		  (add-method gf
			      (make-tuple-type
			       (map resolve-type-ex (caddr e)))
			      (j-eval (cadddr e) env)))
	      gf))

	   ((local)  julia-null)
	   ((=)
	    (if (not (symbol? (cadr e)))
		(error "Invalid lvalue in ="))
	    (let ((v (j-eval (caddr e) env))
		  (a (assq (cadr e) env)))
	      (if a (set-cdr! a v)
		  (table-set! julia-globals (cadr e) v))
	      v))

	   ((call)
	    (let ((f (j-eval (cadr e) env))
		  (args (map (lambda (x) (j-eval x env))
			     (cddr e))))
	      (cond ((procedure? f)  (apply f args))
		    ((and (vector? f)
			  (eq? (vector-ref f 0) 'closure))
		     (j-apply-closure f args))
		    (else
		     ;(assert (generic-function? f))
		     (j-apply-generic f args)))))
	   (else
	    (error "Unhandled tree type" (car e)))))))

; create an environment with actual args bound to formal args
(define (bind-args names args cloenv)
  (if (null? names)
      cloenv
      (let ((formal (car names)))
	(let ((bind
	       (if (and (pair? formal) (eq? (car formal) '...))
		   (cons (cadr formal)
			 (apply julia-tuple args))
		   (cons formal
			 (car args)))))
	  (cons bind
		(bind-args (cdr names) (cdr args) cloenv))))))

(define (j-apply-closure f args)
  (with-exception-catcher
   (lambda (e)
     (if (and (pair? e)
	      (eq? (car e) 'julia-return))
	 (cdr e)
	 (raise e)))
   (lambda ()
     (j-eval (vector-ref f 2)
	     (bind-args (vector-ref f 1) args (vector-ref f 3))))))

(define (j-apply-generic gf args)
  (let* ((argtype (make-tuple-type (map type-of args)))
	 (meth  (best-method gf argtype)))
    (if (not meth)
	(error "No method for function" (vector-ref gf 2)
	       "matching types" (map type-name (type-params-list argtype)))
	(j-apply-closure (cdr meth) args))))

(define (j-toplevel-eval e)
  (j-eval (julia-expand e) '()))

; --- load ---

(define (j-load fname)
  (for-each j-toplevel-eval (julia-parse-file fname)))

(make-builtin 'load j-load)

; --- print and repl ---

(define (print-tuple x)
  (display "(")
  (let loop ((L (tuple-length x))
	     (i 0))
    (if (< i L)
	(begin (julia-print (tuple-ref x i))
	       (if (< i (- L 1))
		   (display ", ")
		   (if (= L 1)
		       (display ",")))
	       (loop L (+ i 1)))
	(display ")"))))

(define (julia-print x)
  (cond
   ((not (vector? x))  (display x))
   ((eq? (vector-ref x 0) 'closure)
    (display x))
   ((eq? (vector-ref x 0) 'generic-function)
    (display "#<generic-function ")
    (display (vector-ref x 2))
    (display ">"))
   ((number? (vector-ref x 0))
    (display "#<primitive-buffer ")
    (display x)
    (display ">"))    
   ((or (eq? (vector-ref x 0) 'tuple)
	(eq? (type-name (type-of x)) 'tuple))
    (print-tuple x))
   ((eq? (type-name (type-of x)) 'buffer)
    (display "buffer(")
    (display (type-name (tuple-ref (type-params (type-of x)) 0)))
    (display "):")
    (display (buffer-data x)))
   (else
    (let* ((t (type-of x))
	   (tn (type-name t)))
      (case tn
	((int32 double) (display (j-unbox x)))
	((boolean) (if (eq? x julia-false)
		       (display "false")
		       (display "true")))
	((symbol) (display (vector-ref x 1)))
	(else
	 (let ((fields (type-fields t))
	       (vals (cdr (vector->list x))))
	   (display tn)
	   (display "(")
	   (let loop ((L (tuple-length x))
		      (i 0))
	     (if (< i L)
		 (begin (display (tuple-ref (tuple-ref fields i) 0))
			(display ":")
			(julia-print (vector-ref x (+ i 1)))
			(if (< i (- L 1))
			    (display ", "))
			(loop L (+ i 1)))
		 (display ")"))))))))))

(make-builtin '_print julia-print)

(define (julia-repl)
  (j-load "start.j")
  (j-load "array.j")
  (j-load "examples.j")

  (display "julia> ")
  (let ((line (read-line)))
    (if (eof-object? line)
	(newline)
	(begin 
	  (with-exception-catcher
	   (lambda (e)
	     ;(raise e)
	     (display (error-exception-message e))
	     (for-each (lambda (x)
			 (display " ") (display x))
		       (error-exception-parameters e))
	     (newline)
	     (newline)
	     (julia-repl))
	   (lambda ()
	     (j-toplevel-eval
	      `(call print (quote ,(j-toplevel-eval (julia-parse line)))))
	     (newline)
	     (newline)
	     (julia-repl)))))))
