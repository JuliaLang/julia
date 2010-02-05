#|
TODO:
* local variable identification pass
* varargs
* apply, splat
* builtin scalar conversions, implicit conversion mechanism
- modules
- separate type for literals
- global var declaration
- optional arguments
- quote, expr, and symbol types, user-level macros
- where clauses
- more builtin functions (shifts, bitwise ops, primitive i/o)
- keyword arguments
- try/catch
|#

(define julia-globals (make-table))

; --- tuples ---

; note: every julia value is represented as a vector whose first
;       element is the type of the value

(define (julia-tuple . args) args)

(define (tuple->list t) t)
(define (list->tuple l) l)
(define (tuples->alist t) t)
(define (alist->tuples l) l)

(define (tuple-append t1 t2) (append t1 t2))

(define (tuple-ref t i) (list-ref t i))
(define (tuple-set! t i x) (set-car! (list-tail t i) x))
(define (tuple-length t) (length t))

(define (tuple? x) (or (pair? x) (null? x)))

(define (make-tuple-type typelist) (list->tuple typelist))

; --- singleton null value ---

(define julia-null (julia-tuple))
(define (j-null? x) (null? x))

; --- type objects, reflection ---

(define (put-type name t) (table-set! julia-globals name t))

(define TagKind (vector 'StructKind
			'TagType 'Type-type julia-null
			(julia-tuple 'name 'super 'parameters)
			#f;(julia-tuple Symbol-type Type-type Tuple-type)
			#f #f))

(define (make-tag-type name super parameters)
  (vector TagKind name super parameters))

(define (tag-type-name t)   (vector-ref t 1))
(define (tag-type-super t)  (vector-ref t 2))
(define (tag-type-params t) (vector-ref t 3))

(define Any-type (make-tag-type 'Any 'Any julia-null))
(define Type-type (make-tag-type 'Type Any-type julia-null))
(define Tuple-type (make-tag-type 'Tuple Any-type julia-null))
(define Symbol-type (make-tag-type 'Symbol Any-type julia-null))

(vector-set! TagKind 2 Type-type)
(vector-set! TagKind 5 (julia-tuple Symbol-type Type-type Tuple-type))

(define FunctionKind (vector 'StructKind
			     'Function Type-type julia-null
			     (julia-tuple 'from 'to)
			     (julia-tuple Type-type Type-type)
			     #f #f))

(define (type-var? a) #f) ; forward decl

(define (make-function-type a b)
  ; convert X-->Y to (X,)-->Y if X is not already a tuple or typevar
  (let ((a (if (and (not (tuple? a))
		    (not (type-var? a)))
	       (julia-tuple a)
	       a)))
    (vector FunctionKind a b)))

(define (func-type? t) (and (vector? t) (eq? (vector-ref t 0) FunctionKind)))
(define function-type? func-type?)

(define (func-fromtype t) (vector-ref t 1))
(define (func-totype   t) (vector-ref t 2))

(define any-func (make-function-type Any-type Any-type))

(define (make-closure proc env)
  (vector any-func proc env))

(define StructKind (vector 'StructKind
			   'StructType TagKind julia-null
			   (julia-tuple 'name 'super 'parameters
					'names 'types 'new 'convert)
			   (julia-tuple Symbol-type Type-type Tuple-type
					Tuple-type Tuple-type
					any-func any-func)
			   #f #f))

(define (make-generic-function name) julia-null) ; forward decl
(define (add-method-for gf types meth) #t)       ; forward decl

(define (make-struct-type name super params fnames ftypes . ctor)
  (let* ((cnvt (make-generic-function 'convert))
	 (T (vector StructKind name super params fnames ftypes
		    #f cnvt)))
    (vector-set! T 6
		 (make-closure
		  (if (pair? ctor)
		      (car ctor)
		      (lambda (ce args)
			(apply vector ce args)))
		  T))
    ; add the identity conversion
    (add-method-for cnvt (julia-tuple T) (make-closure (lambda (ce args)
							 (car args)) #f))
    T))

(vector-set! TagKind 0 StructKind)
(vector-set! FunctionKind 0 StructKind)
(vector-set! StructKind 0 StructKind)

(define (struct-type? t) (and (vector? t) (eq? (vector-ref t 0) StructKind)))

(define (tag-type? t)
  (and (vector? t)
       (or (eq? (vector-ref t 0) TagKind)
	   (eq? (vector-ref t 0) StructKind))))

(define UnionKind (vector StructKind
			  'Union Type-type julia-null
			  (julia-tuple 'types)
			  (julia-tuple Tuple-type)
			  #f #f))

(define (all-pairs? pred lst)
  (every (lambda (first)
	   (every (lambda (second) (pred first second))
		  (filter (lambda (x) (not (eq? x first))) lst)))
	 lst))

(define (make-union-type params)
  ; avoid constructing union types that would force the conform procedure
  ; to perform an exponential search. this would happen if a union contained
  ; two types A and B such that there exists a type X that could match either,
  ; but potentially with different type parameter assignments. For example:
  ; (Union(Complex[T],Complex[S]), T)
  ; This should match (Complex[Int32], Float64), but we can only determine that
  ; by trying the rest of the pattern with both T=Int32 and S=Int32.
  ; Of course this *could* be implemented, but it's way more complex than
  ; what is needed, which is combining disjoint types like () and Int, or
  ; EmptyTree and TreeNode.
  (define (non-overlapping? a b)
    (not (and (has-typevars? a)
	      (has-typevars? b)
	      (conform a b))))
  (if (all-pairs? non-overlapping? params)
      (vector UnionKind (list->tuple params))
      (error "Type pattern too complex")))

(define (union-type? t) (and (vector? t) (eq? (vector-ref t 0) UnionKind)))

(define (union-type-types t) (vector-ref t 1))

(define Bottom-type (make-union-type '()))
(put-type 'Bottom Bottom-type)

; --- type constructors ---

(define TypeConstructor
  (make-struct-type 'TypeConstructor Any-type julia-null
		    (julia-tuple 'parameters 'body)
		    (julia-tuple Tuple-type Type-type)))
(put-type 'TypeConstructor TypeConstructor)

(define (make-type-constructor params body)
  (vector TypeConstructor params body))

(define (type-ctor? t)
  (and (vector? t) (eq? (vector-ref t 0) TypeConstructor)))

(define (type-ctor-params t) (vector-ref t 1))
(define (type-ctor-type t) (vector-ref t 2))

(define TypeVar
  (make-struct-type 'TypeVar Any-type julia-null
		    (julia-tuple 'name 'lb 'ub)
		    (julia-tuple Symbol-type Type-type Type-type)))
(put-type 'TypeVar TypeVar)

(define (make-type-var name . b)
  (vector TypeVar name
	  (if (pair? b) (car b)  Bottom-type)
	  (if (pair? b) (cadr b) Any-type)))

(set! type-var? (lambda (t)
		  (and (vector? t) (eq? (vector-ref t 0) TypeVar))))

(define (type-var-name t) (vector-ref t 1))
(define (type-var-lb   t) (vector-ref t 2))
(define (type-var-ub   t) (vector-ref t 3))

(define (type-vars l) (map (lambda (n) (make-type-var n)) l))

; "convert" a type constructor to a type pattern by passing new
; typevars for all its parameters.
(define (tc->type tc)
  (instantiate-type tc (map (lambda (tv)
			      (make-type-var (gensym)
					     (type-var-lb tv)
					     (type-var-ub tv)))
			    (type-ctor-params tc))))

; --- general type accessors and predicates ---

(define (type-name t)
  (cond ((tag-type? t)    (vector-ref t 1))
	((tuple? t)       'Tuple)
	((type-ctor? t)   (type-name (type-ctor-type t)))
	(else             (vector-ref (vector-ref t 0) 1))))
(define (type-super t)
  (cond ((tuple? t)       Tuple-type)
	((eq? t Any-type) Any-type)
	((tag-type? t)    (vector-ref t 2))
	(else             Any-type)))
(define (type-params t)
  (cond ((tuple? t)       t)
	((tag-type? t)    (vector-ref t 3))
	((union-type? t)  (vector-ref t 1))
	((func-type? t)   (list (func-fromtype t) (func-totype t)))
	(else             julia-null)))
(define (type-field-names t)
  (if (struct-type? t) (vector-ref t 4) julia-null))
(define (type-field-types t)
  (if (struct-type? t) (vector-ref t 5) julia-null))

(define (closure-proc c) (vector-ref c 1))
(define (closure-env c)  (vector-ref c 2))

(define (j-closure? x) (and (vector? x)
			    (func-type? (vector-ref x 0))))

(define (generic-function? x) (and (j-closure? x)
				   (eq? (closure-proc x) j-apply-generic)))

; get the type of a value
(define (type-of v)
  (cond ((eq? v julia-null)  julia-null)
	; for now, allow scheme symbols to act as julia symbols
	((symbol? v)             Symbol-type)
	((string? v)             Any-type)  ; temporary
	((procedure? v)   	 Any-type)
	((number? v)   	         Any-type)
	((tuple? v)              (make-tuple-type
				  (map type-of (tuple->list v))))
	(else                    (vector-ref v 0))))

(define (type? v) (or (and (tuple? v)
			   (every (lambda (x)
				    (or (type-var? x)
					(type? x))) v))
		      (and (vector? v)
			   (let ((tag (vector-ref v 0)))
			     (or (eq? tag UnionKind)
				 (eq? tag StructKind)
				 (eq? tag FunctionKind)
				 (eq? tag TagKind))))))

; --- define primitive types ---

(put-type 'Any Any-type)
(put-type 'Tuple Tuple-type)
(put-type 'Type Type-type)
(put-type 'Symbol Symbol-type)

(define sequence-type
  (let ((v (type-vars '(T))))
    (make-type-constructor v (make-tag-type '... Any-type v))))
(put-type '... sequence-type)

(define (sequence-type? t)
  (and (type? t)
       (eq? (type-name t) '...)))

(put-type 'Function
	  (let ((v (type-vars '(A B))))
	    (make-type-constructor v (apply make-function-type v))))

; --- type functions ---

(define (has-params? t)
  (not (j-null? (type-params t))))

(define (type-param0 t)
  (tuple-ref (type-params t) 0))

(define (type-param t n)
  (tuple-ref (type-params t) n))

(define (j-int32? x)
  (and (vector? x) (eq? (type-name (type-of x)) 'Int32)))

(define (instantiate-type tc params)
  (if (not (every (lambda (p)
		    (or (type? p) (type-var? p) (j-int32? p)))
		  params))
      (error "Invalid parameter for type" (type-name tc)))
  (let ((tp  (type-ctor-params tc)))
    (check-same-length
     params tp
     (lambda () (error "Too few parameters for type" (type-name tc)))
     (lambda () (error "Too many parameters for type" (type-name tc))))
    (instantiate-type- (type-ctor-type tc) (map cons tp params))))

; instantiate a type using the bindings in the given type environment
; (an assoc list from names to types)
(define (instantiate-type- type env)
  (instantiate-type-- type env '()))

(define *type-cache* '())
(define (lookup-type table key)
  (assoc-p key table
	   (lambda (x y)
	     (and (eq? (car x) (car y))
		  (andmap (lambda (u v) (type-eqv? u v))
			  (cdr x) (cdr y))))))
(define (cache-type! key type)
  (set! *type-cache* (cons (cons key type) *type-cache*)))

(define (type-eqv? a b) (type-eqv?- a b '()))

(define (type-eqv?- a b stack)
  (cond ((eq? a b) #t)
	((j-int32? a) (and (j-int32? b) (equal? (j-unbox a) (j-unbox b))))
	((member-p (cons a b) stack (lambda (x y)
				      (and (eq? (car x) (car y))
					   (eq? (cdr x) (cdr y)))))
	 #t)
	((and (type? a) (type? b))
	 (and (eq? (type-name a) (type-name b))
	      (let ((newstack
		     (cons (cons a b) stack)))
		(andmap (lambda (x y)
			  (type-eqv?- x y newstack))
			(type-params a)
			(type-params b)))))
	(else #f)))

(define (instantiate-type-- type env stack)
  (define (recur t) (instantiate-type-- t env stack))
  (cond
   ((type-var? type)    (lookup type env type))
   ((not (type? type))  type)
   ((null? env)         type)
   ((tuple? type)
    (map recur type))
   ((union-type? type)
    (make-union-type (map recur (union-type-types type))))
   ((func-type? type)
    (make-function-type (recur (func-fromtype type))
			(recur (func-totype   type))))
   ((null? (tag-type-params type)) type)
   ((tag-type? type)
    (let* ((i-params (map (lambda (t)
			    (if (eq? t type) type
				(recur t)))
			  (tag-type-params type)))
	   (key  (cons (type-name type) i-params)))
      (cond ((lookup-type stack        key) => cdr)
	    ((lookup-type *type-cache* key) => cdr)
	    ; always instantiate a type using the original type
	    ; constructor and prototype; don't copy a type that's
	    ; already been instantiated (with TypeVars)
	    ((let ((tc (table-ref julia-globals (type-name type) #f)))
	       (and tc
		    (not (eq? (type-ctor-type tc) type))
		    (instantiate-type tc i-params))))
	    ((struct-type? type)
	     (let* ((newstruct
		     (make-struct-type (tag-type-name type)
				       (recur (type-super type)) i-params
				       (type-field-names type)
				       #f
				       (closure-proc (vector-ref type 6))))
		    (newstack (cons (cons key newstruct) stack)))
	       (if (type-field-types type)
		   (vector-set!
		    newstruct 5
		    (map (lambda (ft)
			   (instantiate-type-- ft env newstack))
			 (type-field-types type))))
	       (if (generic-function? (vector-ref type 7))
		   (vector-set!
		    newstruct 7
		    (instantiate-generic-function (vector-ref type 7) env
						  newstack)))
	       (cache-type! key newstruct)
	       newstruct))
	    (else (make-tag-type (tag-type-name type)
				 (recur (type-super type)) i-params)))))))

(define (type-equal? a b)
  (and (subtype? a b)
       (subtype? b a)))

(define (tuple-elementwise? pred child parent)
  (let loop ((cp (type-params child))   ; child parameters
	     (pp (type-params parent))) ; parent parameters
    (let ((cseq (and (pair? cp) (sequence-type? (car cp))))
	  (pseq (and (pair? pp) (sequence-type? (car pp)))))
      (cond
       ((null? cp)            (or (null? pp) pseq))
       ((and cseq (not pseq)) #f)
       ((null? pp)            #f)
       (else
	(and (pred (if cseq
		       (type-param0 (car cp))
		       (car cp))
		   (if pseq
		       (type-param0 (car pp))
		       (car pp)))
	     ; if both end up on sequence types, and
	     ; parameter matched. stop with "yes" now,
	     ; otherwise we'd start looping forever
	     (or (and pseq cseq)
		 (loop (if cseq cp (cdr cp))
		       (if pseq pp (cdr pp))))))))))

(define (tuple-subtype? child parent)
  (tuple-elementwise? subtype? child parent))

(define (subtype? child parent)
  (cond ((eq? child parent)       #t)
	((type-var? parent)       #t
	 #;(and (subtype? child (type-var-ub parent))
	 (subtype? (type-var-lb parent) child)))
	((type-var? child)        #f
	 #;(and (subtype? (type-var-ub child) parent)
	 (subtype? (type-var-lb child) parent)))
	((eq? parent Tuple-type)  (tuple? child))
	((eq? parent Any-type)    #t)
	((eq? child Any-type)     #f)
	
	((and (j-int32? child) (j-int32? parent))
	 (= (j-unbox child) (j-unbox parent)))
	
	; recursively handle union types
	((union-type? child)
	 (every (lambda (t) (subtype? t parent))
		(type-params child)))
	((union-type? parent)
	 (any   (lambda (t) (subtype? child t))
	        (type-params parent)))
	
	((and (tuple? child) (tuple? parent))
	 (tuple-subtype? child parent))
	
	; functions are contravariant in first parameter
	((and (func-type? child)
	      (func-type? parent))
	 (and (or (type-var? (func-fromtype parent))
		  (subtype? (func-fromtype parent)
			    (func-fromtype child)))
	      (subtype? (func-totype child)
			(func-totype parent))))
	
	; handle sibling instantiations of the same generic type.
	((eq? (type-name child)
	      (type-name parent))
	 (let loop ((cp (type-params child))   ; child parameters
		    (pp (type-params parent))) ; parent parameters
	     (cond
	      ((null? cp)  (null? pp))
	      ((null? pp)  #f)
	      (else
	       ; default to invariance
	       (and (type-equal? (car cp) (car pp))
		    (loop (cdr cp) (cdr pp)))))))
	
	; otherwise walk up the type hierarchy
	(else (subtype? (type-super child) parent))))

; tells whether a type conforms to a given type pattern (a type
; containing TypeVars)
; returns #f or an assoc list showing the assignment of
; type parameters that makes the relation hold
(define (conform t pat) (conform- t pat '()))

; generate list of corresponding type components, (type . T) if parameter
; T might correspond to type
(define (conform- child parent env)
  ;(display "conform- ")
  ;(julia-print child) (display " ")
  ;(julia-print parent) (newline)
  (cond ((type-var? parent)
	 (let ((val (lookup parent env #f)))
	   (if val
	       (and (type-eqv? child val)
		    env)
	       (cons (cons parent child) env))))
	((type-var? child) #f)
	
	((and (j-int32? child) (j-int32? parent))
	 (and (= (j-unbox child) (j-unbox parent))
	      env))
	
	((eq? child parent)       env)
	((eq? parent Any-type)    env)
	((eq? child Any-type)     #f)
	
	((union-type? child)
	 (foldl (lambda (t env)
		  (and env
		       (conform- t parent env)))
		env
		(type-params child)))
	((union-type? parent)
	 (any   (lambda (t) (conform- child t env))
	        (type-params parent)))
	
	((and (or (not (has-params? parent))
		  (not (has-params? child))))
	 (and (subtype? child parent)
	      env))
	
	; handle tuple types, or any sibling instantiations of the same
	; generic type. parameters must be consistent.
        ; examples:
        ; (a, a)  conforms  (b, c)  YES
	; (a, b)  conforms  (c, c)  NO
        ; (Int8, Int8)  conforms  (Int, Int)  YES
        ; (a, a)  conforms  (Int8, Int8)  NO
	((eq? (type-name child)
	      (type-name parent))
	 (let loop ((cp (type-params child))  ; child parameters
		    (pp (type-params parent)) ; parent parameters
		    (env env))
	   (let ((cseq (and (pair? cp) (sequence-type? (car cp))))
		 (pseq (and (pair? pp) (sequence-type? (car pp)))))
	     (cond
	      ((null? cp) (cond ((null? pp)  env)
				(pseq        env)
				(else        #f)))
	      ((and cseq (not pseq)) #f)
	      ((null? pp)            #f)
	      (else
	       ; default to covariance
	       (let ((newenv (conform- (if cseq
					   (type-param0 (car cp))
					   (car cp))
				       (if pseq
					   (type-param0 (car pp))
					   (car pp))
				       env)))
		 (and newenv
		      ; if both end up on sequence types, and
		      ; parameter matched. stop with "yes" now,
		      ; otherwise we'd start looping forever
		      (if (and pseq cseq)
			  newenv
			  (loop (if cseq cp (cdr cp))
				(if pseq pp (cdr pp))
				newenv)))))))))
	
	; otherwise walk up the type hierarchy
	(else
	 (conform- (type-super child) parent env))))

(define (has-typevars? t)
  (or (type-var? t)
      (and (type? t)
	   (not (eq? t scalar-type)) ; circular reference problem
	   (any has-typevars? (type-params t)))))

; --- function objects ---

(define (make-lambda-closure lambda-expr cloenv)
  (make-closure j-eval-body (cons lambda-expr cloenv)))

(define (lambda-closure-expr c)
  (let ((e (car (closure-env c))))
    (if (and (pair? e)
	     (eq? (car e) 'lambda))
	e
	(error "Not a lambda closure"))))

(define (lambda-closure-env c)
  (lambda-closure-expr c)  ; for the error check
  (cdr (closure-env c)))

(define (lambda-closure? x)
  (let ((e (closure-env x)))
    (and (pair? e)
	 (pair? (car e))
	 (eq? (caar e) 'lambda))))

; --- type-keyed hash table ---

(define (make-method-table) (vector '()))
(define (mt:mlist mt) (vector-ref mt 0))

(define (method-table-assoc-p methlist type pred)
  (let loop ((m methlist))
    (if (null? m)
	#f
	(if (pred type (caar m))
	    (car m)
	    (loop (cdr m))))))

(define (instantiate-method f env)
  ; env contains static parameter assignments
  ; compilation goes here
  (if (lambda-closure? f)
      (make-lambda-closure
       (let ((e (lambda-closure-expr f)))
	 `(lambda ,(cadr e)
	    ; insert the static parameter values!
	    ,(append (list-head (caddr e) 4)
		     (list (append (map (lambda (p)
					  (cons (type-var-name (car p))
						(cdr p)))
					env)
				   (list-ref (caddr e) 4))))
	    ,(cadddr e)))
       (lambda-closure-env f))
      f))

(define (method-table-assoc methtable type)
  (let ((m (method-table-assoc-p (mt:mlist methtable) type
				 (lambda (t mt)
				   (if (has-typevars? mt)
				       (conform t mt)
				       (subtype? t mt))))))
    (and m
	 (if (has-typevars? (car m))
	     (let* ((env     (conform type (car m)))
		    (newtype (instantiate-type- (car m) env))
		    (newmeth (instantiate-method (cdr m) env)))
	       ; cache result in concrete method table
	       (method-table-insert! methtable newtype newmeth)
	       (if (subtype? type newtype)
		   (cons newtype newmeth)
		   #f))
	     m))))

(define (method-table-insert-p mlist type method pred)
  (let ((m (method-table-assoc-p mlist type pred)))
    (if (and m (type-equal? type (car m)))
	(begin (set-car! m type)   ; replace existing key
	       (set-cdr! m method)
	       mlist)
	(cons-in-order (cons type method) mlist car pred))))

; test whether a type is not more general than another
; this defines the method search order. note it has nothing to do with
; whether a and b are compatible in any way (e.g. assignable, convertible)
(define (type<=? a b)
  (if (has-typevars? a)
      (if (has-typevars? b)
	  (not (not (conform a b)))
	  (subtype? a b))
      (or (subtype? a b)
	  (and (not (subtype? b a))
	       (has-typevars? b)))))

(define (method-table-insert! mt type method)
  (vector-set! mt 0
	       (method-table-insert-p (mt:mlist mt)
				      type method type<=?)))

; --- generic functions ---

(define gf-type any-func)

(define (j-apply-generic ce args)
  (let* ((argtype (make-tuple-type (map type-of args)))
	 (meth    (best-method (vector-ref ce 0) argtype)))
    (if meth
	; applicable without conversion
	((closure-proc (cdr meth)) (closure-env (cdr meth)) args)
	(error "No method for function" (vector-ref ce 1) "matching types"
	       (map julia->string (type-params argtype))))))

(define (make-generic-function name)
  ;(display name) (newline)
  (make-closure j-apply-generic (vector (make-method-table) name)))

(define (instantiate-generic-function gf env stack)
  (let ((meths (map (lambda (p)
		      (cons (instantiate-type--  (car p) env stack)
			    (instantiate-method  (cdr p) env)))
		    (mt:mlist (gf-mtable gf))))
	(newgf (make-generic-function (gf-name gf))))
    (for-each (lambda (p) (add-method-for newgf (car p) (cdr p)))
	      meths)
    newgf))

(define (best-method methtable argtype)
  (method-table-assoc methtable argtype))

(define (gf-mtable gf) (vector-ref (closure-env gf) 0))
(define (gf-name gf)   (vector-ref (closure-env gf) 1))

; add a method for certain types
(define (add-method-for gf types meth)
  (define (add-dummy-type-params t)
    (cond ((type-ctor? t) (tc->type t))
	  ((tuple? t) (map add-dummy-type-params t))
	  ((union-type? t) (make-union-type (map add-dummy-type-params
						 (union-type-types t))))
	  ((func-type? t) (make-function-type
			   (add-dummy-type-params (func-fromtype t))
			   (add-dummy-type-params (func-totype   t))))
	  (else t)))
  (method-table-insert! (gf-mtable gf)
			(add-dummy-type-params types)
			meth)
  #t)

; --- define some key builtin types ---

(define Tensor-type
  (let ((v (type-vars '(T n))))
    (make-type-constructor v (make-tag-type 'Tensor Any-type v))))
(put-type 'Tensor Tensor-type)

(define scalar-type (instantiate-type Tensor-type (list Bottom-type
							Bottom-type)))
(put-type 'Scalar scalar-type)
(define number-type (make-tag-type 'Number scalar-type julia-null))
(put-type 'Number number-type)

(define real-type (make-tag-type 'Real number-type julia-null))
(put-type 'Real real-type)
(define int-type (make-tag-type 'Int real-type julia-null))
(put-type 'Int int-type)
(define float-type (make-tag-type 'Float real-type julia-null))
(put-type 'Float float-type)

(define (make-scalar-type name super)
  (make-struct-type name super julia-null julia-null julia-null
		    (lambda (type args)
		      (j-box type (if (number? (car args))
				      (car args)
				      (j-unbox (car args)))))))

(define bool-type   (make-scalar-type 'Bool   scalar-type))
(define int8-type   (make-scalar-type 'Int8   int-type))
(define uint8-type  (make-scalar-type 'Uint8  int-type))
(define int16-type  (make-scalar-type 'Int16  int-type))
(define uint16-type (make-scalar-type 'Uint16 int-type))
(define int32-type  (make-scalar-type 'Int32  int-type))
(define uint32-type (make-scalar-type 'Uint32 int-type))
(define int64-type  (make-scalar-type 'Int64  int-type))
(define uint64-type (make-scalar-type 'Uint64 int-type))
(define float32-type (make-scalar-type 'Float32 float-type))
(define float64-type (make-scalar-type 'Float64 float-type))

(define buffer-type
  (let ((v (type-vars '(T))))
    (make-type-constructor
     v
     (make-struct-type 'Buffer Any-type v (julia-tuple 'length)
		       (julia-tuple int32-type)
		       (lambda (type args)
			 (make-buffer type (car args)))))))

(define (make-buffer type n)
  (vector type n (make-vector (j-unbox n) 0)))


(put-type 'Bool   bool-type)
(put-type 'Int8   int8-type)
(put-type 'Uint8  uint8-type)
(put-type 'Int16  int16-type)
(put-type 'Uint16 uint16-type)
(put-type 'Int32  int32-type)
(put-type 'Uint32 uint32-type)
(put-type 'Int64  int64-type)
(put-type 'Uint64 uint64-type)
(put-type 'Float32 float32-type)
(put-type 'Float64 float64-type)

(put-type 'Buffer buffer-type)

; --- true and false values ---

(define julia-true (vector bool-type 1))
(define julia-false (vector bool-type 0))

; --- type conversions ---
(define (convert-tuple x to)
  (let loop ((cp (tuple->list x))
	     (pp (type-params to))
	     (result '()))
    (let ((pseq (and (pair? pp) (sequence-type? (car pp)))))
      (cond
       ((null? cp)            (and (or (null? pp) pseq)
				   (list->tuple (reverse result))))
       ((null? pp)            #f)
       (else
	(loop (cdr cp) (if pseq pp (cdr pp))
	      (cons (j-convert (car cp)
			       (if pseq
				   (type-param0 (car pp))
				   (car pp)))
		    result)))))))

(define (j-convert x to-type)
  (if (and (tuple? x) (tuple? to-type))
      (or (convert-tuple x to-type)
	  (error "Invalid tuple conversion"))
      (let ((t (type-of x)))
	(if (subtype? t to-type)
	    x
	    (let* ((m (j-get-field to-type 'convert))
		   (result (j-apply m (list x))))
	      (if (subtype? (type-of result) to-type)
		  result
		  (error "Conversion to" (julia->string to-type)
			 "failed")))))))

; --- builtin functions ---

(define (field-offset obj fld)
  (let ((fl (type-field-names (type-of obj))))
    (let loop ((i 0)
	       (L (tuple-length fl)))
      (if (< i L)
	  (if (eq? fld (tuple-ref fl i))
	      (+ i 1)
	      (loop (+ i 1) L))
	  (error "Type" (type-name (type-of obj)) "has no field" fld)))))

(define (to-symbol x)
  (if (symbol? x) x
      (if (not (eq? (type-of x) Symbol-type))
	  (error "Expected symbol")
	  (vector-ref x 1))))

(define (j-get-field obj fld)
  (vector-ref obj (field-offset obj (to-symbol fld))))

(define (j-set-field obj fld v)
  (let ((i (field-offset obj (to-symbol fld))))
    (vector-set! obj i
		 (j-convert v (tuple-ref (type-field-types (type-of obj))
					 (- i 1)))))
  obj)

(define (j-tuple . args) (if (null? args) julia-null
			     (apply julia-tuple args)))

(define (j-tuple-ref v i) (if (= i 0) (error "Tuple index out of range")
			      (tuple-ref v (- i 1))))

(define (j-buffer-length v) (j-unbox (j-get-field v 'length)))

(define (buffer-data v) (vector-ref v 2))

(define (j-buffer-ref v i)
  (vector-ref (buffer-data v) (- i 1)))

(define (j-buffer-set v i rhs)
  (vector-set! (buffer-data v) (- i 1) rhs))

(define (j-false? x)
  (and (vector? x)
       (eq? (vector-ref x 0) bool-type)
       (= (vector-ref x 1) 0)))

(define (j-is x y) (eq? x y))

(define (j-box type v) (vector type v))
(define (j-unbox v) (vector-ref v 1))
(define (j-box-set b v) (begin (vector-set! b 1 v) julia-null))

; fix scalar type to include a proper int32(0)
; this creates a circular reference
(tuple-set! (vector-ref scalar-type 3) 1 (j-box int32-type 0))
; set element type of scalar to scalar
(tuple-set! (vector-ref scalar-type 3) 0 scalar-type)

#|
function ref(t::Type, params...)
    return instantiate_type(t, params)
end
|#
(let ((ref-gf (make-generic-function 'ref)))
  (table-set! julia-globals 'ref ref-gf)
  (add-method-for ref-gf
		  (julia-tuple TypeConstructor
			       (instantiate-type
				sequence-type (list Any-type)))
		  (make-closure
		   (lambda (ce args)
		     (instantiate-type (car args) (cdr args)))
		   #f)))

; --- evaluator ---

(define *empty-env* '(()))

(define (make-numeric-literal n)
  (if (not (flonum? n))
      (j-box int32-type n)
      (j-box float64-type n)))

(define (scm->julia x)
  (cond ((symbol? x)  x)
	((null?   x)  x)
	((number? x)  (make-numeric-literal x))
	((or (vector? x) (string? x))  x)
	((atom? x)    (error "cannot quote" x))
	(else
	 (j-apply (eval-sym 'expr *empty-env*)
		  (map scm->julia x)))))

(define (julia->scm x)
  (cond ((symbol? x)  x)
	((string? x)  x)
	((pair? x) (map julia->scm x))
	((eq? (type-of x) bool-type) (if (j-false? x) 'false 'true))
	((subtype? (type-of x) number-type) (j-unbox x))
	((eq? (type-name (type-of x)) 'Expr)
	 (cons (julia->scm (j-get-field x 'head))
	       (map julia->scm (julialist->scmlist (j-get-field x 'args)))))
	(else
	 (error "invalid syntax: " x (julia->string x)))))

(define (julialist->scmlist l)
  (if (eq? (type-name (type-of l)) 'EmptyList)
      '()
      (cons (j-get-field l 'head)
	    (julialist->scmlist (j-get-field l 'tail)))))

(define (eval-sym s env)
  (let ((a (assq s (car env))))
    (if a (cdr a)
	(or (table-ref julia-globals s #f)
	    (error "Undefined variable" s)))))

(define (j-bound? s env)
  (or (assq s (car env)) (table-ref julia-globals s #f)))

(define (j-eval e env)
  (cond ((eq? e 'false)                julia-false)
	((eq? e 'true)                 julia-true)
        ((symbol? e)                   (eval-sym e env))
	((number? e)                   (make-numeric-literal e))
	((or (vector? e) (string? e))  e)
	(else
	 (case (car e)
	   ((quote)   (scm->julia (cadr e)))
	   ((null)    julia-null)
	   ((top)     (eval-sym (cadr e) *empty-env*))
	   ((lambda)  e)  ; remaining lambdas are data
	   ((value-or-null)  ; variable value or null if undefined
	    (let ((sym (cadr e)))
	      (if (j-bound? sym env)
		  (eval-sym sym env)
		  julia-null)))
	   ((closure-ref) (tuple-ref (cdr env) (cadr e)))
	   
	   ((=)
	    (if (not (symbol? (cadr e)))
		(error "Invalid lvalue in ="))
	    (let ((v (j-eval (caddr e) env))
		  (a (assq (cadr e) (car env))))
	      (if a (set-cdr! a v)
		  (table-set! julia-globals (cadr e) v))
	      v))
	   
	   ((call)
	    ;(display (cadr e)) (newline)
	    (j-apply (j-eval (cadr e) env)
		     (map (lambda (x) (j-eval x env))
			  (cddr e))))
	   
	   (else
	    (error "Unhandled tree type" (car e)))))))

(define (j-apply f args)
  (if (j-closure? f)
      ((closure-proc f) (closure-env f) args)
      (error "call: expected function")))

(define (sequence-type-ex? e)
  (and (length= e 4)
       (eq? (car e) 'call)
       (equal? (cadr e) '(top ref))
       (eq? (caddr e) '...)))

; create an environment with actual args bound to formal args
(define (bind-args names args)
  (if (null? names)
      '()
      (let* ((formal (car names))
	     (name   (arg-name formal))
	     (bind
	      (if (and (pair? formal) (eq? (car formal) '|::|)
		       (sequence-type-ex? (caddr formal)))
		  (cons name
			(apply julia-tuple args))
		  (cons name
			(car args)))))
	(cons bind
	      (bind-args (cdr names)
			 (if (pair? args) (cdr args) args))))))

(define (j-eval-body ce args)
  (let* ((cenv (cdr ce))
	 (formals (cadr (car ce)))
	 (body (cddr (car ce)))
	 (locl (cdadr (car body)))
	 (static-parameters (list-ref (car body) 4))
	 (code (cadr body))
	 ; env is ((locals...) . closure-env)
	 (env  (cons (append (map (lambda (local) (cons local julia-null))
				  locl)
			     (bind-args formals args)
			     static-parameters)
		     cenv))
	 (L    (vector-length code)))
    ; interpret the body of a function, handling control flow
    (let loop ((ip 0))
      (let ((I (vector-ref code ip)))
	(case (car I)
	  ((label) (loop (+ ip 1)))
	  ((goto)
	   (loop (cadr I)))
	  ((goto-ifnot)
	   (if (j-false? (j-eval (cadr I) env))
	       (loop (caddr I))
	       (loop (+ ip 1))))
	  ((return)
	   (j-eval (cadr I) env))
	  (else
	   (j-eval I env)
	   (loop (+ ip 1))))))))

(define (process-macro-def e)
  (let ((fexp
	 ((pattern-lambda (macro (call name . argl) body)
			  (let ((argl (fsig-to-lambda-list argl)))
			    (function-expr argl body)))
	  e)))
    (if (not fexp)
	(error "Invalid macro definition")
	(table-set! macro-env (cadr (cadr e))
		    (j-eval (cadr (julia-expand fexp)) *empty-env*)))))

(define (j-toplevel-eval e)
  (if (and (pair? e) (eq? (car e) 'macro))
      (process-macro-def e)
      ; lambda with no scope-block means variables assigned to in the
      ; expression stay global.
      (j-apply (j-eval (cadr (julia-expand `(lambda () ,e))) *empty-env*)
	       '())))

; --- initialize builtins ---

(define (ty s) (j-toplevel-eval (julia-parse s)))

(define (make-builtin name T impl)
  (table-set! julia-globals name
	      (make-closure
	       ;(if (string? T) (ty T) T)
	       (lambda (ce args) (apply impl args))
	       #f)))

; low-level intrinsics needed for bootstrapping
; the other builtins' type expressions cannot be evaluated without these
(make-builtin 'new_closure
	      ;"(Any,Tuple)-->Function"
	      (make-function-type (julia-tuple Any-type
					       Tuple-type)
				  any-func)
	      make-lambda-closure)
(make-builtin 'tuple
	      ;"(Any...)-->Tuple"
	      (make-function-type (julia-tuple (instantiate-type
						sequence-type
						(list Any-type)))
				  Tuple-type)
	      j-tuple)

; the following functions are compiler intrinsics, meaning that users
; should generally avoid them. they basically always expand to inline code,
; and may not be available as first-class functions in the final system.
; some require unboxed arguments, which are tricky to deal with.
(make-builtin 'bufferref "(Buffer[`T],Int)-->`T" j-buffer-ref)
(make-builtin 'bufferset "(Buffer[`T],Int,`T)-->`T" j-buffer-set)
(make-builtin 'tupleref "(Tuple,Int)-->Any" j-tuple-ref)
(make-builtin 'tuplelen "(Tuple,)-->Int" tuple-length)
(make-builtin 'box "(Type,`T)-->`T" j-box)
(make-builtin 'unbox "(`T,)-->`T" j-unbox)
(make-builtin 'boxset "(Any,Any)-->()" j-box-set)
(make-builtin 'new_struct_type
	      "(Symbol,Tuple,Type,Tuple)-->Type"
	      (lambda (name super params fnames)
		(if (or (subtype? super Tuple-type)
			(func-type? super)
			(union-type? super)
			(subtype? super buffer-type))
		    (error "Invalid subtyping in definition of" name))
		(if (not (every type-var? (tuple->list params)))
		    (error "Invalid type parameter list for" name))
		(make-struct-type name super params
				  (tuple-append (type-field-names super)
						fnames)
				  #f)))
(make-builtin 'new_struct_fields
	      "(Any,(Type...))-->()"
	      (lambda (t ftypes)
		(let ((t (if (type-ctor? t) (type-ctor-type t) t)))
		  (if (type-field-types t)
		      (error "You can't do that.")
		      (vector-set!
		       t 5
		       (tuple-append (type-field-types (vector-ref t 2))
				     ftypes))))))
(make-builtin 'new_type_constructor
	      "((TypeVar...),Type)-->TypeConstructor"
	      make-type-constructor)
(make-builtin 'new_tag_type
	      "(Symbol, Type, (TypeVar...))-->Type"
	      make-tag-type)
(make-builtin 'typevar "(Symbol,)-->TypeVar" make-type-var)
(make-builtin 'Union "(Any...)-->Type"
	      (lambda args
		(if (not (every (lambda (a)
				  (or (type? a) (type-var? a)))
				args))
		    (error "Invalid union type"))
		(make-union-type args)))
(make-builtin 'add_method
	      "(Symbol, Any, Type, Any)-->Function"
	      (lambda (name gf type meth)
		(let ((gf (if (eq? gf julia-null)
			      (make-generic-function name)
			      gf)))
		  (if (not (generic-function? gf))
		      (error "Variable" name "does not name a function")
		      (add-method-for gf type meth))
		  gf)))

(define (div-int x y)
  (let ((q (/ x y)))
    (if (< q 0)
	(ceiling q)
	(floor q))))
(define (mod-int x y) (- x (* (div-int x y) y)))
; round to n bits in 2's complement, evaluating constants at compile time
(define-macro (ui b n) `(bitwise-and ,n ,(- (arithmetic-shift 1 b) 1)))
(define-macro (si b n)
  (let ((g (gensym)))
    `(let ((,g ,n))
       (if (= (bitwise-and ,g ,(arithmetic-shift 1 (- b 1))) 0)
	   (bitwise-and ,g ,(- (arithmetic-shift 1 b) 1))
	   (- (bitwise-and (- ,g) ,(- (arithmetic-shift 1 b) 1)))))))
(define-macro (uif1 b f) `(lambda (x) (ui ,b (,f x))))
(define-macro (sif1 b f) `(lambda (x) (si ,b (,f x))))
(define-macro (uif2 b f) `(lambda (x y) (ui ,b (,f x y))))
(define-macro (sif2 b f) `(lambda (x y) (si ,b (,f x y))))
(make-builtin 'neg_int64 "(Int64,)-->Int64"      (sif1 64 -))
(make-builtin 'add_int64 "(Int64,Int64)-->Int64" (sif2 64 +))
(make-builtin 'sub_int64 "(Int64,Int64)-->Int64" (sif2 64 -))
(make-builtin 'mul_int64 "(Int64,Int64)-->Int64" (sif2 64 *))
(make-builtin 'div_int64 "(Int64,Int64)-->Int64" (sif2 64 div-int))
(make-builtin 'mod_int64 "(Int64,Int64)-->Int64" (sif2 64 mod-int))
(make-builtin 'neg_int32 "(Int32,)-->Int32"      (sif1 32 -))
(make-builtin 'add_int32 "(Int32,Int32)-->Int32" (sif2 32 +))
(make-builtin 'sub_int32 "(Int32,Int32)-->Int32" (sif2 32 -))
(make-builtin 'mul_int32 "(Int32,Int32)-->Int32" (sif2 32 *))
(make-builtin 'div_int32 "(Int32,Int32)-->Int32" (sif2 32 div-int))
(make-builtin 'mod_int32 "(Int32,Int32)-->Int32" (sif2 32 mod-int))
(make-builtin 'neg_int16 "(Int16,)-->Int16"      (sif1 16 -))
(make-builtin 'add_int16 "(Int16,Int16)-->Int16" (sif2 16 +))
(make-builtin 'sub_int16 "(Int16,Int16)-->Int16" (sif2 16 -))
(make-builtin 'mul_int16 "(Int16,Int16)-->Int16" (sif2 16 *))
(make-builtin 'div_int16 "(Int16,Int16)-->Int16" (sif2 16 div-int))
(make-builtin 'mod_int16 "(Int16,Int16)-->Int16" (sif2 16 mod-int))
(make-builtin 'neg_int8  "(Int8,)-->Int8"      (sif1 8 -))
(make-builtin 'add_int8  "(Int8,Int8)-->Int8"  (sif2 8 +))
(make-builtin 'sub_int8  "(Int8,Int8)-->Int8"  (sif2 8 -))
(make-builtin 'mul_int8  "(Int8,Int8)-->Int8"  (sif2 8 *))
(make-builtin 'div_int8  "(Int8,Int8)-->Int8"  (sif2 8 div-int))
(make-builtin 'mod_int8  "(Int8,Int8)-->Int8"  (sif2 8 mod-int))
(make-builtin 'add_float64 "(Float64,Float64)-->Float64" +)
(make-builtin 'sub_float64 "(Float64,Float64)-->Float64" -)
(make-builtin 'neg_float64 "(Float64,)-->Float64" -)
(make-builtin 'mul_float64 "(Float64,Float64)-->Float64" *)
(make-builtin 'div_float64 "(Float64,Float64)-->Float64"
	      (lambda (x y) (exact->inexact (/ x y))))
(define (j-eq x y) (if (= x y) julia-true julia-false))
(define (j-lt x y) (if (< x y) julia-true julia-false))
(make-builtin 'eq_int32 "(Int32,Int32)-->Bool" j-eq)
(make-builtin 'lt_int32 "(Int32,Int32)-->Bool" j-lt)
(make-builtin 'eq_float64 "(Float64,Float64)-->Bool" j-eq)
(make-builtin 'lt_float64 "(Float64,Float64)-->Bool" j-lt)
(make-builtin 'ne_float64 "(Float64,Float64)-->Bool"
	      (lambda (x y) (if (and (= x x)
				     (= y y)
				     (not (= x y)))
				julia-true
				julia-false)))
(make-builtin 'isnan_float64 "(Float64,)-->Bool"
	      (lambda (x) (if (not (= x x))
			      julia-true
			      julia-false)))
(make-builtin 'isinf_float64 "(Float64,)-->Bool"
	      (lambda (x) (if (or (equal? x +inf.0)
				  (equal? x -inf.0))
			      julia-true
			      julia-false)))
(define (to-int x) (inexact->exact (truncate x)))
(make-builtin 'to_bool "Scalar-->Bool"     (lambda (x) (if (= x 0) 0 1)))
(make-builtin 'to_int8 "Scalar-->Int8"     (lambda (x) (si  8 (to-int x))))
(make-builtin 'to_uint8 "Scalar-->Uint8"   (lambda (x) (ui  8 (to-int x))))
(make-builtin 'to_int16 "Scalar-->Int16"   (lambda (x) (si 16 (to-int x))))
(make-builtin 'to_uint16 "Scalar-->Uint16" (lambda (x) (ui 16 (to-int x))))
(make-builtin 'to_int32 "Scalar-->Int32"   (lambda (x) (si 32 (to-int x))))
(make-builtin 'to_uint32 "Scalar-->Uint32" (lambda (x) (ui 32 (to-int x))))
(make-builtin 'to_int64 "Scalar-->Int64"   (lambda (x) (si 64 (to-int x))))
(make-builtin 'to_uint64 "Scalar-->Uint64" (lambda (x) (ui 64 (to-int x))))
(make-builtin 'to_float64 "Scalar-->Float64" exact->inexact)
(make-builtin '_truncate "Scalar-->Int" to-int)

; the following builtin functions are ordinary first-class functions,
; and can be directly employed by the user.
(make-builtin 'getfield "(Any,Symbol)-->Any" j-get-field)
(make-builtin 'setfield "(Any,Symbol,Any)-->Any" j-set-field)
(make-builtin 'is "(Any,Any)-->Bool" (lambda (x y)
				       (if (j-is x y)
					   julia-true julia-false)))
(make-builtin 'isnull "Any-->Bool" (lambda (x)
				     (if (null? x)
					 julia-true julia-false)))
(make-builtin 'typeof "(Any,)-->Type" type-of)
(make-builtin 'subtype "(Type,Type)-->Bool"
	      (lambda (x t) (if (subtype? x t)
				julia-true julia-false)))
(make-builtin 'istype "(Any,Type)-->Bool"
	      (lambda (x t) (if (subtype? (type-of x) t)
				julia-true julia-false)))
(make-builtin 'typeassert "(Any,Type)-->Any"
	      (lambda (x t) (if (not (subtype? (type-of x) t))
				(error "Type assertion failed:"
				       (julia->string t)))
		      x))
(make-builtin 'apply "(Function[`A,`T],Tuple...)-->`T"
	      (lambda (f . argt)
		(j-apply f (apply append (map tuple->list argt)))))
(make-builtin 'error "Any-->Union" error)
(make-builtin 'convert "(Any,Type)-->Any" j-convert)

; for timing
(make-builtin 'time_thunk "(()-->Any)-->Any"
	      (lambda (f) (time (j-apply f '()))))

; --- load ---

(define (j-load fname)
  (for-each j-toplevel-eval (julia-parse-file fname))
  julia-null)

(make-builtin 'load "(Any,)-->()" j-load)

; --- print and repl ---

(define (julia->string x)
  (with-output-to-string '() (lambda () (call-print x))))

(define (call-print x)
  (j-apply (eval-sym 'print *empty-env*) (list x)))

(define (print-tuple x opn cls)
  (display opn)
  (let loop ((L (tuple-length x))
	     (i 0))
    (if (< i L)
	(begin (call-print (tuple-ref x i))
	       (if (< i (- L 1))
		   (display ", ")
		   (if (= L 1)
		       (display ",")))
	       (loop L (+ i 1)))
	(display cls))))

(define (print-type t)
  (cond ((eq? t scalar-type)
	 (display "Tensor[Scalar, 0]"))
	((func-type? t)
	 (julia-print (func-fromtype t))
	 (display "-->")
	 (julia-print (func-totype t)))
	((union-type? t)
	 (display "Union")
	 (print-tuple (union-type-types t) "(" ")"))
	(else
	 (begin
	   (display (type-name t))
	   (let ((p (type-params t)))
	     (if (> (tuple-length p) 0)
		 (print-tuple p "[" "]")))))))

(define (julia-print x)
  (cond
   ((number? x) (begin (display "#<unboxed number ")
		       (display x)
		       (display ">")))
   ((tuple? x)
    (print-tuple x "(" ")"))
   ((not (vector? x))  (display x))
   ((generic-function? x)
    (display "#<generic-function ")
    (display (gf-name x))
    (display ">"))
   ((j-closure? x)
    (display "#<closure>"))
   ((number? (vector-ref x 0))
    (display "#<primitive-buffer ")
    (display x)
    (display ">"))
   ((type? x)
    (print-type x))
   ((type-var? x)
    (display (type-var-name x)))
   ((eq? (type-name (type-of x)) 'Buffer)
    (display "Buffer[")
    (display (type-name (type-param0 (type-of x))))
    (display "]:")
    (display (buffer-data x)))
   (else
    (let* ((t (type-of x))
	   (tn (type-name t)))
      (case tn
	((Int8 Uint8 Int16 Uint16 Int32 Uint32 Int64 Uint64 Float32 Float64)
	 (display (j-unbox x)))
	((Bool) (if (j-false? x)
		    (display "false")
		    (display "true")))
	((Symbol) (display (vector-ref x 1)))
	(else
	 (let ((fields (type-field-names t))
	       (vals (cdr (vector->list x))))
	   (display tn)
	   (display "(")
	   (let loop ((L (- (vector-length x) 1))
		      (i 0))
	     (if (< i L)
		 (begin (display (tuple-ref fields i))
			(display "=")
			(call-print (vector-ref x (+ i 1)))
			(if (< i (- L 1))
			    (display ", "))
			(loop L (+ i 1)))
		 (display ")"))))))))))

(make-builtin '_print "(Any,)-->()" julia-print)

(define (detect-color)
  (let ((tput (shell-command "tput setaf 9 >&/dev/null")))
    (if (= tput 0)
	#t
	(if (= tput 1)
	    #f
	    (let ((Tenv (with-exception-catcher
			 (lambda (e) #f)
			 (lambda () (getenv "TERM")))))
	      (or (equal? Tenv "xterm")
		  (equal? Tenv "xterm-color")))))))

(define COLOR? (detect-color))

(define banner
(if COLOR?
"\033[1m               \033[32m_\033[37m      
   \033[36m_\033[37m       _ \033[31m_\033[32m(_)\033[35m_\033[37m     |
  \033[36m(_)\033[37m     | \033[31m(_) \033[35m(_)\033[37m    |  pre-release version
   _ _   _| |_  __ _   |
  | | | | | | |/ _` |  |
  | | |_| | | | (_| |  |  \302\2512010 contributors
 _/ |\\__'_|_|_|\\__'_|  |  
|__/                   |\033[0m

"

"               _      
   _       _ _(_)_     |
  (_)     | (_) (_)    |  pre-release version
   _ _   _| |_  __ _   |
  | | | | | | |/ _` |  |
  | | |_| | | | (_| |  |  \302\2512010 contributors
 _/ |\\__'_|_|_|\\__'_|  |  
|__/                   |

"
))

(define (julia-repl)
  (j-load "start.j")
  (j-load "examples.j")
  (display banner)
  (julia-prompt))

(define (julia-prompt)
  (define (display-error-exception e)
    (display (error-exception-message e))
    (for-each (lambda (x)
		(display " ") (display x))
	      (error-exception-parameters e)))
  
  (if COLOR? (display "\033[0m"))
  (display "julia> ")
  (let* ((line (read-line))
	 (str  (make-token-stream (open-input-string (if (eof-object? line)
							 ""
							 line))))
	 (continue?
	  (with-exception-catcher
	   (lambda (e)
	     (display-error-exception e)
	     #t)
	   (lambda ()
	     (let ((expr (julia-parse str)))
	       (check-end-of-input str)
	       (or (equal? line "")     ; empty line is valid
		   (and
		    (not (eq? expr 'Quit))
		    (not (eof-object? expr))
		    (if COLOR? (display "\033[1m\033[36m"))
		    (call-print (j-toplevel-eval expr)))))))))
    (newline)
    (if continue?
	(begin (newline) (julia-prompt)))))
