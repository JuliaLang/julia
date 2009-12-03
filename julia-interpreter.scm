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

; --- singleton null value ---

(define julia-null (julia-tuple))
(define (j-null? x) (null? x))

; --- type objects, reflection ---

(define (put-type name t) (table-set! julia-globals name t))
(define (get-type name)
  (let ((t (table-ref julia-globals name #f)))
    (and (type? t) t)))

; in any-type and Type-type we take pains to avoid circular references
; because gambit can't deal with them well, so there are symbols where
; type objects should go.
(define any-type (vector 'Type 'Any 'Any julia-null julia-null #t))

(define Type-type (vector 'Type 'Type any-type julia-null
			  (julia-tuple
			   (julia-tuple 'name 'Symbol)
			   (julia-tuple 'super 'Type)
			   (julia-tuple 'parameters 'Tuple)
			   (julia-tuple 'fields 'Tuple)
			   (julia-tuple 'abstract 'Bool))
			  #f))

(define (make-type name super params fields)
  (vector Type-type name super params fields #f))

(define (make-abstract-type name super params fields)
  (vector Type-type name super params fields #t))

(define (type-name t) (if (tuple? t) 'Tuple (vector-ref t 1)))
(define (type-super t)
  (if (eq? t any-type)
      any-type
      (if (tuple? t)
	  tuple-type
	  (vector-ref t 2))))
(define (type-params t) (if (tuple? t) t (vector-ref t 3)))
(define (type-fields t) (if (tuple? t) julia-null (vector-ref t 4)))

(define (make-tuple-type typelist) (list->tuple typelist))

(define (closure-proc c) (vector-ref c 1))
(define (closure-env c)  (vector-ref c 2))

(define (j-closure? x) (and (vector? x)
			    (vector? (vector-ref x 0))
			    (eq? (type-name (vector-ref x 0)) 'Function)))

(define (generic-function? x) (and (j-closure? x)
				   (eq? (closure-proc x) j-apply-generic)))

; get the type of a value
(define (type-of v)
  (cond ((eq? v Type-type)   Type-type)
	((eq? v any-type)    Type-type)
	((eq? v julia-null)  julia-null) ; again to avoid circular references
	; for now, allow scheme symbols to act as julia symbols
	((symbol? v)             symbol-type)
	((string? v)             any-type)  ; temporary
	((procedure? v)   	 any-type)
	((number? v)   	         any-type)
	((tuple? v)              (make-tuple-type
				  (map type-of (tuple->list v))))
	(else                    (vector-ref v 0))))

(define (type? v) (or (and (tuple? v)
			   (every (lambda (x)
				    (or (symbol? x)
					(type? x))) v))
		      (and (vector? v)
			   (eq? (type-of v) Type-type))))

(define (j-symbol? v) (and (vector? v) (eq? (vector-ref v 0) symbol-type)))

; --- define primitive types ---

(define tuple-type (make-abstract-type 'Tuple any-type julia-null julia-null))
(put-type 'Tuple tuple-type)

(define sequence-type (make-abstract-type '... any-type (julia-tuple 'T)
					  julia-null))
(put-type '... sequence-type)
(define (sequence-type? t)
  (and (type? t)
       (eq? (type-name t) '...)))

(define Tensor-type (make-abstract-type 'Tensor any-type
					(julia-tuple 'T 'n)
					julia-null))
(put-type 'Tensor Tensor-type)

(define function-type (make-type 'Function any-type
				 (julia-tuple 'A 'B) julia-null))
(put-type 'Function function-type)

(define (make-union-type params)
  (make-abstract-type 'Union any-type (list->tuple params) julia-null))

(define union-type (make-union-type '()))
(put-type 'Union union-type)
(define bottom-type union-type)

; --- type functions ---

(define (type-generic? t)
  (or (symbol? t)
      (and (type? t)
	   (not (eq? t scalar-type)) ; circular reference problem
	   (any (lambda (x) (or (symbol? x)
				(type-generic? x)))
		(type-params t)))))

(define (type-abstract? t)
  (or (type-generic? t)
      (vector-ref t 5)))

(define (has-params? t)
  (not (j-null? (type-params t))))

(define (type-param0 t)
  (tuple-ref (type-params t) 0))

(define (type-param t n)
  (tuple-ref (type-params t) n))

(define (j-int32? x)
  (and (vector? x) (eq? (type-name (type-of x)) 'Int32)))

(define (all-type-params t)
  (cond ((symbol? t)     (list t))
	((not (type? t)) '())
	(else
	 (delete-duplicates
	  (apply append (map (lambda (p)
			       (if (eq? p t) '() ; avoid self pointers
				   (all-type-params p)))
			     (type-params t)))))))

(define (instantiate-type type params)
  (if (eq? type union-type)
      (make-union-type params)
      ; convert X-->Y to (X,)-->Y if X is not already a tuple or symbol
      (let ((params (if (and (pair? params)
			     (eq? (type-name type) 'Function)
			     (symbol? (type-param0 type))
			     (not (tuple? (car params)))
			     (not (symbol? (car params))))
			(cons (julia-tuple (car params))
			      (cdr params))
			params))
	    (tp  (all-type-params type)))
	
	(check-same-length
	 params tp
	 (lambda () (error "Too few parameters for type" (type-name type)))
	 (lambda () (error "Too many parameters for type" (type-name type))))
	
	(instantiate-type- type (map cons tp params)))))

; instantiate a type using the bindings in the given type environment
; (an assoc list from names to types)
(define (instantiate-type- type env)
  (instantiate-type-- type env '()))

; create a non-circular symbolic representation of a type, suitable
; for use as a lookup key
(define (type-lookup-key t)
  (cond ((eq? t (get-type 'Scalar)) 'Scalar)
	((type? t)
	 (let ((p (type-params t)))
	   (if (null? p)
	       (type-name t)
	       (cons (type-name t)
		     (map type-lookup-key p)))))
	((j-int32? t) (j-unbox t))
	((symbol? t)  `(quote ,t))
	(else         t)))

(define (instantiate-type-- type env stack)
  (define (copy-type type super params fields)
    (if (tuple? type)
	params
	(if (vector-ref type 5)
	    (make-abstract-type (type-name type)
				super
				params fields)
	    (make-type (type-name type)
		       super
		       params fields))))
  
  (cond
   ((symbol? type)      (lookup type env type))
   ((not (type? type))  type)
   ((null? env)         type)
   ((= 0 (tuple-length (type-params type)))  type)
   (else
    (let ((i-super  (instantiate-type-- (type-super type) env stack))
	  (i-params (map (lambda (t)
			   (if (eq? t type) type
			       (instantiate-type-- t env stack)))
			 (type-params type))))
      (let* ((key  (cons (type-name type)
			 (map type-lookup-key i-params)))
	     (back (assoc key stack)))
	(if back (cdr back)
	    (let* ((newtype
		    (copy-type type i-super (list->tuple i-params) julia-null))
		   (newstack (cons (cons key newtype) stack)))
	      (if (and (not (tuple? newtype))
		       (not (eq? #f (type-fields type))))
		  (vector-set!
		   newtype 4
		   (alist->tuples
		    (map (lambda (a)
			   (list (car a)
				 (instantiate-type-- (cadr a) env newstack)))
			 (tuples->alist (type-fields type))))))
	      newtype)))))))

(define (rename-type-params t)
  (let* ((p (all-type-params t)))
    (instantiate-type- t (map (lambda (x) (cons x (gensym))) p))))

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
	((symbol? parent)         #t)
	((symbol? child)          #f)
	((eq? parent tuple-type)  (tuple? child))
	((eq? parent any-type)    #t)
	((eq? child any-type)     #f)
	
	((and (j-int32? child) (j-int32? parent))
	 (= (j-unbox child) (j-unbox parent)))
	
	; recursively handle union types
	((eq? (type-name child) 'Union)
	 (every (lambda (t) (subtype? t parent))
		(type-params child)))
	((eq? (type-name parent) 'Union)
	 (any   (lambda (t) (subtype? child t))
	        (type-params parent)))
	
	((and (tuple? child) (tuple? parent))
	 (tuple-subtype? child parent))
	
	; buffers are invariant
	((and (eq? (type-name child) 'Buffer)
	      (eq? (type-name parent) 'Buffer))
	 (type-equal? (type-param0 child)
		      (type-param0 parent)))
	
	; functions are contravariant in first parameter
	((and (eq? (type-name child) 'Function)
	      (eq? (type-name parent) 'Function))
	 (and (or (symbol? (type-param0 parent))
		  (subtype? (type-param0 parent)
			    (type-param0 child)))
	      (subtype? (type-param child 1)
			(type-param parent 1))))
	
	; handle sibling instantiations of the same generic type.
	((eq? (type-name child)
	      (type-name parent))
	 (let loop ((cp (type-params child))   ; child parameters
		    (pp (type-params parent))) ; parent parameters
	     (cond
	      ((null? cp)  (null? pp))
	      ((null? pp)  #f)
	      (else
	       ; default to covariance
	       (and (subtype? (car cp) (car pp))
		    (loop (cdr cp) (cdr pp)))))))
	
	; otherwise walk up the type hierarchy
	(else (subtype? (type-super child) parent))))

(define (tuple-convertible? from to)
  (tuple-elementwise? convertible? from to))

(define (convertible? from to)
  ;(display "convert? ")
  ;(julia-print from) (display " ")
  ;(julia-print to) (newline)
  (if (and (type? from) (type? to))
      (or (subtype? from to)
	  (if (and (eq? (type-name from) 'Function)
		   (eq? (type-name to)   'Function))
	      (and (convertible? (type-param to 0)
				 (type-param from 0))
		   (convertible? (type-param from 1)
				 (type-param to 1)))
	      (if (and (tuple? from) (tuple? to))
		  (tuple-convertible? from to)
		  (not (not (get-conversion from to))))))
      (equal? from to)))

; tells whether a type conforms to a given generic type
; returns #f or an assoc list showing the assignment of
; type parameters that makes the relation hold
(define (conform t gt cnvt?)
  (define (param-search t gt p options env)
    (if (null? p)
	(let ((super (instantiate-type- gt env)))
	  (and (if cnvt?
		   (or (subtype? t super)
		       (and (not (any (lambda (e) (type-abstract? (cdr e)))
				      env))
			    (convertible? t super)))
		   (subtype? t super))
	       env))
	; make sure all parameters in t that this parameter might match
	; are the same symbol, otherwise there's a conflict. e.g.
	; (A, B) doesn't conform to (T, T)
	(and (not (length> (delete-duplicates (filter symbol? (car options)))
			   1))
	     (let try-assignment ((opts (car options)))
	       (if (pair? opts)
		   (or (param-search t gt (cdr p) (cdr options)
				     (cons (cons (car p) (car opts)) env))
		       (try-assignment (cdr opts)))
		   ; no possibilities left for p
		   #f)))))
  
  ;(display "conform? ")
  ;(julia-print t) (display " ")
  ;(julia-print gt) (newline)
  (let ((pairs (conform- t gt '() (if cnvt? convertible? subtype?)))
	(tp    (all-type-params gt)))
    ; post-process to see if all the possible types for a given
    ; parameter can be reconciled
    (and
     pairs
     (let ((options
	    (map (lambda (p)
		   (map car
			(filter (lambda (c) (eq? (cdr c) p))
				pairs)))
		 tp)))
       (param-search t gt tp options '())))))

; generate list of corresponding type components, (type . T) if parameter
; T might correspond to type
; pred is either subtype? or convertible? depending on whether conversion
; is allowed for this lookup.
(define (conform- child parent env pred)
  ;(display "conform- ")
  ;(julia-print child) (display " ")
  ;(julia-print parent) (newline)
  (cond ((symbol? parent)
	 (cons (cons child parent) env))
	((symbol? child) #f)
	
	((and (j-int32? child) (j-int32? parent))
	 (and (= (j-unbox child) (j-unbox parent))
	      env))
	
	((eq? child parent)       env)
	((eq? parent any-type)    env)
	((eq? child any-type)     #f)
	
	((eq? (type-name child) 'Union)
	 (foldl (lambda (t env)
		  (and env
		       (conform- t parent env pred)))
		env
		(type-params child)))
	((eq? (type-name parent) 'Union)
	 ; todo: maybe union all corresponding components together
	 (any   (lambda (t) (conform- child t env pred))
	        (type-params parent)))
	
	((and (or (not (has-params? parent))
		  (not (has-params? child)))
	      (eq? pred subtype?))
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
				(pseq
				 (conform- bottom-type (type-param0 (car pp))
					   env pred))
				(else #f)))
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
				       env pred)))
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
	 (let ((super (conform- (type-super child) parent env pred)))
	   (if (eq? pred subtype?)
	       super
	       (let ((temp
		      (cons super
			    (map (lambda (q)
				   (conform- q parent env subtype?))
				 (possible-conversions child)))))
		 (and (not (every not temp))
		      (delete-duplicates-p
		       (apply append (map (lambda (x) (or x '())) temp))
		       eq?))))))))

; --- define some key builtin types ---

(define scalar-type (instantiate-type Tensor-type (list bottom-type 0)))
(put-type 'Scalar scalar-type)

(define real-type (make-abstract-type 'Real scalar-type julia-null julia-null))
(put-type 'Real real-type)
(define int-type (make-abstract-type 'Int real-type julia-null julia-null))
(put-type 'Int int-type)
(define float-type (make-abstract-type 'Float real-type julia-null julia-null))
(put-type 'Float float-type)

(define bool-type (make-type 'Bool scalar-type julia-null julia-null))
(define int8-type (make-type 'Int8 int-type julia-null julia-null))
(define uint8-type (make-type 'Uint8 int-type julia-null julia-null))
(define int16-type (make-type 'Int16 int-type julia-null julia-null))
(define uint16-type (make-type 'Uint16 int-type julia-null julia-null))
(define int32-type (make-type 'Int32 int-type julia-null julia-null))
(define uint32-type (make-type 'Uint32 int-type julia-null julia-null))
(define int64-type (make-type 'Int64 int-type julia-null julia-null))
(define uint64-type (make-type 'Uint64 int-type julia-null julia-null))
(define single-type (make-type 'Single float-type julia-null julia-null))
(define double-type (make-type 'Double float-type julia-null julia-null))

(define symbol-type (make-type 'Symbol any-type julia-null julia-null))
(define buffer-type (make-type 'Buffer any-type (julia-tuple 'T)
			       (julia-tuple
				(julia-tuple 'length int32-type))))

(put-type 'Any any-type)
(put-type 'Bool bool-type)
(put-type 'Int8 int8-type)
(put-type 'Uint8 uint8-type)
(put-type 'Int16 int16-type)
(put-type 'Uint16 uint16-type)
(put-type 'Int32 int32-type)
(put-type 'Uint32 uint32-type)
(put-type 'Int64 int64-type)
(put-type 'Uint64 uint64-type)
(put-type 'Single single-type)
(put-type 'Double double-type)
(put-type 'Type Type-type)

(put-type 'Symbol symbol-type)
(put-type 'Buffer buffer-type)

; --- true and false values ---

(define julia-true (vector bool-type 1))
(define julia-false (vector bool-type 0))

; --- function objects ---

(define (make-closure ft proc env)
  (if (not (subtype? ft function-type))
      (error "make-closure: not a function type"))
  (vector ft proc env))

(define (function-arg-type f)
  (type-param0 (type-of f)))

(define (make-function-type argtype rettype)
  (instantiate-type function-type (list argtype rettype)))

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
  f)

(define (method-table-assoc methtable type cnvt?)
  (let ((m (method-table-assoc-p (mt:mlist methtable) type
				 (if cnvt?
				     (lambda (t mt)
				       (if (type-generic? mt)
					   (conform t mt #t)
					   (convertible? t mt)))
				     (lambda (t mt)
				       (if (type-generic? mt)
					   (conform t mt #f)
					   (subtype? t mt)))))))
    (and m
	 (if (type-generic? (car m))
	     (let* ((env     (conform type (car m) cnvt?))
		    (newtype (instantiate-type- (car m) env))
		    (newmeth (instantiate-method (cdr m) env)))
	       ; cache result in concrete method table
	       (method-table-insert! methtable newtype newmeth)
	       ; we know the newly-instantiated method is at least applicable
	       ; under conversion, so if cnvt? is true we can always return it
	       (if (or cnvt? (subtype? type newtype))
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
  (if (type-generic? a)
      (if (type-generic? b)
	  (not (not (conform a b #f)))
	  (subtype? a b))
      (or (subtype? a b)
	  (and (not (subtype? b a))
	       (type-generic? b)))))

(define (method-table-insert! mt type method)
  (vector-set! mt 0
	       (method-table-insert-p (mt:mlist mt)
				      type method type<=?)))

; --- generic functions ---

(define gf-type (instantiate-type function-type (list any-type any-type)))

(define (j-apply-generic ce args)
  (let* ((argtype (make-tuple-type (map type-of args)))
	 (meth    (best-method (vector-ref ce 0) argtype #f)))
    (if meth
	; applicable without conversion
	((closure-proc (cdr meth)) (closure-env (cdr meth)) args)
	(let ((meth  (best-method (vector-ref ce 0) argtype #t)))
	  (if (not meth)
	      (error "No method for function" (vector-ref ce 1)
		     "matching types"
		     (map julia->string (type-params argtype)))
	      ; applicable with conversion
	      ((closure-proc (cdr meth)) (closure-env (cdr meth))
	       (tuple->list
		(j-convert (list->tuple args)
			   (car meth)))))))))

(define (make-generic-function name)
  (make-closure gf-type j-apply-generic (vector (make-method-table) name)))

(define (best-method methtable argtype cnvt?)
  (method-table-assoc methtable argtype cnvt?))

(define (gf-mtable gf) (vector-ref (closure-env gf) 0))
(define (gf-name gf)   (vector-ref (closure-env gf) 1))

; add a method for certain types
(define (add-method-for gf types meth)
  (method-table-insert! (gf-mtable gf) types meth)
  #t)

; add a method based on its function type
(define (add-method gf meth)
  (add-method-for gf (function-arg-type meth) meth))

; --- implicit conversions ---

(define conversion-table (make-method-table))

(define (add-conversion from to method)
  (define (non-convertible? t)
    (or (tuple? t)
	(sequence-type? t)
	(and (type? t)
	     (or (eq? (type-name t) 'Union)
		 (eq? (type-name t) 'Function)))))
  (if (non-convertible? from)
      (error "Conversions may not be defined from type" (julia->string from)))
  (if (non-convertible? to)
      (error "Conversions may not be defined to type" (julia->string to)))
  (if (subtype? from to)
      (error "Cannot define a conversion from a type to itself"))
  (method-table-insert! conversion-table
			; NOTE the "flipped" function type, "to-->from"
			; is necessary because method-table looks for
			; a method supporting a superset of the needed
			; behavior, and for conversions we need a type
			; providing a subset of the requested behavior.
			(make-function-type to from)
			method)
  julia-null)

(define (get-conversion from to)
  (let ((m (method-table-assoc conversion-table
			       (make-function-type to from) #f)))
    (and m (cdr m))))

; get a list of types t can be converted to
(define (possible-conversions t)
  (foldl (lambda (te lst)
	   (let ((to-type
		  (let* ((ft   (car te))
			 (from (type-param ft 1))
			 (to   (type-param0 (type-param ft 0))))
		    (if (type-generic? from)
			(let ((env (conform t from #f)))
			  (and env
			       (instantiate-type- to env)))
			(and (subtype? t from)
			     to)))))
	     (if to-type
		 (cons to-type lst)
		 lst)))
	 '()
	 (mt:mlist conversion-table)))

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
	    (let ((m (get-conversion t to-type)))
	      (if m
		  (let ((result (j-apply m (list x to-type))))
		    (if (subtype? (type-of result) to-type)
			result
			(error "Conversion to" (julia->string to-type)
			       "failed")))
		  (error "No conversion from" (julia->string t) "to"
			 (julia->string to-type))))))))

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
  ; TODO: type check/convert
  (vector-set! obj (field-offset obj (to-symbol fld)) v))

(define (j-tuple . args) (if (null? args) julia-null
			     (apply julia-tuple args)))

(define (j-tuple-ref v i) (if (= i 0) (error "Tuple index out of range")
			      (tuple-ref v (- i 1))))

(define (j-buffer-length v) (j-unbox (j-get-field v 'length)))

(define (buffer-data v) (vector-ref v 2))

(define (j-buffer-ref v i)
  (vector-ref (buffer-data v) (- i 1)))

(define (j-buffer-set v i rhs)
  ; TODO: type check/convert
  (vector-set! (buffer-data v) (- i 1) rhs))

(define (j-false? x)
  (and (vector? x)
       (eq? (vector-ref x 0) bool-type)
       (= (vector-ref x 1) 0)))

(define (j-new type args)
  (if (type-abstract? type)
      (error "Cannot instantiate abstract type" (type-name type)))
  (let* ((tn (type-name type))
	 (L (if (eq? tn 'Tuple)
		(length args)
		(if (eq? tn 'Buffer)
		    3
		    (tuple-length (type-fields type)))))
	 (v (make-vector (+ 1 L))))
    (vector-set! v 0 type)
    (if (eq? tn 'Buffer)
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
  (add-method ref-gf (make-closure
		      (instantiate-type
		       function-type
		       (list (julia-tuple Type-type
					  (instantiate-type
					   sequence-type (list any-type)))
			     Type-type))
		      (lambda (ce args)
			(instantiate-type (car args) (cdr args)))
		      #f)))

; --- evaluator ---

(define (scm->julia x)
  x)

(define (make-numeric-literal n)
  (if (not (flonum? n))
      (j-box int32-type n)
      (j-box double-type n)))

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
	   ((top)     (eval-sym (cadr e) '(())))
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
	 (code (cadr body))
	 (env  (cons (append (map (lambda (local) (cons local julia-null))
				  locl)
			     (bind-args formals args))
		     cenv))
	 (L    (vector-length code)))
    ; interpret the body of a function, handling control flow
    (let loop ((ip 0))
      (let ((I (vector-ref code ip)))
	(if (atom? I)
	    (begin (j-eval I env) (loop (+ ip 1)))
	    (case (car I)
	      ((goto)
	       (loop (caadr I)))
	      ((goto-ifnot)
	       (if (j-false? (j-eval (cadr I) env))
		   (loop (caaddr I))
		   (loop (+ ip 1))))
	      ((return)
	       (j-eval (cadr I) env))
	      (else
	       (j-eval I env)
	       (loop (+ ip 1)))))))))

(define (j-toplevel-eval e)
  ; lambda with no scope-block means variables assigned to in the
  ; expression stay global.
  (j-apply (j-eval (cadr (julia-expand `(lambda () ,e))) '(()))
	   '()))

; --- initialize builtins ---

(define (ty s) (j-toplevel-eval (julia-parse s)))

(define (make-builtin name T impl)
  (table-set! julia-globals name
	      (make-closure
	       (if (string? T) (ty T) T)
	       (lambda (ce args) (apply impl args))
	       #f)))

; low-level intrinsics needed for bootstrapping
; the other builtins' type expressions cannot be evaluated without these
(make-builtin 'new_closure
	      ;"(Type,Any,Tuple)-->Function"
	      (instantiate-type function-type
				(list (julia-tuple Type-type
						   any-type
						   tuple-type)
				      function-type))
	      (lambda (t e clo)
		(make-closure t j-eval-body (cons e clo))))
(make-builtin 'tuple
	      ;"(Any...)-->Tuple"
	      (instantiate-type function-type
				(list (julia-tuple (instantiate-type
						    sequence-type
						    (list any-type)))
				      tuple-type))
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
(make-builtin 'add_conversion "(Type,Type,Function)-->()" add-conversion)
(make-builtin 'new_type
	      "(Symbol,(Symbol...),Type)-->Type"
	      (lambda (name params super)
		(if (or (subtype? super tuple-type)
			(subtype? super function-type)
			(subtype? super buffer-type))
		    (error "Invalid subtyping in definition of" name))
		(let ((pl (tuple->list params))
		      (sp (all-type-params super)))
		  (if (not (every symbol? pl))
		      (error "Invalid type parameter list for" name))
		  (if (not (every (lambda (p) (memq p pl))
				  sp))
		      (error "Supertype has unbound parameters in definition of"
			     name)))
		(make-type name super params #f)))
(make-builtin 'new_type_fields
	      "(Type,Type,((Symbol,Type)...))-->()"
	      (lambda (t super fields)
		(if (type-fields t)
		    (error "You can't do that.")
		    (vector-set! t 4
				 (tuple-append (type-fields super) fields)))))
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
(make-builtin 'add_int32 "(Int32,Int32)-->Int32" (sif2 32 +))
(make-builtin 'sub_int32 "(Int32,Int32)-->Int32" (sif2 32 -))
(make-builtin 'neg_int32 "(Int32,)-->Int32"      (sif1 32 -))
(make-builtin 'mul_int32 "(Int32,Int32)-->Int32" (sif2 32 *))
(make-builtin 'div_int32 "(Int32,Int32)-->Int32" (sif2 32 div-int))
(make-builtin 'mod_int32 "(Int32,Int32)-->Int32" (sif2 32 mod-int))
(make-builtin 'add_double "(Double,Double)-->Double" +)
(make-builtin 'sub_double "(Double,Double)-->Double" -)
(make-builtin 'neg_double "(Double,)-->Double" -)
(make-builtin 'mul_double "(Double,Double)-->Double" *)
(make-builtin 'div_double "(Double,Double)-->Double"
	      (lambda (x y) (exact->inexact (/ x y))))
(make-builtin 'eq_int32 "(Int32,Int32)-->Bool"
	      (lambda (x y) (if (= x y)
				julia-true
				julia-false)))
(make-builtin 'eq_double "(Double,Double)-->Bool"
	      (lambda (x y) (if (= x y)
				julia-true
				julia-false)))
(make-builtin 'lt_int32 "(Int32,Int32)-->Bool"
	      (lambda (x y) (if (< x y)
				julia-true
				julia-false)))
(make-builtin 'lt_double "(Double,Double)-->Bool"
	      (lambda (x y) (if (< x y)
				julia-true
				julia-false)))
(make-builtin 'ne_int32 "(Int32,Int32)-->Bool"
	      (lambda (x y) (if (not (= x y))
				julia-true
				julia-false)))
(make-builtin 'ne_double "(Double,Double)-->Bool"
	      (lambda (x y) (if (and (= x x)
				     (= y y)
				     (not (= x y)))
				julia-true
				julia-false)))
(make-builtin 'isnan_double "(Double,)-->Bool"
	      (lambda (x) (if (not (= x x))
			      julia-true
			      julia-false)))
(make-builtin 'isinf_double "(Double,)-->Bool"
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
(make-builtin 'to_double "Scalar-->Double" exact->inexact)
(make-builtin '_truncate "Scalar-->Int" to-int)

; the following builtin functions are ordinary first-class functions,
; and can be directly employed by the user.
(make-builtin 'new "(Type,Any...)-->Any" (lambda (t . args) (j-new t args)))
(make-builtin 'getfield "(Any,Symbol)-->Any" j-get-field)
(make-builtin 'setfield "(Any,Symbol,Any)-->Any" j-set-field)
(make-builtin 'is "(Any,Any)-->Bool" j-is)
(make-builtin 'isnull "Any-->Bool" (lambda (x)
				     (if (null? x)
					 julia-true julia-false)))
(make-builtin 'typeof "(Any,)-->Type" type-of)
(make-builtin 'subtype "(Type,Type)-->Bool"
	      (lambda (x y) (if (subtype? x y)
				julia-true julia-false)))
(make-builtin 'istype "(Any,Type)-->Bool"
	      (lambda (x y) (if (subtype? (type-of x) y)
				julia-true julia-false)))
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
  (with-output-to-string '() (lambda () (julia-print x))))

(define (print-tuple x opn cls)
  (display opn)
  (let loop ((L (tuple-length x))
	     (i 0))
    (if (< i L)
	(begin (julia-print (tuple-ref x i))
	       (if (< i (- L 1))
		   (display ", ")
		   (if (= L 1)
		       (display ",")))
	       (loop L (+ i 1)))
	(display cls))))

(define (print-type t)
  (cond ((eq? t scalar-type)
	 (display "Tensor[Scalar, 0]"))
	((eq? (type-name t) 'Function)
	 (julia-print (tuple-ref (type-params t) 0))
	 (display "-->")
	 (julia-print (tuple-ref (type-params t) 1)))
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
   ((eq? (type-name (type-of x)) 'Buffer)
    (display "Buffer[")
    (display (type-name (type-param0 (type-of x))))
    (display "]:")
    (display (buffer-data x)))
   (else
    (let* ((t (type-of x))
	   (tn (type-name t)))
      (case tn
	((Int8 Uint8 Int16 Uint16 Int32 Uint32 Int64 Uint64 Single Double)
	 (display (j-unbox x)))
	((Bool) (if (j-false? x)
		    (display "false")
		    (display "true")))
	((Symbol) (display (vector-ref x 1)))
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
   _ _   _| |_  __ _ 2 |
  | | | | | | |/ _` |  |
  | | |_| | | | (_| |  |  \302\2512009 contributors
 _/ |\\__'_|_|_|\\__'_|  |  
|__/                   |\033[0m

"

"               _      
   _       _ _(_)_     |
  (_)     | (_) (_)    |  pre-release version
   _ _   _| |_  __ _ 2 |
  | | | | | | |/ _` |  |
  | | |_| | | | (_| |  |  \302\2512009 contributors
 _/ |\\__'_|_|_|\\__'_|  |  
|__/                   |

"
))

(define (julia-repl)
  (define (display-error-exception e)
    (display (error-exception-message e))
    (for-each (lambda (x)
		(display " ") (display x))
	      (error-exception-parameters e)))
  
  (j-load "start.j")
  (j-load "examples.j")
  (display banner)
  
  (let prompt ()
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
		 (and
		  (not (eq? expr 'Quit))
		  (not (eof-object? expr))
		  (begin
		    (if COLOR? (display "\033[1m\033[36m"))
		    (j-toplevel-eval
		     `(call print (quote ,(j-toplevel-eval expr)))))))))))
      (newline)
      (if continue?
	  (begin (newline) (prompt))))))
