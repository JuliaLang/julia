#|
TODO:
* local variable identification pass
* varargs
* apply, splat
- builtin scalar conversions, implicit conversion mechanism
  . separate type for literals
- global var declaration
- range objects
- indexing
- optional arguments
- quote, expr, and symbol types, user-level macros

not likely to be implemented in interpreter:
- more builtin functions (shifts, bitwise ops, primitive i/o)
- keywords
- modules
- more type checks
- try/catch
|#

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
			    (tuple? (vector-ref x 0)))))

; --- singleton null value ---

(define julia-null (julia-tuple))

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
(define (type-params-list t)
  (tuple->list (type-params t)))
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
	((eq? (vector-ref v 0) 'tuple)
	 (let ((tt (make-tuple-type (map type-of (tuple->list v)))))
	   (vector-set! v 0 tt)
	   tt))
	(else
	 (vector-ref v 0))))

(define (type? v) (and (vector? v) (or (tuple? v)
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

; --- type functions ---

(define (j-int32? x)
  (and (vector? x) (eq? (type-name (type-of x)) 'Int32)))

(define (instantiate-type type params)
  ; convert X-->Y to (X,)-->Y if X is not already a tuple or symbol
  (instantiate-type-
   type
   (if (and (pair? params)
	    (eq? (type-name type) 'Function)
	    (symbol? (type-param0 type))
	    (not (tuple? (car params)))
	    (not (symbol? (car params))))
       (cons (julia-tuple (car params))
	     (cdr params))
       params)))

(define (instantiate-type- type params)
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
    (define (check x)
      (or (and (or (symbol? x) (j-int32? x) (number? x)
		   (and (type? x) (not (sequence-type? x))))
	       x)
	  (and (j-symbol? x)
	       (j-unbox x))
	  (error "Invalid type parameter" x)))
    (cond ((null? tp)
	   (if (null? sp) tp
	       (error "Too many parameters for type" (type-name type))))
	  ((symbol? (car tp))
	   (if (null? sp)
	       (error "Too few parameters for type" (type-name type)))
	   (cons (check (car sp))
		 (new-params-list (cdr tp) (cdr sp))))
	  (else
	   (cons (car tp)
		 (new-params-list (cdr tp) sp)))))
  
  (let* ((tp  (type-params-list type))
	 (ts  (filter symbol? tp)))
    (cond ((null? params)
	   type)
	  ((eq? (type-name type) 'Union)
	   (make-union-type params))
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
		      (map (lambda (p) (lookup p env p))
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

(define (type-param0 t)
  (tuple-ref (type-params t) 0))

(define (type-param t n)
  (tuple-ref (type-params t) n))

(define (type-equal? a b)
  (and (subtype? a b)
       (subtype? b a)))

; tells whether child is assignable to parent
; returns #t, #f, or an assoc list witness showing the assignment of
; type parameters that makes the relation hold
(define (subtype? child parent) (subtype?- child parent '()))

(define (subtype?- child parent env)
  (cond ((eq? child parent)       env)
	((eq? parent tuple-type)  (and (tuple? child) env))
	
	((symbol? parent)
	 (let ((u (assq parent env)))
	   (if u
	       (and (or (eq? (cdr u) child)
			; multiple occurrences of the same
			; open parameter must be type-equal
			(and (not (symbol? child))
			     (type-equal? child (cdr u))))
		    env)
	       (cons (cons parent child) env))))
	
	((symbol? child) #f)
	((eq? parent any-type)    env)
	((eq? child any-type)     #f)
	
	((and (j-int32? child) (j-int32? parent))
	 (and (= (j-unbox child) (j-unbox parent))
	      env))
	
	; recursively handle union types
	((eq? (type-name child) 'Union)
	 (and
	  (every (lambda (t) (subtype?- t parent env))
		 (type-params-list child))
	  env))
	((eq? (type-name parent) 'Union)
	 (any   (lambda (t) (subtype?- child t env))
	        (type-params-list parent)))
	
	; buffers are invariant
	((and (eq? (type-name child) 'Buffer)
	      (eq? (type-name parent) 'Buffer))
	 (let ((cp0 (type-param0 child))
	       (pp0 (type-param0 parent)))
	   (and (or (symbol? pp0) (subtype?- pp0 cp0 env))
		(subtype?- cp0 pp0 env))))
	
	; functions are contravariant in first parameter
	((and (eq? (type-name child) 'Function)
	      (eq? (type-name parent) 'Function))
	 (let* ((p (type-param0 parent))
		(e (if (symbol? p)
		       (subtype?- (type-param0 child) p env)
		       (subtype?- p (type-param0 child) env))))
	   (and e
		(subtype?- (type-param child 1) (type-param parent 1) e))))
	
	; handle tuple types, or any sibling instantiations of the same
	; generic type. parameters must be consistent.
        ; examples:
        ; (a, a)  subtype  (b, c)  YES
	; (a, b)  subtype  (c, c)  NO
        ; (Int8, Int8)  subtype  (Int, Int)  YES
        ; (a, a)  subtype  (Int8, Int8)  NO
	((eq? (type-name child)
	      (type-name parent))
	 (let loop ((cp (type-params-list child))  ; child parameters
		    (pp (type-params-list parent)) ; parent parameters
		    (env env))
	   (let ((cseq (and (pair? cp) (sequence-type? (car cp))))
		 (pseq (and (pair? pp) (sequence-type? (car pp)))))
	     (cond
	      ((null? cp) (if (or (null? pp) pseq)
			      env
			      #f))
	      ((and cseq (not pseq)) #f)
	      ((null? pp)            #f)
	      (else
	       ; default to covariance
	       (let ((w (subtype?- (if cseq
				       (type-param0 (car cp))
				       (car cp))
				   (if pseq
				       (type-param0 (car pp))
				       (car pp))
				   env)))
		 (and w
		      ; if both end up on sequence types, and
		      ; parameter matched. stop with "yes" now,
		      ; otherwise we'd start looping forever
		      (if (and pseq cseq)
			  (append w env)
			  (loop (if cseq cp (cdr cp))
				(if pseq pp (cdr pp))
				(append w env))))))))))
	
	; otherwise walk up the type hierarchy
	(else (subtype?- (type-super child) parent env))))

; --- define some key builtin types ---

(define scalar-type (instantiate-type Tensor-type (list union-type 0)))
(put-type 'Scalar scalar-type)

(define real-type (make-abstract-type 'Real scalar-type julia-null julia-null))
(put-type 'Real real-type)
(define int-type (make-abstract-type 'Int real-type julia-null julia-null))
(put-type 'Int int-type)

(define bool-type (make-type 'Bool scalar-type julia-null julia-null))
(define int8-type (make-type 'Int8 int-type julia-null julia-null))
(define uint8-type (make-type 'Uint8 int-type julia-null julia-null))
(define int16-type (make-type 'Int16 int-type julia-null julia-null))
(define uint16-type (make-type 'Uint16 int-type julia-null julia-null))
(define int32-type (make-type 'Int32 int-type julia-null julia-null))
(define uint32-type (make-type 'Uint32 int-type julia-null julia-null))
(define int64-type (make-type 'Int64 int-type julia-null julia-null))
(define uint64-type (make-type 'Uint64 int-type julia-null julia-null))
(define float-type (make-type 'Float real-type julia-null julia-null))
(define double-type (make-type 'Double real-type julia-null julia-null))

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
(put-type 'Float float-type)
(put-type 'Double double-type)
(put-type 'Type Type-type)

(put-type 'Symbol symbol-type)
(put-type 'Buffer buffer-type)

; --- true and false values ---

(define julia-true (vector bool-type 1))
(define julia-false (vector bool-type 0))

; --- processing type-related syntax ---

(define (type-ex-name t)
  (cond ((not (pair? t)) t)
	((eq? (car t) 'tuple) 'tuple)
	((eq? (car t) '...)   '...)
	((eq? (car t) 'quote) 'quote)
	((eq? (car t) 'ref) (cadr t))
	((eq? (car t) '-->) 'Function)
	((and (eq? (car t) 'call)
	      (eq? (cadr t) 'ref))  (caddr t))
	((and (eq? (car t) 'call)
	      (eq? (cadr t) 'tuple))  'tuple)
	(else (error "Invalid type expression" t))))
(define (type-ex-params t)
  (cond ((not (pair? t)) '())
	((eq? (car t) 'tuple) (cdr t))
	((eq? (car t) '...)   (cdr t))
	((eq? (car t) 'quote) (cdr t))
	((eq? (car t) 'ref)   (cddr t))
	((eq? (car t) '-->)   (cdr t))
	((and (eq? (car t) 'call)
	      (eq? (cadr t) 'ref))  (cdddr t))
	((and (eq? (car t) 'call)
	      (eq? (cadr t) 'tuple))  (cddr t))
	(else (error "Invalid type expression" t))))

; handles form (typename name . params), giving a type object
(define (resolve-type name params)
  (define (resolve-params p)
    (map resolve-type-ex p))
  (cond ((eq? name 'tuple)
	 (list->tuple (resolve-params params)))
	((eq? name 'quote) (car params))
	((number? name)    (j-box int32-type name))
	((get-type name) => (lambda (x)
			      (instantiate-type
			       x
			       (resolve-params params))))
	(else
	 (error "Unknown type" name))))

; convert a symbolic type expression to a type object
(define (resolve-type-ex e)
  (resolve-type (type-ex-name e)
		(type-ex-params e)))

(define (ty s) (resolve-type-ex (julia-parse s)))

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
	; convert `T to T, since for a type def all parameters are
	; generic so ` does nothing.
	(tpara (map (lambda (x)
		      (if (and (pair? x)
			       (eq? (car x) 'quote))
			  (cadr x)
			  x))
		    (type-ex-params signature)))
	; fields looks like (block (:: n t) (:: n t) ...)
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
      (put-type tname T)
      T)))

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

(define (make-method-table) '())

(define (method-table-assoc methtable type)
  (let loop ((m methtable))
    (if (null? m)
	#f
	(if (subtype? type (caar m))
	    (car m)
	    (loop (cdr m))))))

(define (cons-in-order item lst key <)
  (if (null? lst)
      (list item)
      (if (< (key item) (key (car lst)))
	  (cons item lst)
	  (cons (car lst) (cons-in-order item (cdr lst) key <)))))

(define (method-table-insert mt type method)
  (let ((m (method-table-assoc mt type)))
    (if (and m (type-equal? type (car m)))
	(begin (set-car! m type)   ; replace existing key
	       (set-cdr! m method)
	       mt)
	(cons-in-order (cons type method) mt car subtype?))))

; --- generic functions ---

(define gf-type (instantiate-type function-type (list any-type any-type)))

(define (j-apply-generic ce args)
  (let* ((argtype (make-tuple-type (map type-of args)))
	 (meth  (best-method (vector-ref ce 0) argtype)))
    (if (not meth)
	(error "No method for function" (vector-ref ce 1)
	       "matching types" (map type-name (type-params-list argtype)))
	((closure-proc meth) (closure-env meth) args))))

(define (make-generic-function name)
  (make-closure gf-type j-apply-generic (vector (make-method-table) name)))

(define (best-method methtable argtype)
  (let ((m (method-table-assoc methtable argtype)))
    (and m (cdr m))))

(define (gf-mtable gf) (vector-ref (closure-env gf) 0))
(define (gf-set-mtable! gf mt) (vector-set! (closure-env gf) 0 mt))
(define (gf-name gf) (vector-ref (closure-env gf) 1))

; add a method for certain types
(define (add-method-for gf types meth)
  (gf-set-mtable! gf (method-table-insert (gf-mtable gf) types meth))
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
	(eq? (type-name t) 'Union)
	(eq? (type-name t) 'Function)))
  (if (or (non-convertible? from)
	  (non-convertible? to))
      (error "Conversions may not be defined for the specified type(s)"))
  (if (type-equal? from to)
      (error "Cannot define a conversion from a type to itself"))
  (set! conversion-table
	(method-table-insert conversion-table
			     ; NOTE the "flipped" function type, "to-->from"
			     ; is necessary because method-table looks for
			     ; a method supporting a superset of the needed
			     ; behavior, and for conversions we need a type
			     ; providing a subset of the requested behavior.
			     (make-function-type to from)
			     method))
  julia-null)

(define (get-conversion from to)
  (let ((m (method-table-assoc conversion-table
			       (make-function-type to from))))
    (and m (cdr m))))

(define (j-convert x to-type)
  (let* ((t (type-of x))
	 (m (get-conversion t to-type)))
    (if m
	(let ((result (j-apply m (list x))))
	  (if (subtype? (type-of result) to-type)
	      result
	      (error "Conversion to" (type-name to-type) "failed")))
	(error "No conversion from" (type-name t) "to" (type-name to-type)))))

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
			      (vector-ref v i)))

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

(define (make-builtin name T impl)
  (table-set! julia-globals name
	      (make-closure
	       (resolve-type-ex (julia-parse T))
	       (lambda (ce args) (apply impl args))
	       #f)))

(define (j-box type v) (vector type v))
(define (j-unbox v) (vector-ref v 1))
(define (j-box-set b v) (begin (vector-set! b 1 v) julia-null))

; fix scalar type to include a proper int32(0)
; this creates a circular reference
(vector-set! (vector-ref scalar-type 3) 2 (j-box int32-type 0))
; set element type of scalar to scalar
(vector-set! (vector-ref scalar-type 3) 1 scalar-type)

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

; the following builtin functions are ordinary first-class functions,
; and can be directly employed by the user.
(make-builtin 'new "(Type,Any...)-->Any" (lambda (t . args) (j-new t args)))
(make-builtin 'getfield "(Any,Symbol)-->Any" j-get-field)
(make-builtin 'setfield "(Any,Symbol,Any)-->Any" j-set-field)
(make-builtin 'is "(Any,Any)-->Bool" j-is)
(make-builtin 'typeof "(Any,)-->Type" type-of)
(make-builtin 'subtype "(Type,Type)-->Bool"
	      (lambda (x y) (if (subtype? x y)
				julia-true julia-false)))
(make-builtin 'istype "(Any,Type)-->Bool"
	      (lambda (x y) (if (subtype? (type-of x) y)
				julia-true julia-false)))
(make-builtin 'tuple "(Any...)-->Tuple" j-tuple)
(make-builtin 'apply "(Function[`A,`T],Tuple...)-->`T"
	      (lambda (f . argt)
		(j-apply f (apply append (map tuple->list argt)))))
(make-builtin 'error "Any-->Union" error)
(make-builtin 'convert "(Any,Type)-->Any" j-convert)

; for timing
(make-builtin 'time_thunk "(()-->Any)-->Any"
	      (lambda (f) (time (j-apply f '()))))

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
(make-builtin 'new_closure "(Type,Any,Tuple)-->Function"
	      (lambda (t e clo)
		(make-closure t j-eval-body (cons e clo))))
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


; --- evaluator ---

(define (scm->julia x)
  (if (symbol? x)
      (vector symbol-type x)
      x))

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
	   
	   ((type)    (type-def (cadr e) (caddr e)))
	   
	   ((closure-ref) (tuple-ref (cdr env) (cadr e)))
	   
	   ((addmethod)
	    (let* ((name (cadr e))
		   (gf (or (and (j-bound? name env)
				(not (eq? (eval-sym name env) julia-null))
				(eval-sym name env))
			   (make-generic-function name))))
	      (if (not (generic-function? gf))
		  (error "Variable" name "does not name a function")
		  (add-method gf (j-eval (caddr e) env)))
	      gf))
	   
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

; create an environment with actual args bound to formal args
(define (bind-args names args)
  (if (null? names)
      '()
      (let* ((formal (car names))
	     (name   (arg-name formal))
	     (bind
	      (if (and (pair? formal) (eq? (car formal) '...))
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

; --- load ---

(define (j-load fname)
  (for-each j-toplevel-eval (julia-parse-file fname))
  julia-null)

(make-builtin 'load "(Any,)-->()" j-load)

; --- print and repl ---

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
   ((tuple? x)
    (print-tuple x "(" ")"))
   ((type? x)
    (print-type x))
   ((eq? (type-name (type-of x)) 'Buffer)
    (display "Buffer(")
    (display (type-name (tuple-ref (type-params (type-of x)) 0)))
    (display "):")
    (display (buffer-data x)))
   (else
    (let* ((t (type-of x))
	   (tn (type-name t)))
      (case tn
	((Int8 Uint8 Int16 Uint16 Int32 Uint32 Int64 Uint64 Double Float)
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
  (j-load "start.j")
  (j-load "array.j")
  (j-load "examples.j")
  (display banner)

  (let prompt ()
    (if COLOR? (display "\033[0m"))
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
	       (prompt))
	     (lambda ()
	       (if COLOR? (display "\033[1m\033[36m"))
	       (j-toplevel-eval
		`(call print (quote ,(j-toplevel-eval (julia-parse line)))))
	       (newline)
	       (newline)
	       (prompt))))))))
