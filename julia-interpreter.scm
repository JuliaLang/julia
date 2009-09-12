(define julia-types (make-table))
(define julia-globals (make-table))

; --- tuples ---

; note: every julia value is represented as a vector whose first
;       element is the type of the value

(define (julia-tuple . args) (apply vector 'tuple args))

(define (tuple->list t) (cdr (vector->list t)))
(define (list->tuple l) (apply julia-tuple l))

(define (tuple-append t1 t2)
  (list->tuple (append (tuple->list t1)
		       (tuple->list t2))))

(define (tuple-ref t i) (vector-ref t (+ i 1)))
(define (tuple-length t) (- (vector-length t) 1))

; --- singleton null value ---

(define julia-null (julia-tuple))

; --- type objects, reflection ---

; in any-type and Type-type we take pains to avoid circular references
; because gambit can't deal with them well, so there are symbols where
; type objects should go.
(define any-type (vector 'Type 'any 'any julia-null julia-null))

(define Type-type (vector 'Type 'Type any-type julia-null
			  (julia-tuple
			   (julia-tuple 'name 'symbol)
			   (julia-tuple 'super 'Type)
			   (julia-tuple 'parameters 'tuple)
			   (julia-tuple 'fields 'tuple))))

(define (make-type name super params fields)
  (vector Type-type name super params fields))

(define (type-name t) (vector-ref t 1))
(define (type-super t)
  (if (eq? t any-type)
      any-type
      (vector-ref t 2)))
(define (type-params t) (vector-ref t 3))
(define (type-params-list t) (tuple->list (type-params t)))
(define (type-fields t) (vector-ref t 4))

; get the type of a value
(define (type-of v)
  (cond ((eq? v Type-type)
	 Type-type)
	((eq? v any-type)
	 Type-type)
	((eq? v julia-null)
	 julia-null) ; again to avoid circular references
	((eq? (vector-ref v 0) 'tuple)
	 (let ((tt (make-type 'tuple any-type
			      (list->tuple
			       (map type-of (tuple->list v)))
			      julia-null)))
	   (vector-set! v 0 tt)
	   tt))
	(else
	 (vector-ref v 0))))

(define (type? v) (eq? (type-of v) Type-type))

; --- define primitive types ---

(define boolean-type (make-type 'boolean any-type julia-null julia-null))
(define int8-type (make-type 'int8 any-type julia-null julia-null))
(define uint8-type (make-type 'uint8 any-type julia-null julia-null))
(define int16-type (make-type 'int16 any-type julia-null julia-null))
(define uint16-type (make-type 'uint16 any-type julia-null julia-null))
(define int32-type (make-type 'int32 any-type julia-null julia-null))
(define uint32-type (make-type 'uint32 any-type julia-null julia-null))
(define int64-type (make-type 'int64 any-type julia-null julia-null))
(define uint64-type (make-type 'uint64 any-type julia-null julia-null))
(define float-type (make-type 'float any-type julia-null julia-null))
(define double-type (make-type 'double any-type julia-null julia-null))
(define size-type (make-type 'size any-type julia-null julia-null))

(define symbol-type (make-type 'symbol any-type julia-null julia-null))
(define buffer-type (make-type 'buffer any-type (julia-tuple 'T) julia-null))
(define function-type (make-type 'function any-type julia-null
				 (julia-tuple
				  (julia-tuple 'methods any-type))))

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
(table-set! julia-types 'function function-type)

; --- singleton true and false values ---

(define julia-true (vector boolean-type 1))
(define julia-false (vector boolean-type 0))

; --- type functions ---

(define (instantiate-type type params)
  (let ((tp  (type-params-list type)))
    (cond ((not (= (length tp)
		   (length params)))
	   (error "Wrong number of parameters for type" (type-name type)))
	  ((null? params)
	   type)
	  (else
	   (make-type (type-name type)
		      (type-super type)
		      (list->tuple params)
		      (let ((flds (map tuple->list
				       (tuple->list (type-fields type))))
			    (env  (map cons tp params)))
			(let ((fnames (map car flds))
			      (ftypes (map cadr flds)))
			  (let ((mapped-ftypes
				 (map (lambda (t)
					(instantiate-type
					 t
					 (map (lambda (p)
						(let ((u (assq p env)))
						  (if u (cdr u) p)))
					      (type-params-list t))))
				      ftypes)))
			    (list->tuple
			     (map list->tuple
				  (map list fnames mapped-ftypes)))))))))))

(define (type-generic? t)
  (and (type? t)
       (any symbol? (type-params-list t))))

(define (type-equal? a b)
  (and (subtype? a b)
       (subtype? b a)))

; tells whether child is assignable to parent
; returns #t, #f, or an assoc list witness showing the assignment of
; type parameters that makes the relation hold
(define (subtype? child parent)
  (cond ; see if the answer is obvious
        ((eq? child parent)    #t)
	((eq? parent any-type) #t)
	((eq? child any-type)  #f)

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
	 (let loop ((cp (type-params-list child))
		    (pp (type-params-list parent))
		    (env '()))
	   (cond ((null? cp) (if (null? pp) (if (null? env) #t env) #f))
		 ((null? pp) #f)
		 ((symbol? (car pp))
		  (let ((u (assq (car pp) env)))
		    (if u
			(and (or (eq? (cdr u)
				      (car cp))
				 (and (not (symbol? (car cp)))
				      (type-equal? (car cp) (cdr u))))
			     env)
			(loop (cdr cp)
			      (cdr pp)
			      (cons (cons (car pp) (car cp)) env)))))
		 ((symbol? (car cp)) #f)
		 ((subtype? (car cp) (car pp)) =>
		  (lambda (w)
		    (loop (cdr cp) (cdr pp) (if (pair? w)
						(append w env)
						env))))
		 (else #f))))

	; otherwise walk up the type hierarchy
	(else (subtype? (type-super child) parent))))

; --- processing type-related syntax ---

(define (type-ex-name t)
  (cond ((symbol? t) t)
	((eq? (car t) 'type) (cadr t))
	((eq? (car t) 'call) (cadr t))
	(else (error "Invalid type expression" t))))
(define (type-ex-params t)
  (cond ((symbol? t) '())
	((eq? (car t) 'type) (cddr t))
	((eq? (car t) 'call) (cddr t))
	(else (error "Invalid type expression" t))))

; handles form (type name . params), giving a type object
(define (resolve-type name params . nested?)
  (cond ((or (eq? name 'tuple) (eq? name 'union))
	 (make-type name any-type
		    (list->tuple
		     (map (lambda (t)
			    (resolve-type (type-ex-name t)
					  (type-ex-params t) #t))
			  params))
		    julia-null))
	#;((assq name type-env) => (lambda (x)
				   (instantiate-type (cdr x) params)))
	((table-ref julia-types name #f) => (lambda (x)
					      (instantiate-type x params)))
	((pair? nested?) name)
	(else
	 (error "Unknown type" name))))

(define (resolve-type-ex e)
  (resolve-type (type-ex-name e)
		(type-ex-params e)))

(define (type-def signature fields)
  (let ((tname (type-ex-name signature))
	(tpara (type-ex-params signature))
	; fields looks like (block (: n t) (: n t) ...)
	(fnames (map cadr (cdr fields)))
	(ftypes (map (lambda (fld)
		       (resolve-type-ex (caddr fld)))
		     (cdr fields))))
    (let ((T
	   (make-type tname any-type tpara
		      (list->tuple
		       (map list->tuple
			    (map list fnames ftypes))))))
      (table-set! julia-types tname T)
      T)))

(define (type-alias name type)
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

(define (method-matches? mtypes types pred)
  (cond ((null? mtypes) (null? types))
	((null? types)  #f)
	((pred (car types) (car mtypes))
	 (method-matches? (cdr mtypes) (cdr types) pred))
	(else #f)))

; is type signature ta more specific than tb?
; assumes ta and tb are the same length
(define (more-specific? ta tb)
  (if (null? ta) #f
      (or
       (and (subtype? (car ta) (car tb))
	    (not (subtype? (car tb) (car ta))))
       (more-specific? (cdr ta) (cdr tb)))))

(define (matching-methods gf types pred)
  (let ((meths (vector-ref gf 1)))
    (filter (lambda (a)
	      (method-matches? (car a) types pred))
	    meths)))

(define (most-specific ml)
  (let loop ((curr (car ml))
	     (ml   (cdr ml)))
    (if (null? ml)
	curr
	(loop (if (more-specific? (caar ml)
				  (car curr))
		  (car ml)
		  curr)
	      (cdr ml)))))

(define (add-method gf types m)
  (let ((matches (matching-methods gf types type-equal?)))
    (if (null? matches)
	(vector-set! gf 1 (cons (cons types m) (vector-ref gf 1)))
	(let ((match (most-specific matches)))
	  ; overwrite existing exactly matching method
	  (begin (set-car! match types)
		 (set-cdr! match m)))))
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

(define (j-get-field obj fld)
  (vector-ref obj (field-offset obj fld)))

(define (j-set-field obj fld v)
  ; TODO: type check
  (vector-set! obj (field-offset obj fld) v))

(define (j-ref v i) (vector-ref v (+ i 1)))

(define (j-set v i rhs) (vector-set! v (+ i 1) rhs))

(define (j-buffer-length v) (- (vector-length v) 1))

(define (j-not x) (if (eq? x julia-false) julia-true julia-false))

(define (j-new type args)
  (let* ((L (if (memq (type-name type) '(tuple buffer))
		(length args)
		(tuple-length (type-fields type))))
	 (v (make-vector (+ 1 L))))
    (vector-set! v 0 type)
    (let loop ((i 0)
	       (args args))
      (if (< i L)
	  (if (null? args)
	      (error "Too few arguments to type constructor" (type-name type))
	      (begin (vector-set! v (+ 1 i) (car args))
		     (loop (+ i 1) (cdr args))))
	  (if (pair? args)
	      (error "Too many arguments to type constructor"
		     (type-name type))
	      v)))))

(define (j-typeof x) (type-of x))

(define (j-is x y) (eq? x y))

(define (make-builtin name impl)
  (table-set! julia-globals name impl))

(define (j-box type v) (vector type v))
(define (j-unbox v) (vector-ref v 1))

(make-builtin 'getfield j-get-field)
(make-builtin 'setfield j-set-field)
(make-builtin 'refany j-ref)
(make-builtin 'tupleref j-ref)
(make-builtin 'setany j-set)
(make-builtin 'bufferlength j-buffer-length)
(make-builtin 'not j-not)
(make-builtin 'typeof j-typeof)
(make-builtin 'is j-is)
(make-builtin 'tuple julia-tuple)
(make-builtin 'box j-box)
(make-builtin 'unbox j-unbox)

(make-builtin 'add_int32 +)
(make-builtin 'add_double +)
(make-builtin 'sub_int32 -)
(make-builtin 'sub_double -)
(make-builtin 'neg_int32 -)
(make-builtin 'neg_double -)
(make-builtin 'mul_int32 *)
(make-builtin 'mul_double *)
(make-builtin 'div_int32 (lambda (x y)
			   (let ((q (/ x y)))
			     (if (< q 0)
				 (ceiling q)
				 (floor q)))))
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


; --- evaluator ---

(define (scm->julia x)
  (if (symbol? x)
      (vector 'symbol x)
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
	   ((while)   (let loop ((v julia-false))
			(if (not (eq? (j-eval (cadr e) env) julia-false))
			    (loop (j-eval (caddr e) env))
			    v)))
	   ((return)  (raise `(julia-return ,(if (pair? (cdr e))
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
			      (map resolve-type-ex (caddr e))
			      (j-eval (cadddr e) env)))
	      gf))

	   ((local)
	    (set! env (cons (cons (cadr e) julia-null) env)))
	   ((=)
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
		     (assert (generic-function? f))
		     (j-apply-generic f args)))))
	   (else
	    (error "Unhandled tree type" (car e)))))))

(define (j-apply-closure f args)
  (with-exception-catcher
   (lambda (e)
     (if (and (pair? e)
	      (eq? (car e) 'julia-return))
	 (cadr e)
	 (raise e)))
   (lambda ()
     (j-eval (vector-ref f 2)
	     (append (map cons
			  (vector-ref f 1)
			  args)
		     (vector-ref f 3))))))

(define (j-apply-generic gf args)
  (let* ((types (map type-of args))
	 (meths (matching-methods gf types subtype?)))
    (if (null? meths)
	(error "No method for function" (vector-ref gf 2)
	       "matching types" (map type-name types))
	(j-apply-closure (cdr (most-specific meths)) args))))

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
   ((or (eq? (vector-ref x 0) 'closure)
	(eq? (vector-ref x 0) 'function)
	(eq? (vector-ref x 0) 'symbol))
    (display x))
   ((or (eq? (vector-ref x 0) 'tuple)
	(eq? (type-name (type-of x)) 'tuple))
    (print-tuple x))
   ((eq? (type-name (type-of x)) 'buffer)
    (display "buffer(")
    (display (tuple-ref (type-params (type-of x)) 0))
    (display "):")
    (print-tuple x))
   (else
    (let* ((t (type-of x))
	   (tn (type-name t)))
      (case tn
	((int32 double) (display (j-unbox x)))
	((boolean) (if (eq? x julia-false)
		       (display "false")
		       (display "true")))
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

(define (julia-repl)
  (display "julia> ")
  (let ((line (read-line)))
    (if (eof-object? line)
	(newline)
	(begin 
	  (with-exception-catcher
	   (lambda (e)
	     (display (error-exception-message e))
	     (for-each (lambda (x)
			 (display " ") (display x))
		       (error-exception-parameters e))
	     (newline)
	     (newline)
	     (julia-repl))
	   (lambda ()
	     (julia-print (j-toplevel-eval (julia-parse line)))
	     (newline)
	     (newline)
	     (julia-repl)))))))
