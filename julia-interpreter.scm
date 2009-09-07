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

(define symbol-type (make-type 'symbol any-type julia-null julia-null))
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

(define vector-type (make-type 'vector any-type (julia-tuple 'T) julia-null))

(table-set! julia-types 'any any-type)
(table-set! julia-types 'symbol symbol-type)
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
(table-set! julia-types 'vector vector-type)

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

(define (type-def signature fields)
  (let ((tname (type-ex-name signature))
	(tpara (type-ex-params signature))
	; fields looks like (block (: n t) (: n t) ...)
	(fnames (map cadr (cdr fields)))
	(ftypes (map (lambda (fld)
		       (resolve-type (type-ex-name (caddr fld))
				     (type-ex-params (caddr fld))))
		     (cdr fields))))
    (let ((T
	   (make-type tname any-type tpara
		      (list->tuple
		       (map list->tuple
			    (map list fnames ftypes))))))
      (table-set! julia-types tname T)
      T)))

(define (type-alias name type)
  (table-set! julia-types name (resolve-type (type-ex-name type)
					     (type-ex-params type))))

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
  (vector-set! obj (field-offset obj fld) v))

(define (j-ref v i) (vector-ref v (+ i 1)))

(define (j-set v i rhs) (vector-set! v (+ i 1) rhs))

(define (j-vector-length v) (- (vector-length v) 1))

(define (j-not x) (if (eq? x julia-false) julia-true julia-false))

(define (j-new type args)
  (let* ((L (if (memq (type-name type) '(tuple vector))
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
