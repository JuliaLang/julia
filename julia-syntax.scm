; TODO:
; * expand lvalues, e.g. (= (call ref A i) x) => (= A (call assign A i x))
; * expand operators like +=
; * expand for into while
; * expand -> and function into lambda/addmethod
; * replace (. a b) with (call get a (quote b))
; * tuple destructuring
; - validate argument lists, replace a=b in arg lists with keyword exprs
; - type parameter renaming stuff
; - use (top x) more consistently
; * make goto-form safe for inlining (delay label to index mapping)

(define (quoted? e) (memq (car e) '(quote top line break)))

(define (lam:args x) (cadr x))
(define (lam:vars x) (llist-vars (lam:args x)))
(define (lam:vinfo x) (caddr x))
(define (lam:body x) (cadddr x))

; convert x => (x), (tuple x y) => (x y)
; used to normalize function signatures like "x->y" and "function +(a,b)"
(define (fsig-to-lambda-list arglist)
  (if (pair? arglist)
      (if (eq? (car arglist) 'tuple)
	  (cdr arglist)
	  arglist)
      (if (symbol? arglist)
	  (list arglist)
	  arglist)))

(define (arg-name v)
  (if (symbol? v)
      v
      (case (car v)
	((...)         (decl-var (cadr v)))
	((= keyword)   (decl-var (caddr v)))
	((|::|)        (decl-var v))
	(else (error "malformed function argument" v)))))

; convert a lambda list into a list of just symbols
(define (llist-vars lst)
  (map arg-name lst))

; get just argument types
(define (llist-types lst)
  (map (lambda (v)
	 (cond ((symbol? v)  'Any)
	       ((not (pair? v))
		(error "malformed function arguments"))
	       (else
		(case (car v)
		  ((...)         `(... ,(decl-type (cadr v))))
		  ((= keyword)   (decl-type (caddr v)))
		  ((|::|)        (decl-type v))
		  (else (error "malformed function arguments" lst))))))
       lst))

; get the variable name part of a declaration, x::int => x
(define (decl-var v)
  (if (and (pair? v) (eq? (car v) '|::|))
      (cadr v)
      v))

(define (decl-type v)
  (if (and (pair? v) (eq? (car v) '|::|))
      (caddr v)
      'Any))

; make an expression safe for multiple evaluation
; for example a[f(x)] => (temp=f(x); a[temp])
; retuns a pair (expr . assignments)
; where 'assignments' is a list of needed assignment statements
(define (remove-argument-side-effects e)
  (let ((a '()))
    (if (not (pair? e))
	(cons e '())
	(cons (map (lambda (x)
		     (if (pair? x)
			 (let ((g (gensym)))
			   (set! a (cons `(= ,g ,x) a))
			   g)
			 x))
		   e)
	      (reverse a)))))

(define (expand-update-operator op lhs rhs)
  (let ((e (remove-argument-side-effects lhs)))
    `(block ,@(cdr e) (= ,(car e) (call ,op ,(car e) ,rhs)))))

; (a > b > c) => (&& (call > a b) (call > b c))
(define (expand-compare-chain e)
  (if (length> e 3)
      (let ((arg2 (caddr e)))
	(if (pair? arg2)
	    (let ((g (gensym)))
	      `(&& (call ,(cadr e) ,(car e) (= ,g ,arg2))
		   ,(expand-compare-chain (cons g (cdddr e)))))
	    `(&& (call ,(cadr e) ,(car e) ,arg2)
		 ,(expand-compare-chain (cddr e)))))
      `(call ,(cadr e) ,(car e) ,(caddr e))))

; : inside indexing means :1:
; a:b and a:b:c are ranges instead of calls to colon
(define (process-indexes i)
  (map (lambda (x)
	 (cond ((eq? x ':) '(: (: 1 :)))
	       ((and (pair? x)
		     (eq? (car x) ':)
		     (length= x 3)
		     (not (eq? (caddr x) ':)))
		`(call (top Range1) ,(cadr x) ,(caddr x)))
	       ((and (pair? x)
		     (eq? (car x) ':)
		     (length= x 4)
		     (not (eq? (cadddr x) ':)))
		`(call (top Range) ,@(cdr x)))
	       (else       x)))
       i))

(define (function-expr argl body)
  (let ((t (llist-types argl))
	(n (llist-vars argl)))
    (let ((argl (map (lambda (n t) `(|::| ,n ,t))
		     n t)))
      `(lambda ,argl
	 (scope-block ,body)))))

(define (symbols->typevars sl upperbounds)
  (if (null? upperbounds)
      (map (lambda (x)    `(call (top typevar) ',x)) sl)
      (map (lambda (x ub) `(call (top typevar) ',x ,ub)) sl upperbounds)))

(define (gf-def-expr- name argl argtypes body)
  `(block
    ,(if (symbol? name)
	 `(if (unbound ,name)
	      (= ,name (call (top new_generic_function) (quote ,name))))
	 `(null))
    (call (top add_method)
	  ,name
	  ,argtypes
	  ,(function-expr argl body))))

(define (sparam-name-bounds sparams names bounds)
  (cond ((null? sparams)
	 (values (reverse names) (reverse bounds)))
	((symbol? (car sparams))
	 (sparam-name-bounds (cdr sparams) (cons (car sparams) names)
			     (cons '(top Any) bounds)))
	((and (length= (car sparams) 4)
	      (eq? (caar sparams) 'comparison)
	      (eq? (caddar sparams) '|<:|))
	 (sparam-name-bounds (cdr sparams) (cons (cadr (car sparams)) names)
			     (cons (cadddr (car sparams)) bounds)))
	(else
	 (error "malformed static parameter list"))))

(define (generic-function-def-expr name sparams argl body)
  (let* ((argl  (fsig-to-lambda-list argl))
	 (types (llist-types argl)))
    (gf-def-expr-
     name argl
     (if (null? sparams)
	 `(tuple ,@types)
	 (receive
	  (names bounds) (sparam-name-bounds sparams '() '())
	  `(scope-block
	    (block
	     ,@(map (lambda (var val) `(= ,var ,val))
		    names
		    (symbols->typevars names bounds))
	     (tuple ,@types)))))
     body)))

(define (struct-def-expr name params super fields)
  (receive
   (params bounds) (sparam-name-bounds params '() '())
   (struct-def-expr- name params bounds super fields)))

(define *ctor-factory-name* (gensym))

(define (struct-def-expr- name params bounds super fields)
  (receive
   (fields defs) (separate (lambda (x) (or (symbol? x)
					   (and (pair? x)
						(eq? (car x) '|::|))))
			   fields)
   (let ((field-names (map decl-var fields))
	 (field-types (map decl-type fields))
	 (T (gensym)))
     `(call
       (lambda (,@params)
	 ; the static parameters are bound to new TypeVars in here,
	 ; so everything that sees the TypeVars is evaluated here.
	 (block
	  (local! ,*ctor-factory-name*)
	  (local! ,T)
	  ,(if (null? defs)
	       `(null)
	       ; create a function that defines constructors, given
	       ; the type and a "new" function.
	       ; a mild hack to feed inference the type of "new":
	       ; when "new" is created internally by the runtime, it is
	       ; tagged with a specific function type. this type is captured
	       ; as a static parameter which is visible to inference through
	       ; a declaration.
	       `(scope-block
		 (block
		  (function (call (curly ,*ctor-factory-name* ,T)
				  ,name (|::| new ,T))
			    (block
			     (|::| new ,T)
			     ,@defs
			     (null))))))
	  (= ,name
	     (call (top new_struct_type)
		   (quote ,name)
		   ,super
		   (tuple ,@params)
		   (tuple ,@(map (lambda (x) `',x) field-names))
		   ,(if (null? defs)
			`(null)
			; pass constructor factory function
			*ctor-factory-name*)))
	  ; now add the type fields, which might reference the type itself.
	  (call (top new_struct_fields)
		,name (tuple ,@field-types))))
       ,@(symbols->typevars params bounds)))))

(define (type-def-expr name params super)
  (receive
   (params bounds)
   (sparam-name-bounds params '() '())
   `(block
     (call
      (lambda ,params
	(block
	 (= ,name
	    (call (top new_tag_type)
		  (quote ,name)
		  ,super
		  (tuple ,@params)))))
      ,@(symbols->typevars params bounds)))))

(define *anonymous-generic-function-name* (gensym))

; patterns that introduce lambdas
(define binding-form-patterns
  (pattern-set
   ; function with static parameters
   (pattern-lambda (function (call (curly name . sparams) . argl) body)
		   (generic-function-def-expr name sparams argl body))

   ; function definition
   (pattern-lambda (function (call name . argl) body)
		   (generic-function-def-expr name '() argl body))

   (pattern-lambda (function (-- arg (-s)) body)
		   `(-> ,arg ,body))
   (pattern-lambda (function (tuple . args) body)
		   `(-> (tuple ,@args) ,body))

   ; expression form function definition
   (pattern-lambda (= (call (curly name . sparams) . argl) body)
		   `(function (call (curly ,name . ,sparams) . ,argl) ,body))
   (pattern-lambda (= (call name . argl) body)
		   `(function (call ,name ,@argl) ,body))

   (pattern-lambda (-> a b)
		   (let ((a (if (and (pair? a)
				     (eq? (car a) 'tuple))
				(cdr a)
				(list a))))
					; TODO: anonymous generic function
		     (function-expr a b)))

   (pattern-lambda (let binds ex)
		   (let loop ((binds binds)
			      (args  ())
			      (inits ())
			      (locls ()))
		     (if (null? binds)
			 `(call (-> (tuple ,@args)
				    (block (local (tuple ,@locls))
					   ,ex))
				,@inits)
			 (cond
			  ((symbol? (car binds))
			   (loop (cdr binds) args inits
				 (cons (car binds) locls)))
			  ((and (length= (car binds) 3)
				(eq? (caar binds) '=)
				(symbol? (cadar binds)))
			   (loop (cdr binds)
				 (cons (cadar binds) args)
				 (cons (caddar binds) inits)
				 locls))
			  (else (error "invalid let syntax"))))))

   ; type definition
   (pattern-lambda (struct (-- name (-s)) (block . fields))
		   (struct-def-expr name '() 'Any fields))

   (pattern-lambda (struct (curly (-- name (-s)) . params) (block . fields))
		   (struct-def-expr name params 'Any fields))

   (pattern-lambda (struct (comparison (-- name (-s)) (-/ |<:|) super)
			   (block . fields))
		   (struct-def-expr name '() super fields))

   (pattern-lambda (struct (comparison (curly (-- name (-s)) . params)
				       (-/ |<:|) super)
			   (block . fields))
		   (struct-def-expr name params super fields))

   )) ; binding-form-patterns

(define (make-assignment l r) `(= ,l ,r))

(define patterns
  (pattern-set
   (pattern-lambda (--> a b)
		   `(curly (top Function) ,a ,b))

   (pattern-lambda (|.| a b)
		   `(call (top getfield) ,a (quote ,b)))

   (pattern-lambda (= (|.| a b) rhs)
		   `(call (top setfield) ,a (quote ,b) ,rhs))

   (pattern-lambda (type (-- name (-s)))
		   (type-def-expr name '() 'Any))
   (pattern-lambda (type (curly (-- name (-s)) . params))
		   (type-def-expr name params 'Any))
   (pattern-lambda (type (comparison (-- name (-s)) (-/ |<:|) super))
		   (type-def-expr name '() super))
   (pattern-lambda (type (comparison (curly (-- name (-s)) . params)
				     (-/ |<:|) super))
		   (type-def-expr name params super))

   ; typealias is an assignment; should be const when that exists
   (pattern-lambda (typealias (-- name (-s)) type-ex)
		   `(= ,name ,type-ex))
   (pattern-lambda (typealias (curly (-- name (-s)) . params) type-ex)
		   `(call (lambda ,params
			    (= ,name (call (top new_type_constructor)
					   (tuple ,@params) ,type-ex)))
			  ,@(receive
			     (params bounds)
			     (sparam-name-bounds params '() '())
			     (symbols->typevars params bounds))))

   (pattern-lambda (comparison . chain) (expand-compare-chain chain))

   ; multiple value assignment
   (pattern-lambda (= (tuple . lhss) x)
		   (if (and (pair? x) (pair? lhss) (eq? (car x) 'tuple)
			    (length= lhss (length (cdr x))))
		       ; (a, b, ...) = (x, y, ...)
		       (let ((temps (map (lambda (x) (gensym)) (cddr x))))
			 `(block
			   ,@(map make-assignment temps (cddr x))
			   (= ,(car lhss) ,(cadr x))
			   ,@(map make-assignment (cdr lhss) temps)
			   (null)))
		       ; (a, b, ...) = other
		       (let ((t (gensym)))
			 `(block
			   (= ,t ,x)
			   ,@(let loop ((lhs lhss)
					(i   1))
			       (if (null? lhs) '((null))
				   (cons `(= ,(car lhs)
					     (call (top tupleref) ,t ,i))
					 (loop (cdr lhs)
					       (+ i 1)))))))))

   (pattern-lambda (= (ref a . idxs) rhs)
		   `(call assign ,a ,rhs ,@(process-indexes idxs)))

   (pattern-lambda (ref a . idxs)
		   `(call ref ,a ,@(process-indexes idxs)))

   (pattern-lambda (curly type . elts)
		   `(call (top instantiate_type) ,type ,@elts))

   ; call with splat
   (pattern-lambda (call f ... (... _) ...)
		   (let ((argl (cddr __)))
		     ; wrap sequences of non-... arguments in tuple()
		     (define (tuple-wrap a run)
		       (if (null? a)
			   (if (null? run) '()
			       (list `(call (top tuple) ,@(reverse run))))
			   (let ((x (car a)))
			     (if (and (length= x 2)
				      (eq? (car x) '...))
				 (if (null? run)
				     (list* (cadr x)
					    (tuple-wrap (cdr a) '()))
				     (list* `(call (top tuple) ,@(reverse run))
					    (cadr x)
					    (tuple-wrap (cdr a) '())))
				 (tuple-wrap (cdr a) (cons x run))))))
		     `(call apply ,f ,@(tuple-wrap argl '()))))

   ; tuple syntax (a, b...)
   ; note, directly inside tuple ... means sequence type
   (pattern-lambda (tuple . args)
		   `(call (top tuple)
			  ,@(map (lambda (x)
				   (if (and (length= x 2)
					    (eq? (car x) '...))
				       `(curly ... ,(cadr x))
				       x))
				 args)))

   (pattern-lambda (... a) `(curly ... ,a))

   ; local x,y,z => local x;local y;local z
   (pattern-lambda (local (tuple . vars))
		   `(block
		     ,@(map (lambda (x) `(local ,x)) vars)))

   ; local x::int=2 => local x::int; x=2
   (pattern-lambda (local (= var rhs))
		   `(block (local ,var)
			   (= ,(decl-var var) ,rhs)))

   ; global x,y,z => global x;global y;global z
   (pattern-lambda (global (tuple . vars))
		   `(block
		     ,@(map (lambda (x) `(global ,x)) vars)))

   ; global x=2 => global x;x=2
   (pattern-lambda (global (= var rhs))
		   `(block (global ,var) (= ,var ,rhs)))

   ; x::T = rhs => x::T; x = rhs
   (pattern-lambda (= (|::| x T) rhs)
		   (let ((e (remove-argument-side-effects x)))
		     `(block ,@(cdr e)
			     (|::| ,(car e) ,T)
			     (= ,(car e) ,rhs))))

   ; adding break/continue support to while loop
   (pattern-lambda (while cnd body)
		   `(scope-block
		     (break-block loop-exit
				  (_while ,cnd
					  (break-block loop-cont
						       ,body)))))

   (pattern-lambda (break) '(break loop-exit))
   (pattern-lambda (continue) '(break loop-cont))

   ;; for loops

   ; for loop over ranges
   (pattern-lambda
    (for (= var (: a b (-? c))) body)
    (begin
      (if (not (symbol? var))
	  (error "invalid for loop syntax: expected symbol"))
      (if c
	  (let ((cnt (gensym))
		(lim (gensym)))
	    `(scope-block
	     (block
	      (= ,cnt 0)
	      (= ,lim (call int32 (call floor (call / (call - ,c ,a) ,b))))
	      (break-block loop-exit
			   (_while (call <= ,cnt ,lim)
				   (block
				    (= ,var (call + ,a (call * ,cnt ,b)))
				    (break-block loop-cont
						 ,body)
				    (= ,cnt (call + 1 ,cnt))))))))
	  (let ((lim (gensym)))
	    `(scope-block
	     (block
	      (= ,var ,a)
	      (= ,lim ,b)
	      (break-block loop-exit
			   (_while (call <= ,var ,lim)
				   (block
				    (break-block loop-cont
						 ,body)
				    (= ,var (call + 1 ,var)))))))))))

   ; for loop over arbitrary vectors
   (pattern-lambda
    (for (= i X) body)
    (let ((coll  (gensym))
	  (state (gensym)))
      `(scope-block
	(block (= ,coll ,X)
	       (= ,state (call (top start) ,coll))
	       (while (call (top !) (call (top done) ,coll ,state))
		      (block
		       (= (tuple ,i ,state) (call (top next) ,coll ,state))
		       ,body))))))

   ; update operators
   (pattern-lambda (+= a b)     (expand-update-operator '+ a b))
   (pattern-lambda (-= a b)     (expand-update-operator '- a b))
   (pattern-lambda (*= a b)     (expand-update-operator '* a b))
   (pattern-lambda (.*= a b)    (expand-update-operator '.* a b))
   (pattern-lambda (/= a b)     (expand-update-operator '/ a b))
   (pattern-lambda (./= a b)    (expand-update-operator './ a b))
   (pattern-lambda (//= a b)    (expand-update-operator '// a b))
   (pattern-lambda (.//= a b)   (expand-update-operator '.// a b))
   (pattern-lambda (|\\=| a b)  (expand-update-operator '|\\| a b))
   (pattern-lambda (|.\\=| a b) (expand-update-operator '|.\\| a b))
   (pattern-lambda (^= a b)     (expand-update-operator '^ a b))
   (pattern-lambda (.^= a b)    (expand-update-operator '.^ a b))
   (pattern-lambda (%= a b)     (expand-update-operator '% a b))
   (pattern-lambda (|\|=| a b)  (expand-update-operator '|\|| a b))
   (pattern-lambda (&= a b)     (expand-update-operator '& a b))
   (pattern-lambda ($= a b)     (expand-update-operator '$ a b))
   (pattern-lambda (<<= a b)    (expand-update-operator '<< a b))
   (pattern-lambda (>>= a b)    (expand-update-operator '>> a b))
   (pattern-lambda (>>>= a b)   (expand-update-operator '>>> a b))

   ;; colon
   (pattern-lambda (: a (-/ :))
		   `(call (top RangeFrom) ,a 1))
   (pattern-lambda (: a b (-/ :))
		   `(call (top RangeFrom) ,a ,b))
   (pattern-lambda (: (: b (-/ :)))
		   `(call (top RangeBy) ,b))
   (pattern-lambda (: (: b c))
		   `(call (top RangeTo) ,b ,c))
   (pattern-lambda (: c)
		   `(call (top RangeTo) 1 ,c))

   (pattern-lambda
    (: a b (-? c))
    (if c
	`(call colon ,a ,c ,b)
	`(call colon ,a ,b 1)))

   ;; hcat, vcat
   (pattern-lambda (hcat . a)
		   `(call hcat ,@a))

   (pattern-lambda (vcat . a)
		   `(call vcat ,@a))

   )) ; patterns

; patterns that verify all syntactic sugar was well-formed
; if any sugary forms remain after the above patterns, it means the
; patterns didn't match, which implies a syntax error.
(define check-desugared
  (pattern-set
   (pattern-lambda (function . any)
		   (error "invalid function definition"))

   (pattern-lambda (for . any)
		   (error "invalid for loop syntax"))

   (pattern-lambda (type . any)
		   (error "invalid type definition"))

   (pattern-lambda (typealias . any)
		   (error "invalid typealias statement"))

   (pattern-lambda (macro . any)
		   (error "macros must be defined at the top level"))

   ))

;; Comprehensions

(define (lower-nd-comprehension expr ranges)
  (let ((result    (gensym))
	(ri        (gensym))
	(oneresult (gensym)))
    ;; evaluate one expression to figure out type and size
    ;; compute just one value by inserting a break inside loops
    (define (evaluate-one ranges)
      (if (null? ranges)
	  `(= ,oneresult ,expr)
	  (if (eq? (car ranges) `:)
	      (evaluate-one (cdr ranges))
	      `(for ,(car ranges)
		    (block ,(evaluate-one (cdr ranges))
			   (break)) ))))

    ;; compute the dimensions of the result
    (define (compute-dims ranges oneresult-dim)
      (if (null? ranges)
	  (list)
	  (if (eq? (car ranges) `:)
	      (cons `(call size ,oneresult ,oneresult-dim)
		    (compute-dims (cdr ranges) (+ oneresult-dim 1)))
	      (cons `(call length ,(caddr (car ranges)))
		    (compute-dims (cdr ranges) oneresult-dim)) )))

    ;; construct loops to cycle over all dimensions of an n-d comprehension
    (define (construct-loops ranges iters oneresult-dim)
      (if (null? ranges)
	  (if (null? iters)
	      `(block (call assign ,result ,expr ,ri)
		      (+= ,ri 1))
	      `(block (call assign ,result (ref ,expr ,@(reverse iters)) ,ri)
		      (+= ,ri 1)) )
	  (if (eq? (car ranges) `:)
	      (let ((i (gensym)))
		`(for (= ,i (: 1 (call size ,oneresult ,oneresult-dim)))
		      ,(construct-loops (cdr ranges) (cons i iters) (+ oneresult-dim 1)) ))
	      `(for ,(car ranges)
		    ,(construct-loops (cdr ranges) iters oneresult-dim) ))))

    ;; Evaluate the comprehension
    `(scope-block
      (block 
       (= ,oneresult (tuple))
       ,(evaluate-one ranges)
       (= ,result (call jl_comprehension_zeros ,oneresult ,@(compute-dims ranges 1) ))
       (= ,ri 1)
       ,(construct-loops (reverse ranges) (list) 1)
       ,result ))))

(define lower-comprehensions
  (pattern-set

   (pattern-lambda
    (comprehension expr . ranges)
    (if (any (lambda (x) (eq? x ':)) ranges)
	(lower-nd-comprehension expr ranges)
    (let ((result    (gensym))
	  (ri        (gensym))
	  (oneresult (gensym))
	  (rv        (map (lambda (x) (gensym)) ranges)))

      ;; get the first value in a range
      (define (first-val range)
	`(call (top ref)
	       (call (top next) ,range (call (top start) ,range)) 1))

      ;; evaluate one expression to figure out type and size
      ;; compute just one value by inserting a break inside loops
      (define (evaluate-one ranges)
	`(block
	  ,@(map (lambda (r)
		   ;; r is (= var range)
		   `(= ,(cadr r) ,(first-val (caddr r))))
		 ranges)
	  (= ,oneresult ,expr)
	  ,oneresult))

      ;; compute the dimensions of the result
      (define (compute-dims ranges)
	(map (lambda (r) `(call length ,(caddr r)))
	     ranges))

      ;; construct loops to cycle over all dimensions of an n-d comprehension
      (define (construct-loops ranges)
        (if (null? ranges)
	    `(block (call assign ,result ,expr ,ri)
		    (+= ,ri 1))
	    `(for ,(car ranges)
		  ,(construct-loops (cdr ranges)))))

      ;; Evaluate the comprehension
      (let ((ranges2
	     (map (lambda (r v) `(= ,(cadr r) ,v)) ranges rv)))
	`(scope-block
	  (block
	   (local ,oneresult)
	   ,@(map (lambda (v r) `(= ,v ,(caddr r))) rv ranges)
	   ;; the evaluate-one code is used by type inference but does not run
	   (if false ,(evaluate-one ranges2))
	   (= ,result (call (top Array)
			    (static_typeof ,oneresult)
			    ,@(compute-dims ranges2)))
	   (= ,ri 1)
	   ,(construct-loops (reverse ranges2))
	   ,result))))))

   ;; cell array comprehensions
   (pattern-lambda
    (cell-comprehension expr . ranges)
    (let ( (result (gensym)) (ri (gensym)) )

      ;; compute the dimensions of the result
      (define (compute-dims ranges)
	(if (null? ranges)
	    (list)
	    (cons `(call length ,(car ranges))
		  (compute-dims (cdr ranges)))))

      ;; construct loops to cycle over all dimensions of an n-d comprehension
      (define (construct-loops ranges)
        (if (null? ranges)
	    `(block (call assign ,result ,expr ,ri)
		    (+= ,ri 1))
	    `(for ,(car ranges)
		  ,(construct-loops (cdr ranges)))))

      ;; Evaluate the comprehension
      `(scope-block
	(block 
	 (= ,result (call (top Array) (top Any) ,@(compute-dims ranges)))
	 (= ,ri 1)
	 ,(construct-loops (reverse ranges))
	 ,result))))

)) ;; lower-comprehensions


; (op (op a b) c) => (a b c) etc.
(define (flatten-op op e)
  (if (not (pair? e)) e
      (apply append
	     (map (lambda (x)
		    (if (and (pair? x) (eq? (car x) op))
			(flatten-op op x)
			(list x)))
		  (cdr e)))))

(define (expand-and e)
  (let ((e (flatten-op '&& e)))
    (let loop ((tail e))
      (if (null? tail)
	  'true
	  (if (null? (cdr tail))
	      (car tail)
	      `(if ,(car tail)
		   ,(loop (cdr tail))
		   false))))))

(define (expand-or e)
  (let ((e (flatten-op '|\|\|| e)))
    (let loop ((tail e))
      (if (null? tail)
	  'false
	  (if (null? (cdr tail))
	      (car tail)
	      (if (symbol? (car tail))
		  `(if ,(car tail) ,(car tail)
		       ,(loop (cdr tail)))
		  (let ((g (gensym)))
		    `(block (= ,g ,(car tail))
			    (if ,g ,g
				,(loop (cdr tail)))))))))))

; conversion to "linear flow form"
;
; This pass removes control flow constructs from value position.
; A "control flow construct" is anything that would require a branch.
;  (block ... (value-expr ... control-expr ...) ...) =>
;  (block ... (= var control-expr) (value-expr ... var ...) ...)
; except the assignment is incorporated into control-expr, so that
; control exprs only occur in statement position.
;
; The conversion works by passing around the intended destination of
; the value being computed: #f for statement position, #t for value position,
; or a symbol if the value needs to be assigned to a particular variable.
; This is the "dest" argument to to-lff.
;
; This also keeps track of tail position, and converts the code so that
; everything in tail position is returned explicitly.
;
; The result is that every expression whose value is needed is either
; a function argument, an assignment RHS, or returned explicitly.
; In this form, expressions can be analyzed freely without fear of
; intervening branches. Similarly, control flow can be analyzed without
; worrying about implicit value locations (the "evaluation stack").
(define (to-LFF e)
  (define (to-blk r)
    (if (length= r 1)
	(car r)
	(cons 'block (reverse r))))
  (define (blk-tail r)
    (reverse r))
  ; to-lff returns (new-ex . stmts) where stmts is a list of statements that
  ; must run before new-ex is valid.
  ;
  ; If the input expression needed to be removed from its original context,
  ; like the 'if' in "1+if(a,b,c)", then new-ex is a symbol holding the
  ; result of the expression.
  ;
  ; If dest is a symbol or #f, new-ex can be a statement.
  ;
  ; We essentially maintain a stack of control-flow constructs that need to be
  ; run in statement position as we walk around an expression. If we hit
  ; statement context, we can dump the control-flow stuff there.
  ; This expression walk is entirely within the "else" clause of the giant
  ; case expression. Everything else deals with special forms.
  (define (to-lff e dest tail)
    (if (or (not (pair? e)) (quoted? e) (equal? e '(null)))
	(cond ((symbol? dest) (cons `(= ,dest ,e) '()))
	      (dest (cons (if tail `(return ,e) e)
			  '()))
	      (else (cons e '())))

	(case (car e)
	  ((=)
	   (if (not (symbol? (cadr e)))
	       (error "invalid assignment statement")
	       (let ((r (to-lff (caddr e) (cadr e) #f)))
		 (cond ((symbol? dest)
			(cons `(block ,(car r)
				      (= ,dest ,(cadr e)))
			      (cdr r)))
		       (dest
			(cons (if tail `(return ,(cadr e)) (cadr e)) r))
		       (else r)))))

	  ((if)
	   (cond ((or tail (eq? dest #f) (symbol? dest))
		  (let ((r (to-lff (cadr e) #t #f)))
		    (cons `(if
			    ,(car r)
			    ,(to-blk (to-lff (caddr e) dest tail))
			    ,(if (length= e 4)
				 (to-blk (to-lff (cadddr e) dest tail))
				 (to-blk (to-lff '(null)  dest tail))))
			  (cdr r))))
		 (else (let ((g (gensym)))
			 (cons g
			       (to-lff e g #f))))))

	  ((&&)
	   (to-lff (expand-and e) dest tail))
	  ((|\|\||)
	   (to-lff (expand-or e) dest tail))

	  ((block)
	   (let* ((g (gensym))
		  (stmts
		   (let loop ((tl (cdr e)))
		     (if (null? tl) '()
			 (if (null? (cdr tl))
			     (cond ((or tail (eq? dest #f) (symbol? dest))
				    (blk-tail (to-lff (car tl) dest tail)))
				   (else
				    (blk-tail (to-lff (car tl) g tail))))
			     (cons (to-blk (to-lff (car tl) #f #f))
				   (loop (cdr tl))))))))
	     (if (and (eq? dest #t) (not tail))
		 (cons g (reverse stmts))
		 (if (and tail (null? stmts))
		     (cons '(return (null))
			   '())
		     (cons (cons 'block stmts)
			   '())))))

	  ((return)
	   (if (and dest (not tail))
	       (error "misplaced return statement")
	       (to-lff (cadr e) #t #t)))

	  ((_while) (cond ((eq? dest #t)
			   (cons (if tail '(return (null)) '(null))
				 (to-lff e #f #f)))
			  (else
			   (let* ((r (to-lff (cadr e) #t #f))
				  (w (cons `(_while ,(car r)
					      ,(to-blk
						(append
						 (cdr r)
						 (to-lff (caddr e) #f #f))))
					   (cdr r))))
			     (if (symbol? dest)
				 (cons `(= ,dest (null)) w)
				 w)))))

	  ((break-block)
	   (let ((r (to-lff (caddr e) dest tail)))
	     (if dest
		 (cons (car r)
		       (list `(break-block ,(cadr e) ,(to-blk (cdr r)))))
		 (cons `(break-block ,(cadr e) ,(car r))
		       (cdr r)))))

	  ((scope-block)
	   (if (and dest (not tail))
	       (let* ((g (gensym))
		      (r (to-lff (cadr e) g tail)))
		 (cons (car (to-lff g dest tail))
		       ; tricky: need to introduce a new local outside the
		       ; scope-block so the scope-block's value can propagate
		       ; out. otherwise the value could be inaccessible due
		       ; to being wrapped inside a scope.
		       `((scope-block ,(to-blk r))
			 (local! ,g))))
	       (let ((r (to-lff (cadr e) dest tail)))
		 (cons `(scope-block ,(to-blk r))
		       '()))))

	  ((break) (if dest
		       (error "misplaced break or continue")
		       (cons e '())))

	  ((lambda)
	   (let ((l `(lambda ,(cadr e)
		       ,(to-blk (to-lff (caddr e) #t #t)))))
	     (if (symbol? dest)
		 (cons `(= ,dest ,l) '())
		 (cons (if tail `(return ,l) l) '()))))

	  ((local global)
	   (if (symbol? dest)
	       (error (string "misplaced " (car e) " declaration")))
	   (cons (to-blk (to-lff '(null) dest tail))
		 (list e)))

	  (else
	   (let ((r (map (lambda (arg) (to-lff arg #t #f))
			 e)))
	     (cond ((symbol? dest)
		    (cons `(= ,dest ,(map car r))
			  (apply append (map cdr r))))
		   (else
		    (let ((ex (map car r)))
		      (cons (if tail `(return ,ex) ex)
			    (apply append (map cdr r)))))))))))
  (to-blk (to-lff e #t #t)))
#|
future issue:
right now scope blocks need to be inside functions:

> (julia-expand '(block (call + 1 (scope-block (block (= a b) c)))))
(block (scope-block (local a) (local #:g13) (block (= a b) (= #:g13 c)))
       (return (call + 1 #:g13)))

> (julia-expand '(scope-block (call + 1 (scope-block (block (= a b) c)))))
(scope-block
 (local #:g15)
 (block (scope-block (local a) (block (= a b) (= #:g15 c)))
	(return (call + 1 #:g15))))

The first one gave something broken, but the second case works.
So far only the second case can actually occur.
|#

(define (declared-global-vars e)
  (if (or (not (pair? e)) (quoted? e))
      '()
      (case (car e)
	((lambda scope-block)  '())
	((global)  (cdr e))
	(else
	 (apply append (map declared-global-vars e))))))

; local variable identification
; convert (scope-block x) to `(scope-block ,@locals ,x)
; where locals is a list of (local x) expressions, derived from two sources:
; 1. (local x) expressions inside this scope-block and lambda
; 2. variables assigned inside this scope-block that don't exist in outer
;    scopes
(define (identify-locals e)
  (define (find-assigned-vars e env)
    (if (or (not (pair? e)) (quoted? e))
	'()
	(case (car e)
	  ((lambda scope-block)  '())
	  ((local local!)  (list (decl-var (cadr e))))
	  ((=)
	   (let ((others (find-assigned-vars (caddr e) env))
		 (v (decl-var (cadr e))))
	     (if (memq v env)
		 others
		 (cons v others))))
	  (else
	   (apply append (map (lambda (x) (find-assigned-vars x env))
			      e))))))
  (define (add-local-decls e env)
    (if (or (not (pair? e)) (quoted? e)) e
	(cond ((eq? (car e) 'lambda)
	       (let* ((env (append (lam:vars e) env))
		      (body (add-local-decls (caddr e) env)))
		 (list 'lambda (cadr e) body)))

	      ((eq? (car e) 'scope-block)
	       (let* ((glob (declared-global-vars (cadr e)))
		      (vars (delete-duplicates
			     (find-assigned-vars
			      ; being declared global prevents a variable
			      ; assignment from introducing a local
			      (cadr e) (append env glob))))
		      (body (add-local-decls (cadr e) (append vars env))))
		 `(scope-block ,@(map (lambda (v) `(local ,v))
				      vars)
			       ,body)))
	      (else
	       ; form (local! x) adds a local to a normal (non-scope) block
	       (let ((newenv (append (declared-local!-vars e) env)))
		 (map (lambda (x)
			(add-local-decls x newenv))
		      e))))))
  (add-local-decls e '()))

(define (declared-local-vars e)
  (map (lambda (x) (decl-var (cadr x)))
       (filter (lambda (x)
		 (and (pair? x)
		      (or (eq? (car x) 'local)
			  (eq? (car x) 'local!))))
	       (cdr e))))
(define (declared-local!-vars e)
  (map cadr
       (filter (lambda (x)
		 (and (pair? x)
		      (eq? (car x) 'local!)))
	       (cdr e))))

; e - expression
; renames - assoc list of (oldname . newname)
; this works on any tree format after identify-locals
(define (rename-vars e renames)
  (define (without alst remove)
    (cond ((null? alst)               '())
	  ((null? remove)             alst)
	  ((memq (caar alst) remove)  (without (cdr alst) remove))
	  (else                       (cons (car alst)
					    (without (cdr alst) remove)))))
  (cond ((null? renames)  e)
	((symbol? e)      (lookup e renames e))
	((not (pair? e))  e)
	((quoted? e)      e)
	(else
	 (let (; remove vars bound by current expr from rename list
	       (new-renames (without renames
				     (case (car e)
				       ((lambda)      (lam:vars e))
				       ((scope-block) (declared-local-vars e))
				       (else '())))))
	   (cons (car e)
		 (map (lambda (x)
			(rename-vars x new-renames))
		      (cdr e)))))))

; remove (scope-block) and (local), convert lambdas to the form
; (lambda (argname...) (locals var...) body)
(define (flatten-scopes e)
  (define scope-block-vars '())
  (define (remove-scope-blocks e)
    (cond ((or (atom? e) (quoted? e)) e)
	  ((eq? (car e) 'lambda) (flatten-scopes e))
	  ((eq? (car e) 'scope-block)
	   (let ((vars (declared-local-vars e))
		 (body (car (last-pair e))))
	     (let* ((newnames (map (lambda (x) (gensym)) vars))
		    (bod (rename-vars (remove-scope-blocks body)
				      (map cons vars newnames))))
	       (set! scope-block-vars (nconc newnames scope-block-vars))
	       bod)))
	  (else (map remove-scope-blocks e))))
  
  (cond ((not (pair? e))   e)
	((quoted? e)       e)
	((eq? (car e)      'lambda)
	 (let* ((argnames  (lam:vars e))
		(body0     (caddr e))
		(body      (if (eq? (car body0) 'scope-block)
			       (car (last-pair body0))
			       body0))
		(l0
		 (if (eq? (car body0) 'scope-block)
		     (filter   ; remove locals conflicting with arg names
		      (lambda (v) (not (memq v argnames)))
		      (declared-local-vars body0))
		     (declared-local!-vars body0)))
		(r-s-b (remove-scope-blocks body)))
	   `(lambda ,(cadr e)
	      (locals ,@l0 ,@scope-block-vars)
	      ,r-s-b)))
	(else (map (lambda (x) (if (not (pair? x)) x
				   (flatten-scopes x)))
		   e))))

(define (make-var-info name) (list name 'Any #f #f))
(define vinfo:name car)
(define vinfo:type cadr)
(define vinfo:capt caddr)
(define (vinfo:set-type! v t) (set-car! (cdr v) t))
(define (vinfo:set-capt! v c) (set-car! (cddr v) c))
(define (vinfo:set-asgn! v a) (set-car! (cdddr v) a))
(define var-info-for assq)

(define (lambda-all-vars e)
  (append (lam:vars e)
	  (cdr (caddr e))))

(define (free-vars e)
  (cond ((symbol? e) (list e))
	((or (atom? e) (quoted? e)) '())
	((eq? (car e) 'lambda)
	 (diff (free-vars (lam:body e))
	       (lambda-all-vars e)))
	(else (unique (apply nconc (map free-vars (cdr e)))))))

; convert each lambda's (locals ...) to
;   (var-info (locals ...) var-info-lst captured-var-infos)
; where var-info-lst is a list of var-info records
(define (analyze-vars e env)
  (cond ((or (atom? e) (quoted? e)) e)
	((and (eq? (car e) '=) (symbol? (cadr e)))
	 (let ((vi (var-info-for (cadr e) env)))
	   (if vi
	       (vinfo:set-asgn! vi #t)))
	 `(= ,(cadr e) ,(analyze-vars (caddr e) env)))
	((or (eq? (car e) 'local) (eq? (car e) 'local!))
	 (if (pair? (cadr e))
	     (analyze-vars (cadr e) env)
	     '(null)))
	((eq? (car e) '|::|)
	 ; handle var::T declaration by storing the type in the var-info
	 ; record. for non-symbols, emit a type assertion.
	 (if (symbol? (cadr e))
	     (let ((vi (var-info-for (cadr e) env)))
	       (if vi
		   (vinfo:set-type! vi (caddr e)))
	       '(null))
	     (let ((e2 (analyze-vars (cadr e) env)))
	       `(call (top typeassert) ,e2 ,(caddr e)))))
	((eq? (car e) 'lambda)
	 (letrec ((args (lam:args e))
		  (locl (cdr (caddr e)))
		  (allv (nconc (map arg-name args) locl))
		  (fv   (diff (free-vars (lam:body e)) allv))
		  (glo  (declared-global-vars (lam:body e)))
		  ; make var-info records for vars introduced by this lambda
		  (vi   (nconc
			 (map (lambda (decl) (make-var-info (decl-var decl)))
			      args)
			 (map make-var-info locl)))
		  ; captured vars: vars from the environment that occur
		  ; in our set of free variables (fv).
		  (cv    (filter (lambda (v) (and (memq (vinfo:name v) fv)
						  (not (memq
							(vinfo:name v) glo))))
				 env))
		  (bod   (analyze-vars
			  (lam:body e)
			  (append vi
				  ; new environment: add our vars
				  (filter (lambda (v)
					    (not (memq (vinfo:name v) allv)))
					  env)))))
	   ; mark all the vars we capture as captured
	   (for-each (lambda (v) (vinfo:set-capt! v #t))
		     cv)
	   `(lambda ,args
	      (var-info ,(caddr e) ,vi ,cv ())
	      ,bod)))
	(else (cons (car e)
		    (map (lambda (x) (analyze-vars x env))
			 (cdr e))))))

(define (analyze-variables e) (analyze-vars e '()))

; remove if, _while, block, break-block, and break
; replaced with goto and gotoifnot
; TODO: remove type-assignment-affecting expressions from conditional branch.
;       needed because there's no program location after the condition
;       is evaluated but before the branch's successors.
;       pulling a complex condition out to a temporary variable creates
;       such a location (the assignment to the variable).
(define (goto-form e)
  (let ((code '())
	(ip   0)
	(label-counter 0))
    (define (emit c)
      (set! code (cons c code))
      (set! ip (+ ip 1)))
    (define (make-label)
      (begin0 label-counter
	      (set! label-counter (+ 1 label-counter))))
    (define (mark-label l) (emit `(label ,l)))
    (define (compile e break-labels vi)
      (if (or (not (pair? e)) (equal? e '(null)))
	  ; atom has no effect, but keep symbols for undefined-var checking
	  (if (symbol? e) (emit e) #f)
	  (case (car e)
	    ((call)  (emit (goto-form e)))
	    ((=)     (let ((vt (vinfo:type
				(or (var-info-for (cadr e) vi) '(#f Any)))))
		       (if (not (eq? vt 'Any))
			   (emit `(= ,(cadr e) (call (top convert) ,vt
						     ,(goto-form (caddr e)))))
			   (emit `(= ,(cadr e) ,(goto-form (caddr e)))))))
	    ((if) (let ((elsel (make-label))
			(endl  (make-label))
			(tail  (and (pair? (caddr e))
				    (eq? (car (caddr e)) 'return))))
		    (emit `(gotoifnot ,(goto-form (cadr e)) ,elsel))
		    (compile (caddr e) break-labels vi)
		    (if (not tail) (emit `(goto ,endl)))
		    (mark-label elsel)
		    (compile (cadddr e) break-labels vi)
		    (if (not tail) (mark-label endl))))
	    ((block) (for-each (lambda (x) (compile x break-labels vi))
			       (cdr e)))
	    ((_while) (let ((topl (make-label))
			    (endl (make-label)))
			(mark-label topl)
			(emit `(gotoifnot ,(goto-form (cadr e)) ,endl))
			(compile (caddr e) break-labels vi)
			(emit `(goto ,topl))
			(mark-label endl)))
	    ((break-block) (let ((endl (make-label)))
			     (compile (caddr e)
				      (cons (cons (cadr e) endl)
					    break-labels)
				      vi)
			     (mark-label endl)))
	    ((break) (let ((labl (assq (cadr e) break-labels)))
		       (if (not labl)
			   (error "break or continue outside loop")
			   (emit `(goto ,(cdr labl))))))
	    ((global) #f)  ; remove global declarations
	    (else  (emit (goto-form e))))))
    (cond ((or (not (pair? e)) (quoted? e)) e)
	  ((eq? (car e) 'lambda)
	   (compile (cadddr e) '() (append (caddr (caddr e))
					   (cadddr (caddr e))))
	   `(lambda ,(cadr e) ,(caddr e)
		    ,(cons 'body (reverse code))))
	  (else (map goto-form e)))))

(define (to-goto-form e)
  (goto-form e))

(define (expand-backquote e)
  (cond ((symbol? e)          `(quote ,e))
	((not (pair? e))      e)
	((eq? (car e) '$)     (cadr e))
	((eq? (car e) 'quote)
	 (if (symbol? (cadr e))
	     e
	     (expand-backquote (expand-backquote (cadr e)))))
	((not (any (lambda (x)
		     (match '($ (tuple (... x))) x))
		   e))
	 `(call (top expr) ,@(map expand-backquote e)))
	(else
	 (let loop ((p (cdr e)) (q '()))
	   (if (null? p)
	       (let ((forms (reverse q)))
		 `(call (top Expr) ,(expand-backquote (car e))
			(call (top append) ,@forms) (top Any)))
	       ; look for splice inside backquote, e.g. (a,$(x...),b)
	       (if (match '($ (tuple (... x))) (car p))
		   (loop (cdr p)
			 (cons (cadr (cadr (cadr (car p)))) q))
		   (loop (cdr p)
			 (cons `(call (top cell_1d)
				      ,(expand-backquote (car p)))
			       q))))))))

(define (julia-expand-backquote e)
  (cond ((not (pair? e)) e)
	((eq? (car e) 'quote)
	 (if (symbol? (cadr e))
	     e
	     (julia-expand-backquote (expand-backquote (cadr e)))))
	(else
	 (map julia-expand-backquote e))))

(define (julia-expand ex)
  (to-goto-form
   (analyze-variables
    (flatten-scopes
     (identify-locals
      (to-LFF
       (pattern-expand patterns
	(pattern-expand lower-comprehensions
	 (pattern-expand binding-form-patterns
	  (julia-expand-backquote ex))))))))))
