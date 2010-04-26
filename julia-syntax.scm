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

(define *julia-interpreter* #f)

(define (quoted? e)
  (and (pair? e)
       (or (eq? (car e) 'quote)
	   (eq? (car e) 'top)
	   (eq? (car e) 'unbound)
	   (eq? (car e) 'line))))

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
; for example a[f(x)] => do(temp=f(x), a[temp])
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
		`(call (top Range) ,(cadr x) 1 ,(caddr x)))
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

(define (symbols->typevars sl)
  (map (lambda (x) `(call (top typevar) ',x)) sl))

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

(define (generic-function-def-expr name sparams argl body)
  (let ((argl (fsig-to-lambda-list argl)))
    (gf-def-expr-
     name argl
     (if (null? sparams)
	 `(tuple ,@(llist-types argl))
	 `(call (lambda ,sparams
		  (tuple ,@(llist-types argl)))
		,@(symbols->typevars sparams)))
     body)))

(define (struct-def-expr name params super fields)
  ; extract the name from a function def expr
  (define (f-exp-name e)
    (let ((head (cadadr e)))
      (if (symbol? head) head
	  (cadr head))))

  (receive
   (funcs fields) (separate
		   (lambda (x)
		     (and (pair? x)
			  (or (eq? (car x) 'function)
			      (and (length= x 3)
				   (eq? (car x) '=)
				   (pair? (cadr x))
				   (eq? (caadr x) 'call))
			      (not (or (eq? (car x) '|::|)
				       (error "invalid struct syntax:" x))))))
		   fields)
   (let ((field-names (map decl-var fields))
	 (field-types (map decl-type fields))
	 (func-names  (delete-duplicates (map f-exp-name funcs)))
	 (func-args   (map (lambda (fexp)
			     (fsig-to-lambda-list (cddadr fexp))) funcs))
	 (func-types  (map (lambda (x) (gensym)) funcs))
	 (func-sparms (map (lambda (fexp)
			     (let ((head (cadadr fexp)))
			       (if (symbol? head) '()
				   (cddr head))))
			   funcs))
	 (tvars       (gensym))
	 (proto       (gensym)))
     `(call
       (lambda ()
	 (scope-block
	  (block
	   (local (tuple ,@func-names ,@func-types ,tvars ,proto))
	   (call
	    (lambda (,@params)
	      ; the static parameters are bound to new TypeVars in here,
	      ; so everything that sees the TypeVars is evaluated here.
	      (block
	       (= ,tvars (tuple ,@params))
	       (= ,proto
		  (call (top new_struct_type)
			(quote ,name)
			,super
			,tvars
			(tuple ,@(map (lambda (x) `',x) field-names))))
	       ; wrap type prototype in a type constructor
	       (= ,name (call (top new_type_constructor) ,tvars ,proto))
	       ; now add the type fields, which might reference the type
	       ; itself. tie the recursive knot.
	       (call (top new_struct_fields)
		     ,name (tuple ,@field-types))
	       ,@(map (lambda (type argl sp)
			`(= ,type
			    ,(if (null? sp)
				 `(tuple ,@(llist-types argl))
				 `(call (lambda ,sp
					  (tuple ,@(llist-types argl)))
					,@(symbols->typevars sp)))))
		      func-types func-args func-sparms)))
	    ,@(symbols->typevars params))
	   ; build method definitions
	   ,@(map (lambda (fdef fargs ftype)
		    (gf-def-expr- (f-exp-name fdef)
				  fargs
				  ftype
				  (caddr fdef)))
		  funcs func-args func-types)
	   ; assign methods to type fields
	   ,@(map (lambda (fname)
		    `(= (|.| ,proto ,fname) ,fname))
		  func-names))))))))

(define (type-def-expr name params super)
  `(block
    (call
     (lambda ,params
       (block
	(= ,name
	   (call (top new_tag_type)
		 (quote ,name)
		 ,super
		 (tuple ,@params)))
	,(if (null? params)
	     `(null)
	     `(= ,name (call (top new_type_constructor)
			     (tuple ,@params) ,name)))))
     ,@(symbols->typevars params))))

(define *anonymous-generic-function-name* (gensym))

(define dotdotdotpattern (pattern-lambda (... a) `(curly ... ,a)))

; patterns that introduce lambdas
(define binding-form-patterns
  (pattern-set
   ; function with static parameters
   (pattern-lambda (function (call (curly name . sparams) . argl) body)
		   (generic-function-def-expr name sparams argl body))

   ; function definition
   (pattern-lambda (function (call name . argl) body)
		   (generic-function-def-expr name '() argl body))

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

   ; macro for timing evaluation
   (pattern-lambda (call (-/ Time) expr)
		   `(call time_thunk (-> (tuple) ,expr)))

   )) ; binding-form-patterns

(define patterns
  (pattern-set
   #;(pattern-lambda (--> a b)
		   `(call curly Function ,a ,b))

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
			  ,@(symbols->typevars params)))

   (pattern-lambda (comparison . chain) (expand-compare-chain chain))

   ; multiple value assignment
   (pattern-lambda (= (tuple . lhss) x)
		   (let ((t (gensym)))
		     `(block (= ,t ,x)
			     ,@(let loop ((lhs lhss)
					  (i   1))
				 (if (null? lhs) '((null))
				     (cons `(= ,(car lhs)
					       (call tupleref ,t ,i))
					   (loop (cdr lhs)
						 (+ i 1))))))))

   (pattern-lambda (= (ref a . idxs) rhs)
		   `(call set ,a ,rhs ,@(process-indexes idxs)))

   (pattern-lambda (ref a . idxs)
		   `(call ref ,a ,@(process-indexes idxs)))

   (pattern-lambda (cell . elts)
		   `(call (top cell_literal) ,@elts))
   (pattern-lambda (curly type . elts)
		   `(call (top instantiate_type) ,type ,@elts))

   ; call with splat
   (pattern-lambda (call f ... (... _) ...)
		   (let ((argl (cddr __)))
		     ; wrap sequences of non-... arguments in tuple()
		     (define (tuple-wrap a run)
		       (if (null? a)
			   (if (null? run) '()
			       (list `(tuple ,@(reverse run))))
			   (let ((x (car a)))
			     (if (and (length= x 2)
				      (eq? (car x) '...))
				 (if (null? run)
				     (list* (cadr x)
					    (tuple-wrap (cdr a) '()))
				     (list* `(tuple ,@(reverse run))
					    (cadr x)
					    (tuple-wrap (cdr a) '())))
				 (tuple-wrap (cdr a) (cons x run))))))
		     `(call apply ,f ,@(tuple-wrap argl '()))))

   ; tuple syntax (a, b...)
   ; note, inside tuple ... means sequence type
   (pattern-lambda (tuple . args)
		   (pattern-expand (list dotdotdotpattern)
				   `(call tuple ,@args)))

   dotdotdotpattern

   ; local x,y,z => local x;local y;local z
   (pattern-lambda (local (tuple . vars))
		   `(block
		     ,@(map (lambda (x) `(local ,x)) vars)))

   ; local x::int=2 => local x::int; x=2
   (pattern-lambda (local (= var rhs))
		   `(block (local ,var)
			   (= ,(decl-var var) ,rhs)))

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
	      (= ,lim (call div (call - ,c ,a) ,b))
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

;; TODO: Special cases of comprehensions must be detected to simplify
;; array indexing inside loop expressions.

(define identify-comprehensions
  (pattern-set

   (pattern-lambda
    (hcat (call (-/ |\||) expr (= i range)) . rest)
    `(comprehension ,expr (= ,i ,range) ,@rest))

   (pattern-lambda
    (hcat (= (call (-/ |\||) expr i) range) . rest)
    `(comprehension ,expr (= ,i ,range) ,@rest))

)) ;; identify-comprehensions

(define lower-comprehensions
  (pattern-set

   ; nd comprehensions
   (pattern-lambda
    (comprehension expr . ranges)
    (let ( (result (gensym)) (ri (gensym)) (oneresult (gensym)) )

      ;; evaluate one expression to figure out type and size
      ;; compute just one value by inserting a break inside loops
      ;; TODO: This should be cleaned up by handling all the 
      ;; appropriate cases of vectors, ranges, and for loop ranges.
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
		(cons `(call size ,oneresult ,oneresult-dim) (compute-dims (cdr ranges) (+ oneresult-dim 1)))
		(cons `(call length ,(car ranges)) (compute-dims (cdr ranges) oneresult-dim)) )))

      ;; construct loops to cycle over all dimensions of an n-d comprehension
      (define (construct-loops ranges iters oneresult-dim)
        (if (null? ranges)
	    (if (null? iters)
		`(block (call set ,result ,expr ,ri)
			(+= ,ri 1))
		`(block (call set ,result (ref ,expr ,@(reverse iters)) ,ri)
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

)) ;; lower-comprehensions


; (op (op a b) c) => (op a b c) etc.
(define (flatten-op op e)
  (if (not (pair? e)) e
      (if (eq? (car e) op)
	  (apply append (map (lambda (x)
			       (let ((x (flatten-op op x)))
				 (if (and (pair? x)
					  (eq? (car x) op))
				     (cdr x)
				     (list x))))
			     e))
	  (map (lambda (x) (flatten-op op x)) e))))

(define (expand-and-or e)
  (if (or (not (pair? e)) (quoted? e)) e
      (case (car e)
	((&&) (let ((e (flatten-op '&& e)))
		(let loop ((tail (cdr e)))
		  (if (null? tail)
		      true
		      (if (null? (cdr tail))
			  (expand-and-or (car tail))
			  `(if ,(expand-and-or (car tail))
			       ,(loop (cdr tail))
			       false))))))
	((|\|\||) (let ((e (flatten-op '|\|\|| e)))
		    (let loop ((tail (cdr e)))
		      (if (null? tail)
			  false
			  (if (null? (cdr tail))
			      (expand-and-or (car tail))
			      (if (symbol? (car tail))
				  `(if ,(car tail) ,(car tail)
				       ,(loop (cdr tail)))
				  (let ((g (gensym)))
				    `(block (= ,g ,(expand-and-or (car tail)))
					    (if ,g ,g
						,(loop (cdr tail)))))))))))
	(else (map expand-and-or e)))))

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
	  ((=)  (let ((r (to-lff (caddr e) (cadr e) #f)))
		  (cond ((symbol? dest)
			 (cons `(block ,(car r)
				       (= ,dest ,(cadr e)))
			       (cdr r)))
			(dest
			 (cons (if tail `(return ,(cadr e)) (cadr e)) r))
			(else r))))

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
						      (to-lff (caddr e) #f #f)))
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

	  ((local)
	   (if (symbol? dest)
	       (error "misplaced local declaration"))
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
	       (let* ((vars (delete-duplicates
			     (find-assigned-vars (cadr e) env)))
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
    (if (null? remove)
	alst
	(filter (lambda (ren)
		  (not (memq (car ren) remove)))
		alst)))
  (cond ((null? renames)  e)
	((symbol? e)      (lookup e renames e))
	((not (pair? e))  e)
	((quoted? e)      e)
	(else
	 (let* ((bound-vars  ; compute vars bound by current expr
		 (case (car e)
		   ((lambda)      (lam:vars e))
		   ((scope-block) (declared-local-vars e))
		   (else '())))
		(new-renames (without renames bound-vars)))
	   (cons (car e)
		 (map (lambda (x)
			(rename-vars x new-renames))
		      (cdr e)))))))

; remove (scope-block) and (local), convert lambdas to the form
; (lambda (argname...) (locals var...) body)
(define (flatten-scopes e)
  ; returns (expr . all-locals)
  (define (remove-scope-blocks e)
    (cond ((or (not (pair? e)) (quoted? e)) (cons e '()))
	  ((eq? (car e) 'lambda) (cons (flatten-scopes e) '()))
	  ((eq? (car e) 'local)  (if (pair? (cadr e))
				     (cons (cadr e) '())
				     (cons '(null) '())))
	  ((eq? (car e) 'local!) (cons '(null) '()))
	  ((eq? (car e) 'scope-block)
	   (let ((vars (declared-local-vars e))
		 (body (car (last-pair e))))
	     (let ((newnames (map (lambda (x) (gensym)) vars))
		   (rec (remove-scope-blocks body)))
	       (cons
		(rename-vars (car rec) (map cons vars newnames))
		(append newnames (cdr rec))))))
	  (else
	   (let ((rec (map remove-scope-blocks e)))
	     (cons (map car rec)
		   (apply append (map cdr rec)))))))

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
	      (locals ,@l0 ,@(cdr r-s-b))
	      ,(car r-s-b))))
	(else (map (lambda (x) (if (not (pair? x)) x (flatten-scopes x))) e))))

(define (make-var-info name) (list 'vinfo name 'Any #f))
(define (vinfo:name v) (list-ref v 1))
(define (vinfo:type v) (list-ref v 2))
(define (vinfo:capt v) (list-ref v 3))
(define (vinfo:set-type! v t) (set-car! (list-tail v 2) t))
(define (vinfo:set-capt! v c) (set-car! (list-tail v 3) c))
(define (var-info-for name vinfo)
  (and (pair? vinfo)
       (or (and (eq? (vinfo:name (car vinfo)) name)
		(car vinfo))
	   (var-info-for name (cdr vinfo)))))

(define (lambda-all-vars e)
  (append (lam:vars e)
	  (cdr (caddr e))))

(define (fix-seq-type t)
  ; wrap (call (top instantiate_type) ... . args) in (tuple ...)
  (if (and (length> t 2)
	   (eq? (car t) 'call)
	   (equal? (cadr t) '(top instantiate_type))
	   (eq? (caddr t) '...))
      `(call (top tuple) ,t)
      t))

; convert each lambda's (locals ...) to
;   (var-info (locals ...) var-info-lst captured-var-names)
; where var-info-lst is a list of var-info records
; returns (expr . captured-vars), where captured-vars is a list of
; (var-info . frame) records.
(define (analyze-vars e env)
  ; returns #f or (var-info . frame)
  (define (var-lookup v env)
    (if (not (pair? env)) #f
	(let ((vi (var-info-for v (car env))))
	  (if vi
	      (cons vi (car env))
	      (var-lookup v (cdr env))))))

  (cond ((symbol? e)
	 (let ((v (var-lookup e env)))
	   (if (and v (not (eq? (cdr v) (car env))))
	       (begin (vinfo:set-capt! (car v) #t)
		      (cons e (list v)))
	       (cons e '()))))
	((or (not (pair? e)) (quoted? e)) (cons e '()))
	((eq? (car e) '|::|)
	 (let ((inner (analyze-vars (cadr e) env)))
	   (if (symbol? (cadr e))
	       (let ((v (var-lookup (cadr e) env)))
		 (if v
		     (vinfo:set-type! (car v) (caddr e)))
		 (cons '(null) (cdr inner)))
	       (cons `(call (top typeassert)
			    ,(car inner) ,(caddr e))
		     (cdr inner)))))
	((eq? (car e) 'lambda)
	 (let* ((lvars (lambda-all-vars e))
		(vi    (map make-var-info lvars))
		(rec   (analyze-vars (lam:body e) (cons vi env)))
		; from this inner function's captured variables, select
		; those that come from frames above ours. we need to
		; capture those variables too, in order to pass them on
		; to this inner function.
		(cv    (filter (lambda (vi)
				 (member-p vi (cdr env)
					   (lambda (v e) (eq? (cdr v) e))))
			       (cdr rec))))
	   (for-each (lambda (decl)
		       (vinfo:set-type! (var-info-for (decl-var decl) vi)
					(fix-seq-type (decl-type decl))))
		     (lam:args e))
	   (cons `(lambda ,(lam:args e)
		   ;(var-info  locals  vinfos  captured         staticparams)
		    (var-info ,(caddr e) ,vi ,(map car (cdr rec)) ())
		    ,(car rec))
		 cv)))
	(else (let ((rec (map (lambda (x) (analyze-vars x env))
			      (cdr e))))
		(cons (cons (car e) (map car rec))
		      (delete-duplicates-p
		       (apply append (map cdr rec))
		       (lambda (a b) (eq? (car a) (car b)))))))))

(define (analyze-variables e) (car (analyze-vars e '())))

; lower closures, using the results of analyze-vars
; . for each captured var v, initialize with v = box(Any,())
; . uses of v change to unbox(v)
; . assignments to v change to boxset(v, value)
; . lambda expressions change to
;   new_closure(lambda-expr, (capt-var1, capt-var2, ...))
; . for each closed var v, uses change to (call unbox (closure-ref idx))
; . assignments to closed var v change to (call boxset (closure-ref idx) rhs)
(define (closure-convert- e vinfo)
  (define (lookup v vinfo)
    (let ((vi (var-info-for v (caddr vinfo))))
	   (if vi
	       (if (vinfo:capt vi) 'boxed 'local)
	       (let ((i (index-p (lambda (xx)
				   (eq? v (vinfo:name xx)))
				 (cadddr vinfo) 0)))
		 (or i 'global)))))
  (define (lookup-var-type v vinfo)
    (let ((vi (var-info-for v (caddr vinfo))))
	   (if vi
	       (vinfo:type vi)
	       (let ((vl (member-p v (cadddr vinfo)
				   (lambda (x y) (eq? x (vinfo:name y))))))
		 (or (and vl (vinfo:type (car vl)))
		     'Any)))))  ; TODO: types of globals?

  (cond ((symbol? e)
	 (let ((l (lookup e vinfo)))
	   (cond ((eq? l 'boxed) `(call (top unbox) ,e))
		 ((number? l)    `(call (top unbox) (closure-ref ,l)))
		 (else e))))
	((not (pair? e)) e)
	((eq? (car e) 'unbound)
	 (let ((v (closure-convert- (cadr e) vinfo)))
	   (if (pair? v)
	       `(box-unbound ,(caddr v))
	       e)))
	((quoted? e) e)
	(else
	 (case (car e)
	   ((=)
	    (let ((l (lookup (cadr e) vinfo))
		  (t (lookup-var-type (cadr e) vinfo))
		  (rhs (closure-convert- (caddr e) vinfo)))
	      (let ((rhs (if (eq? t 'Any)
			     rhs
			     `(call (top convert) ,rhs ,t))))
		(cond ((eq? l 'boxed)
		       `(call (top boxset) ,(cadr e) ,rhs))
		      ((number? l)
		       `(call (top boxset) (closure-ref ,l) ,rhs))
		      (else
		       `(= ,(cadr e) ,rhs))))))
	   ((lambda)
	    (let ((vinf  (lam:vinfo e))
		  (args  (llist-vars (cadr e)))
		  (body0 (lam:body e))
		  (capt  (map vinfo:name
			      (filter vinfo:capt (caddr (lam:vinfo e))))))
	      (let ((body
		     `(block ,@(map (lambda (v)
				      `(= ,v
					  (call (top box) (top Any)
						,@(if (memq v args)
						      (list v)
						      '()))))
				    capt)
			     ,(closure-convert- body0 vinf))))
		`(call new_closure
		       (lambda ,(lam:args e) ,(lam:vinfo e) ,body)
		       (call tuple
			     ; NOTE: to pass captured variables on to other
			     ; closures we must pass the box, not the value
			     ,@(map (lambda (x)
				      (let ((i (index-of x (cadddr vinfo) 0)))
					(if i
					    `(closure-ref ,i)
					    (vinfo:name x))))
				    (cadddr vinf)))))))
	   (else
	    (cons (car e)
		  (map (lambda (x) (closure-convert- x vinfo))
		       (cdr e))))))))

(define (closure-convert e)
  (closure-convert- (analyze-variables e) '(var-info (locals) () () ())))

; remove if, _while, block, break-block, and break
; replaced with goto and goto-ifnot
; TODO: remove type-assignment-affecting expressions from conditional branch.
;       needed because there's no program location after the condition
;       is evaluated but before the branch's successors.
;       pulling a complex condition out to a temporary variable creates
;       such a location (the assignment to the variable).
(define (goto-form e)
  (let ((code '())
	(ip   0))
    (define (emit c)
      (set! code (cons c code))
      (set! ip (+ ip 1)))
    (define (make-label)   (gensym))
    (define (mark-label l) (emit `(label ,l)))
    (define (compile e break-labels)
      (if (or (not (pair? e)) (equal? e '(null)))
	  ; atom has no effect, but keep symbols for undefined-var checking
	  (if (symbol? e) (emit e) #f)
	  (case (car e)
	    ((if) (let ((elsel (make-label))
			(endl  (make-label))
			(tail  (and (pair? (caddr e))
				    (eq? (car (caddr e)) 'return))))
		    (emit `(goto-ifnot ,(goto-form (cadr e)) ,elsel))
		    (compile (caddr e) break-labels)
		    (if (not tail) (emit `(goto ,endl)))
		    (mark-label elsel)
		    (compile (cadddr e) break-labels)
		    (if (not tail) (mark-label endl))))
	    ((_while) (let ((topl (make-label))
			    (endl (make-label)))
			(mark-label topl)
			(emit `(goto-ifnot ,(goto-form (cadr e)) ,endl))
			(compile (caddr e) break-labels)
			(emit `(goto ,topl))
			(mark-label endl)))
	    ((block) (for-each (lambda (x) (compile x break-labels))
			       (cdr e)))
	    ((break-block) (let ((endl (make-label)))
			     (compile (caddr e)
				      (cons (cons (cadr e) endl)
					    break-labels))
			     (mark-label endl)))
	    ((break) (let ((labl (assq (cadr e) break-labels)))
		       (if (not labl)
			   (error "break or continue outside loop")
			   (emit `(goto ,(cdr labl))))))
	    ((call)  (if (not (equal? (cadr e) '(top unbox)))
			 (emit (goto-form e))))
	    (else  (emit (goto-form e))))))
    (cond ((or (not (pair? e)) (quoted? e)) e)
	  ((eq? (car e) 'lambda)
	   (compile (cadddr e) '())
	   `(lambda ,(cadr e) ,(caddr e)
		    ,(if *julia-interpreter*
			 (list->vector (reverse code))
			 (cons 'body (reverse code)))))
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
		       (call (top append) ,@forms)))
	       ; look for splice inside backquote, e.g. (a,$(x...),b)
	       (if (match '($ (tuple (... x))) (car p))
		   (loop (cdr p)
			 (cons (cadr (cadr (cadr (car p)))) q))
		   (loop (cdr p)
			 (cons `(call (top tuple)
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
   (closure-convert
    (flatten-scopes
     (identify-locals
      (to-LFF
       (expand-and-or
	(pattern-expand patterns
	 (pattern-expand lower-comprehensions
	  (pattern-expand binding-form-patterns
	   (pattern-expand identify-comprehensions
	    (julia-expand-backquote ex))))))))))))
