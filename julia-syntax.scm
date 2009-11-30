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
; - make goto-form safe for inlining (delay label to index mapping)

(define (quoted? e)
  (and (pair? e)
       (or (eq? (car e) 'quote)
	   (eq? (car e) 'top)
	   (eq? (car e) 'value-or-null))))

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
	(else (error "Malformed function argument" v)))))

; convert a lambda list into a list of just symbols
(define (llist-vars lst)
  (map arg-name lst))

; get just argument types
(define (llist-types lst)
  (map (lambda (v)
	 (if (symbol? v)
	     'Any
	     (case (car v)
	       ((...)         `(... ,(decl-type (cadr v))))
	       ((= keyword)   (decl-type (caddr v)))
	       ((|::|)        (decl-type v))
	       (else (error "Malformed function arguments" lst)))))
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
; for example a[f(x)] => block(temp=f(x), a[temp])
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

(define (process-indexes i)
  (map (lambda (x) (if (eq? x ':) '(quote :) x)) i))

(define (find-quoted-syms e)
  (if (atom? e)
      '()
      (if (and (eq? (car e) 'quote)
	       (symbol? (cadr e)))
	  (cdr e)
	  (delete-duplicates (apply append (map find-quoted-syms e))))))

; build an expression that evaluates to the requested static parameter,
; given a list of types and argument names
(define (find-static-param p t names)
  (define (expr-search p ex where)
    (cond ((atom? ex) #f)
	  ((and (eq? (car ex) 'quote)
		(eq? (cadr ex) p))
	   where)
	  ((eq? (car ex) 'tuple)
	   (let loop ((elts (cdr ex))
		      (i    1))
	     (and (pair? elts)
		  (or (expr-search p (car elts) `(ref ,where ,i)))
		  (loop (cdr elts)
			(+ i 1)))))
	  ((eq? (car ex) 'ref)
	   (let loop ((elts (cddr ex))
		      (i    1))
	     (and (pair? elts)
		  (or (expr-search p (car elts) `(ref (|.| ,where parameters)
						      ,i))
		      (loop (cdr elts)
			    (+ i 1))))))
	  (else #f)))
  (let ((a (map cons t names)))
    (any (lambda (a)
	   (if (and (pair? (car a))
		    (eq? (caar a) '...))
	       (let ((loc
		      (expr-search p (cadr (car a))
				   `(call typeof (ref ,(cdr a) 1)))))
		 (and loc
		      `(if (call (top isnull) ,(cdr a))
			   (ref Union)
			   ,loc)))
	       (expr-search p (car a) `(call typeof ,(cdr a)))))
	 a)))

(define (function-expr argl body)
  (let ((static-params (find-quoted-syms argl))
	(t (llist-types argl))
	(n (llist-vars argl)))
    (let ((argl (map (lambda (n t) `(|::| ,n ,t))
		     n t))
	  (static-param-inits
	   (map (lambda (p)
		  `(= ,p ,(or (find-static-param p t n)
			      `(ref Union))))
		static-params)))
      `(lambda ,argl
	 (scope-block ,(if (null? static-params)
			   body
			   `(block ,@static-param-inits ,body)))))))

(define (unquote-type-params params)
  (map (lambda (p)
	 (if (and (pair? p)
		  (eq? (car p) 'quote))
	     (cadr p)
	     p))
       params))

; quote symbols in e that appear in list params
(define (quote-type-params e params)
  (cond ((and (symbol? e) (memq e params)) `(quote ,e))
	((atom? e) e)
	((eq? (car e) 'quote) e)
	(else (cons (car e)
		    (map (lambda (x) (quote-type-params x params))
			 (cdr e))))))

(define (typedef-expr name params super fields)
  (let ((field-names (map decl-var fields))
	(field-types (map decl-type fields)))
    `(block
      (= ,name
	 (call (top new_type)
	       (quote ,name)
	       (tuple ,@(map (lambda (x) `',x) params))
	       ,super))
      (call (top new_type_fields)
	    ,name ,super
	    (tuple ,@(map (lambda (n t)
			    `(tuple ',n ,(quote-type-params t params)))
			  field-names field-types))))))

(define *anonymous-generic-function-name* (gensym))

(define dotdotdotpattern (pattern-lambda (... a)
					 `(call (top ref) ... ,a)))

(define patterns
  (list
   (pattern-lambda (--> a b)
		   `(call ref Function ,a ,b))

   (pattern-lambda (|.| a b)
		   `(call getfield ,a (quote ,b)))

   (pattern-lambda (= (|.| a b) rhs)
		   `(call setfield ,a (quote ,b) ,rhs))

   ; type definition
   (pattern-lambda (type (-- name (-s)) (block . fields))
		   (typedef-expr name '() 'Any fields))

   (pattern-lambda (type (ref (-- name (-s)) . params) (block . fields))
		   (typedef-expr name (unquote-type-params params)
				 'Any fields))

   (pattern-lambda (type (comparison (-- name (-s)) (-/ <) super)
			 (block . fields))
		   (typedef-expr name '() super fields))

   (pattern-lambda (type (comparison (ref (-- name (-s)) . params) (-/ <) super)
			 (block . fields))
		   (typedef-expr name (unquote-type-params params)
				 super fields))

   ; typealias is an assignment; should be const when that exists
   (pattern-lambda (typealias (-- name (-s)) type-ex)
		   `(= ,name ,type-ex))

   (pattern-lambda (comparison . chain) (expand-compare-chain chain))

   ; multiple value assignment
   (pattern-lambda (= (tuple . lhss) x)
		   (let ((t (gensym)))
		     `(block (= ,t ,x)
			     ,@(let loop ((lhs lhss)
					  (i   0))
				 (if (null? lhs) '((null))
				     (cons `(= ,(car lhs)
					       (call tupleref
						     ,t (call unbox ,i)))
					   (loop (cdr lhs)
						 (+ i 1))))))))

   (pattern-lambda (= (ref a . idxs) rhs)
		   `(call set ,a ,@(process-indexes idxs) ,rhs))

   (pattern-lambda (ref a . idxs)
		   `(call ref ,a ,@(process-indexes idxs)))

   (pattern-lambda (list . elts)
		   `(call list ,@elts))

   ; function definition
   (pattern-lambda (function (call name . argl) body)
		   (let ((argl (fsig-to-lambda-list argl)))
		     `(= ,name
			 (call (top add_method)
			       (quote ,name)
			       (value-or-null ,name)
			       (tuple ,@(llist-types argl))
			       ,(function-expr argl body)))))

   ; expression form function definition
   (pattern-lambda (= (call name . argl) body)
		   `(function (call ,name ,@argl) ,body))

   (pattern-lambda (-> a b)
		   (let ((a (if (and (pair? a)
				     (eq? (car a) 'tuple))
				(cdr a)
				(list a))))
					; TODO: anonymous generic function
		     (function-expr a b)))

   (pattern-lambda (conversion (--> (|::| var from) to) body)
		   `(call add_conversion ,from ,to
			  (-> (|::| ,var ,from) ,body)))
   
   ; call with splat
   (pattern-lambda (call f ... (... _) ...)
		   (let ((argl (cddr __)))
		     `(call apply ,f
			    ,@(map (lambda (x)
				     (if (and (length= x 2)
					      (eq? (car x) '...))
					 (cadr x)
					 `(tuple ,x)))
				   argl))))

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
	  (error "Invalid for loop syntax: expected symbol"))
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
    (for (= x vec) body)
    (let ((i (gensym)))
      `(for (= ,i (: 1 (call numel ,vec)))
	    (block (= ,x (ref ,vec ,i)) ,body)) ))

   ; macro for timing evaluation
   (pattern-lambda (call (-/ Time) expr)
		   `(call time_thunk (-> (tuple) ,expr)))

   ; update operators
   (pattern-lambda (+= a b) (expand-update-operator '+ a b))
   (pattern-lambda (-= a b) (expand-update-operator '- a b))
   (pattern-lambda (*= a b) (expand-update-operator '* a b))
   (pattern-lambda (/= a b) (expand-update-operator '/ a b))
   (pattern-lambda (^= a b) (expand-update-operator '^ a b))
   (pattern-lambda (%= a b) (expand-update-operator '% a b))
   (pattern-lambda (|\|=| a b) (expand-update-operator '|\|| a b))
   (pattern-lambda (&= a b) (expand-update-operator '& a b))
   (pattern-lambda ($= a b) (expand-update-operator '$ a b))
   (pattern-lambda (<<= a b) (expand-update-operator '<< a b))
   (pattern-lambda (>>= a b) (expand-update-operator '>> a b))

   ;; colon
   (pattern-lambda
    (: a b (-? c))
    (if c
        `(call colon ,a ,c ,b)
        `(call colon ,a ,b 1)))

   )) ; patterns

; patterns that verify all syntactic sugar was well-formed
; if any sugary forms remain after the above patterns, it means the
; patterns didn't match, which implies a syntax error.
(define check-desugared
  (list
   (pattern-lambda (function . any)
		   (error "Invalid function definition"))

   (pattern-lambda (for . any)
		   (error "Invalid for loop syntax"))

   (pattern-lambda (conversion . any)
		   (error "Invalid conversion definition"))

   (pattern-lambda (type . any)
		   (error "Invalid type definition"))

   (pattern-lambda (typealias . any)
		   (error "Invalid typealias statement"))

   ))

(define identify-comprehensions
  (list
   
   ;; Comprehensions
   ;; This is really ugly. Comprehension is interpreted as
   ;; hcat where the first variable is a | b followed by
   ;; other expressions
   (pattern-lambda 
    (hcat (call (-/ |\||) expr (= i range)) . rest)
    `(comp ,expr (= ,i ,range) ,@rest))

)) ;; identify-comprehensions


(define lower-comprehensions
  (list

   ; 1d comprehensions
   (pattern-lambda 
    (comp expr (= i range))
    (let ((result (gensym)))
      `(block (= ,result (call zeros (call numel ,range)))
	      (for (= ,i ,range) (block (= (ref ,result ,i) ,expr)))
	      ,result )))
   
   ; 2d comprehensions
   (pattern-lambda 
    (comp expr (= i range1) (= j range2))
    (let ((result (gensym)))
      `(block (= ,result (call zeros (call numel ,range1) (call numel ,range2)))
              (for (= ,i ,range1)
                   (block (for (= ,j ,range2) 
                               (block (call set ,result ,i ,j ,expr)))))
              ,result )))

)) ;; lower-comprehensions


; (op (op a b) c) => (op a b c) etc.
(define (flatten-op op e)
  (if (atom? e) e
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
  (if (or (atom? e) (quoted? e)) e
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

; remove unnecessary nested blocks
(define (splice-blocks e)
  (define (splice-blocks- body)
    (cond ((atom? body) body)
	  ((equal? body '((block)))  ; leave empty block in value pos.
	   body)
	  ((and (pair? (car body))
		(eq? (caar body) 'block))
	   (append (splice-blocks- (cdar body))
		   (splice-blocks- (cdr body))))
	  ((and (pair? (car body))   ; remove (local ...) forms
		(eq? (caar body) 'local))
	   (splice-blocks- (cdr body)))
	  (else
	   (cons (splice-blocks (car body)) (splice-blocks- (cdr body))))))
  (if (or (atom? e) (quoted? e)) e
      (if (eq? (car e) 'block)
	  (let ((tl (splice-blocks- (cdr e))))
	    (if (length= tl 1)       ; unwrap length-1 blocks
		(car tl)
		(cons 'block tl)))
	  (map splice-blocks e))))

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
    (if (or (atom? e) (quoted? e))
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
	       (error "Misplaced return statement")
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
	   (if (and (eq? dest #t) (not tail))
	       (let* ((g (gensym))
		      (r (to-lff (cadr e) g tail)))
		 (cons g
		       ; tricky: need to introduce a new local outside the
		       ; scope-block so the scope-block's value can propagate
		       ; out. otherwise the value could be inaccessible due
		       ; to being wrapped inside a scope.
		       `((scope-block ,(to-blk r))
			 (local ,g))))
	       (let ((r (to-lff (cadr e) dest tail)))
		 (cons `(scope-block ,(to-blk r))
		       '()))))
	  
	  ((break) (if dest
		       (error "Misplaced break or continue")
		       (cons e '())))
	  
	  ((lambda)
	   (let ((l `(lambda ,(cadr e)
		       ,(to-blk (to-lff (caddr e) #t #t)))))
	     (if (symbol? dest)
		 (cons `(= ,dest ,l) '())
		 (cons (if tail `(return ,l) l) '()))))
	  
	  ((local)
	   (cons e '()))
	  
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
    (if (or (atom? e) (quoted? e))
	'()
	(case (car e)
	  ((lambda scope-block)  '())
	  ((local)  (list (decl-var (cadr e))))
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
    (if (or (atom? e) (quoted? e)) e
	(cond ((eq? (car e) 'lambda)
	       (let* ((env (append (llist-vars (cadr e)) env))
		      (body (add-local-decls (caddr e) env)))
		 (list 'lambda (cadr e) body)))
	      
	      ((eq? (car e) 'scope-block)
	       (let* ((vars (delete-duplicates
			     (find-assigned-vars (cadr e) env)))
		      (body (add-local-decls (cadr e) (append vars env))))
		 `(scope-block ,@(map (lambda (v) `(local ,v))
				      vars)
			       ,body)))
	      (else (map (lambda (x)
			   (add-local-decls x env))
			 e)))))
  (add-local-decls e '()))

(define (scope-block-vars e)
  (map (lambda (x) (decl-var (cadr x)))
       (filter (lambda (x)
		 (and (pair? x)
		      (eq? (car x) 'local)))
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
	((atom? e)        e)
	((quoted? e)      e)
	(else
	 (let* ((bound-vars  ; compute vars bound by current expr
		 (case (car e)
		   ((lambda)      (llist-vars (cadr e)))
		   ((scope-block) (scope-block-vars e))
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
    (cond ((or (atom? e) (quoted? e)) (cons e '()))
	  ((eq? (car e) 'lambda) (cons (flatten-scopes e) '()))
	  ((eq? (car e) 'scope-block)
	   (let ((vars (scope-block-vars e))
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
  
  (cond ((atom? e)   e)
	((quoted? e) e)
	((eq? (car e) 'lambda)
	 (let* ((argnames  (llist-vars (cadr e)))
		(body0     (caddr e))
		(body      (if (eq? (car body0) 'scope-block)
			       (car (last-pair body0))
			       body0))
		(l0
		 (if (eq? (car body0) 'scope-block)
		     (filter   ; remove locals conflicting with arg names
		      (lambda (v) (not (memq v argnames)))
		      (scope-block-vars body0))
		     '()))
		(r-s-b (remove-scope-blocks body)))
	   `(lambda ,(cadr e)
	      (locals ,@l0 ,@(cdr r-s-b))
	      ,(car r-s-b))))
	(else (map flatten-scopes e))))

(define (make-var-info name) (vector name 'Any #f))
(define (vinfo:name v) (vector-ref v 0))
(define (vinfo:type v) (vector-ref v 1))
(define (vinfo:capt v) (vector-ref v 2))
(define (vinfo:set-type! v t) (vector-set! v 1 t))
(define (vinfo:set-capt! v c) (vector-set! v 2 c))
(define (var-info-for name vinfo)
  (and (pair? vinfo)
       (or (and (eq? (vinfo:name (car vinfo)) name)
		(car vinfo))
	   (var-info-for name (cdr vinfo)))))

(define (lambda-all-vars e)
  (append (llist-vars (cadr e))
	  (cdr (caddr e))))

; convert each lambda's (locals ...) to
;   (var-info (locals ...) var-info-lst captured-var-names)
; where var-info-lst is a list of var-info records
; returns (expr . captured-vars), where captured-vars is a list of
; (var-info . frame) records.
(define (analyze-vars e env)
  ; returns #f or (var-info . frame)
  (define (var-lookup v env)
    (if (atom? env) #f
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
	((or (atom? e) (quoted? e)) (cons e '()))
	((eq? (car e) 'lambda)
	 (let* ((lvars (lambda-all-vars e))
		(vi    (map make-var-info lvars))
		(rec   (analyze-vars (cadddr e) (cons vi env)))
		; from this inner function's captured variables, select
		; those that come from frames above ours. we need to
		; capture those variables too, in order to pass them on
		; to this inner function.
		(cv    (filter (lambda (vi)
				 (member-p vi (cdr env)
					   (lambda (v e) (eq? (cdr v) e))))
			       (cdr rec))))
	   (cons `(lambda ,(cadr e)
		    (var-info ,(caddr e) ,vi ,(map (lambda (x)
						     (vinfo:name (car x)))
						   (cdr rec)))
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
;   new_closure(type, `expr, (capt-var1, capt-var2, ...))
; . for each closed var v, uses change to (call unbox (closure-ref idx))
; . assignments to closed var v change to (call boxset (closure-ref idx) rhs)
(define (closure-convert- e vinfo)
  (define (lookup v vinfo)
    (let ((vi (var-info-for v (caddr vinfo))))
	   (if vi
	       (if (vinfo:capt vi) 'boxed 'local)
	       (let ((i (index-of v (cadddr vinfo) 0)))
		 (or i 'global)))))
  
  (cond ((symbol? e)
	 (let ((l (lookup e vinfo)))
	   (cond ((eq? l 'boxed) `(call (top unbox) ,e))
		 ((number? l)    `(call (top unbox) (closure-ref ,l)))
		 (else e))))
	((or (atom? e) (quoted? e)) e)
	(else
	 (case (car e)
	   ((=)
	    (let ((l (lookup (cadr e) vinfo))
		  (rhs (closure-convert- (caddr e) vinfo)))
	      (cond ((eq? l 'boxed)
		     `(call (top boxset) ,(cadr e) ,rhs))
		    ((number? l)
		     `(call (top boxset) (closure-ref ,l) ,rhs))
		    (else
		     `(= ,(cadr e) ,rhs)))))
	   ((lambda)
	    (let ((vinf  (caddr e))
		  (args  (llist-vars (cadr e)))
		  (body0 (cadddr e))
		  (capt  (map vinfo:name
			      (filter vinfo:capt (caddr (caddr e))))))
	      (let ((body
		     `(block ,@(map (lambda (v)
				      `(= ,v
					  (call (top box) (top Any)
						,(if (memq v args)
						     v
						     '(null)))))
				    capt)
			     ,(closure-convert- body0 vinf))))
		`(call new_closure
		       (call ref Function
			     (call tuple ,@(llist-types (cadr e)))
			     Any)
		       (lambda ,(cadr e) ,(caddr e) ,body)
		       (call tuple
			     ; NOTE: to pass captured variables on to other
			     ; closures we must pass the box, not the value
			     ,@(map (lambda (x)
				      (let ((i (index-of x (cadddr vinfo) 0)))
					(if i
					    `(closure-ref ,i)
					    x)))
				    (cadddr vinf)))))))
	   (else
	    (cons (car e)
		  (map (lambda (x) (closure-convert- x vinfo))
		       (cdr e))))))))

(define (closure-convert e)
  (closure-convert- (analyze-variables e) '(var-info (locals) () ())))

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
    (let ((emit
	   (lambda (c)
	     (set! code (cons c code))
	     (set! ip (+ ip 1))))
	  (make-label
	   (lambda () (list #f)))
	  (mark-label
	   (lambda (l) (set-car! l ip))))
      (define (compile e break-labels)
	(if (atom? e) (emit e)
	    (case (car e)
	      ((if) (let ((elsel (make-label))
			  (endl  (make-label)))
		      (emit `(goto-ifnot ,(goto-form (cadr e)) ,elsel))
		      (compile (caddr e) break-labels)
		      (emit `(goto ,endl))
		      (mark-label elsel)
		      (compile (cadddr e) break-labels)
		      (mark-label endl)))
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
	      (else  (emit (goto-form e))))))
      (cond ((or (atom? e) (quoted? e)) e)
	    ((eq? (car e) 'lambda)
	     (let ((body (compile (cadddr e) '())))
	       `(lambda ,(cadr e) ,(caddr e) ,(list->vector (reverse code)))))
	    (else (map goto-form e))))))

(define (julia-expand ex)
  (goto-form
   (splice-blocks
    (closure-convert
     (flatten-scopes
      (identify-locals
       (to-LFF
	(expand-and-or
	 (pattern-expand patterns 
	  (pattern-expand lower-comprehensions
	   (pattern-expand identify-comprehensions ex)))))))))))
