; first passes:
; * expand lvalues, e.g. (= (call ref A i) x) => (= A (call assign A i x))
; * expand operators like +=
; - identify type name contexts and sort out ranges from declarations
; - expand for into while
; * expand -> and function into lambda/addmethod
; * replace (. a b) with (call get a (quote b))
; - cat
; - tuple destructuring
; - validate argument lists, replace a=b in arg lists with keyword exprs

(define (formal-arg-names arglist)
  (if (pair? arglist)
      (map (lambda (x)
	     (if (and (pair? x) (eq? (car x) '...))
		 (list '... (decl-var (cadr x)))
		 (decl-var x)))
	   (if (eq? (car arglist) 'tuple)
	       (cdr arglist)
	       arglist))
      (if (symbol? arglist)
	  (list arglist)
	  arglist)))

(define (formal-arg-types arglist)
  (if (pair? arglist)
      (map (lambda (x)
	     (if (and (pair? x) (eq? (car x) '...))
		 (list '... (decl-type (cadr x)))
		 (decl-type x)))
	   (if (eq? (car arglist) 'tuple)
	       (cdr arglist)
	       arglist))
      (if (null? arglist)
	  arglist
	  (list 'Any))))

; get the variable name part of a declaration, x:int => x
(define (decl-var v)
  (if (and (pair? v) (eq? (car v) '|:|))
      (cadr v)
      v))

(define (decl-type v)
  (if (and (pair? v) (eq? (car v) '|:|))
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

(define patterns
  (list
   (pattern-lambda (-> a b)
		   `(lambda ,(formal-arg-names a)
		      (scope-block ,b)))

   (pattern-lambda (|.| a b)
		   `(call getfield ,a (quote ,b)))

   (pattern-lambda (= (|.| a b) rhs)
		   `(call setfield ,a (quote ,b) ,rhs))

   (pattern-lambda (= (ref a . idxs) rhs)
		   `(call set ,a ,@idxs ,rhs))

   (pattern-lambda (ref a . idxs)
		   `(call ref ,a ,@idxs))

   (pattern-lambda (list . elts)
		   `(call list ,@elts))

   (pattern-lambda (function (call name . argl) body)
		   `(= ,name
		       (addmethod ,name
				  ,(formal-arg-types argl)
				  (lambda ,(formal-arg-names argl)
				    (scope-block ,body)))))
   
   ; call with splat
   (pattern-lambda (call f ... (* _) ...)
		   (let ((argl (cddr __)))
		     (if (length= argl 1)
			 `(call apply ,f ,(cadar argl))
			 `(call apply ,f (build-args
					  ,@(map (lambda (x)
						   (if (and (length= x 2)
							    (eq? (car x) '*))
						       (cadr x)
						       `(tuple ,x)))
						 argl))))))
   
   ; local x,y,z => local x;local y;local z
   (pattern-lambda (local (tuple . vars))
		   `(block
		     ,@(map (lambda (x) `(local ,x)) vars)))

   ; local x:int=2 => local x:int; x=2
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
	      (= ,lim (call / (call - ,c ,a) ,b))
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
    (for (= x list) body)
    (let ((i (gensym)))
      `(for (= ,i (: 1 (call numel ,list)))
	    (block (= ,x (ref ,list ,i)) ,body)) ))

   ; macro for timing evaluation
   (pattern-lambda (call (-/ Time) expr) `(time ,expr))

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

   ;; Comprehensions
   
   ; Compute length of ranges
   (pattern-lambda (call numel (: x z)) `(call + 1 (call - ,z ,x)) )
   (pattern-lambda (call numel (: x y z)) `(call / (call + 1 (call - ,z ,x)) ,y) )

   ; 1d comprehensions
   (pattern-lambda 
    (cat (call (-/ |\||) expr (= i range)) )
    (let ((result (gensym)))
      `(block (= ,result (call zeros (call numel ,range))) 
	      (for (= ,i ,range) (block (call set ,result ,i ,expr)))
	      ,result )))
   
   ; 2d comprehensions
   (pattern-lambda 
    (cat (call (-/ |\||) expr (= i range1)) (= j range2) )
    (let ((result (gensym)))
      `(block (= ,result (call zeros (call numel ,range1) (call numel ,range2)))
	      (for (= ,i ,range1)
		   (block (for (= ,j ,range2) 
			       (block (call set ,result ,i ,j ,expr)))))
	      ,result )))

   )) ; patterns

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
  (if (atom? e) e
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
	  (else
	   (cons (splice-blocks (car body)) (splice-blocks- (cdr body))))))
  (if (atom? e) e
      (if (eq? (car e) 'block)
	  (cons 'block (splice-blocks- (cdr e)))
	  (map splice-blocks e))))

; remove control flow constructs from value position
;  (block ... (value-expr ... control-expr ...) ...) =>
;  (block ... (= var control-expr) (value-expr ... var ...) ...)
; except the assignment is incorporated into control-expr, so that
; control exprs are only in statement position
; these include if, block, break-block, scope-block
; the following don't yield values, so it's an error if they are ever
; found in expression position: _while, return, break
; everything else is not considered a control form, and is left in place
(define (unnest-control- e val?)
  (define (control? e)
    (and (pair? e)
	 (memq (car e) '(= if block break-block scope-block _while break))))
  ; returns (new-e . alist) where alist has entries of the form
  ; (var . expr), where in new-e expr has been replaced with var
  ; val? - are we in value position?
  (define (extract-ctl-forms e val?)
    (if (atom? e) (cons e '())
	(if (control? e)
	    (let ((result
		   (case (car e)
		     ((=)  (let ((r (extract-ctl-forms (caddr e) #t)))
			     (if (and (pair? (cdr r))
				      (control? (caddr e)))
				 (cons (cadr e)
				       (cons (cons (cadr e)
						   (cdar (cdr r)))
					     (cddr r)))
				 (cons `(= ,(cadr e) ,(car r))
				       (cdr r)))))

		     ((if) (let ((r (extract-ctl-forms (cadr e) #t)))
			     (cons `(if ,(car r)
					,(unnest-control- (caddr e) val?)
					,(if (pair? (cdddr e))
					     (unnest-control- (cadddr e) val?)
					     'false))
				   (cdr r))))

		     ((block break-block scope-block)
		      (cons (unnest-control- e val?) '()))
		     
		     ((_while)
		      (cons `(_while ,(unnest-control- (cadr e) #t)
				     ,(unnest-control- (caddr e) #f))
			    '()))
		     
		     ((break)
		      (if val? (error "Misplaced break or continue")
			  (cons e '())))

		     (else #f) ; not reached
		     )))
	      (if val?
		  (cond ((eq? (car e) '=)
			 (cons (cadr e) (cons (cons (cadr e) (car result))
					      (cdr result))))
			(else
			 (let ((g (gensym)))
			   (cons g (cons (cons g (car result))
					 (cdr result))))))
		  result))
	    
	    (case (car e)
	      ((type typealias quote time local)
	       (cons e '()))
	      ((lambda)
	       (cons (list 'lambda (cadr e) (unnest-control (caddr e)))
		     '()))
	      ((addmethod)
	       (cons (list 'addmethod (cadr e) (caddr e)
			   (unnest-control (cadddr e)))
		     '()))
	      (else	      
	       (let ((rec (map (lambda (x) (extract-ctl-forms x #t)) e)))
		 (cons (map car rec)
		       (apply append (map cdr rec)))))))))
  
  ; convert form expr so that it assigns its result to the given variable
  (define (result-convert e var)
    (if (atom? e) (if (eq? e var) e `(= ,var ,e))
	(case (car e)
	  ((=)  e)
	  ((if) `(if ,(cadr e)
		     ,(result-convert (caddr e)  var)
		     ,(result-convert (cadddr e) var)))
	  ((_while) `(block ,e (= ,var (tuple))))
	  ((break-block)
	   (list 'break-block (cadr e) (result-convert (caddr e) var)))
	  ((scope-block)
	   (list 'scope-block (result-convert (cadr e) var)))
	  ((block) (if (null? (cdr e)) `(= ,var (tuple))
		       (cons 'block
			     (let loop ((tl (cdr e)))
			       (if (null? tl) '()
				   (if (null? (cdr tl))
				       (cons (result-convert (car tl) var)
					     (loop (cdr tl)))
				       (cons (car tl) (loop (cdr tl)))))))))
	  (else `(= ,var ,e)))))
  
  ; convert the result of extract-ctl-forms to a list of executable forms
  (define (result-convert-all e val?)
    (let ((r (extract-ctl-forms e val?)))
      (append (map (lambda (p)
		     (result-convert (cdr p) (car p)))
		   (reverse (cdr r)))
	      (list (car r)))))
  
  (if (atom? e) e
      (case (car e)
	((type typealias quote time local)
	 e)
	((break-block)
	 (list 'break-block (cadr e) (unnest-control- (caddr e) val?)))
	((scope-block)
	 (list 'scope-block (unnest-control- (cadr e) val?)))
	((addmethod)
	 (list 'addmethod (cadr e) (caddr e)
	       (unnest-control (cadddr e))))
	((lambda)
	 (list 'lambda (cadr e) (unnest-control (caddr e))))
	((block)
	 (cons 'block
	       (let loop ((tl (cdr e)))
		 (if (null? tl) '()
		     (append (result-convert-all (car tl)
						 (and val? (null? (cdr tl))))
			     (loop (cdr tl)))))))
	(else
	 (let ((rc (result-convert-all e val?)))
	   (if (length= rc 1)
	       (car rc)
	       (cons 'block rc)))))))

(define (unnest-control e) (unnest-control- e #t))

; convert a lambda list into a list of just symbols
(define (lambda-vars lst)
  (map (lambda (v)
	 (if (symbol? v)
	     v
	     (case (car v)
	       ((...)         (cadr v))
	       ((= keyword)   (caddr v))
	       (else (error "Malformed function arguments" lst)))))
       lst))

; local variable identification
; convert (scope-block x) to `(scope-block ,@locals ,x)
; where locals is a list of (local x) expressions, derived from two sources:
; 1. (local x) expressions inside this scope-block and lambda
; 2. variables assigned inside this scope-block that don't exist in outer
;    scopes
(define (identify-locals e)
  (define (find-assigned-vars e env)
    (if (not (pair? e))
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
    (if (not (pair? e)) e
	(cond ((eq? (car e) 'lambda)
	       (list 'lambda (cadr e)
		     (add-local-decls (caddr e)
				      (append (lambda-vars (cadr e)) env))))
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
	(else
	 (let* ((bound-vars  ; compute vars bound by current expr
		 (case (car e)
		   ((lambda)      (lambda-vars (cadr e)))
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
    (cond ((atom? e) (cons e '()))
	  ((eq? (car e) 'lambda) (cons e '()))
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
  
  (cond ((atom? e) e)
	((eq? (car e) 'lambda)
	 (let* ((argnames  (lambda-vars (cadr e)))
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

(define (julia-expand ex)
  ;(splice-blocks
   (flatten-scopes
    (identify-locals
     ;(unnest-control
      (expand-and-or
       (pattern-expand patterns ex)))));))
