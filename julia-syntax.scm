; first passes:
; * expand lvalues, e.g. (= (call ref A i) x) => (= A (call assign A i x))
; - expand operators like +=
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
	     (if (pair? x)
		 (cadr x)
		 x))
	   (if (eq? (car arglist) 'tuple)
	       (cdr arglist)
	       arglist))
      (if (symbol? arglist)
	  (list arglist)
	  arglist)))

(define (formal-arg-types arglist)
  (if (pair? arglist)
      (map (lambda (x)
	     (if (and (pair? x) (eq? (car x) '|:|))
		 (caddr x)
		 'any))
	   (if (eq? (car arglist) 'tuple)
	       (cdr arglist)
	       arglist))
      (if (null? arglist)
	  arglist
	  (list 'any))))

(define (decl-var v)
  (if (and (pair? v) (eq? (car v) '|:|))
      (cadr v)
      v))

(define patterns
  (list
   (pattern-lambda (-> a b)
		   `(lambda ,(formal-arg-names a)
		      (scope-block ,b)))

   (pattern-lambda (|.| a b)
		   `(call getfield ,a (quote ,b)))

   (pattern-lambda (= (|.| a b) rhs)
		   `(call setfield ,a (quote ,b) ,rhs))

   (pattern-lambda (= (ref a (-- idxs ...)) rhs)
		   `(call set ,a ,@idxs ,rhs))

   (pattern-lambda (ref a (-- idxs ...))
		   `(call ref ,a ,@idxs))

   (pattern-lambda (function (call name (-- argl ...)) body)
		   `(= ,name
		       (addmethod ,name
				  ,(formal-arg-types argl)
				  (lambda ,(formal-arg-names argl)
				    (scope-block ,body)))))

   ; local x,y,z => local x;local y;local z
   (pattern-lambda (local (tuple (-- vars ...)))
		   `(block
		     ,@(map (lambda (x) `(local ,x)) vars)))

   ; local x:int=2 => local x:int; x=2
   (pattern-lambda (local (= var rhs))
		   `(block (local ,var)
			   (= ,(decl-var var) ,rhs)))

   ; adding break/continue support to while loop
   (pattern-lambda (while cnd body)
		   `(break-block loop-exit
				 (_while ,cnd
					 (break-block loop-cont
						      ,body))))

   (pattern-lambda (break) '(break loop-exit))
   (pattern-lambda (continue) '(break loop-cont))

   ; for loops
   (pattern-lambda
    (for (= var (: a b (-? c))) body)
    (begin
      (if (not (symbol? var))
	  (error "Invalid for loop syntax: expected symbol"))
      (if c
	  (let ((cnt (gensym))
		(lim (gensym)))
	    `(block
	      (= ,cnt 0)
	      (= ,lim (call / (call - ,c ,a) ,b))
	      (break-block loop-exit
			   (_while (call <= ,cnt ,lim)
				   (block
				    (= ,var (call + ,a (call * ,cnt ,b)))
				    (break-block loop-cont
						 ,body)
				    (= ,cnt (call + 1 ,cnt)))))))
	  `(block
	    (= ,var ,a)
	    (break-block loop-exit
			 (_while (call <= ,var ,b)
				 (block
				  (break-block loop-cont
					       ,body)
				  (= ,var (call + 1 ,var)))))))))

   ; macro for timing evaluation
   (pattern-lambda (call (-/ Time) expr) `(time ,expr))

   ))

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
  (define (identify-locals- e env)
    (if (not (pair? e)) e
	(cond ((eq? (car e) 'lambda)
	       (list 'lambda (cadr e)
		     (identify-locals- (caddr e)
				       (append (cadr e) env))))
	      ((eq? (car e) 'scope-block)
	       (let* ((vars (delete-duplicates
			     (find-assigned-vars (cadr e) env)))
		      (body (identify-locals- (cadr e) (append vars env))))
		 `(scope-block ,@(map (lambda (v) `(local ,v))
				      vars)
			       ,body)))
	      (else (map (lambda (x)
			   (identify-locals- x env))
			 e)))))
  (identify-locals- e '()))

(define (julia-expand ex)
  (identify-locals
   (pattern-expand patterns ex)))
