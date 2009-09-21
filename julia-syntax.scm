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

(define patterns
  (list
   (pattern-lambda (-> a b)
		   `(lambda ,(formal-arg-names a)
		      ,b))

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
				    ,body))))

   (pattern-lambda (while cnd body)
		   `(break-block loop-exit
				 (_while ,cnd
					 (break-block loop-cont
						      ,body))))

   (pattern-lambda (break) '(break loop-exit))
   (pattern-lambda (continue) '(break loop-cont))

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

   (pattern-lambda (call (-/ Time) expr) `(time ,expr))

   ))

(define (julia-expand ex)
  (pattern-expand patterns ex))
