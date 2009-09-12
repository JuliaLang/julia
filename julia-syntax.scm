; first passes:
; - expand lvalues, e.g. (= (call ref A i) x) => (= A (call assign A i x))
; - expand operators like +=
; - identify type name contexts and sort out ranges from declarations
; - expand for into while
; - expand -> and function into lambda/addmethod
; - replace (. a b) with (call get a (quote b))

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

   (pattern-lambda (function (call name (-- argl ...)) body)
		   `(= ,name
		       (addmethod ,name
				  ,(formal-arg-types argl)
				  (lambda ,(formal-arg-names argl)
				    ,body))))
   ))

(define (julia-expand ex)
  (pattern-expand patterns ex))
