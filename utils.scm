(define (atom? x) (not (pair? x)))

(define (last-pair l)
  (if (atom? (cdr l))
      l
      (last-pair (cdr l))))

(define (delete-duplicates lst)
  (if (not (pair? lst))
      lst
      (let ((elt  (car lst))
	    (tail (cdr lst)))
	(if (member elt tail)
	    (delete-duplicates tail)
	    (cons elt
		  (delete-duplicates tail))))))

(define (member-p item lst test)
  (cond ((atom? lst) #f)
        ((test       item (car lst)) lst)
        (else        (member-p item (cdr lst) test))))

(define (delete-duplicates-p lst test)
  (if (not (pair? lst))
      lst
      (let ((elt  (car lst))
	    (tail (cdr lst)))
	(if (member-p elt tail test)
	    (delete-duplicates-p tail test)
	    (cons elt
		  (delete-duplicates-p tail test))))))

(define (list* first . rest)
  (let recur ((x first) (rest rest))
    (if (pair? rest)
	(cons x (recur (car rest) (cdr rest)))
	x)))

(define-macro (begin0 first . rest)
  (let ((g (gensym)))
    `(let ((,g ,first))
       ,@rest
       ,g)))

(define (prn x) (display x) (newline) x)

(define (filter pred lis)
  (let recur ((lis lis))
    (if (not (pair? lis)) lis
	(let ((head (car lis))
	      (tail (cdr lis)))
	  (if (pred head)
	      (let ((new-tail (recur tail)))
		(if (eq? tail new-tail) lis
		    (cons head new-tail)))
	      (recur tail))))))

(define (unique lst)
  (if (null? lst)
      '()
      (cons (car lst)
	    (filter (lambda (x) (not (eq? x (car lst))))
		    (unique (cdr lst))))))

(define (every pred lst)
  (or (not (pair? lst))
      (and (pred (car lst))
           (every pred (cdr lst)))))

(define (any pred lst)
  (and (pair? lst)
       (or (pred (car lst))
           (any pred (cdr lst)))))

(define (length= lst n)
  (cond ((< n 0)     #f)
	((= n 0)     (atom? lst))
	((atom? lst) (= n 0))
	(else        (length= (cdr lst) (- n 1)))))

(define (length> lst n)
  (cond ((< n 0)     lst)
	((= n 0)     (and (pair? lst) lst))
	((atom? lst) (< n 0))
	(else        (length> (cdr lst) (- n 1)))))

(define (lookup elt alst default)
  (let ((a (assq elt alst)))
    (if a (cdr a) default)))

(define (index-of item lst start)
  (cond ((null? lst) #f)
	((eqv? item (car lst)) start)
	(else (index-of item (cdr lst) (+ start 1)))))
