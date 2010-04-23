(define (atom? x) (not (pair? x)))

(define (last-pair l)
  (if (atom? (cdr l))
      l
      (last-pair (cdr l))))

#;(define (list-head lst n)
  (if (<= n 0) '()
      (cons (car lst)
	    (list-head (cdr lst) (- n 1)))))

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

#;(define (assoc-p item lst test)
  (cond ((atom? lst) #f)
	((test       (caar lst) item) (car lst))
	(else        (assoc-p item (cdr lst) test))))

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
    (if (not (pair? lis)) '()
	(let ((head (car lis))
	      (tail (cdr lis)))
	  (if (pred head)
	      (let ((new-tail (recur tail)))
		(if (eq? tail new-tail) lis
		    (cons head new-tail)))
	      (recur tail))))))

(define (separate pred lst)
  (define (separate- pred lst yes no)
    (cond ((null? lst) (values (reverse yes) (reverse no)))
	  ((pred (car lst))
	   (separate- pred (cdr lst) (cons (car lst) yes) no))
	  (else
	   (separate- pred (cdr lst) yes (cons (car lst) no)))))
  (separate- pred lst '() '()))

(define (every pred lst)
  (or (not (pair? lst))
      (and (pred (car lst))
           (every pred (cdr lst)))))

(define (any pred lst)
  (and (pair? lst)
       (or (pred (car lst))
           (any pred (cdr lst)))))

#;(define (andmap proc l . ls)
  (or (null? l)
      (and (apply proc (car l) (map car ls))
           (apply andmap proc (cdr l) (map cdr ls)))))

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

(define (index-p pred lst start)
  (cond ((null? lst) #f)
	((pred (car lst)) start)
	(else (index-p pred (cdr lst) (+ start 1)))))

(define (foldl f zero lst)
  (if (null? lst) zero
      (foldl f (f (car lst) zero) (cdr lst))))

#;(define (check-same-length a b aShort bShort)
  (cond ((and (pair? a) (pair? b))
	 (check-same-length (cdr a) (cdr b) aShort bShort))
	((null? a) (if (null? b) #t (aShort)))
	(else      (bShort))))

#;(define (cons-in-order item lst key <)
  (if (null? lst)
      (list item)
      (if (< (key item) (key (car lst)))
	  (cons item lst)
	  (cons (car lst) (cons-in-order item (cdr lst) key <)))))

(define-macro (gambit-only . forms)
  (if (with-exception-catcher
       (lambda (e) #f)
       ; try a procedure that only exists in gambit
       (lambda () (or (uninterned-keyword? 0) #t)))
      `(begin ,@forms)
      #f))
