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

(define (prn x) (display x) (newline) x)

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
