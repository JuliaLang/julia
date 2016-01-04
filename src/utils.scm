;; for debugging, display x and return it
(define (prn x)
  (with-output-to *stderr*
                  (display x) (newline))
  x)

;; return the mapping for `elt` in `alst`, or `default` if not found
(define (lookup elt alst default)
  (let ((a (assq elt alst)))
    (if a (cdr a) default)))

;; items in `s1` and not in `s2`
(define (diff s1 s2)
  (cond ((null? s1)         '())
        ((memq (car s1) s2) (diff (cdr s1) s2))
        (else               (cons (car s1) (diff (cdr s1) s2)))))

(define (has-dups lst)
  (if (null? lst)
      #f
      (or (memq (car lst) (cdr lst))
          (has-dups (cdr lst)))))

;; does `expr` contain any substructure that satisfies predicate `p`?
(define (contains p expr)
  (or (p expr)
      (and (pair? expr)
           (any (lambda (x) (contains p x))
                expr))))

;; does `expr` contain something `eq?` to `x`, excluding list heads and quoted exprs
(define (expr-contains-eq x expr)
  (or (eq? expr x)
      (and (pair? expr)
           (not (quoted? expr))
           (any (lambda (y) (expr-contains-eq x y))
                (cdr expr)))))

;; same as above, with predicate
(define (expr-contains-p p expr)
  (or (p expr)
      (and (pair? expr)
           (not (quoted? expr))
           (any (lambda (y) (expr-contains-p p y))
                (cdr expr)))))

;; find all subexprs satisfying `p`, applying `key` to each one
(define (expr-find-all p expr key)
  (let ((found (if (p expr)
                   (list (key expr))
                   '())))
    (if (or (atom? expr) (quoted? expr))
        found
        (apply nconc
               found
               (map (lambda (x) (expr-find-all p x key))
                    (cdr expr))))))

(define (butlast lst)
  (if (or (null? lst) (null? (cdr lst)))
      '()
      (cons (car lst) (butlast (cdr lst)))))

(define (last lst)
  (if (null? (cdr lst))
      (car lst)
      (last (cdr lst))))

(define (take-while f xs)
  (cond ((null? xs) '())
        ((f (car xs)) (cons (car xs) (take-while f (cdr xs))))
        (else '())))

(define (without alst remove)
  (cond ((null? alst)               '())
        ((null? remove)             alst)
        ((memq (caar alst) remove)  (without (cdr alst) remove))
        (else                       (cons (car alst)
                                          (without (cdr alst) remove)))))

(define (caddddr x) (car (cdr (cdr (cdr (cdr x))))))
