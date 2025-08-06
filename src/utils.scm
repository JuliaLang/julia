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

(define (intersect s1 s2)
  (filter (lambda (x) (memq x s2)) s1))

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
(define (expr-contains-p p expr (filt (lambda (x) #t)))
  (and (filt expr)
       (or (p expr)
           (and (pair? expr)
                (not (quoted? expr))
                (any (lambda (y) (expr-contains-p p y filt))
                     (cdr expr))))))

(define (expr-replace p expr repl)
  (cond ((p expr) (repl expr))
        ((and (pair? expr) (not (quoted? expr)))
         (cons (car expr)
               (map (lambda (x) (expr-replace p x repl)) (cdr expr))))
        (else expr)))

;; find all subexprs satisfying `p`, applying `key` to each one
(define (expr-find-all p expr key (filt (lambda (x) #t)))
  (if (filt expr)
      (let ((found (if (p expr)
                       (list (key expr))
                       '())))
        (if (or (atom? expr) (quoted? expr))
            found
            (apply nconc
                   found
                   (map (lambda (x) (expr-find-all p x key filt))
                        (cdr expr)))))
      '()))

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

(define (caddddr x) (car (cdr (cdr (cdr (cdr x))))))
(define (cdddddr x) (cdr (cdr (cdr (cdr (cdr x))))))
(define (cadddddr x) (car (cdddddr x)))

(define (table.clone t)
  (let ((nt (table)))
    (table.foldl (lambda (k v z) (put! nt k v))
                 () t)
    nt))

;; `any`, but call predicate on every element in order no matter what
(define (eager-any pred lst)
  (let loop ((lst lst)
             (any #f))
    (if (null? lst)
        any
        (loop (cdr lst)
              (or (pred (car lst)) any)))))

;; construct a table mapping each element of `lst` to its index (1-indexed)
(define (symbol-to-idx-map lst)
  (let ((tbl (table)))
    (let loop ((xs lst) (i 1))
      (if (pair? xs)
          (begin (put! tbl (car xs) i)
                 (loop (cdr xs) (+ i 1)))))
    tbl))

;; keep at most the first element matching a given predicate
(define (keep-first pred lst)
  (cond ((null? lst) lst)
        ((pred (car lst))
         (cons (car lst) (filter (lambda (x) (not (pred x))) (cdr lst))))
        (else
         (cons (car lst) (keep-first pred (cdr lst))))))

(define (take lst n)
  (let loop ((lst lst) (n n) (out '()))
    (if (= n 0) (reverse out)
        (loop (cdr lst) (- n 1) (cons (car lst) out)))))

(define (drop lst n)
  (if (= n 0) lst
      (drop (cdr lst) (- n 1))))

;; functional update at position i
(define (list-set lst i val)
  (append (take lst i) (list val) (drop lst (+ i 1))))
