; -*- scheme -*-

; dictionaries ----------------------------------------------------------------
(define (dict-new) ())

(define (dict-extend dl key value)
  (cond ((null? dl)              (list (cons key value)))
        ((equal? key (caar dl))  (cons (cons key value) (cdr dl)))
        (else (cons (car dl) (dict-extend (cdr dl) key value)))))

(define (dict-lookup dl key)
  (cond ((null? dl)              ())
        ((equal? key (caar dl))  (cdar dl))
        (else (dict-lookup (cdr dl) key))))

(define (dict-keys dl) (map car dl))

; graphs ----------------------------------------------------------------------
(define (graph-empty) (dict-new))

(define (graph-connect g n1 n2)
  (dict-extend
   (dict-extend g n2 (cons n1 (dict-lookup g n2)))
   n1
   (cons n2 (dict-lookup g n1))))

(define (graph-adjacent? g n1 n2) (member n2 (dict-lookup g n1)))

(define (graph-neighbors g n) (dict-lookup g n))

(define (graph-nodes g) (dict-keys g))

(define (graph-add-node g n1) (dict-extend g n1 ()))

(define (graph-from-edges edge-list)
  (if (null? edge-list)
      (graph-empty)
    (graph-connect (graph-from-edges (cdr edge-list))
                   (caar edge-list)
                   (cdar edge-list))))

; graph coloring --------------------------------------------------------------
(define (node-colorable? g coloring node-to-color color-of-node)
  (not (member
        color-of-node
        (map
         (lambda (n)
           (let ((color-pair (assq n coloring)))
             (if (pair? color-pair) (cdr color-pair) ())))
         (graph-neighbors g node-to-color)))))

(define (try-each f lst)
  (if (null? lst) #f
      (let ((ret (f (car lst))))
	(if ret ret (try-each f (cdr lst))))))

(define (color-node g coloring colors uncolored-nodes color)
  (cond
   ((null? uncolored-nodes) coloring)
   ((node-colorable? g coloring (car uncolored-nodes) color)
    (let ((new-coloring
           (cons (cons (car uncolored-nodes) color) coloring)))
      (try-each (lambda (c)
                  (color-node g new-coloring colors (cdr uncolored-nodes) c))
                colors)))))

(define (color-graph g colors)
  (if (null? colors)
      (and (null? (graph-nodes g)) ())
      (color-node g () colors (graph-nodes g) (car colors))))

(define (color-pairs pairs colors)
  (color-graph (graph-from-edges pairs) colors))

; queens ----------------------------------------------------------------------
(define (can-attack x y)
  (let ((x1 (mod x 5))
        (y1 (truncate (/ x 5)))
        (x2 (mod y 5))
        (y2 (truncate (/ y 5))))
    (or (= x1 x2) (= y1 y2) (= (abs (- y2 y1)) (abs (- x2 x1))))))

(define (generate-5x5-pairs)
  (let ((result ()))
    (dotimes (x 25)
      (dotimes (y 25)
        (if (and (not (= x y)) (can-attack x y))
            (set! result (cons (cons x y) result)) ())))
    result))
