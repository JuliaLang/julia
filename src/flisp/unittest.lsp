; -*- scheme -*-
(define-macro (assert-fail expr . what)
  `(assert (trycatch (begin ,expr #f)
		     (lambda (e) ,(if (null? what) #t
				      `(eq? (car e) ',(car what)))))))

(define (every-int n)
  (list (fixnum n) (int8 n) (uint8 n) (int16 n) (uint16 n) (int32 n) (uint32 n)
        (int64 n) (uint64 n)))

(define (every-sint n)
  (list (fixnum n) (int8 n) (int16 n) (int32 n) (int64 n)))

(define (each f l)
  (if (atom? l) ()
      (begin (f (car l))
	     (each f (cdr l)))))

(define (each^2 f l m)
  (each (lambda (o) (each (lambda (p) (f o p)) m)) l))

(define (test-lt a b)
  (each^2 (lambda (neg pos)
            (begin
              (eval `(assert (= -1 (compare ,neg ,pos))))
              (eval `(assert (=  1 (compare ,pos ,neg))))))
          a
          b))

(define (test-eq a b)
  (each^2 (lambda (a b)
            (begin
              (eval `(assert (= 0 (compare ,a ,b))))))
          a
          b))

(test-lt (every-sint -1) (every-int 1))
(test-lt (every-int 0) (every-int 1))
(test-eq (every-int 88) (every-int 88))
(test-eq (every-sint -88) (every-sint -88))

(define (test-square a)
  (each (lambda (i) (eval `(assert (>= (* ,i ,i) 0))))
        a))

(test-square (every-sint -67))
(test-square (every-int 3))
(test-square (every-int 0x80000000))
(test-square (every-sint 0x80000000))
(test-square (every-sint -0x80000000))

(assert (= (* 128 0x02000001) 0x100000080))

(assert (= (/ 1) 1))
(assert (= (/ -1) -1))
(assert (= (/ 2.0) 0.5))

(assert (= (- 4999950000 4999941999) 8001))

(assert (not (eqv? 10 #\newline)))
(assert (not (eqv? #\newline 10)))

; tricky cases involving INT_MIN
(assert (< (- #uint32(0x80000000)) 0))
(assert (> (- #int32(0x80000000)) 0))
(assert (< (- #uint64(0x8000000000000000)) 0))
(assert (> (- #int64(0x8000000000000000)) 0))

(assert (not (equal? #int64(0x8000000000000000) #uint64(0x8000000000000000))))
(assert (equal? (+ #int64(0x4000000000000000) #int64(0x4000000000000000))
		#uint64(0x8000000000000000)))
(assert (equal? (* 2 #int64(0x4000000000000000))
		#uint64(0x8000000000000000)))

(assert (equal? (uint64 (double -123)) #uint64(0xffffffffffffff85)))

(assert (equal? (string 'sym #byte(65) #wchar(945) "blah") "symA\u03B1blah"))

; -0.0 etc.
(assert (not (equal? 0.0 0)))
(assert (equal? 0.0 0.0))
(assert (not (equal? -0.0 0.0)))
(assert (not (equal? -0.0 0)))
(assert (not (eqv? 0.0 0)))
(assert (not (eqv? -0.0 0)))
(assert (not (eqv? -0.0 0.0)))
(assert (= 0.0 -0.0))

; hexadecimal float literals
(assert (= 12.0 0x1.8p3))
(assert (= 0.1875 0x1.8p-3))
(assert (= 0.5 0x.8p0))
(assert (= 12.0 0x1.8p3f))
(assert (= 0.1875 0x1.8p-3f))
(assert (= 0.5 0x.8p0f))

; this crashed once
(for 1 10 (lambda (i) 0))

; failing applications
(assert-fail ((lambda (x) x) 1 2))
(assert-fail ((lambda (x) x)))
(assert-fail ((lambda (x y . z) z) 1))
(assert-fail (car 'x) type-error)
(assert-fail gjegherqpfdf___trejif unbound-error)

; long argument lists
(assert (= (apply + (iota 100000)) 4999950000))
(define ones (map (lambda (x) 1) (iota 80000)))
(assert (= (eval `(if (< 2 1)
		      (+ ,@ones)
		      (+ ,@(cdr ones))))
	   79999))

(define MAX_ARGS 255)

(define as (apply list* (map-int (lambda (x) (gensym)) (+ MAX_ARGS 1))))
(define f (compile `(lambda ,as ,(lastcdr as))))
(assert (equal? (apply f (iota (+ MAX_ARGS 0))) `()))
(assert (equal? (apply f (iota (+ MAX_ARGS 1))) `(,MAX_ARGS)))
(assert (equal? (apply f (iota (+ MAX_ARGS 2))) `(,MAX_ARGS ,(+ MAX_ARGS 1))))

(define as (apply list* (map-int (lambda (x) (gensym)) (+ MAX_ARGS 100))))
(define ff (compile `(lambda ,as (set! ,(car (last-pair as)) 42)
			     ,(car (last-pair as)))))
(assert (equal? (apply ff (iota (+ MAX_ARGS 100))) 42))
(define ff (compile `(lambda ,as (set! ,(car (last-pair as)) 42)
			     (lambda () ,(car (last-pair as))))))
(assert (equal? ((apply ff (iota (+ MAX_ARGS 100)))) 42))

(define as (map-int (lambda (x) (gensym)) 1000))
(define f (compile `(lambda ,as ,(car (last-pair as)))))
(assert (equal? (apply f (iota 1000)) 999))

(define as (apply list* (map-int (lambda (x) (gensym)) 995)))
(define f (compile `(lambda ,as ,(lastcdr as))))
(assert (equal? (apply f (iota 994))  '()))
(assert (equal? (apply f (iota 995))  '(994)))
(assert (equal? (apply f (iota 1000)) '(994 995 996 997 998 999)))

; optional arguments
(assert (equal? ((lambda ((b 0)) b)) 0))
(assert (equal? ((lambda (a (b 2)) (list a b)) 1) '(1 2)))
(assert (equal? ((lambda (a (b 2)) (list a b)) 1 3) '(1 3)))
(assert (equal? ((lambda (a (b 2) (c 3)) (list a b c)) 1) '(1 2 3)))
(assert (equal? ((lambda (a (b 2) (c 3)) (list a b c)) 1 8) '(1 8 3)))
(assert (equal? ((lambda (a (b 2) (c 3)) (list a b c)) 1 8 9) '(1 8 9)))
(assert (equal? ((lambda ((x 0) . r) (list x r))) '(0 ())))
(assert (equal? ((lambda ((x 0) . r) (list x r)) 1 2 3) '(1 (2 3))))

; keyword arguments
(assert (keyword? kw:))
(assert (not (keyword? 'kw)))
(assert (not (keyword? ':)))
(assert (equal? ((lambda (x (a 2) (b: a) . r) (list x a b r)) 1 0 8 4 5)
		'(1 0 0 (8 4 5))))
(assert (equal? ((lambda (x (a 2) (b: a) . r) (list x a b r)) 0 b: 3 1)
		'(0 2 3 (1))))
(define (keys4 (a: 8) (b: 3) (c: 7) (d: 6)) (list a b c d))
(assert (equal? (keys4 a: 10) '(10 3 7 6)))
(assert (equal? (keys4 b: 10) '(8 10 7 6)))
(assert (equal? (keys4 c: 10) '(8 3 10 6)))
(assert (equal? (keys4 d: 10) '(8 3 7 10)))
(assert-fail (keys4 e: 10))   ; unsupported keyword
(assert-fail (keys4 a: 1 b:)) ; keyword with no argument

; cvalues and arrays
(assert (equal? (typeof "") '(array byte)))
(assert-fail (aref #(1) 3) bounds-error)
(define iarr (array 'int64 32 16 8 7 1))
(assert (equal? (aref iarr 0) 32))
(assert (equal? (aref iarr #int8(3)) 7))

; gensyms
(assert (gensym? (gensym)))
(assert (not (gensym? 'a)))
(assert (not (eq? (gensym) (gensym))))
(assert (not (equal? (string (gensym)) (string (gensym)))))
(let ((gs (gensym))) (assert (eq? gs gs)))

; eof object
(assert (eof-object? (eof-object)))
(assert (not (eof-object? 1)))
(assert (not (eof-object? 'a)))
(assert (not (eof-object? '())))
(assert (not (eof-object? #f)))
(assert (not (null? (eof-object))))
(assert (not (builtin? (eof-object))))
(assert (not (function? (eof-object))))

; ok, a couple end-to-end tests as well
(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
(assert (equal? (fib 20) 6765))

(load "color.lsp")
(assert (equal? (color-pairs (generate-5x5-pairs) '(a b c d e))
		'((23 . a) (9 . a) (22 . b) (17 . d) (14 . d) (8 . b) (21 . e)
		  (19 . b) (16 . c) (13 . c) (11 . b) (7 . e) (24 . c) (20 . d)
		  (18 . e) (15 . a) (12 . a) (10 . e) (6 . d) (5 . c) (4 . e)
		  (3 . d) (2 . c) (0 . b) (1 . a))))

; hashing strange things
(assert (equal?
	 (hash '#0=(1 1 #0# . #0#))
	 (hash '#1=(1 1 #1# 1 1 #1# . #1#))))

(assert (not (equal?
	      (hash '#0=(1 1 #0# . #0#))
	      (hash '#1=(1 2 #1# 1 1 #1# . #1#)))))

(assert (equal?
	 (hash '#0=((1 . #0#) . #0#))
	 (hash '#1=((1 . #1#) (1 . #1#) . #1#))))

(assert (not (equal?
	      (hash '#0=((1 . #0#) . #0#))
	      (hash '#1=((2 . #1#) (1 . #1#) . #1#)))))

(assert (not (equal?
	      (hash '#0=((1 . #0#) . #0#))
	      (hash '#1=((1 . #1#) (2 . #1#) . #1#)))))

(assert (equal?
	 (hash '(#0=(#0#) 0))
	 (hash '(#1=(((((#1#))))) 0))))

(assert (not (equal?
	      (hash '(#0=(#0#) 0))
	      (hash '(#1=(((((#1#))))) 1)))))

(assert (equal?
	 (hash #0=[1 [2 [#0#]] 3])
	 (hash #1=[1 [2 [[1 [2 [#1#]] 3]]] 3])))

(assert (not (equal?
	      (hash #0=[1 [2 [#0#]] 3])
	      (hash #1=[1 [2 [[5 [2 [#1#]] 3]]] 3]))))

(assert (equal?
	 (hash #0=[1 #0# [2 [#0#]] 3])
	 (hash #1=[1 #1# [2 [[1 #1# [2 [#1#]] 3]]] 3])))

(assert (not (equal?
	      (hash #0=[1 #0# [2 [#0#]] 3])
	      (hash #1=[6 #1# [2 [[1 #1# [2 [#1#]] 3]]] 3]))))

(assert (equal?
	 (hash [1 [2 [[1 1 [2 [1]] 3]]] 3])
	 (hash [1 [2 [[1 1 [2 [1]] 3]]] 3])))

(assert (not (equal?
	      (hash [6 1 [2 [[3 1 [2 [1]] 3]]] 3])
	      (hash [6 1 [2 [[1 1 [2 [1]] 3]]] 3]))))

(assert (equal? (hash '#0=(1 . #0#))
		(hash '#1=(1 1 . #1#))))

(assert (not (equal? (hash '#0=(1 1 . #0#))
		     (hash '#1=(1 #0# . #1#)))))

(assert (not (equal? (hash (iota 10))
		     (hash (iota 20)))))

(assert (not (equal? (hash (iota 41))
		     (hash (iota 42)))))

(assert (equal? `(a `(b c)) '(a (quasiquote (b c)))))
(assert (equal? ````x '```x))

#t
