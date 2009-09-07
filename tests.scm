(define-macro (assert expr) `(if ,expr #t (error "Assertion failed:" ',expr)))

; --- parser tests ---

(define-macro (tst str expr) `(assert (equal? (julia-parse ,str) ',expr)))

(tst "1+2" (+ 1 2))
(tst "[1 2].*[3 4].'" (.* (cat 1 2) (transpose (cat 3 4))))
(tst "[1,2;3,4]" (cat (cat 1 2) (cat 3 4)))
(tst "1:2:3:4" (: (: 1 2 3) 4))
(tst "1+2*3^-4-10" (- (+ 1 (* 2 (^ 3 (- 4)))) 10))
(tst "b = [[2]].^2" (= b (.^ (cat (cat 2)) 2)))
(tst "f(x+1)[i*2]-1" (- (ref (call f (+ x 1)) (* i 2)) 1))
(tst "A[i^2] = b'" (= (ref A (^ i 2)) (ctranspose b)))
(tst "A[i^2].==b'" (.== (ref A (^ i 2)) (ctranspose b)))
(tst "{f(x),g(x)}" (list (call f x) (call g x)))
(tst "a::b.c" (|.| (|::| a b) c))

; test newline as optional statement separator
(define s (make-token-stream (open-input-string "2\n-3")))
(assert (equal? (parse-eq s) 2))
(assert (equal? (parse-eq s) '(- 3)))
(define s (make-token-stream (open-input-string "(2+\n3)")))
(assert (equal? (parse-eq s) '(+ 2 3)))

; tuples
(tst "2," (tuple 2))
(tst "(2,)" (tuple 2))
(tst "2,3" (tuple 2 3))
(tst "2,3," (tuple 2 3))
(tst "(2,3)" (tuple 2 3))
(tst "(2,3,)" (tuple 2 3))
(tst "()" (tuple))
(tst "( )" (tuple))

; statements
(define s (make-token-stream (open-input-string ";\n;;a;;;b;;\n;")))
(assert (equal? (julia-parse s) '(block)))
(assert (equal? (julia-parse s) '(block a b)))
(assert (equal? (julia-parse s) '(block)))
(assert (eof-object? (julia-parse s)))

(tst
"if a < b
  thing1;;;


  thing2;
elseif c < d
  # this is a comment
  #
  maybe

 #

#


else
    whatever
end"

(if (< a b)
    (block (block thing1) (block thing2))
    (if (< c d)
	(block maybe)
	(block whatever))))

(tst
"while x < n
  x += 1
end
"
(while (< x n) (block (+= x 1))))


; --- pattern matcher tests ---

(assert (equal? (pattern-expand (list (pattern-lambda (+ x x) `(* 2 ,x)))
				'(begin (a) (+ (f) (f)) (b)))
		'(begin (a) (* 2 (f)) (b))))

; a powerful pattern -- flatten all nested uses of an operator into one call
(define-macro (flatten-op op e)
  `(pattern-expand
    (pattern-lambda (,op (-- l ...) (-- inner (,op ...)) (-- r ...))
                    (cons ',op (append l (cdr inner) r)))
    ,e))

(assert (equal? (flatten-op + '(+ (+ 1 2) (+ (+ (+ 3) 4) (+ 5))))
		'(+ 1 2 3 4 5)))
