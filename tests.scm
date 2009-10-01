(define-macro (assert expr) `(if ,expr #t (error "Assertion failed:" ',expr)))

; --- parser tests ---

(define-macro (tst str expr) `(assert (equal? (julia-parse ,str) ',expr)))

(tst "1+2" (call + 1 2))
(tst "[1 2].*[3 4].'" (call .* (cat 1 2) (call transpose (cat 3 4))))
(tst "[1,2;3,4]" (cat (cat 1 2) (cat 3 4)))
(tst "1:2:3:4" (: (: 1 2 3) 4))
(tst "1+2*3^-4-10" (call - (call + 1 (call * 2 (call ^ 3 (call - 4)))) 10))
(tst "b = [[2]].^2" (= b (call .^ (cat (cat 2)) 2)))
(tst "f(x+1)[i*2]-1" (call - (ref (call f (call + x 1)) (call * i 2)) 1))
(tst "A[i^2] = b'" (= (ref A (call ^ i 2)) (call ctranspose b)))
(tst "A[i^2].==b'" (call .== (ref A (call ^ i 2)) (call ctranspose b)))
(tst "{f(x),g(x)}" (list (call f x) (call g x)))
(tst "a::b.c" (|.| (|::| a b) c))

; test newline as optional statement separator
(define s (make-token-stream (open-input-string "2\n-3")))
(assert (equal? (parse-eq s) 2))
(assert (equal? (parse-eq s) '(call - 3)))
(define s (make-token-stream (open-input-string "(2+\n3)")))
(assert (equal? (parse-eq s) '(call + 2 3)))

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

(if (call < a b)
    (block (block thing1) (block thing2))
    (if (call < c d)
	(block maybe)
	(block whatever))))

(tst
"while x < n
  x += 1
end
"
(while (call < x n) (block (+= x 1))))

(tst "f(;)" (call f))
(tst "f(a;)" (call f a))
(tst "f(;a)" (call f (parameters a)))
(tst "f(a,b;)" (call f a b))
(tst "f(;b,c)" (call f (parameters b c)))
(tst "f(b,c;a)" (call f b c (parameters a)))
(tst "f(c;a,b)" (call f c (parameters a b)))
(tst "f(b,c;a,d)" (call f b c (parameters a d)))

(tst "f(b...)" (call f (... b)))
(tst "f(b,c...;)" (call f b (... c)))
(tst "f(b,c:int...;)" (call f b (... (: c int))))
(tst "f(b,c...;a,)" (call f b (... c) (parameters a)))

(tst "(s...)" (tuple (... s)))
(tst "(s...,)" (tuple (... s)))
(tst "(s...;)" (tuple (... s)))
(tst "(a,s...)" (tuple a (... s)))
(tst "(a,s...,)" (tuple a (... s)))
(tst "(s...,b)" (tuple (... s) b))
(tst "(s...;b)" (tuple (... s) (parameters b)))

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

; --- type system tests ---

(define-macro (assert-subtype t1 t2)
  `(assert (subtype? ',(resolve-type-ex (julia-parse t1))
		     ',(resolve-type-ex (julia-parse t2)))))

(define-macro (assert-!subtype t1 t2)
  `(assert (not (subtype? ',(resolve-type-ex (julia-parse t1))
			  ',(resolve-type-ex (julia-parse t2))))))

(assert-subtype "Int8" "Int")
(assert-subtype "Int32" "Int")
(assert-subtype "(a, a)" "(b, c)")
(assert-!subtype "(a, b)" "(c, c)")
(assert-subtype "(Int8,Int8)" "(Int, Int)")
(assert-subtype "Tensor[Double,2]" "Tensor[Scalar,2]")
(assert-!subtype "Tensor[Double,1]" "Tensor[Scalar,2]")
(assert-subtype "(Int, Int...)" "(Int, Scalar...)")
(assert-subtype "(Int, Double, Int...)" "(Int, Scalar...)")
(assert-subtype "(Int, Double)" "(Int, Scalar...)")
(assert-subtype "(Int32,)" "(Scalar...)")
(assert-subtype "()" "(Scalar...)")
(assert-!subtype "(Int32...)" "(Int32,)")
(assert-!subtype "(Int32...)" "(Scalar,Int,)")
(assert-subtype "(Buffer[Int8], Buffer[Int8])" "(Buffer[T], Buffer[T])")
(assert-!subtype "(Buffer[Int8], Buffer[Int16])" "(Buffer[T], Buffer[T])")
