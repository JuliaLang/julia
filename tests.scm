(define-macro (assert expr) `(if ,expr #t (error "Assertion failed:" ',expr)))

; --- parser tests ---

(define-macro (tst str expr) `(assert (equal? (julia-parse ,str) ',expr)))

(tst "1+2" (call + 1 2))
(tst "[1,2].*[3,4].'" (call .* (hcat 1 2) (call transpose (hcat 3 4))))
(tst "[1,2;3,4]" (vcat (hcat 1 2) (hcat 3 4)))
(tst "[]" (hcat))
(tst "[[]]" (hcat (hcat)))
(tst "[a,]" (hcat a))
(tst "[a;]" (vcat a))
(tst "1:2" (: 1 2))
(tst "1:2:3" (: 1 2 3))
(tst "1:" (: 1 :))
(tst ":2" (: 2))
(tst "1:2:" (: 1 2 :))
(tst ":1:2" (: (: 1 2)))
(tst ":1:" (: (: 1 :)))
(tst "1:2:3:4" (: (: 1 2 3) 4))
(tst "1+2*3^-4-10" (call - (call + 1 (call * 2 (call ^ 3 (call - 4)))) 10))
(tst "b = [[2]].^2" (= b (call .^ (hcat (hcat 2)) 2)))
(tst "f(x+1)[i*2]-1" (call - (ref (call f (call + x 1)) (call * i 2)) 1))
(tst "A[i^2] = b'" (= (ref A (call ^ i 2)) (call ctranspose b)))
(tst "A[i^2].==b'" (comparison (ref A (call ^ i 2)) .== (call ctranspose b)))
(tst "{f(x),g(x)}" (list (call f x) (call g x)))
(tst "a::b.c" (|::| a (|.| b c)))
(tst "f(b,a...,c)" (call f b (... a) c))
(tst "(+)" +)
(tst "f(+,3)" (call f + 3))
(tst "f(3,+)" (call f 3 +))
(tst "a<b<c" (comparison a < b < c))
(tst "a<b>=c" (comparison a < b >= c))
(tst "a<(b<c)==d" (comparison a < (comparison b < c) == d))
(tst "a?b:c" (if a b c))
(tst "a||b ? (1 : 2) : 3" (if (|\|\|| a b) (: 1 2) 3))
(tst "a||b ? 1 : (2 : 3)" (if (|\|\|| a b) 1 (: 2 3)))
(tst "f(x, a?b:c, y)" (call f x (if a b c) y))

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

(if (comparison a < b)
    (block (block thing1) (block thing2))
    (if (comparison c < d)
	(block maybe)
	(block whatever))))

(tst
"while x < n
  x += 1
end
"
(while (comparison x < n) (block (+= x 1))))

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
(tst "f(b,c:int...;)" (call f b (... (|:| c int))))
(tst "f(b,c::int...;)" (call f b (... (|::| c int))))
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
(define-macro (flatten-op-pat op e)
  `(pattern-expand
    (list (pattern-lambda (,op (-- l ...) (-- inner (,op ...)) (-- r ...))
			  (cons ',op (append l (cdr inner) r))))
    ,e))

(assert (equal? (flatten-op-pat + '(+ (+ 1 2) (+ (+ (+ 3) 4) (+ 5))))
		'(+ 1 2 3 4 5)))

; --- type system tests ---

(define-macro (assert-subtype t1 t2)
  `(assert (subtype? ',(ty t1) ',(ty t2))))

(define-macro (assert-!subtype t1 t2)
  `(assert (not (subtype? ',(ty t1) ',(ty t2)))))

(assert-subtype "Int8" "Int")
(assert-subtype "Int32" "Int")
(assert-subtype "(Int8,Int8)" "(Int, Int)")
(assert-!subtype "Tensor[Float64,2]" "Tensor[Scalar,2]")
(assert-!subtype "Tensor[Float64,1]" "Tensor[Scalar,2]")
(assert-subtype "(Int, Int...)" "(Int, Scalar...)")
(assert-subtype "(Int, Float64, Int...)" "(Int, Scalar...)")
(assert-subtype "(Int, Float64)" "(Int, Scalar...)")
(assert-subtype "(Int32,)" "(Scalar...)")
(assert-subtype "()" "(Scalar...)")
(assert-!subtype "(Int32...)" "(Int32,)")
(assert-!subtype "(Int32...)" "(Scalar,Int,)")
(assert-!subtype "(Int...,)" "(Int, Int, Int...)")
(assert-!subtype "Buffer[Int8]" "Buffer[Any]")
(assert-!subtype "Buffer[Any]" "Buffer[Int8]")
(assert-subtype "Buffer[Int8]" "Buffer[Int8]")

(define (assert-conform t1 t2)
  (assert (conform t1 t2)))

(define (assert-!conform t1 t2)
  (assert (not (conform t1 t2))))

(let ((a (make-type-var 'a))
      (b (make-type-var 'b))
      (c (make-type-var 'c))
      (T (make-type-var 'T)))
  (assert-conform (julia-tuple a a)
		  (julia-tuple b c))
  (assert-!conform (julia-tuple a b)
		   (julia-tuple c c))
  (assert-conform (ty "(Int32,Int32,Int32)")
		  (julia-tuple (instantiate-type sequence-type (list T))))
  (assert-!conform (ty "(Int32,Int8,Int32)")
		   (julia-tuple (instantiate-type sequence-type (list T))))
  (assert-conform (ty "(Int32,)")
		  (julia-tuple scalar-type
			       (instantiate-type sequence-type (list T))))
  (assert-!conform (julia-tuple int32-type
				(instantiate-type sequence-type (list T)))
		   (ty "(Scalar,)"))
  (assert-conform (ty "(Buffer[Int8], Buffer[Int8])")
		  (julia-tuple (instantiate-type buffer-type (list T))
			       (instantiate-type buffer-type (list T))))
  (assert-!conform (ty "(Buffer[Int8], Buffer[Int16])")
		   (julia-tuple (instantiate-type buffer-type (list T))
				(instantiate-type buffer-type (list T)))))

; --- lowering tests ---

; use match to compare, because symbol names will not be equal due to gensyms
(assert (match
	 '(lambda (x y)
	    (locals n f size g6 g7)
	    (block (= n 0)
		   (= f (lambda (x n) (locals) (+ x n)))
		   (block (= y 1) (= g7 10) (= g6 12)
			  (call * g6 (call + y g7)))))
	 (flatten-scopes '(lambda (x y)
			    (scope-block
			     (local n)
			     (local f)
			     (local size)
			     (block (= n 0)
				    (= f (lambda (x n) (+ x n)))
				    (scope-block
				     (local z)
				     (local n)
				     (block
				      (= y 1)
				      (= n 10)
				      (= z 12)
				      (call * z (call + y n))))))))))

(assert (match
	 '(scope-block
	   (block (local g9)
		  (scope-block (= g9 b))
		  (return (call + 1 g9))))
	 (to-LFF
	  '(scope-block (call + 1 (scope-block b))))))

(assert (equal?
	 '(lambda ((|::| x Int8)) (scope-block x))
	 (pattern-expand patterns (julia-parse "x::Int8->x"))))
