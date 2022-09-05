(define (info . args)
        (for-each (lambda (e) (display e) (display " ")) args)
        (display "\n")
        (car (reverse! args)))
(define (stream-pos s)
  (- (io.pos (aref s 1))
     (if (aref s 0)
         0
         0)))
(define parse-level (list '() 0 'toplevel ))
(define-macro (with-level current . body)
  `(with-bindings ((parse-level (list (car parse-level) (+ (cadr parse-level) 1) ,current)))
                  ,@body))
(define-macro (parse-define params . body)
  `(define ,params
      (with-level ',(car params)
        (let ((start-pos ,(if (member 's params) '(stream-pos s) -1)))
          (info (cdr parse-level) start-pos ,(if (member 's params) '(ts:last-tok s) -1) "<=")
          (let ((result (begin ,@body)) (end-pos ,(if (member 's params) '(stream-pos s) -1)))
              (info (cdr parse-level) (string start-pos ":" end-pos) "=>" result))))))
(define enable-debug #f)
(load "jlfrontend.scm")

; grep 'define (parse-' julia-parser.scm|sed 's/^(define (\(parse-[^ ]\+\) s.*/\1/g' | grep -v define | tr '\n' ' '
(define parse-names '(
  parse-Nary parse-block parse-stmts parse-eq parse-eq* parse-assignment parse-comma parse-pair parse-cond parse-arrow parse-or parse-and parse-comparison parse-pipe< parse-pipe> parse-range parse-chain parse-with-chains parse-expr parse-term parse-rational parse-shift parse-unary-subtype parse-where-chain parse-where parse-juxtapose parse-unary parse-unary-call parse-factor parse-factor-with-initial-ex parse-factor-after parse-decl parse-decl-with-initial-ex parse-call parse-call-with-initial-ex parse-unary-prefix parse-def parse-call-chain parse-subtype-spec parse-struct-field parse-struct-def parse-resword parse-do parse-imports parse-macro-name parse-atsym parse-import-dots parse-import-path parse-import parse-comma-separated parse-comma-separated-assignments parse-iteration-spec parse-comma-separated-iters parse-space-separated-exprs parse-call-arglist parse-arglist parse-vect parse-generator parse-comprehension parse-array parse-cat parse-paren parse-paren- parse-raw-literal parse-string-literal parse-interpolate parse-atom parse-docstring
))

(and enable-debug
  (for-each (lambda (name)
                    (eval `(define ,(symbol (string "-" name "-old")) ,name))
                    (eval `(parse-define (,name s . args)
                                  (apply ,(symbol (string "-" name "-old")) (cons s args)))))
            parse-names))

; ((curry1 parse-atom) (make-token-stream (open-input-string "1")))
; ((curry1 parse-raw-literal #\') (make-token-stream (open-input-string "'ls -la'")))
(define (curry1 func . args)
  (lambda (s) (apply func (cons s args))))

(define underscore-gen-block #f)

(define-macro (parse-expect s production expected)
  `(let ((production-name ',(if (list? production) (cadr production) production)))
        (info "======>" 'test (string "'" ,s "'") production-name)
        (set! *gensy-counter* 1)
        (let ((result (julia-parse ,s ,production)))
             (if (equal? result ,expected)
                 #t
                 (error (string "parse \"" ,s "\" failed: "
                                result " != " ,expected))))))

(define (comma-expected origin newsy (paren #f))
  (if paren
    `(-> ,newsy (tuple ,origin ,newsy))
    `(tuple ,origin (-> ,newsy ,newsy))))
(define-macro (parse-expect-underscore s production expected . expected2)
  `(and (parse-expect ,s ,production ,expected)
        (let* ((expected2 ,(if (pair? expected2) (car expected2) expected))
               (result2 (parse-expect ,(string "(" s ")") ,production expected2))
               (newsy (named-gensy '_))
               (comma-expected-f (comma-expected expected2 newsy #f))
               (comma-expected-t (comma-expected expected2 newsy #t)))
          (and result2
               (parse-expect ,(string "(" s "),_") ,production comma-expected-f)
               (parse-expect ,(string "((" s "),_)") ,production comma-expected-t)
               ))))
(define-macro (parse-expect-underscore-function s production expected expected2)
  `(and (parse-expect ,s ,production ,expected)
        (parse-expect ,(string "function " s " end") parse-stmts '(function ,,expected2 (block (line 1 none) (line 1 none))))
        (parse-expect ,(string s " = 1") parse-stmts '(= ,,expected2 (block (line 1 none) 1)))))

(define left-paren #\()
(define right-paren #\))

(and
(and
  (parse-expect "1+1" parse-stmts '(call + 1 1))
  (parse-expect "x->x,1" parse-stmts '(tuple (-> x (block (line 1 none) x)) 1))
  (parse-expect "(sqrt ∘ +)(3, 6)" parse-stmts '(call (call ∘ sqrt +) 3 6))
  (parse-expect "b, c" parse-comma '(tuple b c))
  (parse-expect "b" parse-comma 'b)
  (parse-expect "(x::Int, a::Array{T} where T>:Int) -> 1" parse-stmts
    '(-> (tuple (:: x Int) (:: a (where (curly Array T) (>: T Int)))) (block (line 1 none) 1)))
  (parse-expect "((x::Int, a::Array{T}) where T>:Int) -> 1" parse-stmts
    '(-> (where (tuple (:: x Int) (:: a (curly Array T))) (>: T Int)) (block (line 1 none) 1)))
  (parse-expect "(x::Int, a::Array{T} where T>:Int) -> 1" parse-stmts
    '(-> (tuple (:: x Int) (:: a (where (curly Array T) (>: T Int)))) (block (line 1 none) 1)))
  (parse-expect "x::Int -> 1" parse-stmts
    '(-> (:: x Int) (block (line 1 none) 1)))
  (parse-expect "::Int -> 1" parse-stmts
    '(-> (:: Int) (block (line 1 none) 1)))

  (parse-expect "&" parse-unary-prefix '&)
  (parse-expect "&a" parse-unary-prefix '(& a))
  (parse-expect "::Int" parse-unary-prefix '(:: Int))
  (parse-expect "++a" parse-unary-prefix '++) ; fallback to atom ++

  ; atom
  (parse-expect "'c'" parse-atom '#\c)
  (parse-expect "'\"'" parse-atom '#\")
  (parse-expect "'\\x00'" parse-atom '(julia_char 0))
  (parse-expect ":" parse-atom ':)
  (parse-expect ":a" parse-atom '(quote a))
  (parse-expect ":'c'" parse-atom '(quote #\c))
  ; (parse-expect "(:)" parse-atom ':)
  (parse-expect "a" parse-atom 'a)
  (parse-expect "var\"my var\"" parse-atom (symbol "my var"))
  (parse-expect "true" parse-atom '(true))
  (parse-expect "false" parse-atom '(false))
  (parse-expect "()" parse-atom '(tuple))
  (parse-expect "(1)" parse-atom 1)
  (parse-expect "(1,2)" parse-atom '(tuple 1 2))
  (parse-expect "[]" parse-atom '(vect))
  (parse-expect "[1]" parse-atom '(vect 1))
  (parse-expect "{}" parse-atom '(braces))
  (parse-expect "{1,2}" parse-atom '(braces 1 2)) ; vect => braces
  (parse-expect "{1 2}" parse-atom '(bracescat (row 1 2))) ; hcat => bracescat @ row
  (parse-expect "{x for x in a}" parse-atom '(braces (generator x (= x a)))) ; comprehension => braces
  (parse-expect "{1;2}" parse-atom '(bracescat 1 2))  ; else => bracescat
  (parse-expect "\"str\"" parse-atom "str")
  (parse-expect "@m x" parse-atom '(macrocall @m (line 1 none) x))
  (parse-expect "@m(x)" parse-atom '(macrocall @m (line 1 none) x))
  (parse-expect "`ls -la`" parse-atom '(macrocall (core @cmd) (line 1 none) "ls -la"))
  (parse-expect "f(x)" parse-atom 'f) ; extra call would be ignored

  ; raw-literal
  (parse-expect "'ls -la'" (curry1 parse-raw-literal #\') "ls -la")
  (parse-expect "|ls -la|" (curry1 parse-raw-literal #\|) "ls -la")
  (parse-expect "\"ls -la\\\'\\\"\"" (curry1 parse-string-literal #\" #f) '("ls -la'\""))
  (parse-expect "\"ls -la\\\'\\\"\"" (curry1 parse-string-literal #\" #t) '("ls -la\\'\""))
  (parse-expect "\'ls -la\\\'\\\"\'" (curry1 parse-string-literal #\' #f) '("ls -la'\""))
  (parse-expect "\'ls -la\\\'\\\"\'" (curry1 parse-string-literal #\' #t) '("ls -la'\\\""))

  ; parse-arglist note left-paren would be token by caller while right-paren token by callee
  (parse-expect "a, b, c)" (curry1 parse-arglist right-paren) '(a b c))
  (parse-expect "a, b; c)" (curry1 parse-arglist right-paren) '((parameters c) a b))
  (parse-expect "a, b; c)" (curry1 parse-arglist right-paren #t) '((parameters (line 1 none) c) a b))
  (parse-expect "a; b, c]" (curry1 parse-arglist #\]) '((parameters b c) a))
  (parse-expect "x for x in c)" (curry1 parse-arglist right-paren) '((generator x (= x c))))

  (parse-expect "a)" (curry1 parse-paren- #t) '(a . #f))
  (parse-expect "a, b)" (curry1 parse-paren- #t) '((tuple a b) . #t))
  (parse-expect "a=1)" (curry1 parse-paren- #t) '((= a 1) . #f))
  (parse-expect "a=1, b=2)" (curry1 parse-paren- #t) '((tuple (= a 1) (= b 2)) . #t))

  ; parse-eq
  (parse-expect "a, b" parse-eq '(tuple a b))
  (parse-expect "a, b" parse-eq* 'a)
  #t)


; sed -i 's/ \(#[0-9]\+#_\)/ |\1|/g'
;; this test case are align with that in scala
(and
  (parse-expect-underscore "(_+(1))" parse-stmts
    '(-> |#1#_| (call + |#1#_| 1)))
  (parse-expect-underscore "_+_" parse-stmts
    '(-> (tuple |#1#_| |#2#_|) (call + |#1#_| |#2#_|)))
  (parse-expect-underscore "_,_" parse-stmts
    '(tuple (-> |#1#_| |#1#_|) (-> |#2#_| |#2#_|))
    '(-> (tuple |#1#_| |#2#_|) (tuple |#1#_| |#2#_|)))
  (parse-expect-underscore "_+1,_" parse-stmts
    '(tuple (-> |#1#_| (call + |#1#_| 1)) (-> |#2#_| |#2#_|))
    '(-> |#2#_| (tuple (-> |#1#_| (call + |#1#_| 1)) |#2#_|)))
  (parse-expect-underscore "f(_)" parse-stmts
    '(-> |#1#_| (call f |#1#_|)))
  (parse-expect-underscore "f(_),_" parse-stmts
    '(tuple (-> |#1#_| (call f |#1#_|)) (-> |#2#_| |#2#_|))
    '(-> |#2#_| (tuple (-> |#1#_| (call f |#1#_|)) |#2#_|)))
  (parse-expect-underscore "f(_,_+1)" parse-stmts
    '(-> |#1#_| (call f |#1#_| (-> |#2#_| (call + |#2#_| 1)))))
  (parse-expect-underscore "f(_+1,_)" parse-stmts
    '(-> |#2#_| (call f (-> |#1#_| (call + |#1#_| 1)) |#2#_|)))
  (parse-expect-underscore "_.a(_).b(_)" parse-stmts
    '(-> (tuple |#1#_| |#2#_| |#3#_|) (call (|.| (call (|.| |#1#_| 'a) |#2#_|) 'b) |#3#_|)))
  (parse-expect-underscore "_.a(_).b(_),_" parse-stmts
    '(tuple (-> (tuple |#1#_| |#2#_| |#3#_|) (call (|.| (call (|.| |#1#_| 'a) |#2#_|) 'b) |#3#_|)) (-> |#4#_| |#4#_|))
    '(-> |#4#_| (tuple (-> (tuple |#1#_| |#2#_| |#3#_|) (call (|.| (call (|.| |#1#_| 'a) |#2#_|) 'b) |#3#_|)) |#4#_|)))
  (parse-expect-underscore "b(_) + 1", parse-stmts
    '(-> |#1#_| (call + (call b |#1#_|) 1)))
  (parse-expect-underscore "(1, b(_, 2))", parse-stmts
    '(tuple 1 (-> |#1#_| (call b |#1#_| 2))))
  (parse-expect-underscore "(1, b(_, 2)+1)", parse-stmts
    '(tuple 1 (-> |#1#_| (call + (call b |#1#_| 2) 1))))
  (parse-expect-underscore "a(1, b(_, 2))", parse-stmts
    '(call a 1 (-> |#1#_| (call b |#1#_| 2))))
  (parse-expect-underscore "a(1, b(_, 2)+1)", parse-stmts
    '(call a 1 (-> |#1#_| (call + (call b |#1#_| 2) 1))))
  (parse-expect-underscore "f(_, _+1) + 2", parse-stmts
    '(-> |#1#_| (call + (call f |#1#_| (-> |#2#_| (call + |#2#_| 1))) 2)))

  (parse-expect-underscore "a=_" parse-stmts
    '(= a (-> |#1#_| |#1#_|))
    '(-> |#1#_| (= a |#1#_|)))
  (parse-expect-underscore "a=(_)" parse-stmts
    '(= a (-> |#1#_| |#1#_|)))
  (parse-expect-underscore "a=_+1" parse-stmts ; shall (a=_+1) means x->a=x+1
    '(= a (-> |#1#_| (call + |#1#_| 1))))
  #t)

(and
  (parse-expect-underscore "[_, _+1]" parse-stmts
    '(-> |#1#_| (vect |#1#_| (-> |#2#_| (call + |#2#_| 1)))))
  (parse-expect-underscore "[_+a for a in arr]" parse-stmts ; shall this be x->[x+a for a in arr]
    '(comprehension (generator (-> |#1#_| (call + |#1#_| a)) (= a arr))))
  (parse-expect-underscore "_[_]" parse-stmts
    '(-> (tuple |#1#_| |#2#_|) (ref |#1#_| |#2#_|)))
  (parse-expect-underscore "_[_, _]" parse-stmts
    '(-> (tuple |#1#_| |#2#_| |#3#_|) (ref |#1#_| |#2#_| |#3#_|)))
  (parse-expect-underscore "_[_, _+1]" parse-stmts
    '(-> (tuple |#1#_| |#2#_|) (ref |#1#_| |#2#_| (-> |#3#_| (call + |#3#_| 1)))))
  #t)

(load "test/underscore-github.scm")

(and
  (parse-expect-underscore "_=1" parse-stmts
    '(= _ 1))
  (parse-expect-underscore "f(_) = 1" parse-stmts
    '(= (call f _) (block (line 1 none) 1)))
  (parse-expect-underscore "f(_; a=_, _=2) = 1" parse-stmts
    '(= (call f (parameters (kw a (-> |#2#_| |#2#_|)) (kw _ 2)) _) (block (line 1 none) 1)))
  (parse-expect-underscore "f((x, _)) = 1" parse-stmts
    '(= (call f (tuple x _)) (block (line 1 none) 1)))
  (parse-expect-underscore "f((x, _); a=_) = 1" parse-stmts
    '(= (call f (parameters (kw a (-> |#2#_| |#2#_|))) (tuple x _)) (block (line 1 none) 1)))
  (parse-expect-underscore "a, _ = t" parse-stmts
    '(= (tuple a _) t)
    '(tuple a (= _ t)))
  (parse-expect-underscore "a, _... = t" parse-stmts
    '(= (tuple a (... _)) t)
    '(tuple a (= (... _) t)))
  (parse-expect-underscore "a, f(_) = t" parse-stmts
    '(= (tuple a (call f _)) t)
    '(tuple a (= (call f _) (block (line 1 none) t))))
  (parse-expect-underscore "a, (_, b) = t" parse-stmts
    '(= (tuple a (tuple _ b)) t)
    '(tuple a (= (tuple _ b) t)))

  (parse-expect-underscore "a, (_, b) = _ = t" parse-stmts
    '(= (tuple a (tuple _ b)) (= _ t))
    '(tuple a (= (tuple _ b) (= _ t))))
  (parse-expect-underscore "_ = a, (_, b) = t" parse-stmts
    '(= _ (= (tuple a (tuple _ b)) t))
    '(tuple (= _ a) (= (tuple _ b) t)))
  (parse-expect-underscore "(_, a) -> 1" parse-stmts
    '(-> (tuple _ a) (block (line 1 none) 1)))
  (parse-expect-underscore "_ -> 1" parse-stmts
    '(-> _ (block (line 1 none) 1)))
  (parse-expect-underscore "_::Int -> 1" parse-stmts
    '(-> (:: _ Int) (block (line 1 none) 1)))

  (parse-expect-underscore-function "f(_)" parse-stmts
    '(-> |#1#_| (call f |#1#_|))
    '(call f _))
  (parse-expect-underscore-function "f(_::Any)" parse-stmts
    '(-> |#1#_| (call f (:: |#1#_| Any)))
    '(call f (:: _ Any)))
  (parse-expect-underscore-function "f((_, a)::Pair)" parse-stmts
    '(call f (:: (-> |#1#_| (tuple |#1#_| a)) Pair))
    '(call f (:: (tuple _ a) Pair)))
  (parse-expect-underscore-function "f(_, a)" parse-stmts
    '(-> |#1#_| (call f |#1#_| a))
    '(call f _ a))
  (parse-expect-underscore-function "f(_; a)" parse-stmts
    '(-> |#1#_| (call f (parameters a) |#1#_|))
    '(call f (parameters a) _))
  (parse-expect-underscore-function "f(_; _)" parse-stmts
    '(-> (tuple |#1#_| |#2#_|) (call f (parameters |#2#_|) |#1#_|))
    '(call f (parameters _) _))
  (parse-expect-underscore-function "f(_; _=_)" parse-stmts
    '(-> (tuple |#1#_| |#3#_|) (call f (parameters (kw _ |#3#_|)) |#1#_|))
    '(call f (parameters (kw _ (-> |#3#_| |#3#_|))) _))
  (parse-expect-underscore-function "f(a; kw=_)" parse-stmts
    '(-> |#1#_| (call f (parameters (kw kw |#1#_|)) a))
    '(call f (parameters (kw kw (-> |#1#_| |#1#_|))) a))
  (parse-expect-underscore-function "f(a; kw=_+1)" parse-stmts
    '(call f (parameters (kw kw (-> |#1#_| (call + |#1#_| 1)))) a)
    '(call f (parameters (kw kw (-> |#1#_| (call + |#1#_| 1)))) a))
  (parse-expect-underscore-function "f(_, _=_; kw=_)" parse-stmts
    '(-> (tuple |#1#_| |#3#_| |#4#_|) (call f (parameters (kw kw |#4#_|)) |#1#_| (kw _ |#3#_|)))
    '(call f (parameters (kw kw (-> |#4#_| |#4#_|))) _ (kw _ (-> |#3#_| |#3#_|))))

  ; array.jl:213
  (parse-expect-underscore "elsize(@nospecialize _::Type{A}) where {T,A<:Array{T}} = aligned_sizeof(T)" parse-stmts
    '(= (where (call elsize (macrocall @nospecialize (line 1 none) (:: _ (curly Type A)))) T (<: A (curly Array T))) (block (line 1 none) (call aligned_sizeof T))))
  ; multidimensional.jl:643
  (parse-expect-underscore "@inline simd_inner_length(iter::CartesianPartition, (_, len, _)::Tuple{Int,Int,CartesianIndex}) = len" parse-stmts
    '(macrocall @inline (line 1 none) (= (call simd_inner_length (:: iter CartesianPartition) (:: (tuple _ len _) (curly Tuple Int Int CartesianIndex))) (block (line 1 none) len))))
  ; compiler/abstractinterpretation.jl:970
  (parse-expect-underscore "function const_prop_argument_heuristic(_::AbstractInterpreter, (; fargs, argtypes)::ArgInfo, sv::InferenceState) end" parse-stmts
    '(function (call const_prop_argument_heuristic (:: _ AbstractInterpreter) (:: (tuple (parameters fargs argtypes)) ArgInfo) (:: sv InferenceState)) (block (line 1 none) (line 1 none))))
  ; compiler/typeinfer.jl:254
  (parse-expect-underscore "(caller for (caller, _, _) = results)" parse-stmts
    '(generator caller (= (tuple caller _ _) results)))
  (parse-expect-underscore "(caller for (caller, _, _) in results)" parse-stmts
    '(generator caller (= (tuple caller _ _) results)))
  (parse-expect-underscore "for (caller, _, _) in results end" parse-stmts
    '(for (= (tuple caller _ _) results) (block (line 1 none) (line 1 none))))
  ; compiler/ssair/inlining.jl:404
  (parse-expect-underscore "for ((_, idx′), stmt′) in inline_compact end" parse-stmts
    '(for (= (tuple (tuple _ idx′) stmt′) inline_compact) (block (line 1 none) (line 1 none))))
  ; compiler/tfuncs.jl:2274
  (parse-expect-underscore "function setglobal!_tfunc(@nospecialize(M), @nospecialize(s), @nospecialize(v), @nospecialize(_=Symbol)) end" parse-stmts
    '(function (call setglobal!_tfunc (macrocall @nospecialize (line 1 none) M) (macrocall @nospecialize (line 1 none) s) (macrocall @nospecialize (line 1 none) v) (macrocall @nospecialize (line 1 none) (= _ Symbol))) (block (line 1 none) (line 1 none))))
  ; compiler/ssair/EscapeAnalysis/EscapeAnalysis.jl:1353
  (parse-expect-underscore "escape_builtin!(::typeof(===), _...) = return false" parse-stmts
    '(= (call escape_builtin! (:: (call typeof ===)) (... _)) (block (line 1 none) (return (false)))))
  ; compiler/inference.jl:2196
  (parse-expect-underscore "@test @inferred(g26172(Val(10))) === ntuple(_ -> nothing, 10)" parse-stmts
    '(macrocall @test (line 1 none) (call === (macrocall @inferred (line 1 none) (call g26172 (call Val 10))) (call ntuple (-> _ (block (line 1 none) nothing)) 10))))
  ; compiler/codegen.jl:356
  (parse-expect-underscore "mktemp() do f_22330, _ end" parse-stmts
    '(do (call mktemp) (-> (tuple f_22330 _) (block (line 1 none)))))
  ; lock.jl:45
  (parse-expect-underscore "struct ReentrantLock <: AbstractLock\n_::NTuple{Int === Int32 ? 2 : 3, Int}\nend" parse-stmts
    '(struct (false) (<: ReentrantLock AbstractLock) (block (line 2 none) (:: _ (curly NTuple (if (call === Int Int32) 2 3) Int)))))
  ; normal.jl:222 in @eval
  (parse-expect-underscore "_randfun = Symbol(:_, randfun)" parse-stmts
    '(= _randfun (call Symbol '_ randfun)))
  ; test/testhelpers/arrayindexingtypes.jl:24
  ; T24Linear{T  }(X::AbstractArray{_,N}) where {T,N,_} = T24Linear{T,N}(X)
  ; test/intfuncs.jl:247
  (parse-expect-underscore "try gcdx(x, m)[1] == 1 catch _ true end" parse-stmts
    '(try (block (line 1 none) (call == (ref (call gcdx x m) 1) 1)) _ (block (line 1 none) (true))))
  ; https://github.com/JuliaLang/julia/pull/39139
  ; @test [_ for _ in 1:5] == 1:5

  ; test/REPL.jl:
  (parse-expect "CompletionFoo.?([1,2,3], 2.0)" parse-stmts
    '(call (|.| CompletionFoo '?) (vect 1 2 3) 2.0))
  ; REPL/src/REPLCompletions.jl
  (parse-expect-underscore "_([1,2,3], 2.0)" parse-stmts
    '(-> |#1#_| (call |#1#_| (vect 1 2 3) 2.0)))
  (parse-expect-underscore "__([1,2,3], 2.0)" parse-stmts
    '(call __ (vect 1 2 3) 2.0))
  ; test/syntax.jl:1188
  (parse-expect-underscore "2e3_\"x\"" parse-stmts
    '(call * 2000.0 (macrocall @__str (line 1 none) "x")))
  (parse-expect-underscore "_\"x\"_" parse-stmts
    '(macrocall @__str (line 1 none) "x" "_"))
  ; test/syntax.jl:1431
  (parse-expect "
    A = function (s, o...)
      f(a, b) do
      end
    end,
    B = function (s, o...)
      f(a, b) do
      end
    end" parse-stmts
    '(= A (= (tuple (function (tuple s (... o)) (block (line 2 none) (line 3 none) (do (call f a b) (-> (tuple) (block (line 4 none)))))) B) (function (tuple s (... o)) (block (line 6 none) (line 7 none) (do (call f a b) (-> (tuple) (block (line 8 none)))))))))
  ; test/syntax.jl:1864
  (parse-expect-underscore "f30656(T) = (t, _)::Pair -> t >= T" parse-stmts
    '(= (call f30656 T) (block (line 1 none) (-> (:: (tuple t _) Pair) (block (line 1 none) (call >= t T))))))
  ; test/misc.jl:709
  (parse-expect-underscore "function closefunc(_) end" parse-stmts
    '(function (call closefunc _) (block (line 1 none) (line 1 none))))
  #t)
)
