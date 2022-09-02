(load "jlfrontend.scm")

; ((curry1 parse-atom) (make-token-stream (open-input-string "1")))
; ((curry1 parse-raw-literal #\') (make-token-stream (open-input-string "'ls -la'")))
(define (curry1 func . args)
  (lambda (s) (apply func (cons s args))))

(define-macro (parse-expect s production expected)
  `(let ((production-name ',(if (list? production) (cadr production) production)))
        (info "======>" 'test (string "'" ,s "'") production-name)
        (set! *gensy-counter* 1)
        (let ((result (julia-parse ,s ,production)))
             (if (equal? result ,expected)
                 #t
                 (error (string "parse \"" ,s "\" failed: "
                                result " != " ,expected))))))

(define left-paren #\()
(define right-paren #\))

(and
  (parse-expect "1+1" parse-stmts '(call + 1 1))
  (parse-expect "x->x,1" parse-stmts '(tuple (-> x (block (line 1 none) x)) 1))
  (parse-expect "(sqrt ∘ +)(3, 6)" parse-stmts '(call (call ∘ sqrt +) 3 6))
  (parse-expect "b, c" parse-comma '(tuple b c))
  (parse-expect "b" parse-comma 'b)

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

(define underscore-gen-block #f)

(and
  (parse-expect "((_+(1))),_" parse-stmts
    '(-> |#2#_| (tuple (-> |#1#_| (call + |#1#_| 1)) |#2#_|)))
  (parse-expect "_+_,_" parse-stmts
    '(-> |#3#_| (tuple (-> (tuple |#1#_| |#2#_|) (call + |#1#_| |#2#_|)) |#3#_|)))
  #t)
