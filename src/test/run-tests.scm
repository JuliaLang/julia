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

(define (unpack-block ex) ; might has line-node
  (let* ((args (cadr ex))
          (args (if (is-car? args 'tuple) (cdr args) (list args)))
          (args (map get-decl-name args))
          (body (caddr ex)))
        (if (and (is-car? body 'block))
            (list args (cadr body) (caddr body))
            (list args #f body))))
(define (underscore-equal? result expected (result-args '()) (expected-args '()))
  (and (eqv? (length result-args) (length expected-args))
  (let loop ((result result) (expected expected))
    (cond ((eqv? result expected) #t)
          ((and (memq result result-args) (memq expected expected-args))
           (eqv? (length (member result result-args)) (length (member expected expected-args))))
          ((and (is-underscore-block? result) (is-underscore-block? expected))
           (let ((a (unpack-block result)) (b (unpack-block expected)))
                (underscore-equal? (caddr a) (caddr b) (car a) (car b))))
          ((not (and (pair? result) (pair? expected))) (equal? result expected))
          (else (and (loop (car result) (car expected)) (loop (cdr result) (cdr expected))))))))
(define-macro (parse-expect s production expected)
  `(let ((production-name ',(if (list? production) (cadr production) production)))
        (info "======>" 'test (string "'" ,s "'") production-name)
        (set! *gensy-counter* 1)
        (let ((result (julia-parse ,s ,production)))
             (if (underscore-equal? result ,expected)
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
        (parse-expect ,s parse-decls ,expected2)
        (parse-expect ,(string "function " s " end") parse-stmts '(function ,,expected2 (block (line 1 none) (line 1 none))))
        (parse-expect ,(string s " = 1") parse-stmts '(= ,,expected2 (block (line 1 none) 1)))))

(define left-paren #\()
(define right-paren #\))

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

(load "test/underscore-github.scm")
(load "test/underscore-test.scm")

(info "all pass" #t)
