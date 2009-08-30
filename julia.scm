;(if (not (top-level-bound? 'open-input-string))
;    (load (string *install-dir* "/aliases.scm")))

(define (delete-duplicates lst)
  (if (null? lst)
      lst
      (let ((elt  (car lst))
	    (tail (cdr lst)))
	(if (member elt tail)
	    (delete-duplicates tail)
	    (cons elt
		  (delete-duplicates tail))))))

(define-macro (begin0 first . rest)
  (let ((g (gensym)))
    `(let ((,g ,first))
       ,@rest
       ,g)))

(define ops-by-prec
  '((= := += -= *= /= ^= %= |= &= $= => <<= >>=)
    (||)
    (&&)
    (-> <-)
    (> < >= <= == != .> .< .>= .<= .== .!= .= .!)
    (<< >>)
    (: ..)
    (+ - | $)
    (* / ./ % & .* \ .\)
    (^ .^)
    (::)))

; unused characters: @ $ ` " ?

(define unary-ops '(- + ! ~))

(define trans-op (string->symbol ".'"))
(define ctrans-op (string->symbol "'"))
(define dot-op (string->symbol "."))

(define operators (list* '~ ctrans-op trans-op dot-op
			 (delete-duplicates (apply append ops-by-prec))))

(define op-chars
  (delete-duplicates
   (apply append
	  (map string->list (map symbol->string operators)))))

; --- lexer ---

(define num-start-char?
  (let ((chrs (string->list "0123456789.")))
    (lambda (c) (memv c chrs))))
(define special-char?
  (let ((chrs (string->list "()[]{},;")))
    (lambda (c) (memv c chrs))))
(define (newline? c) (eqv? c #\newline))
(define (identifier-char? c) (or (and (char>=? c #\A)
				      (char<=? c #\Z))
				 (and (char>=? c #\a)
				      (char<=? c #\z))
				 (and (char>=? c #\0)
				      (char<=? c #\9))
				 (eqv? c #\_)))
(define (opchar? c) (memv c op-chars))
(define (operator? c) (memq c operators))

(define (skip-ws port newlines?)
  (let ((c (peek-char port)))
    (if (and (not (eof-object? c)) (char-whitespace? c)
	     (or newlines? (not (newline? c))))
	(begin (read-char port)
	       (skip-ws port newlines?)))))

(define (skip-to-eol port)
  (let ((c (read-char port)))
    (cond ((eof-object? c)    c)
	  ((eqv? c #\newline) (unread-char c port) c)
	  (else               (skip-to-eol port)))))

; pred - should we consider a character?
; valid - does adding the next character to the token produce
;         a valid token?
(define (accum-tok c pred valid port)
  (let ((str (open-output-string)))
    (let loop ((c c)
	       (first? #t))
      (if (and (not (eof-object? c)) (pred c)
	       (or first?
		   (valid (string-append (get-output-string str)
					 (string c)))))
	  (begin (write-char c str)
		 (read-char port)
		 (loop (peek-char port) #f))
	  (get-output-string str)))))

(define (yes x) #t)

(define (read-number port)
  (let ((str (open-output-string)))
    (define (allow ch)
      (let ((c (peek-char port)))
	(and (eqv? c ch)
	     (begin (write-char (read-char port) str) #t))))
    (define (read-digs)
      (let ((d (accum-tok (peek-char port) char-numeric? yes port)))
	(and (not (equal? d ""))
	     (not (eof-object? d))
	     (display d str)
	     #t)))
    (allow #\.)
    (read-digs)
    (allow #\.)
    (read-digs)
    (if (or (allow #\e) (allow #\E))
	(begin (or (allow #\+) (allow #\-))
	       (read-digs)))
    (let* ((s (get-output-string str))
	   (n (string->number s)))
      (if n n
	  (error "Invalid numeric constant " s)))))

(define (prn x) (display x) (newline) x)

(define (string->op s)
  ; . needs to be handled specially because guile does not
  ; implement symbol escaping. arrrrgh!!
  (if (string=? s ".")
      #\.
      (string->symbol s)))

(define (next-token port)
  (skip-ws port #f)
  (let ((c (peek-char port)))
    (cond ((or (eof-object? c) (newline? c) (special-char? c))
	   (read-char port))

	  ((eqv? c #\#) (skip-to-eol port) (next-token port))
	  
	  ((char-numeric? c) (read-number port))
	  
	  ((and (num-start-char? c)
		(let ((c (read-char port))
		      (nextc (peek-char port)))
		  (unread-char c port)
		  (and (char-numeric? nextc)
		       (read-number port)))))
	  
	  ((opchar? c)  (string->op
			 (accum-tok c opchar?
				    (lambda (x) (operator? (string->symbol x)))
				    port)))

	  ((identifier-char? c) (string->symbol (accum-tok c identifier-char?
							   yes port)))

	  ; TODO: strings

	  (else (error "Invalid character" (read-char port))))))

; --- parser ---

(define (make-token-stream s) (cons #f s))

(define (peek-token s)
  (let ((port     (cdr s))
	(last-tok (car s)))
    (if last-tok last-tok
	(begin (set-car! s (next-token port))
	       (car s)))))

(define (require-token s)
  (define (req-token s)
    (let ((port     (cdr s))
	  (last-tok (car s)))
      (if (and last-tok (not (eof-object? last-tok)))
	  last-tok
	  (let ((t (next-token port)))
	    (if (eof-object? t)
		(error "Premature end of input")
		(begin (set-car! s t)
		       (car s)))))))
  (let ((t (req-token s)))
    ; when an actual token is needed, skip newlines
    (if (newline? t)
	(begin (take-token s)
	       (require-token s))
	t)))

(define (take-token s)
  (begin0 (car s)
	  (set-car! s #f)))

; parse left-to-right binary operator
; produces structures like (+ (+ (+ 2 3) 4) 5)
(define (parse-LtoR s down ops)
  (let loop ((ex (down s)))
    (let ((t (peek-token s)))
      (if (not (memq t ops))
	  ex
	  (loop (list (take-token s) ex (down s)))))))

; parse right-to-left binary operator
; produces structures like (= a (= b (= c d)))
(define (parse-RtoL s down ops)
  (let ((ex (down s)))
    (let ((t (peek-token s)))
      (if (not (memq t ops))
	  ex
	  (list (take-token s) ex (parse-RtoL s down ops))))))

; parse a@b@c@... as (@ a b c ...) for some operator @
; op: the operator to look for
; head: the expression head to yield in the result, e.g. "a;b" => (block a b)
; closers: a list of tokens that will stop the process
; allow-empty: if true will ignore runs of the operator, like a@@@@b
; ow, my eyes!!
(define (parse-Nary s down op head closers allow-empty)
  (let loop ((ex
              ; in allow-empty mode skip leading runs of operator
	      (if (and allow-empty (eqv? (require-token s) op))
		  '()
		  (list (down s))))
	     (first? #t))
    (let ((t (peek-token s)))
      (if (not (eqv? t op))
	  (if (or (null? ex) (pair? (cdr ex)) (not first?))
	      ; () => (head)
	      ; (ex2 ex1) => (head ex1 ex2)
              ; (ex1) ** if operator appeared => (head ex1) (handles "(x,)")
	      (cons head (reverse ex))
	      ; (ex1) => ex1
	      (car ex))
	  (begin (take-token s)
		 ; allow input to end with the operator, as in a;b; or (x,)
		 (if (or (eof-object? (peek-token s))
			 (memv (peek-token s) closers)
			 (and allow-empty
			      (eqv? (peek-token s) op)))
		     (loop ex #f)
		     (loop (cons (down s) ex) #f)))))))

; colon is strange; 3 arguments with 2 colons yields one call:
; 1:2   => (: 1 2)
; 1:2:3 => (: 1 2 3)
; a simple state machine is up to the task.
(define (parse-range s)
  (let loop ((ex (parse-expr s))
	     (first? #t))
    (let ((t (peek-token s)))
      (cond ((not (eq? t ':))
	     ex)
	    (first?
	     (loop (list (take-token s) ex (parse-expr s)) #f))
	    (else
	     (take-token s)
	     (loop (append ex (list (parse-expr s))) #t))))))

; the principal non-terminals follow, in increasing precedence order

(define (parse-block s) (parse-Nary s parse-block-stmts #\newline 'block
				    '(end else elseif) #t))
(define (parse-block-stmts s) (parse-Nary s parse-eq #\; 'block
					  '(end else elseif #\newline) #t))
(define (parse-stmts s) (parse-Nary s parse-eq    #\; 'block '(#\newline) #t))

(define (parse-eq s)    (parse-RtoL s parse-comma (list-ref ops-by-prec 0)))
; parse-eq* is used where commas are special and not used for tuples,
; for example in an argument list
(define (parse-eq* s)   (parse-RtoL s parse-or    (list-ref ops-by-prec 0)))
(define (parse-comma s) (parse-Nary s parse-or    #\, 'tuple '( #\) ) #f))
(define (parse-or s)    (parse-LtoR s parse-and   (list-ref ops-by-prec 1)))
(define (parse-and s)   (parse-LtoR s parse-arrow (list-ref ops-by-prec 2)))
(define (parse-arrow s) (parse-RtoL s parse-ineq  (list-ref ops-by-prec 3)))
(define (parse-ineq s)  (parse-LtoR s parse-shift (list-ref ops-by-prec 4)))
(define (parse-shift s) (parse-LtoR s parse-range (list-ref ops-by-prec 5)))
;(define (parse-range s) (parse-LtoR s parse-expr  (list-ref ops-by-prec 6)))
(define (parse-expr s)  (parse-LtoR s parse-term  (list-ref ops-by-prec 7)))
(define (parse-term s)  (parse-LtoR s parse-unary (list-ref ops-by-prec 8)))

; flag an error for tokens that cannot begin an expression
(define (check-unexpected tok)
  (if (memv tok '(#\, #\) #\] #\} #\; end else elseif))
      (error "Unexpected token" tok)))

(define (parse-unary s)
  (let ((t (require-token s)))
    (check-unexpected t)
    (if (memq t unary-ops)
	(list (take-token s) (parse-unary s))
	(parse-factor s))))

; handle ^, .^, and postfix transpose operator
(define (parse-factor-h s down ops)
  (let ((ex (down s)))
    (let ((t (peek-token s)))
      (cond ((eq? t ctrans-op)
	     (take-token s)
	     (list 'ctranspose ex))
	    ((eq? t trans-op)
	     (take-token s)
	     (list 'transpose ex))
	    ((not (memq t ops))
	     ex)
	    (else
	     (list (take-token s) ex (parse-factor-h s parse-unary ops)))))))

; -2^3 is parsed as -(2^3), so call parse-call for the first argument,
; and parse-unary from then on (to handle 2^-3)
(define (parse-factor s)
  (parse-factor-h s parse-call (list-ref ops-by-prec 9)))

; parse function call, indexing, dot, and :: expressions
; also handles looking for syntactic reserved words
(define (parse-call s)
  (define (loop ex)
    (let ((t (peek-token s)))
      (case t
	((:: #\.)                (loop (list (take-token s) ex (parse-atom s))))
	((#\( )   (take-token s) (loop (list* 'call ex (parse-arglist s #\) ))))
	((#\[ )   (take-token s) (loop (list* 'ref  ex (parse-arglist s #\] ))))
	(else ex))))
  
  (let ((ex (parse-atom s)))
    (if (memq ex '(begin while if for try))
	(parse-keyword s ex)
	(loop ex))))

; parse block structures
(define (parse-keyword s word)
  (define (expect-end s)
    (let ((t (peek-token s)))
      (if (eq? t 'end)
	  (take-token s)
	  (error "Expected end"))))
  (case word
    ((begin)  (begin0 (parse-block s)
		      (expect-end s)))
    ((while)  (begin0 (list 'while (parse-or s) (parse-block s))
		      (expect-end s)))
    ((for)    (begin0 (list 'for (parse-eq* s) (parse-block s))
		      (expect-end s)))
    ((if)
     (let* ((test (parse-or s))
	    (then (parse-block s))
	    (nxt  (require-token s)))
       (take-token s)
       (case nxt
	 ((end)     (list 'if test then))
	 ((elseif)  (list 'if test then (parse-keyword s 'if)))
	 ((else)    (list 'if test then (parse-keyword s 'begin)))
	 (else (error "Improperly terminated if statement")))))
    ((try) #f ; TODO
     )
    (else (error "Unhandled keyword"))))

; handle function call argument list, or any comma-delimited list
; that's not a tuple
(define (parse-arglist s closer)
  (let loop ((lst '()))
    (let ((t (require-token s)))
      (if (equal? t closer)
	  (begin (take-token s) (reverse! lst))
	  (let ((nxt (parse-eq* s)))
	    (let ((c (peek-token s)))
	      (cond ((equal? c #\,)
		     (begin (take-token s) (loop (cons nxt lst))))
		    ((equal? c closer)     (loop (cons nxt lst)))
		    (else (error "Comma expected")))))))))

; parse [] concatenation expressions
(define (parse-vector s)
  (define (fix v) (cons 'cat (reverse v)))
  (let loop ((vec '())
	     (outer '()))
    (let ((t (require-token s)))
      (cond ((eqv? t #\])
	     (take-token s)
	     (if (pair? outer)
		 (fix (cons (fix vec) outer))
		 (fix vec)))

	    ((eqv? t #\;)
	     (take-token s)
	     (loop '() (cons (fix vec) outer)))

	    ((eqv? t #\,)
	     (take-token s)
	     (loop vec outer))

	    (else
	     (loop (cons (parse-eq* s) vec) outer))))))

; parse numbers, identifiers, parenthesized expressions, lists, vectors, etc.
(define (parse-atom s)
  (let ((t (require-token s)))
    (cond ((or (string? t) (number? t)) (take-token s))

	  ((eqv? t #\( )
	   (take-token s)
	   (if (eqv? (peek-token s) #\) )
	       (begin (take-token s) '(tuple))
	       (let* ((ex (parse-eq s))
		      (t (require-token s)))
		 (if (eqv? t #\) )
		     (begin (take-token s) ex)
		     (error "Expected )")))))

	  ((eqv? t #\{ )
	   (take-token s)
	   (cons 'list (parse-arglist s #\})))

	  ((eqv? t #\[ )
	   (take-token s)
	   (parse-vector s))

	  ; TODO: prefix keywords, various quoting/escaping

	  (else (take-token s)))))

; --- main entry point ---

(define (julia-parse s)
  (cond ((string? s)
	 (julia-parse (make-token-stream (open-input-string s))))
	((port? s)
	 (julia-parse (make-token-stream s)))
	(else
	 ; as a special case, allow early end of input if there is
	 ; nothing left but whitespace
	 (skip-ws (cdr s) #t)
	 (if (eqv? (peek-token s) #\newline) (take-token s))
	 (let ((t (peek-token s)))
	   (if (eof-object? t)
	       t
	       (parse-stmts s))))))

; --- tests ---

(define-macro (assert expr) `(if ,expr #t (error "Assertion failed:" ',expr)))
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
(tst "a::b.c" (#\. (:: a b) c))

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

; call f on a stream until the stream runs out of data
(define (read-all-of f s)
  (let loop ((lines '())
	     (curr  (f s)))
    (if (eof-object? curr)
	(reverse! lines)
	(loop (cons curr lines) (f s)))))

; for testing. generally no need to tokenize a whole stream in advance.
(define (julia-tokenize port)
  (read-all-of next-token port))

(define (julia-parse-file filename)
  (read-all-of julia-parse (make-token-stream (open-input-file filename))))
