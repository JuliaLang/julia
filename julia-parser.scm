#|
TODO:
- parsing try/catch
* parsing typealias
* semicolons in argument lists, for keywords
|#

(define ops-by-prec
  '((= := += -= *= /= ^= %= |\|=| &= $= => <<= >>=)
    (|\|\||)
    (&&)
    (-> <-)
    (> < >= <= == != |.>| |.<| |.>=| |.<=| |.==| |.!=| |.=| |.!|)
    (<< >>)
    (: ..)
    (+ - |\|| $)
    (* / |./| % & |.*| |\\| |.\\|)
    (^ |.^|)
    (|::| |.|)))

; unused characters: @ ` ? prefix'
; possible: use ` for quoting and escaping, $ for unquote
;           leave @ as an operator
; no character literals; unicode kind of makes them obsolete. strings instead.
; decide what to do about true, false, and null

(define unary-ops '(- + ! ~))

; operators that are special forms, not function names
(define syntactic-operators
  '(= := += -= *= /= ^= %= |\|=| &= $= => <<= >>=
      -> |\|\|| && : |::| |.|))

(define (syntactic-op? op) (memq op syntactic-operators))

(define trans-op (string->symbol ".'"))
(define ctrans-op (string->symbol "'"))
(define vararg-op (string->symbol "..."))

(define operators (list* '~ ctrans-op trans-op vararg-op
			 (delete-duplicates (apply append ops-by-prec))))

(define op-chars
  (delete-duplicates
   (apply append
	  (map string->list (map symbol->string operators)))))

; --- lexer ---

(define special-char?
  (let ((chrs (string->list "()[]{},;`")))
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
  (let ((c (peek-char port)))
    (cond ((eof-object? c)    c)
	  ((eqv? c #\newline) c)
	  (else               (read-char port)
			      (skip-to-eol port)))))

; pred - should we consider a character?
; valid - does adding the next character to the token produce
;         a valid token?
(define (accum-tok c pred valid port)
  (let loop ((str '())
	     (c c)
	     (first? #t))
    (if (and (not (eof-object? c)) (pred c)
	     (or first?
		 (valid (string-append (list->string (reverse str))
				       (string c)))))
	(begin (read-char port)
	       (loop (cons c str) (peek-char port) #f))
	(list->string (reverse str)))))

(define (yes x) #t)

(define (read-number port . leadingdot)
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
    (if (pair? leadingdot)
	(write-char #\. str)
	(allow #\.))
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

(define (read-operator port c)
  (string->symbol
   (accum-tok c opchar?
	      (lambda (x) (operator? (string->symbol x)))
	      port)))

(define (next-token port)
  (skip-ws port #f)
  (let ((c (peek-char port)))
    (cond ((or (eof-object? c) (newline? c) (special-char? c))
	   (read-char port))

	  ((eqv? c #\#) (skip-to-eol port) (next-token port))
	  
	  ((char-numeric? c) (read-number port))
	  
	  ; . is difficult to handle; it could start a number or operator
	  ((and (eqv? c #\.)
		(let ((c (read-char port))
		      (nextc (peek-char port)))
		  (cond ((char-numeric? nextc)
			 (read-number port c))
			((opchar? nextc)
			 (string->symbol
			  (string-append (string c)
					 (symbol->string
					  (read-operator port nextc)))))
			(else '|.|)))))
	  
	  ((opchar? c)  (read-operator port c))

	  ((identifier-char? c) (string->symbol (accum-tok c identifier-char?
							   yes port)))

	  ((eqv? c #\")  (read port))

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
	  (begin (take-token s)
		 (if (syntactic-op? t)
		     (loop (list t ex (down s)))
		     (loop (list 'call t ex (down s)))))))))

; parse right-to-left binary operator
; produces structures like (= a (= b (= c d)))
(define (parse-RtoL s down ops)
  (let ((ex (down s)))
    (let ((t (peek-token s)))
      (if (not (memq t ops))
	  ex
	  (begin (take-token s)
		 (if (syntactic-op? t)
		     (list t ex (parse-RtoL s down ops))
		     (list 'call t ex (parse-RtoL s down ops))))))))

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
	      ; (ex1) ** if operator appeared => (head ex1) (handles "x;")
	      (cons head (reverse ex))
	      ; (ex1) => ex1
	      (car ex))
	  (begin (take-token s)
		 ; allow input to end with the operator, as in a;b;
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
; we will leave : expressions as a syntax form, not a call to ':',
; so they can be processed into either ranges or declarations
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
; parse-eq* is used where commas are special, for example in an argument list
(define (parse-eq* s)   (parse-RtoL s parse-or    (list-ref ops-by-prec 0)))
; parse-comma is needed for commas outside parens, for example a = b,c
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
	(list 'call (take-token s) (parse-unary s))
	(parse-factor s))))

; handle ^, .^, and postfix transpose operator
(define (parse-factor-h s down ops)
  (let ((ex (down s)))
    (let ((t (peek-token s)))
      (cond ((eq? t ctrans-op)
	     (take-token s)
	     (list 'call 'ctranspose ex))
	    ((eq? t trans-op)
	     (take-token s)
	     (list 'call 'transpose ex))
	    ((not (memq t ops))
	     ex)
	    (else
	     (list 'call
		   (take-token s) ex (parse-factor-h s parse-unary ops)))))))

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
	((|::| |.|)
	 (loop (list (take-token s) ex (parse-atom s))))
	((#\( )   (take-token s)
	 (if (memq ex '(block quote))
	     ; some names are syntactic and not function calls
	     (loop (list* ex       (parse-arglist s #\) )))
	     (loop (list* 'call ex (parse-arglist s #\) )))))
	((#\[ )   (take-token s)
	 ; ref is syntax, so we can distinguish
	 ; a[i] = x  from
	 ; ref(a,i) = x  which is invalid
	 (loop (list* 'ref  ex (parse-arglist s #\] ))))
	(else ex))))
  
  (let* ((do-kw? (not (eqv? (peek-token s) #\`)))
	 (ex (parse-atom s)))
    (if (and do-kw?
	     (memq ex '(begin while if for try function type typealias local)))
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
    ((local)  (list 'local (parse-eq s)))
    ((function)
     (let ((sig (parse-call s)))
       (begin0 (list word sig (parse-block s))
	       (expect-end s))))
    ((type)
     (let ((sig (parse-ineq s)))
       (begin0 (list word sig (parse-block s))
	       (expect-end s))))
    ((typealias)
     (list 'typealias (parse-call s) (parse-call s)))
    ((try) #f ; TODO
     )
    (else (error "Unhandled keyword"))))

; handle function call argument list, or any comma-delimited list.
; . an extra comma at the end is allowed
; . expressions after a ; are enclosed in (parameters ...)
; . an expression followed by ... becomes (... x)
(define (parse-arglist s closer)
  (let loop ((lst '()))
    (let ((t (require-token s)))
      (if (equal? t closer)
	  (begin (take-token s)
		 (reverse lst))
	  (if (equal? t #\;)
	      (begin (take-token s)
		     (if (equal? (peek-token s) closer)
			 ; allow f(a, b; )
			 (begin (take-token s)
				(reverse lst))
			 (reverse (cons (cons 'parameters (loop '()))
					lst))))
	      (let* ((nxt (parse-eq* s))
		     (c (peek-token s))
		     (nxt (if (eq? c '...)
			      (list '... nxt)
			      nxt))
		     (c (if (eq? c '...)
			    (begin (take-token s)
				   (peek-token s))
			    c)))
		(cond ((equal? c #\,)
		       (begin (take-token s) (loop (cons nxt lst))))
		      ((equal? c #\;)        (loop (cons nxt lst)))
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
	       ; here we parse the first subexpression separately, so
	       ; we can look for a comma to see if it's a tuple. if we
	       ; just called parse-arglist instead, we couldn't distinguish
	       ; (x) from (x,)
	       (let* ((ex (parse-eq* s))
		      (t (require-token s)))
		 (cond ((eqv? t #\) )
			(begin (take-token s) ex))
		       ((eqv? t #\, )
			(begin (take-token s)
			       (list* 'tuple ex (parse-arglist s #\) ))))
		       ((eq? t '...)
			(begin (take-token s)
			       (if (eqv? (peek-token s) #\,)
				   (take-token s))
			       (list* 'tuple (list '... ex)
				      (parse-arglist s #\) ))))
		       (else
			(error "Expected )"))))))

	  ((eqv? t #\{ )
	   (take-token s)
	   (cons 'list (parse-arglist s #\})))

	  ((eqv? t #\[ )
	   (take-token s)
	   (parse-vector s))

	  ((eqv? t #\` )
	   (take-token s)
	   (let ((op (peek-token s)))
	     (take-token s)
	     (if (eqv? op #\`)
		 (error "Expected token in ``")
		 (if (not (eqv? (peek-token s) #\`))
		     (error "Expected closing `")))
	     (take-token s)
	     op))

	  ((eq? t 'return)
	   (take-token s)
	   (list 'return (parse-eq s)))

	  ((or (eq? t 'break) (eq? t 'continue))
	   (list (take-token s)))

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

; call f on a stream until the stream runs out of data
(define (read-all-of f s)
  (let loop ((lines '())
	     (curr  (f s)))
    (if (eof-object? curr)
	(reverse lines)
	(loop (cons curr lines) (f s)))))

; for testing. generally no need to tokenize a whole stream in advance.
(define (julia-tokenize port)
  (read-all-of next-token port))

(define (julia-parse-file filename)
  (read-all-of julia-parse (make-token-stream (open-input-file filename))))
