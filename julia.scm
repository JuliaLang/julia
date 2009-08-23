(if (not (top-level-bound? 'open-input-string))
    (load "aliases.scm"))

(define (delete-duplicates lst)
  (if (atom? lst)
      lst
      (let ((elt  (car lst))
	    (tail (cdr lst)))
	(if (member elt tail)
	    (delete-duplicates tail)
	    (cons elt
		  (delete-duplicates tail))))))

(define ops-by-prec
  '((= := += -= *= /= ^= %= \|= &= $= => |:|)
    (\|\|)
    (&&)
    (-> <-)
    (> < >= <= == != .> .< .>= .<= .== .!=)
    (.. <<)
    (+ - \| $)
    (* / ./ % & .* \\ .\\)
    (^ .^)
    (::)))

(define unary-ops '(- + ! ~))

(define operators (list* '~
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
  (let ((chrs (string->list "()[]{},")))
    (lambda (c) (memv c chrs))))
(define (identifier-char? c) (or (and (char>=? c #\A)
				      (char<=? c #\Z))
				 (and (char>=? c #\a)
				      (char<=? c #\z))
				 (and (char>=? c #\0)
				      (char<=? c #\9))
				 (eqv? c #\_)))
(define (opchar? c) (memv c op-chars))
(define (operator? c) (memq c operators))

(define (skip-ws port)
  (let ((c (peek-char port)))
    (if (and (not (eof-object? c)) (char-whitespace? c))
	(begin (read-char port)
	       (skip-ws port)))))

(define (read-to-eol port)
  (let ((c (read-char port)))
    (unless (eqv? c #\newline)
	    (read-to-eol port))))

; pred - should we consider a character?
; valid - does adding the next character to the token produce
;         a valid token?
(define (accum-tok c pred valid port)
  (let ((str (open-output-string)))
    (let loop ((c c))
      (if (and (not (eof-object? c)) (pred c)
	       (valid (string-append (get-output-string str)
				     (string c))))
	  (begin (write-char c str)
		 (read-char port)
		 (loop (peek-char port)))
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

(define (prn x) (print x) (newline) x)

(define (next-token port)
  (skip-ws port)
  (let ((c (peek-char port)))
    (cond ((eof-object? c) (read-char port) c)

	  ((eqv? c #\#) (read-to-eol port) (next-token port))
	  
	  ((num-start-char? c) (read-number port))

	  ((opchar? c)  (string->symbol
			 (accum-tok c opchar?
				    (lambda (x) (operator? (string->symbol x)))
				    port)))

	  ((identifier-char? c) (string->symbol (accum-tok c identifier-char?
							   yes port)))

	  ((special-char? c) (read-char port))

	  (else (error "Invalid character" (read-char port))))))

(define (tokenize port)
  (read-all-of next-token port))

; --- parser ---

(define peek-token)
(define take-token)

(let ((last-tok #f))
  (set! peek-token
	(lambda (s) (if last-tok last-tok
			(begin (set! last-tok (next-token s))
			       last-tok))))
  (set! take-token
	(lambda () (let ((t last-tok)) (set! last-tok #f) t))))

(define (julia-parse s)
  (if (string? s)
      (julia-parse (open-input-string s))
      (begin
	(take-token)
	(parse-eq s))))

(define (parse-LtoR s down ops)
  (let loop ((ex (down s)))
    (let ((t (peek-token s)))
      (if (not (memq t ops))
	  ex
	  (loop (list (take-token) ex (down s)))))))

(define (parse-RtoL s down ops)
  (let ((ex (down s)))
    (let ((t (peek-token s)))
      (if (not (memq t ops))
	  ex
	  (list (take-token) ex (parse-RtoL s down ops))))))

(define (parse-eq s)    (parse-RtoL s parse-or    (list-ref ops-by-prec 0)))
(define (parse-or s)    (parse-LtoR s parse-and   (list-ref ops-by-prec 1)))
(define (parse-and s)   (parse-LtoR s parse-arrow (list-ref ops-by-prec 2)))
(define (parse-arrow s) (parse-RtoL s parse-ineq  (list-ref ops-by-prec 3)))
(define (parse-ineq s)  (parse-RtoL s parse-range (list-ref ops-by-prec 4)))
(define (parse-range s) (parse-LtoR s parse-expr  (list-ref ops-by-prec 5)))
(define (parse-expr s)  (parse-LtoR s parse-term  (list-ref ops-by-prec 6)))
(define (parse-term s)  (parse-LtoR s parse-unary (list-ref ops-by-prec 7)))

(define (check-unexpected tok)
  (if (memv tok '(#\, #\) #\] #\} #\;))
      (error "Unexpected token" (string tok))))

(define (parse-unary s)
  (let ((t (peek-token s)))
    (check-unexpected t)
    (if (memq t unary-ops)
	(list (take-token) (parse-unary s))
	(parse-factor s))))

(define (parse-factor-h s down ops)
  (let ((ex (down s)))
    (let ((t (peek-token s)))
      (if (not (memq t ops))
	  ex
	  (list (take-token) ex (parse-factor-h s parse-unary ops))))))

(define (parse-factor s)
  (parse-factor-h s parse-call (list-ref ops-by-prec 8)))

(define (parse-call s)
  (define (loop ex)
    (let ((t (peek-token s)))
      (case t
	((:: |.|)              (loop (list  (take-token) ex (parse-atom s))))
	((#\( )   (take-token) (loop (list* 'call ex (parse-arglist s #\) ))))
	((#\[ )   (take-token) (loop (list* 'ref  ex (parse-arglist s #\] ))))
	(else ex))))
  
  (let ((ex (parse-atom s)))
    (if (memq ex '(begin end while if for try))
	(parse-keyword s ex)
	(loop ex))))

(define (parse-keyword s word)
  (void)
  ; TODO
)

(define (parse-arglist s closer)
  (let loop ((lst ()))
    (let ((t (peek-token s)))
      (if (equal? t closer)
	  (begin (take-token) (reverse! lst))
	  (let ((nxt (parse-eq s)))
	    (let ((c (peek-token s)))
	      (cond ((equal? c #\,)
		     (begin (take-token) (loop (cons nxt lst))))
		    ((equal? c closer)   (loop (cons nxt lst)))
		    (else (error "Comma expected")))))))))

(define (parse-atom s)
  (let ((t (peek-token s)))
    (cond ((or (string? t) (number? t)) (take-token))

	  ((eqv? t #\( )
	   (take-token)
	   (let ((ex (parse-arglist s #\) )))
	     (if (or (null? ex)
		     (pair? (cdr ex)))
		 (cons 'tuple ex)
		 (car ex))))

	  ; TODO: vector syntax, etc.

	  (else (take-token)))))
