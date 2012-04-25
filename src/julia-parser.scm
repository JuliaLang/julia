(define ops-by-prec
  '#((= := += -= *= /= //= .//= .*= ./= |\\=| |.\\=| ^= .^= %= |\|=| &= $= => <<= >>= >>>= ~)
     (?)
     (|\|\||)
     (&&)
     ; note: there are some strange-looking things in here because
     ; the way the lexer works, every prefix of an operator must also
     ; be an operator.
     (<- -- -->)
     (> < >= <= == != |.>| |.<| |.>=| |.<=| |.==| |.!=| |.=| |.!| |<:| |>:|)
     (: ..)
     (+ - |\|| $)
     (<< >> >>>)
     (* / |./| % & |.*| |\\| |.\\|)
     (// .//)
     (^ |.^|)
     (|::|)
     (|.|)))

(define-macro (prec-ops n) `(aref ops-by-prec ,n))

(define normal-ops (vector.map identity ops-by-prec))
(define no-pipe-ops (vector.map identity ops-by-prec))
(vector-set! no-pipe-ops 7 '(+ - $))
(define range-colon-enabled #t)
; in space-sensitive mode "x -y" is 2 expressions, not a subtraction
(define space-sensitive #f)
; treat 'end' like a normal symbol instead of a reserved word
(define end-symbol #f)
(define current-filename 'none)

(define-macro (with-normal-ops . body)
  `(with-bindings ((ops-by-prec normal-ops)
		   (range-colon-enabled #t)
		   (space-sensitive #f))
		  ,@body))

(define-macro (without-bitor . body)
  `(with-bindings ((ops-by-prec no-pipe-ops))
		  ,@body))

(define-macro (without-range-colon . body)
  `(with-bindings ((range-colon-enabled #f))
		  ,@body))

(define-macro (with-space-sensitive . body)
  `(with-bindings ((space-sensitive #t))
		  ,@body))

(define-macro (with-end-symbol . body)
  `(with-bindings ((end-symbol #t))
		  ,@body))

(define assignment-ops (prec-ops 0))

(define (assignment? e)
  (and (pair? e) (eq? (car e) '=)))

(define unary-ops '(+ - ! ~ $ & |<:| |>:|))

; operators that are both unary and binary
(define unary-and-binary-ops '(+ - $ & ~))

; operators that are special forms, not function names
(define syntactic-operators
  '(= := += -= *= /= //= .//= .*= ./= |\\=| |.\\=| ^= .^= %= |\|=| &= $= =>
      <<= >>= >>>= -> --> |\|\|| && : |::| |.|))
(define syntactic-unary-operators '($ &))

(define reserved-words '(begin while if for try return break continue
			 function macro quote let local global const
			 abstract typealias type bitstype
			 module import export ccall))

(define (syntactic-op? op) (memq op syntactic-operators))
(define (syntactic-unary-op? op) (memq op syntactic-unary-operators))

(define trans-op (string->symbol ".'"))
(define ctrans-op (string->symbol "'"))
(define vararg-op (string->symbol "..."))

(define operators (list* '~ '! '-> ctrans-op trans-op vararg-op
			 (delete-duplicates
			  (apply append (vector->list ops-by-prec)))))

(define op-chars
  (list->string
   (delete-duplicates
    (apply append
	   (map string->list (map symbol->string operators))))))

; --- lexer ---

(define special-char?
  (let ((chrs (string->list "()[]{},;\"`@")))
    (lambda (c) (memv c chrs))))
(define (newline? c) (eqv? c #\newline))
(define (identifier-char? c) (or (and (char>=? c #\A)
				      (char<=? c #\Z))
				 (and (char>=? c #\a)
				      (char<=? c #\z))
				 (and (char>=? c #\0)
				      (char<=? c #\9))
				 (char>=? c #\uA1)
				 (eqv? c #\_)))
;; characters that can be in an operator
(define (opchar? c) (string.find op-chars c))
;; characters that can follow . in an operator
(define (dot-opchar? c) (and (char? c) (string.find "*^/\\" c)))
(define (operator? c) (memq c operators))

(define (skip-to-eol port)
  (let ((c (peek-char port)))
    (cond ((eof-object? c)    c)
	  ((eqv? c #\newline) c)
	  (else               (read-char port)
			      (skip-to-eol port)))))

(define (read-operator port c)
  (read-char port)
  (if (and (eqv? c #\*) (eqv? (peek-char port) #\*))
      (error "use ^ instead of **"))
  (if (or (eof-object? (peek-char port)) (not (opchar? (peek-char port))))
      (symbol (string c)) ; 1-char operator
      (let loop ((str (string c))
		 (c   (peek-char port)))
	(if (and (not (eof-object? c)) (opchar? c))
	    (let ((newop (string str c)))
	      (if (operator? (string->symbol newop))
		  (begin (read-char port)
			 (loop newop (peek-char port)))
		  (string->symbol str)))
	    (string->symbol str)))))

(define (accum-tok-eager c pred port)
  (let loop ((str '())
	     (c c))
    (if (and (not (eof-object? c)) (pred c))
	(begin (read-char port)
	       (loop (cons c str) (peek-char port)))
	(list->string (reverse str)))))

(define (char-hex? c)
  (or (char-numeric? c)
      (and (>= c #\a) (<= c #\f))
      (and (>= c #\A) (<= c #\F))))

(define (read-number port . leadingdot)
  (let ((str  (open-output-string))
	(pred char-numeric?))
    (define (allow ch)
      (let ((c (peek-char port)))
	(and (eqv? c ch)
	     (begin (write-char (read-char port) str) #t))))
    (define (disallow-dot)
      (if (eqv? (peek-char port) #\.)
	  (begin (read-char port)
		 (if (dot-opchar? (peek-char port))
		     (io.ungetc port #\.)
		     (error (string "invalid numeric constant "
				    (get-output-string str) #\.))))))
    (define (read-digs)
      (let ((d (accum-tok-eager (peek-char port) pred port)))
	(and (not (equal? d ""))
	     (not (eof-object? d))
	     (display d str)
	     #t)))
    (if (pair? leadingdot)
	(write-char #\. str)
	(if (eqv? (peek-char port) #\0)
	    (begin (write-char (read-char port) str)
		   (if (allow #\x)
		       (set! pred char-hex?)))
	    (allow #\.)))
    (read-digs)
    (if (eqv? (peek-char port) #\.)
	(begin (read-char port)
	       (if (dot-opchar? (peek-char port))
		   (io.ungetc port #\.)
		   (begin (write-char #\. str)
			  (read-digs)
			  (disallow-dot)))))
    (let ((c (peek-char port)))
      (if (or (eqv? c #\e) (eqv? c #\E))
	  (begin (read-char port)
		 (let ((d (peek-char port)))
		   (if (and (not (eof-object? d))
			    (or (char-numeric? d) (eqv? d #\+) (eqv? d #\-)))
		       (begin (write-char c str)
			      (write-char (read-char port) str)
			      (read-digs)
			      (disallow-dot))
		       (io.ungetc port c))))))
    (let* ((s (get-output-string str))
	   (n (string->number s (if (eq? pred char-hex?) 16 10))))
      (if n
	  (if (eq? pred char-hex?)
	      (sized-uint-literal n s)
	      (if (and (integer? n) (> n 9223372036854775807))
	          (error (string "invalid numeric constant " s))
	          n))
	  (error (string "invalid numeric constant " s))))))

(define (sized-uint-literal n s)
  (let ((l (length s)))
    (cond ((< l 5)  (uint8  n))
	  ((< l 7)  (uint16 n))
	  ((< l 11) (uint32 n))
	  (else     (uint64 n)))))

(define (skip-ws-and-comments port)
  (skip-ws port #t)
  (if (eqv? (peek-char port) #\#)
      (begin (skip-to-eol port)
	     (skip-ws-and-comments port)))
  #t)

(define (next-token port s)
  (aset! s 2 (eq? (skip-ws port #f) #t))
  (let ((c (peek-char port)))
    (cond ((or (eof-object? c) (newline? c))  (read-char port))

	  ((special-char? c)    (read-char port))

	  ((char-numeric? c)    (read-number port))
	  
	  ((eqv? c #\#)         (skip-to-eol port) (next-token port s))
	  
	  ; . is difficult to handle; it could start a number or operator
	  ((and (eqv? c #\.)
		(let ((c (read-char port))
		      (nextc (peek-char port)))
		  (cond ((eof-object? nextc)
			 '|.|)
			((char-numeric? nextc)
			 (read-number port c))
			((opchar? nextc)
			 (string->symbol
			  (string-append (string c)
					 (symbol->string
					  (read-operator port nextc)))))
			(else '|.|)))))
	  
	  ((opchar? c)  (read-operator port c))

	  ((identifier-char? c) (accum-julia-symbol c port))

	  #;((eqv? c #\")
	   (with-exception-catcher
	    (lambda (e)
	      (error "invalid string literal"))
	    (lambda () (read port))))

	  (else (error (string "invalid character " (read-char port)))))))

; --- parser ---

(define (make-token-stream s) (vector #f s #t #f))
(define-macro (ts:port s)       `(aref ,s 1))
(define-macro (ts:last-tok s)   `(aref ,s 0))
(define-macro (ts:set-tok! s t) `(aset! ,s 0 ,t))
(define-macro (ts:space? s)     `(aref ,s 2))
(define-macro (ts:pbtok s)      `(aref ,s 3))
(define (ts:put-back! s t)
  (if (ts:pbtok s)
      (error "too many pushed-back tokens (internal error)")
      (aset! s 3 t)))

(define (peek-token s)
  (or (ts:pbtok s)
      (ts:last-tok s)
      (begin (ts:set-tok! s (next-token (ts:port s) s))
	     (ts:last-tok s))))

(define (require-token s)
  (let ((t (or (ts:pbtok s) (ts:last-tok s) (next-token (ts:port s) s))))
    (if (eof-object? t)
	(error "incomplete: premature end of input")
	(if (newline? t)
	    (begin (take-token s)
		   (require-token s))
	    (begin (if (not (ts:pbtok s)) (ts:set-tok! s t))
		   t)))))

(define (take-token s)
  (or
   (begin0 (ts:pbtok s)
	   (aset! s 3 #f))
   (begin0 (ts:last-tok s)
	   (ts:set-tok! s #f))))

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

(define (parse-cond s)
  (let ((ex (parse-or s)))
    (if (not (eq? (peek-token s) '?))
	ex
	(begin (take-token s)
	       (let ((then (without-range-colon (parse-eq* s))))
		 (if (not (eq? (take-token s) ':))
		     (error "colon expected in ? expression")
		     (list 'if ex then (parse-cond s))))))))

(define (invalid-initial-token? tok)
  (or (eof-object? tok)
      (memv tok '(#\) #\] #\} else elseif catch))))

(define (line-number-node s)
  `(line ,(input-port-line (ts:port s))))

(define (line-number-filename-node s)
  `(line ,(input-port-line (ts:port s)) ,current-filename))

;; insert line/file for short-form function defs, otherwise leave alone
(define (short-form-function-loc ex lno)
  (if (and (pair? ex)
	   (eq? (car ex) '=)
	   (pair? (cadr ex))
	   (eq? (caadr ex) 'call))
      `(= ,(cadr ex) (block (line ,lno ,current-filename) ,(caddr ex)))
      ex))

; parse a@b@c@... as (@ a b c ...) for some operator @
; op: the operator to look for
; head: the expression head to yield in the result, e.g. "a;b" => (block a b)
; closers: a list of tokens that will stop the process
;          however, this doesn't consume the closing token, just looks at it
; allow-empty: if true will ignore runs of the operator, like a@@@@b
; ow, my eyes!!
(define (parse-Nary s down op head closers allow-empty)
  (if (invalid-initial-token? (require-token s))
      (error (string "unexpected " (peek-token s))))
  (if (memv (require-token s) closers)
      (list head)  ; empty block
      (let loop ((ex
                  ;; in allow-empty mode skip leading runs of operator
		  (if (and allow-empty (eqv? (require-token s) op))
		      '()
		      (if (eqv? op #\newline)
			  (let ((loc (line-number-node s)))
			    ;; note: line-number must happen before (down s)
			    (list (down s) loc))
			  (list (down s)))))
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
			 (if (eqv? op #\newline)
			     (let ((loc (line-number-node s)))
			       (loop (list* (down s) loc ex) #f))
			     (loop (cons (down s) ex) #f)))))))))

; colon is strange; 3 arguments with 2 colons yields one call:
; 1:2   => (: 1 2)
; 1:2:3 => (: 1 2 3)
; 1:    => (: 1 :)
; 1:2:  => (: 1 2 :)
;; not enabled:
;;; :2    => (: 2)
;;; :1:2  => (: (: 1 2))
;;; :1:   => (: (: 1 :))
; a simple state machine is up to the task.
; we will leave : expressions as a syntax form, not a call to ':',
; so they can be processed by syntax passes.
(define (parse-range s)
  (if (not range-colon-enabled)
      (return (parse-expr s)))
  (let loop ((ex (parse-expr s))
	     (first? #t))
    (let* ((t   (peek-token s))
	   (spc (ts:space? s)))
      (if (not (eq? t ':))
	  ex
	  (begin (take-token s)
		 (if (and space-sensitive spc
			  (or (peek-token s) #t) (not (ts:space? s)))
		     ;; "a :b" in space sensitive mode
		     (begin (ts:put-back! s ':)
			    ex)
		     (let ((argument
			    (if (closing-token? (peek-token s))
				':  ; missing last argument
				(parse-expr s))))
		       (if first?
			   (loop (list t ex argument) #f)
			   (loop (append ex (list argument)) #t)))))))))

; the principal non-terminals follow, in increasing precedence order

(define (parse-block s) (parse-Nary s parse-block-stmts #\newline 'block
				    '(end else elseif catch) #t))
(define (parse-block-stmts s) (parse-Nary s parse-eq #\; 'block
					  '(end else elseif catch #\newline)
					  #t))
(define (parse-stmts s) (parse-Nary s parse-eq    #\; 'block '(#\newline) #t))

(define (parse-eq s)
  (let ((lno (input-port-line (ts:port s))))
    (short-form-function-loc
     (parse-RtoL s parse-comma (prec-ops 0)) lno)))
; parse-eq* is used where commas are special, for example in an argument list
(define (parse-eq* s)   (parse-RtoL s parse-cond  (prec-ops 0)))
; parse-comma is needed for commas outside parens, for example a = b,c
(define (parse-comma s) (parse-Nary s parse-cond  #\, 'tuple '( #\) ) #f))
(define (parse-or s)    (parse-LtoR s parse-and   (prec-ops 2)))
(define (parse-and s)   (parse-LtoR s parse-arrow (prec-ops 3)))
(define (parse-arrow s) (parse-RtoL s parse-ineq  (prec-ops 4)))
(define (parse-ineq s)  (parse-comparison s (prec-ops 5)))

; parse left to right, combining chains of certain operators into 1 call
; e.g. a+b+c => (call + a b c)
(define (parse-expr s)
  (let ((ops (prec-ops 7)))
    (let loop ((ex       (parse-shift s))
	       (chain-op #f))
      (let* ((t   (peek-token s))
	     (spc (ts:space? s)))
	(if (not (memq t ops))
	    ex
	    (begin
	      (take-token s)
	      (cond ((and space-sensitive spc (memq t unary-and-binary-ops)
			  (or (peek-token s) #t) (not (ts:space? s)))
		     ;; here we have "x -y"
		     (ts:put-back! s t)
		     ex)
		    ((eq? t chain-op)
		     (loop (append ex (list (parse-shift s)))
			   chain-op))
		    (else
		     (loop (list 'call t ex (parse-shift s))
			   (and (eq? t '+) t))))))))))

(define (parse-shift s) (parse-LtoR s parse-term (prec-ops 8)))

; given an expression and the next token, is there a juxtaposition
; operator between them?
(define (juxtapose? expr t)
  (and (not (operator? t))
       (not (operator? expr))
       (not (memq t reserved-words))
       (not (closing-token? t))
       (not (newline? t))
       (or (number? expr)
	   (not (memv t '(#\( #\[ #\{))))))

(define (parse-term s)
  (let ((ops (prec-ops 9)))
    (let loop ((ex       (parse-rational s))
	       (chain-op #f))
      (let ((t (peek-token s)))
	(cond ((and (juxtapose? ex t)
		    (not (ts:space? s)))
	       (if (eq? chain-op '*)
		   (loop (append ex (list (parse-rational s)))
			 chain-op)
		   (loop (list 'call '* ex (parse-rational s))
			 '*)))
	      ((not (memq t ops))
	       ex)
	      ((eq? t chain-op)
	       (begin (take-token s)
		      (loop (append ex (list (parse-rational s)))
			    chain-op)))
	      (else
	       (begin (take-token s)
		      (loop (list 'call t ex (parse-rational s))
			    (and (eq? t '*) t)))))))))

(define (parse-comparison s ops)
  (let loop ((ex (parse-range s))
	     (first #t))
    (let ((t (peek-token s)))
      (if (not (memq t ops))
	  ex
	  (begin (take-token s)
		 (if first
		     (loop (list 'comparison ex t (parse-range s)) #f)
		     (loop (append ex (list t (parse-range s))) #f)))))))

; flag an error for tokens that cannot begin an expression
(define (closing-token? tok)
  (or (eof-object? tok)
      (and (eq? tok 'end) (not end-symbol))
      (memv tok '(#\, #\) #\] #\} #\; else elseif catch))))

(define (parse-unary s)
  (let ((t (require-token s)))
    (if (closing-token? t)
	(error (string "unexpected " t)))
    (cond ((memq t unary-ops)
	   (let ((op (take-token s))
		 (next (peek-token s)))
	     (cond ((closing-token? next)
		    op)  ; return operator by itself, as in (+)
		   ((syntactic-unary-op? op)
		    (list op (parse-unary s)))
		   ((eqv? next #\{)  ;; this case is +{T}(x::T) = ...
		    (ts:put-back! s op)
		    (parse-factor s))
		   (else
		    (let ((arg (parse-unary s)))
		      (if (and (pair? arg)
			       (eq? (car arg) 'tuple))
			  (list* 'call op (cdr arg))
			  (list  'call op arg)))))))
	  ((eq? t '|::|)
	   ;; allow ::T, omitting argument name
	   (take-token s)
	   `(|::| ,(parse-call s)))
	  (else
	   (parse-factor s)))))

; handle ^, .^, and postfix ...
(define (parse-factor-h s down ops)
  (let ((ex (down s)))
    (let ((t (peek-token s)))
      (cond ((eq? t '...)
	     (take-token s)
	     (list '... ex))
	    ((not (memq t ops))
	     ex)
	    (else
	     (list 'call
		   (take-token s) ex (parse-factor-h s parse-unary ops)))))))

(define (parse-rational s) (parse-LtoR s parse-unary (prec-ops 10)))

; -2^3 is parsed as -(2^3), so call parse-decl for the first argument,
; and parse-unary from then on (to handle 2^-3)
(define (parse-factor s)
  (parse-factor-h s parse-decl (prec-ops 11)))

(define (parse-decl s) (parse-LtoR s parse-call (prec-ops 12)))

; parse function call, indexing, dot, and transpose expressions
; also handles looking for syntactic reserved words
(define (parse-call s)
  (let ((ex (parse-atom s)))
    (if (memq ex reserved-words)
	(parse-resword s ex)
	(let loop ((ex ex))
	  (let ((t (peek-token s)))
	    (if (or (and space-sensitive (ts:space? s)
			 (memv t '(#\( #\[ #\{ |'| #\")))
		    (and (number? ex)  ;; 2(...) is multiply, not call
			 (eqv? t #\()))
		ex
		(case t
		  ((#\( )   (take-token s)
		   (loop (list* 'call ex (parse-arglist s #\) ))))
		  ((#\[ )   (take-token s)
	           ; ref is syntax, so we can distinguish
	           ; a[i] = x  from
	           ; ref(a,i) = x
		   (loop (list* 'ref ex
				(with-end-symbol
				 (parse-arglist s #\] )))))
		  ((|.|)
		   (take-token s)
		   (if (eqv? (peek-token s) #\()
		       (loop `(|.| ,ex ,(parse-atom s)))
		       (loop `(|.| ,ex (quote ,(parse-atom s))))))
		  ((|.'| |'|) (take-token s)
		   (loop (list t ex)))
		  ((#\{ )   (take-token s)
		   (loop (list* 'curly ex (parse-arglist s #\} ))))
		  ((#\")
		   (if (and (symbol? ex) (not (operator? ex))
			    (not (ts:space? s)))
		       ;; custom prefixed string literals, x"s" => @x_str "s"
		       (let ((str (begin (take-token s)
					 (parse-string-literal s)))
			     (macname (symbol (string ex '_str))))
			 (if (and (symbol? (peek-token s)) (not (ts:space? s)))
			     ;; string literal suffix, "s"x
			     (loop `(macrocall ,macname ,(car str)
					       ,(string (take-token s))))
			     (loop `(macrocall ,macname ,(car str)))))
		       ex))
		  ((->)  (take-token s)
		   ;; -> is unusual: it binds tightly on the left and
		   ;; loosely on the right.
		   (list '-> ex (parse-eq* s)))
		  (else ex))))))))

;(define (parse-dot s)  (parse-LtoR s parse-atom (prec-ops 13)))

; parse expressions or blocks introduced by syntactic reserved words
(define (parse-resword s word)
  (define current-line (input-port-line (ts:port s)))
  (define (expect-end s)
    (let ((t (peek-token s)))
      (if (eq? t 'end)
	  (take-token s)
	  (error (string "incomplete: " word " at "
			 current-filename ":" current-line
			 " requires end")))))
  (with-normal-ops
  (case word
    ((begin)  (begin0 (parse-block s)
		      (expect-end s)))
    ((quote)  (begin0 (list 'quote (parse-block s))
		      (expect-end s)))
    ((while)  (begin0 (list 'while (parse-cond s) (parse-block s))
		      (expect-end s)))
    ((for)
     (let* ((ranges (parse-comma-separated-iters s))
	    (body   (parse-block s)))
       (expect-end s)
       (let nest ((r ranges))
	 (if (null? r)
	     body
	     `(for ,(car r) ,(nest (cdr r)))))))
    ((if)
     (let* ((test (parse-cond s))
	    (then (if (memq (require-token s) '(else elseif))
		      '(block)
		      (parse-block s)))
	    (nxt  (require-token s)))
       (take-token s)
       (case nxt
	 ((end)     (list 'if test then))
	 ((elseif)  (list 'if test then (parse-resword s 'if)))
	 ((else)    (list 'if test then (parse-resword s 'begin)))
	 (else      (error (string "unexpected " nxt))))))
    ((let)
     (let* ((binds (if (eqv? (peek-token s) #\newline)
		       (begin (take-token s)
			      '())
		       (parse-comma-separated-assignments s)))
	    (ex    (parse-block s)))
       (expect-end s)
       `(let ,ex ,@binds)))
    ((global local)
     (let* ((const (and (eq? (peek-token s) 'const)
			(take-token s)))
	    (expr  (cons word (parse-comma-separated-assignments s))))
       (if const
	   `(const ,expr)
	   expr)))
    ((function)
     (let* ((paren (eqv? (require-token s) #\())
	    (sig   (parse-call s))
	    (def   (if (symbol? sig)
		       (if paren
			   ;; in "function (x)" the (x) is a tuple
			   `(tuple ,sig)
			   ;; function foo  =>  syntax error
			   (error "expected ( in function definition"))
		       (if (not (and (pair? sig)
				     (or (eq? (car sig) 'call)
					 (eq? (car sig) 'tuple))))
			   (error "expected ( in function definition")
			   sig)))
	    (loc   (begin (skip-ws-and-comments (ts:port s))
			  (line-number-filename-node s)))
	    (body  (parse-block s)))
       (expect-end s)
       (if (and (length> body 1)
		(pair? (cadr body))
		(eq? (caadr body) 'line))
	   (set-car! (cdr body) loc))
       (list word def body)))
    ((macro)
     (let ((sig (parse-call s)))
       (begin0 (list word sig (parse-block s))
	       (expect-end s))))
    ((abstract)
     (list 'abstract (parse-ineq s)))
    ((type)
     (let ((sig (parse-ineq s)))
       (begin0 (list word sig (parse-block s))
	       (expect-end s))))
    ((bitstype)
     (list 'bitstype (parse-atom s) (parse-ineq s)))
    ((typealias)
     (let ((lhs (parse-call s)))
       (if (and (pair? lhs) (eq? (car lhs) 'call))
	   ;; typealias X (...) is tuple type alias, not call
	   (list 'typealias (cadr lhs) (cons 'tuple (cddr lhs)))
	   (list 'typealias lhs (parse-arrow s)))))
    ((try)
     (let* ((try-block (if (eq? (require-token s) 'catch)
			   '(block)
			   (parse-block s)))
	    (nxt       (require-token s)))
       (take-token s)
       (case nxt
	 ((end)   (list 'try try-block #f '(block)))
	 ((catch) (let* ((var
			  (if (eqv? (peek-token s) #\newline)
			      #f
			      (parse-atom s)))
			 (catch-block (parse-block s)))
		    (expect-end s)
		    (list 'try try-block var catch-block)))
	 (else    (error (string "unexpected " nxt))))))
    ((return)          (let ((t (peek-token s)))
			 (if (or (eqv? t #\newline) (closing-token? t))
			     (list 'return '(null))
			     (list 'return (parse-eq s)))))
    ((break continue)  (list word))
    ((const)
     (let ((assgn (parse-eq s)))
       (if (not (and (pair? assgn)
		     (or (eq? (car assgn) '=)
			 (eq? (car assgn) 'global)
			 (eq? (car assgn) 'local))))
	   (error "expected assignment after const")
	   `(const ,assgn))))
    ((module)
     (let ((name (parse-atom s)))
       (begin0 (list word name (parse-block s))
	       (expect-end s))))
    ((ccall)
     (if (not (eqv? (peek-token s) #\())
	 (error "expected ( after ccall"))
     (take-token s)
     (let ((al (parse-arglist s #\))))
       (if (and (length> al 1)
		(memq (cadr al) '(cdecl stdcall fastcall)))
	   ;; place (callingconv) at end of arglist
	   `(ccall ,(car al) ,@(cddr al) (,(cadr al)))
	   `(ccall ,.al))))
    (else (error "unhandled reserved word")))))

; parse comma-separated assignments, like "i=1:n,j=1:m,..."
(define (parse-comma-separated-assignments s)
  (let loop ((ranges '()))
    (let ((r (parse-eq* s)))
      (case (peek-token s)
	((#\,)  (take-token s) (loop (cons r ranges)))
	(else   (reverse! (cons r ranges)))))))

; as above, but allows both "i=r" and "i in r"
(define (parse-comma-separated-iters s)
  (let loop ((ranges '()))
    (let ((r (parse-eq* s)))
      (let ((r (cond ((and (pair? r) (eq? (car r) '=))
		      r)
		     ((eq? r ':)
		      r)
		     ((eq? (peek-token s) 'in)
		      (begin (take-token s)
			     `(= ,r ,(parse-eq* s))))
		     (else
		      (error "invalid iteration specification")))))
	(case (peek-token s)
	  ((#\,)  (take-token s) (loop (cons r ranges)))
	  (else   (reverse! (cons r ranges))))))))

(define (parse-space-separated-exprs s)
  (let ((inside-vec space-sensitive))
    (with-space-sensitive
     (let loop ((exprs '()))
       (if (or (closing-token? (peek-token s))
	       (newline? (peek-token s))
	       (and inside-vec (eq? (peek-token s) '|\||)))
	   (reverse! exprs)
	   (let ((e (parse-eq s)))
	     (case (peek-token s)
	       ((#\newline)   (reverse! (cons e exprs)))
	       (else          (loop (cons e exprs))))))))))

; handle function call argument list, or any comma-delimited list.
; . an extra comma at the end is allowed
; . expressions after a ; are enclosed in (parameters ...)
; . an expression followed by ... becomes (... x)
(define (parse-arglist s closer)
  (with-normal-ops (parse-arglist- s closer)))
(define (parse-arglist- s closer)
  (let loop ((lst '()))
    (let ((t (require-token s)))
      (if (equal? t closer)
	  (begin (take-token s)
		 (reverse lst))
	  (if (equal? t #\;)
	      (begin (take-token s)
		     (if (equal? (peek-token s) closer)
			 ;; allow f(a, b; )
			 (begin (take-token s)
				(reverse lst))
			 (reverse (cons (cons 'parameters (loop '()))
					lst))))
	      (let* ((nxt (parse-eq* s))
		     (c (require-token s)))
		(if (assignment? nxt)
		    (error "assignment in function calls not allowed"))
		(cond ((eqv? c #\,)
		       (begin (take-token s) (loop (cons nxt lst))))
		      ((eqv? c #\;)          (loop (cons nxt lst)))
		      ((equal? c closer)     (loop (cons nxt lst)))
		      ;; newline character isn't detectable here
		      #;((eqv? c #\newline)
		       (error "unexpected line break in argument list"))
		      ((memv c '(#\] #\}))
		       (error (string "unexpected " c
				      " in argument list")))
		      (else
		       (error "missing comma or ) in argument list")))))))))

; parse [] concatenation expressions and {} cell expressions
(define (parse-vcat s first closer)
  (let loop ((lst '())
	     (nxt first))
    (let ((t (require-token s)))
      (if (eqv? t closer)
	  (begin (take-token s)
		 (cons 'vcat (reverse (cons nxt lst))))
	  (case t
	    ((#\,)
	     (take-token s)
	     (if (eqv? (require-token s) closer)
		 ;; allow ending with ,
		 (begin (take-token s)
			(cons 'vcat (reverse (cons nxt lst))))
		 (loop (cons nxt lst) (parse-eq* s))))
	    ((#\;)
	     (error "unexpected semicolon in array expression"))
	    ((#\] #\})
	     (error (string "unexpected " t)))
	    (else
	     (error "missing separator in array expression")))))))

(define (parse-matrix s first closer)
  (define (fix head v) (cons head (reverse v)))
  (define (update-outer v outer)
    (cond ((null? v)       outer)
	  ((null? (cdr v)) (cons (car v) outer))
	  (else            (cons (fix 'row v) outer))))
  (let loop ((vec   (list first))
	     (outer '()))
    (let ((t  (if (eqv? (peek-token s) #\newline)
		  #\newline
		  (require-token s))))
      (if (eqv? t closer)
	  (begin (take-token s)
		 (if (pair? outer)
		     (fix 'vcat (update-outer vec outer))
		     (if (or (null? vec) (null? (cdr vec)))
			 (fix 'vcat vec)     ; [x]   => (vcat x)
			 (fix 'hcat vec))))  ; [x y] => (hcat x y)
	  (case t
	    ((#\; #\newline)
	     (take-token s) (loop '() (update-outer vec outer)))
	    ((#\,)
	     (error "unexpected comma in matrix expression"))
	    ((#\] #\})
	     (error (string "unexpected " t)))
	    (else
	     (loop (cons (parse-eq* s) vec) outer)))))))

(define (parse-cat s closer)
  (with-normal-ops
   (with-space-sensitive
    (parse-cat- s closer))))
(define (parse-cat- s closer)
  (if (eqv? (require-token s) closer)
      (begin (take-token s)
	     (list 'vcat))  ; [] => (vcat)
      (let ((first (without-bitor (parse-eq* s))))
	(case (peek-token s)
	  ;; dispatch to array syntax, comprehension, or matrix syntax
	  ((#\,)
	   (parse-vcat s first closer))
	  ((|\||)
	   (take-token s)
	   (let ((r (parse-comma-separated-iters s)))
	     (if (not (eqv? (require-token s) closer))
		 (error (string "expected " closer))
		 (take-token s))
	     `(comprehension ,first ,@r)))
	  (else
	   (parse-matrix s first closer))))))

; for sequenced evaluation inside expressions: e.g. (a;b, c;d)
(define (parse-stmts-within-expr s)
  (parse-Nary s parse-eq* #\; 'block '(#\, #\) ) #t))

(define (parse-tuple s first)
  (let loop ((lst '())
	     (nxt first))
    (if (assignment? nxt)
	(error "invalid syntax in tuple"))
    (let ((t (require-token s)))
      (case t
	((#\))
	 (take-token s)
	 (cons 'tuple (reverse (cons nxt lst))))
	((#\,)
	 (take-token s)
	 (if (eqv? (require-token s) #\))
	     ;; allow ending with ,
	     (begin (take-token s)
		    (cons 'tuple (reverse (cons nxt lst))))
	     (loop (cons nxt lst) (parse-eq* s))))
	((#\;)
	 (error "unexpected semicolon in tuple"))
	#;((#\newline)
	 (error "unexpected line break in tuple"))
	((#\] #\})
	 (error (string "unexpected " t " in tuple")))
	(else
	 (error "missing separator in tuple"))))))

(define (not-eof-2 c)
  (if (eof-object? c)
      (error "incomplete: invalid ` syntax")
      c))

(define (parse-backquote s)
  (let ((b (open-output-string))
	(p (ts:port s)))
    (let loop ((c (read-char p)))
      (if (eqv? c #\`)
	  #t
	  (begin (if (eqv? c #\\)
		     (let ((nextch (read-char p)))
		       (if (eqv? nextch #\`)
			   (write-char nextch b)
			   (begin (write-char #\\ b)
				  (write-char (not-eof-2 nextch) b))))
		     (write-char (not-eof-2 c) b))
		 (loop (read-char p)))))
    (let ((str (io.tostring! b)))
      `(macrocall cmd ,str))))

(define (not-eof-3 c)
  (if (eof-object? c)
      (error "incomplete: invalid string syntax")
      c))

; reads a raw string literal with no processing.
; quote can be escaped with \, but the \ is left in place.
; returns ("str" . b), b is a boolean telling whether interpolation is used
(define (parse-string-literal s)
  (let ((b (open-output-string))
	(p (ts:port s))
	(interpolate #f))
    (let loop ((c (read-char p)))
      (if (eqv? c #\")
	  #t
	  (begin (if (eqv? c #\\)
		     (let ((nextch (read-char p)))
		       (begin (write-char #\\ b)
			      (write-char (not-eof-3 nextch) b)))
		     (begin
		       (if (eqv? c #\$)
			   (set! interpolate #t))
		       (write-char (not-eof-3 c) b)))
		 (loop (read-char p)))))
    (cons (io.tostring! b) interpolate)))

(define (not-eof-1 c)
  (if (eof-object? c)
      (error "incomplete: invalid character literal")
      c))

(define (unescape-string s)
  (with-exception-catcher
   (lambda (e) (error "invalid escape sequence"))
   (lambda ()
     ;; process escape sequences using lisp read
     (read (open-input-string (string #\" s #\"))))))

; parse numbers, identifiers, parenthesized expressions, lists, vectors, etc.
(define (parse-atom s)
  (let ((t (require-token s)))
    (cond ((or (string? t) (number? t)) (take-token s))

	  ;; char literal
	  ((eq? t '|'|)
	   (take-token s)
	   (let ((firstch (read-char (ts:port s))))
	     (if (eqv? firstch #\')
	      (error "invalid character literal")
	      (if (and (not (eqv? firstch #\\))
		       (not (eof-object? firstch))
		       (eqv? (peek-char (ts:port s)) #\'))
	       ;; easy case: 1 character, no \
	       (begin (read-char (ts:port s)) firstch)
	       (let ((b (open-output-string)))
		 (let loop ((c firstch))
		   (if (eqv? c #\')
		       #t
		       (begin (write-char (not-eof-1 c) b)
			      (if (eqv? c #\\)
				  (write-char
				   (not-eof-1 (read-char (ts:port s))) b))
			      (loop (read-char (ts:port s))))))
		 (let ((str (unescape-string (io.tostring! b))))
		   (if (= (length str) 1)
		       ;; one byte, e.g. '\xff'. maybe not valid UTF-8, but we
		       ;; want to use the raw value as a codepoint in this case.
		       (wchar (aref str 0))
		       (if (or (not (= (string-length str) 1))
			       (not (string.isutf8 str)))
			   (error "invalid character literal")
			   (string.char str 0)))))))))

	  ;; symbol/expression quote
	  ((eq? t ':)
	   (take-token s)
	   (if (closing-token? (peek-token s))
	       ':
	       (let ((ex (parse-atom s)))
		 (list 'quote ex))))

	  ;; identifier
	  ((symbol? t) (take-token s))

	  ;; parens or tuple
	  ((eqv? t #\( )
	   (take-token s)
	   (with-normal-ops
	   (if (eqv? (require-token s) #\) )
	       ;; empty tuple ()
	       (begin (take-token s) '(tuple))
	       ;; here we parse the first subexpression separately, so
	       ;; we can look for a comma to see if it's a tuple. this lets us
	       ;; distinguish (x) from (x,)
	       (let* ((ex (parse-eq* s))
		      (t (require-token s)))
		 (cond ((eqv? t #\) )
			(take-token s)
			;; value in parentheses (x)
			(if (and (pair? ex) (eq? (car ex) '...))
			    `(tuple ,ex)
			    ex))
		       ((eqv? t #\, )
			;; tuple (x,) (x,y) (x...) etc.
			(parse-tuple s ex))
		       ((eqv? t #\;)
			;; parenthesized block (a;b;c)
			(take-token s)
			(let* ((blk (parse-stmts-within-expr s))
			       (tok (require-token s)))
			  (if (eqv? tok #\,)
			      (error "unexpected comma in statement block"))
			  (if (not (eqv? tok #\)))
			      (error "missing separator in statement block"))
			  (take-token s)
			  `(block ,ex ,blk)))
		       #;((eqv? t #\newline)
			(error "unexpected line break in tuple"))
		       ((memv t '(#\] #\}))
			(error (string "unexpected " t " in tuple")))
		       (else
			(error "missing separator in tuple")))))))

	  ;; cell expression
	  ((eqv? t #\{ )
	   (take-token s)
	   (if (eqv? (require-token s) #\})
	       (begin (take-token s) '(cell1d))
	       (let ((vex (parse-cat s #\})))
		 (cond ((eq? (car vex) 'comprehension)
			(cons 'cell-comprehension (cdr vex)))
		       ((eq? (car vex) 'hcat)
			`(cell2d 1 ,(length (cdr vex)) ,@(cdr vex)))
		       (else  ; (vcat ...)
			(if (and (pair? (cadr vex)) (eq? (caadr vex) 'row))
			    (let ((nr (length (cdr vex)))
				  (nc (length (cdadr vex))))
			      ;; make sure all rows are the same length
			      (if (not (every
					(lambda (x)
					  (and (pair? x)
					       (eq? (car x) 'row)
					       (length= (cdr x) nc)))
					(cddr vex)))
				  (error "inconsistent shape in cell expression"))
			      `(cell2d ,nr ,nc
				       ,@(apply append
						;; transpose to storage order
						(apply map list
						       (map cdr (cdr vex))))))
			    (if (any (lambda (x) (and (pair? x)
						      (eq? (car x) 'row)))
				     (cddr vex))
				(error "inconsistent shape in cell expression")
				`(cell1d ,@(cdr vex)))))))))

	  ;; cat expression
	  ((eqv? t #\[ )
	   (take-token s)
	   (parse-cat s #\]))

	  ;; string literal
	  ((eqv? t #\")
	   (take-token s)
	   (let ((ps (parse-string-literal s)))
	     (if (cdr ps)
		 `(macrocall str ,(car ps))
		 (let ((str (unescape-string (car ps))))
		   (if (not (string.isutf8 str))
		       (error "invalid UTF-8 sequence"))
		   str))))

	  ;; macro call
	  ((eqv? t #\@)
	   (take-token s)
	   (let ((head (parse-atom s)))
	     (if (not (symbol? head))
		 (error (string "invalid macro use @" head)))
	     `(macrocall ,head ,@(parse-space-separated-exprs s))))

	  ;; command syntax
	  ((eqv? t #\`)
	   (take-token s)
	   (parse-backquote s))

	  (else (error (string "invalid syntax: " (take-token s)))))))

; --- main entry point ---

;; can optionally specify which grammar production to parse.
;; default is parse-stmts.
(define (julia-parse s . production)
  (cond ((string? s)
	 (apply julia-parse (make-token-stream (open-input-string s))
		production))
	((port? s)
	 (apply julia-parse (make-token-stream s) production))
	((eof-object? s)
	 s)
	(else
	 ;; as a special case, allow early end of input if there is
	 ;; nothing left but whitespace
	 (skip-ws-and-comments (ts:port s))
	 (if (eqv? (peek-token s) #\newline) (take-token s))
	 (let ((t (peek-token s)))
	   (if (eof-object? t)
	       t
	       ((if (null? production) parse-stmts (car production))
		s))))))
