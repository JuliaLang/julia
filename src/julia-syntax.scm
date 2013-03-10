(define (quoted? e) (memq (car e) '(quote top line break)))

(define (lam:args x) (cadr x))
(define (lam:vars x) (llist-vars (lam:args x)))
(define (lam:vinfo x) (caddr x))
(define (lam:body x) (cadddr x))

;; allow (:: T) => (:: #gensym T) in formal argument lists
(define (fix-arglist l)
  (map (lambda (a)
	 (if (and (pair? a) (eq? (car a) '|::|) (null? (cddr a)))
	     `(|::| ,(gensy) ,(cadr a))
	     a))
       l))

(define (arg-name v)
  (cond ((and (symbol? v) (not (eq? v 'true)) (not (eq? v 'false)))
	 v)
	((not (pair? v))
	 (error (string "malformed function arguments " v)))
	(else
	 (case (car v)
	   ((...)         (decl-var (cadr v)))
	   ((= keyword)   (decl-var (caddr v)))
	   ((|::|)        (decl-var v))
	   (else (error (string "malformed function argument " v)))))))

; convert a lambda list into a list of just symbols
(define (llist-vars lst)
  (map arg-name lst))

(define (arg-type v)
  (cond ((symbol? v)  'Any)
	((not (pair? v))
	 (error (string "malformed function arguments " v)))
	(else
	 (case (car v)
	   ((...)         `(... ,(decl-type (cadr v))))
	   ((= keyword)   (decl-type (caddr v)))
	   ((|::|)        (decl-type v))
	   (else (error
		  (string "malformed function arguments " v)))))))

; get just argument types
(define (llist-types lst)
  (map arg-type lst))

(define (decl? e)
  (and (pair? e) (eq? (car e) '|::|)))

; get the variable name part of a declaration, x::int => x
(define (decl-var v)
  (if (decl? v) (cadr v) v))

(define (decl-type v)
  (if (decl? v) (caddr v) 'Any))

(define (sym-dot? e)
  (and (length= e 3) (eq? (car e) '|.|)
       (symbol? (cadr e))))

; make an expression safe for multiple evaluation
; for example a[f(x)] => (temp=f(x); a[temp])
; retuns a pair (expr . assignments)
; where 'assignments' is a list of needed assignment statements
(define (remove-argument-side-effects e)
  (let ((a '()))
    (if (not (pair? e))
	(cons e '())
	(cons (map (lambda (x)
		     (if (and (pair? x) (not (quoted? x))
			      (not (sym-dot? x)))
			 (let ((g (gensy)))
			   (if (or (eq? (car x) '...) (eq? (car x) '&))
			       (if (and (pair? (cadr x))
					(not (quoted? (cadr x))))
				   (begin (set! a (cons `(= ,g ,(cadr x)) a))
					  `(,(car x) ,g))
				   x)
			       (begin (set! a (cons `(= ,g ,x) a))
				      g)))
			 x))
		   e)
	      (reverse a)))))

(define (expand-update-operator- op lhs rhs)
  (let ((e (remove-argument-side-effects lhs)))
    `(block ,@(cdr e)
	    (= ,(car e) (call ,op ,(car e) ,rhs)))))

(define (expand-update-operator op lhs rhs)
  (if (and (pair? lhs) (eq? (car lhs) 'ref))
      ;; expand indexing inside op= first, to remove "end" and ":"
      (let* ((ex (apply-patterns patterns lhs))
	     (stmts (butlast (cdr ex)))
	     (refex (last    (cdr ex)))
	     (nuref `(ref ,(caddr refex) ,@(cdddr refex))))
	`(block ,@stmts
		,(expand-update-operator- op nuref rhs)))
      (expand-update-operator- op lhs rhs)))

(define (dotop? o) (eqv? (string.char (string o) 0) #\.))

;; accumulate a series of comparisons, with the given "and" constructor,
;; exit criteria, and "take" function that consumes part of a list,
;; returning (expression . rest)
(define (comp-accum e make-and done? take)
  (let loop ((e e)
	     (expr '()))
    (if (done? e) (cons expr e)
	(let ((ex_rest (take e)))
	  (loop (cdr ex_rest)
		(if (null? expr)
		    (car ex_rest)
		    (make-and expr (car ex_rest))))))))

(define (add-init arg arg2 expr)
  (if (eq? arg arg2) expr
      `(block (= ,arg2 ,arg) ,expr)))

;; generate a comparison from e.g. (a < b ...)
;; returning (expr . rest)
(define (compare-one e)
  (let* ((arg   (caddr e))
	 (arg2  (if (and (pair? arg)
			 (pair? (cdddr e)))
		    (gensy) arg)))
    (if (and (not (dotop? (cadr e)))
	     (length> e 5)
	     (pair? (cadddr (cdr e)))
	     (dotop? (cadddr (cddr e))))
	;; look ahead: if the 2nd argument of the next comparison is also
	;; an argument to an eager (dot) op, make sure we don't skip the
	;; initialization of its variable by short-circuiting
	(let ((s (gensy)))
	  (cons `(block
		  ,@(if (eq? arg arg2) '() `((= ,arg2 ,arg)))
		  (= ,s ,(cadddr (cdr e)))
		  (call ,(cadr e) ,(car e) ,arg2))
		(list* arg2 (cadddr e) s (cddddr (cdr e)))))
	(cons
	 (add-init arg arg2
		   `(call ,(cadr e) ,(car e) ,arg2))
	 (cons arg2 (cdddr e))))))

;; convert a series of scalar comparisons into && expressions
(define (expand-scalar-compare e)
  (comp-accum e
	      (lambda (a b) `(&& ,a ,b))
	      (lambda (x) (or (not (length> x 2)) (dotop? (cadr x))))
	      compare-one))

;; convert a series of scalar and vector comparisons into & calls,
;; combining as many scalar comparisons as possible into short-circuit
;; && sequences.
(define (expand-vector-compare e)
  (comp-accum e
	      (lambda (a b) `(call & ,a ,b))
	      (lambda (x) (not (length> x 2)))
	      (lambda (e)
		(if (dotop? (cadr e))
		    (compare-one e)
		    (expand-scalar-compare e)))))

(define (expand-compare-chain e)
  (car (expand-vector-compare e)))

;; last = is this last index?
(define (end-val a n tuples last)
  (if (null? tuples)
      (if last
	  (if (= n 1)
	      `(call (top endof) ,a)
	      `(call (top trailingsize) ,a ,n))
	      #;`(call (top div)
		     (call (top length) ,a)
		     (call (top *)
			   ,@(map (lambda (d) `(call (top size) ,a ,(1+ d)))
				  (iota (- n 1)))))
	  `(call (top size) ,a ,n))
      (let ((dimno `(call (top +) ,(- n (length tuples))
			  ,@(map (lambda (t) `(call (top length) ,t))
				 tuples))))
	(if last
	    `(call (top trailingsize) ,a ,dimno)
	    `(call (top size) ,a ,dimno)))))

; replace end inside ex with (call (top size) a n)
; affects only the closest ref expression, so doesn't go inside nested refs
(define (replace-end ex a n tuples last)
  (cond ((eq? ex 'end)                (end-val a n tuples last))
	((or (atom? ex) (quoted? ex)) ex)
	((eq? (car ex) 'ref)
	 ;; inside ref only replace within the first argument
	 (list* 'ref (replace-end (cadr ex) a n tuples last)
		(cddr ex)))
	(else
	 (cons (car ex)
	       (map (lambda (x) (replace-end x a n tuples last))
		    (cdr ex))))))

; translate index x from colons to ranges
(define (expand-index-colon x)
  (cond ((eq? x ':) `(call (top colon) 1 end))
	((and (pair? x)
	      (eq? (car x) ':))
	 (cond ((length= x 3)
		(if (eq? (caddr x) ':)
		    ;; (: a :) a:
		    `(call (top colon) ,(cadr x) end)
		    ;; (: a b)
		    `(call (top colon) ,(cadr x) ,(caddr x))))
	       ((length= x 4)
		(if (eq? (cadddr x) ':)
		    ;; (: a b :) a:b:
		    `(call (top colon) ,(cadr x) ,(caddr x) end)
		    ;; (: a b c)
		    `(call (top colon) ,@(cdr x))))
	       (else x)))
	(else x)))

;; : inside indexing means 1:end
;; expand end to size(a,n),
;;     or div(length(a), prod(size(a)[1:(n-1)])) for the last index
;; a = array being indexed, i = list of indexes
;; returns (values index-list stmts) where stmts are statements that need
;; to execute first.
(define (process-indexes a i)
  (let loop ((lst i)
	     (n   1)
	     (stmts '())
	     (tuples '())
	     (ret '()))
    (if (null? lst)
	(values (reverse ret) (reverse stmts))
	(let ((idx  (car lst))
	      (last (null? (cdr lst))))
	  (if (and (pair? idx) (eq? (car idx) '...))
	      (if (symbol? (cadr idx))
		  (loop (cdr lst) (+ n 1)
			stmts
			(cons (cadr idx) tuples)
			(cons `(... ,(replace-end (cadr idx) a n tuples last))
			      ret))
		  (let ((g (gensy)))
		    (loop (cdr lst) (+ n 1)
			  (cons `(= ,g ,(replace-end (cadr idx) a n tuples last))
				stmts)
			  (cons g tuples)
			  (cons `(... ,g) ret))))
	      (loop (cdr lst) (+ n 1)
		    stmts tuples
		    (cons (replace-end (expand-index-colon idx) a n tuples last)
			  ret)))))))

(define (make-decl n t) `(|::| ,n ,(if (and (pair? t) (eq? (car t) '...))
				       `(curly Vararg ,(cadr t))
				       t)))

(define (function-expr argl body)
  (let ((t (llist-types argl))
	(n (llist-vars argl)))
    (if (has-dups n)
	(error "function argument names not unique"))
    (let ((argl (map make-decl n t)))
      `(lambda ,argl
	 (scope-block ,body)))))

;; GF method does not need to keep decl expressions on lambda args
;; except for rest arg
(define (method-lambda-expr argl body)
  (let ((argl (map (lambda (x)
		     (if (and (pair? x) (eq? (car x) '...))
			 (make-decl (arg-name x) (arg-type x))
			 (arg-name x)))
		   argl)))
    `(lambda ,argl
       (scope-block ,body))))

(define (symbols->typevars sl upperbounds bnd)
  (let ((bnd (if bnd '(true) '())))
    (if (null? upperbounds)
	(map (lambda (x)    `(call (top TypeVar) ',x ,@bnd)) sl)
	(map (lambda (x ub) `(call (top TypeVar) ',x ,ub ,@bnd)) sl upperbounds))))

(define (sparam-name-bounds sparams names bounds)
  (cond ((null? sparams)
	 (values (reverse names) (reverse bounds)))
	((symbol? (car sparams))
	 (sparam-name-bounds (cdr sparams) (cons (car sparams) names)
			     (cons '(top Any) bounds)))
	((and (length= (car sparams) 3)
	      (eq? (caar sparams) '|<:|)
	      (symbol? (cadar sparams)))
	 (sparam-name-bounds (cdr sparams) (cons (cadr (car sparams)) names)
			     (cons (caddr (car sparams)) bounds)))
	(else
	 (error "malformed type parameter list"))))

(define (method-def-expr name sparams argl body)
  (if (has-dups (llist-vars argl))
      (error "function argument names not unique"))
  (if (not (symbol? name))
      (error (string "invalid method name " name)))
  (let* ((types (llist-types argl))
	 (body  (method-lambda-expr argl body)))
    (if (null? sparams)
	`(method ,name (tuple ,@types) ,body (tuple))
	(receive
	 (names bounds) (sparam-name-bounds sparams '() '())
	 (let ((f (gensy)))
	   `(call (lambda (,@names ,f)
		    (method ,name (tuple ,@types) ,f (tuple ,@names)))
		  ,@(symbols->typevars names bounds #t)
		  ,body))))))

(define (struct-def-expr name params super fields mut)
  (receive
   (params bounds) (sparam-name-bounds params '() '())
   (struct-def-expr- name params bounds super (flatten-blocks fields) mut)))

(define (default-inner-ctor name field-names field-types)
  `(function (call ,name
		   ,@(map make-decl field-names field-types))
	     (block
	      (call new ,@field-names))))

(define (default-outer-ctor name field-names field-types params bounds)
  `(function (call (curly ,name
			  ,@(map (lambda (p b) `(<: ,p ,b))
				 params bounds))
		   ,@(map make-decl field-names field-types))
	     (block
	      (call (curly ,name ,@params) ,@field-names))))

(define (new-call Texpr args field-names field-types mutabl)
  (cond ((length> args (length field-names))
	 `(call (top error) "new: too many arguments"))
	(else
	 `(new ,Texpr
	       ,@(map (lambda (fty val)
			`(call (top convert) ,fty ,val))
		      (list-head field-types (length args)) args)))))

(define (rewrite-ctor ctor Tname params field-names field-types mutabl)
  (define (ctor-body body)
    `(block ;; make type name global
	    (global ,Tname)
	    ,(pattern-replace (pattern-set
			       (pattern-lambda
				(call (-/ new) . args)
				(new-call (if (null? params)
					      Tname
					      `(curly ,Tname ,@params))
					  args
					  field-names
					  field-types
					  mutabl)))
			      body)))
  (let ((ctor2
	 (pattern-replace
	  (pattern-set
	   (pattern-lambda (function (call name . sig) body)
			   `(function ,(cadr __) ,(ctor-body body)))
	   (pattern-lambda (= (call name . sig) body)
			   `(= ,(cadr __) ,(ctor-body body)))
	   (pattern-lambda (function (call (curly name . p) . sig) body)
			   `(function ,(cadr __) ,(ctor-body body)))
	   (pattern-lambda (= (call (curly name . p) . sig) body)
			   `(= ,(cadr __) ,(ctor-body body))))
	  ctor)))
    ctor2))

;; remove line numbers and nested blocks
(define (flatten-blocks e)
  (if (atom? e)
      e
      (apply append!
	     (map (lambda (x)
		    (cond ((atom? x) (list x))
			  ((eq? (car x) 'line) '())
			  ((eq? (car x) 'block) (cdr (flatten-blocks x)))
			  (else (list x))))
		  e))))

(define (struct-def-expr- name params bounds super fields mut)
  (receive
   (fields defs) (separate (lambda (x) (or (symbol? x) (decl? x)))
			   fields)
   (let* ((field-names (map decl-var fields))
	  (field-types (map decl-type fields))
	  (defs2 (if (null? defs)
		     (list (default-inner-ctor name field-names field-types))
		     defs)))
     (if (null? params)
	 `(block
	   (global ,name)
	   (const ,name)
	   (composite_type ,name (tuple ,@params)
			   (tuple ,@(map (lambda (x) `',x) field-names))
			   (null) ,super (tuple ,@field-types)
			   ,mut)
	   (call
	    (lambda ()
	      (scope-block
	       (block
		(global ,name)
		,@(map (lambda (c)
			 (rewrite-ctor c name '() field-names field-types mut))
		       defs2)))))
	   (null))
	 ;; parametric case
	 `(block
	   (scope-block
	    (block
	     (global ,name)
	     (const ,name)
	     ,@(map (lambda (v) `(local ,v)) params)
	     ,@(map make-assignment params (symbols->typevars params bounds #f))
	     (composite_type ,name (tuple ,@params)
			     (tuple ,@(map (lambda (x) `',x) field-names))
			     (lambda (,name)
			       (scope-block
				;; don't capture params; in here they are static
				;; parameters
				(block
				 (global ,@params)
				 ,@(map
				    (lambda (c)
				      (rewrite-ctor c name params field-names
						    field-types mut))
				    defs2)
				 ,name)))
			     ,super (tuple ,@field-types)
			     ,mut)))
	   (scope-block
	    (block
	     (global ,@params)
	     ,@(if (null? defs)
		   `(,(default-outer-ctor name field-names field-types
			params bounds))
		   '())))
	   (null))))))

(define (abstract-type-def-expr name params super)
  (receive
   (params bounds)
   (sparam-name-bounds params '() '())
   `(block
     (const ,name)
     ,@(map (lambda (v) `(local ,v)) params)
     ,@(map make-assignment params (symbols->typevars params bounds #f))
     (abstract_type ,name (tuple ,@params) ,super))))

(define (bits-def-expr n name params super)
  (receive
   (params bounds)
   (sparam-name-bounds params '() '())
   `(block
     (const ,name)
     ,@(map (lambda (v) `(local ,v)) params)
     ,@(map make-assignment params (symbols->typevars params bounds #f))
     (bits_type ,name (tuple ,@params) ,n ,super))))

; take apart a type signature, e.g. T{X} <: S{Y}
(define (analyze-type-sig ex)
  (or ((pattern-lambda (-- name (-s))
		       (values name '() 'Any)) ex)
      ((pattern-lambda (curly (-- name (-s)) . params)
		       (values name params 'Any)) ex)
      ((pattern-lambda (|<:| (-- name (-s)) super)
		       (values name '() super)) ex)
      ((pattern-lambda (|<:| (curly (-- name (-s)) . params) super)
		       (values name params super)) ex)
      (error "invalid type signature")))

;; insert calls to convert() in ccall, and pull out expressions that might
;; need to be rooted before conversion.
(define (lower-ccall name RT atypes args)
  (define (ccall-conversion T x)
    (cond ((eq? T 'Any)  x)
	  ((and (pair? x) (eq? (car x) '&))
	   `(& (call (top ptr_arg_convert) ,T ,(cadr x))))
	  (else
	   `(call (top cconvert) ,T ,x))))
  (define (argument-root a)
    ;; something to keep rooted for this argument
    (cond ((and (pair? a) (eq? (car a) '&))
	   (argument-root (cadr a)))
	  ((and (pair? a) (sym-dot? a))
	   (cadr a))
	  ((symbol? a)  a)
	  (else         0)))
  (let loop ((F atypes)  ;; formals
	     (A args)    ;; actuals
	     (stmts '()) ;; initializers
	     (C '()))    ;; converted
    (if (or (null? F) (null? A))
	`(block
	  ,.(reverse! stmts)
	  (call (top ccall) ,name ,RT (tuple ,@atypes) ,.(reverse! C)
		,@A))
	(let* ((a     (car A))
	       (isseq (and (pair? (car F)) (eq? (caar F) '...)))
	       (ty    (if isseq (cadar F) (car F)))
	       (rt (if (eq? ty 'Any)
		       0
		       (argument-root a)))
	       (ca (cond ((eq? ty 'Any)
			  a)
			 ((and (pair? a) (eq? (car a) '&))
			  (if (and (pair? (cadr a)) (not (sym-dot? (cadr a))))
			      (let ((g (gensy)))
				(begin
				  (set! stmts (cons `(= ,g ,(cadr a)) stmts))
				  `(& ,g)))
			      a))
			 ((and (pair? a) (not (sym-dot? a)) (not (quoted? a)))
			  (let ((g (gensy)))
			    (begin
			      (set! stmts (cons `(= ,g ,a) stmts))
			      g)))
			 (else
			  a))))
	  (loop (if isseq F (cdr F)) (cdr A) stmts
		(list* rt (ccall-conversion ty ca) C))))))

(define (replace-return e bb ret retval)
  (cond ((or (atom? e) (quoted? e)) e)
	((or (eq? (car e) 'lambda)
	     (eq? (car e) 'function)
	     (eq? (car e) '->)) e)
	((eq? (car e) 'return)
	 `(block (= ,ret true)
		 (= ,retval ,(cadr e))
		 (break ,bb)))
	(else (map (lambda (x) (replace-return x bb ret retval)) e))))

;; patterns that introduce lambdas
(define binding-form-patterns
  (pattern-set
   ;; function with static parameters
   (pattern-lambda (function (call (curly name . sparams) . argl) body)
		   (method-def-expr name sparams (fix-arglist argl) body))

   ;; function definition
   (pattern-lambda (function (call name . argl) body)
		   (method-def-expr name '() (fix-arglist argl) body))

   (pattern-lambda (function (tuple . args) body)
		   `(-> (tuple ,@args) ,body))

   ;; expression form function definition
   (pattern-lambda (= (call (curly name . sparams) . argl) body)
		   `(function (call (curly ,name . ,sparams) . ,argl) ,body))
   (pattern-lambda (= (call name . argl) body)
		   `(function (call ,name ,@argl) ,body))

   ;; anonymous function
   (pattern-lambda (-> a b)
		   (let ((a (if (and (pair? a)
				     (eq? (car a) 'tuple))
				(cdr a)
				(list a))))
		     (function-expr (fix-arglist a)
				    `(block
				      ,@(map (lambda (d)
					       `(= ,(cadr d)
						   (typeassert ,@(cdr d))))
					     (filter decl? a))
				      ,b))))

   ;; let
   (pattern-lambda (let ex . binds)
		   (let loop ((binds binds)
			      (args  ())
			      (inits ())
			      (locls ())
			      (stmts ()))
		     (if (null? binds)
			 (begin
			   (if (has-dups args)
			       (error "let variables not unique"))
			   `(call (-> (tuple ,@args)
				      (block ,@(if (null? locls)
						   '()
						   `((local ,@locls)))
					     ,@stmts
					     ,ex))
				  ,@inits))
			 (cond
			  ((or (symbol? (car binds)) (decl? (car binds)))
			   ;; just symbol -> add local
			   (loop (cdr binds) args inits
				 (cons (car binds) locls)
				 stmts))
			  ((and (length= (car binds) 3)
				(eq? (caar binds) '=))
			   ;; some kind of assignment
			   (cond
			    ((or (symbol? (cadar binds))
				 (decl?   (cadar binds)))
			     ;; a=b -> add argument
			     (loop (cdr binds)
				   (cons (cadar binds) args)
				   (cons (caddar binds) inits)
				   locls stmts))
				   #;(cons (cadar binds) locls)
				   #;(cons `(= ,(decl-var (cadar binds))
					     ,(caddar binds))
					 stmts);))
			    ((and (pair? (cadar binds))
				  (eq? (caadar binds) 'call))
			     ;; f()=c
			     (let ((asgn (cadr (julia-expand0 (car binds)))))
			       (loop (cdr binds) args inits
				     (cons (cadr asgn) locls)
				     (cons asgn stmts))))
			    (else (error "invalid let syntax"))))
			  (else (error "invalid let syntax"))))))

   ;; macro definition
   (pattern-lambda (macro (call name . argl) body)
		   `(macro ,(symbol (string #\@ name))
		      (-> (tuple ,@argl) ,body)))

   ;; type definition
   (pattern-lambda (type mut sig (block . fields))
		   (receive (name params super) (analyze-type-sig sig)
			    (struct-def-expr name params super fields mut)))

   ;; try with finally
   (pattern-lambda (try tryb var catchb finalb)
		   (let ((err (gensy))
			 (ret (gensy))
			 (retval (gensy))
			 (bb  (gensy))
			 (val (gensy)))
		     (let ((tryb   (replace-return tryb bb ret retval))
			   (catchb (replace-return catchb bb ret retval)))
		       `(scope-block
			 (block
			  (local ,retval)
			  (= ,err false)
			  (= ,ret false)
			  (break-block
			   ,bb
			   (= ,val (try ,(if catchb
					     `(try ,tryb ,var ,catchb)
					     tryb)
					#f
					(= ,err true))))
			  ,finalb
			  (if ,err (ccall 'jl_rethrow Void (tuple)))
			  (if ,ret (return ,retval))
			  ,val)))))

   ;; try - catch
   (pattern-lambda (try tryb var catchb)
		   (if (symbol? var)
		       `(trycatch (scope-block ,tryb)
				  (scope-block
				   (block (= ,var (the_exception))
					  ,catchb)))
		       `(trycatch (scope-block ,tryb)
				  (scope-block ,catchb))))

   )) ; binding-form-patterns

;; a copy of the above patterns, but returning the names of vars
;; introduced by the forms, instead of their transformations.
(define vars-introduced-by-patterns
  (pattern-set
   ;; function with static parameters
   (pattern-lambda (function (call (curly name . sparams) . argl) body)
		   (cons 'varlist (llist-vars (fix-arglist argl))))

   ;; function definition
   (pattern-lambda (function (call name . argl) body)
		   (cons 'varlist (llist-vars (fix-arglist argl))))

   (pattern-lambda (function (tuple . args) body)
		   `(-> (tuple ,@args) ,body))

   ;; expression form function definition
   (pattern-lambda (= (call (curly name . sparams) . argl) body)
		   `(function (call (curly ,name . ,sparams) . ,argl) ,body))
   (pattern-lambda (= (call name . argl) body)
		   `(function (call ,name ,@argl) ,body))

   ;; anonymous function
   (pattern-lambda (-> a b)
		   (let ((a (if (and (pair? a)
				     (eq? (car a) 'tuple))
				(cdr a)
				(list a))))
		     (cons 'varlist (llist-vars (fix-arglist a)))))

   ;; let
   (pattern-lambda (let ex . binds)
		   (let loop ((binds binds)
			      (args  ())
			      (inits ())
			      (locls ())
			      (stmts ()))
		     (if (null? binds)
			 (cons 'varlist
			       (append! (llist-vars (fix-arglist args))
					locls))
			 (cond
			  ((or (symbol? (car binds)) (decl? (car binds)))
			   ;; just symbol -> add local
			   (loop (cdr binds) args inits
				 (cons (car binds) locls)
				 stmts))
			  ((and (length= (car binds) 3)
				(eq? (caar binds) '=))
			   ;; some kind of assignment
			   (cond
			    ((or (symbol? (cadar binds))
				 (decl?   (cadar binds)))
			     ;; a=b -> add argument
			     (loop (cdr binds)
				   (cons (cadar binds) args)
				   (cons (caddar binds) inits)
				   locls stmts))
			    ((and (pair? (cadar binds))
				  (eq? (caadar binds) 'call))
			     ;; f()=c
			     (let ((asgn (cadr (julia-expand0 (car binds)))))
			       (loop (cdr binds) args inits
				     (cons (cadr asgn) locls)
				     (cons asgn stmts))))
			    (else '())))
			  (else '())))))

   ;; macro definition
   (pattern-lambda (macro (call name . argl) body)
		   `(-> (tuple ,@argl) ,body))

   (pattern-lambda (try tryb var catchb finalb)
		   (if var (list 'varlist var) '()))
   (pattern-lambda (try tryb var catchb)
		   (if var (list 'varlist var) '()))

   )) ; vars-introduced-by-patterns

; local x, y=2, z => local x;local y;local z;y = 2
(define (expand-decls what binds)
  (if (not (list? binds))
      (error (string "invalid " what " declaration")))
  (let loop ((b       binds)
	     (vars    '())
	     (assigns '()))
    (if (null? b)
	(if (and (null? assigns)
		 (length= vars 1))
	    `(,what ,(car vars))
	    `(block
	      ,@(map (lambda (x) `(,what ,x)) vars)
	      ,@(reverse assigns)))
	(let ((x (car b)))
	  (cond ((and (pair? x) (memq (car x) assignment-ops))
		 (loop (cdr b)
		       (cons (cadr x) vars)
		       (cons `(,(car x) ,(decl-var (cadr x)) ,(caddr x))
			     assigns)))
		((and (pair? x) (eq? (car x) '|::|))
		 (loop (cdr b)
		       (cons (decl-var x) vars)
		       (cons x assigns)))
		((symbol? x)
		 (loop (cdr b) (cons x vars) assigns))
		(else
		 (error (string "invalid syntax in " what " declaration"))))))))

(define (make-assignment l r) `(= ,l ,r))
(define (assignment? e) (and (pair? e) (eq? (car e) '=)))

(define (const-check-symbol s)
  (if (not (symbol? s))
      (error "expected identifier after const")
      s))

(define (qualified-const-expr binds __)
  (let ((vs (map (lambda (b)
		   (if (assignment? b)
		       (const-check-symbol (decl-var (cadr b)))
		       (error "expected assignment after const")))
		 binds)))
    `(block ,@(map (lambda (v) `(const ,v)) vs)
	    ,(cadr __))))

;; convert (lhss...) = (tuple ...) to assignments, eliminating the tuple
(define (tuple-to-assignments lhss x)
  (let loop ((lhss lhss)
	     (rhss (cdr x))
	     (stmts '())
	     (after '())
	     (elts  '()))
    (if (null? lhss)
	`(block ,@(reverse stmts)
		,@(reverse after)
		(unnecessary-tuple (tuple ,@(reverse elts))))
	(let ((L (car lhss))
	      (R (car rhss)))
	  (if (and (symbol? L)
		   ;; overwrite var immediately if it doesn't occur elsewhere
		   (not (contains (lambda (e) (eq? e L)) (cdr rhss))))
	      (loop (cdr lhss)
		    (cdr rhss)
		    (cons (make-assignment L R) stmts)
		    after
		    (cons L elts))
	      (let ((temp (gensy)))
		(loop (cdr lhss)
		      (cdr rhss)
		      (cons (make-assignment temp R) stmts)
		      (cons (make-assignment L temp) after)
		      (cons temp elts))))))))

;; convert (lhss...) = x to tuple indexing, handling the general case
(define (lower-tuple-assignment lhss x)
  (let ((t (gensy)))
    `(block
      (= ,t ,x)
      ,@(let loop ((lhs lhss)
		   (i   1))
	  (if (null? lhs) '((null))
	      (cons `(= ,(car lhs)
			(call (top tupleref) ,t ,i))
		    (loop (cdr lhs)
			  (+ i 1)))))
      ,t)))

(define patterns
  (pattern-set
   (pattern-lambda (block)
		   `(block (null)))

   (pattern-lambda (|.| a b)
		   `(call (top getfield) ,a ,b))

   (pattern-lambda (= (|.| a b) rhs)
		   (let ((aa (if (atom? a) a (gensy)))
			 (bb (if (or (atom? b) (quoted? b)) b (gensy))))
		     `(block
		       ,@(if (eq? aa a) '() `((= ,aa ,a)))
		       ,@(if (eq? bb b) '() `((= ,bb ,b)))
		       (call (top setfield) ,aa ,bb
			     (call (top convert)
				   (call (top fieldtype) ,aa ,bb)
				   ,rhs)))))

   (pattern-lambda (abstract sig)
		   (receive (name params super) (analyze-type-sig sig)
			    (abstract-type-def-expr name params super)))

   (pattern-lambda (bitstype n sig)
		   (receive (name params super) (analyze-type-sig sig)
			    (bits-def-expr n name params super)))

   ; typealias is an assignment; should be const when that exists
   (pattern-lambda (typealias (-- name (-s)) type-ex)
		   `(const (= ,name ,type-ex)))
   (pattern-lambda (typealias (curly (-- name (-s)) . params) type-ex)
		   (receive
		    (params bounds)
		    (sparam-name-bounds params '() '())
		    `(call (lambda ,params
			     (const
			      (= ,name (call (top TypeConstructor)
					     (tuple ,@params) ,type-ex))))
			   ,@(symbols->typevars params bounds #f))))

   (pattern-lambda (comparison . chain) (expand-compare-chain chain))

   ;; multiple value assignment a,b = x
   (pattern-lambda
    (= (tuple . lhss) x)
    (if (and (pair? x) (pair? lhss) (eq? (car x) 'tuple)
	     (length= lhss (length (cdr x))))
	;; (a, b, ...) = (x, y, ...)
	(tuple-to-assignments lhss x)
	;; (a, b, ...) = other
	(let* ((xx  (if (and (symbol? x) (not (memq x lhss)))
			x (gensy)))
	       (ini (if (eq? x xx) '() `((= ,xx ,x))))
	       (st  (gensy)))
	  `(block
	    ,@ini
	    (= ,st (call (top start) ,xx))
	    ,.(map (lambda (i lhs)
		     (lower-tuple-assignment
		      (list lhs st)
		      `(call (|.| (top Base) (quote indexed_next)) ,xx ,(+ i 1) ,st)))
		   (iota (length lhss))
		   lhss)
	    ,xx))))

   (pattern-lambda (= (ref a . idxs) rhs)
		   (let* ((reuse (and (pair? a)
				      (contains (lambda (x)
						  (or (eq? x 'end)
						      (and (pair? x)
							   (eq? (car x) ':))))
						idxs)))
			  (arr   (if reuse (gensy) a))
			  (stmts (if reuse `((= ,arr ,a)) '())))
		     (let* ((rrhs (and (pair? rhs) (not (quoted? rhs))))
			    (r    (if rrhs (gensy) rhs))
			    (rini (if rrhs `((= ,r ,rhs)) '())))
		       (receive
			(new-idxs stuff) (process-indexes arr idxs)
			`(block
			  ,@stmts
			  ,@stuff
			  ,@rini
			  (call setindex! ,arr ,r ,@new-idxs)
			  ,r)))))

   (pattern-lambda (ref a . idxs)
		   (let* ((reuse (and (pair? a)
				      (contains (lambda (x)
						  (or (eq? x 'end)
						      (eq? x ':)
						      (and (pair? x)
							   (eq? (car x) ':))))
						idxs)))
			  (arr   (if reuse (gensy) a))
			  (stmts (if reuse `((= ,arr ,a)) '())))
		     (receive
		      (new-idxs stuff) (process-indexes arr idxs)
		      `(block
			,@(append stmts stuff)
			(call getindex ,arr ,@new-idxs)))))

   (pattern-lambda (curly type . elts)
		   `(call (top apply_type) ,type ,@elts))

   ;; call with splat
   (pattern-lambda (call f ... (... _) ...)
		   (let ((argl (cddr __)))
		     ; wrap sequences of non-... arguments in tuple()
		     (define (tuple-wrap a run)
		       (if (null? a)
			   (if (null? run) '()
			       (list `(call (top tuple) ,@(reverse run))))
			   (let ((x (car a)))
			     (if (and (length= x 2)
				      (eq? (car x) '...))
				 (if (null? run)
				     (list* (cadr x)
					    (tuple-wrap (cdr a) '()))
				     (list* `(call (top tuple) ,@(reverse run))
					    (cadr x)
					    (tuple-wrap (cdr a) '())))
				 (tuple-wrap (cdr a) (cons x run))))))
		     `(call (top apply) ,f ,@(tuple-wrap argl '()))))

   ; tuple syntax (a, b...)
   ; note, directly inside tuple ... means Vararg type
   (pattern-lambda (tuple . args)
		   `(call (top tuple)
			  ,@(map (lambda (x)
				   (if (and (length= x 2)
					    (eq? (car x) '...))
				       `(curly Vararg ,(cadr x))
				       x))
				 args)))

   ;; dict syntax
   (pattern-lambda (dict . args)
		   `(call (top Dict)
			  (tuple ,@(map cadr  args))
			  (tuple ,@(map caddr args))))

   ;; typed dict syntax
   (pattern-lambda (typed_dict atypes . args)
		   (if (and (length= atypes 3)
			    (eq? (car atypes) '=>))
		       `(call (curly (top Dict) ,(cadr atypes) ,(caddr atypes))
			      (tuple ,@(map cadr  args))
			      (tuple ,@(map caddr args)))
		       (error (string "invalid typed_dict syntax " atypes))))

   ;; cell array syntax
   (pattern-lambda (cell1d . args)
		   (cond ((any (lambda (e) (and (pair? e) (eq? (car e) '...)))
			       args)
			  `(call (top cell_1d) ,@args))
			 (else
			  (let ((name (gensy)))
			    `(block (= ,name (call (top Array) (top Any)
						   ,(length args)))
				    ,@(map (lambda (i elt)
					     `(call (top arrayset) ,name
						    ,elt ,(+ 1 i)))
					   (iota (length args))
					   args)
				    ,name)))))

   (pattern-lambda (cell2d nr nc . args)
		   (if (any (lambda (e) (and (pair? e) (eq? (car e) '...)))
			    args)
		       `(call (top cell_2d) ,nr ,nc ,@args)
		       (let ((name (gensy)))
			 `(block (= ,name (call (top Array) (top Any)
						,nr ,nc))
				 ,@(map (lambda (i elt)
					  `(call (top arrayset) ,name
						 ,elt ,(+ 1 i)))
					(iota (* nr nc))
					args)
				 ,name))))

   ;; expand anything but "local x" with one symbol
   ;; local x,y,z => local x;local y;local z
   (pattern-lambda (local (-s)) __)
   (pattern-lambda (local . binds)
		   (expand-decls 'local binds))

   ;; global x,y,z => global x;global y;global z
   (pattern-lambda (global (-s)) __)
   (pattern-lambda (global . binds)
		   (expand-decls 'global binds))

   ; x::T = rhs => x::T; x = rhs
   (pattern-lambda (= (|::| x T) rhs)
		   (let ((e (remove-argument-side-effects x)))
		     `(block ,@(cdr e)
			     (|::| ,(car e) ,T)
			     (= ,(car e) ,rhs))))

   ; <expr>::T => typeassert(expr, T)
   (pattern-lambda (|::| (-- expr (-^ (-s))) T)
		   `(call (top typeassert) ,expr ,T))

   ;; ::T outside arg list syntax error
   (pattern-lambda (|::| _)
		   (error "invalid :: syntax"))

   ;; constant definition
   (pattern-lambda (const (= lhs rhs))
		   `(block (const ,(const-check-symbol (decl-var lhs)))
			   (= ,lhs ,rhs)))
   (pattern-lambda (const (global . binds))
		   (qualified-const-expr binds __))
   (pattern-lambda (const (local . binds))
		   (qualified-const-expr binds __))

   ;; incorrect multiple return syntax [a, b, ...] = foo
   (pattern-lambda (= (vcat . args) rhs)
		   (error "use \"(a, b) = ...\" to assign multiple values"))

   ; adding break/continue support to while loop
   (pattern-lambda (while cnd body)
		   `(scope-block
		     (break-block loop-exit
				  (_while ,cnd
					  (break-block loop-cont
						       ,body)))))

   (pattern-lambda (break) '(break loop-exit))
   (pattern-lambda (continue) '(break loop-cont))

   ;; for loops

   (pattern-lambda
    (for (= lhs (: a b)) body)
    (begin
      (let ((cnt (gensy))
	    (lim (if (number? b) b (gensy))))
	`(scope-block
	  (block
	   (= ,cnt ,a)
	   ,@(if (eq? lim b) '() `((= ,lim ,b)))
	   (break-block loop-exit
			(_while (call (top <=) ,cnt ,lim)
				(block
				 (= ,lhs ,cnt)
				 (break-block loop-cont
					      ,body)
				 (= ,cnt (call (top convert)
					       (call (top typeof) ,cnt)
					       (call (top +) 1 ,cnt)))))))))))

   ; for loop over arbitrary vectors
   (pattern-lambda
    (for (= lhs X) body)
    (let ((coll  (gensy))
	  (state (gensy)))
      `(scope-block
	(block (= ,coll ,X)
	       (= ,state (call (top start) ,coll))
	       (while (call (top !) (call (top done) ,coll ,state))
		      (block
		       ,(lower-tuple-assignment (list lhs state)
						`(call (top next) ,coll ,state))
		       ,body))))))

   ;; update operators
   (pattern-lambda (+= a b)     (expand-update-operator '+ a b))
   (pattern-lambda (-= a b)     (expand-update-operator '- a b))
   (pattern-lambda (*= a b)     (expand-update-operator '* a b))
   (pattern-lambda (.*= a b)    (expand-update-operator '.* a b))
   (pattern-lambda (/= a b)     (expand-update-operator '/ a b))
   (pattern-lambda (./= a b)    (expand-update-operator './ a b))
   (pattern-lambda (//= a b)    (expand-update-operator '// a b))
   (pattern-lambda (.//= a b)   (expand-update-operator '.// a b))
   (pattern-lambda (|\\=| a b)  (expand-update-operator '|\\| a b))
   (pattern-lambda (|.\\=| a b) (expand-update-operator '|.\\| a b))
   (pattern-lambda (|.+=| a b)  (expand-update-operator '|.+| a b))
   (pattern-lambda (|.-=| a b)  (expand-update-operator '|.-| a b))
   (pattern-lambda (^= a b)     (expand-update-operator '^ a b))
   (pattern-lambda (.^= a b)    (expand-update-operator '.^ a b))
   (pattern-lambda (%= a b)     (expand-update-operator '% a b))
   (pattern-lambda (|\|=| a b)  (expand-update-operator '|\|| a b))
   (pattern-lambda (&= a b)     (expand-update-operator '& a b))
   (pattern-lambda ($= a b)     (expand-update-operator '$ a b))
   (pattern-lambda (<<= a b)    (expand-update-operator '<< a b))
   (pattern-lambda (>>= a b)    (expand-update-operator '>> a b))
   (pattern-lambda (>>>= a b)   (expand-update-operator '>>> a b))

   (pattern-lambda (: a (-/ :))     (error "invalid ':' outside indexing"))
   (pattern-lambda (: a b (-/ :))   (error "invalid ':' outside indexing"))
   (pattern-lambda (: (: b (-/ :))) (error "invalid ':' outside indexing"))
   (pattern-lambda (: (: b c))      (error "invalid ':' outside indexing"))
   (pattern-lambda (: c)            (error "invalid ':' outside indexing"))

   (pattern-lambda (: a b c)
		   `(call (top colon) ,a ,b ,c))

   (pattern-lambda (: a b)
		   `(call (top colon) ,a ,b))

   ;; hcat, vcat
   (pattern-lambda (hcat . a)
		   `(call (top hcat) ,@a))

   (pattern-lambda (vcat . a)
		   (if (any (lambda (x)
			      (and (pair? x) (eq? (car x) 'row)))
			    a)
		       ;; convert nested hcat inside vcat to hvcat
		       (let ((rows (map (lambda (x)
					  (if (and (pair? x) (eq? (car x) 'row))
					      (cdr x)
					      (list x)))
					a)))
			 `(call (top hvcat)
				(tuple ,@(map length rows))
				,@(apply nconc rows)))
		       `(call (top vcat) ,@a)))

   (pattern-lambda (typed_hcat t . a)
                   (let ((result (gensy))
                         (ncols (length a)))
                     `(block
                       (if (call (top !) (call (top isa) ,t Type))
                           (call error "invalid array index"))
                       (= ,result (call (top Array) ,t 1 ,ncols))
                       ,@(map (lambda (x i) `(call (top setindex!) ,result ,x ,i))
                              a (cdr (iota (+ ncols 1))))
                       ,result)))

   (pattern-lambda (typed_vcat t . rows)
     (if (any (lambda (x) (not (and (pair? x) (eq? 'row (car x))))) rows)
         (error "invalid array literal")
         (let ((result (gensy))
               (nrows (length rows))
               (ncols (length (cdar rows))))
           (if (any (lambda (x) (not (= (length (cdr x)) ncols))) rows)
               (error "invalid array literal")
               `(block
                 (if (call (top !) (call (top isa) ,t Type))
                     (call error "invalid array index"))
                 (= ,result (call (top Array) ,t ,nrows ,ncols))
                 ,@(apply nconc
                     (map
                       (lambda (row i)
                         (map
                           (lambda (x j) `(call (top setindex!) ,result ,x ,i ,j))
                           (cdr row)
                           (cdr (iota (+ ncols 1)))))
                       rows
                       (cdr (iota (+ nrows 1)))))
                 ,result)))))

   ;; transpose operator
   (pattern-lambda (|'| a) `(call ctranspose ,a))
   (pattern-lambda (|.'| a) `(call transpose ,a))

   ;; transposed multiply
   (pattern-lambda (call (-/ *) (|'| a) (|'| b))   `(call Ac_mul_Bc ,a ,b))
   (pattern-lambda (call (-/ *) (|.'| a) (|.'| b)) `(call At_mul_Bt ,a ,b))
   (pattern-lambda (call (-/ *) (|'| a) b)  `(call Ac_mul_B ,a ,b))
   (pattern-lambda (call (-/ *) a (|'| b))  `(call A_mul_Bc ,a ,b))
   (pattern-lambda (call (-/ *) (|.'| a) b) `(call At_mul_B ,a ,b))
   (pattern-lambda (call (-/ *) a (|.'| b)) `(call A_mul_Bt ,a ,b))

   ;; transposed divide
   (pattern-lambda (call (-/ /) (|'| a) (|'| b))   `(call Ac_rdiv_Bc ,a ,b))
   (pattern-lambda (call (-/ /) (|.'| a) (|.'| b)) `(call At_rdiv_Bt ,a ,b))
   (pattern-lambda (call (-/ /) (|'| a) b)  `(call Ac_rdiv_B ,a ,b))
   (pattern-lambda (call (-/ /) a (|'| b))  `(call A_rdiv_Bc ,a ,b))
   (pattern-lambda (call (-/ /) (|.'| a) b) `(call At_rdiv_B ,a ,b))
   (pattern-lambda (call (-/ /) a (|.'| b)) `(call A_rdiv_Bt ,a ,b))

   (pattern-lambda (call (-/ \\) (|'| a) (|'| b))   `(call Ac_ldiv_Bc ,a ,b))
   (pattern-lambda (call (-/ \\) (|.'| a) (|.'| b)) `(call At_ldiv_Bt ,a ,b))
   (pattern-lambda (call (-/ \\) (|'| a) b)  `(call Ac_ldiv_B ,a ,b))
   (pattern-lambda (call (-/ \\) a (|'| b))  `(call A_ldiv_Bc ,a ,b))
   (pattern-lambda (call (-/ \\) (|.'| a) b) `(call At_ldiv_B ,a ,b))
   (pattern-lambda (call (-/ \\) a (|.'| b)) `(call A_ldiv_Bt ,a ,b))

   (pattern-lambda (ccall name RT argtypes . args)
		   (begin
		     (if (not (and (pair? argtypes)
				   (eq? (car argtypes) 'tuple)))
			 (error "ccall argument types must be a tuple; try (T,)"))
		     (lower-ccall name RT (cdr argtypes) args)))

   ;; comprehensions
   (pattern-lambda
    (comprehension expr . ranges)
    (if (any (lambda (x) (eq? x ':)) ranges)
	(lower-nd-comprehension '() expr ranges)
    (let ((result    (gensy))
	  (ri        (gensy))
	  (initlabl  (gensy))
	  (oneresult (gensy))
	  (rv        (map (lambda (x) (gensy)) ranges)))

      ;; compute the dimensions of the result
      (define (compute-dims ranges)
	(map (lambda (r) `(call (top length) ,(caddr r)))
	     ranges))

      ;; construct loops to cycle over all dimensions of an n-d comprehension
      (define (construct-loops ranges)
	(if (null? ranges)
	    `(block (= ,oneresult ,expr)
		    (type_goto ,initlabl)
		    (call (top setindex!) ,result ,oneresult ,ri)
		    (+= ,ri 1))
	    `(for ,(car ranges)
		  ,(construct-loops (cdr ranges)))))

      ;; Evaluate the comprehension
      (let ((loopranges
	     (map (lambda (r v) `(= ,(cadr r) ,v)) ranges rv)))
	`(block
	  ,@(map (lambda (v r) `(= ,v ,(caddr r))) rv ranges)
	  (scope-block
	   (block
	   (local ,oneresult)
	   ,@(map (lambda (r) `(local ,r))
		  (apply append (map (lambda (r) (lhs-vars (cadr r))) ranges)))
	   (label ,initlabl)
	   (= ,result (call (top Array)
			    (static_typeof ,oneresult)
			    ,@(compute-dims loopranges)))
	   (= ,ri 1)
	   ,(construct-loops (reverse loopranges))
	   ,result)))))))

   ;; typed array comprehensions
   (pattern-lambda
    (typed_comprehension atype expr . ranges)
    (if (any (lambda (x) (eq? x ':)) ranges)
	(lower-nd-comprehension atype expr ranges)
    (let ( (result (gensy))
	   (ri (gensy))
	   (rs (map (lambda (x) (gensy)) ranges)) )

      ;; compute the dimensions of the result
      (define (compute-dims ranges)
	(map (lambda (r) `(call (top length) ,r))
	     ranges))

      ;; construct loops to cycle over all dimensions of an n-d comprehension
      (define (construct-loops ranges rs)
	(if (null? ranges)
	    `(block (call (top setindex!) ,result ,expr ,ri)
		    (+= ,ri 1))
	    `(for (= ,(cadr (car ranges)) ,(car rs))
		  ,(construct-loops (cdr ranges) (cdr rs)))))

      ;; Evaluate the comprehension
      `(block
	,@(map make-assignment rs (map caddr ranges))
	(local ,result)
	(= ,result (call (top Array) ,atype ,@(compute-dims rs)))
	(scope-block
	(block
	 ,@(map (lambda (r) `(local ,r))
		(apply append (map (lambda (r) (lhs-vars (cadr r))) ranges)))
	 (= ,ri 1)
	 ,(construct-loops (reverse ranges) (reverse rs))
	 ,result))))))

   ;; dict comprehensions
   (pattern-lambda
    (dict_comprehension expr . ranges)
    (if (any (lambda (x) (eq? x ':)) ranges)
	(error "invalid iteration syntax")
    (let ((result   (gensy))
	  (initlabl (gensy))
	  (onekey   (gensy))
	  (oneval   (gensy))
	  (rv         (map (lambda (x) (gensy)) ranges)))

      ;; construct loops to cycle over all dimensions of an n-d comprehension
      (define (construct-loops ranges)
	(if (null? ranges)
	    `(block (= ,onekey ,(cadr expr))
		    (= ,oneval ,(caddr expr))
		    (type_goto ,initlabl)
		    (call (top setindex!) ,result ,oneval ,onekey))
	    `(for ,(car ranges)
		  ,(construct-loops (cdr ranges)))))

      ;; Evaluate the comprehension
      (let ((loopranges
	     (map (lambda (r v) `(= ,(cadr r) ,v)) ranges rv)))
	`(block
	  ,@(map (lambda (v r) `(= ,v ,(caddr r))) rv ranges)
	  (scope-block
	   (block
	   (local ,onekey)
	   (local ,oneval)
	   ,@(map (lambda (r) `(local ,r))
		  (apply append (map (lambda (r) (lhs-vars (cadr r))) ranges)))
	   (label ,initlabl)
	   (= ,result (call (curly (top Dict)
				   (static_typeof ,onekey)
				   (static_typeof ,oneval))))
	   ,(construct-loops (reverse loopranges))
	   ,result)))))))

   ;; typed dict comprehensions
   (pattern-lambda
    (typed_dict_comprehension atypes expr . ranges)
    (if (any (lambda (x) (eq? x ':)) ranges)
	(error "invalid iteration syntax")
    (if (not (and (length= atypes 3)
		  (eq? (car atypes) '=>)))
	(error "invalid typed_dict_comprehension syntax")
    (let ( (result (gensy))
	   (rs (map (lambda (x) (gensy)) ranges)) )

      ;; construct loops to cycle over all dimensions of an n-d comprehension
      (define (construct-loops ranges rs)
	(if (null? ranges)
	    `(call (top setindex!) ,result ,(caddr expr) ,(cadr expr))
	    `(for (= ,(cadr (car ranges)) ,(car rs))
		  ,(construct-loops (cdr ranges) (cdr rs)))))

      ;; Evaluate the comprehension
      `(block
	,@(map make-assignment rs (map caddr ranges))
	(local ,result)
	(= ,result (call (curly (top Dict) ,(cadr atypes) ,(caddr atypes))))
	(scope-block
	(block
	 ,@(map (lambda (r) `(local ,r))
		(apply append (map (lambda (r) (lhs-vars (cadr r))) ranges)))
	 ,(construct-loops (reverse ranges) (reverse rs))
	 ,result)))))))

   )) ; patterns

(define (lower-nd-comprehension atype expr ranges)
  (let ((result    (gensy))
	(ri        (gensy))
	(oneresult (gensy)))
    ;; evaluate one expression to figure out type and size
    ;; compute just one value by inserting a break inside loops
    (define (evaluate-one ranges)
      (if (null? ranges)
	  `(= ,oneresult ,expr)
	  (if (eq? (car ranges) `:)
	      (evaluate-one (cdr ranges))
	      `(for ,(car ranges)
		    (block ,(evaluate-one (cdr ranges))
			   (break)) ))))

    ;; compute the dimensions of the result
    (define (compute-dims ranges oneresult-dim)
      (if (null? ranges)
	  (list)
	  (if (eq? (car ranges) `:)
	      (cons `(call (top size) ,oneresult ,oneresult-dim)
		    (compute-dims (cdr ranges) (+ oneresult-dim 1)))
	      (cons `(call (top length) ,(caddr (car ranges)))
		    (compute-dims (cdr ranges) oneresult-dim)) )))

    ;; construct loops to cycle over all dimensions of an n-d comprehension
    (define (construct-loops ranges iters oneresult-dim)
      (if (null? ranges)
	  (if (null? iters)
	      `(block (call (top setindex!) ,result ,expr ,ri)
		      (+= ,ri 1))
	      `(block (call (top setindex!) ,result (ref ,expr ,@(reverse iters)) ,ri)
		      (+= ,ri 1)) )
	  (if (eq? (car ranges) `:)
	      (let ((i (gensy)))
		`(for (= ,i (: 1 (call (top size) ,oneresult ,oneresult-dim)))
		      ,(construct-loops (cdr ranges) (cons i iters) (+ oneresult-dim 1)) ))
	      `(for ,(car ranges)
		    ,(construct-loops (cdr ranges) iters oneresult-dim) ))))

    (define (get-eltype)
      (if (null? atype)
	`((call (top eltype) ,oneresult))
	`(,atype)))

    ;; Evaluate the comprehension
    `(scope-block
      (block
       (= ,oneresult (tuple))
       ,(evaluate-one ranges)
       (= ,result (call (top Array) ,@(get-eltype)
			,@(compute-dims ranges 1)))
       (= ,ri 1)
       ,(construct-loops (reverse ranges) (list) 1)
       ,result ))))

(define (lhs-vars e)
  (cond ((symbol? e) (list e))
	((and (pair? e) (eq? (car e) 'tuple))
	 (apply append (map lhs-vars (cdr e))))
	(else '())))

; (op (op a b) c) => (a b c) etc.
(define (flatten-op op e)
  (if (not (pair? e)) e
      (apply append
	     (map (lambda (x)
		    (if (and (pair? x) (eq? (car x) op))
			(flatten-op op x)
			(list x)))
		  (cdr e)))))

(define (expand-and e)
  (let ((e (flatten-op '&& e)))
    (let loop ((tail e))
      (if (null? tail)
	  'true
	  (if (null? (cdr tail))
	      (car tail)
	      `(if ,(car tail)
		   ,(loop (cdr tail))
		   false))))))

(define (expand-or e)
  (let ((e (flatten-op '|\|\|| e)))
    (let loop ((tail e))
      (if (null? tail)
	  'false
	  (if (null? (cdr tail))
	      (car tail)
	      (if (symbol? (car tail))
		  `(if ,(car tail) ,(car tail)
		       ,(loop (cdr tail)))
		  (let ((g (gensy)))
		    `(block (= ,g ,(car tail))
			    (if ,g ,g
				,(loop (cdr tail)))))))))))

;; in "return x()" inside a try block, "x()" is not really in tail position
;; since we need to pop the exception handler first. convert these cases
;; to "tmp = x(); return tmp"
(define (fix-try-block-returns e)
  (cond ((or (atom? e) (quoted? e))  e)
	((and (eq? (car e) 'return) (or (symbol? (cadr e)) (pair? (cadr e))))
	 (let ((sym (gensy)))
	   `(block (local! ,sym)
		   (= ,sym ,(cadr e))
		   (return ,sym))))
	((eq? (car e) 'lambda) e)
	(else
	 (cons (car e)
	       (map fix-try-block-returns (cdr e))))))

; conversion to "linear flow form"
;
; This pass removes control flow constructs from value position.
; A "control flow construct" is anything that would require a branch.
;  (block ... (value-expr ... control-expr ...) ...) =>
;  (block ... (= var control-expr) (value-expr ... var ...) ...)
; except the assignment is incorporated into control-expr, so that
; control exprs only occur in statement position.
;
; The conversion works by passing around the intended destination of
; the value being computed: #f for statement position, #t for value position,
; or a symbol if the value needs to be assigned to a particular variable.
; This is the "dest" argument to to-lff.
;
; This also keeps track of tail position, and converts the code so that
; everything in tail position is returned explicitly.
;
; The result is that every expression whose value is needed is either
; a function argument, an assignment RHS, or returned explicitly.
; In this form, expressions can be analyzed freely without fear of
; intervening branches. Similarly, control flow can be analyzed without
; worrying about implicit value locations (the "evaluation stack").
(define *lff-line* 0)
(define (to-LFF e)
  (set! *lff-line* 0)
  (with-exception-catcher
   (lambda (e)
     (if (and (> *lff-line* 0) (pair? e) (eq? (car e) 'error))
	 (let ((msg (cadr e)))
	   (raise `(error ,(string msg " at line " *lff-line*))))
	 (raise e)))
   (lambda () (to-LFF- e))))

(define (to-LFF- e)
  (define (to-blk r)
    (if (length= r 1)
	(car r)
	(cons 'block (reverse r))))
  (define (blk-tail r)
    (reverse r))
  ; to-lff returns (new-ex . stmts) where stmts is a list of statements that
  ; must run before new-ex is valid.
  ;
  ; If the input expression needed to be removed from its original context,
  ; like the 'if' in "1+if(a,b,c)", then new-ex is a symbol holding the
  ; result of the expression.
  ;
  ; If dest is a symbol or #f, new-ex can be a statement.
  ;
  ; We essentially maintain a stack of control-flow constructs that need to be
  ; run in statement position as we walk around an expression. If we hit
  ; statement context, we can dump the control-flow stuff there.
  ; This expression walk is entirely within the "else" clause of the giant
  ; case expression. Everything else deals with special forms.
  (define (to-lff e dest tail)
    (if (or (not (pair? e)) (memq (car e) '(quote top))
	    (equal? e '(null)))
	(cond ((symbol? dest) (cons `(= ,dest ,e) '()))
	      (dest (cons (if tail `(return ,e) e)
			  '()))
	      (else (cons e '())))

	(case (car e)
	  ((=)
	   (if (or (not (symbol? (cadr e)))
		   (eq? (cadr e) 'true)
		   (eq? (cadr e) 'false))
	       (error (string "invalid assignment location " (cadr e)))
	       (let ((r (to-lff (caddr e) (cadr e) #f)))
		 (cond ((symbol? dest)
			(cons `(block ,(car r)
				      (= ,dest ,(cadr e)))
			      (cdr r)))
		       (dest
			(cons (if tail `(return ,(cadr e)) (cadr e)) r))
		       (else r)))))

	  ((if)
	   (cond ((or tail (eq? dest #f) (symbol? dest))
		  (let ((r (to-lff (cadr e) #t #f)))
		    (cons `(if
			    ,(car r)
			    ,(to-blk (to-lff (caddr e) dest tail))
			    ,(if (length= e 4)
				 (to-blk (to-lff (cadddr e) dest tail))
				 (to-blk (to-lff '(null)  dest tail))))
			  (cdr r))))
		 (else (let ((g (gensy)))
			 (cons g
			       (cons `(local! ,g) (to-lff e g #f)))))))

	  ((line)
	   (set! *lff-line* (cadr e))
	   (cons e '()))

	  ((trycatch)
	   (cond ((and (eq? dest #t) (not tail))
		  (let ((g (gensy)))
		    (cons g
			  (cons `(local! ,g) (to-lff e g #f)))))
		 (else
		  (cons `(trycatch ,(fix-try-block-returns
				     (to-blk (to-lff (cadr e) dest tail)))
				   ,(to-blk (to-lff (caddr e) dest tail)))
			()))))

	  ((&&)
	   (to-lff (expand-and e) dest tail))
	  ((|\|\||)
	   (to-lff (expand-or e) dest tail))

	  ((block)
	   (if (length= e 2)
	       (to-lff (cadr e) dest tail)
	       (let* ((g (gensy))
		      (stmts
		       (let loop ((tl (cdr e)))
			 (if (null? tl) '()
			     (if (null? (cdr tl))
				 (cond ((or tail (eq? dest #f) (symbol? dest))
					(blk-tail (to-lff (car tl) dest tail)))
				       (else
					(blk-tail (to-lff (car tl) g tail))))
				 (cons (to-blk (to-lff (car tl) #f #f))
				       (loop (cdr tl))))))))
		 (if (and (eq? dest #t) (not tail))
		     (cons g (reverse stmts))
		     (if (and tail (null? stmts))
			 (cons '(return (null))
			       '())
			 (cons (cons 'block stmts)
			       '()))))))

	  ((return)
	   (if (and dest (not tail))
	       (error "misplaced return statement")
	       (to-lff (cadr e) #t #t)))

	  ((_while) (cond ((eq? dest #t)
			   (cons (if tail '(return (null)) '(null))
				 (to-lff e #f #f)))
			  (else
			   (let* ((r (to-lff (cadr e) #t #f))
				  (w (cons `(_while ,(car r)
					      ,(to-blk
						(append
						 (cdr r)
						 (to-lff (caddr e) #f #f))))
					   (cdr r))))
			     (if (symbol? dest)
				 (cons `(= ,dest (null)) w)
				 w)))))

	  ((break-block)
	   (let ((r (to-lff (caddr e) dest tail)))
	     (if dest
		 (cons (car r)
		       (list `(break-block ,(cadr e) ,(to-blk (cdr r)))))
		 (cons `(break-block ,(cadr e) ,(car r))
		       (cdr r)))))

	  ((scope-block)
	   (if (and dest (not tail))
	       (let* ((g (gensy))
		      (r (to-lff (cadr e) g tail)))
		 (cons (car (to-lff g dest tail))
		       ; tricky: need to introduce a new local outside the
		       ; scope-block so the scope-block's value can propagate
		       ; out. otherwise the value could be inaccessible due
		       ; to being wrapped inside a scope.
		       `((scope-block ,(to-blk r))
			 (local! ,g))))
	       (let ((r (to-lff (cadr e) dest tail)))
		 (cons `(scope-block ,(to-blk r))
		       '()))))

	  ;; move the break to the list of preceding statements. value is
	  ;; null but this will never be observed.
	  ((break) (cons '(null) (list e)))

	  ((lambda)
	   (let ((l `(lambda ,(cadr e)
		       ,(to-blk (to-lff (caddr e) #t #t)))))
	     (if (symbol? dest)
		 (cons `(= ,dest ,l) '())
		 (cons (if tail `(return ,l) l) '()))))

	  ((local global)
	   (if (symbol? dest)
	       (error (string "misplaced " (car e) " declaration")))
	   (cons (to-blk (to-lff '(null) dest tail))
		 (list e)))

	  ((|::|)
	   (if dest
	       ;; convert to typeassert or decl based on whether it's in
	       ;; value or statement position.
	       (to-lff `(typeassert ,@(cdr e)) dest tail)
	       (to-lff `(decl ,@(cdr e)) dest tail)))

	  ((unnecessary-tuple)
	   (if dest
	       (to-lff (cadr e) dest tail)
	       ;; remove if not in value position
	       (to-lff '(null) dest tail)))

	  (else
	   (if (and dest (not tail) (eq? (car e) 'method))
	       (error (string "misplaced method definition for " (cadr e))))
	   (let ((r (map (lambda (arg) (to-lff arg #t #f))
			 (cdr e))))
	     (cond ((symbol? dest)
		    (cons `(= ,dest ,(cons (car e) (map car r)))
			  (apply append (map cdr r))))
		   (else
		    (let ((ex (cons (car e) (map car r))))
		      (cons (if tail `(return ,ex) ex)
			    (apply append (map cdr r)))))))))))
  (to-blk (to-lff e #t #t)))
#|
future issue:
right now scope blocks need to be inside functions:

> (julia-expand '(block (call + 1 (scope-block (block (= a b) c)))))
(block (scope-block (local a) (local #:g13) (block (= a b) (= #:g13 c)))
       (return (call + 1 #:g13)))

> (julia-expand '(scope-block (call + 1 (scope-block (block (= a b) c)))))
(scope-block
 (local #:g15)
 (block (scope-block (local a) (block (= a b) (= #:g15 c)))
	(return (call + 1 #:g15))))

The first one gave something broken, but the second case works.
So far only the second case can actually occur.
|#

(define (declared-global-vars e)
  (if (or (not (pair? e)) (quoted? e))
      '()
      (case (car e)
	((lambda scope-block)  '())
	((global)  (cdr e))
	(else
	 (apply append (map declared-global-vars e))))))

(define (check-dups locals)
  (if (and (pair? locals) (pair? (cdr locals)))
      (or (and (memq (car locals) (cdr locals))
	       (error (string "local " (car locals) " declared twice")))
	  (check-dups (cdr locals))))
  locals)

(define (find-assigned-vars e env)
  (if (or (not (pair? e)) (quoted? e))
      '()
      (case (car e)
	((lambda scope-block)  '())
	((= method)
	 (let ((v (decl-var (cadr e))))
	   (if (memq v env)
	       '()
	       (list v))))
	(else
	 (apply append! (map (lambda (x) (find-assigned-vars x env))
			     e))))))

(define (find-local-decls e env)
  (if (or (not (pair? e)) (quoted? e))
      '()
      (case (car e)
	((lambda scope-block)  '())
	((local)  (list (decl-var (cadr e))))
	(else
	 (apply append! (map (lambda (x) (find-local-decls x env))
			     e))))))

(define (find-local!-decls e env)
  (if (or (not (pair? e)) (quoted? e))
      '()
      (case (car e)
	((lambda scope-block)  '())
	((local!)  (list (decl-var (cadr e))))
	(else
	 (apply append! (map (lambda (x) (find-local!-decls x env))
			     e))))))

(define (find-locals e env)
  (delete-duplicates
   (append! (check-dups (find-local-decls e env))
	    (find-local!-decls e env)
	    (find-assigned-vars e env))))

;; local variable identification
;; convert (scope-block x) to `(scope-block ,@locals ,x)
;; where locals is a list of (local x) expressions, derived from two sources:
;; 1. (local x) expressions inside this scope-block and lambda
;; 2. variables assigned inside this scope-block that don't exist in outer
;;    scopes
(define (add-local-decls e env)
  (if (or (not (pair? e)) (quoted? e)) e
      (cond ((eq? (car e) 'lambda)
	     (let* ((env (append (lam:vars e) env))
		    (body (add-local-decls (caddr e) env)))
	       (list 'lambda (cadr e) body)))

	    ((eq? (car e) 'scope-block)
	     (let* ((glob (declared-global-vars (cadr e)))
		    (vars (find-locals
			   ;; being declared global prevents a variable
			   ;; assignment from introducing a local
			   (cadr e) (append env glob)))
		    (body (add-local-decls (cadr e) (append vars glob env))))
	       `(scope-block ,@(map (lambda (v) `(local ,v))
				    vars)
			     ,body)))
	    (else
	     ;; form (local! x) adds a local to a normal (non-scope) block
	     (let ((newenv (append (declared-local!-vars e) env)))
	       (map (lambda (x)
		      (add-local-decls x newenv))
		    e))))))

(define (identify-locals e) (add-local-decls e '()))

(define (declared-local-vars e)
  (map (lambda (x) (decl-var (cadr x)))
       (filter (lambda (x)
		 (and (pair? x)
		      (or (eq? (car x) 'local)
			  (eq? (car x) 'local!))))
	       (cdr e))))
(define (declared-local!-vars e)
  (map cadr
       (filter (lambda (x)
		 (and (pair? x)
		      (eq? (car x) 'local!)))
	       (cdr e))))

(define (without alst remove)
  (cond ((null? alst)               '())
	((null? remove)             alst)
	((memq (caar alst) remove)  (without (cdr alst) remove))
	(else                       (cons (car alst)
					  (without (cdr alst) remove)))))

; e - expression
; renames - assoc list of (oldname . newname)
; this works on any tree format after identify-locals
(define (rename-vars e renames)
  (cond ((null? renames)  e)
	((symbol? e)      (lookup e renames e))
	((not (pair? e))  e)
	((quoted? e)      e)
	(else
	 (let (; remove vars bound by current expr from rename list
	       (new-renames
		(without renames
			 (case (car e)
			   ((lambda)
			    (append (lambda-all-vars e)
				    (declared-global-vars (cadddr e))))
			   ((scope-block)
			    (append (declared-local-vars e)
				    (declared-global-vars (cadr e))))
			   (else '())))))
	   (cons (car e)
		 (map (lambda (x)
			(rename-vars x new-renames))
		      (cdr e)))))))

;; all vars used in e outside x
(define (vars-used-outside e x)
  (cond ((symbol? e) (list e))
	((or (atom? e) (quoted? e)) '())
	((eq? e x) '())
	((eq? (car e) 'lambda)
	 (diff (free-vars (lam:body e))
	       (lambda-all-vars e)))
	(else (unique
	       (apply nconc
		      (map (lambda (e) (vars-used-outside e x)) (cdr e)))))))

(define (flatten-lambda-scopes e)
  (cond ((or (atom? e) (quoted? e)) e)
	((eq? (car e) 'lambda) (flatten-scopes e))
	(else (map flatten-lambda-scopes e))))

;; remove (scope-block) and (local), convert lambdas to the form
;; (lambda (argname...) (locals var...) body)
(define (flatten-scopes e)
  (define scope-block-vars '())
  (define (remove-scope-blocks e context usedv)
    (cond ((or (atom? e) (quoted? e)) e)
	  ((eq? (car e) 'lambda) e)
	  ((eq? (car e) 'scope-block)
	   (let ((vars (declared-local-vars e))
		 (body (car (last-pair e))))
	     (let* ((outer    (append usedv (vars-used-outside context e)))
		    ;; only rename conflicted vars
		    (to-ren   (filter (lambda (v) (memq v outer)) vars))
		    (newnames (map (lambda (x) (gensy)) to-ren))
		    (bod      (rename-vars (remove-scope-blocks body e outer)
					   (map cons to-ren newnames))))
	       (set! scope-block-vars (nconc newnames scope-block-vars))
	       (set! scope-block-vars (nconc (diff vars to-ren)
					     scope-block-vars))
	       bod)))
	  (else (map (lambda (e) (remove-scope-blocks e context usedv))
		     e))))

  (cond ((not (pair? e))   e)
	((quoted? e)       e)
	((eq? (car e)      'lambda)
	 (let* ((argnames  (lam:vars e))
		(body      (caddr e))
		(body2     (flatten-lambda-scopes body))
		(r-s-b     (remove-scope-blocks body2 body2 argnames)))
	   (for-each (lambda (v)
		       (if (memq v argnames)
			   (error (string "local " v
					  " conflicts with argument"))))
		     (declared-local-vars body))
	   `(lambda ,(cadr e)
	      (locals ,@scope-block-vars)
	      ,r-s-b)))
	(else (map (lambda (x) (if (not (pair? x)) x
				   (flatten-scopes x)))
		   e))))

(define (make-var-info name) (list name 'Any 0))
(define vinfo:name car)
(define vinfo:type cadr)
(define (vinfo:capt v) (< 0 (logand (caddr v) 1)))
(define (vinfo:const v) (< 0 (logand (caddr v) 8)))
(define (vinfo:set-type! v t) (set-car! (cdr v) t))
;; record whether var is captured
(define (vinfo:set-capt! v c) (set-car! (cddr v)
					(if c
					    (logior (caddr v) 1)
					    (logand (caddr v) -2))))
;; whether var is assigned
(define (vinfo:set-asgn! v a) (set-car! (cddr v)
					(if a
					    (logior (caddr v) 2)
					    (logand (caddr v) -3))))
;; whether var is assigned by an inner function
(define (vinfo:set-iasg! v a) (set-car! (cddr v)
					(if a
					    (logior (caddr v) 4)
					    (logand (caddr v) -5))))
;; whether var is const
(define (vinfo:set-const! v a) (set-car! (cddr v)
					 (if a
					     (logior (caddr v) 8)
					     (logand (caddr v) -9))))

(define var-info-for assq)

(define (lambda-all-vars e)
  (append (lam:vars e)
	  (cdr (caddr e))))

(define (free-vars e)
  (cond ((symbol? e) (list e))
	((or (atom? e) (quoted? e)) '())
	((eq? (car e) 'lambda)
	 (diff (free-vars (lam:body e))
	       (lambda-all-vars e)))
	(else (unique (apply nconc (map free-vars (cdr e)))))))

; convert each lambda's (locals ...) to
;   ((localvars...) var-info-lst captured-var-infos)
; where var-info-lst is a list of var-info records
(define (analyze-vars e env captvars)
  (cond ((or (atom? e) (quoted? e)) e)
	((eq? (car e) '=)
	 (let ((vi (var-info-for (cadr e) env)))
	   (if vi
	       (begin
		 (vinfo:set-asgn! vi #t)
		 (if (assq (car vi) captvars)
		     (vinfo:set-iasg! vi #t)))))
	 `(= ,(cadr e) ,(analyze-vars (caddr e) env captvars)))
	((or (eq? (car e) 'local) (eq? (car e) 'local!))
	 '(null))
	((eq? (car e) 'typeassert)
	 ;(let ((vi (var-info-for (cadr e) env)))
	 ;  (if vi
	 ;      (begin (vinfo:set-type! vi (caddr e))
	 ;	     (cadr e))
	 `(call (top typeassert) ,(cadr e) ,(caddr e)))
	((or (eq? (car e) 'decl) (eq? (car e) '|::|))
	 ; handle var::T declaration by storing the type in the var-info
	 ; record. for non-symbols or globals, emit a type assertion.
	 (let ((vi (var-info-for (cadr e) env)))
	   (if vi
	       (begin (if (not (eq? (vinfo:type vi) 'Any))
			  (error (string "multiple type declarations for "
					 (cadr e))))
		      (if (assq (cadr e) captvars)
			  (error (string "type of " (cadr e)
					 " declared in inner scope")))
		      (vinfo:set-type! vi (caddr e))
		      '(null))
	       `(call (top typeassert) ,(cadr e) ,(caddr e)))))
	((eq? (car e) 'lambda)
	 (letrec ((args (lam:args e))
		  (locl (cdr (caddr e)))
		  (allv (nconc (map arg-name args) locl))
		  (fv   (diff (free-vars (lam:body e)) allv))
		  (glo  (declared-global-vars (lam:body e)))
		  ; make var-info records for vars introduced by this lambda
		  (vi   (nconc
			 (map (lambda (decl) (make-var-info (decl-var decl)))
			      args)
			 (map make-var-info locl)))
		  ; captured vars: vars from the environment that occur
		  ; in our set of free variables (fv).
		  (cv    (filter (lambda (v) (and (memq (vinfo:name v) fv)
						  (not (memq
							(vinfo:name v) glo))))
				 env))
		  (bod   (analyze-vars
			  (lam:body e)
			  (append vi
				  ; new environment: add our vars
				  (filter (lambda (v)
					    (and
					     (not (memq (vinfo:name v) allv))
					     (not (memq (vinfo:name v) glo))))
					  env))
			  cv)))
	   ; mark all the vars we capture as captured
	   (for-each (lambda (v) (vinfo:set-capt! v #t))
		     cv)
	   `(lambda ,args
	      (,(cdaddr e) ,vi ,cv)
	      ,bod)))
	((eq? (car e) 'localize)
	 ;; special feature for @spawn that wraps a piece of code in a "let"
	 ;; binding each free variable.
	 (let ((env-vars (map vinfo:name env))
	       (localize-vars (cddr e)))
	   (let ((vs (filter
		      (lambda (v) (or (memq v localize-vars)
				      (memq v env-vars)))
		      (free-vars (cadr e)))))
	     (analyze-vars
	      `(call (lambda ,vs ,(caddr (cadr e)) ,(cadddr (cadr e)))
		     ,@vs)
	      env captvars))))
	((eq? (car e) 'method)
	 (let ((vi (var-info-for (cadr e) env)))
	   (if vi
	       (begin
		 (vinfo:set-asgn! vi #t)
		 (if (assq (car vi) captvars)
		     (vinfo:set-iasg! vi #t)))))
	 `(method ,(cadr e)
		  ,(analyze-vars (caddr  e) env captvars)
		  ,(analyze-vars (cadddr e) env captvars)
		  ,(cadddr (cdr e))))
	(else (cons (car e)
		    (map (lambda (x) (analyze-vars x env captvars))
			 (cdr e))))))

(define (analyze-variables e) (analyze-vars e '() '()))

; remove if, _while, block, break-block, and break
; replaced with goto and gotoifnot
; TODO: remove type-assignment-affecting expressions from conditional branch.
;       needed because there's no program location after the condition
;       is evaluated but before the branch's successors.
;       pulling a complex condition out to a temporary variable creates
;       such a location (the assignment to the variable).
(define (goto-form e)
  (let ((code '())
	(ip   0)
	(label-counter 0)
	(label-map '())
	(handler-level 0))
    (define (emit c)
      (set! code (cons c code))
      (set! ip (+ ip 1)))
    (define (make-label)
      (begin0 label-counter
	      (set! label-counter (+ 1 label-counter))))
    (define (mark-label l) (emit `(label ,l)))
    (define (make&mark-label)
      (if (and (pair? code) (pair? (car code)) (eq? (caar code) 'label))
	  ;; use current label if there is one
	  (cadr (car code))
	  (let ((l (make-label)))
	    (mark-label l)
	    l)))
    (define (compile e break-labels vi)
      (if (or (not (pair? e)) (equal? e '(null)))
	  ; atom has no effect, but keep symbols for undefined-var checking
	  #f #;(if (symbol? e) (emit e) #f)
	  (case (car e)
	    ((call)  (emit (goto-form e)))
	    ((=)     (let ((vt (vinfo:type
				(or (var-info-for (cadr e) vi) '(#f Any)))))
		       (if (not (eq? vt 'Any))
			   (emit `(= ,(cadr e) (call (top convert) ,vt
						     ,(goto-form (caddr e)))))
			   (emit `(= ,(cadr e) ,(goto-form (caddr e)))))))
	    ((if) (let ((test     `(gotoifnot ,(goto-form (cadr e)) _))
			(end-jump `(goto _))
			(tail     (and (pair? (caddr e))
				       (eq? (car (caddr e)) 'return))))
		    (emit test)
		    (compile (caddr e) break-labels vi)
		    (if (and (not tail)
			     (not (equal? (cadddr e) '(null))))
			(emit end-jump))
		    (set-car! (cddr test) (make&mark-label))
		    (compile (cadddr e) break-labels vi)
		    (if (not tail)
			(set-car! (cdr end-jump) (make&mark-label)))))
	    ((block) (for-each (lambda (x) (compile x break-labels vi))
			       (cdr e)))
	    ((_while) (let ((topl (make&mark-label))
			    (endl (make-label)))
			(emit `(gotoifnot ,(goto-form (cadr e)) ,endl))
			(compile (caddr e) break-labels vi)
			(emit `(goto ,topl))
			(mark-label endl)))
	    ((break-block) (let ((endl (make-label)))
			     (compile (caddr e)
				      (cons (list (cadr e) endl handler-level)
					    break-labels)
				      vi)
			     (mark-label endl)))
	    ((break) (let ((labl (assq (cadr e) break-labels)))
		       (if (not labl)
			   (error "break or continue outside loop")
			   (begin
			     (if (> handler-level (caddr labl))
				 (emit `(leave
					 ,(- handler-level (caddr labl)))))
			     (emit `(goto ,(cadr labl)))))))
	    ((return) (begin
			(if (> handler-level 0)
			    (emit `(leave ,handler-level)))
			(emit (goto-form e))))
	    ((label) (let ((m (assq (cadr e) label-map)))
		       (if m
			   (emit `(label ,(cdr m)))
			   (let ((l (make&mark-label)))
			     (set! label-map
				   (cons (cons (cadr e) l) label-map))))))
	    ((type_goto) (let ((m (assq (cadr e) label-map)))
			   (if m
			       (emit `(type_goto ,(cdr m)))
			       (let ((l (make-label)))
				 (set! label-map
				       (cons (cons (cadr e) l) label-map))
				 (emit `(type_goto ,l))))))
	    ;; exception handlers are lowered using
	    ;; (enter L) - push handler with catch block at label L
	    ;; (leave n) - pop N exception handlers
	    ;; (the_exception) - get the thrown object
	    ((trycatch)
	     (let ((catch (make-label))
		   (endl  (make-label)))
	       (emit `(enter ,catch))
	       (set! handler-level (+ handler-level 1))
	       (compile (cadr e) break-labels vi)
	       (set! handler-level (- handler-level 1))
	       (if (not (and (pair? (car code)) (eq? (caar code) 'return)))
		   ;; try ends in return, no need to handle flow off end of it
		   (begin (emit `(leave 1))
			  (emit `(goto ,endl)))
		   (set! endl #f))
	       (mark-label catch)
	       (emit `(leave 1))
	       (compile (caddr e) break-labels vi)
	       (if endl
		   (mark-label endl))
	       ))

	    ((global) #f)  ; remove global declarations
	    (else  (emit (goto-form e))))))
    (cond ((or (not (pair? e)) (quoted? e)) e)
	  ((eq? (car e) 'lambda)
	   (compile (cadddr e) '() (append (cadr (caddr e))
					   (caddr (caddr e))))
	   `(lambda ,(cadr e) ,(caddr e)
		    ,(cons 'body (reverse! code))))
	  (else (cons (car e)
		      (map goto-form (cdr e)))))))

(define (to-goto-form e)
  (goto-form e))

;; macro expander

(define (expand-backquote e)
  (cond ((or (eq? e 'true) (eq? e 'false))  e)
	((symbol? e)          `(quote ,e))
	((not (pair? e))      e)
	((eq? (car e) '$)     (cadr e))
	((and (eq? (car e) 'quote) (pair? (cadr e)))
	 (expand-backquote (expand-backquote (cadr e))))
	((not (any (lambda (x)
		     (match '($ (tuple (... x))) x))
		   e))
	 `(call (top Expr) ,@(map expand-backquote e)))
	(else
	 (let loop ((p (cdr e)) (q '()))
	   (if (null? p)
	       (let ((forms (reverse q)))
		 `(call (top splicedexpr) ,(expand-backquote (car e))
			(call (top append_any) ,@forms)))
	       ;; look for splice inside backquote, e.g. (a,$(x...),b)
	       (if (match '($ (tuple (... x))) (car p))
		   (loop (cdr p)
			 (cons (cadr (cadadr (car p))) q))
		   (loop (cdr p)
			 (cons `(cell1d ,(expand-backquote (car p)))
			       q))))))))

(define (julia-expand-macros e)
  (cond ((not (pair? e))     e)
	((and (eq? (car e) 'quote) (pair? (cadr e)))
	 ;; backquote is essentially a built-in macro at the moment
	 (julia-expand-macros (expand-backquote (cadr e))))
	((eq? (car e) 'macrocall)
	 ;; expand macro
	 (let ((form
               (apply invoke-julia-macro (cadr e) (cddr e))))
	   (if (not form)
	       (error (string "macro " (cadr e) " not defined")))
	   (if (and (pair? form) (eq? (car form) 'error))
	       (error (cadr form)))
	   (let ((form (car form))
		 (m    (cdr form)))
	     ;; m is the macro's def module, or #f if def env === use env
	     (julia-expand-macros
	      (resolve-expansion-vars form m)))))
	(else
	 (map julia-expand-macros e))))

(define (pair-with-gensyms v)
  (map (lambda (s) (cons s (named-gensy s))) v))

(define (unescape e)
  (if (and (pair? e) (eq? (car e) 'escape))
      (cadr e)
      e))

(define (resolve-expansion-vars- e env m)
  (cond ((or (eq? e 'true) (eq? e 'false) (eq? e 'end))
	 e)
	((symbol? e)
	 (let ((a (assq e env)))
	   (if a (cdr a)
	       (if m `(|.| ,m (quote ,e))
		   e))))
	((or (not (pair? e)) (quoted? e))
	 e)
	(else
	 (case (car e)
	   ((escape) (cadr e))
	   ((macrocall)
	    `(macrocall ,@(map (lambda (x)
				 (resolve-expansion-vars- x env m))
			       (cdr e))))
	   ((type)
	    `(type ,(cadr e) ,(unescape (caddr e))
		   ;; type has special behavior: identifiers inside are
		   ;; field names, not expressions.
		   ,(map (lambda (x)
			   (cond ((atom? x) x)
				 ((and (pair? x) (eq? (car x) '|::|))
				  `(|::| ,(cadr x)
				    ,(resolve-expansion-vars- (caddr x) env m)))
				 (else
				  (resolve-expansion-vars- x env m))))
			 (cadddr e))))
	   ;; todo: trycatch
	   (else
	    (cons (car e)
		  (map (lambda (x)
			 (resolve-expansion-vars-
			  x
			  (append!
			   (filter (lambda (x)
				     (not (assq (car x) env)))
				   (pair-with-gensyms (vars-introduced-by x)))
			   env)
			  m))
		       (cdr e))))))))

;; decl-var that also identifies f in f()=...
(define (decl-var* e)
  (cond ((not (pair? e))       e)
	((eq? (car e) 'escape) '())
	((eq? (car e) 'call)   (decl-var* (cadr e)))
	((eq? (car e) '=)      (decl-var* (cadr e)))
	((eq? (car e) 'curly)  (decl-var* (cadr e)))
	(else                  (decl-var e))))

(define (find-declared-vars-in-expansion e decl)
  (if (or (not (pair? e)) (quoted? e))
      '()
      (cond ((eq? (car e) 'escape)  '())
	    ((eq? (car e) decl)     (map decl-var* (cdr e)))
	    (else
	     (apply append! (map (lambda (x)
				   (find-declared-vars-in-expansion x decl))
				 e))))))

(define (find-assigned-vars-in-expansion e)
  (if (or (not (pair? e)) (quoted? e))
      '()
      (case (car e)
	((escape)  '())
	((= function)
	 (append! (filter
		   symbol?
		   (if (and (pair? (cadr e)) (eq? (car (cadr e)) 'tuple))
		       (map decl-var* (cdr (cadr e)))
		       (list (decl-var* (cadr e)))))
		  (find-assigned-vars-in-expansion (caddr e))))
	(else
	 (apply append! (map find-assigned-vars-in-expansion e))))))

(define (vars-introduced-by e)
  (let ((v (pattern-expand1 vars-introduced-by-patterns e)))
    (if (and (pair? v) (eq? (car v) 'varlist))
	(cdr v)
	'())))

(define (env-for-expansion e)
  (let ((v (diff (delete-duplicates
		  (append! (find-declared-vars-in-expansion e 'local)
			   (find-assigned-vars-in-expansion e)
			   (vars-introduced-by e)))
		 (find-declared-vars-in-expansion e 'global))))
    (pair-with-gensyms v)))

(define (resolve-expansion-vars e m)
  ;; expand binding form patterns
  ;; keep track of environment, rename locals to gensyms
  ;; and wrap globals in (getfield module var) for macro's home module
  (resolve-expansion-vars- e (env-for-expansion e) m))

;; expander entry point

(define (julia-expand1 ex)
  (to-goto-form
   (analyze-variables
    (flatten-scopes
     (identify-locals ex)))))

(define (julia-expand01 ex)
  (to-LFF
   (pattern-expand patterns
    (pattern-expand binding-form-patterns ex))))

(define (julia-expand0 ex)
  (let ((e (julia-expand-macros ex)))
    (if (and (pair? e) (eq? (car e) 'toplevel))
	`(toplevel ,@(map julia-expand01 (cdr e)))
	(julia-expand01 e))))

(define (julia-expand ex)
  (julia-expand1 (julia-expand0 ex)))
