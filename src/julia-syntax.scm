(define (quoted? e) (memq (car e) '(quote top line break inert)))

(define (lam:args x) (cadr x))
(define (lam:vars x) (llist-vars (lam:args x)))
(define (lam:vinfo x) (caddr x))
(define (lam:body x) (cadddr x))

;; allow (:: T) => (:: #gensym T) in formal argument lists
(define (fill-missing-argname a)
  (if (and (pair? a) (eq? (car a) '|::|) (null? (cddr a)))
      `(|::| ,(gensy) ,(cadr a))
      a))
(define (fix-arglist l)
  (if (any vararg? (butlast l))
      (error "invalid ... on non-final argument"))
  (map (lambda (a)
         (cond ((and (pair? a) (eq? (car a) 'kw))
                `(kw ,(fill-missing-argname (cadr a)) ,(caddr a)))
               ((and (pair? a) (eq? (car a) '...))
                `(... ,(fill-missing-argname (cadr a))))
               (else
                (fill-missing-argname a))))
       l))

(define (deparse-arglist l (sep ",")) (string.join (map deparse l) sep))

(define (deparse e)
  (cond ((or (symbol? e) (number? e)) (string e))
        ((eq? e #t) "true")
        ((eq? e #f) "false")
        ((eq? (typeof e) 'julia_value)
         (let ((s (string e)))
           (if (string.find s "#<julia: ")
               ;; successfully printed as a julia value
               (string.sub s 9 (string.dec s (length s)))
               s)))
        ((atom? e) (string e))
        ((eq? (car e) '|.|)
         (string (deparse (cadr e)) '|.|
                 (if (and (pair? (caddr e)) (memq (caaddr e) '(quote inert)))
                     (deparse (cadr (caddr e)))
                     (string #\( (deparse (caddr e)) #\)))))
        ((memq (car e) '(... |'| |.'|))
         (string (deparse (cadr e)) (car e)))
        ((syntactic-op? (car e))
         (string (deparse (cadr e)) (car e) (deparse (caddr e))))
        ((memq (car e) '($ &))
         (string (car e) (deparse (cadr e))))
        ((eq? (car e) '|::|)
         (if (length> e 2)
             (string (deparse (cadr e)) (car e) (deparse (caddr e)))
             (string (car e) (deparse (cadr e)))))
        (else (case (car e)
                ((tuple)
                 (string #\( (deparse-arglist (cdr e))
                         (if (length= e 2) #\, "")
                         #\)))
                ((cell1d) (string #\{ (deparse-arglist (cdr e)) #\}))
                ((call)   (string (deparse (cadr e)) #\( (deparse-arglist (cddr e)) #\)))
                ((ref)    (string (deparse (cadr e)) #\[ (deparse-arglist (cddr e)) #\]))
                ((curly)  (string (deparse (cadr e)) #\{ (deparse-arglist (cddr e)) #\}))
                ((quote)  (string ":(" (deparse (cadr e)) ")"))
                ((vcat)   (string #\[ (deparse-arglist (cdr e)) #\]))
                ((hcat)   (string #\[ (deparse-arglist (cdr e) " ") #\]))
                ((global local const)
                 (string (car e) " " (deparse (cadr e))))
                ((:)
                 (string (deparse (cadr e)) ': (deparse (caddr e))
                         (if (length> e 3)
                             (string ': (deparse (cadddr e)))
                             "")))
                ((comparison) (apply string (map deparse (cdr e))))
                ((in) (string (deparse (cadr e)) " in " (deparse (caddr e))))
                ((jlgensym) (string "GenSym(" (cdr e) ")"))
                (else
                 (string e))))))

(define (bad-formal-argument v)
  (error (string #\" (deparse v) #\" " is not a valid function argument name")))

(define (arg-name v)
  (cond ((and (symbol? v) (not (eq? v 'true)) (not (eq? v 'false)))
         v)
        ((not (pair? v))
         (bad-formal-argument v))
        (else
         (case (car v)
           ((... kw)      (decl-var (cadr v)))
           ((|::|)
            (if (not (symbol? (cadr v)))
                (bad-formal-argument (cadr v)))
            (decl-var v))
           (else (bad-formal-argument v))))))

; convert a lambda list into a list of just symbols
(define (llist-vars lst)
  (map arg-name (filter (lambda (a)
                          (not (and (pair? a)
                                    (eq? (car a) 'parameters))))
                        lst)))

(define (llist-keywords lst)
  (apply append
         (map (lambda (a)
                (if (and (pair? a) (eq? (car a) 'parameters))
                    (map arg-name (cdr a))
                    '()))
              lst)))

(define (arg-type v)
  (cond ((symbol? v)  'Any)
        ((not (pair? v))
         (bad-formal-argument v))
        (else
         (case (car v)
           ((...) (if (eq? (length v) 3)
                      `(... ,(decl-type (cadr v)) ,(caddr v))
                      `(... ,(decl-type (cadr v)))))
           ((|::|)
            (if (not (symbol? (cadr v)))
                (bad-formal-argument (cadr v)))
            (decl-type v))
           (else (bad-formal-argument v))))))

; get just argument types
(define (llist-types lst)
  (map arg-type lst))

(define (decl? e)
  (and (pair? e) (eq? (car e) '|::|)))

(define (jlgensym? e)
  (and (pair? e) (eq? (car e) 'jlgensym)))

(define (symbol-like? e)
  (or (symbol? e) (jlgensym? e)))

; get the variable name part of a declaration, x::int => x
(define (decl-var v)
  (if (decl? v) (cadr v) v))

(define (decl-type v)
  (if (decl? v) (caddr v) 'Any))

(define (sym-dot? e)
  (and (length= e 3) (eq? (car e) '|.|)
       (symbol-like? (cadr e))))

(define (effect-free? e)
  (or (not (pair? e)) (jlgensym? e) (sym-dot? e) (quoted? e) (equal? e '(null))))

(define (undot-name e)
  (if (symbol? e)
      e
      (cadr (caddr e))))

(define make-jlgensym
  (let ((jlgensym-counter 0))
    (lambda ()
      (begin0 `(jlgensym ,jlgensym-counter)
              (set! jlgensym-counter (+ 1 jlgensym-counter))))))

; make an expression safe for multiple evaluation
; for example a[f(x)] => (temp=f(x); a[temp])
; retuns a pair (expr . assignments)
; where 'assignments' is a list of needed assignment statements
(define (remove-argument-side-effects e)
  (let ((a '()))
    (cond
     ((and (decl? e) (symbol? (cadr e)))
      (cons (cadr e) (list e)))
     ((not (pair? e))
      (cons e '()))
     (else
      (cons (map (lambda (x)
                   (cond
                    ((and (decl? x) (symbol? (cadr x)))
                     (set! a (cons x a))
                     (cadr x))
                    ((not (effect-free? x))
                     (let ((g (make-jlgensym)))
                       (if (or (eq? (car x) '...) (eq? (car x) '&))
                           (if (and (pair? (cadr x))
                                    (not (quoted? (cadr x))))
                               (begin (set! a (cons `(= ,g ,(cadr x)) a))
                                      `(,(car x) ,g))
                               x)
                           (begin (set! a (cons `(= ,g ,x) a))
                                  g))))
                    (else
                     x)))
                 e)
            (reverse a))))))

(define (expand-update-operator- op lhs rhs declT)
  (let ((e (remove-argument-side-effects lhs)))
    `(block ,@(cdr e)
            ,(if (null? declT)
                 `(= ,(car e) (call ,op ,(car e) ,rhs))
                 `(= ,(car e) (call ,op (:: ,(car e) ,(car declT)) ,rhs))))))

(define (expand-update-operator op lhs rhs . declT)
  (cond ((and (pair? lhs) (eq? (car lhs) 'ref))
         ;; expand indexing inside op= first, to remove "end" and ":"
         (let* ((ex (partially-expand-ref lhs))
                (stmts (butlast (cdr ex)))
                (refex (last    (cdr ex)))
                (nuref `(ref ,(caddr refex) ,@(cdddr refex))))
           `(block ,@stmts
                   ,(expand-update-operator- op nuref rhs declT))))
        ((and (pair? lhs) (eq? (car lhs) '|::|))
         ;; (+= (:: x T) rhs)
         (let ((e (remove-argument-side-effects (cadr lhs)))
               (T (caddr lhs)))
           `(block ,@(cdr e)
                   ,(expand-update-operator op (car e) rhs T))))
        (else
         (expand-update-operator- op lhs rhs declT))))

(define (dotop? o) (and (symbol? o) (eqv? (string.char (string o) 0) #\.)))

(define (partially-expand-ref e)
  (let ((a    (cadr e))
        (idxs (cddr e)))
    (let* ((reuse (and (pair? a)
                       (contains (lambda (x)
                                   (or (eq? x 'end)
                                       (eq? x ':)
                                       (and (pair? x)
                                            (eq? (car x) ':))))
                                 idxs)))
           (arr   (if reuse (make-jlgensym) a))
           (stmts (if reuse `((= ,arr ,a)) '())))
      (receive
       (new-idxs stuff) (process-indexes arr idxs)
       `(block
         ,@(append stmts stuff)
         (call getindex ,arr ,@new-idxs))))))

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
                    (make-jlgensym) arg)))
    (if (and (not (dotop? (cadr e)))
             (length> e 5)
             (pair? (cadddr (cdr e)))
             (dotop? (cadddr (cddr e))))
        ;; look ahead: if the 2nd argument of the next comparison is also
        ;; an argument to an eager (dot) op, make sure we don't skip the
        ;; initialization of its variable by short-circuiting
        (let ((s (make-jlgensym)))
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
                          ,.(map (lambda (t) `(call (top length) ,t))
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
  (cond ((eq? x ':) `(call colon 1 end))
        ((and (pair? x)
              (eq? (car x) ':))
         (cond ((length= x 3)
                (if (eq? (caddr x) ':)
                    ;; (: a :) a:
                    `(call colon ,(cadr x) end)
                    ;; (: a b)
                    `(call colon ,(cadr x) ,(caddr x))))
               ((length= x 4)
                (if (eq? (cadddr x) ':)
                    ;; (: a b :) a:b:
                    `(call colon ,(cadr x) ,(caddr x) end)
                    ;; (: a b c)
                    `(call colon ,@(cdr x))))
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
              (if (symbol-like? (cadr idx))
                  (loop (cdr lst) (+ n 1)
                        stmts
                        (cons (cadr idx) tuples)
                        (cons `(... ,(replace-end (cadr idx) a n tuples last))
                              ret))
                  (let ((g (make-jlgensym)))
                    (loop (cdr lst) (+ n 1)
                          (cons `(= ,g ,(replace-end (cadr idx) a n tuples last))
                                stmts)
                          (cons g tuples)
                          (cons `(... ,g) ret))))
              (loop (cdr lst) (+ n 1)
                    stmts tuples
                    (cons (replace-end (expand-index-colon idx) a n tuples last)
                          ret)))))))

(define (make-decl n t) `(|::| ,n ,t))

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
                     (if (vararg? x)
                         (make-decl (arg-name x) (arg-type x))
                         (if (varargexpr? x)
                             (if (pair? (caddr x))
                                 x
                                 `(|::| ,(arg-name x) (curly Vararg Any)))
                         (arg-name x))))
                   argl)))
    `(lambda ,argl
       (scope-block ,body))))

(define (symbols->typevars sl upperbounds bnd)
  (let ((bnd (if bnd '(true) '())))
    (if (null? upperbounds)
        (map (lambda (x)    `(call (top TypeVar) ',x ,@bnd)) sl)
        (map (lambda (x ub) `(call (top TypeVar) ',x ,ub ,@bnd)) sl upperbounds))))

(define (sparam-name sp)
  (cond ((symbol? sp)
         sp)
        ((and (length= sp 3)
              (eq? (car sp) '|<:|)
              (symbol? (cadr sp)))
         (cadr sp))
        (else (error "malformed type parameter list"))))

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

(define (method-expr-name m)
  (let ((lhs (cadr m)))
    (cond ((symbol? lhs)       lhs)
          ((eq? (car lhs) 'kw) (cadr lhs))
          (else                lhs))))

(define (sym-ref? e)
  (or (symbol? e)
      (and (length= e 3) (eq? (car e) '|.|)
           (or (atom? (cadr e)) (sym-ref? (cadr e)))
           (pair? (caddr e)) (memq (car (caddr e)) '(quote inert))
           (symbol? (cadr (caddr e))))))

;; convert final (... x) to (curly Vararg x)
(define (dots->vararg a)
  (if (null? a) a
      (let ((head (butlast a))
	    (las  (last a)))
	(if (vararg? las)
	    `(,@head (curly Vararg ,(cadr las)))
	    `(,@head ,las)))))

(define (method-def-expr- name sparams argl body isstaged)
  (receive
   (names bounds) (sparam-name-bounds sparams '() '())
   (begin
     (let ((anames (llist-vars argl)))
       (if (has-dups anames)
           (error "function argument names not unique"))
       (if (has-dups names)
           (error "function static parameter names not unique"))
       (if (any (lambda (x) (memq x names)) anames)
           (error "function argument and static parameter names must be distinct")))
     (if (not (or (sym-ref? name)
                  (and (pair? name) (eq? (car name) 'kw)
                       (sym-ref? (cadr name)))))
         (error (string "invalid method name \"" (deparse name) "\"")))
     (let* ((types (llist-types argl))
            (body  (method-lambda-expr argl body)))
       (if (null? sparams)
           `(method ,name (call (top svec) (curly Tuple ,@(dots->vararg types)) (call (top svec)))
		    ,body ,isstaged)
           `(method ,name
                    (call (lambda ,names
                            (call (top svec) (curly Tuple ,@(dots->vararg types)) (call (top svec) ,@names)))
                          ,@(symbols->typevars names bounds #t))
                    ,body ,isstaged))))))

(define (vararg? x) (and (pair? x) (eq? (car x) '...)))
(define (varargexpr? x) (and
                         (pair? x)
                         (eq? (car x) '::)
                         (or
                          (eq? (caddr x) 'Vararg)
                          (and
                           (pair? (caddr x))
                           (length> (caddr x) 1)
                           (eq? (cadr (caddr x)) 'Vararg)))))
(define (trans?  x) (and (pair? x) (eq? (car x) '|.'|)))
(define (ctrans? x) (and (pair? x) (eq? (car x) '|'|)))

(define (const-default? x)
  (or (number? x) (string? x) (char? x) (and (pair? x) (memq (car x) '(quote inert)))))

(define (keywords-method-def-expr name sparams argl body isstaged)
  (let* ((kargl (cdar argl))  ;; keyword expressions (= k v)
         (pargl (cdr argl))   ;; positional args
         (body  (if (and (pair? body) (eq? (car body) 'block))
                    body
                    `(block ,body)))
         ;; 1-element list of vararg argument, or empty if none
         (vararg (let ((l (if (null? pargl) '() (last pargl))))
                   (if (vararg? l)
                       (list l) '())))
         ;; positional args without vararg
         (pargl (if (null? vararg) pargl (butlast pargl)))
         ;; keywords glob
         (restkw (let ((l (last kargl)))
                   (if (vararg? l)
                       (list (cadr l)) '())))
         (kargl (if (null? restkw) kargl (butlast kargl)))
         ;; the keyword::Type expressions
         (vars     (map cadr kargl))
         ;; keyword default values
         (vals     (map caddr kargl))
         ;; just the keyword names
         (keynames (map decl-var vars))
         ;; do some default values depend on other keyword arguments?
         (ordered-defaults (any (lambda (v) (contains
                                             (lambda (x) (eq? x v))
                                             vals))
                                keynames))
         ;; 1-element list of function's line number node, or empty if none
         (lno  (if (and (pair? (cdr body))
                        (pair? (cadr body)) (eq? (caadr body) 'line))
                   (list (cadr body))
                   '()))
         ;; body statements, minus line number node
         (stmts (if (null? lno) (cdr body) (cddr body)))
         (positional-sparams
          (filter (lambda (s)
                    (let ((name (if (symbol? s) s (cadr s))))
                      (or (expr-contains-eq name (cons 'list pargl))
                          (and (pair? vararg) (expr-contains-eq name (car vararg)))
                          (not (expr-contains-eq name (cons 'list kargl))))))
                  sparams))
         (keyword-sparams
          (filter (lambda (s)
                    (let ((name (if (symbol? s) s (cadr s))))
                      (not (expr-contains-eq name (cons 'list positional-sparams)))))
                  sparams))
         (keyword-sparam-names
          (map (lambda (s) (if (symbol? s) s (cadr s))) keyword-sparams)))
    (let ((kw (gensy)) (i (gensy)) (ii (gensy)) (elt (gensy)) (rkw (gensy))
          (mangled (symbol (string "__"
                                   (undot-name name)
                                   "#"
                                   (string.sub (string (gensym)) 1)
                                   "__")))
          (flags (map (lambda (x) (gensy)) vals)))
      `(block
        ;; call with keyword args pre-sorted - original method code goes here
        ,(method-def-expr-
          mangled sparams
          `(,@vars ,@restkw ,@pargl ,@vararg)
          `(block
            ,@(if (null? lno) '()
                  (list (append (car lno) (list (undot-name name)))))
            ,@stmts) isstaged)

        ;; call with no keyword args
        ,(method-def-expr-
          name positional-sparams (append pargl vararg)
          `(block
            ,(if (null? lno)
                 `(line 0 || ||)
                 (append (car lno) '(||)))
            ,@(if (not ordered-defaults)
                  '()
                  (map make-assignment keynames vals))
            ;; call mangled(vals..., [rest_kw ,]pargs..., [vararg]...)
            (return (call ,mangled
                          ,@(if ordered-defaults keynames vals)
                          ,@(if (null? restkw) '() '((cell1d)))
                          ,@(map arg-name pargl)
                          ,@(if (null? vararg) '()
                                (list `(... ,(arg-name (car vararg))))))))
          #f)

        ;; call with unsorted keyword args. this sorts and re-dispatches.
        ,(method-def-expr-
          (list 'kw name) (filter
                           ;; remove sparams that don't occur, to avoid printing
                           ;; the warning twice
                           (lambda (s)
                             (let ((name (if (symbol? s) s (cadr s))))
                               (expr-contains-eq name (cons 'list argl))))
                           positional-sparams)
          `((:: ,kw (top Array)) ,@pargl ,@vararg)
          `(block
            (line 0 || ||)
            ;; initialize keyword args to their defaults, or set a flag telling
            ;; whether this keyword needs to be set.
            ,@(map (lambda (name dflt flag)
                     (if (const-default? dflt)
                         `(= ,name ,dflt)
                         `(= ,flag true)))
                   keynames vals flags)
            ,@(if (null? restkw) '()
                  `((= ,rkw (cell1d))))
            ;; for i = 1:(length(kw)>>1)
            (for (= ,i (: 1 (call (top >>) (call (top length) ,kw) 1)))
                 (block
                  ;; ii = i*2 - 1
                  (= ,ii (call (top -) (call (top *) ,i 2) 1))
                  (= ,elt (call (top arrayref) ,kw ,ii))
                  ,(foldl (lambda (kvf else)
                            (let* ((k    (car kvf))
                                   (rval0 `(call (top arrayref) ,kw
                                                 (call (top +) ,ii 1)))
                                   ;; note: if the "declared" type of a KW arg
                                   ;; includes something from keyword-sparam-names,
                                   ;; then don't assert it here, since those static
                                   ;; parameters don't have values yet.
                                   ;; instead, the type will be picked up when the
                                   ;; underlying method is called.
                                   (rval (if (and (decl? k)
                                                  (not (any (lambda (s)
                                                              (expr-contains-eq s (caddr k)))
                                                            keyword-sparam-names)))
                                             `(call (top typeassert)
                                                    ,rval0
                                                    ,(caddr k))
                                             rval0)))
                              ;; if kw[ii] == 'k; k = kw[ii+1]::Type; end
                              `(if (comparison ,elt === (quote ,(decl-var k)))
                                   (block
                                    (= ,(decl-var k) ,rval)
                                    ,@(if (not (const-default? (cadr kvf)))
                                          `((= ,(caddr kvf) false))
                                          '()))
                                   ,else)))
                          (if (null? restkw)
                              ;; if no rest kw, give error for unrecognized
                              `(call (top kwerr) ,elt)
                              ;; otherwise add to rest keywords
                              `(ccall 'jl_cell_1d_push Void (tuple Any Any)
                                      ,rkw (tuple ,elt
                                                  (call (top arrayref) ,kw
                                                        (call (top +) ,ii 1)))))
                          (map list vars vals flags))))
            ;; set keywords that weren't present to their default values
            ,@(apply append
                     (map (lambda (name dflt flag)
                            (if (const-default? dflt)
                                '()
                                `((if ,flag (= ,name ,dflt)))))
                          keynames vals flags))
            ;; finally, call the core function
            (return (call ,mangled
                          ,@keynames
                          ,@(if (null? restkw) '() (list rkw))
                          ,@(map arg-name pargl)
                          ,@(if (null? vararg) '()
                                (list `(... ,(arg-name (car vararg))))))))
          #f)
        ;; return primary function
        ,name))))

(define (optional-positional-defs name sparams req opt dfl body isstaged overall-argl . kw)
  (let ((lno  (if (and (pair? body) (pair? (cdr body))
                       (pair? (cadr body)) (eq? (caadr body) 'line))
                  (list (cadr body))
                  '())))
  `(block
    ,@(map (lambda (n)
             (let* ((passed (append req (list-head opt n)))
                    ;; only keep static parameters used by these arguments
                    (sp     (filter (lambda (sp)
                                      (contains (lambda (e) (eq? e (sparam-name sp)))
                                                passed))
                                    sparams))
                    (vals   (list-tail dfl n))
                    (absent (list-tail opt n)) ;; absent arguments
                    (body
                     (if (any (lambda (defaultv)
                                ;; does any default val expression...
                                (contains (lambda (e)
                                            ;; contain "e" such that...
                                            (any (lambda (a)
                                                   ;; "e" is in an absent arg
                                                   (contains (lambda (u)
                                                               (eq? u e))
                                                             a))
                                                 absent))
                                          defaultv))
                              vals)
                         ;; then add only one next argument
                         `(block
                           ,@lno
                           (call ,name ,@kw ,@(map arg-name passed) ,(car vals)))
                         ;; otherwise add all
                         `(block
                           ,@lno
                           (call ,name ,@kw ,@(map arg-name passed) ,@vals)))))
               (method-def-expr name sp (append kw passed) body #f)))
           (iota (length opt)))
    ,(method-def-expr name sparams overall-argl body isstaged))))

(define (method-def-expr name sparams argl body isstaged)
  (if (any kwarg? argl)
      ;; has optional positional args
      (begin
        (let check ((l     argl)
                    (seen? #f))
          (if (pair? l)
              (if (kwarg? (car l))
                  (check (cdr l) #t)
                  (if (and seen? (not (vararg? (car l))))
                      (error "optional positional arguments must occur at end")
                      (check (cdr l) #f)))))
        (receive
         (kws argl) (separate kwarg? argl)
         (let ((opt  (map cadr  kws))
               (dfl  (map caddr kws)))
           (if (has-parameters? argl)
               ;; both!
               ;; separate into keyword version with all positional args,
               ;; and a series of optional-positional-defs that delegate keywords
               (let ((kw   (car argl))
                     (argl (cdr argl)))
                 (check-kw-args (cdr kw))
                 (receive
                  (vararg req) (separate vararg? argl)
                  (optional-positional-defs name sparams req opt dfl body isstaged
                                            (cons kw (append req opt vararg))
                                            `(parameters (... ,(gensy))))))
               ;; optional positional only
               (receive
                (vararg req) (separate vararg? argl)
                (optional-positional-defs name sparams req opt dfl body isstaged
                                          (append req opt vararg)))))))
      (if (has-parameters? argl)
          ;; keywords only
          (begin (check-kw-args (cdar argl))
                 (keywords-method-def-expr name sparams argl body isstaged))
          ;; neither
          (method-def-expr- name sparams argl body isstaged))))

(define (struct-def-expr name params super fields mut)
  (receive
   (params bounds) (sparam-name-bounds params '() '())
   (struct-def-expr- name params bounds super (flatten-blocks fields) mut)))

;; replace field names with gensyms if they conflict with field-types
(define (safe-field-names field-names field-types)
  (if (any (lambda (v) (contains (lambda (e) (eq? e v)) field-types))
           field-names)
      (map (lambda (x) (gensy)) field-names)
      field-names))

(define (default-inner-ctors name field-names field-types gen-specific?)
  (let* ((field-names (safe-field-names field-names field-types))
         (any-ctor
          ;; definition with Any for all arguments
          `(function (call ,name ,@field-names)
                     (block
                      (call new ,@field-names)))))
    (if (and gen-specific? (any (lambda (t) (not (eq? t 'Any))) field-types))
        (list
         ;; definition with field types for all arguments
         `(function (call ,name
                          ,@(map make-decl field-names field-types))
                    (block
                     (call new ,@field-names)))
         any-ctor)
        (list any-ctor))))

(define (default-outer-ctor name field-names field-types params bounds)
  (let ((field-names (safe-field-names field-names field-types)))
    `(function (call (curly ,name
                            ,@(map (lambda (p b) `(<: ,p ,b))
                                   params bounds))
                     ,@(map make-decl field-names field-types))
               (block
                (call (curly ,name ,@params) ,@field-names)))))

(define (new-call Tname type-params params args field-names field-types mutabl)
  (if (any vararg? args)
      (error "... is not supported inside \"new\""))
  (if (any kwarg? args)
      (error "\"new\" does not accept keyword arguments"))
  (if (length> params (length type-params))
      (error "too few type parameters specified in \"new{...}\""))
  (let ((Texpr (if (null? type-params)
                   `(|.| ,(current-julia-module) ',Tname)
                   `(curly (|.| ,(current-julia-module) ',Tname)
                           ,@type-params))))
    (cond ((length> args (length field-names))
           `(call (top error) "new: too many arguments"))
          (else
           (if (equal? type-params params)
               `(new ,Texpr ,@(map (lambda (fty val)
                                     `(call (top convert) ,fty ,val))
                                   (list-head field-types (length args)) args))
               (let ((tn (gensy)))
                 `(let (new ,tn ,@(map (lambda (fld val)
                                         `(call (top convert)
                                                (call (top fieldtype) ,tn (quote ,fld))
                                                ,val))
                                       (list-head field-names (length args)) args))
                    (= ,tn ,Texpr))))))))

;; insert a statement after line number node
(define (prepend-stmt stmt body)
  (if (and (pair? body) (eq? (car body) 'block))
      (cond ((atom? (cdr body))
             `(block ,stmt (null)))
            ((and (pair? (cadr body)) (eq? (caadr body) 'line))
             `(block ,(cadr body) ,stmt ,@(cddr body)))
            (else
             `(block ,stmt ,@(cdr body))))
      body))

;; insert item at start of arglist
(define (arglist-unshift sig item)
  (if (and (pair? sig) (pair? (car sig)) (eq? (caar sig) 'parameters))
      `(,(car sig) ,item ,@(cdr sig))
      `(,item ,@sig)))

(define (ctor-signature name params bounds method-params sig)
  (if (null? params)
      (if (null? method-params)
          (cons `(call call ,@(arglist-unshift sig `(|::| ,(gensy) (curly Type ,name))))
                params)
          (cons `(call (curly call ,@method-params)
                       ,@(arglist-unshift sig `(|::| ,(gensy) (curly Type ,name))))
                params))
      (if (null? method-params)
          (cons `(call (curly call ,@(map (lambda (p b) `(<: ,p ,b)) params bounds))
                       ,@(arglist-unshift sig `(|::| ,(gensy) (curly Type (curly ,name ,@params)))))
                params)
          ;; rename parameters that conflict with user-written method parameters
          (let ((new-params (map (lambda (p) (if (memq p method-params)
                                                 (gensy)
                                                 p))
                                 params)))
            (cons `(call (curly call ,@(map (lambda (p b) `(<: ,p ,b)) new-params bounds)
                                ,@method-params)
                         ,@(arglist-unshift sig `(|::| ,(gensy) (curly Type (curly ,name ,@new-params)))))
                  new-params)))))

(define (ctor-def keyword name Tname params bounds method-params sig ctor-body body)
  (if (eq? name Tname)
      (let* ((temp   (ctor-signature name params bounds method-params sig))
             (sig    (car temp))
             (params (cdr temp)))
        `(,keyword ,sig ,(ctor-body body params)))
      `(,keyword (call (curly ,name ,@method-params) ,@sig)
                 ;; pass '() in order to require user-specified parameters with
                 ;; new{...} inside a non-ctor inner definition.
                 ,(ctor-body body '()))))

(define (rewrite-ctor ctor Tname params bounds field-names field-types mutabl)
  (define (ctor-body body type-params)
    (pattern-replace (pattern-set
                      (pattern-lambda
                       (call (-/ new) . args)
                       (new-call Tname type-params params
                                 args field-names field-types mutabl))
                      (pattern-lambda
                       (call (curly (-/ new) . p) . args)
                       (new-call Tname p params
                                 args field-names field-types mutabl)))
                     body))
  (pattern-replace
   (pattern-set
    (pattern-lambda (function (call (curly name . p) . sig) body)
                    (ctor-def 'function name Tname params bounds p sig ctor-body body))
    (pattern-lambda (stagedfunction (call (curly name . p) . sig) body)
                    (ctor-def 'stagedfunction name Tname params bounds p sig ctor-body body))
    (pattern-lambda (function (call name . sig) body)
                    (ctor-def 'function name Tname params bounds '() sig ctor-body body))
    (pattern-lambda (stagedfunction (call name . sig) body)
                    (ctor-def 'stagedfunction name Tname params bounds '() sig ctor-body body))
    (pattern-lambda (= (call (curly name . p) . sig) body)
                    (ctor-def 'function name Tname params bounds p sig ctor-body body))
    (pattern-lambda (= (call name . sig) body)
                    (ctor-def 'function name Tname params bounds '() sig ctor-body body)))
   ctor))

;; check if there are any calls to new with fewer than n arguments
(define (ctors-min-initialized expr)
  (and (pair? expr)
   (min
    ((pattern-lambda
      (call (-/ new) . args)
      (length args)) (car expr))
    ((pattern-lambda
      (call (curly (-/ new) . p) . args)
      (length args)) (car expr))
    (ctors-min-initialized (car expr))
    (ctors-min-initialized (cdr expr)))))

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
   (let* ((defs        (filter (lambda (x) (not (effect-free? x))) defs))
          (field-names (map decl-var fields))
          (field-types (map decl-type fields))
          (defs2 (if (null? defs)
                     (default-inner-ctors name field-names field-types (null? params))
                     defs))
          (min-initialized (min (ctors-min-initialized defs) (length fields))))
     (for-each (lambda (v)
                 (if (not (symbol? v))
                     (error (string "field name \"" (deparse v) "\" is not a symbol"))))
               field-names)
     (if (null? params)
         `(block
           (global ,name) (const ,name)
           (composite_type ,name (call (top svec) ,@params)
                           (call (top svec) ,@(map (lambda (x) `',x) field-names))
                           ,super (call (top svec) ,@field-types) ,mut ,min-initialized)
           (call
            (lambda ()
              (scope-block
               (block
                (global ,name) (global call)
                ,@(map (lambda (c)
                         (rewrite-ctor c name '() '() field-names field-types mut))
                       defs2)))))
           (null))
         ;; parametric case
         `(block
           (scope-block
            (block
             (global ,name) (const ,name)
             ,@(map (lambda (v) `(local ,v)) params)
             ,@(map make-assignment params (symbols->typevars params bounds #t))
             (composite_type ,name (call (top svec) ,@params)
                             (call (top svec) ,@(map (lambda (x) `',x) field-names))
                             ,super (call (top svec) ,@field-types) ,mut ,min-initialized)))
           ;; "inner" constructors
           (call
            (lambda ()
              (scope-block
               (block
                (global ,name) (global call)
                ,@(map (lambda (c)
                         (rewrite-ctor c name params bounds field-names field-types mut))
                       defs2)))))
           ;; "outer" constructors
           ,@(if (and (null? defs)
                      ;; don't generate an outer constructor if the type has
                      ;; parameters not mentioned in the field types. such a
                      ;; constructor would not be callable anyway.
                      (every (lambda (sp)
                               (expr-contains-eq sp (cons 'list field-types)))
                             params))
                 `((scope-block
                    (block
                     (global ,name)
                     ,(default-outer-ctor name field-names field-types
                        params bounds))))
                 '())
           (null))))))

(define (abstract-type-def-expr name params super)
  (receive
   (params bounds)
   (sparam-name-bounds params '() '())
   `(block
     (const ,name)
     ,@(map (lambda (v) `(local ,v)) params)
     ,@(map make-assignment params (symbols->typevars params bounds #f))
     (abstract_type ,name (call (top svec) ,@params) ,super))))

(define (bits-def-expr n name params super)
  (receive
   (params bounds)
   (sparam-name-bounds params '() '())
   `(block
     (const ,name)
     ,@(map (lambda (v) `(local ,v)) params)
     ,@(map make-assignment params (symbols->typevars params bounds #f))
     (bits_type ,name (call (top svec) ,@params) ,n ,super))))

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
  (let loop ((F atypes)  ;; formals
             (A args)    ;; actuals
             (stmts '()) ;; initializers
             (C '()))    ;; converted
    (if (or (null? F) (null? A))
        `(block
          ,.(reverse! stmts)
          (call (top ccall) ,name ,RT (call (top svec) ,@(dots->vararg atypes))
		,.(reverse! C)
                ,@A))
        (let* ((a     (car A))
               (isseq (and (pair? (car F)) (eq? (caar F) '...)))
               (ty    (if isseq (cadar F) (car F))))
          (if (eq? ty 'Any)
            (loop (if isseq F (cdr F)) (cdr A) stmts (list* 0 a C))
            (let* ((g (make-jlgensym))
                   (isamp (and (pair? a) (eq? (car a) '&)))
                   (a (if isamp (cadr a) a))
                   (stmts (cons `(= ,g (call (top ,(if isamp 'ptr_arg_cconvert 'cconvert)) ,ty ,a)) stmts))
                   (ca `(call (top ,(if isamp 'ptr_arg_unsafe_convert 'unsafe_convert)) ,ty ,g)))
              (loop (if isseq F (cdr F)) (cdr A) stmts
                    (list* g (if isamp `(& ,ca) ca) C))))))))

(define (block-returns? e)
  (if (assignment? e)
      (block-returns? (caddr e))
      (and (pair? e) (eq? (car e) 'block)
           (any return? (cdr e)))))

(define (replace-return e bb ret retval)
  (cond ((or (atom? e) (quoted? e)) e)
        ((or (eq? (car e) 'lambda)
             (eq? (car e) 'function)
         (eq? (car e) 'stagedfunction)
             (eq? (car e) '->)) e)
        ((eq? (car e) 'return)
         `(block ,@(if ret `((= ,ret true)) '())
                 (= ,retval ,(cadr e))
                 (break ,bb)))
        (else (map (lambda (x) (replace-return x bb ret retval)) e))))

(define (expand-binding-forms e)
  (cond
   ((atom? e) e)
   ((quoted? e) e)
   (else
    (case (car e)
      ((function stagedfunction)
       (let ((name (cadr e)))
         (if (pair? name)
             (if (eq? (car name) 'call)
                 (expand-binding-forms
                  (if (and (pair? (cadr name))
                           (eq? (car (cadr name)) 'curly))
                      (method-def-expr (cadr (cadr name))
                                       (cddr (cadr name))
                                       (fix-arglist (cddr name))
                                       (caddr e) (eq? (car e) 'stagedfunction))
                      (method-def-expr (cadr name)
                                       '()
                                       (fix-arglist (cddr name))
                                       (caddr e) (eq? (car e) 'stagedfunction))))
                 (if (eq? (car name) 'tuple)
                     (expand-binding-forms
                      `(-> ,name ,(caddr e)))
                     e))
             e)))

      ((->)
       (let ((a (cadr e))
             (b (caddr e)))
         (let ((a (if (and (pair? a)
                           (eq? (car a) 'tuple))
                      (cdr a)
                      (list a))))
           (expand-binding-forms
            (function-expr (fix-arglist a)
                           `(block
                             ,.(map (lambda (d)
                                      `(= ,(cadr d)
                                          (typeassert ,@(cdr d))))
                                    (filter decl? a))
                             ,b))))))

      ((let)
       (let ((ex (cadr e))
             (binds (cddr e)))
         (expand-binding-forms
          (if
           (null? binds)
           `(scope-block (block ,ex))
           (let loop ((binds (reverse binds))
                      (blk   ex))
             (if (null? binds)
                 blk
                 (cond
                  ((or (symbol? (car binds)) (decl? (car binds)))
                   ;; just symbol -> add local
                   (loop (cdr binds)
                         `(scope-block
                           (block
                            (local ,(car binds))
                            (newvar ,(decl-var (car binds)))
                            ,blk))))
                  ((and (length= (car binds) 3)
                        (eq? (caar binds) '=))
                   ;; some kind of assignment
                   (cond
                    ((or (symbol? (cadar binds))
                         (decl?   (cadar binds)))
                     (let ((vname (decl-var (cadar binds))))
                       (loop (cdr binds)
                             (if (contains (lambda (x) (eq? x vname))
                                           (caddar binds))
                                 (let ((tmp (make-jlgensym)))
                                   `(scope-block
                                     (block (= ,tmp ,(caddar binds))
                                      (scope-block
                                       (block
                                        (local ,(cadar binds))
                                        (newvar ,vname)
                                        (= ,vname ,tmp)
                                        ,blk)))))
                                 `(scope-block
                                   (block
                                    (local ,(cadar binds))
                                    (newvar ,vname)
                                    (= ,vname ,(caddar binds))
                                    ,blk))))))
                    ((and (pair? (cadar binds))
                          (eq? (caadar binds) 'call))
                     ;; f()=c
                     (let ((asgn (cadr (julia-expand0 (car binds)))))
                       (loop (cdr binds)
                             `(scope-block
                               (block
                                (local ,(cadr asgn))
                                (newvar ,(cadr asgn))
                                ,asgn
                                ,blk)))))
                    (else (error "invalid let syntax"))))
                  (else (error "invalid let syntax")))))))))

      ((macro)
       (cond ((and (pair? (cadr e))
                   (eq? (car (cadr e)) 'call)
                   (symbol? (cadr (cadr e))))
              `(macro ,(symbol (string #\@ (cadr (cadr e))))
                 ,(expand-binding-forms
                   `(-> (tuple ,@(cddr (cadr e)))
                        ,(caddr e)))))
             ((symbol? (cadr e))  ;; already expanded
              e)
             (else
              (error "invalid macro definition"))))

      ((type)
       (let ((mut (cadr e))
             (sig (caddr e))
             (fields (cdr (cadddr e))))
         (expand-binding-forms
          (receive (name params super) (analyze-type-sig sig)
                   (struct-def-expr name params super fields mut)))))

      ((try)
       (if (length= e 5)
           (let (;; expand inner try blocks first, so their return statements
                 ;; will have been moved for `finally`, causing correct
                 ;; chaining behavior when the current (outer) try block is
                 ;; expanded.
                 (tryb (expand-binding-forms (cadr e)))
                 (var  (caddr e))
                 (catchb (cadddr e))
                 (finalb (cadddr (cdr e))))
             (if (has-unmatched-symbolic-goto? tryb)
                 (error "goto from a try/finally block is not permitted"))
             (let ((hasret (or (contains return? tryb)
                               (contains return? catchb))))
               (let ((err (gensy))
                     (ret (and hasret
                               (or (not (block-returns? tryb))
                                   (and catchb
                                        (not (block-returns? catchb))))
                               (gensy)))
                     (retval (gensy))
                     (bb  (gensy))
                     (val (gensy))) ;; this is jlgensym, but llvm has trouble determining that it dominates all uses
                 (let ((tryb   (replace-return tryb bb ret retval))
                       (catchb (replace-return catchb bb ret retval)))
                   (expand-binding-forms
                    `(scope-block
                      (block
                       (local ,retval)
                       (local ,val)
                       (= ,err false)
                       ,@(if ret `((= ,ret false)) '())
                       (break-block
                        ,bb
                        (try (= ,val
                                ,(if catchb
                                     `(try ,tryb ,var ,catchb)
                                     tryb))
                             #f
                             (= ,err true)))
                       ,finalb
                       (if ,err (ccall 'jl_rethrow Void (tuple)))
                       ,(if hasret
                            (if ret
                                `(if ,ret (return ,retval) ,val)
                                `(return ,retval))
                            val))))))))
           (if (length= e 4)
               (let ((tryb (cadr e))
                     (var  (caddr e))
                     (catchb (cadddr e)))
                 (expand-binding-forms
                  (if (symbol-like? var)
                      `(trycatch (scope-block ,tryb)
                                 (scope-block
                                  (block (= ,var (the_exception))
                                         ,catchb)))
                      `(trycatch (scope-block ,tryb)
                                 (scope-block ,catchb)))))
               (map expand-binding-forms e))))

      ((=)
       (if (and (pair? (cadr e))
                (eq? (car (cadr e)) 'call))
           (expand-binding-forms (cons 'function (cdr e)))
           (map expand-binding-forms e)))

      ((const)
       (if (atom? (cadr e))
           e
           (case (car (cadr e))
             ((global local)
              (expand-binding-forms
               (qualified-const-expr (cdr (cadr e)) e)))
             ((=)
              (let ((lhs (cadr (cadr e)))
                    (rhs (caddr (cadr e))))
                (let ((vars (if (and (pair? lhs) (eq? (car lhs) 'tuple))
                                (cdr lhs)
                                (list lhs))))
                  `(block
                    ,.(map (lambda (v)
                             `(const ,(const-check-symbol (decl-var v))))
                           vars)
                    ,(expand-binding-forms `(= ,lhs ,rhs))))))
             (else
              e))))

      ((local global)
       (if (and (symbol? (cadr e)) (length= e 2))
           e
           (expand-binding-forms (expand-decls (car e) (cdr e)))))

      ((typealias)
       (if (and (pair? (cadr e))
                (eq? (car (cadr e)) 'curly))
           (let ((name (cadr (cadr e)))
                 (params (cddr (cadr e)))
                 (type-ex (caddr e)))
             (receive
              (params bounds)
              (sparam-name-bounds params '() '())
              `(call (lambda ,params
                       (block
                        (const ,name)
                        (= ,name (call (top TypeConstructor)
                                       (call (top svec) ,@params)
                                       ,(expand-binding-forms type-ex)))))
                     ,@(symbols->typevars params bounds #t))))
           (expand-binding-forms
            `(const (= ,(cadr e) ,(caddr e))))))

      (else
       (map expand-binding-forms e))))))

;; a copy of the above patterns, but returning the names of vars
;; introduced by the forms, instead of their transformations.
(define vars-introduced-by-patterns
  (pattern-set
   ;; function with static parameters
   (pattern-lambda
    (function (call (curly name . sparams) . argl) body)
    (cons 'varlist (append (llist-vars (fix-arglist argl))
                           (apply nconc
                                  (map (lambda (v) (trycatch
                                                    (list (sparam-name v))
                                                    (lambda (e) '())))
                                       sparams)))))

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
                              (vars  '()))
                     (if (null? binds)
                         (cons 'varlist vars)
                         (cond
                          ((or (symbol? (car binds)) (decl? (car binds)))
                           ;; just symbol -> add local
                           (loop (cdr binds)
                                 (cons (decl-var (car binds)) vars)))
                          ((and (length= (car binds) 3)
                                (eq? (caar binds) '=))
                           ;; some kind of assignment
                           (cond
                            ((or (symbol? (cadar binds))
                                 (decl?   (cadar binds)))
                             ;; a=b -> add argument
                             (loop (cdr binds)
                                   (cons (decl-var (cadar binds)) vars)))
                            ((and (pair? (cadar binds))
                                  (eq? (caadar binds) 'call))
                             ;; f()=c
                             (let ((asgn (cadr (julia-expand0 (car binds)))))
                               (loop (cdr binds)
                                     (cons (cadr asgn) vars))))
                            (else '())))
                          (else '())))))

   ;; macro definition
   (pattern-lambda (macro (call name . argl) body)
                   `(-> (tuple ,@argl) ,body))

   (pattern-lambda (try tryb var catchb finalb)
                   (if var (list 'varlist var) '()))
   (pattern-lambda (try tryb var catchb)
                   (if var (list 'varlist var) '()))

   ;; type definition
   (pattern-lambda (type mut (<: (curly tn . tvars) super) body)
                   (list* 'varlist (cons (unescape tn) (unescape tn)) '(new . new)
                          (map typevar-expr-name tvars)))
   (pattern-lambda (type mut (curly tn . tvars) body)
                   (list* 'varlist (cons (unescape tn) (unescape tn)) '(new . new)
                          (map typevar-expr-name tvars)))
   (pattern-lambda (type mut (<: tn super) body)
                   (list 'varlist (cons (unescape tn) (unescape tn)) '(new . new)))
   (pattern-lambda (type mut tn body)
                   (list 'varlist (cons (unescape tn) (unescape tn)) '(new . new)))

   )) ; vars-introduced-by-patterns

(define keywords-introduced-by-patterns
  (pattern-set
   (pattern-lambda (function (call (curly name . sparams) . argl) body)
                   (cons 'varlist (llist-keywords (fix-arglist argl))))

   (pattern-lambda (function (call name . argl) body)
                   (cons 'varlist (llist-keywords (fix-arglist argl))))

   (pattern-lambda (= (call (curly name . sparams) . argl) body)
                   `(function (call (curly ,name . ,sparams) . ,argl) ,body))
   (pattern-lambda (= (call name . argl) body)
                   `(function (call ,name ,@argl) ,body))
   ))

(define (assigned-name e)
  (if (and (pair? e) (memq (car e) '(call curly)))
      (assigned-name (cadr e))
      e))

; local x, y=2, z => local x;local y;local z;y = 2
(define (expand-decls what binds)
  (if (not (list? binds))
      (error (string "invalid \"" what "\" declaration")))
  (let loop ((b       binds)
             (vars    '())
             (assigns '()))
    (if (null? b)
        (if (and (null? assigns)
                 (length= vars 1))
            `(,what ,(car vars))
            `(block
              ,.(map (lambda (x) `(,what ,x)) vars)
              ,.(reverse assigns)))
        (let ((x (car b)))
          (cond ((assignment-like? x)
                 (loop (cdr b)
                       (cons (assigned-name (cadr x)) vars)
                       (cons `(,(car x) ,(decl-var (cadr x)) ,(caddr x))
                             assigns)))
                ((and (pair? x) (eq? (car x) '|::|))
                 (loop (cdr b)
                       (cons (decl-var x) vars)
                       (cons x assigns)))
                ((symbol? x)
                 (loop (cdr b) (cons x vars) assigns))
                (else
                 (error (string "invalid syntax in \"" what "\" declaration"))))))))

(define (make-assignment l r) `(= ,l ,r))
(define (assignment? e) (and (pair? e) (eq? (car e) '=)))
(define (return? e) (and (pair? e) (eq? (car e) 'return)))

(define (const-check-symbol s)
  (if (not (symbol? s))
      (error "expected identifier after \"const\"")
      s))

(define (qualified-const-expr binds __)
  (let ((vs (map (lambda (b)
                   (if (assignment? b)
                       (const-check-symbol (decl-var (cadr b)))
                       (error "expected assignment after \"const\"")))
                 binds)))
    `(block ,@(map (lambda (v) `(const ,v)) vs)
            ,(cadr __))))

(define (eq-sym? a b)
  (or (eq? a b) (and (jlgensym? a) (jlgensym? b) (eqv? (cdr a) (cdr b)))))

(define (mem-jlgensym item lst)
  (cond ((atom? lst) #f)
        ((and (jlgensym? (car lst)) (eq? (car lst) item)) #t)
        (#t (mem-jlgensym item (cdr lst)))))

;; convert (lhss...) = (tuple ...) to assignments, eliminating the tuple
(define (tuple-to-assignments lhss0 x)
  (let loop ((lhss lhss0)
             (assigned lhss0)
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
          (if (and (symbol-like? L)
                   (or (not (pair? R)) (quoted? R) (equal? R '(null)))
                   ;; overwrite var immediately if it doesn't occur elsewhere
                   (not (contains (lambda (e) (eq-sym? e L)) (cdr rhss)))
                   (not (contains (lambda (e) (eq-sym? e R)) assigned)))
              (loop (cdr lhss)
                    (cons L assigned)
                    (cdr rhss)
                    (cons (make-assignment L R) stmts)
                    after
                    (cons R elts))
              (let ((temp (make-jlgensym)))
                (loop (cdr lhss)
                      (cons L assigned)
                      (cdr rhss)
                      (cons (make-assignment temp R) stmts)
                      (cons (make-assignment L temp) after)
                      (cons temp elts))))))))

;; convert (lhss...) = x to tuple indexing
(define (lower-tuple-assignment lhss x)
  (let ((t (make-jlgensym)))
    `(block
      (= ,t ,x)
      ,@(let loop ((lhs lhss)
                   (i   1))
          (if (null? lhs) '((null))
              (cons `(= ,(car lhs)
                        (call (top getfield) ,t ,i))
                    (loop (cdr lhs)
                          (+ i 1)))))
      ,t)))

(define (check-kw-args kw)
  (let ((invalid (filter (lambda (x) (not (or (kwarg? x)
                                              (vararg? x))))
                         kw)))
    (if (pair? invalid)
        (if (and (pair? (car invalid)) (eq? 'parameters (caar invalid)))
            (error "more than one semicolon in argument list")
            (cond ((symbol? (car invalid))
                   (error (string "keyword argument \"" (car invalid) "\" needs a default value")))
                  (else
                   (error (string "invalid keyword argument syntax \""
                                  (deparse (car invalid))
                                  "\" (expected assignment)"))))))))

(define (lower-kw-call f kw pa)
  (if (any (lambda (x) (and (pair? x) (eq? (car x) 'parameters)))
           kw)
      (error "more than one semicolon in argument list"))
  (receive
   (keys restkeys) (separate kwarg? kw)
   (let ((keyargs (apply append
                         (map (lambda (a)
                                (if (not (symbol? (cadr a)))
                                    (error (string "keyword argument is not a symbol: \""
                                                   (deparse (cadr a)) "\"")))
                                (if (vararg? (caddr a))
                                    (error "splicing with \"...\" cannot be used for a keyword argument value"))
                                `((quote ,(cadr a)) ,(caddr a)))
                              keys))))
     (if (null? restkeys)
         `(call (top kwcall) call ,(length keys) ,@keyargs
                ,f (call (top Array) (top Any) ,(* 2 (length keys)))
                ,@pa)
         (let ((container (make-jlgensym)))
           `(block
             (= ,container (call (top Array) (top Any) ,(* 2 (length keys))))
             ,@(map (lambda (rk)
                    (let* ((k (make-jlgensym))
                           (v (make-jlgensym))
                           (push-expr `(ccall 'jl_cell_1d_push2 Void
                                             (tuple Any Any Any)
                                             ,container
                                             (|::| ,k (top Symbol))
                                             ,v)))
                      (if (vararg? rk)
                          `(for (= (tuple ,k ,v) ,(cadr rk))
                                ,push-expr)
                          `(block (= (tuple ,k ,v) ,rk)
                                  ,push-expr))))
                  restkeys)
             ,(let ((kw-call `(call (top kwcall) call ,(length keys) ,@keyargs
                                    ,f ,container ,@pa)))
                (if (not (null? keys))
                    kw-call
                    `(if (call (top isempty) ,container)
                         (call ,f ,@pa)
                         ,kw-call)))))))))

(define (expand-transposed-op e ops)
  (let ((a (caddr e))
        (b (cadddr e)))
    (cond ((ctrans? a)
           (if (ctrans? b)
               `(call ,(aref ops 0) #;Ac_mul_Bc ,(expand-forms (cadr a))
                      ,(expand-forms (cadr b)))
               `(call ,(aref ops 1) #;Ac_mul_B ,(expand-forms (cadr a))
                      ,(expand-forms b))))
          ((trans? a)
           (if (trans? b)
               `(call ,(aref ops 2) #;At_mul_Bt ,(expand-forms (cadr a))
                      ,(expand-forms (cadr b)))
               `(call ,(aref ops 3) #;At_mul_B ,(expand-forms (cadr a))
                      ,(expand-forms b))))
          ((ctrans? b)
           `(call ,(aref ops 4) #;A_mul_Bc ,(expand-forms a)
                  ,(expand-forms (cadr b))))
          ((trans? b)
           `(call ,(aref ops 5) #;A_mul_Bt ,(expand-forms a)
                  ,(expand-forms (cadr b))))
          (else
           `(call ,(cadr e) ,(expand-forms a) ,(expand-forms b))))))

(define (lower-update-op e)
  (expand-forms
   (expand-update-operator
    (let ((str (string (car e))))
      (symbol (string.sub str 0 (- (length str) 1))))
    (cadr e)
    (caddr e))))

(define (expand-for while lhs X body)
  ;; (for (= lhs X) body)
  (let ((coll  (make-jlgensym))
        (state (gensy)))
    `(scope-block
      (block (= ,coll ,(expand-forms X))
             (= ,state (call (top start) ,coll))
             ,(expand-forms
               `(,while
                 (call (top !) (call (top done) ,coll ,state))
                 (scope-block
                  (block
                   ;; NOTE: enable this to force loop-local var
                   #;,@(map (lambda (v) `(local ,v)) (lhs-vars lhs))
                   ,(lower-tuple-assignment (list lhs state)
                                            `(call (top next) ,coll ,state))
                   ,body))))))))

(define (map-expand-forms e) (map expand-forms e))

(define (expand-forms e)
  (if (atom? e)
      e
      ((get expand-table (car e) map-expand-forms) e)))

(define expand-table
  (table
   'quote identity
   'inert identity
   'top   identity
   'line  identity

   'lambda
   (lambda (e) (list* 'lambda (map expand-forms (cadr e)) (map expand-forms (cddr e))))

   'block
   (lambda (e)
     (if (null? (cdr e))
         '(block (null))
         (map expand-forms e)))

   '|.|
   (lambda (e)
     `(call (top getfield) ,(expand-forms (cadr e)) ,(expand-forms (caddr e))))

   'in
   (lambda (e)
     `(call in ,(expand-forms (cadr e)) ,(expand-forms (caddr e))))

   '=
   (lambda (e)
     (if (or (atom? (cadr e)) (jlgensym? (cadr e)))
         `(= ,(cadr e) ,(expand-forms (caddr e)))
         (case (car (cadr e))
           ((|.|)
            ;; a.b =
            (let ((a (cadr (cadr e)))
                  (b (caddr (cadr e)))
                  (rhs (caddr e)))
              (let ((aa (if (atom? a) a (make-jlgensym)))
                    (bb (if (or (atom? b) (quoted? b)) b (make-jlgensym))))
                `(block
                  ,.(if (eq? aa a) '() `((= ,aa ,(expand-forms a))))
                  ,.(if (eq? bb b) '() `((= ,bb ,(expand-forms b))))
                  (call (top setfield!) ,aa ,bb
                        (call (top convert)
                              (call (top fieldtype) (call (top typeof) ,aa) ,bb)
                              ,(expand-forms rhs)))))))

           ((tuple)
            ;; multiple assignment
            (let ((lhss (cdr (cadr e)))
                  (x    (caddr e)))
              (if (and (pair? x) (pair? lhss) (eq? (car x) 'tuple)
                       (length= lhss (length (cdr x))))
                  ;; (a, b, ...) = (x, y, ...)
                  (expand-forms
                   (tuple-to-assignments lhss x))
                  ;; (a, b, ...) = other
                  (let* ((xx  (if (and (symbol? x) (not (memq x lhss)))
                                  x (make-jlgensym)))
                         (ini (if (eq? x xx) '() `((= ,xx ,(expand-forms x)))))
                         (st  (gensy)))
                    `(block
                      ,@ini
                      (= ,st (call (top start) ,xx))
                      ,.(map (lambda (i lhs)
                               (expand-forms
                                (lower-tuple-assignment
                                 (list lhs st)
                                 `(call (top indexed_next)
                                        ,xx ,(+ i 1) ,st))))
                             (iota (length lhss))
                             lhss)
                      ,xx)))))

           ((typed_hcat)
            (error "invalid spacing in left side of indexed assignment"))
           ((typed_vcat)
            (error "unexpected \";\" in left side of indexed assignment"))

           ((ref)
            ;; (= (ref a . idxs) rhs)
            (let ((a    (cadr (cadr e)))
                  (idxs (cddr (cadr e)))
                  (rhs  (caddr e)))
              (let* ((reuse (and (pair? a)
                                 (contains (lambda (x)
                                             (or (eq? x 'end)
                                                 (and (pair? x)
                                                      (eq? (car x) ':))))
                                           idxs)))
                     (arr   (if reuse (make-jlgensym) a))
                     (stmts (if reuse `((= ,arr ,(expand-forms a))) '())))
                (let* ((rrhs (and (pair? rhs) (not (jlgensym? rhs)) (not (quoted? rhs))))
                       (r    (if rrhs (make-jlgensym) rhs))
                       (rini (if rrhs `((= ,r ,(expand-forms rhs))) '())))
                  (receive
                   (new-idxs stuff) (process-indexes arr idxs)
                   `(block
                     ,@stmts
                     ,.(map expand-forms stuff)
                     ,@rini
                     ,(expand-forms
                       `(call setindex! ,arr ,r ,@new-idxs))
                     ,r))))))

           ((|::|)
            ;; (= (|::| x T) rhs)
            (let ((x (cadr (cadr e)))
                  (T (caddr (cadr e)))
                  (rhs (caddr e)))
              (let ((e (remove-argument-side-effects x)))
                (expand-forms
                 `(block ,@(cdr e)
                         (|::| ,(car e) ,T)
                         (= ,(car e) ,rhs))))))

           ((vcat)
            ;; (= (vcat . args) rhs)
            (error "use \"(a, b) = ...\" to assign multiple values"))

           (else
            (error "invalid assignment location")))))

   'abstract
   (lambda (e)
     (let ((sig (cadr e)))
       (expand-forms
        (receive (name params super) (analyze-type-sig sig)
                 (abstract-type-def-expr name params super)))))

   'bitstype
   (lambda (e)
     (let ((n (cadr e))
           (sig (caddr e)))
       (expand-forms
        (receive (name params super) (analyze-type-sig sig)
                 (bits-def-expr n name params super)))))

   'comparison
   (lambda (e)
     (expand-forms (expand-compare-chain (cdr e))))

   'ref
   (lambda (e)
     (let ((args (cddr e)))
       (if (has-parameters? args)
         (error "unexpected semicolon in array expression")
         (expand-forms (partially-expand-ref e)))))

   'curly
   (lambda (e)
     (expand-forms `(call (top apply_type) ,@(cdr e))))

   'call
   (lambda (e)
     (if (length> e 2)
         (let ((f (cadr e)))
           (cond ((and (pair? (caddr e))
                       (eq? (car (caddr e)) 'parameters))
                  ;; (call f (parameters . kwargs) ...)
                  (expand-forms
                    (receive
                      (kws args) (separate kwarg? (cdddr e))
                      (lower-kw-call f (append kws (cdr (caddr e))) args))))
                 ((any kwarg? (cddr e))
                  ;; (call f ... (kw a b) ...)
                  (expand-forms
                   (receive
                    (kws args) (separate kwarg? (cddr e))
                    (lower-kw-call f kws args))))
                 ((any vararg? (cddr e))
                  ;; call with splat
                  (let ((argl (cddr e)))
                    ;; wrap sequences of non-... arguments in tuple()
                    (define (tuple-wrap a run)
                      (if (null? a)
                          (if (null? run) '()
                              (list `(call (top tuple) ,.(reverse run))))
                          (let ((x (car a)))
                            (if (and (length= x 2)
                                     (eq? (car x) '...))
                                (if (null? run)
                                    (list* (cadr x)
                                           (tuple-wrap (cdr a) '()))
                                    (list* `(call (top tuple) ,.(reverse run))
                                           (cadr x)
                                           (tuple-wrap (cdr a) '())))
                                (tuple-wrap (cdr a) (cons x run))))))
                    (expand-forms
                     `(call (top _apply) call ,f ,@(tuple-wrap argl '())))))

                 ((and (eq? (cadr e) '*) (length= e 4))
                  (expand-transposed-op
                   e
                   #(Ac_mul_Bc Ac_mul_B At_mul_Bt At_mul_B A_mul_Bc A_mul_Bt)))
                 ((and (eq? (cadr e) '/) (length= e 4))
                  (expand-transposed-op
                   e
                   #(Ac_rdiv_Bc Ac_rdiv_B At_rdiv_Bt At_rdiv_B A_rdiv_Bc A_rdiv_Bt)))
                 ((and (eq? (cadr e) '\\) (length= e 4))
                  (expand-transposed-op
                   e
                   #(Ac_ldiv_Bc Ac_ldiv_B At_ldiv_Bt At_ldiv_B A_ldiv_Bc A_ldiv_Bt)))
                 (else
                  (map expand-forms e))))
         (map expand-forms e)))

   'tuple
   (lambda (e)
     (for-each (lambda (x)
		 ;; assignment inside tuple looks like a keyword argument
		 (if (assignment? x)
		     (error "assignment not allowed inside tuple")))
	       (cdr e))
     (expand-forms `(call (top tuple) ,@(cdr e))))

   'dict
   (lambda (e)
     ;; TODO: deprecate
     `(call (top Dict)
            ,.(map expand-forms (cdr e))))

   'typed_dict
   (lambda (e)
     ;; TODO: deprecate
     (let ((atypes (cadr e))
           (args   (cddr e)))
       (if (and (length= atypes 3)
                (eq? (car atypes) '=>))
           `(call (call (top apply_type) (top Dict)
                        ,(expand-forms (cadr atypes))
                        ,(expand-forms (caddr atypes)))
                  ,.(map expand-forms args))
           (error (string "invalid \"typed_dict\" syntax " (deparse atypes))))))

   '=>
   (lambda (e) `(call => ,(expand-forms (cadr e)) ,(expand-forms (caddr e))))

   'cell1d
   (lambda (e)
     (let ((args (cdr e)))
       (cond ((has-parameters? args)
              (error "unexpected semicolon in array expression"))
             ((any vararg? args)
              (expand-forms
               `(call (top cell_1d) ,@args)))
             (else
              (let ((name (make-jlgensym)))
                `(block (= ,name (call (top Array) (top Any)
                                       ,(length args)))
                        ,.(map (lambda (i elt)
                                 `(call (top arrayset) ,name
                                        ,(expand-forms elt) ,(+ 1 i)))
                               (iota (length args))
                               args)
                        ,name))))))

   'cell2d
   (lambda (e)
     (let ((nr (cadr e))
           (nc (caddr e))
           (args (cdddr e)))
       (if (any vararg? args)
           (expand-forms
            `(call (top cell_2d) ,nr ,nc ,@args))
           (let ((name (make-jlgensym)))
             `(block (= ,name (call (top Array) (top Any)
                                    ,nr ,nc))
                     ,.(map (lambda (i elt)
                              `(call (top arrayset) ,name
                                     ,(expand-forms elt) ,(+ 1 i)))
                            (iota (* nr nc))
                            args)
                     ,name)))))

   'string
   (lambda (e) (expand-forms `(call (top string) ,@(cdr e))))

   '|::|
   (lambda (e)
     (if (length= e 2)
         (error "invalid \"::\" syntax"))
     (if (and (length= e 3) (not (symbol-like? (cadr e))))
         `(call (top typeassert)
                ,(expand-forms (cadr e)) ,(expand-forms (caddr e)))
         (map expand-forms e)))

   'while
   (lambda (e)
     `(scope-block
       (break-block loop-exit
                    (_while ,(expand-forms (cadr e))
                            (break-block loop-cont
                                         ,(expand-forms (caddr e)))))))

   'inner-while
   (lambda (e)
     `(scope-block
        (_while ,(expand-forms (cadr e))
                (break-block loop-cont
                             ,(expand-forms (caddr e))))))

   'break
   (lambda (e)
     (if (pair? (cdr e))
         e
         '(break loop-exit)))

   'continue (lambda (e) '(break loop-cont))

   'for
   (lambda (e)
     (let nest ((ranges (if (eq? (car (cadr e)) 'block)
                            (cdr (cadr e))
                            (list (cadr e))))
                (first  #t))
       (expand-for (if first 'while 'inner-while)
                   (cadr (car ranges))
                   (caddr (car ranges))
                   (if (null? (cdr ranges))
                       (caddr e)  ;; body
                       (nest (cdr ranges) #f)))))

   '+=     lower-update-op
   '-=     lower-update-op
   '*=     lower-update-op
   '.*=    lower-update-op
   '/=     lower-update-op
   './=    lower-update-op
   '//=    lower-update-op
   './/=   lower-update-op
   '|\\=|  lower-update-op
   '|.\\=| lower-update-op
   '|.+=|  lower-update-op
   '|.-=|  lower-update-op
   '^=     lower-update-op
   '.^=    lower-update-op
   '÷=     lower-update-op
   '.÷=    lower-update-op
   '%=     lower-update-op
   '.%=    lower-update-op
   '|\|=|  lower-update-op
   '&=     lower-update-op
   '$=     lower-update-op
   '<<=    lower-update-op
   '>>=    lower-update-op
   '>>>=   lower-update-op

   ':
   (lambda (e)
     (if (or (length= e 2)
             (and (length= e 3)
                  (eq? (caddr e) ':))
             (and (length= e 4)
                  (eq? (cadddr e) ':)))
         (error "invalid \":\" outside indexing"))
     `(call colon ,.(map expand-forms (cdr e))))

   'vect
   (lambda (e) (expand-forms `(call (top vect) ,@(cdr e))))

   'hcat
   (lambda (e) (expand-forms `(call hcat ,.(map expand-forms (cdr e)))))

   'vcat
   (lambda (e)
     (let ((a (cdr e)))
       (if (has-parameters? a)
         (error "unexpected semicolon in array expression")
         (expand-forms
           (if (any (lambda (x)
                      (and (pair? x) (eq? (car x) 'row)))
                    a)
             ;; convert nested hcat inside vcat to hvcat
             (let ((rows (map (lambda (x)
                               (if (and (pair? x) (eq? (car x) 'row))
                                   (cdr x)
                                   (list x)))
                             a)))
               `(call hvcat
                   (tuple ,.(map length rows))
                     ,.(apply append rows)))
             `(call vcat ,@a))))))

   'typed_hcat
   (lambda (e) `(call (top typed_hcat) ,(expand-forms (cadr e)) ,.(map expand-forms (cddr e))))

   'typed_vcat
   (lambda (e)
     (let ((t (cadr e))
           (a (cddr e)))
       (expand-forms
        (if (any (lambda (x)
             (and (pair? x) (eq? (car x) 'row)))
           a)
            ;; convert nested hcat inside vcat to hvcat
            (let ((rows (map (lambda (x)
                               (if (and (pair? x) (eq? (car x) 'row))
                                   (cdr x)
                                   (list x)))
                             a)))
              `(call (top typed_hvcat) ,t
                     (tuple ,.(map length rows))
                     ,.(apply append rows)))
            `(call (top typed_vcat) ,t ,@a)))))

   '|'|  (lambda (e) `(call ctranspose ,(expand-forms (cadr e))))
   '|.'| (lambda (e) `(call  transpose ,(expand-forms (cadr e))))

   'ccall
   (lambda (e)
     (if (length> e 3)
         (let ((name (cadr e))
               (RT   (caddr e))
               (argtypes (cadddr e))
               (args (cddddr e)))
           (begin
             (if (not (and (pair? argtypes)
                           (eq? (car argtypes) 'tuple)))
	       (if (and (pair? RT)
			(eq? (car RT) 'tuple))
                 (error "ccall argument types must be a tuple; try \"(T,)\" and check if you specified a correct return type")
                 (error "ccall argument types must be a tuple; try \"(T,)\"")))
             (expand-forms
              (lower-ccall name RT (cdr argtypes) args))))
         e))

   'comprehension
   (lambda (e)
     (expand-forms (lower-comprehension #f       (cadr e) (cddr e))))

   'typed_comprehension
   (lambda (e)
     (expand-forms (lower-comprehension (cadr e) (caddr e) (cdddr e))))

   'dict_comprehension
   (lambda (e)
     (expand-forms (lower-dict-comprehension (cadr e) (cddr e))))

   'typed_dict_comprehension
   (lambda (e)
     (expand-forms (lower-typed-dict-comprehension (cadr e) (caddr e) (cdddr e))))))

(define (lower-nd-comprehension atype expr ranges)
  (let ((result      (make-jlgensym))
        (ri          (gensy))
        (oneresult   (gensy)))
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
                      (= ,ri (call (top +) ,ri) 1))
              `(block (call (top setindex!) ,result (ref ,expr ,@(reverse iters)) ,ri)
                      (= ,ri (call (top +) ,ri 1))) )
          (if (eq? (car ranges) `:)
              (let ((i (make-jlgensym)))
                `(for (= ,i (: 1 (call (top size) ,oneresult ,oneresult-dim)))
                      ,(construct-loops (cdr ranges) (cons i iters) (+ oneresult-dim 1)) ))
              `(for ,(car ranges)
                    ,(construct-loops (cdr ranges) iters oneresult-dim) ))))

    ;; Evaluate the comprehension
    `(scope-block
      (block
       (local ,oneresult)
       ,(evaluate-one ranges)
       (= ,result (call (top Array) ,(if atype atype `(call (top eltype) ,oneresult))
                        ,@(compute-dims ranges 1)))
       (= ,ri 1)
       ,(construct-loops (reverse ranges) (list) 1)
       ,result ))))

(define (lower-comprehension atype expr ranges)
  (if (any (lambda (x) (eq? x ':)) ranges)
      (lower-nd-comprehension atype expr ranges)
  (let ((result    (make-jlgensym))
        (ri        (gensy))
        (initlabl  (if atype #f (make-jlgensym)))
        (oneresult (make-jlgensym))
        (lengths   (map (lambda (x) (make-jlgensym)) ranges))
        (states    (map (lambda (x) (gensy)) ranges))
        (is        (map (lambda (x) (gensy)) ranges))
        (rv        (map (lambda (x) (make-jlgensym)) ranges)))

    ;; construct loops to cycle over all dimensions of an n-d comprehension
    (define (construct-loops ranges rv is states lengths)
      (if (null? ranges)
          `(block (= ,oneresult ,expr)
                  ,@(if atype '() `((type_goto ,initlabl ,oneresult)))
                  (boundscheck false)
                  (call (top setindex!) ,result ,oneresult ,ri)
                  (boundscheck pop)
                  (= ,ri (call (top +) ,ri 1)))
          `(block
            (= ,(car states) (call (top start) ,(car rv)))
            (local (:: ,(car is) (call (top typeof) ,(car lengths))))
            (= ,(car is) 0)
            (while (call (top !=) ,(car is) ,(car lengths))
                   (scope-block
                   (block
                    (= ,(car is) (call (top +) ,(car is) 1))
                    (= (tuple ,(cadr (car ranges)) ,(car states))
                       (call (top next) ,(car rv) ,(car states)))
                    ;; *** either this or force all for loop vars local
                    ,.(map (lambda (r) `(local ,r))
                           (lhs-vars (cadr (car ranges))))
                    ,(construct-loops (cdr ranges) (cdr rv) (cdr is) (cdr states) (cdr lengths))))))))

    ;; Evaluate the comprehension
    `(block
      ,.(map (lambda (v r) `(= ,v ,(caddr r))) rv ranges)
      ,.(map (lambda (v r) `(= ,v (call (top length) ,r))) lengths rv)
      (scope-block
       (block
        ,@(if atype '() `((label ,initlabl)))
        (= ,result (call (top Array)
                         ,(if atype atype `(static_typeof ,oneresult))
                         ,@lengths))
        (= ,ri 1)
        ,(construct-loops (reverse ranges) (reverse rv) is states (reverse lengths))
        ,result))))))

(define (lower-dict-comprehension expr ranges)
  (let ((result   (make-jlgensym))
        (initlabl (make-jlgensym))
        (onekey   (make-jlgensym))
        (oneval   (make-jlgensym))
        (rv         (map (lambda (x) (make-jlgensym)) ranges)))

    ;; construct loops to cycle over all dimensions of an n-d comprehension
    (define (construct-loops ranges)
      (if (null? ranges)
          `(block (= ,onekey ,(cadr expr))
                  (= ,oneval ,(caddr expr))
                  (type_goto ,initlabl ,onekey ,oneval)
                  (call (top setindex!) ,result ,oneval ,onekey))
          `(for ,(car ranges)
                (block
                 ;; *** either this or force all for loop vars local
                 ,.(map (lambda (r) `(local ,r))
                        (lhs-vars (cadr (car ranges))))
                 ,(construct-loops (cdr ranges))))))

    ;; Evaluate the comprehension
    (let ((loopranges
           (map (lambda (r v) `(= ,(cadr r) ,v)) ranges rv)))
      `(block
        ,.(map (lambda (v r) `(= ,v ,(caddr r))) rv ranges)
        (scope-block
         (block
          #;,@(map (lambda (r) `(local ,r))
          (apply append (map (lambda (r) (lhs-vars (cadr r))) ranges)))
          (label ,initlabl)
          (= ,result (call (curly (top Dict)
                                  (static_typeof ,onekey)
                                  (static_typeof ,oneval))))
          ,(construct-loops (reverse loopranges))
          ,result))))))

(define (lower-typed-dict-comprehension atypes expr ranges)
  (if (not (and (length= atypes 3)
                (eq? (car atypes) '=>)))
      (error "invalid \"typed_dict_comprehension\" syntax")
      (let ( (result (make-jlgensym))
             (rs (map (lambda (x) (make-jlgensym)) ranges)) )

        ;; construct loops to cycle over all dimensions of an n-d comprehension
        (define (construct-loops ranges rs)
          (if (null? ranges)
              `(call (top setindex!) ,result ,(caddr expr) ,(cadr expr))
              `(for (= ,(cadr (car ranges)) ,(car rs))
                    (block
                     ;; *** either this or force all for loop vars local
                     ,.(map (lambda (r) `(local ,r))
                            (lhs-vars (cadr (car ranges))))
                     ,(construct-loops (cdr ranges) (cdr rs))))))

        ;; Evaluate the comprehension
        `(block
          ,.(map make-assignment rs (map caddr ranges))
          (= ,result (call (curly (top Dict) ,(cadr atypes) ,(caddr atypes))))
          (scope-block
           (block
            #;,@(map (lambda (r) `(local ,r))
            (apply append (map (lambda (r) (lhs-vars (cadr r))) ranges)))
            ,(construct-loops (reverse ranges) (reverse rs))
            ,result))))))

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
              (if (symbol-like? (car tail))
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
  (cond ((or (atom? e) (jlgensym? e) (quoted? e))  e)
        ((and (eq? (car e) 'return) (or (symbol-like? (cadr e)) (pair? (cadr e))))
         (let ((sym (make-jlgensym)))
           `(block (= ,sym ,(cadr e))
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
   (lambda () (to-blk (to-lff e #t #t)))))

(define (to-blk r)
  (if (length= r 1) (car r) (cons 'block (reverse r))))

(define (blk-tail r) (reverse r))

;; apply to-lff to each subexpr and combine results
(define (map-to-lff e dest tail)
  (let ((r (map (lambda (arg) (to-lff arg #t #f))
                (cdr e))))
    (cond ((symbol-like? dest)
           (cons `(= ,dest ,(cons (car e) (map car r)))
                 (apply append (map cdr (reverse r)))))
          (else
           (let ((ex (cons (car e) (map car r))))
             (cons (if tail `(return ,ex) ex)
                   (apply append (map cdr (reverse r)))))))))

;; to-lff returns (new-ex . stmts) where stmts is a list of statements that
;; must run before new-ex is valid.
;;
;; If the input expression needed to be removed from its original context,
;; like the 'if' in "1+if(a,b,c)", then new-ex is a symbol holding the
;; result of the expression.
;;
;; If dest is a symbol or #f, new-ex can be a statement.
;;
;; We essentially maintain a stack of control-flow constructs that need to be
;; run in statement position as we walk around an expression. If we hit
;; statement context, we can dump the control-flow stuff there.
;; This expression walk is entirely within the "else" clause of the giant
;; case expression. Everything else deals with special forms.
(define (to-lff e dest tail)
  (if (effect-free? e)
      (cond ((symbol-like? dest)
             (if (and (pair? e) (eq? (car e) 'break))
                 ;; odd corner case: sometimes try/finally generates
                 ;; a (break ) as an assignment RHS
                 (to-lff e #f #f)
                 (cons `(= ,dest ,e) '())))
            (dest (cons (if tail `(return ,e) e)
                        '()))
            (else (cons e '())))

      (case (car e)
        ((call)  ;; ensure left-to-right evaluation of arguments
         (let ((assigned
                ;; vars assigned in each arg
                ;; start with cddr since each arg only considers subsequent ones
                (map (lambda (x) (expr-find-all assignment-like? x cadr))
                     (cddr e))))
           (if (every null? assigned)
               ;; no assignments
               (map-to-lff e dest tail)
               ;; has assignments
               (let each-arg ((ass  assigned)
                              (args (cdr e))
                              (tmp  '())
                              (newa '()))
                 ;; if an argument contains vars assigned in later arguments,
                 ;; lift out a temporary assignment for it.
                 (if (null? ass)
                     (if (null? tmp)
                         (map-to-lff e dest tail)
                         (to-lff `(block
                                   ,.(reverse tmp)
                                   (call ,.(reverse newa) ,(car args)))
                                 dest tail))
                     (if (expr-contains-p (lambda (v) (if (jlgensym? v)
                                            (any (lambda (vlist) (mem-jlgensym v vlist)) ass)
                                            (any (lambda (vlist) (memq v vlist)) ass)))
                                          (car args))
                         (let ((g (make-jlgensym)))
                           (each-arg (cdr ass) (cdr args)
                                     (cons `(= ,g ,(car args)) tmp)
                                     (cons g newa)))
                         (each-arg (cdr ass) (cdr args)
                                   tmp
                                   (cons (car args) newa))))))))

        ((=)
         (if (or (not (symbol-like? (cadr e)))
                 (eq? (cadr e) 'true)
                 (eq? (cadr e) 'false))
             (error (string "invalid assignment location \"" (deparse (cadr e)) "\"")))
         (let ((LHS (cadr e))
               (RHS (caddr e)))
           (cond ((not dest)
                  (to-lff RHS LHS #f))
                 #;((assignment? RHS)
                 (let ((r (to-lff RHS dest #f)))
                 (list* (if tail `(return ,(car r)) (car r))
                 `(= ,LHS ,(car r))
                 (cdr r))))
                 ((and (effect-free? RHS)
                       ;; need temp var for `x::Int = x` (issue #6896)
                       (not (eq? RHS (decl-var LHS))))
                  (cond ((symbol-like? dest)
                            (list `(= ,LHS ,RHS)
                                  `(= ,dest ,RHS)))
                        (dest  (list (if tail `(return ,RHS) RHS)
                                     `(= ,LHS ,RHS)))
                        (else  (list e))))
                 (else
                  (to-lff (let ((val (make-jlgensym)))
                            `(block (= ,val ,RHS)
                                    (= ,LHS ,val)
                                    ,val))
                          dest tail)))))

        ((if)
         (cond ((or (and tail (not (jlgensym? dest))) (eq? dest #f) (symbol? dest))
                (let ((r (to-lff (cadr e) #t #f)))
                  (cons `(if
                          ,(car r)
                          ,(to-blk (to-lff (caddr e) dest tail))
                          ,(if (length= e 4)
                               (to-blk (to-lff (cadddr e) dest tail))
                               (to-blk (to-lff '(null)  dest tail))))
                        (cdr r))))
               (else (let* ((g (gensy))
                            (stmts (cons g
                                         (cons `(local! ,g) (to-lff e g tail)))))
                       (if (jlgensym? dest) (cons `(= ,dest ,g) stmts) stmts)))))

        ((line)
         (set! *lff-line* (cadr e))
         (cons e '()))

        ((trycatch)
         (cond ((and (eq? dest #t) (not tail))
                (let ((g (gensy)))
                  (list* g
                         `(local! ,g)
                         (to-lff e g #f))))
               (else (let* ((g (if (jlgensym? dest) (gensy) dest))
                            (stmts (cons `(trycatch ,(fix-try-block-returns
                                                      (to-blk (to-lff (cadr e) g tail)))
                                                     ,(to-blk (to-lff (caddr e) g tail)))
                                          ())))
                       (if (jlgensym? dest) (cons `(= ,dest ,g) stmts) stmts)))))

        ((&&)
         (to-lff (expand-and e) dest tail))
        ((|\|\||)
         (to-lff (expand-or e) dest tail))

        ((block)
         (if (length= e 2)
             (to-lff (cadr e) dest tail)
             (let* ((g (make-jlgensym))
                    (stmts
                     (let loop ((tl (cdr e)))
                       (if (null? tl) '()
                           (if (null? (cdr tl))
                               (cond ((or tail (eq? dest #f) (symbol-like? dest))
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
                                (w (cons `(_while ,(to-blk (cdr r))
                                                  ,(car r)
                                                  ,(to-blk
                                                    (to-lff (caddr e) #f #f)))
                                         '())))
                           (if (symbol-like? dest)
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
         (if (and dest (not tail) (not (jlgensym? dest)))
             (let* ((g (make-jlgensym))
                    (r (to-lff (cadr e) g tail)))
               (cons (car (to-lff g dest tail))
                   (cons `(scope-block ,(to-blk r))
                         '())))
             (let ((r (to-lff (cadr e) dest tail)))
               (cons `(scope-block ,(to-blk r))
                     '()))))

        ;; move the break to the list of preceding statements. value is
        ;; null but this will never be observed.
        ((break) (cons '(null) (list e)))

        ((lambda)
         (let ((l `(lambda ,(cadr e)
                     ,(to-blk (to-lff (caddr e) #t #t)))))
           (if (symbol-like? dest)
               (cons `(= ,dest ,l) '())
               (cons (if tail `(return ,l) l) '()))))

        ((local global)
         (if dest
             (error (string "misplaced \"" (car e) "\" declaration")))
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

        ((method)
         (if (and dest (not tail))
             (let ((ex (to-lff (method-expr-name e) dest tail))
                   (fu (to-lff e #f #f)))
               (cons (car ex)
                     (append fu (cdr ex))))
             (map-to-lff e dest tail)))

        ((symbolicgoto symboliclabel)
         (cons (if tail '(return (null)) '(null))
               (map-to-lff e #f #f)))

        (else
         (map-to-lff e dest tail)))))

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
               (error (string "local \"" (car locals) "\" declared twice")))
          (check-dups (cdr locals))))
  locals)

(define (find-assigned-vars e env)
  (if (or (not (pair? e)) (quoted? e))
      '()
      (case (car e)
        ((lambda scope-block)  '())
        ((method)
         (let ((v (decl-var (method-expr-name e))))
           (if (or (not (symbol? v)) (memq v env))
               '()
               (list v))))
        ((=)
         (let ((v (decl-var (cadr e))))
           (if (or (jlgensym? v) (memq v env))
               '()
               (list v))))
        (else
         (apply append! (map (lambda (x) (find-assigned-vars x env))
                             e))))))

(define (find-decls kind e env)
  (if (or (not (pair? e)) (quoted? e))
      '()
      (cond ((or (eq? (car e) 'lambda) (eq? (car e) 'scope-block))
             '())
            ((eq? (car e) kind)
             (list (decl-var (cadr e))))
            (else
             (apply append! (map (lambda (x) (find-decls kind x env))
                                 e))))))

(define (find-local-decls  e env) (find-decls 'local  e env))
(define (find-local!-decls e env) (find-decls 'local! e env))

(define (find-locals e env glob)
  (delete-duplicates
   (append! (check-dups (find-local-decls e env))
            ;; const decls on non-globals also introduce locals
            (diff (find-decls 'const e env) glob)
            (find-local!-decls e env)
            (find-assigned-vars e env))))

(define (remove-local-decls e)
  (cond ((or (not (pair? e)) (quoted? e)) e)
        ((or (eq? (car e) 'scope-block) (eq? (car e) 'lambda)) e)
        ((eq? (car e) 'block)
         (map remove-local-decls
              (filter (lambda (x) (not (and (pair? x) (eq? (car x) 'local))))
                      e)))
        (else
         (map remove-local-decls e))))

;; local variable identification
;; convert (scope-block x) to `(scope-block ,@locals ,x)
;; where locals is a list of (local x) expressions, derived from two sources:
;; 1. (local x) expressions inside this scope-block and lambda
;; 2. (const x) expressions in a scope-block where x is not declared global
;; 3. variables assigned inside this scope-block that don't exist in outer
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
                           (cadr e) (append env glob) glob))
                    (body (add-local-decls (cadr e) (append vars glob env)))
                    (lineno (if (and (length> body 1)
                                     (pair? (cadr body))
                                     (eq? 'line (car (cadr body))))
                                (list (cadr body))
                                '()))
                    (body (if (null? lineno)
                              body
                              `(,(car body) ,@(cddr body)))))
               (for-each (lambda (v)
                           (if (memq v vars)
                               (error (string "variable \"" v "\" declared both local and global"))))
                         glob)
               `(scope-block ,@lineno
                             ;; place local decls after initial line node
                             ,.(map (lambda (v) `(local ,v))
                                    vars)
                             ,(remove-local-decls body))))
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
  (table.keys (free-vars- e (table) x)))

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
                 (body (cons 'block (cdr e))));(car (last-pair e))))
             (let* ((outer    (append usedv (vars-used-outside context e)))
                    ;; only rename conflicted vars
                    (to-ren   (filter (lambda (v) (memq v outer)) vars))
                    (newnames (map named-gensy to-ren))
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
                           (error (string "local \"" v "\" conflicts with argument"))))
                     (declared-local-vars body))
           `(lambda ,(cadr e)
              (locals ,@scope-block-vars)
              ,r-s-b)))
        (else (map (lambda (x) (if (not (pair? x)) x
                                   (flatten-scopes x)))
                   e))))

(define (has-unmatched-symbolic-goto? e)
  (let ((label-refs (table))
        (label-defs (table)))
    (find-symbolic-label-refs e label-refs)
    (find-symbolic-label-defs e label-defs)
    (any not (map (lambda (k) (get label-defs k #f))
                  (table.keys label-refs)))))

(define (symbolic-label-handler-levels e levels handler-level)
  (if (or (not (pair? e)) (quoted? e))
      '()
      (case (car e)
        ((trycatch)
         (symbolic-label-handler-levels (cadr e) levels (+ handler-level 1)))
        ((symboliclabel)
         (put! levels (cadr e) handler-level))
        (else
         (map (lambda (x) (symbolic-label-handler-levels x levels handler-level)) e)))))

(define (find-symbolic-label-defs e tbl)
  (if (or (not (pair? e)) (quoted? e))
      '()
      (if (eq? (car e) 'symboliclabel)
          (put! tbl (cadr e) #t)
          (map (lambda (x) (find-symbolic-label-defs x tbl)) e))))

(define (find-symbolic-label-refs e tbl)
  (if (or (not (pair? e)) (quoted? e))
      '()
      (if (eq? (car e) 'symbolicgoto)
          (put! tbl (cadr e) #t)
          (map (lambda (x) (find-symbolic-label-refs x tbl)) e))))

(define (find-symbolic-labels e)
  (let ((defs (table))
        (refs (table)))
    (find-symbolic-label-defs e defs)
    (find-symbolic-label-refs e refs)
    (table.foldl
     (lambda (label v labels)
       (if (has? refs label)
           (cons label labels)
           labels))
     '() defs)))

(define (rename-symbolic-labels- e relabel)
  (cond
   ((or (not (pair? e)) (quoted? e)) e)
   ((eq? (car e) 'symbolicgoto)
    (let ((newlabel (assq (cadr e) relabel)))
      (if newlabel `(symbolicgoto ,(cdr newlabel)) e)))
   ((eq? (car e) 'symboliclabel)
    (let ((newlabel (assq (cadr e) relabel)))
      (if newlabel `(symboliclabel ,(cdr newlabel)) e)))
   (else (map (lambda (x) (rename-symbolic-labels- x relabel)) e))))

(define (rename-symbolic-labels e)
  (let* ((labels (find-symbolic-labels e))
         (relabel (pair-with-gensyms labels)))
    (rename-symbolic-labels- e relabel)))

(define (max-jlgensym e)
  (if (or (not (pair? e)) (quoted? e))
      -1
      (if (jlgensym? e) (cadr e)
          (foldl (lambda (x l) (max (max-jlgensym x) l)) -1 e))))

(define (renumber-jlgensym- e tbl next-jlgensym)
  (cond
   ((or (not (pair? e)) (quoted? e)) e)
   ((jlgensym? e)
      (let ((n (get tbl (cadr e) #f)))
        (if n n
          (let ((n (next-jlgensym))) (put! tbl (cadr e) n) n))))
   (else (map (lambda (x) (renumber-jlgensym- x tbl next-jlgensym)) e))))

(define (renumber-jlgensym e)
  (let ((jlgensym-counter 0))
    (define (next-jlgensym)
      (begin0 `(jlgensym ,jlgensym-counter)
              (set! jlgensym-counter (+ 1 jlgensym-counter))))
    (renumber-jlgensym- e (table) next-jlgensym)))

(define (make-var-info name) (list name 'Any 0))
(define vinfo:name car)
(define vinfo:type cadr)
(define (vinfo:capt v) (< 0 (logand (caddr v) 1)))
(define (vinfo:asgn v) (< 0 (logand (caddr v) 2)))
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
;; whether var is assigned once
(define (vinfo:set-sa! v a) (set-car! (cddr v)
                                      (if a
                                          (logior (caddr v) 16)
                                          (logand (caddr v) -17))))

(define var-info-for assq)

(define (lambda-all-vars e)
  (append (lam:vars e)
          (cdr (caddr e))))

(define (free-vars- e tab excl)
  (cond ((symbol? e) (put! tab e #t))
        ((or (atom? e) (quoted? e)) tab)
        ((eq? e excl) tab)
        ((eq? (car e) 'lambda)
         (let ((bound (lambda-all-vars e)))
           (for-each (lambda (v) (if (not (memq v bound)) (put! tab v #t)))
                     (free-vars (lam:body e))))
         tab)
        (else
         (for-each (lambda (x) (free-vars- x tab excl))
                   (cdr e))
         tab)))

(define *free-vars-secret-value* (list 0))
(define (free-vars e)
  (table.keys (free-vars- e (table) *free-vars-secret-value*)))

(define (caddddr x) (car (cdr (cdr (cdr (cdr x))))))
; convert each lambda's (locals ...) to
;   ((localvars...) var-info-lst captured-var-infos)
; where var-info-lst is a list of var-info records
(define (analyze-vars e env captvars)
  (if (or (atom? e) (quoted? e))
      e
      (case (car e)
        ((=)
         (let ((vi (var-info-for (cadr e) env)))
           (if vi
               (begin
                 (if (vinfo:asgn vi)
                     (vinfo:set-sa! vi #f)
                     (vinfo:set-sa! vi #t))
                 (vinfo:set-asgn! vi #t)
                 (if (assq (car vi) captvars)
                     (vinfo:set-iasg! vi #t)))))
         `(= ,(cadr e) ,(analyze-vars (caddr e) env captvars)))
        #;((or (eq? (car e) 'local) (eq? (car e) 'local!))
         '(null))
        ((typeassert)
         ;(let ((vi (var-info-for (cadr e) env)))
         ;  (if vi
         ;      (begin (vinfo:set-type! vi (caddr e))
         ;           (cadr e))
         `(call (top typeassert) ,(cadr e) ,(caddr e)))
        ((decl |::|)
         ; handle var::T declaration by storing the type in the var-info
         ; record. for non-symbols or globals, emit a type assertion.
         (let ((vi (var-info-for (cadr e) env)))
           (if vi
               (begin (if (not (eq? (vinfo:type vi) 'Any))
                          (error (string "multiple type declarations for \""
                                         (cadr e) "\"")))
                      (if (assq (cadr e) captvars)
                          (error (string "type of \"" (cadr e)
                                         "\" declared in inner scope")))
                      (vinfo:set-type! vi (caddr e))
                      '(null))
               `(call (top typeassert) ,(cadr e) ,(caddr e)))))
        ((lambda)
         (let* ((args (lam:args e))
                (locl (cdr (caddr e)))
                (allv (nconc (map arg-name args) locl))
                (fv   (let* ((fv (diff (free-vars (lam:body e)) allv))
                             ;; add variables referenced in declared types for free vars
                             (dv (apply nconc (map (lambda (v)
                                                     (let ((vi (var-info-for v env)))
                                                       (if vi (free-vars (vinfo:type vi)) '())))
                                                   fv))))
                        (append (diff dv fv) fv)))
                (glo  (declared-global-vars (lam:body e)))
                ;; make var-info records for vars introduced by this lambda
                (vi   (nconc
                       (map (lambda (decl) (make-var-info (decl-var decl)))
                            args)
                       (map make-var-info locl)))
                ;; captured vars: vars from the environment that occur
                ;; in our set of free variables (fv).
                (cv    (filter (lambda (v) (and (memq (vinfo:name v) fv)
                                                (not (memq
                                                      (vinfo:name v) glo))))
                               env))
                ;; count of GenSym references in this function (or an array of their types)
                (gensym_types (+ (max-jlgensym (lam:body e)) 1))
                (bod   (analyze-vars
                        (lam:body e)
                        (append vi
                                ;; new environment: add our vars
                                (filter (lambda (v)
                                          (and
                                           (not (memq (vinfo:name v) allv))
                                           (not (memq (vinfo:name v) glo))))
                                        env))
                        cv)))
           ;; mark all the vars we capture as captured
           (for-each (lambda (v) (vinfo:set-capt! v #t))
                     cv)
           `(lambda ,args
              (,(cdaddr e) ,vi ,cv ,gensym_types)
              ,bod)))
        ((localize)
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
        ((method)
         (let ((vi (var-info-for (method-expr-name e) env)))
           (if vi
               (begin
                 (vinfo:set-asgn! vi #t)
                 ;; note: method defs require a memory loc. (issue #7658)
                 (vinfo:set-sa! vi #f)
                 (if (assq (car vi) captvars)
                     (vinfo:set-iasg! vi #t)))))
         `(method ,(cadr e)
                  ,(analyze-vars (caddr  e) env captvars)
                  ,(analyze-vars (cadddr e) env captvars)
          ,(caddddr e)))
        (else (cons (car e)
                    (map (lambda (x) (analyze-vars x env captvars))
                         (cdr e)))))))

(define (analyze-variables e) (analyze-vars e '() '()))

(define (not-bool e)
  (cond ((memq e '(true #t))  'false)
        ((memq e '(false #f)) 'true)
        (else                 `(call (top !) ,e))))

;; remove if, _while, block, break-block, and break
;; replaced with goto and gotoifnot
;; TODO: remove type-assignment-affecting expressions from conditional branch.
;;       needed because there's no program location after the condition
;;       is evaluated but before the branch's successors.
;;       pulling a complex condition out to a temporary variable creates
;;       such a location (the assignment to the variable).
(define (goto-form e)
  (cond ((or (not (pair? e)) (quoted? e)) e)
        ((eq? (car e) 'lambda)
         `(lambda ,(cadr e) ,(caddr e)
                  ,(compile-body (cadddr e) (append (cadr (caddr e))
                                                    (caddr (caddr e))))))
        (else (cons (car e)
                    (map goto-form (cdr e))))))

(define (compile-body e vi)
  (let ((code '())
        (label-counter 0)
        (label-map (table))
        (label-decl (table))
        (label-level (table))
        (handler-level 0))
    (symbolic-label-handler-levels e label-level 0)
    (define (emit c)
      (set! code (cons c code)))
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
    (define (compile e break-labels)
      (if (or (not (pair? e)) (equal? e '(null)))
          ;; atom has no effect, but keep symbols for undefined-var checking
          #f #;(if (symbol? e) (emit e) #f)
          (case (car e)
            ((call)  (emit (goto-form e)))
            ((=)     (let ((vt (vinfo:type
                                (or (var-info-for (cadr e) vi) '(#f Any)))))
                       (if (not (eq? vt 'Any))
                           (emit `(= ,(cadr e)
                                     (call (top typeassert)
                                           (call (top convert) ,vt
                                                 ,(goto-form (caddr e)))
                                           ,vt)))
                           (emit `(= ,(cadr e) ,(goto-form (caddr e)))))))
            ((if) (let ((test     `(gotoifnot ,(goto-form (cadr e)) _))
                        (end-jump `(goto _))
                        (tail     (and (pair? (caddr e))
                                       (eq? (car (caddr e)) 'return))))
                    (emit test)
                    (compile (caddr e) break-labels)
                    (if (and (not tail)
                             (not (equal? (cadddr e) '(null))))
                        (emit end-jump))
                    (set-car! (cddr test) (make&mark-label))
                    (compile (cadddr e) break-labels)
                    (if (not tail)
                        (set-car! (cdr end-jump) (make&mark-label)))))
            ((block) (for-each (lambda (x) (compile x break-labels))
                               (cdr e)))
            ((_while)
             (let ((test-blk (cadr e))
                   (endl (make-label)))
               (if (or (atom? test-blk) (equal? test-blk '(block)))
                   ;; if condition is simple, compile it twice in order
                   ;; to generate a single branch per iteration.
                   (let ((topl (make-label)))
                     (compile test-blk break-labels)
                     (emit `(gotoifnot ,(goto-form (caddr e)) ,endl))
                     (mark-label topl)
                     (compile (cadddr e) break-labels)
                     (compile test-blk break-labels)
                     (emit `(gotoifnot ,(not-bool (goto-form (caddr e))) ,topl))
                     (mark-label endl))

                   (let ((topl (make&mark-label)))
                     (compile test-blk break-labels)
                     (emit `(gotoifnot ,(goto-form (caddr e)) ,endl))
                     (compile (cadddr e) break-labels)
                     (emit `(goto ,topl))
                     (mark-label endl)))))

            ((break-block) (let ((endl (make-label)))
                             (compile (caddr e)
                                      (cons (list (cadr e) endl handler-level)
                                            break-labels))
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
            ((label) (let ((m (get label-map (cadr e) #f)))
                       (if m
                           (emit `(label ,m))
                           (let ((l (make&mark-label)))
                             (put! label-map (cadr e) l)))
                       (put! label-decl (cadr e) #t)))
            ((symboliclabel) (let ((m (get label-map (cadr e) #f)))
                               (if m
                                   (if (get label-decl (cadr e) #f)
                                       (error (string "label \"" (cadr e) "\" defined multiple times"))
                                       (emit `(label ,m)))
                                   (let ((l (make&mark-label)))
                                     (put! label-map (cadr e) l)))
                               (put! label-decl (cadr e) #t)))
            ((symbolicgoto) (let ((m (get label-map (cadr e) #f))
                                  (target-level (get label-level (cadr e) #f)))
                              (cond
                               ((not target-level)
                                (error (string "label \"" (cadr e) "\" referenced but not defined")))
                               ((> target-level handler-level)
                                (error (string "cannot goto label \"" (cadr e) "\" inside try/catch block")))
                               ((< target-level handler-level)
                                (emit `(leave ,(- handler-level target-level)))))
                              (if m
                                  (emit `(goto ,m))
                                  (let ((l (make-label)))
                                    (put! label-map (cadr e) l)
                                    (emit `(goto ,l))))))
            ((type_goto) (let((m (get label-map (cadr e) #f)))
                           (if m
                               (emit `(type_goto ,m ,@(cddr e)))
                               (let ((l (make-label)))
                                 (put! label-map (cadr e) l)
                                 (emit `(type_goto ,l ,@(cddr e)))))))
            ;; exception handlers are lowered using
            ;; (enter L) - push handler with catch block at label L
            ;; (leave n) - pop N exception handlers
            ;; (the_exception) - get the thrown object
            ((trycatch)
             (let ((catch (make-label))
                   (endl  (make-label)))
               (emit `(enter ,catch))
               (set! handler-level (+ handler-level 1))
               (compile (cadr e) break-labels)
               (set! handler-level (- handler-level 1))
               (if (not (and (pair? (car code)) (eq? (caar code) 'return)))
                   ;; try ends in return, no need to handle flow off end of it
                   (begin (emit `(leave 1))
                          (emit `(goto ,endl)))
                   (set! endl #f))
               (mark-label catch)
               (emit `(leave 1))
               (compile (caddr e) break-labels)
               (if endl
                   (mark-label endl))
               ))

            ((global) #f)  ; remove global declarations
            ((local!) #f)
            ((jlgensym) #f)
            ((local)
             ;; emit (newvar x) where captured locals are introduced.
             (let* ((vname (cadr e))
                    (vinf  (var-info-for vname vi)))
               (if (and vinf
                        (not (and (pair? code)
                                  (equal? (car code) `(newvar ,vname))))
                        ;; TODO: remove the following expression to re-null
                        ;; all variables when they are allocated. see issue #1571
                        (vinfo:capt vinf)
                        )
                   (emit `(newvar ,vname))
                   #f)))
            ((newvar)
             ;; avoid duplicate newvar nodes
             (if (not (and (pair? code) (equal? (car code) e)))
                 (emit e)
                 #f))
            (else  (emit (goto-form e))))))
    (compile e '())
    (cons 'body (reverse! code))))

(define to-goto-form goto-form)

;; macro expander

(define (splice-expr? e)
  ;; ($ (tuple (... x)))
  (and (length= e 2)          (eq? (car e)   '$)
       (length= (cadr e) 2)   (eq? (caadr e) 'tuple)
       (vararg? (cadadr e))))

(define (wrap-with-splice x)
  `(call (top _expr) (inert $)
	 (call (top _expr) (inert tuple)
	       (call (top _expr) (inert |...|) ,x))))

(define (julia-bq-bracket x d)
  (if (splice-expr? x)
      (if (= d 0)
	  (cadr (cadr (cadr x)))
	  (list 'cell1d
		(wrap-with-splice (julia-bq-expand (cadr (cadr (cadr x))) (- d 1)))))
      (list 'cell1d (julia-bq-expand x d))))

(define (julia-bq-expand x d)
  (cond ((or (eq? x 'true) (eq? x 'false))  x)
	((or (symbol? x) (jlgensym? x))     (list 'inert x))
        ((atom? x)  x)
        ((eq? (car x) 'quote)
	 `(call (top _expr) (inert quote) ,(julia-bq-expand (cadr x) (+ d 1))))
        ((eq? (car x) '$)
	 (if (and (= d 0) (length= x 2))
	     (cadr x)
	     (if (splice-expr? (cadr x))
		 `(call (top splicedexpr) (inert $)
			(call (top append_any) ,(julia-bq-bracket (cadr x) (- d 1))))
		 `(call (top _expr) (inert $) ,(julia-bq-expand (cadr x) (- d 1))))))
        ((not (contains (lambda (e) (and (pair? e) (eq? (car e) '$))) x))
         `(copyast (inert ,x)))
	((not (any splice-expr? x))
	 `(call (top _expr) ,.(map (lambda (ex) (julia-bq-expand ex d)) x)))
	(else
	 (let loop ((p (cdr x)) (q '()))
	   (if (null? p)
	       (let ((forms (reverse q)))
		 `(call (top splicedexpr) ,(julia-bq-expand (car x) d)
			(call (top append_any) ,@forms)))
	       (loop (cdr p) (cons (julia-bq-bracket (car p) d) q)))))))

(define (julia-expand-macros e)
  (cond ((not (pair? e))     e)
        ((eq? (car e) 'quote)
         ;; backquote is essentially a built-in macro at the moment
         (julia-expand-macros (julia-bq-expand (cadr e) 0)))
        ((eq? (car e) 'inert)
         e)
        ((eq? (car e) 'macrocall)
         ;; expand macro
         (let ((form
		(if (and (length> e 2) (pair? (caddr e)) (eq? (caaddr e) 'triple_quoted_string))
		    ;; for a custom triple-quoted string literal, first invoke mstr
		    ;; to handle unindenting
		    (apply invoke-julia-macro (cadr e)
			   (julia-expand-macros `(macrocall @mstr ,(cadr (caddr e))))
			   (cdddr e))
		    (apply invoke-julia-macro (cadr e) (cddr e)))))
           (if (not form)
               (error (string "macro \"" (cadr e) "\" not defined")))
           (if (and (pair? form) (eq? (car form) 'error))
               (error (cadr form)))
           (let ((form (car form))
                 (m    (cdr form)))
             ;; m is the macro's def module, or #f if def env === use env
             (rename-symbolic-labels
              (julia-expand-macros
               (resolve-expansion-vars form m))))))
        (else
         (map julia-expand-macros e))))

(define (pair-with-gensyms v)
  (map (lambda (s)
         (if (pair? s)
             s
             (cons s (named-gensy s))))
       v))

(define (unescape e)
  (if (and (pair? e) (eq? (car e) 'escape))
      (cadr e)
      e))

(define (typevar-expr-name e)
  (if (symbol? e) e
      (cadr e)))

(define (new-expansion-env-for x env)
  (append!
   (filter (lambda (v)
             (not (assq (car v) env)))
           (append!
            (pair-with-gensyms (vars-introduced-by x))
            (map (lambda (v) (cons v v))
                 (keywords-introduced-by x))))
   env))

(define (resolve-expansion-vars-with-new-env x env m inarg)
  (resolve-expansion-vars-
   x
   (if (and (pair? x) (eq? (car x) 'let))
       ;; let is strange in that it needs both old and new envs within
       ;; the same expression
       env
       (new-expansion-env-for x env))
   m inarg))

(define (resolve-expansion-vars- e env m inarg)
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
           ((jlgensym) e)
           ((escape) (cadr e))
           ((using import importall export) (map unescape e))
           ((macrocall)
        (if (or (eq? (cadr e) '@label) (eq? (cadr e) '@goto)) e
            `(macrocall ,.(map (lambda (x)
                                 (resolve-expansion-vars- x env m inarg))
                               (cdr e)))))
           ((symboliclabel) e)
           ((symbolicgoto) e)
           ((type)
            `(type ,(cadr e) ,(resolve-expansion-vars- (caddr e) env m inarg)
                   ;; type has special behavior: identifiers inside are
                   ;; field names, not expressions.
                   ,(map (lambda (x)
                           (cond ((atom? x) x)
                                 ((and (pair? x) (eq? (car x) '|::|))
                                  `(|::| ,(cadr x)
                                    ,(resolve-expansion-vars- (caddr x) env m inarg)))
                                 (else
                                  (resolve-expansion-vars-with-new-env x env m inarg))))
                         (cadddr e))))

           ((parameters)
            (cons 'parameters
                  (map (lambda (x)
                         (resolve-expansion-vars- x env m #f))
                       (cdr e))))

           ((= function)
            (if (and (pair? (cadr e)) (eq? (caadr e) 'call))
                ;; in (kw x 1) inside an arglist, the x isn't actually a kwarg
                `(,(car e) (call ,(resolve-expansion-vars-with-new-env (cadadr e) env m inarg)
                                 ,@(map (lambda (x)
                                          (resolve-expansion-vars-with-new-env x env m #t))
                                        (cddr (cadr e))))
                  ,(resolve-expansion-vars-with-new-env (caddr e) env m inarg))
                `(,(car e) ,@(map (lambda (x)
                                    (resolve-expansion-vars-with-new-env x env m inarg))
                                  (cdr e)))))

           ((kw)
            (if (and (pair? (cadr e))
                     (eq? (caadr e) '|::|))
                `(kw (|::|
                      ,(if inarg
                           (resolve-expansion-vars- (cadr (cadr e)) env m inarg)
                           ;; in keyword arg A=B, don't transform "A"
                           (cadr (cadr e)))
                      ,(resolve-expansion-vars- (caddr (cadr e)) env m inarg))
                     ,(resolve-expansion-vars- (caddr e) env m inarg))
                `(kw ,(if inarg
                          (resolve-expansion-vars- (cadr e) env m inarg)
                          (cadr e))
                     ,(resolve-expansion-vars- (caddr e) env m inarg))))

           ((let)
            (let* ((newenv (new-expansion-env-for e env))
                   (body   (resolve-expansion-vars- (cadr e) newenv m inarg)))
              `(let ,body
                 ,@(map
                    (lambda (bind)
                      (if (assignment? bind)
                          (make-assignment
                           ;; expand binds in old env with dummy RHS
                           (cadr (resolve-expansion-vars- (make-assignment (cadr bind) 0)
                                                          newenv m inarg))
                           ;; expand initial values in old env
                           (resolve-expansion-vars- (caddr bind) env m inarg))
                          bind))
                    (cddr e)))))

           ;; todo: trycatch
           (else
            (cons (car e)
                  (map (lambda (x)
                         (resolve-expansion-vars-with-new-env x env m inarg))
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

(define (keywords-introduced-by e)
  (let ((v (pattern-expand1 keywords-introduced-by-patterns e)))
    (if (and (pair? v) (eq? (car v) 'varlist))
        (cdr v)
        '())))

(define (env-for-expansion e)
  (let ((globals (find-declared-vars-in-expansion e 'global)))
    (let ((v (diff (delete-duplicates
                    (append! (find-declared-vars-in-expansion e 'local)
                             (find-assigned-vars-in-expansion e)
                             (map (lambda (x)
                                    (if (pair? x) (car x) x))
                                  (vars-introduced-by e))))
                   globals)))
      (append!
       (pair-with-gensyms v)
       (map (lambda (s) (cons s s))
            (diff (keywords-introduced-by e) globals))))))

(define (resolve-expansion-vars e m)
  ;; expand binding form patterns
  ;; keep track of environment, rename locals to gensyms
  ;; and wrap globals in (getfield module var) for macro's home module
  (resolve-expansion-vars- e (env-for-expansion e) m #f))

;; expander entry point

(define (julia-expand1 ex)
  (to-goto-form
    (analyze-variables
     (renumber-jlgensym
      (flatten-scopes
       (identify-locals ex))))))

(define (julia-expand01 ex)
  (to-LFF
   (expand-forms
    (expand-binding-forms ex))))

(define (julia-expand0 ex)
  (let ((e (julia-expand-macros ex)))
    (if (and (pair? e) (eq? (car e) 'toplevel))
        `(toplevel ,.(map julia-expand01 (cdr e)))
        (julia-expand01 e))))

(define (julia-expand ex)
  (julia-expand1 (julia-expand0 ex)))
