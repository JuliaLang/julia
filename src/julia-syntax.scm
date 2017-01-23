;; ignored variable name. TODO replace with _?
(define UNUSED '|#unused#|)

;; pass 1: syntax desugaring

;; allow (:: T) => (:: #gensym T) in formal argument lists
(define (fill-missing-argname a unused)
  (if (and (pair? a) (eq? (car a) '|::|) (null? (cddr a)))
      `(|::| ,(if unused UNUSED (gensy)) ,(cadr a))
      a))
(define (fix-arglist l (unused #t))
  (if (any vararg? (butlast l))
      (error "invalid ... on non-final argument"))
  (map (lambda (a)
         (cond ((and (pair? a) (eq? (car a) 'kw))
                `(kw ,(fill-missing-argname (cadr a) unused) ,(caddr a)))
               ((and (pair? a) (eq? (car a) '...))
                `(... ,(fill-missing-argname (cadr a) unused)))
               (else
                (fill-missing-argname a unused))))
       l))

;; identify some expressions that are safe to repeat
(define (effect-free? e)
  (or (not (pair? e)) (ssavalue? e) (sym-dot? e) (quoted? e) (equal? e '(null))))

;; expanding comparison chains: (comparison a op b op c ...)

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

;; generate first comparison call, converting e.g. (a < b < c)
;; to ((call < a b) b < c)
(define (compare-one e)
  (let* ((arg   (caddr e))
         (arg2  (if (and (pair? arg)
                         (pair? (cdddr e)))
                    (make-ssavalue) arg)))
    (if (and (not (dotop? (cadr e)))
             (length> e 5)
             (pair? (cadddr (cdr e)))
             (dotop? (cadddr (cddr e))))
        ;; look ahead: if the 2nd argument of the next comparison is also
        ;; an argument to an eager (dot) op, make sure we don't skip the
        ;; initialization of its variable by short-circuiting
        (let ((s (make-ssavalue)))
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
              (lambda (a b) `(call .& ,a ,b))
              (lambda (x) (not (length> x 2)))
              (lambda (e)
                (if (dotop? (cadr e))
                    (compare-one e)
                    (expand-scalar-compare e)))))

(define (expand-compare-chain e)
  (car (expand-vector-compare e)))

;; return the appropriate computation for an `end` symbol for indexing
;; the array `a` in the `n`th index.
;; `tuples` are a list of the splatted arguments that precede index `n`
;; `last` = is this last index?
;; returns a call to endof(a), trailingsize(a,n), or size(a,n)
(define (end-val a n tuples last)
  (if (null? tuples)
      (if last
          (if (= n 1)
              `(call (top endof) ,a)
              `(call (top trailingsize) ,a ,n))
          `(call (top size) ,a ,n))
      (let ((dimno `(call (top +) ,(- n (length tuples))
                          ,.(map (lambda (t) `(call (top length) ,t))
                                 tuples))))
        (if last
            `(call (top trailingsize) ,a ,dimno)
            `(call (top size) ,a ,dimno)))))

;; replace `end` for the closest ref expression, so doesn't go inside nested refs
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

;; go through indices and replace the `end` symbol
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
                  (let ((g (make-ssavalue)))
                    (loop (cdr lst) (+ n 1)
                          (cons `(= ,g ,(replace-end (cadr idx) a n tuples last))
                                stmts)
                          (cons g tuples)
                          (cons `(... ,g) ret))))
              (loop (cdr lst) (+ n 1)
                    stmts tuples
                    (cons (replace-end idx a n tuples last) ret)))))))

;; GF method does not need to keep decl expressions on lambda args
;; except for rest arg
(define (method-lambda-expr argl body rett)
  (let ((argl (map arg-name argl))
        (body (if (and (pair? body) (eq? (car body) 'block))
                  (if (null? (cdr body))
                      `(block (null))
                      body)
                  `(block ,body))))
    `(lambda ,argl ()
             (scope-block
              ,(if (eq? rett 'Any)
                   body
                   (let ((meta (take-while (lambda (x) (and (pair? x)
                                                            (memq (car x) '(line meta))))
                                           (cdr body)))
                         (val (last body)))
                     ;; wrap one-liners in `convert` instead of adding an ssavalue
                     (if (and (length= (cdr body) (+ 1 (length meta)))
                              (not (expr-contains-p return? (if (return? val)
                                                                (cadr val)
                                                                val))))
                         `(,(car body) ,@meta
                           ,(if (return? val)
                                `(return ,(convert-for-type-decl (cadr val) rett))
                                (convert-for-type-decl val rett)))
                         (let ((R (make-ssavalue)))
                           `(,(car body) ,@meta
                             (= ,R ,rett)
                             (meta ret-type ,R)
                             ,@(list-tail body (+ 1 (length meta))))))))))))

;; convert x<:T<:y etc. exprs into (name lower-bound upper-bound)
;; a bound is #f if not specified
(define (analyze-typevar e)
  (cond ((symbol? e)  (list e #f #f))
        ((eq? (car e) 'var-bounds)  (cdr e))
        ((and (eq? (car e) 'comparison) (length= e 6))
         (cons (cadddr e)
               (cond ((and (eq? (caddr e) '|<:|) (eq? (caddr (cddr e)) '|<:|))
                      (list (cadr e) (last e)))
                     (else (error "invalid bounds in \"where\"")))))
        ((eq? (car e) '|<:|)
         (list (cadr e) #f (caddr e)))
        ((eq? (car e) '|>:|)
         (list (cadr e) (caddr e) #f))
        (else (error "invalid variable expression in \"where\""))))

(define (sparam-name-bounds params)
  (let ((bounds (map analyze-typevar params)))
    (values (map car bounds) bounds)))

;; construct expression to allocate a TypeVar
(define (bounds-to-TypeVar v)
  (let ((v  (car v))
        (lb (cadr v))
        (ub (caddr v)))
    `(call (core TypeVar) ',v
           ,@(if ub
                 (if lb (list lb ub) (list ub))
                 (if lb (list lb '(core Any)) '())))))

(define (method-expr-name m)
  (let ((name (cadr m)))
       (cond ((not (pair? name)) name)
             ((eq? (car name) 'outerref) (cadr name))
             ;((eq? (car name) 'globalref) (caddr name))
             (else name))))

;; extract static parameter names from a (method ...) expression
(define (method-expr-static-parameters m)
  (if (eq? (car (caddr m)) 'block)
      (let ((lst '()))
        (pattern-replace
         (pattern-set
          (pattern-lambda (= v (call (core (-/ TypeVar)) (quote T) ...))
                          (begin (set! lst (cons T lst)) __)))
         (butlast (cdr (caddr m))))
        (reverse! lst))
      '()))

;; expressions of the form a.b.c... where everything is a symbol
(define (sym-ref? e)
  (or (symbol? e)
      (and (length= e 3) (eq? (car e) 'globalref))
      (and (length= e 2) (eq? (car e) 'outerref))
      (and (length= e 3) (eq? (car e) '|.|)
           (or (atom? (cadr e)) (sym-ref? (cadr e)))
           (pair? (caddr e)) (memq (car (caddr e)) '(quote inert))
           (symbol? (cadr (caddr e))))))

;; e.g. Base.(:+) is deprecated in favor of Base.:+
(define (deprecate-dotparen e)
  (if (and (length= e 3) (eq? (car e) '|.|)
           (or (atom? (cadr e)) (sym-ref? (cadr e)))
           (length= (caddr e) 2) (eq? (caaddr e) 'tuple)
           (pair? (cadr (caddr e))) (memq (caadr (caddr e)) '(quote inert)))
      (let* ((s_ (cdadr (caddr e)))
             (s (if (symbol? s_) s_
                    (if (and (length= s_ 1) (symbol? (car s_))) (car s_) #f))))
        (if s
            (let ((newe (list (car e) (cadr e) (cadr (caddr e))))
                  (S (deparse `(quote ,s)))) ; #16295
              (syntax-deprecation #f (string (deparse (cadr e)) ".(" S ")")
                                  (string (deparse (cadr e)) "." S))
              newe)
            e))
      e))

;; convert final (... x) to (curly Vararg x)
(define (dots->vararg a)
  (if (null? a) a
      (let ((head (butlast a))
            (las  (last a)))
        (if (vararg? las)
            `(,@head (curly Vararg ,(cadr las)))
            `(,@head ,las)))))

(define (hidden-name? s)
  (and (symbol? s)
       (eqv? (string.char (string s) 0) #\#)))

(define (is-call-name? name)
  (or (eq? name 'call) (and (pair? name) (sym-ref? name)
                            (equal? (caddr name) '(inert call)))))

(define (replace-vars e renames)
  (cond ((symbol? e)      (lookup e renames e))
        ((or (not (pair? e)) (quoted? e))  e)
        ((memq (car e) '(-> function scope-block)) e)
        (else
         (cons (car e)
               (map (lambda (x) (replace-vars x renames))
                    (cdr e))))))

(define (replace-outer-vars e renames)
  (cond ((and (pair? e) (eq? (car e) 'outerref)) (lookup (cadr e) renames e))
        ((or (not (pair? e)) (quoted? e))  e)
        ((memq (car e) '(-> function scope-block)) e)
        (else
         (cons (car e)
               (map (lambda (x) (replace-outer-vars x renames))
                    (cdr e))))))

;; construct the (method ...) expression for one primitive method definition,
;; assuming optional and keyword args are already handled
(define (method-def-expr- name sparams argl body isstaged (rett 'Any))
  (if
   (any kwarg? argl)
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
        (receive
         (vararg req) (separate vararg? argl)
         (optional-positional-defs name sparams req opt dfl body isstaged
                                   (append req opt vararg) rett)))))
   ;; no optional positional args
   (let ((names (map car sparams)))
     (let ((anames (llist-vars argl)))
       (if (has-dups (filter (lambda (x) (not (eq? x UNUSED))) anames))
           (error "function argument names not unique"))
       (if (has-dups names)
           (error "function static parameter names not unique"))
       (if (any (lambda (x) (and (not (eq? x UNUSED)) (memq x names))) anames)
           (error "function argument and static parameter names must be distinct")))
     (if (or (and name (not (sym-ref? name))) (eq? name 'true) (eq? name 'false))
         (error (string "invalid function name \"" (deparse name) "\"")))
     (let* ((iscall (is-call-name? name))
            (name  (if iscall #f name))
            (types (llist-types argl))
            (body  (method-lambda-expr argl body rett))
            ;; HACK: the typevars need to be bound to ssavalues, since this code
            ;; might be moved to a different scope by closure-convert.
            (temps (map (lambda (x) (make-ssavalue)) names))
            (renames (map cons names temps))
            (mdef
             (if (null? sparams)
                 `(method ,name (call (core svec) (call (core svec) ,@(dots->vararg types)) (call (core svec)))
                          ,body ,isstaged)
                 `(method ,name
                          (block
                           ,@(let loop ((n       names)
                                        (t       temps)
                                        (sp      (map bounds-to-TypeVar sparams))
                                        (ren     '())
                                        (assigns '()))
                               (if (null? n)
                                   (reverse! assigns)
                                   (loop (cdr n) (cdr t) (cdr sp)
                                         (cons (cons (car n) (car t)) ren)
                                         ;; each static param can see just the previous ones
                                         (cons (make-assignment (car t) (replace-vars (car sp) ren))
                                               assigns))))
                           (call (core svec) (call (core svec)
                                                   ,@(dots->vararg
                                                      (map (lambda (ty)
                                                             (replace-vars ty renames))
                                                           types)))
                                 (call (core svec) ,@temps)))
                          ,body ,isstaged))))
       (if (and iscall (not (null? argl)))
           (let* ((n (arg-name (car argl)))
                  (n (if (hidden-name? n) "" n))
                  (t (deparse (arg-type (car argl)))))
             (syntax-deprecation #f
                                 (string "call(" n "::" t ", ...)")
                                 (string "(" n "::" t ")(...)"))))
       (if (symbol? name)
           `(block (method ,name) ,mdef (unnecessary ,name))  ;; return the function
           mdef)))))

;; keyword default values that can be assigned right away. however, this creates
;; a quasi-bug (part of issue #9535) where it can be hard to predict when a
;; keyword argument will throw an UndefVarError.
(define (const-default? x)
  (or (number? x) (string? x) (char? x) (and (pair? x) (memq (car x) '(quote inert)))
      (eq? x 'true) (eq? x 'false)))

(define empty-vector-any '(call (core AnyVector) 0))

(define (keywords-method-def-expr name sparams argl body isstaged rett)
  (let* ((kargl (cdar argl))  ;; keyword expressions (= k v)
         (pargl (cdr argl))   ;; positional args
         (body  (if (and (pair? body) (eq? (car body) 'block))
                    body
                    `(block ,body)))
         (ftype (decl-type (car pargl)))
         ;; 1-element list of vararg argument, or empty if none
         (vararg (let ((l (if (null? pargl) '() (last pargl))))
                   (if (or (vararg? l) (varargexpr? l))
                       (list l) '())))
         ;; positional args without vararg
         (pargl (if (null? vararg) pargl (butlast pargl)))
         ;; positional args with everything required; for use by the core function
         (not-optional (map (lambda (a)
                              (if (kwarg? a) (cadr a) a))
                            pargl))
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
         ;; list of function's initial line number and meta nodes (empty if none)
         (prologue (extract-method-prologue body))
         ;; body statements
         (stmts (cdr body))
         (positional-sparams
          (filter (lambda (s)
                    (let ((name (car s)))
                      (or (expr-contains-eq name (cons 'list pargl))
                          (and (pair? vararg) (expr-contains-eq name (car vararg)))
                          (not (expr-contains-eq name (cons 'list kargl))))))
                  sparams))
         (keyword-sparams
          (filter (lambda (s)
                    (not (any (lambda (p) (eq? (car p) (car s)))
                              positional-sparams)))
                  sparams)))
    (let ((kw (gensy)) (i (gensy)) (ii (gensy)) (elt (gensy))
          (rkw (if (null? restkw) '() (symbol (string (car restkw) "..."))))
          (mangled (symbol (string "#" (if name (undot-name name) 'call) "#"
                                   (string (current-julia-module-counter)))))
          (flags (map (lambda (x) (gensy)) vals)))
      `(block
        ;; call with no keyword args
        ,(method-def-expr-
          name positional-sparams (append pargl vararg)
          `(block
            ,@prologue
            ,@(if (not ordered-defaults)
                  '()
                  (append! (map (lambda (kwname) `(local ,kwname)) keynames)
                           (map make-assignment keynames vals)))
            ;; call mangled(vals..., [rest_kw,] pargs..., [vararg]...)
            (return (call ,mangled
                          ,@(if ordered-defaults keynames vals)
                          ,@(if (null? restkw) '() (list empty-vector-any))
                          ,@(map arg-name pargl)
                          ,@(if (null? vararg) '()
                                (list `(... ,(arg-name (car vararg))))))))
          #f)

        ;; call with keyword args pre-sorted - original method code goes here
        ,(method-def-expr-
          mangled sparams
          `((|::| ,mangled (call (core typeof) ,mangled)) ,@vars ,@restkw
            ;; strip type off function self argument if not needed for a static param.
            ;; then it is ok for cl-convert to move this definition above the original def.
            ,(if (decl? (car not-optional))
                 (if (any (lambda (sp)
                            (expr-contains-eq (car sp) (caddr (car not-optional))))
                          positional-sparams)
                     (car not-optional)
                     (decl-var (car not-optional)))
                 (car not-optional))
            ,@(cdr not-optional) ,@vararg)
          `(block
            ,@stmts) isstaged rett)

        ;; call with unsorted keyword args. this sorts and re-dispatches.
        ,(method-def-expr-
          name
          (filter ;; remove sparams that don't occur, to avoid printing the warning twice
           (lambda (s) (expr-contains-eq (car s) (cons 'list argl)))
           positional-sparams)
          `((|::|
             ;; if there are optional positional args, we need to be able to reference the function name
             ,(if (any kwarg? pargl) (gensy) UNUSED)
             (call (core kwftype) ,ftype)) (:: ,kw (core AnyVector)) ,@pargl ,@vararg)
          `(block
            ;; initialize keyword args to their defaults, or set a flag telling
            ;; whether this keyword needs to be set.
            ,@(map (lambda (kwname) `(local ,kwname)) keynames)
            ,@(map (lambda (name dflt flag)
                     (if (const-default? dflt)
                         `(= ,name ,dflt)
                         `(= ,flag true)))
                   keynames vals flags)
            ,@(if (null? restkw) '()
                  `((= ,rkw ,empty-vector-any)))
            ;; for i = 1:(length(kw)>>1)
            (for (= ,i (: 1 (call (top >>) (call (top length) ,kw) 1)))
                 (block
                  ;; ii = i*2 - 1
                  (= ,ii (call (top -) (call (top *) ,i 2) 1))
                  (= ,elt (call (core arrayref) ,kw ,ii))
                  ,(foldl (lambda (kvf else)
                            (let* ((k    (car kvf))
                                   (rval0 `(call (core arrayref) ,kw
                                                 (call (top +) ,ii 1)))
                                   ;; note: if the "declared" type of a KW arg
                                   ;; includes something from keyword-sparams
                                   ;; then don't assert it here, since those static
                                   ;; parameters don't have values yet.
                                   ;; instead, the type will be picked up when the
                                   ;; underlying method is called.
                                   (rval (if (and (decl? k)
                                                  (not (any (lambda (s)
                                                              (expr-contains-eq (car s) (caddr k)))
                                                            keyword-sparams)))
                                             `(call (core typeassert)
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
                              `(call (top kwerr) ,kw ,@(map arg-name pargl)
                                     ,@(if (null? vararg) '()
                                           (list `(... ,(arg-name (car vararg))))))
                              ;; otherwise add to rest keywords
                              `(foreigncall 'jl_array_ptr_1d_push (core Void) (call (core svec) Any Any)
                                      ,rkw 0 (tuple ,elt
                                                  (call (core arrayref) ,kw
                                                        (call (top +) ,ii 1))) 0))
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
        ,(if (or (not (symbol? name)) (is-call-name? name))
             '(null) name)))))

;; prologue includes line number node and eventual meta nodes
(define (extract-method-prologue body)
  (if (pair? body)
      (take-while (lambda (e)
                    (and (pair? e) (or (eq? (car e) 'line) (eq? (car e) 'meta))))
                  (cdr body))
      '()))

(define (optional-positional-defs name sparams req opt dfl body isstaged overall-argl rett)
  (let ((prologue (extract-method-prologue body)))
    `(block
      ,@(map (lambda (n)
               (let* ((passed (append req (list-head opt n)))
                      ;; only keep static parameters used by these arguments
                      (sp     (filter (lambda (sp)
                                        (contains (lambda (e) (eq? e (car sp)))
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
                             ,@prologue
                             (call ,(arg-name (car req)) ,@(map arg-name (cdr passed)) ,(car vals)))
                           ;; otherwise add all
                           `(block
                             ,@prologue
                             (call ,(arg-name (car req)) ,@(map arg-name (cdr passed)) ,@vals)))))
                 (method-def-expr- name sp passed body #f)))
             (iota (length opt)))
      ,(method-def-expr- name sparams overall-argl body isstaged rett))))

;; strip empty (parameters ...), normalizing `f(x;)` to `f(x)`.
(define (remove-empty-parameters argl)
  (if (and (has-parameters? argl) (null? (cdar argl)))
      (cdr argl)
      argl))

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

;; method-def-expr checks for keyword arguments, and if there are any, calls
;; keywords-method-def-expr to expand the definition into several method
;; definitions that do not use keyword arguments.
;; definitions without keyword arguments are passed to method-def-expr-,
;; which handles optional positional arguments by adding the needed small
;; boilerplate definitions.
(define (method-def-expr name sparams argl body isstaged rett)
  (let ((argl (remove-empty-parameters argl)))
    (if (has-parameters? argl)
        ;; has keywords
        (begin (check-kw-args (cdar argl))
               (keywords-method-def-expr name sparams argl body isstaged rett))
        ;; no keywords
        (method-def-expr- name sparams argl body isstaged rett))))

(define (struct-def-expr name params super fields mut)
  (receive
   (params bounds) (sparam-name-bounds params)
   (struct-def-expr- name params bounds super (flatten-blocks fields) mut)))

;; replace field names with gensyms if they conflict with field-types
(define (safe-field-names field-names field-types)
  (if (any (lambda (v) (contains (lambda (e) (eq? e v)) field-types))
           field-names)
      (map (lambda (x) (gensy)) field-names)
      field-names))

(define (default-inner-ctors name field-names field-types gen-specific? locs)
  (let* ((field-names (safe-field-names field-names field-types))
         (any-ctor
          ;; definition with Any for all arguments
          `(function (call ,name ,@field-names)
                     (block
                      ,@locs
                      (call new ,@field-names)))))
    (if (and gen-specific? (any (lambda (t) (not (eq? t 'Any))) field-types))
        (list
         ;; definition with field types for all arguments
         `(function (call ,name
                          ,@(map make-decl field-names field-types))
                    (block
                     ,@locs
                     (call new ,@field-names)))
         any-ctor)
        (list any-ctor))))

(define (default-outer-ctor name field-names field-types params bounds locs)
  (let ((field-names (safe-field-names field-names field-types)))
    `(function (call (curly ,name
                            ,@(map (lambda (b) (cons 'var-bounds b)) bounds))
                     ,@(map make-decl field-names field-types))
               (block
                ,@locs
                (call (curly ,name ,@params) ,@field-names)))))

(define (new-call Tname type-params params args field-names field-types)
  (if (any vararg? args)
      (error "... is not supported inside \"new\""))
  (if (any kwarg? args)
      (error "\"new\" does not accept keyword arguments"))
  (if (length> params (length type-params))
      (error "too few type parameters specified in \"new{...}\""))
  (let ((Texpr (if (null? type-params)
                   `(outerref ,Tname)
                   `(curly (outerref ,Tname)
                           ,@type-params))))
    (cond ((length> args (length field-names))
           `(call (top error) "new: too many arguments"))
          (else
           (if (equal? type-params params)
               `(new ,Texpr ,@(map (lambda (fty val)
                                     `(call (top convert) ,fty ,val))
                                   (list-head field-types (length args)) args))
               (let ((tn (make-ssavalue)))
                 `(block
                   (= ,tn ,Texpr)
                   (new ,tn ,@(map (lambda (fld val)
                                     `(call (top convert)
                                            (call (core fieldtype) ,tn (quote ,fld))
                                            ,val))
                                   (list-head field-names (length args)) args)))))))))

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
          (cons `(call (|::| (curly (core Type) ,name)) ,@sig)
                params)
          (cons `(call (curly (|::| (curly (core Type) ,name)) ,@method-params) ,@sig)
                params))
      (if (null? method-params)
          (cons `(call (curly (|::| (curly (core Type) (curly ,name ,@params)))
                              ,@(map (lambda (b) (cons 'var-bounds b)) bounds))
                       ,@sig)
                params)
          ;; rename parameters that conflict with user-written method parameters
          (let ((new-params (map (lambda (p) (if (memq p method-params)
                                                 (gensy)
                                                 p))
                                 params)))
            (cons `(call (curly (|::| (curly (core Type) (curly ,name ,@new-params)))
                                ,@(map (lambda (n b) (list* 'var-bounds n (cdr b))) new-params bounds)
                                ,@method-params)
                         ,@sig)
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

;; rewrite calls to `new( ... )` to `new` expressions on the appropriate
;; type, determined by the containing constructor definition.
(define (rewrite-ctor ctor Tname params bounds field-names field-types)
  (define (ctor-body body type-params)
    (pattern-replace (pattern-set
                      (pattern-lambda
                       (call (-/ new) . args)
                       (new-call Tname type-params params
                                 (map (lambda (a) (ctor-body a type-params)) args)
                                 field-names field-types))
                      (pattern-lambda
                       (call (curly (-/ new) . p) . args)
                       (new-call Tname p params
                                 (map (lambda (a) (ctor-body a type-params)) args)
                                 field-names field-types)))
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

(define (struct-def-expr- name params bounds super fields0 mut)
  (receive
   (fields defs) (separate (lambda (x) (or (symbol? x) (decl? x)))
                           fields0)
   (let* ((defs        (filter (lambda (x) (not (effect-free? x))) defs))
          (locs        (if (and (pair? fields0) (pair? (car fields0)) (eq? (caar fields0) 'line))
                           (list (car fields0))
                           '()))
          (field-names (map decl-var fields))
          (field-types (map decl-type fields))
          (defs2 (if (null? defs)
                     (default-inner-ctors name field-names field-types (null? params) locs)
                     defs))
          (min-initialized (min (ctors-min-initialized defs) (length fields))))
     (let ((dups (has-dups field-names)))
       (if dups (error (string "duplicate field name: \"" (car dups) "\" is not unique"))))
     (for-each (lambda (v)
                 (if (not (symbol? v))
                     (error (string "field name \"" (deparse v) "\" is not a symbol"))))
               field-names)
     `(block
       (scope-block
        (block
         (global ,name) (const ,name)
         ,@(map (lambda (v) `(local ,v)) params)
         ,@(map (lambda (n v) (make-assignment n (bounds-to-TypeVar v))) params bounds)
         (composite_type ,name (call (core svec) ,@params)
                         (call (core svec) ,@(map (lambda (x) `',x) field-names))
                         ,super (call (core svec) ,@field-types) ,mut ,min-initialized)))
       ;; "inner" constructors
       (scope-block
        (block
         (global ,name)
         ,@(map (lambda (c)
                  (rewrite-ctor c name params bounds field-names field-types))
                defs2)))
       ;; "outer" constructors
       ,@(if (and (null? defs)
                  (not (null? params))
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
                    params bounds locs))))
             '())
       (null)))))

(define (abstract-type-def-expr name params super)
  (receive
   (params bounds) (sparam-name-bounds params)
   `(block
     (global ,name) (const ,name)
     (scope-block
      (block
       ,@(map (lambda (v) `(local ,v)) params)
       ,@(map (lambda (n v) (make-assignment n (bounds-to-TypeVar v))) params bounds)
       (abstract_type ,name (call (core svec) ,@params) ,super))))))

(define (bits-def-expr n name params super)
  (receive
   (params bounds) (sparam-name-bounds params)
   `(block
     (global ,name) (const ,name)
     (scope-block
      (block
       ,@(map (lambda (v) `(local ,v)) params)
       ,@(map (lambda (n v) (make-assignment n (bounds-to-TypeVar v))) params bounds)
       (bits_type ,name (call (core svec) ,@params) ,n ,super))))))

;; take apart a type signature, e.g. T{X} <: S{Y}
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
          (foreigncall ,name ,RT (call (core svec) ,@(dots->vararg atypes))
                ,.(reverse! C)
                ,@A))
        (let* ((a     (car A))
               (isseq (and (pair? (car F)) (eq? (caar F) '...)))
               (ty    (if isseq (cadar F) (car F))))
          (if (eq? ty 'Any)
              (loop (if isseq F (cdr F)) (cdr A) stmts (list* 0 a C))
              (let* ((g (make-ssavalue))
                     (isamp (and (pair? a) (eq? (car a) '&)))
                     (a (if isamp (cadr a) a))
                     (stmts (cons `(= ,g (call (top ,(if isamp 'ptr_arg_cconvert 'cconvert)) ,ty ,a)) stmts))
                     (ca `(call (top ,(if isamp 'ptr_arg_unsafe_convert 'unsafe_convert)) ,ty ,g)))
                (loop (if isseq F (cdr F)) (cdr A) stmts
                      (list* g (if isamp `(& ,ca) ca) C))))))))

(define (expand-function-def e)   ;; handle function or stagedfunction
  (let ((name (cadr e)))
    (if (and (pair? name) (memq (car name) '(tuple block)))
        (expand-forms (cons '-> (cdr e)))
        (expand-function-def- e))))

;; convert (where (where x S) T) to (where x T S)
(define (flatten-where-expr e)
  (let loop ((ex e)
             (vars '()))
    (if (and (pair? ex) (eq? (car ex) 'where))
        (loop (cadr ex) (append! (reverse (cddr ex)) vars))
        `(where ,ex ,.(reverse! vars)))))

(define (expand-function-def- e)
  (let* ((name  (cadr e))
         (where (if (and (pair? name) (eq? (car name) 'where))
                    (let ((w (flatten-where-expr name)))
                      (begin0 (cddr w)
                              (set! name (cadr w))))
                    #f))
         (dcl   (and (pair? name) (eq? (car name) '|::|)))
         (rett  (if dcl (caddr name) 'Any))
         (name  (if dcl (cadr name) name)))
    (cond ((and (length= e 2) (symbol? name))
           (if (or (eq? name 'true) (eq? name 'false))
               (error (string "invalid function name \"" name "\"")))
           `(method ,name))
          ((not (pair? name))  e)
          ((eq? (car name) 'call)
           (let* ((head    (cadr name))
                  (argl    (cddr name))
                  (has-sp  (and (not where) (pair? head) (eq? (car head) 'curly)))
                  (name    (deprecate-dotparen (if has-sp (cadr head) head)))
                  (op (let ((op_ (maybe-undotop name))) ; handle .op -> broadcast deprecation
                        (if op_
                            (syntax-deprecation #f (string "function " (deparse name) "(...)")
                                                (string "function Base.broadcast(::typeof(" (deparse op_) "), ...)")))
                        op_))
                  (name (if op '(|.| Base (inert broadcast)) name))
                  (argl (if op (cons `(|::| (call (core Typeof) ,op)) argl) argl))
                  (sparams (map analyze-typevar (cond (has-sp (cddr head))
                                                      (where  where)
                                                      (else   '()))))
                  (isstaged (eq? (car e) 'stagedfunction))
                  (adj-decl (lambda (n) (if (and (decl? n) (length= n 2))
                                            `(|::| |#self#| ,(cadr n))
                                            n)))
                  ;; fill in first (closure) argument
                  (farg    (if (decl? name)
                               (adj-decl name)
                               `(|::| |#self#| (call (core Typeof) ,name))))
                  (argl    (fix-arglist
                            (if (and (not (decl? name)) (eq? (undot-name name) 'call))
                                (cons (adj-decl (car argl)) (cdr argl))
                                (arglist-unshift argl farg))
                            (and (not (any kwarg? argl)) (not (and (pair? argl)
                                                                   (pair? (car argl))
                                                                   (eq? (caar argl) 'parameters))))))
                  (name    (if (or (decl? name) (and (pair? name) (eq? (car name) 'curly)))
                               #f name)))
             (expand-forms
              (method-def-expr name sparams argl (caddr e) isstaged rett))))
          (else e))))

;; handle ( )->( ) function expressions. blocks `(a;b=1)` on the left need to be
;; converted to argument lists with kwargs.
(define (expand-arrow e)
  (let* ((a     (cadr e))
         (body  (caddr e))
         (where (if (and (pair? a) (eq? (car a) 'where))
                    (let ((w (flatten-where-expr a)))
                      (begin0 (cddr w)
                              (set! a (cadr w))))
                    #f))
         (argl (if (pair? a)
                   (if (eq? (car a) 'tuple)
                       (map =-to-kw (cdr a))
                       (if (eq? (car a) 'block)
                           (cond ((length= a 1) '())
                                 ((length= a 2) (list (cadr a)))
                                 ((length= a 3)
                                  (if (assignment? (caddr a))
                                      `((parameters (kw ,@(cdr (caddr a)))) ,(cadr a))
                                      `((parameters ,(caddr a)) ,(cadr a))))
                                 (else
                                  (error "more than one semicolon in argument list")))
                           (list (=-to-kw a))))
                   (list a)))
         ;; TODO: always use a specific special name like #anon# or _, then ignore
         ;; this as a local variable name.
         (name (symbol (string "#" (current-julia-module-counter)))))
    (expand-forms
     `(block (local ,name)
             (function
              ,(if where
                   `(where (call ,name ,@argl) ,@where)
                   `(call ,name ,@argl))
              ,body)))))

(define (expand-let e)
  (let ((ex (cadr e))
        (binds (cddr e)))
    (expand-forms
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
                            (let ((tmp (make-ssavalue)))
                              `(scope-block
                                (block (= ,tmp ,(caddar binds))
                                       (scope-block
                                        (block
                                         (local-def ,(cadar binds))
                                         (= ,vname ,tmp)
                                         ,blk)))))
                            `(scope-block
                              (block
                               (local-def ,(cadar binds))
                               (= ,vname ,(caddar binds))
                               ,blk))))))
               ((and (pair? (cadar binds))
                     (or (eq? (caadar binds) 'call)
                         (and (eq? (caadar binds) 'comparison)
                              (length= (cadar binds) 4))))
                ;; f()=c
                (let* ((asgn (butlast (expand-forms (car binds))))
                       (name (cadr (cadar binds)))
                       (name (cond ((symbol? name) name)
                                   ((and (pair? name) (eq? (car name) 'curly))
                                    (cadr name))
                                   (else (error "invalid let syntax")))))
                  (loop (cdr binds)
                        `(scope-block
                          (block
                           (local-def ,name)
                           ,asgn
                           ,blk)))))
               ;; (a, b, c, ...) = rhs
               ((and (pair? (cadar binds))
                     (eq? (caadar binds) 'tuple))
                (let ((vars (lhs-vars (cadar binds))))
                  (loop (cdr binds)
                        `(scope-block
                          (block
                           ,@(map (lambda (v) `(local-def ,v)) vars)
                           ,(car binds)
                           ,blk)))))
               (else (error "invalid let syntax"))))
             (else (error "invalid let syntax")))))))))

(define (expand-macro-def e)
  (cond ((and (pair? (cadr e))
              (eq? (car (cadr e)) 'call)
              (symbol? (cadr (cadr e))))
         (let ((anames (remove-empty-parameters (cddr (cadr e)))))
           (if (has-parameters? anames)
               (error "macros cannot accept keyword arguments"))
           (expand-forms
            `(function (call ,(symbol (string #\@ (cadr (cadr e))))
                             ,@(map (lambda (v)
                                      (if (symbol? v)
                                          `(|::| ,v (core ANY))
                                          v))
                                    anames))
                       ,@(cddr e)))))
        (else
         (error "invalid macro definition"))))

(define (expand-type-def e)
  (let ((mut (cadr e))
        (sig (caddr e))
        (fields (cdr (cadddr e))))
    (let loop ((f fields))
      (if (null? f)
          '()
          (let ((x (car f)))
            (cond ((or (symbol? x) (decl? x) (and (pair? x) (eq? (car x) 'line)))
                   (loop (cdr f)))
                  ((and (assignment? x) (or (symbol? (cadr x)) (decl? (cadr x))))
                   (error (string "\"" (deparse x) "\" inside type definition is reserved")))
                  (else '())))))
    (expand-forms
     (receive (name params super) (analyze-type-sig sig)
              (struct-def-expr name params super fields mut)))))

;; the following are for expanding `try` blocks

(define (find-symbolic-label-defs e tbl)
  (if (or (not (pair? e)) (quoted? e))
      '()
      (if (eq? (car e) 'symboliclabel)
          (put! tbl (cadr e) #t)
          (for-each (lambda (x) (find-symbolic-label-defs x tbl)) e))))

(define (find-symbolic-label-refs e tbl)
  (if (or (not (pair? e)) (quoted? e))
      '()
      (if (eq? (car e) 'symbolicgoto)
          (put! tbl (cadr e) #t)
          (for-each (lambda (x) (find-symbolic-label-refs x tbl)) e))))

(define (has-unmatched-symbolic-goto? e)
  (let ((label-refs (table))
        (label-defs (table)))
    (find-symbolic-label-refs e label-refs)
    (find-symbolic-label-defs e label-defs)
    (any not (map (lambda (k) (get label-defs k #f))
                  (table.keys label-refs)))))

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

(define (expand-try e)
  (if (length= e 5)
      (let (;; expand inner try blocks first, so their return statements
            ;; will have been moved for `finally`, causing correct
            ;; chaining behavior when the current (outer) try block is
            ;; expanded.
            (tryb (expand-forms (cadr e)))
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
                              (and (not (eq? catchb 'false))
                                   (not (block-returns? catchb))))
                          (gensy)))
                (retval (if hasret (gensy) #f))
                (bb  (gensy))
                (finally-exception (gensy))
                (val (gensy))) ;; this is an ssavalue, but llvm has trouble determining that it dominates all uses
            (let ((tryb   (replace-return tryb bb ret retval))
                  (catchb (replace-return catchb bb ret retval)))
              (expand-forms
               `(scope-block
                 (block
                  ,@(if hasret `((local ,retval)) '())
                  (local ,val)
                  (local ,finally-exception)
                  (= ,err false)
                  ,@(if ret `((= ,ret false)) '())
                  (break-block
                   ,bb
                   (try (= ,val
                           ,(if (not (eq? catchb 'false))
                                `(try ,tryb ,var ,catchb)
                                tryb))
                        false
                        (= ,err true)))
                  (= ,finally-exception (the_exception))
                  ,finalb
                  (if ,err (foreigncall 'jl_rethrow_other (core Void) (call (core svec) Any) ,finally-exception 0))
                  ,(if hasret
                       (if ret
                           `(if ,ret (return ,retval) ,val)
                           `(return ,retval))
                       val))))))))
      (if (length= e 4)
          (let ((tryb (cadr e))
                (var  (caddr e))
                (catchb (cadddr e)))
            (expand-forms
             (if (and (symbol-like? var) (not (eq? var 'false)))
                 `(trycatch (scope-block ,tryb)
                            (scope-block
                             (block (= ,var (the_exception))
                                    ,catchb)))
                 `(trycatch (scope-block ,tryb)
                            (scope-block ,catchb)))))
          (map expand-forms e))))

(define (expand-typealias e)
  (if (and (pair? (cadr e))
           (eq? (car (cadr e)) 'curly))
      (let ((name (cadr (cadr e)))
            (params (cddr (cadr e)))
            (type-ex (caddr e)))
        (receive
         (params bounds) (sparam-name-bounds params)
         `(block
           (const ,name)
           (= ,name
              (scope-block
               (block
                ,@(map (lambda (v) `(local ,v)) params)
                ,@(map (lambda (l r) (make-assignment l (expand-forms (bounds-to-TypeVar r))))
                       params bounds)
                ,(foldl (lambda (var type) `(call (core UnionAll) ,var ,type))
                        (expand-forms type-ex)
                        (reverse params))))))))
      (expand-forms
       `(const (= ,(cadr e) ,(caddr e))))))

;; take apart e.g. `const a::Int = 0` into `const a; a::Int = 0`
(define (expand-const-decl e)
  (let ((arg (cadr e)))
    (if (atom? arg)
        e
        (case (car arg)
          ((global local local-def)
           (for-each (lambda (b) (if (not (assignment? b))
                                     (error "expected assignment after \"const\"")))
                     (cdr arg))
           (expand-forms (expand-decls (car arg) (cdr arg) #t)))
          ((= |::|)
           (expand-forms (expand-decls 'const (cdr e) #f)))
          (else e)))))

(define (expand-local-or-global-decl e)
  (if (and (symbol? (cadr e)) (length= e 2))
      e
      (expand-forms (expand-decls (car e) (cdr e) #f))))

(define (assigned-name e)
  (if (and (pair? e) (memq (car e) '(call curly)))
      (assigned-name (cadr e))
      e))

;; local x, y=2, z => local x;local y;local z;y = 2
(define (expand-decls what binds const?)
  (if (not (list? binds))
      (error (string "invalid \"" what "\" declaration")))
  (let loop ((b       binds)
             (vars    '())
             (assigns '()))
    (if (null? b)
        `(block
          ,.(if const?
                (map (lambda (x) `(const ,x)) vars)
                '())
          ,.(map (lambda (x) `(,what ,x)) vars)
          ,.(reverse assigns))
        (let ((x (car b)))
          (cond ((or (assignment-like? x) (function-def? x))
                 (loop (cdr b)
                       (append (lhs-decls (assigned-name (cadr x))) vars)
                       (cons `(,(car x) ,(all-decl-vars (cadr x)) ,(caddr x))
                             assigns)))
                ((and (pair? x) (eq? (car x) '|::|))
                 (loop (cdr b)
                       (cons (decl-var x) vars)
                       (cons `(decl ,@(cdr x)) assigns)))
                ((symbol? x)
                 (loop (cdr b) (cons x vars) assigns))
                (else
                 (error (string "invalid syntax in \"" what "\" declaration"))))))))

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
                (unnecessary (tuple ,@(reverse elts))))
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
              (let ((temp (make-ssavalue)))
                (loop (cdr lhss)
                      (cons L assigned)
                      (cdr rhss)
                      (cons (make-assignment temp R) stmts)
                      (cons (make-assignment L temp) after)
                      (cons temp elts))))))))

;; convert (lhss...) = x to tuple indexing
(define (lower-tuple-assignment lhss x)
  (let ((t (make-ssavalue)))
    `(block
      (= ,t ,x)
      ,@(let loop ((lhs lhss)
                   (i   1))
          (if (null? lhs) '((null))
              (cons `(= ,(car lhs)
                        (call (core getfield) ,t ,i))
                    (loop (cdr lhs)
                          (+ i 1)))))
      ,t)))

;; make an expression safe for multiple evaluation
;; for example a[f(x)] => (temp=f(x); a[temp])
;; retuns a pair (expr . assignments)
;; where 'assignments' is a list of needed assignment statements
(define (remove-argument-side-effects e)
  (let ((a '()))
    (cond
     ((not (pair? e))
      (cons e '()))
     (else
      (cons (map (lambda (x)
                   (cond
                    ((not (effect-free? x))
                     (let ((g (make-ssavalue)))
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

;; lower function call containing keyword arguments
(define (lower-kw-call f kw pa)
  (let ((container (make-ssavalue)))
    (let loop ((kw kw)
               (initial-kw '()) ;; keyword args before any splats
               (stmts '())
               (has-kw #f))     ;; whether there are definitely >0 kwargs
      (if (null? kw)
          (if (null? stmts)
              `(call (call (core kwfunc) ,f) (call (top vector_any) ,@(reverse initial-kw)) ,f ,@pa)
              `(block
                (= ,container (call (top vector_any) ,@(reverse initial-kw)))
                ,@(reverse stmts)
                ,(if has-kw
                     `(call (call (core kwfunc) ,f) ,container ,f ,@pa)
                     (let* ((expr_stmts (remove-argument-side-effects `(call ,f ,@pa)))
                            (pa         (cddr (car expr_stmts)))
                            (stmts      (cdr expr_stmts)))
                       `(block
                         ,@stmts
                         (if (call (top isempty) ,container)
                             (call ,f ,@pa)
                             (call (call (core kwfunc) ,f) ,container ,f ,@pa)))))))
          (let ((arg (car kw)))
            (cond ((and (pair? arg) (eq? (car arg) 'parameters))
                   (error "more than one semicolon in argument list"))
                  ((kwarg? arg)
                   (if (not (symbol? (cadr arg)))
                       (error (string "keyword argument is not a symbol: \""
                                      (deparse (cadr arg)) "\"")))
                   (if (vararg? (caddr arg))
                       (error "splicing with \"...\" cannot be used for a keyword argument value"))
                   (if (null? stmts)
                       (loop (cdr kw) (list* (caddr arg) `(quote ,(cadr arg)) initial-kw) stmts #t)
                       (loop (cdr kw) initial-kw
                             (cons `(foreigncall 'jl_array_ptr_1d_push2 (core Void) (call (core svec) Any Any Any)
                                           ,container 0
                                           (|::| (quote ,(cadr arg)) (core Symbol)) 0
                                           ,(caddr arg) 0)
                                   stmts)
                             #t)))
                  (else
                   (loop (cdr kw) initial-kw
                         (cons (let* ((k (make-ssavalue))
                                      (v (make-ssavalue))
                                      (push-expr `(foreigncall 'jl_array_ptr_1d_push2 (core Void) (call (core svec) Any Any Any)
                                                         ,container 0
                                                         (|::| ,k (core Symbol)) 0
                                                         ,v 0)))
                                 (if (vararg? arg)
                                     `(for (= (tuple ,k ,v) ,(cadr arg))
                                           ,push-expr)
                                     `(block (= (tuple ,k ,v) ,arg)
                                             ,push-expr)))
                               stmts)
                         (or has-kw (not (vararg? arg)))))))))))

;; convert e.g. A'*B to Ac_mul_B(A,B)
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

;; convert `a+=b` to `a=a+b`
(define (expand-update-operator- op op= lhs rhs declT)
  (let ((e (remove-argument-side-effects lhs)))
    `(block ,@(cdr e)
            ,(if (null? declT)
                 `(,op= ,(car e) (call ,op ,(car e) ,rhs))
                 `(,op= ,(car e) (call ,op (:: ,(car e) ,(car declT)) ,rhs))))))

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
           (arr   (if reuse (make-ssavalue) a))
           (stmts (if reuse `((= ,arr ,a)) '())))
      (receive
       (new-idxs stuff) (process-indexes arr idxs)
       `(block
         ,@(append stmts stuff)
         (call getindex ,arr ,@new-idxs))))))

(define (expand-update-operator op op= lhs rhs . declT)
  (cond ((and (pair? lhs) (eq? (car lhs) 'ref))
         ;; expand indexing inside op= first, to remove "end" and ":"
         (let* ((ex (partially-expand-ref lhs))
                (stmts (butlast (cdr ex)))
                (refex (last    (cdr ex)))
                (nuref `(ref ,(caddr refex) ,@(cdddr refex))))
           `(block ,@stmts
                   ,(expand-update-operator- op op= nuref rhs declT))))
        ((and (pair? lhs) (eq? (car lhs) '|::|))
         ;; (+= (:: x T) rhs)
         (let ((e (remove-argument-side-effects (cadr lhs)))
               (T (caddr lhs)))
           `(block ,@(cdr e)
                   ,(expand-update-operator op op= (car e) rhs T))))
        (else
         (if (and (pair? lhs)
                  (not (memq (car lhs) '(|.| tuple vcat typed_hcat typed_vcat))))
             (error (string "invalid assignment location \"" (deparse lhs) "\"")))
         (expand-update-operator- op op= lhs rhs declT))))

(define (lower-update-op e)
  (expand-forms
   (let ((str (string (car e))))
     (expand-update-operator
      (symbol (string.sub str 0 (- (length str) 1)))
      (if (= (string.char str 0) #\.) '.= '=)
      (cadr e)
      (caddr e)))))

(define (expand-and e)
  (let ((e (cdr (flatten-ex '&& e))))
    (let loop ((tail e))
      (if (null? tail)
          'true
          (if (null? (cdr tail))
              (car tail)
              `(if ,(car tail)
                   ,(loop (cdr tail))
                   false))))))

(define (expand-or e)
  (let ((e (cdr (flatten-ex '|\|\|| e))))
    (let loop ((tail e))
      (if (null? tail)
          'false
          (if (null? (cdr tail))
              (car tail)
              (if (symbol-like? (car tail))
                  `(if ,(car tail) ,(car tail)
                       ,(loop (cdr tail)))
                  (let ((g (make-ssavalue)))
                    `(block (= ,g ,(car tail))
                            (if ,g ,g
                                ,(loop (cdr tail)))))))))))

(define (expand-forms e)
  (if (or (atom? e) (memq (car e) '(quote inert top core globalref outerref line module toplevel ssavalue null meta)))
      e
      (let ((ex (get expand-table (car e) #f)))
        (if ex
            (ex e)
            (cons (car e)
                  (map expand-forms (cdr e)))))))

(define (expand-for while lhs X body)
  ;; (for (= lhs X) body)
  (let ((coll  (make-ssavalue))
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

;; convert an operator parsed as (op a b) to (call op a b)
(define (syntactic-op-to-call e)
  `(call ,(car e) ,(expand-forms (cadr e)) ,(expand-forms (caddr e))))

;; wrap `expr` in a function appropriate for consuming values from given ranges
(define (func-for-generator-ranges expr range-exprs)
  (let* ((vars    (map cadr range-exprs))
         (argname (if (and (length= vars 1) (symbol? (car vars)))
                      (car vars)
                      (gensy)))
         (splat (cond ((eq? argname (car vars))  '())
                      ((length= vars 1)
                       `(,@(map (lambda (v) `(local ,v)) (lhs-vars (car vars)))
                         (= ,(car vars) ,argname)))
                      (else
                       `(,@(map (lambda (v) `(local ,v)) (lhs-vars `(tuple ,@vars)))
                         (= (tuple ,@vars) ,argname))))))
    (if (and (null? splat)
             (length= expr 3) (eq? (car expr) 'call)
             (eq? (caddr expr) argname)
             (not (expr-contains-eq argname (cadr expr))))
        (cadr expr)  ;; eta reduce `x->f(x)` => `f`
        `(-> ,argname (block ,@splat ,expr)))))

(define (ref-to-view expr)
  (if (and (pair? expr) (eq? (car expr) 'ref))
      (let* ((ex (partially-expand-ref expr))
             (stmts (butlast (cdr ex)))
             (refex (last    (cdr ex)))
             (nuref `(call (top dotview) ,(caddr refex) ,@(cdddr refex))))
        `(block ,@stmts ,nuref))
      expr))

; fuse nested calls to expr == f.(args...) into a single broadcast call,
; or a broadcast! call if lhs is non-null.
(define (expand-fuse-broadcast lhs rhs)
  (define (fuse? e) (and (pair? e) (eq? (car e) 'fuse)))
  (define (anyfuse? exprs)
    (if (null? exprs) #f (if (fuse? (car exprs)) #t (anyfuse? (cdr exprs)))))
  (define (to-lambda f args kwargs) ; convert f to anonymous function with hygienic tuple args
    (define (genarg arg) (if (vararg? arg) (list '... (gensy)) (gensy)))
    ; (To do: optimize the case where f is already an anonymous function, in which
    ;  case we only need to hygienicize the arguments?   But it is quite tricky
    ;  to fully handle splatted args, typed args, keywords, etcetera.  And probably
    ;  the extra function call is harmless because it will get inlined anyway.)
    (let ((genargs (map genarg args))) ; hygienic formal parameters
      (if (null? kwargs)
          `(-> ,(cons 'tuple genargs) (call ,f ,@genargs)) ; no keyword args
          `(-> ,(cons 'tuple genargs) (call ,f (parameters ,@kwargs) ,@genargs)))))
  (define (from-lambda f) ; convert (-> (tuple args...) (call func args...)) back to func
    (if (and (pair? f) (eq? (car f) '->) (pair? (cadr f)) (eq? (caadr f) 'tuple)
             (pair? (caddr f)) (eq? (caaddr f) 'call) (equal? (cdadr f) (cdr (cdaddr f))))
        (car (cdaddr f))
        f))
  (define (fuse-args oldargs) ; replace (fuse f args) with args in oldargs list
    (define (fargs newargs oldargs)
      (if (null? oldargs)
          newargs
          (fargs (if (fuse? (car oldargs))
                     (append (reverse (caddar oldargs)) newargs)
                     (cons (car oldargs) newargs))
                 (cdr oldargs))))
    (reverse (fargs '() oldargs)))
  (define (fuse-funcs f args) ; for (fuse g a) in args, merge/inline g into f
    ; any argument A of f that is (fuse g a) gets replaced by let A=(body of g):
    (define (fuse-lets fargs args lets)
      (if (null? args)
          lets
          (if (fuse? (car args))
              (fuse-lets (cdr fargs) (cdr args) (cons (list '= (car fargs) (caddr (cadar args))) lets))
              (fuse-lets (cdr fargs) (cdr args) lets))))
    (let ((fargs (cdadr f))
          (fbody (caddr f)))
      `(->
        (tuple ,@(fuse-args (map (lambda (oldarg arg) (if (fuse? arg)
                                                 `(fuse _ ,(cdadr (cadr arg)))
                                                 oldarg))
                                 fargs args)))
        (let ,fbody ,@(reverse (fuse-lets fargs args '()))))))
  (define (dot-to-fuse e) ; convert e == (. f (tuple args)) to (fuse f args)
    (define (make-fuse f args) ; check for nested (fuse f args) exprs and combine
      (define (split-kwargs args) ; return (cons keyword-args positional-args) extracted from args
        (define (sk args kwargs pargs)
          (if (null? args)
              (cons kwargs pargs)
              (if (kwarg? (car args))
                  (sk (cdr args) (cons (car args) kwargs) pargs)
                  (sk (cdr args) kwargs (cons (car args) pargs)))))
        (if (has-parameters? args)
            (sk (reverse (cdr args)) (cdar args) '())
            (sk (reverse args) '() '())))
      (let* ((kws.args (split-kwargs args))
             (kws (car kws.args))
             (args (cdr kws.args)) ; fusing occurs on positional args only
             (args_ (map dot-to-fuse args)))
        (if (anyfuse? args_)
            `(fuse ,(fuse-funcs (to-lambda f args kws) args_) ,(fuse-args args_))
            `(fuse ,(to-lambda f args kws) ,args_))))
    (if (and (pair? e) (eq? (car e) '|.|))
        (let ((f (cadr e)) (x (caddr e)))
          (if (or (eq? (car x) 'quote) (eq? (car x) 'inert) (eq? (car x) '$))
              `(call (core getfield) ,f ,x)
              (make-fuse f (cdr x))))
        (if (and (pair? e) (eq? (car e) 'call) (dotop? (cadr e)))
            (make-fuse (undotop (cadr e)) (cddr e))
            e)))
  ; given e == (fuse lambda args), compress the argument list by removing (pure)
  ; duplicates in args, inlining literals, and moving any varargs to the end:
  (define (compress-fuse e)
    (define (findfarg arg args fargs) ; for arg in args, return corresponding farg
      (if (eq? arg (car args))
          (car fargs)
          (findfarg arg (cdr args) (cdr fargs))))
    (if (fuse? e)
        (let ((f (cadr e))
              (args (caddr e)))
          (define (cf old-fargs old-args new-fargs new-args renames varfarg vararg)
            (if (null? old-args)
                (let ((nfargs (if (null? varfarg) new-fargs (cons varfarg new-fargs)))
                      (nargs (if (null? vararg) new-args (cons vararg new-args))))
                  `(fuse (-> (tuple ,@(reverse nfargs)) ,(replace-vars (caddr f) renames))
                         ,(reverse nargs)))
                (let ((farg (car old-fargs)) (arg (car old-args)))
                  (cond
                   ((and (vararg? farg) (vararg? arg)) ; arg... must be the last argument
                    (if (null? varfarg)
                        (cf (cdr old-fargs) (cdr old-args)
                            new-fargs new-args renames farg arg)
                        (if (eq? (cadr vararg) (cadr arg))
                            (cf (cdr old-fargs) (cdr old-args)
                                new-fargs new-args (cons (cons (cadr farg) (cadr varfarg)) renames)
                                varfarg vararg)
                            (error "multiple splatted args cannot be fused into a single broadcast"))))
                   ((julia-scalar? arg) ; inline numeric literals etc.
                    (cf (cdr old-fargs) (cdr old-args)
                        new-fargs new-args
                        (cons (cons farg arg) renames)
                        varfarg vararg))
                   ((and (symbol? arg) (memq arg new-args)) ; combine duplicate args
                                        ; (note: calling memq for every arg is O(length(args)^2) ...
                                        ;  ... would be better to replace with a hash table if args is long)
                    (cf (cdr old-fargs) (cdr old-args)
                        new-fargs new-args
                        (cons (cons farg (findfarg arg new-args new-fargs)) renames)
                        varfarg vararg))
                   (else
                    (cf (cdr old-fargs) (cdr old-args)
                        (cons farg new-fargs) (cons arg new-args) renames varfarg vararg))))))
          (cf (cdadr f) args '() '() '() '() '()))
        e)) ; (not (fuse? e))
  (let ((e (compress-fuse (dot-to-fuse rhs))) ; an expression '(fuse func args) if expr is a dot call
        (lhs-view (ref-to-view lhs))) ; x[...] expressions on lhs turn in to view(x, ...) to update x in-place
    (if (fuse? e)
        (if (null? lhs)
            (expand-forms `(call (top broadcast) ,(from-lambda (cadr e)) ,@(caddr e)))
            (expand-forms `(call (top broadcast!) ,(from-lambda (cadr e)) ,lhs-view ,@(caddr e))))
        (if (null? lhs)
            (expand-forms e)
            (expand-forms `(call (top broadcast!) (top identity) ,lhs-view ,e))))))

;; table mapping expression head to a function expanding that form
(define expand-table
  (table
   'function       expand-function-def
   'stagedfunction expand-function-def
   '->             expand-arrow
   'let            expand-let
   'macro          expand-macro-def
   'type           expand-type-def
   'try            expand-try
   'typealias      expand-typealias

   'lambda
   (lambda (e)
     `(lambda ,(map expand-forms (cadr e))
        ,@(if (length= e 3) '(()) '())
        ,@(map expand-forms (cddr e))))

   'block
   (lambda (e)
     (cond ((null? (cdr e)) '(null))
           ((and (null? (cddr e))
                 (not (and (pair? (cadr e))
                           (eq? (car (cadr e)) 'line))))
            (expand-forms (cadr e)))
           (else
            `(block
              ,.(map (lambda (x)
                       (if (and (decl? x) (length= (cdr x) 2) (symbol? (cadr x)))
                           `(impl-decl ,@(map expand-forms (cdr x)))
                           (expand-forms x)))
                     (butlast (cdr e)))
              ,(expand-forms (last (cdr e)))))))

   '|.|
   (lambda (e) ; e = (|.| f x)
     (expand-fuse-broadcast '() e))

   '.=
   (lambda (e)
     (expand-fuse-broadcast (cadr e) (caddr e)))

   '|<:| syntactic-op-to-call
   '|>:| syntactic-op-to-call

   'where
   (lambda (e)
     (let* ((bounds (analyze-typevar (caddr e)))
            (v  (car bounds)))
       (expand-forms
        `(let (call (core UnionAll) ,v ,(cadr e))
           (= ,v ,(bounds-to-TypeVar bounds))))))

   'const  expand-const-decl
   'local  expand-local-or-global-decl
   'global expand-local-or-global-decl

   '=
   (lambda (e)
     (define lhs (cadr e))
     (cond
      ((and (pair? lhs)
            (or (eq? (car lhs) 'call)
                (eq? (car lhs) 'where)
                (and (eq? (car lhs) '|::|)
                     (pair? (cadr lhs))
                     (eq? (car (cadr lhs)) 'call))))
       (expand-forms (cons 'function (cdr e))))
      ((and (pair? lhs)
            (eq? (car lhs) 'comparison)
            (length= lhs 4))
       ;; allow defining functions that use comparison syntax
       (expand-forms (list* 'function
                            `(call ,(caddr lhs) ,(cadr lhs) ,(cadddr lhs)) (cddr e))))
      ((assignment? (caddr e))
       ;; chain of assignments - convert a=b=c to `b=c; a=c`
       (let loop ((lhss (list lhs))
                  (rhs  (caddr e)))
         (if (assignment? rhs)
             (loop (cons (cadr rhs) lhss) (caddr rhs))
             (let ((rr (if (symbol-like? rhs) rhs (make-ssavalue))))
               (expand-forms
                `(block ,.(if (eq? rr rhs) '() `((= ,rr ,rhs)))
                        ,@(map (lambda (l) `(= ,l ,rr))
                               lhss)
                        (unnecessary ,rr)))))))
      ((symbol-like? lhs)
       `(= ,lhs ,(expand-forms (caddr e))))
      ((atom? lhs)
       (error (string "invalid assignment location \"" (deparse lhs) "\"")))
      (else
       (case (car lhs)
         ((globalref)
          ;; M.b =
          (let* ((rhs (caddr e))
                 (rr  (if (or (symbol-like? rhs) (atom? rhs)) rhs (make-ssavalue))))
            `(block
              ,.(if (eq? rr rhs) '() `((= ,rr ,(expand-forms rhs))))
              (= ,lhs ,rr)
              (unnecessary ,rr))))
         ((|.|)
          ;; a.b =
          (let* ((a   (cadr lhs))
                 (b_  (caddr lhs))
                 (b   (if (and (length= b_ 2) (eq? (car b_) 'tuple))
                          (begin
                            (syntax-deprecation #f
                             (string (deparse a) ".(" (deparse (cadr b_)) ") = ...")
                             (string "setfield!(" (deparse a) ", " (deparse (cadr b_)) ", ...)"))
                            (cadr b_))
                          b_))
                 (rhs (caddr e)))
            (let ((aa (if (symbol-like? a) a (make-ssavalue)))
                  (bb (if (or (atom? b) (symbol-like? b) (and (pair? b) (quoted? b)))
                          b (make-ssavalue)))
                  (rr (if (or (symbol-like? rhs) (atom? rhs)) rhs (make-ssavalue))))
              `(block
                ,.(if (eq? aa a)   '() `((= ,aa ,(expand-forms a))))
                ,.(if (eq? bb b)   '() `((= ,bb ,(expand-forms b))))
                ,.(if (eq? rr rhs) '() `((= ,rr ,(expand-forms rhs))))
                (call (core setfield!) ,aa ,bb
                      (call (top convert)
                            (call (core fieldtype) (call (core typeof) ,aa) ,bb)
                            ,rr))
                (unnecessary ,rr)))))
         ((tuple)
          ;; multiple assignment
          (let ((lhss (cdr lhs))
                (x    (caddr e)))
            (if (and (pair? x) (pair? lhss) (eq? (car x) 'tuple)
                     (length= lhss (length (cdr x))))
                ;; (a, b, ...) = (x, y, ...)
                (expand-forms
                 (tuple-to-assignments lhss x))
                ;; (a, b, ...) = other
                (let* ((xx  (if (and (symbol? x) (not (memq x lhss)))
                                x (make-ssavalue)))
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
                    (unnecessary ,xx))))))
         ((typed_hcat)
          (error "invalid spacing in left side of indexed assignment"))
         ((typed_vcat)
          (error "unexpected \";\" in left side of indexed assignment"))
         ((ref)
          ;; (= (ref a . idxs) rhs)
          (let ((a    (cadr lhs))
                (idxs (cddr lhs))
                (rhs  (caddr e)))
            (let* ((reuse (and (pair? a)
                               (contains (lambda (x)
                                           (or (eq? x 'end)
                                               (and (pair? x)
                                                    (eq? (car x) ':))))
                                         idxs)))
                   (arr   (if reuse (make-ssavalue) a))
                   (stmts (if reuse `((= ,arr ,(expand-forms a))) '()))
                   (rrhs (and (pair? rhs) (not (ssavalue? rhs)) (not (quoted? rhs))))
                   (r    (if rrhs (make-ssavalue) rhs))
                   (rini (if rrhs `((= ,r ,(expand-forms rhs))) '())))
              (receive
               (new-idxs stuff) (process-indexes arr idxs)
               `(block
                 ,@stmts
                 ,.(map expand-forms stuff)
                 ,@rini
                 ,(expand-forms
                   `(call setindex! ,arr ,r ,@new-idxs))
                 (unnecessary ,r))))))
         ((|::|)
          ;; (= (|::| x T) rhs)
          (let ((x (cadr lhs))
                (T (caddr lhs))
                (rhs (caddr e)))
            (let ((e (remove-argument-side-effects x)))
              (expand-forms
               `(block ,@(cdr e)
                       (decl ,(car e) ,T)
                       (= ,(car e) ,rhs))))))
         ((vcat)
          ;; (= (vcat . args) rhs)
          (error "use \"(a, b) = ...\" to assign multiple values"))
         (else
          (error (string "invalid assignment location \"" (deparse lhs) "\"")))))))

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
   (lambda (e) (expand-forms (expand-compare-chain (cdr e))))

   'ref
   (lambda (e)
     (let ((args (cddr e)))
       (if (has-parameters? args)
           (error "unexpected semicolon in array expression")
           (expand-forms (partially-expand-ref e)))))

   'curly
   (lambda (e) (expand-forms `(call (core apply_type) ,@(cdr e))))

   'call
   (lambda (e)
     (if (length> e 2)
         (let ((f (cadr e)))
           (cond ((dotop? f)
                  (expand-fuse-broadcast '() `(|.| ,(undotop f) (tuple ,@(cddr e)))))
                 ((and (eq? f 'ccall) (length> e 4))
                  (let* ((cconv (cadddr e))
                         (have-cconv (memq cconv '(cdecl stdcall fastcall thiscall llvmcall)))
                         (after-cconv (if have-cconv (cddddr e) (cdddr e)))
                         (name (caddr e))
                         (RT   (car after-cconv))
                         (argtypes (cadr after-cconv))
                         (args (cddr after-cconv)))
                        (begin
                          (if (not (and (pair? argtypes)
                                        (eq? (car argtypes) 'tuple)))
                              (if (and (pair? RT)
                                       (eq? (car RT) 'tuple))
                                  (error "ccall argument types must be a tuple; try \"(T,)\" and check if you specified a correct return type")
                                  (error "ccall argument types must be a tuple; try \"(T,)\"")))
                          (expand-forms
                           (lower-ccall name RT (cdr argtypes)
                            (if have-cconv (append args (list (list cconv))) args)))))) ;; place (callingconv) at end of arglist
                 ((and (pair? (caddr e))
                       (eq? (car (caddr e)) 'parameters))
                  ;; (call f (parameters . kwargs) ...)
                  (expand-forms
                   (receive
                    (kws args) (separate kwarg? (cdddr e))
                    (let ((kws (append kws (cdr (caddr e)))))
                      (if (null? kws)
                          ;; empty parameters block; issue #18845
                          `(call ,f ,@args)
                          (lower-kw-call f kws args))))))
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
                              (list `(call (core tuple) ,.(reverse run))))
                          (let ((x (car a)))
                            (if (and (length= x 2)
                                     (eq? (car x) '...))
                                (if (null? run)
                                    (list* (cadr x)
                                           (tuple-wrap (cdr a) '()))
                                    (list* `(call (core tuple) ,.(reverse run))
                                           (cadr x)
                                           (tuple-wrap (cdr a) '())))
                                (tuple-wrap (cdr a) (cons x run))))))
                    (expand-forms
                     `(call (core _apply) ,f ,@(tuple-wrap argl '())))))

                 ((and (eq? f '*) (length= e 4))
                  (expand-transposed-op
                   e
                   #(Ac_mul_Bc Ac_mul_B At_mul_Bt At_mul_B A_mul_Bc A_mul_Bt)))
                 ((and (eq? f '/) (length= e 4))
                  (expand-transposed-op
                   e
                   #(Ac_rdiv_Bc Ac_rdiv_B At_rdiv_Bt At_rdiv_B A_rdiv_Bc A_rdiv_Bt)))
                 ((and (eq? f '\\) (length= e 4))
                  (expand-transposed-op
                   e
                   #(Ac_ldiv_Bc Ac_ldiv_B At_ldiv_Bt At_ldiv_B A_ldiv_Bc A_ldiv_Bt)))
                 (else
                  (map expand-forms e))))
         (map expand-forms e)))

   'tuple
   (lambda (e)
     (if (and (length> e 1) (pair? (cadr e)) (eq? (caadr e) 'parameters))
         (error "unexpected semicolon in tuple"))
     (if (any assignment? (cdr e))
         (error "assignment not allowed inside tuple"))
     (expand-forms `(call (core tuple) ,@(cdr e))))

   '=>
   (lambda (e) `(call => ,(expand-forms (cadr e)) ,(expand-forms (caddr e))))

   'cell1d (lambda (e) (error "{ } vector syntax is discontinued"))
   'cell2d (lambda (e) (error "{ } matrix syntax is discontinued"))

   'string
   (lambda (e) (expand-forms `(call (top string) ,@(cdr e))))

   '|::|
   (lambda (e)
     (if (not (length= e 3))
         (error "invalid \"::\" syntax"))
     (if (not (symbol-like? (cadr e)))
         `(call (core typeassert)
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

   '&&     (lambda (e) (expand-forms (expand-and e)))
   '|\|\|| (lambda (e) (expand-forms (expand-or  e)))

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
   '|.\|=|  lower-update-op
   '&=     lower-update-op
   '.&=     lower-update-op
   '$=     lower-update-op
   '⊻=     lower-update-op
   '.⊻=     lower-update-op
   '<<=    lower-update-op
   '.<<=    lower-update-op
   '>>=    lower-update-op
   '.>>=    lower-update-op
   '>>>=   lower-update-op
   '.>>>=   lower-update-op

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
     (syntax-deprecation #f "Expr(:ccall)" "Expr(:call, :ccall)")
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

   'generator
   (lambda (e)
     (let* ((expr  (cadr e))
            (filt? (eq? (car (caddr e)) 'filter))
            (range-exprs (if filt? (cddr (caddr e)) (cddr e)))
            (ranges (map caddr range-exprs))
            (iter (if (length= ranges 1)
                      (car ranges)
                      `(call (top product) ,@ranges)))
            (iter (if filt?
                      `(call (|.| (top Iterators) 'Filter)
                             ,(func-for-generator-ranges (cadr (caddr e)) range-exprs)
                             ,iter)
                      iter)))
       (expand-forms
        `(call (top Generator)
               ,(func-for-generator-ranges expr range-exprs)
               ,iter))))

   'flatten
   (lambda (e) `(call (top Flatten) ,(expand-forms (cadr e))))

   'comprehension
   (lambda (e)
     (if (length> e 2)
         ;; backwards compat for macros that generate :comprehension exprs
         (expand-forms `(comprehension (generator ,@(cdr e))))
         (begin (if (and (eq? (caadr e) 'generator)
                         (any (lambda (x) (eq? x ':)) (cddr (cadr e))))
                    (error "comprehension syntax with `:` ranges has been removed"))
                (expand-forms `(call (top collect) ,(cadr e))))))

   'typed_comprehension
   (lambda (e)
     (expand-forms
      (or (and (eq? (caaddr e) 'generator)
               (let ((ranges (cddr (caddr e))))
                 (if (any (lambda (x) (eq? x ':)) ranges)
                     (error "comprehension syntax with `:` ranges has been removed"))
                 (and (every (lambda (x) (and (pair? x) (eq? (car x) '=)
                                              (pair? (caddr x)) (eq? (car (caddr x)) ':)))
                             ranges)
                      ;; TODO: this is a hack to lower simple comprehensions to loops very
                      ;; early, to greatly reduce the # of functions and load on the compiler
                      (lower-comprehension (cadr e) (cadr (caddr e)) ranges))))
          `(call (top collect) ,(cadr e) ,(caddr e)))))

   'dict_comprehension
   (lambda (e)
     (syntax-deprecation #f "[a=>b for (a,b) in c]" "Dict(a=>b for (a,b) in c)")
     (expand-forms `(call (top Dict) ,(cadr e))))

   'typed_dict_comprehension
   (lambda (e) (expand-forms
                `(call (call (core apply_type) (top Dict) ,@(cdr (cadr e)))
                       ,(caddr e))))))

(define (lower-comprehension atype expr ranges)
  (let ((result    (make-ssavalue))
        (ri        (gensy))
        (oneresult (make-ssavalue))
        (lengths   (map (lambda (x) (make-ssavalue)) ranges))
        (states    (map (lambda (x) (gensy)) ranges))
        (is        (map (lambda (x) (gensy)) ranges))
        (rv        (map (lambda (x) (make-ssavalue)) ranges)))

    ;; construct loops to cycle over all dimensions of an n-d comprehension
    (define (construct-loops ranges rv is states lengths)
      (if (null? ranges)
          `(block (= ,oneresult ,expr)
                  (inbounds true)
                  (call (top setindex!) ,result ,oneresult ,ri)
                  (inbounds pop)
                  (= ,ri (call (top +) ,ri 1)))
          `(block
            (= ,(car states) (call (top start) ,(car rv)))
            (local (:: ,(car is) (call (core typeof) ,(car lengths))))
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
        (= ,result (call (curly Array ,atype ,(length lengths)) ,@lengths))
        (= ,ri 1)
        ,(construct-loops (reverse ranges) (reverse rv) is states (reverse lengths))
        ,result)))))

(define (lhs-vars e)
  (cond ((symbol? e) (list e))
        ((decl? e)   (list (decl-var e)))
        ((and (pair? e) (eq? (car e) 'tuple))
         (apply append (map lhs-vars (cdr e))))
        (else '())))

(define (lhs-decls e)
  (cond ((symbol? e) (list e))
        ((decl? e)   (list e))
        ((and (pair? e) (eq? (car e) 'tuple))
         (apply append (map lhs-decls (cdr e))))
        (else '())))

(define (all-decl-vars e)  ;; map decl-var over every level of an assignment LHS
  (cond ((decl? e)   (decl-var e))
        ((and (pair? e) (eq? (car e) 'tuple))
         (cons 'tuple (map all-decl-vars (cdr e))))
        (else e)))

;; pass 2: identify and rename local vars

(define (check-dups locals others)
  (if (pair? locals)
      (if (or (memq (car locals) (cdr locals)) (memq (car locals) others))
          (error (string "local \"" (car locals) "\" declared twice"))
          (check-dups (cdr locals) others)))
  locals)

(define (find-assigned-vars e env)
  (if (or (not (pair? e)) (quoted? e))
      '()
      (case (car e)
        ((lambda scope-block module toplevel)  '())
        ((method)
         (let ((v (decl-var (method-expr-name e))))
           (append!
            (if (length= e 2) '() (find-assigned-vars (caddr e) env))
            (if (or (not (symbol? v)) (memq v env))
                '()
                (list v)))))
        ((=)
         (let ((v (decl-var (cadr e)))
               (rest (find-assigned-vars (caddr e) env)))
           (if (or (ssavalue? v) (memq v env))
               rest
               (cons v rest))))
        (else
         (apply append! (map (lambda (x) (find-assigned-vars x env))
                             e))))))

(define (find-decls kind e)
  (if (or (not (pair? e)) (quoted? e))
      '()
      (cond ((memq (car e) '(lambda scope-block module toplevel))
             '())
            ((eq? (car e) kind)
             (list (decl-var (cadr e))))
            (else
             (apply append! (map (lambda (x) (find-decls kind x))
                                 e))))))

(define (find-local-decls e) (find-decls 'local e))
(define (find-local-def-decls e) (find-decls 'local-def e))
(define (find-global-decls e) (find-decls 'global e))

(define (implicit-locals e env glob)
  ;; const decls on non-globals introduce locals
  (append! (diff (find-decls 'const e) glob)
           (find-assigned-vars e env)))

(define (unbound-vars e bound tab)
  (cond ((or (eq? e 'true) (eq? e 'false) (eq? e UNUSED)) tab)
        ((symbol? e) (if (not (memq e bound)) (put! tab e #t)) tab)
        ((or (not (pair? e)) (quoted? e)) tab)
        ((memq (car e) '(lambda scope-block module toplevel)) tab)
        ((eq? (car e) 'break-block) (unbound-vars (caddr e) bound tab))
        ((eq? (car e) 'with-static-parameters) (unbound-vars (cadr e) bound tab))
        (else (for-each (lambda (x) (unbound-vars x bound tab))
                            (cdr e))
              tab)))

;; local variable identification and renaming, derived from:
;; 1. (local x) expressions inside this scope-block and lambda
;; 2. (const x) expressions in a scope-block where x is not declared global
;; 3. variables assigned inside this scope-block that don't exist in outer
;;    scopes
;; returns lambdas in the form (lambda (args...) (locals...) body)
(define (resolve-scopes- e env implicitglobals lam renames newlam)
  (cond ((symbol? e) (let ((r (assq e renames)))
                       (if r (cdr r) e))) ;; return the renaming for e, or e
        ((or (not (pair? e)) (quoted? e) (memq (car e) '(toplevel global))) e)
        ((eq? (car e) 'local) '(null)) ;; remove local decls
        ((eq? (car e) 'local-def) '(null)) ;; remove local decls
        ((eq? (car e) 'implicit-global) '(null)) ;; remove implicit-global decls
        ((eq? (car e) 'lambda)
         (let* ((lv (lam:vars e))
                (env (append lv env))
                (body (resolve-scopes- (lam:body e) env
                                       ;; don't propagate implicit globals
                                       ;; issue #7234
                                       '()
                                       e
                                       (filter (lambda (ren) (not (memq (car ren) lv)))
                                               renames)
                                       #t)))
               `(lambda ,(cadr e) ,(caddr e) ,body)))
        ((eq? (car e) 'scope-block)
         (let* ((blok (cadr e)) ;; body of scope-block expression
                (other-locals (if lam (caddr lam) '())) ;; locals that are explicitly part of containing lambda expression
                (iglo (find-decls 'implicit-global blok)) ;; implicitly defined globals used in blok
                (glob (diff (find-global-decls blok) iglo)) ;; all globals declared in blok
                (vars-def (check-dups (find-local-def-decls blok) '()))
                (locals-declared (check-dups (find-local-decls blok) vars-def))
                (locals-implicit (diff (implicit-locals
                                         blok
                                         ;; being declared global prevents a variable
                                         ;; assignment from introducing a local
                                         (append env glob implicitglobals iglo)
                                         (append glob iglo))
                                       vars-def))
                (vars (delete-duplicates (append! locals-declared locals-implicit)))
                (all-vars (append vars vars-def))
                (need-rename?
                 (lambda (vars)
                  ;; compute the set of locals introduced by this scope which
                  ;; have the same name as a variable used in an outer scope
                  (if (or newlam (not lam))
                      '()
                       (filter (lambda (v) (or (memq v env)
                                               (memq v other-locals)
                                               (memq v (caddr lam))))
                               vars))))
                (need-rename (need-rename? vars))
                (need-rename-def (need-rename? vars-def))
                ;; new gensym names for conflicting variables
                (renamed (map named-gensy need-rename))
                (renamed-def (map named-gensy need-rename-def))
                (new-env (append all-vars glob env)) ;; all variables declared in or outside blok
                (new-iglo-table ;; initial list of implicit globals from outside blok which aren't part of the local vars
                  (let ((tab (table)))
                    (for-each (lambda (v) (if (not (memq v all-vars)) (put! tab v #t))) iglo)
                    (for-each (lambda (v) (if (not (memq v all-vars)) (put! tab v #t))) implicitglobals)
                    tab))
                (new-iglo (table.keys ;; compute list of all globals used implicitly in blok
                            (unbound-vars blok
                                          new-env ;; list of everything else
                                          new-iglo-table)))
                ;; combine the list of new renamings with the inherited list
                (new-renames (append (map cons need-rename renamed) ;; map from definition name -> gensym name
                                     (map cons need-rename-def renamed-def)
                                     (map (lambda (g) (cons g `(outerref ,g))) new-iglo)
                                     (filter (lambda (ren) ;; old renames list, with anything in vars removed
                                               (not (or (memq (car ren) all-vars)
                                                        (memq (car ren) iglo)
                                                        (memq (car ren) implicitglobals)
                                                        (memq (car ren) glob))))
                                             renames)))
                (body (resolve-scopes- blok new-env new-iglo lam new-renames #f))
                (real-new-vars (append (diff vars need-rename) renamed))
                (real-new-vars-def (append (diff vars-def need-rename-def) renamed-def)))
               (for-each (lambda (v)
                           (if (memq v all-vars)
                               (error (string "variable \"" v "\" declared both local and global"))))
                         glob)
               (if lam ;; update in-place the list of local variables in lam
                   (set-car! (cddr lam)
                             (append! (caddr lam) real-new-vars real-new-vars-def)))
               (insert-after-meta ;; return the new, expanded scope-block
                (if (and (pair? body) (eq? (car body) 'block))
                    body
                    `(block ,body))
                (append! (map (lambda (v) `(local ,v)) real-new-vars)
                         (map (lambda (v) `(local-def ,v)) real-new-vars-def)))))
        ((eq? (car e) 'module)
         (error "module expression not at top level"))
        ((eq? (car e) 'break-block)
         `(break-block ,(cadr e) ;; ignore type symbol of break-block expression
                       ,(resolve-scopes- (caddr e) env implicitglobals lam renames #f))) ;; body of break-block expression
        ((eq? (car e) 'with-static-parameters)
         `(with-static-parameters ;; ignore list of sparams in break-block expression
            ,(resolve-scopes- (cadr e) env implicitglobals lam renames #f)
            ,@(cddr e))) ;; body of break-block expression
        (else
         (cons (car e)
               (map (lambda (x)
                      (resolve-scopes- x env implicitglobals lam renames #f))
                    (cdr e))))))

(define (resolve-scopes e) (resolve-scopes- e '() '() #f '() #f))

;; pass 3: analyze variables

;; names of arguments and local vars
(define (lambda-all-vars e)
  (append (lam:vars e) (caddr e)))

;; compute set of variables referenced in a lambda but not bound by it
(define (free-vars- e tab)
  (cond ((or (eq? e 'true) (eq? e 'false) (eq? e UNUSED)) tab)
        ((symbol? e) (put! tab e #t))
        ((and (pair? e) (eq? (car e) 'outerref)) (put! tab (cadr e) #t))
        ((and (pair? e) (eq? (car e) 'break-block)) (free-vars- (caddr e) tab))
        ((and (pair? e) (eq? (car e) 'with-static-parameters)) (free-vars- (cadr e) tab))
        ((or (atom? e) (quoted? e)) tab)
        ((eq? (car e) 'lambda)
         (let ((bound (lambda-all-vars e)))
           (for-each (lambda (v) (if (not (memq v bound)) (put! tab v #t)))
                     (free-vars (lam:body e))))
         tab)
        (else
         (for-each (lambda (x) (free-vars- x tab))
                   (cdr e))
         tab)))

(define (free-vars e)
  (table.keys (free-vars- e (table))))

(define (analyze-vars-lambda e env captvars sp new-sp)
  (let* ((args (lam:args e))
         (locl (caddr e))
         (allv (nconc (map arg-name args) locl))
         (fv   (let* ((fv (diff (free-vars (lam:body e)) allv))
                      ;; add variables referenced in declared types for free vars
                      (dv (apply nconc (map (lambda (v)
                                              (let ((vi (var-info-for v env)))
                                                (if vi (free-vars (vinfo:type vi)) '())))
                                            fv))))
                 (append (diff dv fv) fv)))
         (glo  (find-global-decls (lam:body e)))
         ;; make var-info records for vars introduced by this lambda
         (vi   (nconc
                (map (lambda (decl) (make-var-info (decl-var decl)))
                     args)
                (map make-var-info locl)))
         (capt-sp (filter (lambda (v) (and (memq v fv) (not (memq v glo)) (not (memq v new-sp))))
                          sp))
         ;; captured vars: vars from the environment that occur
         ;; in our set of free variables (fv).
         (cv    (append (filter (lambda (v) (and (memq (vinfo:name v) fv)
                                                 (not (memq (vinfo:name v) new-sp))
                                                 (not (memq (vinfo:name v) glo))))
                                env)
                        (map make-var-info capt-sp))))
    (analyze-vars (lam:body e)
                  (append vi
                          ;; new environment: add our vars
                          (filter (lambda (v)
                                    (and (not (memq (vinfo:name v) allv))
                                         (not (memq (vinfo:name v) glo))))
                                  env))
                  cv (delete-duplicates (append new-sp sp)))
    ;; mark all the vars we capture as captured
    (for-each (lambda (v) (vinfo:set-capt! v #t))
              cv)
    (set-car! (cddr e)
              `(,vi ,cv 0 ,(delete-duplicates (append new-sp capt-sp))))
    e))

;; this pass records information about variables used by closure conversion.
;; finds which variables are assigned or captured, and records variable
;; type declarations.
;; this info is recorded by setting the second argument of `lambda` expressions
;; in-place to
;;   (var-info-lst captured-var-infos ssavalues static_params)
;; where var-info-lst is a list of var-info records
(define (analyze-vars e env captvars sp)
  (if (or (atom? e) (quoted? e))
      e
      (case (car e)
        ((local-def) ;; a local that we know has an assignment that dominates all usages
         (let ((vi (var-info-for (cadr e) env)))
              (vinfo:set-never-undef! vi #t)))
        ((=)
         (let ((vi (var-info-for (cadr e) env)))
           (if vi
               (begin (if (vinfo:asgn vi)
                          (vinfo:set-sa! vi #f)
                          (vinfo:set-sa! vi #t))
                      (vinfo:set-asgn! vi #t))))
         (analyze-vars (caddr e) env captvars sp))
        ((call)
         (let ((vi (var-info-for (cadr e) env)))
           (if vi
               (vinfo:set-called! vi #t))
           (for-each (lambda (x) (analyze-vars x env captvars sp))
                     (cdr e))))
        ((decl impl-decl)
         ;; handle var::T declaration by storing the type in the var-info
         ;; record. for non-symbols or globals, emit a type assertion.
         (let ((vi (var-info-for (cadr e) env)))
           (if vi
               (begin (if (not (eq? (vinfo:type vi) 'Any))
                          (error (string "multiple type declarations for \""
                                         (cadr e) "\"")))
                      (if (assq (cadr e) captvars)
                          (error (string "type of \"" (cadr e)
                                         "\" declared in inner scope")))
                      (vinfo:set-type! vi (caddr e))))))
        ((lambda)
         (analyze-vars-lambda e env captvars sp '()))
        ((with-static-parameters)
         ;; (with-static-parameters func_expr sp_1 sp_2 ...)
         (assert (eq? (car (cadr e)) 'lambda))
         (analyze-vars-lambda (cadr e) env captvars sp
                              (cddr e)))
        ((method)
         (if (length= e 2)
             (let ((vi (var-info-for (method-expr-name e) env)))
               (if vi
                   (begin (if (vinfo:asgn vi)
                              (vinfo:set-sa! vi #f)
                              (vinfo:set-sa! vi #t))
                          (vinfo:set-asgn! vi #t)))
               e)
             (begin (analyze-vars (caddr e) env captvars sp)
                    (assert (eq? (car (cadddr e)) 'lambda))
                    (analyze-vars-lambda (cadddr e) env captvars sp
                                         (method-expr-static-parameters e)))))
        ((module toplevel) e)
        (else (for-each (lambda (x) (analyze-vars x env captvars sp))
                        (cdr e))))))

(define (analyze-variables! e) (analyze-vars e '() '() '()) e)

;; pass 4: closure conversion

;; this pass lifts all inner functions to the top level by generating
;; a type for them. for example `f(x) = y->(y+x)` is converted to
#|
immutable yt{T}
    x::T
end

(self::yt)(y) = y + self.x

f(x) = yt(x)
|#

(define (type-for-closure-parameterized name P names fields types super)
   (let ((n (length P)))
    `((thunk
      (lambda ()
        (() () 0 ())
        (body (global ,name) (const ,name)
              ,@(map (lambda (p n) `(= ,p (call (core TypeVar) ',n (core Any)))) P names)
              (composite_type ,name (call (core svec) ,@P)
                              (call (core svec) ,@(map (lambda (v) `',v) fields))
                              ,super
                              (call (core svec) ,@types) false ,(length fields))
              (return (null))))))))

(define (type-for-closure name fields super)
  `((thunk (lambda ()
            (() () 0 ())
            (body (global ,name) (const ,name)
                  (composite_type ,name (call (core svec))
                                  (call (core svec) ,@(map (lambda (v) `',v) fields))
                                  ,super
                                  (call (core svec) ,@(map (lambda (v) 'Any) fields))
                                  false ,(length fields))
                  (return (null)))))))


;; better versions of above, but they get handled wrong in many places
;; need to fix that in order to handle #265 fully (and use the definitions)

;; template for generating a closure type with parameters
;(define (type-for-closure-parameterized name P names fields types super)
;  (let ((n (length P)))
;        `((global ,name)
;          (const ,name)
;          ,@(map (lambda (p n) `(= ,p (call (core TypeVar) ',n (core Any)))) P names)
;          (composite_type ,name (call (core svec) ,@P)
;                          (call (core svec) ,@(map (lambda (v) `',v) fields))
;                          ,super
;                          (call (core svec) ,@types) false ,(length fields)))))

;; ... and without parameters
;(define (type-for-closure name fields super)
;  `((global ,name)
;    (const ,name)
;    (composite_type ,name (call (core svec))
;                    (call (core svec) ,@(map (lambda (v) `',v) fields))
;                    ,super
;                    (call (core svec) ,@(map (lambda (v) 'Any) fields))
;                    false ,(length fields))))


(define (vinfo:not-capt vi)
  (list (car vi) (cadr vi) (logand (caddr vi) (lognot 5))))

(define (clear-capture-bits vinfos)
  (map vinfo:not-capt vinfos))

(define (convert-lambda lam fname interp capt-sp)
  `(lambda ,(lam:args lam)
     (,(clear-capture-bits (car (lam:vinfo lam)))
      ()
      ,(caddr (lam:vinfo lam))
      ,(delete-duplicates (append (lam:sp lam) capt-sp)))
     ,(add-box-inits-to-body
       lam
       (cl-convert (cadddr lam) fname lam (table) #f interp))))

(define (convert-for-type-decl rhs t)
  (if (eq? t 'Any)
      rhs
      `(call (core typeassert)
             (call (top convert) ,t ,rhs)
             ,t)))

;; convert assignment to a closed variable to a setfield! call.
;; while we're at it, generate `convert` calls for variables with
;; declared types.
;; when doing this, the original value needs to be preserved, to
;; ensure the expression `a=b` always returns exactly `b`.
(define (convert-assignment var rhs0 fname lam interp)
  (let* ((vi (assq var (car  (lam:vinfo lam))))
         (cv (assq var (cadr (lam:vinfo lam))))
         (vt  (or (and vi (vinfo:type vi))
                  (and cv (vinfo:type cv))
                  'Any))
         (closed (and cv (vinfo:asgn cv) (vinfo:capt cv)))
         (capt   (and vi (vinfo:asgn vi) (vinfo:capt vi))))
    (if (and (not closed) (not capt) (eq? vt 'Any))
        `(= ,var ,rhs0)
        (let* ((rhs1 (if (or (ssavalue? rhs0) (simple-atom? rhs0)
                             (equal? rhs0 '(the_exception)))
                         rhs0
                         (make-ssavalue)))
               (rhs  (if (eq? vt 'Any)
                         rhs1
                         (convert-for-type-decl rhs1 (cl-convert vt fname lam #f #f interp))))
               (ex (cond (closed `(call (core setfield!)
                                        ,(if interp
                                             `($ ,var)
                                             `(call (core getfield) ,fname (inert ,var)))
                                        (inert contents)
                                        ,rhs))
                         (capt `(call (core setfield!) ,var (inert contents) ,rhs))
                         (else `(= ,var ,rhs)))))
          (if (eq? rhs1 rhs0)
              `(block ,ex ,rhs0)
              `(block (= ,rhs1 ,rhs0)
                      ,ex
                      ,rhs1))))))

;; replace leading (function) argument type with `typ`
(define (fix-function-arg-type te typ iskw namemap type-sp)
  (let* ((typapp (caddr te))
         (types  (pattern-replace
                  (pattern-set
                   (pattern-lambda (call (core (-/ Typeof)) name)
                                   (get namemap name __)))
                  (cddr typapp)))
         (closure-type (if (null? type-sp)
                           typ
                           `(call (core apply_type) ,typ ,@type-sp)))
         (newtypes
          (if iskw
              `(,(car types) ,(cadr types) ,closure-type ,@(cdddr types))
              `(,closure-type ,@(cdr types)))))
    `(call (core svec) (call (core svec) ,@newtypes)
           (call (core svec) ,@(append (cddr (cadddr te)) type-sp)))))

;; collect all toplevel-butlast expressions inside `e`, and return
;; (ex . stmts), where `ex` is the expression to evaluated and
;; `stmts` is a list of statements to move to the top level.
;; TODO: this implementation seems quite inefficient.
(define (lift-toplevel e)
  (if (atom? e) (cons e '())
      (let* ((rec (map lift-toplevel e))
             (e2  (map car rec))
             (tl  (apply append (map cdr rec))))
        (if (eq? (car e) 'toplevel-butlast)
            (cons (last e2) (append tl (butlast (cdr e2))))
            (cons e2 tl)))))

(define (first-non-meta blk)
  (let loop ((xs (cdr blk)))
    (if (null? xs)
        #f
        (let ((elt (car xs)))
          (if (and (pair? elt) (eq? (car elt) 'meta))
              (loop (cdr xs))
              elt)))))

;; return `body` with `stmts` inserted after any meta nodes
(define (insert-after-meta body stmts)
  (let ((meta (take-while (lambda (x) (and (pair? x)
                                           (memq (car x) '(line meta))))
                          (cdr body))))
    `(,(car body)
      ,@meta
      ,@stmts
      ,@(list-tail body (+ 1 (length meta))))))

(define (add-box-inits-to-body lam body)
  (let ((args (map arg-name (lam:args lam)))
        (vis  (car (lam:vinfo lam))))
    ;; insert Box allocations for captured/assigned arguments
    (insert-after-meta
     body
     (apply append
            (map (lambda (arg)
                   (let ((vi (assq arg vis)))
                     (if (and vi (vinfo:asgn vi) (vinfo:capt vi))
                         `((= ,arg (call (core Box) ,arg)))
                         '())))
                 args)))))

(define (take-statements-while pred body)
  (let ((acc '()))
    (define (take expr)
      ;; returns #t as long as exprs match and we should continue
      (cond ((and (pair? expr) (memq (car expr) '(block body)))
             (let loop ((xs (cdr expr)))
               (cond ((null? xs) #t)
                     ((take (car xs)) (loop (cdr xs)))
                     (else #f))))
            ((pred expr)
             (set! acc (cons expr acc))
             #t)
            (else #f)))
    (take body)
    (reverse! acc)))

;; clear capture bit for vars assigned once at the top, to avoid allocating
;; some unnecessary Boxes.
(define (lambda-optimize-vars! lam)
  (define (expr-uses-var ex v)
    (cond ((assignment? ex) (expr-contains-eq v (caddr ex)))
          ((eq? (car ex) 'method)
           (and (length> ex 2)
                (assq v (cadr (lam:vinfo (cadddr ex))))))
          (else (expr-contains-eq v ex))))
  (assert (eq? (car lam) 'lambda))
  (let ((vi (car (lam:vinfo lam))))
    (if (and (any vinfo:capt vi)
             (any vinfo:sa vi))
        (let* ((leading
                (filter (lambda (x) (and (pair? x)
                                         (let ((cx (car x)))
                                           (or (and (eq? cx 'method) (length> x 2))
                                               (eq? cx '=)
                                               (eq? cx 'call)))))
                        (take-statements-while
                         (lambda (e)
                           (or (atom? e)
                               (memq (car e) '(quote top core line inert local local-def unnecessary
                                               meta inbounds boundscheck simdloop
                                               implicit-global global globalref outerref
                                               const = null method call))))
                         (lam:body lam))))
               (unused (map cadr (filter (lambda (x) (memq (car x) '(method =)))
                                         leading))))
              ;; TODO: reorder leading statements to put assignments where the RHS is
              ;; `simple-atom?` at the top.
              (for-each (lambda (e)
                          (set! unused (filter (lambda (v) (not (expr-uses-var e v)))
                                               unused))
                          (if (and (memq (car e) '(method =)) (memq (cadr e) unused))
                              (let ((v (assq (cadr e) vi)))
                                   (if v (vinfo:set-never-undef! v #t)))))
                        leading)))
    (for-each (lambda (v)
                (if (and (vinfo:sa v) (vinfo:never-undef v))
                    (set-car! (cddr v) (logand (caddr v) (lognot 5)))))
              vi)
    lam))

(define (is-var-boxed? v lam)
  (or (let ((vi (assq v (car (lam:vinfo lam)))))
        (and vi (vinfo:asgn vi) (vinfo:capt vi)))
      (let ((cv (assq v (cadr (lam:vinfo lam)))))
        (and cv (vinfo:asgn cv) (vinfo:capt cv)))))

(define (toplevel-preserving? e)
  (and (pair? e) (memq (car e) '(if block body trycatch))))

(define (map-cl-convert exprs fname lam namemap toplevel interp)
  (if toplevel
      (map (lambda (x)
             (let ((tl (lift-toplevel (cl-convert x fname lam namemap
                                                  (and toplevel (toplevel-preserving? x))
                                                  interp))))
               (if (null? (cdr tl))
                   (car tl)
                   `(block ,@(cdr tl) ,(car tl)))))
           exprs)
      (map (lambda (x) (cl-convert x fname lam namemap #f interp)) exprs)))

(define (cl-convert e fname lam namemap toplevel interp)
  (if (and (not lam)
           (not (and (pair? e) (memq (car e) '(lambda method macro)))))
      (if (atom? e) e
          (cons (car e) (map-cl-convert (cdr e) fname lam namemap toplevel interp)))
      (cond
       ((symbol? e)
        (let ((vi (assq e (car  (lam:vinfo lam))))
              (cv (assq e (cadr (lam:vinfo lam)))))
          (cond ((eq? e fname) e)
                ((memq e (lam:sp lam)) e)
                (cv
                 (let ((access (if interp
                                   `($ (call (core QuoteNode) ,e))
                                   `(call (core getfield) ,fname (inert ,e)))))
                   (if (and (vinfo:asgn cv) (vinfo:capt cv))
                       (let ((val `(call (core getfield) ,access (inert contents))))
                         (if (eq? (vinfo:type cv) 'Any)
                             val
                             `(call (core typeassert) ,val
                                    ,(cl-convert (vinfo:type cv) fname lam namemap toplevel interp))))
                       access)))
                (vi
                 (if (and (vinfo:asgn vi) (vinfo:capt vi))
                     (let ((val `(call (core getfield) ,e (inert contents))))
                       (if (eq? (vinfo:type vi) 'Any)
                           val
                           `(call (core typeassert) ,val
                                  ,(cl-convert (vinfo:type vi) fname lam namemap toplevel interp))))
                     e))
                (else e))))
       ((atom? e) e)
       (else
        (case (car e)
          ((quote top core globalref outerref line break inert module toplevel null meta) e)
          ((=)
           (let ((var (cadr e))
                 (rhs (cl-convert (caddr e) fname lam namemap toplevel interp)))
             (if (ssavalue? var)
                 `(= ,var ,rhs)
                 (convert-assignment var rhs fname lam interp))))
          ((local-def) ;; make new Box for local declaration of defined variable
           (let ((vi (assq (cadr e) (car (lam:vinfo lam)))))
             (if (and vi (vinfo:asgn vi) (vinfo:capt vi))
                 `(= ,(cadr e) (call (core Box)))
                 '(null))))
          ((local) ;; convert local declarations to newvar statements
           (let ((vi (assq (cadr e) (car (lam:vinfo lam)))))
             (if (and vi (vinfo:asgn vi) (vinfo:capt vi))
                 `(= ,(cadr e) (call (core Box)))
                 (if (vinfo:never-undef vi)
                     '(null)
                     `(newvar ,(cadr e))))))
          ((const)
           (if (or (assq (cadr e) (car  (lam:vinfo lam)))
                   (assq (cadr e) (cadr (lam:vinfo lam))))
               '(null)
               e))
          ((method)
           (let* ((name  (method-expr-name e))
                  (short (length= e 2))  ;; function f end
                  (lam2  (if short #f (cadddr e)))
                  (vis   (if short '(() () ()) (lam:vinfo lam2)))
                  (cvs   (map car (cadr vis)))
                  (local? (lambda (s) (and (symbol? s)
                               (or (assq s (car  (lam:vinfo lam)))
                                   (assq s (cadr (lam:vinfo lam)))))))
                  (local (and lam (local? name)))
                  (sig      (and (not short) (caddr e)))
                  (sp-inits (if (or short (not (eq? (car sig) 'block)))
                                '()
                                (map-cl-convert (butlast (cdr sig))
                                                fname lam namemap toplevel interp)))
                  (sig      (and sig (if (eq? (car sig) 'block)
                                         (last sig)
                                         sig))))
             (if local
                 (begin (if (memq name (lam:args lam))
                            (error (string "cannot add method to function argument " name)))
                        (if (eqv? (string.char (string name) 0) #\@)
                            (error "macro definition not allowed inside a local scope"))))
             (if lam2
                 (begin
                   ;; mark all non-arguments as assigned, since locals that are never assigned
                   ;; need to be handled the same as those that are (i.e., boxed).
                   (for-each (lambda (vi) (vinfo:set-asgn! vi #t))
                             (list-tail (car (lam:vinfo lam2)) (length (lam:args lam2))))
                   (lambda-optimize-vars! lam2)))
             (if (not local) ;; not a local function; will not be closure converted to a new type
                 (cond (short e)
                       ((null? cvs)
                        `(block
                          ,@sp-inits
                          (method ,name ,(cl-convert sig fname lam namemap toplevel interp)
                                  (lambda ,(cadr lam2)
                                    (,(clear-capture-bits (car vis))
                                     ,@(cdr vis))
                                    ,(add-box-inits-to-body
                                      lam2
                                      (cl-convert (cadddr lam2) 'anon lam2 (table) #f interp)))
                                  ,(last e))))
                       (else
                        (let* ((exprs     (lift-toplevel (convert-lambda lam2 '|#anon| #t '())))
                               (top-stmts (cdr exprs))
                               (newlam    (renumber-slots-and-labels (linearize (car exprs)))))
                          `(toplevel-butlast
                            ,@top-stmts
                            ,@sp-inits
                            (method ,name ,(cl-convert sig fname lam namemap toplevel interp)
                                    ,(julia-expand-macros `(quote ,newlam))
                                    ,(last e))))))
                 ;; local case - lift to a new type at top level
                 (let* ((exists (get namemap name #f))
                        (type-name  (or exists
                                        (and name
                                             (symbol (string "#" name "#" (current-julia-module-counter))))))
                        (alldefs (expr-find-all
                                  (lambda (ex) (and (eq? (car ex) 'method)
                                                    (not (eq? ex e))
                                                    (length> ex 2)
                                                    (eq? (method-expr-name ex) name)))
                                  (lam:body lam)
                                  identity
                                  (lambda (x) (and (pair? x) (not (eq? (car x) 'lambda))))))
                        (all-capt-vars   (delete-duplicates
                                          (apply append  ;; merge captured vars from all definitions
                                                 cvs
                                                 (map (lambda (methdef)
                                                        (map car (cadr (lam:vinfo (cadddr methdef)))))
                                                      alldefs))))
                        (all-sparams   (delete-duplicates  ;; static params from all definitions
                                        (apply append
                                               (if lam2 (lam:sp lam2) '())
                                               (map (lambda (methdef) (lam:sp (cadddr methdef)))
                                                    alldefs))))
                        (capt-sp (simple-sort (intersect all-capt-vars all-sparams))) ; the intersection is the list of sparams that need to be captured
                        (capt-vars (diff all-capt-vars capt-sp)) ; remove capt-sp from capt-vars
                        (find-locals-in-method-sig (lambda (methdef)
                                                     (expr-find-all
                                                      (lambda (e) (and (pair? e) (eq? (car e) 'outerref)
                                                                       (let ((s (cadr e)))
                                                                            (and (symbol? s)
                                                                                 (not (eq? name s))
                                                                                 (not (memq s capt-sp))
                                                                                 (or ;(local? s) ; TODO: make this work for local variables too?
                                                                                   (memq s (lam:sp lam)))))))
                                                      (caddr methdef)
                                                      (lambda (e) (cadr e)))))
                        (sig-locals (simple-sort
                                     (delete-duplicates  ;; locals used in sig from all definitions
                                      (apply append      ;; will convert these into sparams for dispatch
                                             (if lam2 (find-locals-in-method-sig e) '())
                                             (map find-locals-in-method-sig alldefs)))))
                        (closure-param-names (append capt-sp sig-locals)) ; sparams for the closure method declaration
                        (closure-param-syms (map (lambda (s) (make-ssavalue)) closure-param-names))
                        (typedef  ;; expression to define the type
                         (let* ((fieldtypes (map (lambda (v)
                                                   (if (is-var-boxed? v lam)
                                                       '(core Box)
                                                       (make-ssavalue)))
                                                  capt-vars))
                                (para (append closure-param-syms
                                              (filter (lambda (v) (ssavalue? v)) fieldtypes)))
                                (fieldnames (append closure-param-names (filter (lambda (v) (not (is-var-boxed? v lam))) capt-vars))))
                           (if (null? para)
                               (type-for-closure type-name capt-vars '(core Function))
                               (type-for-closure-parameterized type-name para fieldnames capt-vars fieldtypes '(core Function)))))
                        (mk-method ;; expression to make the method
                          (if short '()
                              (let* ((iskw ;; TODO jb/functions need more robust version of this
                                      (contains (lambda (x) (eq? x 'kwftype)) sig))
                                     (renamemap (map cons closure-param-names closure-param-syms))
                                     (arg-defs (replace-outer-vars
                                                (fix-function-arg-type sig type-name iskw namemap closure-param-syms)
                                                renamemap)))
                                    (append (map (lambda (gs tvar)
                                                   (make-assignment gs `(call (core TypeVar) ',tvar (core Any))))
                                                  closure-param-syms closure-param-names)
                                            `((method #f ,(cl-convert arg-defs fname lam namemap toplevel interp)
                                                  ,(convert-lambda lam2
                                                                   (if iskw
                                                                       (caddr (lam:args lam2))
                                                                       (car (lam:args lam2)))
                                                                   #f closure-param-names)
                                                  ,(last e)))))))
                        (mk-closure  ;; expression to make the closure
                         (let* ((var-exprs (map (lambda (v)
                                                  (let ((cv (assq v (cadr (lam:vinfo lam)))))
                                                    (if cv
                                                        (if interp
                                                            `($ (call (core QuoteNode) ,v))
                                                            `(call (core getfield) ,fname (inert ,v)))
                                                        v)))
                                                capt-vars))
                                (P (append
                                    (map (lambda (n) `(outerref ,n)) closure-param-names)
                                    (filter identity (map (lambda (v ve)
                                                            (if (is-var-boxed? v lam)
                                                                #f
                                                                `(call (core typeof) ,ve)))
                                                          capt-vars var-exprs)))))
                           `(new ,(if (null? P)
                                      type-name
                                      `(call (core apply_type) ,type-name ,@P))
                                 ,@var-exprs))))
                   `(toplevel-butlast
                     ,@(if exists
                           '()
                            (begin (and name (put! namemap name type-name))
                                   typedef))
                     ,@sp-inits
                     ,@mk-method
                     ,(if exists
                          '(null)
                          (convert-assignment name mk-closure fname lam interp)))))))
          ((lambda)  ;; happens inside (thunk ...) and generated function bodies
           (for-each (lambda (vi) (vinfo:set-asgn! vi #t))
                     (list-tail (car (lam:vinfo e)) (length (lam:args e))))
           `(lambda ,(cadr e)
              (,(clear-capture-bits (car (lam:vinfo e)))
               () ,@(cddr (lam:vinfo e)))
              (block
               ,@(map-cl-convert (cdr (lam:body e)) 'anon
                                 (lambda-optimize-vars! e)
                                 (table)
                                 (null? (cadr e)) ;; only toplevel thunks have 0 args
                                 interp))))
          ;; remaining `::` expressions are type assertions
          ((|::|)
           (cl-convert `(call (core typeassert) ,@(cdr e)) fname lam namemap toplevel interp))
          ;; remaining `decl` expressions are only type assertions if the
          ;; argument is global or a non-symbol.
          ((impl-decl)
           (if (or (assq (cadr e) (car  (lam:vinfo lam)))
                   (assq (cadr e) (cadr (lam:vinfo lam))))
               (let ((str-e (deparse `(|::| ,@(cdr e)))))
                    (syntax-deprecation #f str-e (string "local " str-e))
                    '(null))
               (cl-convert `(call (core typeassert) ,@(cdr e)) fname lam namemap toplevel interp)))
          ((decl)
           (cond ((not (symbol? (cadr e)))
                  (cl-convert `(call (core typeassert) ,@(cdr e)) fname lam namemap toplevel interp))
                 ((or (assq (cadr e) (car  (lam:vinfo lam)))
                      (assq (cadr e) (cadr (lam:vinfo lam))))
                  '(null))
                 (else (syntax-deprecation #f (string "global " (deparse `(|::| ,@(cdr e)))) "typeassert")
                       (cl-convert `(call (core typeassert) ,@(cdr e)) fname lam namemap toplevel interp))))
          ;; `with-static-parameters` expressions can be removed now; used only by analyze-vars
          ((with-static-parameters)
           (cl-convert (cadr e) fname lam namemap toplevel interp))
          (else (cons (car e)
                      (map-cl-convert (cdr e) fname lam namemap toplevel interp))))))))

(define (closure-convert e) (cl-convert e #f #f #f #f #f))

;; pass 5: convert to linear IR

;; with this enabled, all nested calls are assigned to numbered locations
(define *very-linear-mode* #f)

(define (linearize e)
  (cond ((or (not (pair? e)) (quoted? e)) e)
        ((eq? (car e) 'lambda)
         (set-car! (cdddr e) (compile-body (cadddr e) (append (car (caddr e))
                                                              (cadr (caddr e)))
                                           e)))
        (else (for-each linearize (cdr e))))
  e)

;; this pass behaves like an interpreter on the given code.
;; to perform stateful operations, it calls `emit` to record that something
;; needs to be done. in value position, it returns an expression computing
;; the needed value. in the future, all intermediate values will have
;; numbered slots (or be simple immediate values), and then those will be the
;; only possible returned values.
(define (compile-body e vi lam)
  (let ((code '())
        (filename 'none)
        (first-line #t)
        (rett #f)
        (arg-map #f)          ;; map arguments to new names if they are assigned
        (label-counter 0)     ;; counter for generating label addresses
        (label-map (table))   ;; maps label names to generated addresses
        (label-level (table)) ;; exception handler level of each label
        (handler-goto-fixups '())  ;; `goto`s that might need `leave` exprs added
        (handler-level 0))  ;; exception handler nesting depth
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
    (define (emit-return x)
      (let ((rv (if (> handler-level 0)
                    (let ((tmp (if (or (simple-atom? x) (ssavalue? x) (equal? x '(null)))
                                   #f (make-ssavalue))))
                      (if tmp (emit `(= ,tmp ,x)))
                      (emit `(leave ,handler-level))
                      (or tmp x))
                    x)))
        (if rett
            (emit `(return ,(convert-for-type-decl rv rett)))
            (emit `(return ,rv)))))
    (define (new-mutable-var . name)
      (let ((g (if (null? name) (gensy) (named-gensy (car name)))))
        (set-car! (lam:vinfo lam) (append (car (lam:vinfo lam)) `((,g Any 2))))
        g))
    ;; evaluate the arguments of a call, creating temporary locations as needed
    (define (compile-args lst break-labels)
      (if (null? lst) '()
          (let ((temps? (or *very-linear-mode*
                            (any (lambda (e)
                                   (expr-contains-p (lambda (x) (or (and (assignment? x) (symbol? (cadr x)))
                                                                    (and (pair? x) (eq? (car x) 'block))))
                                                    e))
                                 (cdr lst))))
                (simple? (every (lambda (x) (or (simple-atom? x) (symbol? x) (ssavalue? x)
                                                (and (pair? x)
                                                     (memq (car x) '(quote inert top core globalref outerref copyast)))))
                                lst)))
            (let loop ((lst  lst)
                       (vals '()))
              (if (null? lst)
                  (reverse! vals)
                  (let* ((arg (car lst))
                         (aval (compile arg break-labels #t #f)))
                    (loop (cdr lst)
                          (cons (if (and temps? (not simple?)
                                         (not (simple-atom? arg))  (not (ssavalue? arg))
                                         (not (simple-atom? aval)) (not (ssavalue? aval))
                                         (not (and (pair? arg)
                                                   (memq (car arg) '(& quote inert top core globalref outerref copyast))))
                                         (not (and (symbol? arg)
                                                   (or (null? (cdr lst))
                                                       (null? vals)))))
                                    (let ((tmp (make-ssavalue)))
                                      (emit `(= ,tmp ,aval))
                                      tmp)
                                    aval)
                                vals))))))))
    (define (compile-cond ex break-labels)
      (let ((cnd (compile ex break-labels #t #f)))
        (if (and *very-linear-mode*
                 (not (or (simple-atom? cnd) (ssavalue? cnd) (symbol? cnd))))
            (let ((tmp (make-ssavalue)))
              (emit `(= ,tmp ,cnd))
              tmp)
            cnd)))
    ;; the interpreter loop. `break-labels` keeps track of the labels to jump to
    ;; for all currently closing break-blocks.
    ;; `value` means we are in a context where a value is required; a meaningful
    ;; value must be returned.
    ;; `tail` means we are in tail position, where a value needs to be `return`ed
    ;; from the current function.
    (define (compile e break-labels value tail)
      (if (or (not (pair? e)) (memq (car e) '(null ssavalue quote inert top core copyast the_exception $
                                                   globalref outerref cdecl stdcall fastcall thiscall llvmcall)))
          (let ((e (if (and arg-map (symbol? e))
                       (get arg-map e e)
                       e)))
            (cond (tail  (emit-return e))
                  (value e)
                  ((or (eq? e 'true) (eq? e 'false)) #f)
                  ((symbol? e) (emit e) #f)  ;; keep symbols for undefined-var checking
                  ((and (pair? e) (eq? (car e) 'outerref)) (emit e) #f)  ;; keep globals for undefined-var checking
                  ((and (pair? e) (eq? (car e) 'globalref)) (emit e) #f) ;; keep globals for undefined-var checking
                  (else #f)))
          (case (car e)
            ((call new foreigncall)
             (let* ((args (if (eq? (car e) 'foreigncall)
                              ;; NOTE: 2nd and 3rd arguments of ccall must be left in place
                              ;;       the 1st should be compiled if an atom.
                              (append (list)
                                      (cond (atom? (cadr e) (compile-args (list (cadr e)) break-labels))
                                            (else (cadr e)))
                                      (list-head (cddr e) 2)
                                      (compile-args (list-tail e 4) break-labels))
                              (compile-args (cdr e) break-labels)))
                    (callex (cons (car e) args)))
               (cond (tail (emit-return callex))
                     (value callex)
                     ((eq? (car e) 'new) #f)
                     (else (emit callex)))))
            ((=)
             (let* ((rhs (compile (caddr e) break-labels #t #f))
                    (lhs (cadr e))
                    (lhs (if (and arg-map (symbol? lhs))
                             (get arg-map lhs lhs)
                             lhs)))
               (if value
                   (let ((rr (if (or (atom? rhs) (ssavalue? rhs) (eq? (car rhs) 'null))
                                 rhs (make-ssavalue))))
                     (if (not (eq? rr rhs))
                         (emit `(= ,rr ,rhs)))
                     (emit `(= ,lhs ,rr))
                     (if tail (emit-return rr))
                     rr)
                   (emit `(= ,lhs ,rhs)))))
            ((block body)
             (let* ((last-fname filename)
                    (fnm        (first-non-meta e))
                    (fname      (if (and (length> e 1) (pair? fnm) (eq? (car fnm) 'line)
                                         (length> fnm 2))
                                    (caddr fnm)
                                    filename))
                    (file-diff  (not (eq? fname last-fname)))
                    ;; don't need a filename node for start of function
                    (need-meta  (and file-diff
                                     (not (eq? e (lam:body lam))))))
               (if file-diff (set! filename fname))
               (if need-meta (emit `(meta push_loc ,fname)))
               (begin0
                (let loop ((xs (cdr e)))
                  (if (null? (cdr xs))
                      (compile (car xs) break-labels value tail)
                      (begin (compile (car xs) break-labels #f #f)
                             (loop (cdr xs)))))
                (if need-meta
                    (if (or (not tail)
                            (and (pair? (car code))
                                 (or (eq? (cdar code) 'meta)
                                     (eq? (cdar code) 'line))))
                        (emit '(meta pop_loc))
                        ;; If we need to return the last non-meta expression
                        ;; splice the pop before the result
                        (let ((retv (car code))
                              (body (cdr code)))
                          (set! code body)
                          (if (complex-return? retv)
                              (let ((tmp (make-ssavalue)))
                                (emit `(= ,tmp ,(cadr retv)))
                                (emit '(meta pop_loc))
                                (emit `(return ,tmp)))
                              (begin
                                (emit '(meta pop_loc))
                                (emit retv))))))
                (if file-diff (set! filename last-fname)))))
            ((return)
             (compile (cadr e) break-labels #t #t)
             '(null))
            ((unnecessary)
             ;; `unnecessary` marks expressions generated by lowering that
             ;; do not need to be evaluated if their value is unused.
             (if value
                 (compile (cadr e) break-labels value tail)
                 #f))
            ((if)
             (let ((test `(gotoifnot ,(compile-cond (cadr e) break-labels) _))
                   (end-jump `(goto _))
                   (val (if (and value (not tail)) (new-mutable-var) #f)))
               (emit test)
               (let ((v1 (compile (caddr e) break-labels value tail)))
                 (if val (emit `(= ,val ,v1)))
                 (if (and (not tail) (or (length> e 3) val))
                     (emit end-jump))
                 (set-car! (cddr test) (make&mark-label))
                 (let ((v2 (if (length> e 3)
                               (compile (cadddr e) break-labels value tail)
                               '(null))))
                   (if val (emit `(= ,val ,v2)))
                   (if (not tail)
                       (set-car! (cdr end-jump) (make&mark-label))
                       (if (length= e 3)
                           (emit-return v2)))
                   val))))
            ((_while)
             (let* ((endl (make-label))
                    (topl (make&mark-label))
                    (test (compile-cond (cadr e) break-labels)))
               (emit `(gotoifnot ,test ,endl))
               (compile (caddr e) break-labels #f #f)
               (emit `(goto ,topl))
               (mark-label endl)))
            ((break-block)
             (let ((endl (make-label)))
               (begin0 (compile (caddr e)
                                (cons (list (cadr e) endl handler-level)
                                      break-labels)
                                value #f)
                       (mark-label endl)))
             (if value (compile '(null) break-labels value tail)))
            ((break)
             (let ((labl (assq (cadr e) break-labels)))
               (if (not labl)
                   (error "break or continue outside loop")
                   (begin
                     (if (> handler-level (caddr labl))
                         (emit `(leave ,(- handler-level (caddr labl)))))
                     (emit `(goto ,(cadr labl)))))))
            ((label symboliclabel)
             (if (eq? (car e) 'symboliclabel)
                 (if (has? label-level (cadr e))
                     (error (string "label \"" (cadr e) "\" defined multiple times"))
                     (put! label-level (cadr e) handler-level)))
             (let ((m (get label-map (cadr e) #f)))
               (if m
                   (emit `(label ,m))
                   (put! label-map (cadr e) (make&mark-label)))
               (if tail
                   (emit-return '(null))
                   (if value (error "misplaced label")))))
            ((symbolicgoto)
             (let* ((m (get label-map (cadr e) #f))
                    (m (or m (let ((l (make-label)))
                               (put! label-map (cadr e) l)
                               l))))
               (emit `(null))  ;; save space for `leave` that might be needed
               (emit `(goto ,m))
               (set! handler-goto-fixups
                     (cons (list code handler-level (cadr e)) handler-goto-fixups))
               '(null)))

            ;; exception handlers are lowered using
            ;; (enter L) - push handler with catch block at label L
            ;; (leave n) - pop N exception handlers
            ((trycatch)
             (let ((catch (make-label))
                   (endl  (make-label)))
               (emit `(enter ,catch))
               (set! handler-level (+ handler-level 1))
               (let* ((v1  (compile (cadr e)
                                    break-labels value #f))
                      (val (if (and value (not tail))
                               (new-mutable-var) #f)))
                 (if val (emit `(= ,val ,v1)))
                 (if tail
                     (begin (emit-return v1)
                            (set! endl #f))
                     (begin (emit '(leave 1))
                            (emit `(goto ,endl))))
                 (set! handler-level (- handler-level 1))
                 (mark-label catch)
                 (emit `(leave 1))
                 (let ((v2 (compile (caddr e) break-labels value tail)))
                   (if val (emit `(= ,val ,v2)))
                   (if endl (mark-label endl))
                   val))))

            ((method)
             (if (length> e 2)
                 (begin (emit `(method ,(or (cadr e) 'false)
                                       ,(compile (caddr e) break-labels #t #f)
                                       ,(linearize (cadddr e))
                                       ,(if (car (cddddr e)) 'true 'false)))
                        (if value (compile '(null) break-labels value tail)))
                 (cond (tail  (emit-return e))
                       (value e)
                       (else  (emit e)))))
            ((lambda)
             (let ((temp (linearize e)))
               (if tail
                   (emit-return temp)
                   (emit temp))))

            ((&)
             (assert (and value (not tail)))
             `(& ,(compile (cadr e) break-labels value tail)))

            ((newvar)
             ;; avoid duplicate newvar nodes
             (if (not (and (pair? code) (equal? (car code) e)))
                 (emit e)
                 #f))
            ((global) ; remove global declarations
             (if value (error "misplaced \"global\" declaration"))
             (let ((vname (cadr e)))
               (if (var-info-for vname vi)
                   ;; issue #7264
                   (error (string "`global " vname "`: " vname " is local variable in the enclosing scope"))
                   #f)))
            ((local-def) #f)
            ((local) #f)
            ((implicit-global) #f)
            ((const) (emit e))

            ;; top level expressions returning values
            ((abstract_type bits_type composite_type thunk toplevel module)
             (case (car e)
               ((abstract_type)
                (let* ((para (compile (caddr e) break-labels #t #f))
                       (supe (compile (cadddr e) break-labels #t #f)))
                  (emit `(abstract_type ,(cadr e) ,para ,supe))))
               ((bits_type)
                (let* ((para (compile (caddr e) break-labels #t #f))
                       (supe (compile (list-ref e 4) break-labels #t #f)))
                  (emit `(bits_type ,(cadr e) ,para ,(cadddr e) ,supe))))
               ((composite_type)
                (let* ((para (compile (caddr e) break-labels #t #f))
                       (supe (compile (list-ref e 4) break-labels #t #f))
                       (ftys (compile (list-ref e 5) break-labels #t #f)))
                  (emit `(composite_type ,(cadr e) ,para ,(cadddr e) ,supe ,ftys ,@(list-tail e 6)))))
               (else
                (emit e)))
             (if tail (emit-return '(null)))
             '(null))

            ;; other top level expressions and metadata
            ((import importall using export line meta inbounds boundscheck simdloop)
             (let ((have-ret? (and (pair? code) (pair? (car code)) (eq? (caar code) 'return))))
               (cond ((eq? (car e) 'line)
                      (if first-line
                          (begin (set! first-line #f)
                                 (emit e))
                          ;; strip filenames out of non-initial line nodes
                          (emit `(line ,(cadr e)))))
                     ((and (eq? (car e) 'meta) (length> e 2) (eq? (cadr e) 'ret-type))
                      (assert (or (not value) tail))
                      (assert (not rett))
                      (set! rett (caddr e)))
                     (else
                      (emit e)))
               (if (and tail (not have-ret?))
                   (emit-return '(null)))
               '(null)))
            ((...)
             (error "\"...\" expression outside call"))
            (else
             (error (string "unhandled expr " e))))))
    ;; introduce new slots for assigned arguments
    (for-each (lambda (v)
                (if (vinfo:asgn v)
                    (begin
                      (if (not arg-map)
                          (set! arg-map (table)))
                      (put! arg-map (car v) (new-mutable-var (car v))))))
              (list-head vi (length (lam:args lam))))
    (compile e '() #t #t)
    (for-each (lambda (x)
                (let ((point (car x))
                      (hl    (cadr x))
                      (lab   (caddr x)))
                  (let ((target-level (get label-level lab #f)))
                    (cond ((not target-level)
                           (error (string "label \"" lab "\" referenced but not defined")))
                          ((> target-level hl)
                           (error (string "cannot goto label \"" lab "\" inside try/catch block")))
                          ((= target-level hl)
                           (set-cdr! point (cddr point))) ;; remove empty slot
                          (else
                           (set-car! (cdr point) `(leave ,(- hl target-level))))))))
              handler-goto-fixups)
    (let* ((stmts (reverse! code))
           (di    (definitely-initialized-vars stmts vi))
           (body  (cons 'body (filter (lambda (e)
                                        (not (and (pair? e) (eq? (car e) 'newvar)
                                                  (has? di (cadr e)))))
                                      stmts))))
      (if arg-map
          (insert-after-meta
           body
           (table.foldl (lambda (k v lst) (cons `(= ,v ,k) lst))
                        '() arg-map))
          body))))

;; find newvar nodes that are unnecessary because (1) the variable is not
;; captured, and (2) the variable is assigned before any branches.
;; this is used to remove newvar nodes that are not needed for re-initializing
;; variables to undefined (see issue #11065). it doesn't look for variable
;; *uses*, because any variables used-before-def that also pass this test
;; are *always* used undefined, and therefore don't need to be *re*-initialized.
(define (definitely-initialized-vars stmts vi)
  (let ((vars (table))
        (di   (table)))
    (let loop ((stmts stmts))
      (if (null? stmts)
          di
          (begin
            (let ((e (car stmts)))
              (cond ((and (pair? e) (eq? (car e) 'newvar))
                     (let ((vinf (var-info-for (cadr e) vi)))
                       (if (and vinf (not (vinfo:capt vinf)))
                           (put! vars (cadr e) #t))))
                    ((and (pair? e) (eq? (car e) '=))
                     (if (has? vars (cadr e))
                         (begin (del! vars (cadr e))
                                (put! di (cadr e) #t))))
                    ((and (pair? e) (memq (car e) '(goto gotoifnot)))
                     (set! vars (table)))))
            (loop (cdr stmts)))))))

;; pass 6: renumber slots and labels

(define (listify-lambda lam)
  ;; insert `list` expression heads to make the lambda vinfo lists valid expressions
  (let ((vi (lam:vinfo lam)))
    `(lambda (list ,@(cadr lam))
       (list (list ,@(map (lambda (l) (cons 'list l))
                          (car vi)))
             (list ,@(cadr vi)) ,(caddr vi) (list ,@(cadddr vi)))
       ,@(cdddr lam))))

(define (label-to-idx-map body)
  (let ((tbl (table)))
    (let loop ((stmts (cdr body))
               (prev  (cdr body))
               (i 1))
      (if (pair? stmts)
          (let* ((el  (car stmts))
                 (nxt (cdr stmts))
                 ;; is next statement also a label?
                 (nl  (and (pair? nxt) (pair? (car nxt)) (eq? (caar nxt) 'label))))
            (if (and (pair? el) (eq? (car el) 'label))
                (begin (put! tbl (cadr el) i)
                       (loop nxt
                             (if nl prev nxt)
                             (if nl ;; merge adjacent labels
                                 (begin (set-cdr! prev (cdr nxt))
                                        i)
                                 (+ i 1))))
                (loop nxt nxt (+ i 1))))))
    tbl))

(define (renumber-labels! lam label2idx)
  (let loop ((stmts (cdr (lam:body lam))))
    (if (pair? stmts)
        (let ((el (car stmts)))
          (if (pair? el)
              (case (car el)
                ((label goto enter) (set-car! (cdr el) (get label2idx (cadr el))))
                ((gotoifnot)        (set-car! (cddr el) (get label2idx (caddr el))))
                (else #f)))
          (loop (cdr stmts))))))

(define (symbol-to-idx-map lst)
  (let ((tbl (table)))
    (let loop ((xs lst) (i 1))
      (if (pair? xs)
          (begin (put! tbl (car xs) i)
                 (loop (cdr xs) (+ i 1)))))
    tbl))

(define (renumber-lambda lam)
  (renumber-labels! lam (label-to-idx-map (lam:body lam)))
  (define nssavalues 0)
  (define ssavalue-table (table))
  (define nslots (length (car (lam:vinfo lam))))
  (define slot-table (symbol-to-idx-map (map car (car (lam:vinfo lam)))))
  (define sp-table (symbol-to-idx-map (lam:sp lam)))
  (define (renumber-slots e)
    (cond ((symbol? e)
           (let ((idx (get slot-table e #f)))
             (if idx `(slot ,idx) e)))
          ((and (pair? e) (eq? (car e) 'outerref))
           (let ((idx (get sp-table (cadr e) #f)))
                (if idx `(static_parameter ,idx) (cadr e))))
          ((or (atom? e) (quoted? e)) e)
          ((ssavalue? e)
           (let ((idx (or (get ssavalue-table (cadr e) #f)
                          (begin0 nssavalues
                                  (put! ssavalue-table (cadr e) nssavalues)
                                  (set! nssavalues (+ nssavalues 1))))))
             `(ssavalue ,idx)))
          ((eq? (car e) 'lambda)
           (renumber-lambda e))
          (else (cons (car e)
                      (map renumber-slots (cdr e))))))
  (let ((body (renumber-slots (lam:body lam)))
        (vi   (lam:vinfo lam)))
    (listify-lambda
     `(lambda ,(cadr lam)
        (,(car vi) ,(cadr vi) ,nssavalues ,(last vi))
        ,body))))

(define (renumber-slots-and-labels ex)
  (if (atom? ex) ex
      (if (eq? (car ex) 'lambda)
          (renumber-lambda ex)
          (cons (car ex)
                (map renumber-slots-and-labels (cdr ex))))))

;; expander entry point

(define (julia-expand1 ex)
  (renumber-slots-and-labels
   (linearize
    (closure-convert
     (analyze-variables!
      (resolve-scopes ex))))))

(define julia-expand0 expand-forms)

(define (julia-expand ex)
  (julia-expand1
   (julia-expand0
    (julia-expand-macros ex))))
