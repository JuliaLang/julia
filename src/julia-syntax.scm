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
      (error "invalid \"...\" on non-final argument"))
  (map (lambda (a)
         (cond ((and (pair? a) (eq? (car a) 'kw))
                `(kw ,(fill-missing-argname (cadr a) unused) ,(caddr a)))
               ((and (pair? a) (eq? (car a) '...))
                `(... ,(fill-missing-argname (cadr a) unused)))
               (else
                (fill-missing-argname a unused))))
       l))

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
    (if (and (not (dotop-named? (cadr e)))
             (length> e 5)
             (pair? (cadddr (cdr e)))
             (dotop-named? (cadddr (cddr e))))
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
              (lambda (x) (or (not (length> x 2)) (dotop-named? (cadr x))))
              compare-one))

;; convert a series of scalar and vector comparisons into & calls,
;; combining as many scalar comparisons as possible into short-circuit
;; && sequences.
(define (expand-vector-compare e)
  (comp-accum e
              (lambda (a b) `(call .& ,a ,b))
              (lambda (x) (not (length> x 2)))
              (lambda (e)
                (if (dotop-named? (cadr e))
                    (compare-one e)
                    (expand-scalar-compare e)))))

(define (expand-compare-chain e)
  (car (expand-vector-compare e)))

;; return the appropriate computation for a `begin` or `end` symbol for indexing
;; the array `a` in the `n`th index.
;; `tuples` are a list of the splatted arguments that precede index `n`
;; `last` = is this last index?
;; returns a call to lastindex(a) or lastindex(a,n)
(define (end-val a n tuples last)
  (if (null? tuples)
      (if (and last (= n 1))
          `(call (top lastindex) ,a)
          `(call (top lastindex) ,a ,n))
      (let ((dimno `(call (top +) ,(- n (length tuples))
                          ,.(map (lambda (t) `(call (top length) ,t))
                                 tuples))))
            `(call (top lastindex) ,a ,dimno))))

(define (begin-val a n tuples last)
  (if (null? tuples)
      (if (and last (= n 1))
          `(call (top firstindex) ,a)
          `(call (top first) (call (top axes) ,a ,n)))
      (let ((dimno `(call (top +) ,(- n (length tuples))
                          ,.(map (lambda (t) `(call (top length) ,t))
                                 tuples))))
            `(call (top first) (call (top axes) ,a ,dimno)))))

;; replace `begin` and `end` for the closest ref expression, so doesn't go inside nested refs
(define (replace-beginend ex a n tuples last)
  (cond ((eq? ex 'end)                (end-val a n tuples last))
        ((eq? ex 'begin)              (begin-val a n tuples last))
        ((or (atom? ex) (quoted? ex)) ex)
        ((eq? (car ex) 'ref)
         ;; inside ref only replace within the first argument
         (list* 'ref (replace-beginend (cadr ex) a n tuples last)
                (cddr ex)))
        (else
         (cons (car ex)
               (map (lambda (x) (replace-beginend x a n tuples last))
                    (cdr ex))))))

;; go through indices and replace the `begin` or `end` symbol
;; a = array being indexed, i = list of indices
;; returns (values index-list stmts) where stmts are statements that need
;; to execute first.
(define (process-indices a i)
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
                        (cons `(... ,(replace-beginend (cadr idx) a n tuples last))
                              ret))
                  (let ((g (make-ssavalue)))
                    (loop (cdr lst) (+ n 1)
                          (cons `(= ,g ,(replace-beginend (cadr idx) a n tuples last))
                                stmts)
                          (cons g tuples)
                          (cons `(... ,g) ret))))
              (loop (cdr lst) (+ n 1)
                    stmts tuples
                    (cons (replace-beginend idx a n tuples last) ret)))))))

;; GF method does not need to keep decl expressions on lambda args
;; except for rest arg
(define (method-lambda-expr argl body rett)
  (let ((argl (map (lambda (x)
                     (let ((n (arg-name x)))
                       (if (underscore-symbol? n) UNUSED n)))
                   argl))
        (body (blockify body)))
    `(lambda ,argl ()
             (scope-block
              ,(if (equal? rett '(core Any))
                   body
                   (let ((meta (take-while (lambda (x) (and (pair? x)
                                                            (memq (car x) '(line meta))))
                                           (cdr body)))
                         (R (make-ssavalue)))
                     `(,(car body) ,@meta
                       (= ,R ,rett)
                       (meta ret-type ,R)
                       ,@(list-tail body (+ 1 (length meta))))))))))

;; convert x<:T<:y etc. exprs into (name lower-bound upper-bound)
;; a bound is #f if not specified
(define (analyze-typevar e)
  (define (check-sym s)
    (if (symbol? s)
        s
        (error (string "invalid type parameter name \"" (deparse s) "\""))))
  (cond ((atom? e) (list (check-sym e) #f #f))
        ((eq? (car e) 'var-bounds)  (cdr e))
        ((and (eq? (car e) 'comparison) (length= e 6))
         (cons (check-sym (cadddr e))
               (cond ((and (eq? (caddr e) '|<:|) (eq? (caddr (cddr e)) '|<:|))
                      (list (cadr e) (last e)))
                     (else (error "invalid bounds in \"where\"")))))
        ((eq? (car e) '|<:|)
         (list (check-sym (cadr e)) #f (caddr e)))
        ((eq? (car e) '|>:|)
         (list (check-sym (cadr e)) (caddr e) #f))
        (else (error "invalid variable expression in \"where\""))))

(define (sparam-name-bounds params)
  (let ((bounds (map analyze-typevar params)))
    (values (map car bounds) bounds)))

(define (unmangled-name v)
  (if (eq? v '||)
      v
      (let ((s (string v)))
        (if (eqv? (string.char s 0) #\#)
            (symbol (last (string-split s "#")))
            v))))

;; construct expression to allocate a TypeVar
(define (bounds-to-TypeVar v (unmangle #f))
  (let ((v  ((if unmangle unmangled-name identity) (car v)))
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
  (let ((type-ex (caddr m)))
    (if (eq? (car type-ex) 'block)
        ;; extract ssavalue labels of sparams from the svec-of-sparams argument to `method`
        (let ((sp-ssavals (cddr (cadddr (last type-ex)))))
          (map (lambda (a)  ;; extract T from (= v (call (core TypeVar) (quote T) ...))
                 (cadr (caddr (caddr a))))
               (filter (lambda (e)
                         (and (pair? e) (eq? (car e) '=) (member (cadr e) sp-ssavals)))
                       (cdr type-ex))))
        '())))

;; expressions of the form a.b.c... where everything is a symbol
(define (sym-ref? e)
  (or (symbol? e)
      (and (length= e 3) (eq? (car e) 'globalref))
      (and (length= e 2) (eq? (car e) 'outerref))
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

(define (replace-vars e renames)
  (cond ((symbol? e)      (lookup e renames e))
        ((or (not (pair? e)) (quoted? e))  e)
        ((memq (car e) '(-> function scope-block)) e)
        (else
         (cons (car e)
               (map (lambda (x) (replace-vars x renames))
                    (cdr e))))))

(define (make-generator-function name sp-names arg-names body)
  (let ((arg-names (append sp-names
                           (map (lambda (n)
                                  (if (eq? n '|#self#|) (gensy) n))
                                arg-names))))
    (let ((body (insert-after-meta body  ;; don't specialize on generator arguments
                                   `((meta nospecialize ,@arg-names)))))
      `(block
        (global ,name)
        (function (call ,name ,@arg-names) ,body)))))

;; select the `then` or `else` part of `if @generated` based on flag `genpart`
(define (generated-part- x genpart)
  (cond ((or (atom? x) (quoted? x) (function-def? x)) x)
        ((if-generated? x)
         (if genpart `($ ,(caddr x)) (cadddr x)))
        (else (cons (car x)
                    (map (lambda (e) (generated-part- e genpart)) (cdr x))))))

(define (generated-version body)
  `(block
    ,(julia-bq-macro (generated-part- body #t))))

(define (non-generated-version body)
  (generated-part- body #f))

;; Remove and return the line number for the start of the function definition
(define (maybe-remove-functionloc! body)
  (let* ((prologue (extract-method-prologue body))
         (prologue-lnos (filter linenum? prologue))
         (functionloc (if (pair? prologue-lnos)
                          (car prologue-lnos)
                          ; Fallback - take first line anywhere in body
                          (let ((lnos (filter linenum? body)))
                            (if (null? lnos) '(line 0 none) (car lnos))))))
    (if (length> prologue-lnos 1)
        ; First of two line numbers in prologue is function definition location
        ; which should be removed from the body.
        (let loop ((stmts body))
          (if (eq? functionloc (cadr stmts))
              (set-cdr! stmts (cddr stmts))
              (loop (cdr stmts)))))
    functionloc))

;; construct the (method ...) expression for one primitive method definition,
;; assuming optional and keyword args are already handled
(define (method-def-expr- name sparams argl body (rett '(core Any)))
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
         (optional-positional-defs name sparams req opt dfl body
                                   (append req opt vararg) rett)))))
   ;; no optional positional args
   (let* ((names (map car sparams))
          (anames (map (lambda (x) (if (underscore-symbol? x) UNUSED x)) (llist-vars argl)))
          (unused_anames (filter (lambda (x) (not (eq? x UNUSED))) anames)))
     (if (has-dups unused_anames)
         (error (string "function argument name not unique: \"" (car (has-dups unused_anames)) "\"")))
     (if (has-dups names)
         (error "function static parameter names not unique"))
     (if (any (lambda (x) (and (not (eq? x UNUSED)) (memq x names))) anames)
         (error "function argument and static parameter names must be distinct"))
     (if (or (and name (not (sym-ref? name))) (not (valid-name? name)))
         (error (string "invalid function name \"" (deparse name) "\"")))
     (let* ((loc (maybe-remove-functionloc! body))
            (generator (if (expr-contains-p if-generated? body (lambda (x) (not (function-def? x))))
                           (let* ((gen    (generated-version body))
                                  (nongen (non-generated-version body))
                                  (gname  (symbol (string (gensy) "#" (current-julia-module-counter))))
                                  (gf     (make-generator-function gname names anames gen)))
                             (set! body (insert-after-meta
                                         nongen
                                         `((meta generated
                                                 (new (core GeneratedFunctionStub)
                                                      ,gname
                                                      ,(cons 'list anames)
                                                      ,(if (null? sparams)
                                                           'nothing
                                                           (cons 'list (map car sparams)))
                                                      ,(cadr loc)
                                                      (inert ,(caddr loc))
                                                      (false))))))
                             (list gf))
                           '()))
            (types (llist-types argl))
            (body  (method-lambda-expr argl body rett))
            ;; HACK: the typevars need to be bound to ssavalues, since this code
            ;; might be moved to a different scope by closure-convert.
            (temps (map (lambda (x) (make-ssavalue)) names))
            (renames (map cons names temps))
            (mdef
             (if (null? sparams)
                 `(method ,name
                          (call (core svec)
                                (call (core svec) ,@(dots->vararg types))
                                (call (core svec))
                                (inert ,loc))
                          ,body)
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
                                 (call (core svec) ,@temps)
                                 (inert ,loc)))
                          ,body))))
       (if (or (symbol? name) (globalref? name))
           `(block ,@generator (method ,name) ,mdef (unnecessary ,name))  ;; return the function
           (if (not (null? generator))
               `(block ,@generator ,mdef)
               mdef))))))

;; wrap expr in nested scopes assigning names to vals
(define (scopenest names vals expr)
  (if (null? names)
      expr
      `(let (= ,(car names) ,(car vals))
         (block
          ,(scopenest (cdr names) (cdr vals) expr)))))

(define (keywords-method-def-expr name sparams argl body rett)
  (let* ((kargl (cdar argl))  ;; keyword expressions (= k v)
         (annotations (map (lambda (a) `(meta ,(cadr a) ,(arg-name (cadr (caddr a)))))
                           (filter nospecialize-meta? kargl)))
         (kargl (map (lambda (a)
                       (if (nospecialize-meta? a) (caddr a) a))
                     kargl))
         (pargl (cdr argl))   ;; positional args
         (body  (blockify body))
         (ftype (decl-type (car pargl)))
         ;; 1-element list of vararg argument, or empty if none
         (vararg (let ((l (if (null? pargl) '() (last pargl))))
                   (if (or (vararg? l) (varargexpr? l))
                       (list l) '())))
         ;; positional args with vararg
         (pargl-all pargl)
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
         (kargl (let ((kws (if (null? restkw) kargl (butlast kargl))))
                  (if (any vararg? kws)
                      (error "invalid \"...\" on non-final keyword argument"))
                  kws))
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
         (positional-sparams (filter-sparams (cons 'list pargl-all) sparams))
         (keyword-sparams
          (filter (lambda (s)
                    (not (any (lambda (p) (eq? (car p) (car s)))
                              positional-sparams)))
                  sparams)))
    (let ((kw      (gensy))
          (rkw     (if (null? restkw) (make-ssavalue) (symbol (string (car restkw) "..."))))
          (mangled (let ((und (and name (undot-name name))))
                     (symbol (string (if (and name (= (string.char (string name) 0) #\#))
                                         ""
                                         "#")
                                     (or und '_) "#"
                                     (string (current-julia-module-counter)))))))
      ;; this is a hack: nest these statements inside a call so they get closure
      ;; converted together, allowing all needed types to be defined before any methods.
      `(call (core ifelse) (false) (false) (block
        ;; forward-declare function so its type can occur in the signature of the inner method below
        ,@(if (or (symbol? name) (globalref? name)) `((method ,name)) '())

        ;; call with keyword args pre-sorted - original method code goes here
        ,(method-def-expr-
          mangled sparams
          `((|::| ,mangled (call (core typeof) ,mangled)) ,@vars ,@restkw
            ;; strip type off function self argument if not needed for a static param.
            ;; then it is ok for cl-convert to move this definition above the original def.
            ,@not-optional ,@vararg)
          (insert-after-meta `(block
                               ,@stmts)
                             (cons `(meta nkw ,(+ (length vars) (length restkw)))
                                   annotations))
          rett)

        ;; call with no keyword args
        ,(method-def-expr-
          name positional-sparams pargl-all
          `(block
            ,@(without-generated prologue)
            ,(let (;; call mangled(vals..., [rest_kw,] pargs..., [vararg]...)
                   (ret `(return (call ,mangled
                                       ,@(if ordered-defaults keynames vals)
                                       ,@(if (null? restkw) '() `((call (top pairs) (call (core NamedTuple)))))
                                       ,@(map arg-name pargl)
                                       ,@(if (null? vararg) '()
                                             (list `(... ,(arg-name (car vararg)))))))))
               (if ordered-defaults
                   (scopenest keynames vals ret)
                   ret))))

        ;; call with unsorted keyword args. this sorts and re-dispatches.
        ,(method-def-expr-
          name positional-sparams
          `((|::|
             ;; if there are optional positional args, we need to be able to reference the function name
             ,(if (any kwarg? pargl) (gensy) UNUSED)
             (call (core kwftype) ,ftype)) ,kw ,@pargl ,@vararg)
          `(block
            ,@(filter linenum? prologue)
            ;; nospecialize meta for just positional args
            ,@(map (lambda (m)
                     `(meta ,(cadr m) ,@(filter (lambda (v) (not (memq v keynames)))
                                                (cddr m))))
                   (filter nospecialize-meta? prologue))
            ,(scopenest
              keynames
              (map (lambda (v dflt)
                     (let* ((k     (decl-var v))
                            (rval0 `(call (top getindex) ,kw (inert ,k)))
                            ;; note: if the "declared" type of a KW arg includes something
                            ;; from keyword-sparams then don't assert it here, since those
                            ;; static parameters don't have values yet. instead, the type
                            ;; will be picked up when the underlying method is called.
                            (rval (if (and (decl? v)
                                           (not (any (lambda (s)
                                                       (expr-contains-eq (car s) (caddr v)))
                                                     keyword-sparams)))
                                      (let ((T    (caddr v))
                                            (temp (make-ssavalue)))
                                        `(block (= ,temp ,rval0)
                                                (if (call (core isa) ,temp ,T)
                                                    (null)
                                                    (call (core throw)
                                                          (new (core TypeError)
                                                               (inert |keyword argument|)
                                                               (inert ,k)
                                                               ,T
                                                               ,temp)))
                                                ,temp))
                                      rval0)))
                       `(if (call (top haskey) ,kw (quote ,k))
                            ,rval
                            ,dflt)))
                   vars vals)
              `(block
                (= ,rkw (call (top pairs)
                              ,(if (null? keynames)
                                   kw
                                   `(call (top structdiff) ,kw (curly (core NamedTuple)
                                                                      (tuple ,@(map quotify keynames)))))))
                ,@(if (null? restkw)
                      `((if (call (top isempty) ,rkw)
                            (null)
                            (call (top kwerr) ,kw ,@(map arg-name pargl)
                                  ,@(if (null? vararg) '()
                                        (list `(... ,(arg-name (car vararg))))))))
                      '())
                (return (call ,mangled  ;; finally, call the core function
                              ,@keynames
                              ,@(if (null? restkw) '() (list rkw))
                              ,@(map arg-name pargl)
                              ,@(if (null? vararg) '()
                                    (list `(... ,(arg-name (car vararg)))))))))))
        ;; return primary function
        ,(if (not (symbol? name))
             '(null) name))))))

;; prologue includes line number node and eventual meta nodes
(define (extract-method-prologue body)
  (if (pair? body)
      (take-while (lambda (e)
                    (and (pair? e) (or (eq? (car e) 'line) (eq? (car e) 'meta))))
                  (cdr body))
      '()))

(define (without-generated stmts)
  (filter (lambda (x) (not (or (generated-meta? x)
                               (generated_only-meta? x))))
          stmts))

;; keep only sparams used by `expr` or other sparams
(define (filter-sparams expr sparams)
  (let loop ((filtered '())
             (params   sparams))
    (cond ((null? params)
           (reverse! filtered))
          ((or (expr-contains-eq (caar params) expr)
               (any (lambda (v) (expr-contains-eq (caar params) v))
                    (cdr params)))
           (loop (cons (car params) filtered) (cdr params)))
          (else
           (loop filtered (cdr params))))))

(define (optional-positional-defs name sparams req opt dfl body overall-argl rett)
  (let ((prologue (without-generated (extract-method-prologue body))))
    `(block
      ,@(map (lambda (n)
               (let* ((passed (append req (list-head opt n)))
                      ;; only keep static parameters used by these arguments
                      (sp     (filter-sparams (cons 'list passed) sparams))
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
                 (method-def-expr- name sp passed body)))
             (iota (length opt)))
      ,(method-def-expr- name sparams overall-argl body rett))))

;; strip empty (parameters ...), normalizing `f(x;)` to `f(x)`.
(define (remove-empty-parameters argl)
  (if (and (has-parameters? argl) (null? (cdar argl)))
      (cdr argl)
      argl))

(define (check-kw-args kw)
  (let ((invalid (filter (lambda (x) (not (or (kwarg? x) (vararg? x)
                                              (and (nospecialize-meta? x)
                                                   (or (kwarg? (caddr x)) (vararg? (caddr x)))))))
                         kw)))
    (if (pair? invalid)
        (if (and (pair? (car invalid)) (eq? 'parameters (caar invalid)))
            (error "more than one semicolon in argument list")
            (error (string "invalid keyword argument syntax \""
                           (deparse (car invalid)) "\""))))))

; replace unassigned kw args with assignment to throw() call (forcing the caller to assign the keyword)
(define (throw-unassigned-kw-args argl)
  (define (throw-unassigned argname)
    `(call (core throw) (call (core UndefKeywordError) (inert ,argname))))
  (define (to-kw x)
    (cond ((symbol? x) `(kw ,x ,(throw-unassigned x)))
          ((decl? x) `(kw ,x ,(throw-unassigned (cadr x))))
          ((nospecialize-meta? x) `(meta ,(cadr x) ,(to-kw (caddr x))))
          (else x)))
  (if (has-parameters? argl)
      (cons (cons 'parameters
                  (map to-kw (cdar argl)))
            (cdr argl))
      argl))

;; method-def-expr checks for keyword arguments, and if there are any, calls
;; keywords-method-def-expr to expand the definition into several method
;; definitions that do not use keyword arguments.
;; definitions without keyword arguments are passed to method-def-expr-,
;; which handles optional positional arguments by adding the needed small
;; boilerplate definitions.
(define (method-def-expr name sparams argl body rett)
  (let ((argl (throw-unassigned-kw-args (remove-empty-parameters argl))))
    (if (has-parameters? argl)
        ;; has keywords
        (begin (check-kw-args (cdar argl))
               (keywords-method-def-expr name sparams argl body rett))
        ;; no keywords
        (method-def-expr- name sparams argl body rett))))

(define (struct-def-expr name params super fields mut)
  (receive
   (params bounds) (sparam-name-bounds params)
   (struct-def-expr- name params bounds super (flatten-blocks fields) mut)))

;; replace field names with gensyms if they conflict with field-types
(define (safe-field-names field-names field-types)
  (if (any (lambda (v) (contains (lambda (e) (eq? e v)) field-types))
           field-names)
      (map (lambda (x) (gensy)) field-names)
      ;; use a different name for a field called `_`
      (map (lambda (x) (if (eq? x '_) (gensy) x)) field-names)))

(define (with-wheres call wheres)
  (if (pair? wheres)
      `(where ,call ,@wheres)
      call))

(define (default-inner-ctors name field-names field-types params bounds locs)
  (let* ((field-names (safe-field-names field-names field-types))
         (any-ctor
          ;; definition with Any for all arguments
          `(function ,(with-wheres
                       `(call ,(if (pair? params)
                                   `(curly ,name ,@params)
                                   name)
                              ,@field-names)
                       (map (lambda (b) (cons 'var-bounds b)) bounds))
                     (block
                      ,@locs
                      (call new ,@field-names)))))
    (if (and (null? params) (any (lambda (t) (not (equal? t '(core Any))))
                                 field-types))
        (list
         ;; definition with field types for all arguments
         ;; only if any field type is not Any, checked at runtime
         `(if ,(foldl (lambda (t u)
                        `(&& ,u (call (core ===) (core Any) ,t)))
                      `(call (core ===) (core Any) ,(car field-types))
                      (cdr field-types))
            (block)
            (function (call ,name
                            ,@(map make-decl field-names field-types))
                      (block
                       ,@locs
                       (new (outerref ,name) ,@field-names))))
         any-ctor)
        (list any-ctor))))

(define (default-outer-ctor name field-names field-types params bounds locs)
  (let ((field-names (safe-field-names field-names field-types)))
    `(function ,(with-wheres
                 `(call ,name ,@(map make-decl field-names field-types))
                 (map (lambda (b) (cons 'var-bounds b)) bounds))
               (block
                ,@locs
                (call (curly ,name ,@params) ,@field-names)))))

(define (new-call Tname type-params sparams params args field-names field-types)
  (if (any kwarg? args)
      (error "\"new\" does not accept keyword arguments"))
  (if (length> params (length type-params))
      (error "too few type parameters specified in \"new{...}\""))
  (if (length> type-params (length params))
      (error "too many type parameters specified in \"new{...}\""))
  (let* ((Texpr (if (null? type-params)
                    `(outerref ,Tname)
                    `(curly (outerref ,Tname)
                            ,@type-params)))
         (tn (make-ssavalue))
         (field-convert (lambda (fld fty val)
                          (if (equal? fty '(core Any))
                              val
                              `(call (top convert)
                                     ,(if (and (equal? type-params params) (memq fty params) (memq fty sparams))
                                          fty ; the field type is a simple parameter, the usage here is of a
                                              ; local variable (currently just handles sparam) for the bijection of params to type-params
                                          `(call (core fieldtype) ,tn ,(+ fld 1)))
                                     ,val)))))
    (cond ((length> (filter (lambda (a) (not (vararg? a))) args) (length field-names))
           `(call (core throw) (call (top ArgumentError)
                                     ,(string "new: too many arguments (expected " (length field-names) ")"))))
          ((any vararg? args)
           (if (every (lambda (ty) (equal? ty '(core Any)))
                      field-types)
               `(splatnew ,Texpr (call (core tuple) ,@args))
               (let ((argt (make-ssavalue))
                     (nf (make-ssavalue)))
                 `(block
                   (= ,tn ,Texpr)
                   (= ,argt (call (core tuple) ,@args))
                   (= ,nf (call (core nfields) ,argt))
                   (if (call (top ult_int) ,nf ,(length field-names))
                       (call (core throw) (call (top ArgumentError)
                                                ,(string "new: too few arguments (expected " (length field-names) ")"))))
                   (if (call (top ult_int) ,(length field-names) ,nf)
                       (call (core throw) (call (top ArgumentError)
                                                ,(string "new: too many arguments (expected " (length field-names) ")"))))
                   (new ,tn ,@(map (lambda (fld fty) (field-convert fld fty `(call (core getfield) ,argt ,(+ fld 1) (false))))
                                   (iota (length field-names)) (list-head field-types (length field-names))))))))
          (else
            `(block
              (= ,tn ,Texpr)
              (new ,tn ,@(map field-convert (iota (length args)) (list-head field-types (length args)) args)))))))

;; insert item at start of arglist
(define (arglist-unshift sig item)
  (if (and (pair? sig) (pair? (car sig)) (eq? (caar sig) 'parameters))
      `(,(car sig) ,item ,@(cdr sig))
      `(,item ,@sig)))

(define (linenode-string lno)
  (cond ((length= lno 2) (string " around line " (cadr lno)))
        ((length= lno 3) (string " around " (caddr lno) ":" (cadr lno)))
        (else "")))

(define (ctor-def name Tname ctor-body sig body wheres)
  (let* ((curly?     (and (pair? name) (eq? (car name) 'curly)))
         (curlyargs  (if curly? (cddr name) '()))
         (name       (if curly? (cadr name) name))
         (sparams (map car (map analyze-typevar wheres))))
    (cond ((not (eq? name Tname))
           `(function ,(with-wheres `(call ,(if curly?
                                                `(curly ,name ,@curlyargs)
                                                name)
                                           ,@sig)
                                    wheres)
                      ;; pass '() in order to require user-specified parameters with
                      ;; new{...} inside a non-ctor inner definition.
                      ,(ctor-body body '() sparams)))
          (else
           `(function ,(with-wheres `(call ,(if curly?
                                                `(curly ,name ,@curlyargs)
                                                name)
                                           ,@sig)
                                    wheres)
                      ,(ctor-body body curlyargs sparams))))))

;; rewrite calls to `new( ... )` to `new` expressions on the appropriate
;; type, determined by the containing constructor definition.
(define (rewrite-ctor ctor Tname params field-names field-types)
  (define (ctor-body body type-params sparams)
    (pattern-replace (pattern-set
                      (pattern-lambda
                       (call (-/ new) . args)
                       (new-call Tname type-params sparams params
                                 (map (lambda (a) (ctor-body a type-params sparams)) args)
                                 field-names field-types))
                      (pattern-lambda
                       (call (curly (-/ new) . p) . args)
                       (new-call Tname p sparams params
                                 (map (lambda (a) (ctor-body a type-params sparams)) args)
                                 field-names field-types)))
                     body))
  (pattern-replace
   (pattern-set
    ;; definitions without `where`
    (pattern-lambda (function       (-$ (call name . sig) (|::| (call name . sig) _t)) body)
                    (ctor-def name Tname ctor-body sig body #f))
    (pattern-lambda (= (-$ (call name . sig) (|::| (call name . sig) _t)) body)
                    (ctor-def name Tname ctor-body sig body #f))
    ;; definitions with `where`
    (pattern-lambda (function       (where (-$ (call name . sig) (|::| (call name . sig) _t)) . wheres) body)
                    (ctor-def name Tname ctor-body sig body wheres))
    (pattern-lambda (= (where (-$ (call name . sig) (|::| (call name . sig) _t)) . wheres) body)
                    (ctor-def name Tname ctor-body sig body wheres)))

   ;; flatten `where`s first
   (pattern-replace
    (pattern-set
     (pattern-lambda (where (where . rest1) . rest2)
                     (flatten-where-expr __)))
    ctor)))

;; check if there are any calls to new with fewer than n arguments
(define (ctors-min-initialized expr)
  (and (pair? expr)
       (min
        ((pattern-lambda (call (-/ new) . args)
                         (length args))
         (car expr))
        ((pattern-lambda (call (curly (-/ new) . p) . args)
                         (length args))
         (car expr))
        (ctors-min-initialized (car expr))
        (ctors-min-initialized (cdr expr)))))

(define (struct-def-expr- name params bounds super fields0 mut)
  (receive
   (fields defs) (separate (lambda (x) (or (symbol? x) (decl? x)))
                           fields0)
   (let* ((defs        (filter (lambda (x) (not (effect-free? x))) defs))
          (locs        (if (and (pair? fields0) (linenum? (car fields0)))
                           (list (car fields0))
                           '()))
          (field-names (map decl-var fields))
          (field-types (map decl-type fields))
          (defs2 (if (null? defs)
                     (default-inner-ctors name field-names field-types params bounds locs)
                     defs))
          (min-initialized (min (ctors-min-initialized defs) (length fields)))
          (prev (make-ssavalue)))
     (let ((dups (has-dups field-names)))
       (if dups (error (string "duplicate field name: \"" (car dups) "\" is not unique"))))
     (for-each (lambda (v)
                 (if (not (symbol? v))
                     (error (string "field name \"" (deparse v) "\" is not a symbol"))))
               field-names)
     `(block
       (global ,name) (const ,name)
       (scope-block
        (block
         (local-def ,name)
         ,@(map (lambda (v) `(local ,v)) params)
         ,@(map (lambda (n v) (make-assignment n (bounds-to-TypeVar v #t))) params bounds)
         (toplevel-only struct (outerref ,name))
         (= ,name (call (core _structtype) (thismodule) (inert ,name) (call (core svec) ,@params)
                        (call (core svec) ,@(map quotify field-names))
                        ,mut ,min-initialized))
         (call (core _setsuper!) ,name ,super)
         (if (isdefined (outerref ,name))
             (block
              (= ,prev (outerref ,name))
              (if (call (core _equiv_typedef) ,prev ,name)
                  ;; if this is compatible with an old definition, use the existing type object
                  ;; and its parameters
                  (block (= ,name ,prev)
                         ,@(if (pair? params)
                               `((= (tuple ,@params) (|.|
                                                      ,(foldl (lambda (_ x) `(|.| ,x (quote body)))
                                                              prev
                                                              params)
                                                      (quote parameters))))
                               '()))
                  ;; otherwise do an assignment to trigger an error
                  (= (outerref ,name) ,name)))
             (= (outerref ,name) ,name))
         (call (core _typebody!) ,name (call (core svec) ,@field-types))
         (null)))
       ;; "inner" constructors
       (scope-block
        (block
         (global ,name)
         ,@(map (lambda (c)
                  (rewrite-ctor c name params field-names field-types))
                defs2)))
       ;; "outer" constructors
       ,@(if (and (null? defs)
                  (not (null? params))
                  ;; To generate an outer constructor, each parameter must occur in a field
                  ;; type, or in the bounds of a subsequent parameter.
                  ;; Otherwise the constructor would not work, since the parameter values
                  ;; would never be specified.
                  (let loop ((root-types field-types)
                             (sp         (reverse bounds)))
                    (or (null? sp)
                        (let ((p (car sp)))
                          (and (expr-contains-eq (car p) (cons 'list root-types))
                               (loop (append (cdr p) root-types)
                                     (cdr sp)))))))
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
       (local-def ,name)
       ,@(map (lambda (v) `(local ,v)) params)
       ,@(map (lambda (n v) (make-assignment n (bounds-to-TypeVar v #t))) params bounds)
       (toplevel-only abstract_type)
       (= ,name (call (core _abstracttype) (thismodule) (inert ,name) (call (core svec) ,@params)))
       (call (core _setsuper!) ,name ,super)
       (call (core _typebody!) ,name)
       (if (&& (isdefined (outerref ,name))
               (call (core _equiv_typedef) (outerref ,name) ,name))
           (null)
           (= (outerref ,name) ,name))
       (null))))))

(define (primitive-type-def-expr n name params super)
  (receive
   (params bounds) (sparam-name-bounds params)
   `(block
     (global ,name) (const ,name)
     (scope-block
      (block
       (local-def ,name)
       ,@(map (lambda (v) `(local ,v)) params)
       ,@(map (lambda (n v) (make-assignment n (bounds-to-TypeVar v #t))) params bounds)
       (toplevel-only primitive_type)
       (= ,name (call (core _primitivetype) (thismodule) (inert ,name) (call (core svec) ,@params) ,n))
       (call (core _setsuper!) ,name ,super)
       (call (core _typebody!) ,name)
       (if (&& (isdefined (outerref ,name))
               (call (core _equiv_typedef) (outerref ,name) ,name))
           (null)
           (= (outerref ,name) ,name))
       (null))))))

;; take apart a type signature, e.g. T{X} <: S{Y}
(define (analyze-type-sig ex)
  (or ((pattern-lambda (-- name (-s))
                       (values name '() '(core Any))) ex)
      ((pattern-lambda (curly (-- name (-s)) . params)
                       (values name params '(core Any))) ex)
      ((pattern-lambda (|<:| (-- name (-s)) super)
                       (values name '() super)) ex)
      ((pattern-lambda (|<:| (curly (-- name (-s)) . params) super)
                       (values name params super)) ex)
      (error "invalid type signature")))

;; insert calls to convert() in ccall, and pull out expressions that might
;; need to be rooted before conversion.
(define (lower-ccall name RT atypes args cconv)
  (let loop ((F atypes)  ;; formals
             (A args)    ;; actuals
             (stmts '()) ;; initializers
             (T '())     ;; argument types (F converted)
             (C '())     ;; argument values (A converted)
             (GC '()))   ;; GC roots
    (if (and (null? F) (not (null? A)))
        (error "more arguments than types for ccall"))
    (if (and (null? A) (not (or (null? F) (and (pair? F) (vararg? (car F)) (null? (cdr F))))))
        (error "more types than arguments for ccall"))
    (let ((isseq (and (not (null? F)) (vararg? (car F)))))
      (if (and isseq (null? T))
          (error "C ABI prohibits vararg without one required argument"))
      (if (null? A)
          `(block
            ,.(reverse! stmts)
            (foreigncall ,name ,RT (call (core svec) ,@(reverse! T))
                         ,(if isseq (- (length atypes) 1) 0) ; 0 or number of arguments before ... in definition
                         ',cconv
                         ,.(reverse! C)
                         ,@GC)) ; GC root ordering is arbitrary
          (let* ((a     (car A))
                 (ty    (if isseq (cadar F) (car F))))
            (if (and isseq (not (null? (cdr F)))) (error "only the trailing ccall argument type should have \"...\""))
            (if (eq? ty 'Any)
                (loop (if isseq F (cdr F)) (cdr A) stmts (list* '(core Any) T) (list* a C) GC)
                (let* ((g (make-ssavalue))
                       (stmts (cons `(= ,g (call (top cconvert) ,ty ,a)) stmts))
                       (ca `(call (top unsafe_convert) ,ty ,g)))
                  (loop (if isseq F (cdr F)) (cdr A) stmts
                        (list* ty T) (list* ca C) (list* g GC)))))))))

(define (expand-function-def e)   ;; handle function definitions
  (define (just-arglist? ex)
    (and (pair? ex)
         (or (memq (car ex) '(tuple block ...))
             (and (eq? (car ex) 'where)
                  (just-arglist? (cadr ex))))))
  (let ((name (cadr e)))
    (if (just-arglist? name)
        (expand-forms (cons '-> (cdr e)))
        (expand-function-def- e))))

;; convert (where (where x S) T) to (where x T S)
(define (flatten-where-expr e)
  (let loop ((ex e)
             (vars '()))
    (if (and (pair? ex) (eq? (car ex) 'where))
        (loop (cadr ex) (append! (reverse (cddr ex)) vars))
        `(where ,ex ,.(reverse! vars)))))

(define (lower-destructuring-args argl)
  (define (check-lhs a)
    (if (expr-contains-p (lambda (e) (or (decl? e) (assignment? e) (kwarg? e)))
                         a)
        (error (string "invalid argument destructuring syntax \"" (deparse a) "\""))
        a))
  (define (transform-arg a)
    (cond ((and (pair? a) (eq? (car a) 'tuple))
           (let ((a2 (gensy)))
             (cons a2 `(local (= ,(check-lhs a) ,a2)))))
          ((or (and (decl? a) (length= a 3)) (kwarg? a))
           (let ((x (transform-arg (cadr a))))
             (cons `(,(car a) ,(car x) ,(caddr a)) (cdr x))))
          ((vararg? a)
           (let ((x (transform-arg (cadr a))))
             (cons `(... ,(car x)) (cdr x))))
          (else (cons a #f))))
  (let loop ((argl  argl)
             (newa  '())
             (stmts '()))
    (if (null? argl)
        (cons (reverse newa) (if (null? stmts)
                                 stmts
                                 ;; return `nothing` from the assignments (issue #26518)
                                 (reverse (cons '(null) stmts))))
        (let ((a (transform-arg (car argl))))
          (loop (cdr argl) (cons (car a) newa)
                (if (cdr a) (cons (cdr a) stmts) stmts))))))

(define (expand-function-def- e)
  (let* ((name  (cadr e))
         (where (if (and (pair? name) (eq? (car name) 'where))
                    (let ((w (flatten-where-expr name)))
                      (begin0 (cddr w)
                              (if (not (and (pair? (cadr w)) (memq (caadr w) '(call |::|))))
                                  (error (string "invalid assignment location \"" (deparse name) "\"")))
                              (set! name (cadr w))))
                    #f))
         (dcl   (and (pair? name) (eq? (car name) '|::|)))
         (rett  (if dcl (caddr name) '(core Any)))
         (name  (if dcl (cadr name) name)))
    (cond ((and (length= e 2) (or (symbol? name) (globalref? name)))
           (if (not (valid-name? name))
               (error (string "invalid function name \"" name "\"")))
           `(method ,name))
          ((not (pair? name))  e)
          ((eq? (car name) 'call)
           (let* ((head    (cadr name))
                  (argl    (cddr name))
                  (name    (check-dotop head))
                  (annotations (map (lambda (a) `(meta ,(cadr a) ,(arg-name (caddr a))))
                                    (filter nospecialize-meta? argl)))
                  (body (insert-after-meta (caddr e) annotations))
                  (argl (map (lambda (a)
                               (if (nospecialize-meta? a) (caddr a) a))
                             argl))
                  (raw-typevars (or where '()))
                  (sparams (map analyze-typevar raw-typevars))
                  (adj-decl (lambda (n) (if (and (decl? n) (length= n 2))
                                            `(|::| |#self#| ,(cadr n))
                                            n)))
                  ;; fill in first (closure) argument
                  (farg    (if (decl? name)
                               (adj-decl name)
                               `(|::| |#self#| (call (core Typeof) ,name))))
                  (argl-stmts (lower-destructuring-args argl))
                  (argl       (car argl-stmts))
                  (body       (insert-after-meta body (cdr argl-stmts)))
                  (argl    (fix-arglist
                            (arglist-unshift argl farg)
                            (and (not (any kwarg? argl)) (not (and (pair? argl)
                                                                   (pair? (car argl))
                                                                   (eq? (caar argl) 'parameters))))))
                  (name    (if (or (decl? name) (and (pair? name) (memq (car name) '(curly where))))
                               #f name)))
             (expand-forms
              (method-def-expr name sparams argl body rett))))
          (else
           (error (string "invalid assignment location \"" (deparse name) "\""))))))

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
                   (tuple-to-arglist (filter (lambda (x) (not (linenum? x))) a))
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

(define (let-binds e)
  (if (and (pair? (cadr e))
           (eq? (car (cadr e)) 'block))
      (cdr (cadr e))
      (list (cadr e))))

(define (expand-let e (hard? #t))
  (let ((ex    (caddr e))
        (binds (let-binds e))
        (hs    (if hard? '((hardscope)) '())))
    (expand-forms
     (if
      (null? binds)
      `(scope-block (block ,@hs ,ex))
      (let loop ((binds (reverse binds))
                 (blk   ex))
        (if (null? binds)
            blk
            (cond
             ((or (symbol? (car binds)) (decl? (car binds)))
              ;; just symbol -> add local
              (loop (cdr binds)
                    `(scope-block
                      (block ,@hs
                       (local ,(car binds))
                       ,blk))))
             ((and (length= (car binds) 3)
                   (eq? (caar binds) '=))
              ;; some kind of assignment
              (cond
               ((eventually-call? (cadar binds))
                ;; f() = c
                (let ((name (assigned-name (cadar binds))))
                  (if (not (symbol? name))
                      (error "invalid let syntax"))
                  (loop (cdr binds)
                        `(scope-block
                          (block ,@hs
                           ,(if (expr-contains-eq name (caddar binds))
                                `(local ,name) ;; might need a Box for recursive functions
                                `(local-def ,name))
                           ,(car binds)
                           ,blk)))))
               ((or (symbol? (cadar binds))
                    (decl?   (cadar binds)))
                (let ((vname (decl-var (cadar binds))))
                  (loop (cdr binds)
                        (if (expr-contains-eq vname (caddar binds))
                            (let ((tmp (make-ssavalue)))
                              `(scope-block
                                (block ,@hs
                                       (= ,tmp ,(caddar binds))
                                       (scope-block
                                        (block
                                         (local-def ,(cadar binds))
                                         (= ,vname ,tmp)
                                         ,blk)))))
                            `(scope-block
                              (block ,@hs
                               (local-def ,(cadar binds))
                               (= ,vname ,(caddar binds))
                               ,blk))))))
               ;; (a, b, c, ...) = rhs
               ((and (pair? (cadar binds))
                     (eq? (caadar binds) 'tuple))
                (let ((vars (lhs-vars (cadar binds))))
                  (loop (cdr binds)
                        (if (expr-contains-p (lambda (x) (memq x vars)) (caddr (car binds)))
                            ;; use more careful lowering if there are name conflicts. issue #25652
                            (let ((temp (make-ssavalue)))
                              `(block
                                (= ,temp ,(caddr (car binds)))
                                (scope-block
                                 (block ,@hs
                                  ,@(map (lambda (v) `(local-def ,v)) vars)
                                  (= ,(cadr (car binds)) ,temp)
                                  ,blk))))
                            `(scope-block
                              (block ,@hs
                               ,@(map (lambda (v) `(local-def ,v)) vars)
                               ,(car binds)
                               ,blk))))))
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
                             (|::| __source__ (core LineNumberNode))
                             (|::| __module__ (core Module))
                             ,@(map (lambda (v)
                                      (if (symbol? v)
                                          `(meta nospecialize ,v)
                                          v))
                                    anames))
                       ,@(cddr e)))))
        ((and (length= e 2) (symbol? (cadr e)))
         (expand-forms `(function ,(symbol (string #\@ (cadr e))))))
        (else
         (error "invalid macro definition"))))

(define (expand-struct-def e)
  (let ((mut (cadr e))
        (sig (caddr e))
        (fields (cdr (cadddr e))))
    (let loop ((f fields))
      (if (null? f)
          '()
          (let ((x (car f)))
            (cond ((or (symbol? x) (decl? x) (linenum? x))
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

(define (expand-try e)
  (let ((tryb   (cadr e))
        (var    (caddr e))
        (catchb (cadddr e)))
    (cond ((length= e 5)
           (if (has-unmatched-symbolic-goto? tryb)
               (error "goto from a try/finally block is not permitted"))
           (let ((finalb (cadddr (cdr e))))
             (expand-forms
              `(tryfinally
                ,(if (not (equal? catchb '(false)))
                     `(try ,tryb ,var ,catchb)
                     `(scope-block ,tryb))
                (scope-block ,finalb)))))
          ((length= e 4)
           (expand-forms
            (if (symbol-like? var)
                `(trycatch (scope-block ,tryb)
                           (scope-block
                            (block (= ,var (the_exception))
                                   ,catchb)))
                `(trycatch (scope-block ,tryb)
                           (scope-block ,catchb)))))
          (else
           (error "invalid \"try\" form")))))

(define (expand-unionall-def name type-ex)
  (if (and (pair? name)
           (eq? (car name) 'curly))
      (let ((name   (cadr name))
            (params (cddr name)))
        (if (null? params)
            (error (string "empty type parameter list in \"" (deparse `(= (curly ,name) ,type-ex)) "\"")))
        `(block
          (const-if-global ,name)
          ,(expand-forms
            `(= ,name (where ,type-ex ,@params)))))
      (expand-forms
       `(const (= ,name ,type-ex)))))

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

;; given a complex assignment LHS, return the symbol that will ultimately be assigned to
(define (assigned-name e)
  (cond ((atom? e) e)
        ((or (memq (car e) '(call curly where))
             (and (eq? (car e) '|::|) (eventually-call? e)))
         (assigned-name (cadr e)))
        (else e)))

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
          (cond ((and (symbol-like? L)
                      (or (not (pair? R)) (quoted? R) (equal? R '(null)))
                      ;; overwrite var immediately if it doesn't occur elsewhere
                      (not (contains (lambda (e) (eq-sym? e L)) (cdr rhss)))
                      (not (contains (lambda (e) (eq-sym? e R)) assigned)))
                 (loop (cdr lhss)
                       (cons L assigned)
                       (cdr rhss)
                       (cons (make-assignment L R) stmts)
                       after
                       (cons R elts)))
                ((vararg? R)
                 (let ((temp (make-ssavalue)))
                   `(block ,@(reverse stmts)
                           ,(make-assignment temp (cadr R))
                           ,@(reverse after)
                           (= (tuple ,@lhss) ,temp)
                           (unnecessary (tuple ,@(reverse elts) (... ,temp))))))
                (else
                 (let ((temp (if (eventually-call? L) (gensy) (make-ssavalue))))
                   (loop (cdr lhss)
                         (cons L assigned)
                         (cdr rhss)
                         (cons (make-assignment temp R) stmts)
                         (cons (make-assignment L temp) after)
                         (cons temp elts)))))))))

;; convert (lhss...) = x to tuple indexing
(define (lower-tuple-assignment lhss x)
  (let ((t (make-ssavalue)))
    `(block
      (= ,t ,x)
      ,@(let loop ((lhs lhss)
                   (i   1))
          (if (null? lhs) '()
              (cons (if (eventually-call? (car lhs))
                        ;; if this is a function assignment, avoid putting our ssavalue
                        ;; inside the function and instead create a capture-able variable.
                        ;; issue #22032
                        (let ((temp (gensy)))
                          `(block
                            (= ,temp (call (core getfield) ,t ,i))
                            (= ,(car lhs) ,temp)))
                        `(= ,(car lhs)
                            (call (core getfield) ,t ,i)))
                    (loop (cdr lhs)
                          (+ i 1)))))
      ,t)))

;; make an expression safe for multiple evaluation
;; for example a[f(x)] => (temp=f(x); a[temp])
;; returns a pair (expr . assignments)
;; where 'assignments' is a list of needed assignment statements
(define (remove-argument-side-effects e (tup #f))
  (if (not (pair? e))
      (cons e '())
      (let ((a '()))
        (define (arg-to-temp x)
          (cond ((effect-free? x)  x)
                ((or (eq? (car x) '...) (eq? (car x) '&))
                 `(,(car x) ,(arg-to-temp (cadr x))))
                ((or (eq? (car x) 'kw) (and tup (eq? (car x) '=)))
                 `(,(car x) ,(cadr x) ,(arg-to-temp (caddr x))))
                ((eq? (car x) 'parameters)
                 `(parameters ,@(map arg-to-temp (cdr x))))
                ((eq? (car x) 'tuple)
                 (let ((tmp (remove-argument-side-effects x #t)))
                   (set! a (revappend (cdr tmp) a))
                   (car tmp)))
                (else
                 (let ((g (make-ssavalue)))
                   (begin (set! a (cons `(= ,g ,x) a))
                          g)))))
        (cons (cons (car e) (map arg-to-temp (cdr e)))
              (reverse a)))))

(define (lower-kw-call f args)
  (let* ((para (if (has-parameters? args) (cdar args) '()))
         (args (if (has-parameters? args) (cdr args) args)))
    (let* ((parg-stmts (remove-argument-side-effects `(call ,f ,@args)))
           (call-ex    (car parg-stmts))
           (fexpr      (cadr call-ex))
           (cargs      (cddr call-ex)))
      `(block
        ,.(cdr parg-stmts)
        ,(receive
          (kws pargs) (separate kwarg? cargs)
          (lower-kw-call- fexpr (append! kws para) pargs))))))

(define (lower-kw-call- fexpr kw pa)
  (define (kwcall-unless-empty f pa kw-container-test kw-container)
    `(if (call (top isempty) ,kw-container-test)
         (call ,f ,@pa)
         (call (call (core kwfunc) ,f) ,kw-container ,f ,@pa)))

  (let ((f            (if (sym-ref? fexpr) fexpr (make-ssavalue)))
        (kw-container (make-ssavalue)))
    `(block
      ,@(if (eq? f fexpr) '() `((= ,f, fexpr)))
      (= ,kw-container ,(lower-named-tuple kw
                                           (lambda (name) (string "keyword argument \"" name
                                                                  "\" repeated in call to \"" (deparse fexpr) "\""))
                                           "keyword argument"
                                           "keyword argument syntax"))
      ,(if (every vararg? kw)
           (kwcall-unless-empty f pa kw-container kw-container)
           `(call (call (core kwfunc) ,f) ,kw-container ,f ,@pa)))))

;; convert `a+=b` to `a=a+b`
(define (expand-update-operator- op op= lhs rhs declT)
  (let* ((e      (remove-argument-side-effects lhs))
         (newlhs (car e))
         (temp   (and (eq? op= '|.=|) (pair? newlhs) (not (eq? (car newlhs) 'ref))
                      (make-ssavalue)))
         (e      (if temp
                     (cons temp (append (cdr e) (list `(= ,temp ,newlhs))))
                     e))
         (newlhs (or temp newlhs)))
    (if (and (pair? lhs) (eq? (car lhs) 'tuple))
        (let loop ((a (cdr newlhs))
                   (b (cdr lhs)))
          (if (pair? a)
              ;; if remove-argument-side-effects needed to replace an expression with
              ;; an ssavalue, then it can't be updated by assignment. issue #30062
              (begin (if (and (ssavalue? (car a)) (not (ssavalue? (car b))))
                         (error (string "invalid multiple assignment location \"" (deparse (car b)) "\"")))
                     (loop (cdr a) (cdr b))))))
    `(block ,@(cdr e)
            ,(if (null? declT)
                 `(,op= ,newlhs (call ,op ,newlhs ,rhs))
                 `(,op= ,newlhs (call ,op (:: ,newlhs ,(car declT)) ,rhs))))))

(define (partially-expand-ref e)
  (let ((a    (cadr e))
        (idxs (cddr e)))
    (let* ((reuse (and (pair? a)
                       (contains (lambda (x) (or (eq? x 'begin) (eq? x 'end)))
                                 idxs)))
           (arr   (if reuse (make-ssavalue) a))
           (stmts (if reuse `((= ,arr ,a)) '())))
      (receive
       (new-idxs stuff) (process-indices arr idxs)
       `(block
         ,@(append stmts stuff)
         (call (top getindex) ,arr ,@new-idxs))))))

(define (expand-update-operator op op= lhs rhs . declT)
  (cond ((and (pair? lhs) (eq? (car lhs) 'ref))
         ;; expand indexing inside op= first, to remove "begin", "end", and ":"
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
         (if (and (pair? lhs) (eq? op= '=)
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
          '(true)
          (if (null? (cdr tail))
              (car tail)
              `(if ,(car tail)
                   ,(loop (cdr tail))
                   (false)))))))

(define (expand-or e)
  (let ((e (cdr (flatten-ex '|\|\|| e))))
    (let loop ((tail e))
      (if (null? tail)
          '(false)
          (if (null? (cdr tail))
              (car tail)
              (if (symbol-like? (car tail))
                  `(if ,(car tail) ,(car tail)
                       ,(loop (cdr tail)))
                  (let ((g (make-ssavalue)))
                    `(block (= ,g ,(car tail))
                            (if ,g ,g
                                ,(loop (cdr tail)))))))))))

(define (expand-for lhss itrs body)
  (define (outer? x) (and (pair? x) (eq? (car x) 'outer)))
  (let ((copied-vars  ;; variables not declared `outer` are copied in the innermost loop
         ;; TODO: maybe filter these to remove vars not assigned in the loop
         (delete-duplicates
          (filter (lambda (x) (not (underscore-symbol? x)))
                  (apply append
                         (map lhs-vars
                              (filter (lambda (x) (not (outer? x))) (butlast lhss))))))))
    `(break-block
      loop-exit
      ,(let nest ((lhss lhss)
                  (itrs itrs))
         (if (null? lhss)
             body
             (let* ((coll  (make-ssavalue))
                    (next  (gensy))
                    (state (make-ssavalue))
                    (outer (outer? (car lhss)))
                    (lhs   (if outer (cadar lhss) (car lhss)))
                    (body
                     `(block
                       ,@(if (not outer)
                             (map (lambda (v) `(local ,v)) (lhs-vars lhs))
                             '())
                       ,(lower-tuple-assignment (list lhs state) next)
                       ,(nest (cdr lhss) (cdr itrs))))
                    (body
                     (if (null? (cdr lhss))
                         `(break-block
                           loop-cont
                           (soft-let (block ,@(map (lambda (v) `(= ,v ,v)) copied-vars))
                             ,body))
                         `(scope-block ,body))))
               `(block (= ,coll ,(car itrs))
                       (= ,next (call (top iterate) ,coll))
                       ;; TODO avoid `local declared twice` error from this
                       ;;,@(if outer `((local ,lhs)) '())
                       ,@(if outer `((require-existing-local ,lhs)) '())
                       (if (call (top not_int) (call (core ===) ,next (null)))
                           (_do_while
			    (block ,body
				   (= ,next (call (top iterate) ,coll ,state)))
			    (call (top not_int) (call (core ===) ,next (null))))))))))))

;; wrap `expr` in a function appropriate for consuming values from given ranges
(define (func-for-generator-ranges expr range-exprs flat outervars)
  (let* ((vars    (map cadr range-exprs))
         (argname (if (and (length= vars 1) (symbol? (car vars)))
                      (car vars)
                      (gensy)))
         (myvars  (lhs-vars `(tuple ,@vars)))
         (splat (cond ((eq? argname (car vars))  '())
                      ((length= vars 1)
                       `(,@(map (lambda (v) `(local ,v)) myvars)
                         (= ,(car vars) ,argname)))
                      (else
                       `(,@(map (lambda (v) `(local ,v)) myvars)
                         (= (tuple ,@vars) ,argname))))))
    (cond
     ((eq? expr argname)
      ;; use `identity` for x->x
      `(top identity))
     ((and (null? splat)
           (length= expr 3) (eq? (car expr) 'call)
           (eq? (caddr expr) argname)
           (not (dotop-named? (cadr expr)))
           (not (expr-contains-eq argname (cadr expr))))
      ;; eta reduce `x->f(x)` => `f`
      (cadr expr))
     (else
      (let ((expr (cond ((and flat (pair? expr) (eq? (car expr) 'generator))
                         (expand-generator expr #f (delete-duplicates (append outervars myvars))))
                        ((and flat (pair? expr) (eq? (car expr) 'flatten))
                         (expand-generator (cadr expr) #t (delete-duplicates (append outervars myvars))))
                        ((pair? outervars)
                         `(let (block ,@(map (lambda (v) `(= ,v ,v)) (filter (lambda (x) (not (underscore-symbol? x)))
                                                                             outervars)))
                            ,expr))
                        (else expr))))
        `(-> ,argname (block ,@splat ,expr)))))))

(define (expand-generator e flat outervars)
  (let* ((expr  (cadr e))
         (filt? (eq? (car (caddr e)) 'filter))
         (range-exprs (if filt? (cddr (caddr e)) (cddr e)))
         (ranges (map caddr range-exprs))
         (iter (if (length= ranges 1)
                   (car ranges)
                   `(call (top product) ,@ranges)))
         (iter (if filt?
                   `(call (top Filter)
                          ,(func-for-generator-ranges (cadr (caddr e)) range-exprs #f '())
                          ,iter)
                   iter))
         (gen  `(call (top Generator)
                      ,(func-for-generator-ranges expr range-exprs flat outervars)
                      ,iter)))
    (expand-forms
     (if flat
         `(call (top Flatten) ,gen)
         gen))))

(define (ref-to-view expr)
  (if (and (pair? expr) (eq? (car expr) 'ref))
      (let* ((ex (partially-expand-ref expr))
             (stmts (butlast (cdr ex)))
             (refex (last    (cdr ex)))
             (nuref `(call (top dotview) ,(caddr refex) ,@(cdddr refex))))
        `(block ,@stmts ,nuref))
      expr))

; lazily fuse nested calls to expr == f.(args...) into a single broadcast call,
; or a broadcast! call if lhs is non-null.
(define (expand-fuse-broadcast lhs rhs)
  (define (fuse? e) (and (pair? e) (eq? (car e) 'fuse)))
  (define (dot-to-fuse e (top #f)) ; convert e == (. f (tuple args)) to (fuse f args)
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
      (let* ((kws+args (split-kwargs args)) ; fusing occurs on positional args only
             (kws (car kws+args))
             (kws (if (null? kws) kws (list (cons 'parameters kws))))
             (args (map dot-to-fuse (cdr kws+args)))
             (make `(call (top ,(if (null? kws) 'broadcasted 'broadcasted_kwsyntax)) ,@kws ,f ,@args)))
        (if top (cons 'fuse make) make)))
    (if (and (pair? e) (eq? (car e) '|.|))
        (let ((f (cadr e)) (x (caddr e)))
          (cond ((or (atom? x) (eq? (car x) 'quote) (eq? (car x) 'inert) (eq? (car x) '$))
                 `(call (top getproperty) ,f ,x))
                ((eq? (car x) 'tuple)
                 (if (and (eq? (identifier-name f) '^) (length= x 3) (integer? (caddr x)))
                     (make-fuse '(top literal_pow)
                                (list f (cadr x) (expand-forms `(call (call (core apply_type) (top Val) ,(caddr x))))))
                     (make-fuse f (cdr x))))
                (else
                 (error (string "invalid syntax \"" (deparse e) "\"")))))
        (if (and (pair? e) (eq? (car e) 'call) (dotop-named? (cadr e)))
            (let ((f (undotop (cadr e))) (x (cddr e)))
              (if (and (eq? (identifier-name f) '^) (length= x 2) (integer? (cadr x)))
                  (make-fuse '(top literal_pow)
                             (list f (car x) (expand-forms `(call (call (core apply_type) (top Val) ,(cadr x))))))
                  (make-fuse f x)))
            e)))
  (let ((e (dot-to-fuse rhs #t)) ; an expression '(fuse func args) if expr is a dot call
        (lhs-view (ref-to-view lhs))) ; x[...] expressions on lhs turn in to view(x, ...) to update x in-place
    (if (fuse? e)
        ; expanded to a fuse op call
        (if (null? lhs)
            (expand-forms `(call (top materialize) ,(cdr e)))
            (expand-forms `(call (top materialize!) ,lhs-view ,(cdr e))))
        ; expanded to something else (like a getfield)
        (if (null? lhs)
            (expand-forms e)
            (expand-forms `(call (top materialize!) ,lhs-view
                                 (call (top broadcasted) (top identity) ,e)))))))


(define (expand-where body var)
  (let* ((bounds (analyze-typevar var))
         (v  (car bounds)))
    `(let (= ,v ,(bounds-to-TypeVar bounds))
       (call (core UnionAll) ,v ,body))))

(define (expand-wheres body vars)
  (if (null? vars)
      body
      (expand-where (expand-wheres body (cdr vars)) (car vars))))

; given e = (curly T params...), return (newparams . whereparams) where any <:X expression
; in params is converted to T and T<:X is added to whereparams; similarly for >:X.
; (This implements the syntactic sugar Foo{<:Bar} --> Foo{T} where T<:Bar.)
(define (extract-implicit-whereparams e)
  (define (extract params newparams whereparams)
    (if (null? params)
        (cons (reverse newparams) (reverse whereparams))
        (let ((p (car params)))
          (if (and (length= p 2) (or (eq? (car p) '|<:|) (eq? (car p) '|>:|)))
              (let ((T (gensy)))
                (extract (cdr params) (cons T newparams) (cons (list (car p) T (cadr p)) whereparams)))
              (extract (cdr params) (cons p newparams) whereparams)))))
  (extract (cddr e) '() '()))

(define (named-tuple-expr names values)
  `(call (curly (core NamedTuple) (tuple ,@names))
         ;; NOTE: don't use `tuple` head, so an assignment expression as a value
         ;; doesn't turn this into another named tuple.
         (call (core tuple) ,@values)))

(define (lower-named-tuple lst
                           (dup-error-fn (lambda (name) (string "field name \"" name "\" repeated in named tuple")))
                           (name-str     "named tuple field")
                           (syntax-str   "named tuple element"))
  (let* ((names (apply append
                       (map (lambda (x)
                              (cond ((symbol? x) (list x))
                                    ((and (or (assignment? x) (kwarg? x)) (symbol? (cadr x)))
                                     (list (cadr x)))
                                    ((and (length= x 3) (eq? (car x) '|.|))
                                     (list (cadr (caddr x))))
                                    (else '())))
                            lst)))
         (dups (has-dups names)))
    (if dups
        (error (dup-error-fn (car dups)))))
  (define (not-vararg x)
    (if (vararg? x)
        (error (string "\"...\" expression cannot be used as " name-str " value"))
        x))
  (define (to-nt n v)
    (if (null? n)
        #f
        (named-tuple-expr (reverse! (map quotify n)) (reverse v))))
  (define (merge old new)
    (if old
        (if new
            `(call (top merge) ,old ,new)
            old)
        new))
  (let loop ((L             lst)
             (current-names '())
             (current-vals  '())
             (expr          #f))
    (if (null? L)
        (or (merge expr (to-nt current-names current-vals))
            ;; if that result is #f the named tuple is empty
            '(call (core NamedTuple)))
        (let ((el (car L)))
          (cond ((or (assignment? el) (kwarg? el))
                 (if (not (symbol? (cadr el)))
                     (error (string "invalid " name-str " name \"" (deparse (cadr el)) "\"")))
                 (not-vararg (caddr el))
                 (loop (cdr L)
                       (cons (cadr el) current-names)
                       (cons (caddr el) current-vals)
                       expr))
                ((symbol? el)  ;; x  =>  x = x
                 (loop (cdr L)
                       (cons el current-names)
                       (cons el current-vals)
                       expr))
                ((and (length= el 3) (eq? (car el) '|.|)   ;; a.x  =>  x = a.x
                      (quoted-sym? (caddr el)))
                 (loop (cdr L)
                       (cons (cadr (caddr el)) current-names)
                       (cons el current-vals)
                       expr))
                ((and (length= el 4) (eq? (car el) 'call) (eq? (cadr el) '=>))
                 (loop (cdr L)
                       '()
                       '()
                       (merge (merge expr (to-nt current-names current-vals))
                              (named-tuple-expr (list (caddr el)) (list (not-vararg (cadddr el)))))))
                ((vararg? el)
                 (loop (cdr L)
                       '()
                       '()
                       (let ((current (merge expr (to-nt current-names current-vals))))
                         (if current
                             (merge current (cadr el))
                             `(call (top merge) (call (top NamedTuple)) ,(cadr el))))))
                (else
                 (error (string "invalid " syntax-str " \"" (deparse el) "\""))))))))

;; move an assignment into the last statement of a block to keep more statements at top level
(define (sink-assignment lhs rhs)
  (if (and (pair? rhs) (eq? (car rhs) 'block))
      (let ((rr (reverse (cdr rhs))))
        `(block ,@(reverse (cdr rr))
                (= ,lhs ,(car rr))))
      `(= ,lhs ,rhs)))

(define (expand-forms e)
  (if (or (atom? e) (memq (car e) '(quote inert top core globalref outerref module toplevel ssavalue null true false meta using import export thismodule toplevel-only)))
      e
      (let ((ex (get expand-table (car e) #f)))
        (if ex
            (ex e)
            (cons (car e)
                  (map expand-forms (cdr e)))))))

;; table mapping expression head to a function expanding that form
(define expand-table
  (table
   'function       expand-function-def
   '->             expand-arrow
   'let            expand-let
   'soft-let       (lambda (e) (expand-let e #f))
   'macro          expand-macro-def
   'struct         expand-struct-def
   'try            expand-try

   'lambda
   (lambda (e)
     `(lambda ,(map expand-forms (cadr e))
        ,@(if (length= e 3) '(()) '())
        ,@(map expand-forms (cddr e))))

   'block
   (lambda (e)
     (cond ((null? (cdr e)) '(null))
           ((and (null? (cddr e))
                 (not (linenum? (cadr e))))
            (expand-forms (cadr e)))
           (else
            (cons 'block
                  (map expand-forms (cdr e))))))

   '|.|
   (lambda (e) ; e = (|.| f x)
     (expand-fuse-broadcast '() e))

   '.=
   (lambda (e)
     (expand-fuse-broadcast (cadr e) (caddr e)))

   '|<:|
   (lambda (e) (expand-forms `(call |<:| ,@(cdr e))))
   '|>:|
   (lambda (e) (expand-forms `(call |>:| ,@(cdr e))))

   'where
   (lambda (e) (expand-forms (expand-wheres (cadr e) (cddr e))))

   'const  expand-const-decl
   'local  expand-local-or-global-decl
   'global expand-local-or-global-decl
   'local-def expand-local-or-global-decl

   '=
   (lambda (e)
     (define lhs (cadr e))
     (define (function-lhs? lhs)
       (and (pair? lhs)
            (or (eq? (car lhs) 'call)
                (eq? (car lhs) 'where)
                (and (eq? (car lhs) '|::|)
                     (pair? (cadr lhs))
                     (eq? (car (cadr lhs)) 'call)))))
     (define (assignment-to-function lhs e)  ;; convert '= expr to 'function expr
       (cons 'function (cdr e)))
     (cond
      ((function-lhs? lhs)
       (expand-forms (assignment-to-function lhs e)))
      ((and (pair? lhs)
            (eq? (car lhs) 'curly))
       (expand-unionall-def (cadr e) (caddr e)))
      ((assignment? (caddr e))
       ;; chain of assignments - convert a=b=c to `b=c; a=c`
       (let loop ((lhss (list lhs))
                  (rhs  (caddr e)))
         (if (and (assignment? rhs) (not (function-lhs? (cadr rhs))))
             (loop (cons (cadr rhs) lhss) (caddr rhs))
             (let ((rr (if (symbol-like? rhs) rhs (make-ssavalue))))
               (expand-forms
                `(block ,.(if (eq? rr rhs) '() `((= ,rr ,(if (assignment? rhs)
                                                             (assignment-to-function (cadr rhs) rhs)
                                                             rhs))))
                        ,@(map (lambda (l) `(= ,l ,rr))
                               lhss)
                        (unnecessary ,rr)))))))
      ((or (and (symbol-like? lhs) (valid-name? lhs))
           (globalref? lhs) (outerref? lhs))
       (sink-assignment lhs (expand-forms (caddr e))))
      ((atom? lhs)
       (error (string "invalid assignment location \"" (deparse lhs) "\"")))
      (else
       (case (car lhs)
         ((|.|)
          ;; a.b =
          (let* ((a   (cadr lhs))
                 (b  (caddr lhs))
                 (rhs (caddr e)))
            (if (and (length= b 2) (eq? (car b) 'tuple))
                (error (string "invalid syntax \""
                               (string (deparse a) ".(" (deparse (cadr b)) ") = ...") "\"")))
            (let ((aa (if (symbol-like? a) a (make-ssavalue)))
                  (bb (if (or (atom? b) (symbol-like? b) (and (pair? b) (quoted? b)))
                          b (make-ssavalue)))
                  (rr (if (or (symbol-like? rhs) (atom? rhs)) rhs (make-ssavalue))))
              `(block
                ,.(if (eq? aa a)   '() (list (sink-assignment aa (expand-forms a))))
                ,.(if (eq? bb b)   '() (list (sink-assignment bb (expand-forms b))))
                ,.(if (eq? rr rhs) '() (list (sink-assignment rr (expand-forms rhs))))
                (call (top setproperty!) ,aa ,bb ,rr)
                (unnecessary ,rr)))))
         ((tuple)
          ;; multiple assignment
          (let ((lhss (cdr lhs))
                (x    (caddr e)))
            (define (sides-match? l r)
              ;; l and r either have equal lengths, or r has a trailing ...
              (cond ((null? l)          (null? r))
                    ((null? r)          #f)
                    ((vararg? (car r))  (null? (cdr r)))
                    (else               (sides-match? (cdr l) (cdr r)))))
            (if (and (pair? x) (pair? lhss) (eq? (car x) 'tuple)
                     (sides-match? lhss (cdr x)))
                ;; (a, b, ...) = (x, y, ...)
                (expand-forms
                 (tuple-to-assignments lhss x))
                ;; (a, b, ...) = other
                (let* ((xx  (if (or (and (symbol? x) (not (memq x lhss)))
                                    (ssavalue? x))
                                x (make-ssavalue)))
                       (ini (if (eq? x xx) '() (list (sink-assignment xx (expand-forms x)))))
                       (n   (length lhss))
                       (st  (gensy)))
                  `(block
                    ,@ini
                    ,.(map (lambda (i lhs)
                             (expand-forms
                              (lower-tuple-assignment
                               (if (= i (- n 1))
                                   (list lhs)
                                   (list lhs st))
                               `(call (top indexed_iterate)
                                      ,xx ,(+ i 1) ,.(if (eq? i 0) '() `(,st))))))
                           (iota n)
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
                               (contains (lambda (x) (eq? x 'end))
                                         idxs)))
                   (arr   (if reuse (make-ssavalue) a))
                   (stmts (if reuse `((= ,arr ,(expand-forms a))) '()))
                   (rrhs (and (pair? rhs) (not (ssavalue? rhs)) (not (quoted? rhs))))
                   (r    (if rrhs (make-ssavalue) rhs))
                   (rini (if rrhs (list (sink-assignment r (expand-forms rhs))) '())))
              (receive
               (new-idxs stuff) (process-indices arr idxs)
               `(block
                 ,@stmts
                 ,.(map expand-forms stuff)
                 ,@rini
                 ,(expand-forms
                   `(call (top setindex!) ,arr ,r ,@new-idxs))
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

   'primitive
   (lambda (e)
     (let ((sig (cadr e))
           (n   (caddr e)))
       (expand-forms
        (receive (name params super) (analyze-type-sig sig)
                 (primitive-type-def-expr n name params super)))))

   'comparison
   (lambda (e) (expand-forms (expand-compare-chain (cdr e))))

   'ref
   (lambda (e)
     (let ((args (cddr e)))
       (if (has-parameters? args)
           (error "unexpected semicolon in array expression")
           (expand-forms (partially-expand-ref e)))))

   'curly
   (lambda (e)
     (if (has-parameters? (cddr e))
         (error (string "unexpected semicolon in \"" (deparse e) "\"")))
     (if (any assignment? (cddr e))
         (error (string "misplaced assignment statement in \"" (deparse e) "\"" )))
     (let* ((p (extract-implicit-whereparams e))
            (curlyparams (car p))
            (whereparams (cdr p)))
       (if (null? whereparams)
           (expand-forms `(call (core apply_type) ,@(cdr e)))
           (expand-forms `(where (curly ,(cadr e) ,@curlyparams) ,@whereparams)))))

   'call
   (lambda (e)
     (if (length> e 2)
         (let ((f (cadr e)))
           (cond ((dotop-named? f)
                  (expand-fuse-broadcast '() `(|.| ,(undotop f) (tuple ,@(cddr e)))))
                 ((eq? f 'ccall)
                  (if (not (length> e 4)) (error "too few arguments to ccall"))
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
                           (lower-ccall name RT (cdr argtypes) args
                                        (if have-cconv cconv 'ccall))))))
                 ((any kwarg? (cddr e))       ;; f(..., a=b, ...)
                  (expand-forms (lower-kw-call f (cddr e))))
                 ((has-parameters? (cddr e))  ;; f(...; ...)
                  (expand-forms
                   (if (null? (cdr (car (cddr e))))
                       ;; empty parameters block; issue #18845
                       `(call ,f ,@(cdddr e))
                       (lower-kw-call f (cddr e)))))
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
                     `(call (core _apply_iterate) (top iterate) ,f ,@(tuple-wrap argl '())))))

                 ((and (eq? (identifier-name f) '^) (length= e 4) (integer? (cadddr e)))
                  (expand-forms
                   `(call (top literal_pow) ,f ,(caddr e) (call (call (core apply_type) (top Val) ,(cadddr e))))))
                 (else
                  (map expand-forms e))))
         (map expand-forms e)))

   'do
   (lambda (e)
     (let* ((call (cadr e))
            (f    (cadr call))
            (argl (cddr call))
            (af   (caddr e)))
       (expand-forms
        (if (has-parameters? argl)
            `(call ,f ,(car argl) ,af ,@(cdr argl))
            `(call ,f ,af ,@argl)))))

   'tuple
   (lambda (e)
     (cond ((and (length> e 1) (pair? (cadr e)) (eq? (caadr e) 'parameters))
            (if (length= e 2)
                (expand-forms (lower-named-tuple (cdr (cadr e))))
                (error "unexpected semicolon in tuple")))
           ((any assignment? (cdr e))
            (expand-forms (lower-named-tuple (cdr e))))
           (else
            (expand-forms `(call (core tuple) ,@(cdr e))))))

   'braces    (lambda (e) (error "{ } vector syntax is discontinued"))
   'bracescat (lambda (e) (error "{ } matrix syntax is discontinued"))

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
     `(break-block loop-exit
                   (_while ,(expand-forms (cadr e))
                           (break-block loop-cont
                                        (scope-block ,(blockify (expand-forms (caddr e))))))))

   'break
   (lambda (e)
     (if (pair? (cdr e))
         e
         '(break loop-exit)))

   'continue (lambda (e) '(break loop-cont))

   'for
   (lambda (e)
     (let ((ranges (if (eq? (car (cadr e)) 'block)
                       (cdr (cadr e))
                       (list (cadr e)))))
       (expand-forms (expand-for (map cadr ranges) (map caddr ranges) (caddr e)))))

   '&&     (lambda (e) (expand-forms (expand-and e)))
   '|\|\|| (lambda (e) (expand-forms (expand-or  e)))

   '&      (lambda (e) (error (string "invalid syntax " (deparse e))))

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

   '|...|
   (lambda (e) (error "\"...\" expression outside call"))

   '$
   (lambda (e) (error "\"$\" expression outside quote"))

   'vect
   (lambda (e)
     (if (has-parameters? (cdr e))
         (error "unexpected semicolon in array expression"))
     (if (any assignment? (cdr e))
         (error (string "misplaced assignment statement in \"" (deparse e) "\"")))
     (expand-forms `(call (top vect) ,@(cdr e))))

   'hcat
   (lambda (e)
     (if (any assignment? (cdr e))
         (error (string "misplaced assignment statement in \"" (deparse e) "\"")))
     (expand-forms `(call (top hcat) ,@(cdr e))))

   'vcat
   (lambda (e)
     (let ((a (cdr e)))
       (if (any assignment? a)
           (error (string "misplaced assignment statement in \"" (deparse e) "\"")))
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
                  `(call (top hvcat)
                         (tuple ,.(map length rows))
                         ,.(apply append rows)))
                `(call (top vcat) ,@a))))))

   'typed_hcat
   (lambda (e)
     (if (any assignment? (cddr e))
         (error (string "misplaced assignment statement in \"" (deparse e) "\"")))
     (expand-forms `(call (top typed_hcat) ,@(cdr e))))

   'typed_vcat
   (lambda (e)
     (let ((t (cadr e))
           (a (cddr e)))
       (if (any assignment? (cddr e))
           (error (string "misplaced assignment statement in \"" (deparse e) "\"")))
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

   '|'|  (lambda (e) (expand-forms `(call |'| ,(cadr e))))

   'generator
   (lambda (e)
     (check-no-return e)
     (expand-generator e #f '()))

   'flatten
   (lambda (e) (expand-generator (cadr e) #t '()))

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
                 (and (every (lambda (x) (and (pair? x) (eq? (car x) '=)))
                             ranges)
                      ;; TODO: this is a hack to lower simple comprehensions to loops very
                      ;; early, to greatly reduce the # of functions and load on the compiler
                      (lower-comprehension (cadr e) (cadr (caddr e)) ranges))))
          `(call (top collect) ,(cadr e) ,(caddr e)))))

    'gc_preserve
    (lambda (e)
      (let* ((s (make-ssavalue))
             (r (make-ssavalue)))
        `(block
          (= ,s (gc_preserve_begin ,@(cddr e)))
          (= ,r ,(expand-forms (cadr e)))
          (gc_preserve_end ,s)
          ,r)))

    'line
    (lambda (e)
      (set! *current-desugar-loc* e)
      e)
    ))

(define (has-return? e)
  (expr-contains-p return? e (lambda (x) (not (function-def? x)))))

(define (check-no-return e)
  (if (has-return? e)
      (error "\"return\" not allowed inside comprehension or generator")))

(define (has-break-or-continue? e)
  (expr-contains-p (lambda (x) (and (pair? x) (memq (car x) '(break continue))))
                   e
                   (lambda (x) (not (and (pair? x)
                                         (memq (car x) '(for while)))))))

(define (lower-comprehension ty expr itrs)
  (check-no-return expr)
  (if (has-break-or-continue? expr)
      (error "break or continue outside loop"))
  (let ((result    (gensy))
        (idx       (gensy))
        (oneresult (make-ssavalue))
        (prod      (make-ssavalue))
        (isz       (make-ssavalue))
        (szunk     (make-ssavalue))
        (iv        (map (lambda (x) (make-ssavalue)) itrs)))

    ;; construct loops over all iterators
    (define (construct-loops itrs iv)
      (if (null? itrs)
          `(block (= ,oneresult ,expr)
                  (inbounds (true))
                  (if ,szunk
                      (call (top push!) ,result ,oneresult)
                      (call (top setindex!) ,result ,oneresult ,idx))
                  (inbounds pop)
                  (= ,idx (call (top add_int) ,idx 1)))
          `(for (= ,(cadr (car itrs)) ,(car iv))
                ,(construct-loops (cdr itrs) (cdr iv)))))

    (let ((overall-itr (if (length= itrs 1) (car iv) prod)))
      `(scope-block
        (block
         (local ,result) (local ,idx)
         ,.(map (lambda (v r) `(= ,v ,(caddr r))) iv itrs)
         ,.(if (length= itrs 1)
               '()
               `((= ,prod (call (top product) ,@iv))))
         (= ,isz (call (top IteratorSize) ,overall-itr))
         (= ,szunk (call (core isa) ,isz (top SizeUnknown)))
         (if ,szunk
             (= ,result (call (curly (core Array) ,ty 1) (core undef) 0))
             (= ,result (call (top _array_for) ,ty ,overall-itr ,isz)))
         (= ,idx (call (top first) (call (top LinearIndices) ,result)))
         ,(construct-loops (reverse itrs) (reverse iv))
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
  (cond ((eventually-call? e) e)
        ((decl? e)   (decl-var e))
        ((and (pair? e) (eq? (car e) 'tuple))
         (cons 'tuple (map all-decl-vars (cdr e))))
        (else e)))

;; pass 2: identify and rename local vars

(define (find-assigned-vars e)
  (if (or (not (pair? e)) (quoted? e))
      '()
      (case (car e)
        ((lambda scope-block module toplevel)  '())
        ((method)
         (let ((v (decl-var (method-expr-name e))))
           (append!
            (if (length= e 2) '() (find-assigned-vars (caddr e)))
            (if (not (symbol? v))
                '()
                (list v)))))
        ((=)
         (let ((v (decl-var (cadr e)))
               (rest (find-assigned-vars (caddr e))))
           (if (or (ssavalue? v) (globalref? v) (outerref? v) (underscore-symbol? v))
               rest
               (cons v rest))))
        (else
         (apply append! (map find-assigned-vars e))))))

(define (find-decls kind e)
  (if (or (not (pair? e)) (quoted? e))
      '()
      (cond ((memq (car e) '(lambda scope-block module toplevel))
             '())
            ((eq? (car e) kind)
             (if (underscore-symbol? (cadr e))
                 '()
                 (list (decl-var (cadr e)))))
            (else
             (apply append! (map (lambda (x) (find-decls kind x))
                                 e))))))

(define (find-local-decls e) (find-decls 'local e))
(define (find-local-def-decls e) (find-decls 'local-def e))
(define (find-global-decls e) (find-decls 'global e))

(define (find-scope-decl e kind)
  (expr-contains-p
   (lambda (x) (and (pair? x) (eq? (car x) kind) x))
   e
   (lambda (x) (not (and (pair? x)
                         (memq (car x) '(lambda scope-block module toplevel)))))))

(define (check-valid-name e)
  (or (valid-name? e)
      (error (string "invalid identifier name \"" e "\""))))

(define (make-scope (lam #f) (args '()) (locals '()) (globals '()) (sp '()) (renames '()) (prev #f)
                    (soft? #f) (hard? #f) (implicit-globals '()) (warn-vars #f))
  (vector lam args locals globals sp renames prev soft? hard? implicit-globals warn-vars))
(define (scope:lam s)     (aref s 0))
(define (scope:args s)    (aref s 1))
(define (scope:locals s)  (aref s 2))
(define (scope:globals s) (aref s 3))
(define (scope:sp s)      (aref s 4))
(define (scope:renames s) (aref s 5))
(define (scope:prev s)    (aref s 6))
(define (scope:soft? s)   (aref s 7))
(define (scope:hard? s)   (aref s 8))
(define (scope:implicit-globals s) (aref s 9))
(define (scope:warn-vars s) (aref s 10))

(define (var-kind var scope (exclude-top-level-globals #f))
  (if scope
      (or (and (memq var (scope:args scope))    'argument)
          (and (memq var (scope:locals scope))  'local)
          (and (memq var (scope:globals scope))
               (if (and exclude-top-level-globals
                        (null? (lam:vars (scope:lam scope)))
                        ;; don't inherit global decls from the outermost scope block
                        ;; in a top-level expression.
                        (or (not (scope:prev scope))
                            (not (scope:prev (scope:prev scope)))))
                   'none 'global))
          (and (memq var (scope:sp scope))      'static-parameter)
          (var-kind var (scope:prev scope) exclude-top-level-globals))
      'none))

(define (in-scope? var scope) (not (eq? (var-kind var scope) 'none)))

(define (warn-var?! v scope)
  (and scope
       (let ((w (scope:warn-vars scope)))
         (if (and w (has? w v))
             (begin (del! w v)
                    #t)
             (warn-var?! v (scope:prev scope))))))

(define (all-local-names scope)
  (define (all-lists s)
    (if s
        (list* (scope:args s) (scope:sp s) (scope:locals s) (all-lists (scope:prev s)))
        '()))
  (apply append (all-lists scope)))

;; returns lambdas in the form (lambda (args...) (locals...) body)
(define (resolve-scopes- e scope (sp '()) (loc #f))
  (cond ((symbol? e)
         (let lookup ((scope scope))
           (if scope
               (cond ((memq e (scope:args scope)) e)
                     ((memq e (scope:globals scope)) `(outerref ,e))
                     (else
                      (let ((r (assq e (scope:renames scope))))
                        (cond (r (cdr r))
                              ((memq e (scope:locals scope)) e)
                              ((memq e (scope:sp scope)) e)
                              (else
                               (lookup (scope:prev scope)))))))
               (if (underscore-symbol? e)
                   e
                   `(outerref ,e)))))
        ((or (not (pair? e)) (quoted? e) (memq (car e) '(toplevel symbolicgoto symboliclabel toplevel-only)))
         e)
        ((eq? (car e) 'global)
         (check-valid-name (cadr e))
         e)
        ((memq (car e) '(local local-def))
         (check-valid-name (cadr e))
         ;; remove local decls
         '(null))
        ((eq? (car e) 'require-existing-local)
         (if (not (in-scope? (cadr e) scope))
             (error "no outer local variable declaration exists for \"for outer\""))
         '(null))
        ((or (eq? (car e) 'softscope) (eq? (car e) 'hardscope))
         '(null))
        ((eq? (car e) 'locals)
         (let* ((names (filter (lambda (v)
                                 (and (not (gensym? v))
                                      (not (length= (string-split (string v) "#") 2))))
                               (all-local-names scope)))
                (names (delete-duplicates
                        (filter (lambda (v) (not (eq? v '||)))
                                (map unmangled-name names))))
                (d (make-ssavalue)))
           `(block (= ,d (call (call (core apply_type) (top Dict) (core Symbol) (core Any))))
                   ,@(map (lambda (v)
                            (let ((var (resolve-scopes- v scope)))
                              `(if (isdefined ,var)
                                   (call (top setindex!) ,d ,var (quote ,v)))))
                          names)
                   ,d)))
        ((eq? (car e) 'islocal)
         (if (memq (var-kind (cadr e) scope) '(global none))
             '(false)
             '(true)))
        ((eq? (car e) 'lambda)
         (let* ((args (lam:vars e))
                (body (resolve-scopes- (lam:body e) (make-scope e args '() '() sp '() scope))))
           `(lambda ,(cadr e) ,(caddr e) ,body)))
        ((eq? (car e) 'scope-block)
         (let* ((blok            (cadr e)) ;; body of scope-block expression
                (lam             (scope:lam scope))
                (argnames        (lam:vars lam))
                (toplevel?       (and (null? argnames) (eq? e (lam:body lam))))
                (current-locals  (caddr lam)) ;; locals created so far in our lambda
                (globals         (find-global-decls blok))
                (assigned        (find-assigned-vars blok))
                (locals-def      (find-local-def-decls blok))
                (local-decls     (find-local-decls blok))
                (hard?           (and (null? argnames) (or (scope:hard? scope)
                                                           (find-scope-decl blok 'hardscope))))
                (soft?           (and (null? argnames) (not hard?)
                                      (let ((ss (find-scope-decl blok 'softscope)))
                                        (cond ((not ss) (scope:soft? scope))
                                              ((equal? (cadr ss) '(true))  #t)
                                              ((equal? (cadr ss) '(false)) #f)
                                              (else (scope:soft? scope))))))
                (nonloc-assigned (filter (lambda (v) (and (not (memq v locals-def))
                                                          (not (memq v local-decls))))
                                         assigned))
                (implicit-globals (if toplevel? nonloc-assigned '()))
                (implicit-locals
                 (filter (if toplevel?
                             ;; make only assigned gensyms implicitly local at top level
                             some-gensym?
                             (lambda (v) (and (memq (var-kind v scope #t) '(none static-parameter))
                                              (not (and soft?
                                                        (or (memq v (scope:implicit-globals scope))
                                                            (defined-julia-global v))))
                                              (not (memq v globals)))))
                         nonloc-assigned))
                (locals-nondef   (delete-duplicates (append local-decls implicit-locals)))
                (need-rename?    (lambda (vars)
                                   (filter (lambda (v) (or (memq v current-locals) (in-scope? v scope)))
                                           vars)))
                (need-rename     (need-rename? locals-nondef))
                (need-rename-def (need-rename? locals-def))
                ;; new gensym names for conflicting variables
                (renamed         (map named-gensy need-rename))
                (renamed-def     (map named-gensy need-rename-def))
                (newnames        (append (diff locals-nondef need-rename) renamed))
                (newnames-def    (append (diff locals-def need-rename-def) renamed-def))
                (warn-vars
                 (and (not toplevel?) (null? argnames) (not soft?) (not hard?)
                      (let ((vars (filter (lambda (v)
                                            (and (or (memq v (scope:implicit-globals scope))
                                                     (defined-julia-global v))
                                                 (eq? (var-kind v scope) 'none)
                                                 (not (memq v globals))))
                                          nonloc-assigned)))
                        (if (pair? vars)
                            (let ((t (table)))
                              (for-each (lambda (v) (put! t v #t))
                                        vars)
                              t)
                            #f)))))

           (for-each (lambda (v)
                       (if (or (memq v locals-def) (memq v local-decls))
                           (error (string "variable \"" v "\" declared both local and global")))
                       (if (and (null? argnames) (memq (var-kind v scope) '(argument local)))
                           (error (string "`global " v "`: " v " is a local variable in its enclosing scope"))))
                     globals)
           (if (and (pair? argnames) (eq? e (lam:body lam)))
               (for-each (lambda (v)
                           (if (memq v argnames)
                               (error (string "local variable name \"" v "\" conflicts with an argument"))))
                         local-decls))
           (for-each (lambda (lst)
                       (for-each (lambda (v)
                                   (if (eq? (var-kind v scope) 'static-parameter)
                                       (error (string "local variable name \"" v "\" conflicts with a static parameter"))))
                                 lst))
                     (list local-decls implicit-locals))
           (if lam
               (set-car! (cddr lam)
                         (append (caddr lam) newnames newnames-def)))
           (insert-after-meta ;; return the new, expanded scope-block
            (blockify
             (resolve-scopes- blok
                              (make-scope lam
                                          '()
                                          (append locals-nondef locals-def)
                                          globals
                                          '()
                                          (append (map cons need-rename renamed)
                                                  (map cons need-rename-def renamed-def))
                                          scope
                                          (and soft? (null? argnames))
                                          hard?
                                          (if toplevel?
                                              implicit-globals
                                              (scope:implicit-globals scope))
                                          warn-vars)
                              '()
                              loc))
            (append! (map (lambda (v) `(local ,v)) newnames)
                     (map (lambda (v) `(local-def ,v)) newnames-def)))
           ))
        ((eq? (car e) 'module)
         (error "\"module\" expression not at top level"))
        ((eq? (car e) 'break-block)
         `(break-block ,(cadr e) ;; ignore type symbol of break-block expression
                       ,(resolve-scopes- (caddr e) scope '() loc))) ;; body of break-block expression
        ((eq? (car e) 'with-static-parameters)
         `(with-static-parameters
           ,(resolve-scopes- (cadr e) scope (cddr e) loc)
           ,@(cddr e)))
        ((and (eq? (car e) 'method) (length> e 2))
         `(method
           ,(resolve-scopes- (cadr   e) scope)
           ,(resolve-scopes- (caddr  e) scope)
           ,(resolve-scopes- (cadddr e) scope (method-expr-static-parameters e))))
        (else
         (if (and (eq? (car e) '=) (symbol? (cadr e))
                  scope (null? (lam:vars (scope:lam scope)))
                  (warn-var?! (cadr e) scope)
                  (= *scopewarn-opt* 1))
             (let* ((v    (cadr e))
                    (loc  (extract-line-file loc))
                    (line (if (= (car loc) 0) (julia-current-line) (car loc)))
                    (file (if (eq? (cadr loc) 'none) (julia-current-file) (cadr loc))))
               (lowering-warning
                1000 'warn (symbol (string file line)) file line
                (string "Assignment to `" v "` in soft scope is ambiguous "
                        "because a global variable by the same name exists: "
                        "`" v "` will be treated as a new local. "
                        "Disambiguate by using `local " v "` to suppress this warning or "
                        "`global " v "` to assign to the existing global variable."))))
         (cons (car e)
               (map (lambda (x)
                      (if (linenum? x)
                          (set! loc x))
                      (resolve-scopes- x scope '() loc))
                    (cdr e))))))

(define (resolve-scopes e) (resolve-scopes- e #f))

;; pass 3: analyze variables

;; names of arguments and local vars
(define (lambda-all-vars e)
  (append (lam:vars e) (caddr e)))

;; compute set of variables referenced in a lambda but not bound by it
(define (free-vars- e tab)
  (cond ((or (eq? e UNUSED) (underscore-symbol? e)) tab)
        ((symbol? e) (put! tab e #t))
        ((and (pair? e) (eq? (car e) 'outerref)) tab)
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

(define (analyze-vars-lambda e env captvars sp new-sp (methsig #f))
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
         (sig-fv (if methsig (free-vars methsig) '()))
         (glo  (find-global-decls (lam:body e)))
         ;; make var-info records for vars introduced by this lambda
         (vi   (nconc
                (map (lambda (decl) (make-var-info (decl-var decl)))
                     args)
                (map make-var-info locl)))
         (capt-sp (filter (lambda (v) (or (and (memq v fv) (not (memq v glo)) (not (memq v new-sp)))
                                          (memq v sig-fv)))
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
      (begin
        (if (symbol? e)
            (let ((vi (var-info-for e env)))
              (if vi
                  (vinfo:set-read! vi #t))))
        e)
      (case (car e)
        ((local-def) ;; a local that we know has an assignment that dominates all usages
         (let ((vi (var-info-for (cadr e) env)))
              (vinfo:set-never-undef! vi #t)))
        ((=)
         (let ((vi (and (symbol? (cadr e)) (var-info-for (cadr e) env))))
           (if vi ; if local or captured
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
        ((decl)
         ;; handle var::T declaration by storing the type in the var-info
         ;; record. for non-symbols or globals, emit a type assertion.
         (let ((vi (var-info-for (cadr e) env)))
           (if vi
               (begin (if (not (equal? (vinfo:type vi) '(core Any)))
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
                                         (method-expr-static-parameters e)
                                         (caddr e)))))
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
  (let ((n (length P))
        (s (make-ssavalue)))
    `((thunk
       (lambda ()
         (() () 0 ())
         (block (global ,name) (const ,name)
                ,@(map (lambda (p n) `(= ,p (call (core TypeVar) ',n (core Any)))) P names)
                (= ,s (call (core _structtype) (thismodule) (inert ,name) (call (core svec) ,@P)
                            (call (core svec) ,@(map quotify fields))
                            (false) ,(length fields)))
                (= (outerref ,name) ,s)
                (call (core _setsuper!) ,name ,super)
                (call (core _typebody!) ,name (call (core svec) ,@types))
                (return (null))))))))

(define (type-for-closure name fields super)
  (let ((s (make-ssavalue)))
    `((thunk (lambda ()
               (() () 0 ())
               (block (global ,name) (const ,name)
                      (= ,s (call (core _structtype) (thismodule) (inert ,name) (call (core svec))
                                  (call (core svec) ,@(map quotify fields))
                                  (false) ,(length fields)))
                      (= (outerref ,name) ,s)
                      (call (core _setsuper!) ,name ,super)
                      (call (core _typebody!) ,name
                            (call (core svec) ,@(map (lambda (v) '(core Box)) fields)))
                      (return (null))))))))

;; better versions of above, but they get handled wrong in many places
;; need to fix that in order to handle #265 fully (and use the definitions)

;; template for generating a closure type with parameters
;(define (type-for-closure-parameterized name P names fields types super)
;  (let ((n (length P)))
;        `((global ,name)
;          (const ,name)
;          ,@(map (lambda (p n) `(= ,p (call (core TypeVar) ',n (core Any)))) P names)
;          (struct_type ,name (call (core svec) ,@P)
;                       (call (core svec) ,@(map quotify fields))
;                       ,super
;                       (call (core svec) ,@types) (false) ,(length fields)))))

;; ... and without parameters
;(define (type-for-closure name fields super)
;  `((global ,name)
;    (const ,name)
;    (struct_type ,name (call (core svec))
;                 (call (core svec) ,@(map quotify fields))
;                 ,super
;                 (call (core svec) ,@(map (lambda (v) 'Any) fields))
;                 (false) ,(length fields))))


(define (vinfo:not-capt vi)
  (list (car vi) (cadr vi) (logand (caddr vi) (lognot 5))))

(define (clear-capture-bits vinfos)
  (map vinfo:not-capt vinfos))

(define (convert-lambda lam fname interp capt-sp)
  (let ((body (add-box-inits-to-body
                lam (cl-convert (cadddr lam) fname lam (table) (table) #f interp))))
    `(lambda ,(lam:args lam)
       (,(clear-capture-bits (car (lam:vinfo lam)))
        ()
        ,(caddr (lam:vinfo lam))
        ,(delete-duplicates (append (lam:sp lam) capt-sp)))
      ,body)))

;; renumber ssavalues assigned in an expr, allowing it to be repeated
(define (renumber-assigned-ssavalues e)
  (let ((vals (expr-find-all (lambda (x) (and (assignment? x) (ssavalue? (cadr x))))
                             e
                             cadadr)))
    (if (null? vals)
        e
        (let ((repl (table)))
          (for-each (lambda (id) (put! repl id (make-ssavalue)))
                    vals)
          (let do-replace ((x e))
            (if (or (atom? x) (quoted? x))
                x
                (if (eq? (car x) 'ssavalue)
                    (or (get repl (cadr x) #f) x)
                    (cons (car x)
                          (map do-replace (cdr x))))))))))

(define (convert-for-type-decl rhs t)
  (if (equal? t '(core Any))
      rhs
      (let* ((temp (if (or (atom? t) (ssavalue? t) (quoted? t))
                       #f
                       (make-ssavalue)))
             (ty   (or temp t))
             (ex   `(call (core typeassert)
                          (call (top convert) ,ty ,rhs)
                          ,ty)))
        (if temp
            `(block (= ,temp ,(renumber-assigned-ssavalues t)) ,ex)
            ex))))

;; convert assignment to a closed variable to a setfield! call.
;; while we're at it, generate `convert` calls for variables with
;; declared types.
;; when doing this, the original value needs to be preserved, to
;; ensure the expression `a=b` always returns exactly `b`.
(define (convert-assignment var rhs0 fname lam interp)
  (cond
    ((symbol? var)
     (let* ((vi (assq var (car  (lam:vinfo lam))))
            (cv (assq var (cadr (lam:vinfo lam))))
            (vt  (or (and vi (vinfo:type vi))
                     (and cv (vinfo:type cv))
                     '(core Any)))
            (closed (and cv (vinfo:asgn cv) (vinfo:capt cv)))
            (capt   (and vi (vinfo:asgn vi) (vinfo:capt vi))))
       (if (and (not closed) (not capt) (equal? vt '(core Any)))
           `(= ,var ,rhs0)
           (let* ((rhs1 (if (or (simple-atom? rhs0)
                                (equal? rhs0 '(the_exception)))
                            rhs0
                            (make-ssavalue)))
                  (rhs  (if (equal? vt '(core Any))
                            rhs1
                            (convert-for-type-decl rhs1 (cl-convert vt fname lam #f #f #f interp))))
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
     ((and (pair? var) (or (eq? (car var) 'outerref)
                           (eq? (car var) 'globalref)))

      `(= ,var ,rhs0))
     ((ssavalue? var)
      `(= ,var ,rhs0))
     (else
       (error (string "invalid assignment location \"" (deparse var) "\"")))))

(define (rename-sig-types ex namemap)
  (pattern-replace
   (pattern-set
    (pattern-lambda (call (core (-/ Typeof)) name)
                    (get namemap name __)))
   ex))

;; replace leading (function) argument type with `typ`
(define (fix-function-arg-type te typ iskw namemap type-sp)
  (let* ((typapp (caddr te))
         (types  (rename-sig-types (cddr typapp) namemap))
         (closure-type (if (null? type-sp)
                           typ
                           `(call (core apply_type) ,typ ,@type-sp)))
         (newtypes
          (if iskw
              `(,(car types) ,(cadr types) ,closure-type ,@(cdddr types))
              `(,closure-type ,@(cdr types))))
         (loc (caddddr te)))
    `(call (core svec)
           (call (core svec) ,@newtypes)
           (call (core svec) ,@(append (cddr (cadddr te)) type-sp))
           ,loc)))

;; collect all toplevel-butfirst expressions inside `e`, and return
;; (ex . stmts), where `ex` is the expression to evaluated and
;; `stmts` is a list of statements to move to the top level.
(define (lift-toplevel e)
  (let ((top '()))
    (define (lift- e)
      (if (or (atom? e) (quoted? e))
          e
          (let ((e (cons (car e) (map lift- (cdr e)))))
            (if (eq? (car e) 'toplevel-butfirst)
                (begin (set! top (cons (cddr e) top))
                       (cadr e))
                e))))
    (let ((e2 (lift- e)))
      (let ((stmts (apply append (reverse top))))
        ;; move all type definitions first
        (receive (structs others)
                 (separate (lambda (x) (and (pair? x) (eq? (car x) 'thunk)))
                           stmts)
                 (cons e2 (append structs others)))))))

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
  (if (null? stmts)
      body
      (let ((meta (take-while (lambda (x) (and (pair? x)
                                               (memq (car x) '(line meta))))
                              (cdr body))))
        `(,(car body)
          ,@meta
          ,@stmts
          ,@(list-tail body (+ 1 (length meta)))))))

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

;; find all methods for the same function as `ex` in `body`
(define (all-methods-for ex body)
  (let ((mname (method-expr-name ex)))
    (expr-find-all (lambda (s)
                     (and (length> s 2) (eq? (car s) 'method)
                          (eq? (method-expr-name s) mname)))
                   body
                   identity
                   (lambda (x) (and (pair? x) (not (eq? (car x) 'lambda)))))))

(define lambda-opt-ignored-exprs
  (Set '(quote top core line inert local local-def unnecessary copyast
         meta inbounds boundscheck loopinfo decl aliasscope popaliasscope
         thunk with-static-parameters toplevel-only
         global globalref outerref const-if-global thismodule
         const null true false ssavalue isdefined toplevel module lambda error
         gc_preserve_begin gc_preserve_end import using export)))

(define (local-in? s lam)
  (or (assq s (car  (lam:vinfo lam)))
      (assq s (cadr (lam:vinfo lam)))))

;; Try to identify never-undef variables, and then clear the `captured` flag for single-assigned,
;; never-undef variables to avoid allocating unnecessary `Box`es.
(define (lambda-optimize-vars! lam)
  (assert (eq? (car lam) 'lambda))
  ;; memoize all-methods-for to avoid O(n^2) behavior
  (define allmethods-table (table))
  (define (get-methods ex stmts)
    (let ((mn (method-expr-name ex)))
      (if (has? allmethods-table mn)
          (get allmethods-table mn)
          (let ((am (all-methods-for ex stmts)))
            (put! allmethods-table mn am)
            am))))
  ;; This does a basic-block-local dominance analysis to find variables that
  ;; are never used undef.
  (let ((vi     (car (lam:vinfo lam)))
        (unused (table))  ;; variables not (yet) used (read from) in the current block
        (live   (table))  ;; variables that have been set in the current block
        (seen   (table))) ;; all variables we've seen assignments to
    ;; Collect candidate variables: those that are captured (and hence we want to optimize)
    ;; and only assigned once. This populates the initial `unused` table.
    (for-each (lambda (v)
                (if (and (vinfo:capt v) (vinfo:sa v))
                    (put! unused (car v) #t)))
              vi)
    (define (restore old)
      (table.foreach (lambda (k v)
                       (if (not (has? old k))
                           (put! unused k v)))
                     live)
      (set! live old))
    (define (kill)
      ;; when we see control flow, empty live set back into unused set
      (restore (table)))
    (define (mark-used var)
      ;; remove variable from the unused table
      (if (has? unused var)
          (del! unused var)))
    (define (assign! var)
      (if (has? unused var)
          ;; When a variable is assigned, move it to the live set to protect
          ;; it from being removed from `unused`.
          (begin (put! live var #t)
                 (put! seen var #t)
                 (del! unused var))))
    (define (visit e)
      ;; returns whether e contained a symboliclabel
      (cond ((atom? e) (if (symbol? e) (mark-used e))
             #f)
            ((lambda-opt-ignored-exprs (car e))
             #f)
            ((eq? (car e) 'scope-block)
             (visit (cadr e)))
            ((memq (car e) '(block call new splatnew _do_while))
             (eager-any visit (cdr e)))
            ((eq? (car e) 'break-block)
             (visit (caddr e)))
            ((eq? (car e) 'return)
             (begin0 (visit (cadr e))
                     (kill)))
            ((memq (car e) '(break label symbolicgoto))
             (kill)
             #f)
            ((eq? (car e) 'symboliclabel)
             (kill)
             #t)
            ((memq (car e) '(if elseif _while trycatch tryfinally))
             (let ((prev (table.clone live)))
               (if (eager-any (lambda (e) (begin0 (visit e)
                                                  (kill)))
                              (cdr e))
                   ;; if there is a label inside, we could have skipped a prior
                   ;; variable initialization
                   (begin (kill) #t)
                   (begin (restore prev) #f))))
            ((eq? (car e) '=)
             (begin0 (visit (caddr e))
                     (assign! (cadr e))))
            ((eq? (car e) 'method)
             (if (length> e 2)
                 (let* ((mn          (method-expr-name e))
                        ;; a method expression captures a variable if any methods for
                        ;; the same function do.
                        (all-methods (if (local-in? mn lam)
                                         (get-methods e (lam:body lam))
                                         (list e))))
                   (for-each (lambda (ex)
                               (for-each mark-used
                                         (map car (cadr (lam:vinfo (cadddr ex))))))
                             all-methods)
                   (assign! (cadr e))))
             #f)
            (else
             (eager-any visit (cdr e))
             ;; in all other cases there's nothing to do except assert that
             ;; all expression heads have been handled.
             #;(assert (memq (car e) '(foreigncall cfunction |::|))))))
    (visit (lam:body lam))
    ;; Finally, variables can be marked never-undef if they were set in the first block,
    ;; or are currently live, or are back in the unused set (because we've left the only
    ;; block that uses them, or possibly because they have no uses at all).
    (for-each (lambda (v)
                (if (has? seen v)
                    (let ((vv (assq v vi)))
                      (vinfo:set-never-undef! vv #t))))
              (append (table.keys live) (table.keys unused)))
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
  (and (pair? e) (memq (car e) '(if elseif block trycatch tryfinally))))

(define (map-cl-convert exprs fname lam namemap defined toplevel interp)
  (if toplevel
      (map (lambda (x)
             (let ((tl (lift-toplevel (cl-convert x fname lam namemap defined
                                                  (and toplevel (toplevel-preserving? x))
                                                  interp))))
               (if (null? (cdr tl))
                   (car tl)
                   `(block ,@(cdr tl) ,(car tl)))))
           exprs)
      (map (lambda (x) (cl-convert x fname lam namemap defined #f interp)) exprs)))

(define (cl-convert e fname lam namemap defined toplevel interp)
  (if (and (not lam)
           (not (and (pair? e) (memq (car e) '(lambda method macro)))))
      (if (atom? e) e
          (cons (car e) (map-cl-convert (cdr e) fname lam namemap defined toplevel interp)))
      (cond
       ((symbol? e)
        (define (new-undef-var name)
          (let ((g (named-gensy name)))
            (set-car! (lam:vinfo lam) (append (car (lam:vinfo lam)) `((,g Any 32))))
            g))
        (define (get-box-contents box typ)
          ; lower in an UndefVar check to a similarly named variable (ref #20016)
          ; so that closure lowering Box introduction doesn't impact the error message
          ; and the compiler is expected to fold away the extraneous null check
          (let* ((access (if (symbol? box) box (make-ssavalue)))
                 (undeftest `(call (core isdefined) ,access (inert contents)))
                 (undefvar (new-undef-var e))
                 (undefcheck `(if ,undeftest (null) (block (newvar ,undefvar) ,undefvar)))
                 (val `(call (core getfield) ,access (inert contents)))
                 (val (if (equal? typ '(core Any))
                          val
                          `(call (core typeassert) ,val
                                 ,(cl-convert typ fname lam namemap defined toplevel interp)))))
            `(block
               ,@(if (eq? box access) '() `((= ,access ,box)))
               ,undefcheck
               ,val)))
        (let ((vi (assq e (car  (lam:vinfo lam))))
              (cv (assq e (cadr (lam:vinfo lam)))))
          (cond ((eq? e fname) e)
                ((memq e (lam:sp lam)) e)
                (cv
                 (let ((access (if interp
                                   `($ (call (core QuoteNode) ,e))
                                   `(call (core getfield) ,fname (inert ,e)))))
                   (if (and (vinfo:asgn cv) (vinfo:capt cv))
                       (get-box-contents access (vinfo:type cv))
                       access)))
                (vi
                 (if (and (vinfo:asgn vi) (vinfo:capt vi))
                     (get-box-contents e (vinfo:type vi))
                     e))
                (else e))))
       ((atom? e) e)
       (else
        (case (car e)
          ((quote top core globalref outerref thismodule line break inert module toplevel null true false meta) e)
          ((toplevel-only)
           ;; hack to avoid generating a (method x) expr for struct types
           (if (eq? (cadr e) 'struct)
               (put! defined (caddr e) #t))
           e)
          ((=)
           (let ((var (cadr e))
                 (rhs (cl-convert (caddr e) fname lam namemap defined toplevel interp)))
             (convert-assignment var rhs fname lam interp)))
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
          ((const) e)
          ((const-if-global)
           (if (local-in? (cadr e) lam)
               '(null)
               `(const ,(cadr e))))
          ((isdefined) ;; convert isdefined expr to function for closure converted variables
           (let* ((sym (cadr e))
                  (vi (and (symbol? sym) (assq sym (car  (lam:vinfo lam)))))
                  (cv (and (symbol? sym) (assq sym (cadr (lam:vinfo lam))))))
             (cond ((eq? sym fname) e)
                   ((memq sym (lam:sp lam)) e)
                   (cv
                    (if (and (vinfo:asgn cv) (vinfo:capt cv))
                        (let ((access (if interp
                                          `($ (call (core QuoteNode) ,sym))
                                          `(call (core getfield) ,fname (inert ,sym)))))
                          `(call (core isdefined) ,access (inert contents)))
                        '(true)))
                   (vi
                    (if (and (vinfo:asgn vi) (vinfo:capt vi))
                        `(call (core isdefined) ,sym (inert contents))
                        e))
                   (else e))))
          ((method)
           (let* ((name  (method-expr-name e))
                  (short (length= e 2))  ;; function f end
                  (lam2  (if short #f (cadddr e)))
                  (vis   (if short '(() () ()) (lam:vinfo lam2)))
                  (cvs   (map car (cadr vis)))
                  (local? (lambda (s) (and lam (symbol? s) (local-in? s lam))))
                  (local (and (not (outerref? (cadr e))) (local? name)))
                  (sig      (and (not short) (caddr e)))
                  (sp-inits (if (or short (not (eq? (car sig) 'block)))
                                '()
                                (map-cl-convert (butlast (cdr sig))
                                                fname lam namemap defined toplevel interp)))
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
                 (cond (short (if (has? defined (cadr e))
                                  e
                                  (begin
                                    (put! defined (cadr e) #t)
                                    `(toplevel-butfirst
                                      ;; wrap in toplevel-butfirst so it gets moved higher along with
                                      ;; closure type definitions
                                      ,e
                                      (thunk (lambda () (() () 0 ()) (block (return ,e))))))))
                       ((null? cvs)
                        `(block
                          ,@sp-inits
                          (method ,(cadr e) ,(cl-convert
                                          ;; anonymous functions with keyword args generate global
                                          ;; functions that refer to the type of a local function
                                          (rename-sig-types sig namemap)
                                          fname lam namemap defined toplevel interp)
                                  ,(let ((body (add-box-inits-to-body
                                                lam2
                                                (cl-convert (cadddr lam2) 'anon lam2 (table) (table) #f interp))))
                                     `(lambda ,(cadr lam2)
                                        (,(clear-capture-bits (car vis))
                                         ,@(cdr vis))
                                        ,body)))))
                       (else
                        (let* ((exprs     (lift-toplevel (convert-lambda lam2 '|#anon| #t '())))
                               (top-stmts (cdr exprs))
                               (newlam    (compact-and-renumber (linearize (car exprs)) 'none 0)))
                          `(toplevel-butfirst
                            (block ,@sp-inits
                                   (method ,name ,(cl-convert sig fname lam namemap defined toplevel interp)
                                           ,(julia-bq-macro newlam)))
                            ,@top-stmts))))

                 ;; local case - lift to a new type at top level
                 (let* ((exists (get defined name #f))
                        (type-name  (or (get namemap name #f)
                                        (and name
                                             (symbol (string (if (= (string.char (string name) 0) #\#)
                                                                 ""
                                                                 "#")
                                                             name "#" (current-julia-module-counter))))))
                        (alldefs (expr-find-all
                                  (lambda (ex) (and (length> ex 2) (eq? (car ex) 'method)
                                                    (not (eq? ex e))
                                                    (eq? (method-expr-name ex) name)))
                                  (lam:body lam)
                                  identity
                                  (lambda (x) (and (pair? x) (not (eq? (car x) 'lambda)))))))
                 (and name (put! namemap name type-name))
                 (let* ((all-capt-vars   (delete-duplicates
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
                        (moved-vars  ;; signature-local vars that should be moved to the top-level
                         (if lam2
                             (delete-duplicates
                              (expr-find-all
                               (lambda (s) (and (symbol? s)
                                                (not (eq? name s))
                                                (not (memq s capt-sp))
                                                (if (and (length> (lam:args lam) 0) (local? s))
                                                    ;; local variable used in signature. either move it,
                                                    ;; or give an error if there are uses outside the sig.
                                                    (if (or (expr-contains-p (lambda (v) (eq? v s))
                                                                             (lam:body lam)
                                                                             (lambda (ex) (not (or (eq? ex (caddr e))
                                                                                                   (and (pair? ex)
                                                                                                        (eq? (car ex) 'local))))))
                                                            ;; var must be assigned within the signature block.
                                                            ;; otherwise it might not be assigned at all.
                                                            (not
                                                             (expr-contains-p (lambda (e)
                                                                                (and (pair? e)
                                                                                     (memq (car e) '(= method))
                                                                                     (eq? (cadr e) s)))
                                                                              (caddr e))))
                                                        (if (has? namemap s)
                                                            #f
                                                            (error (string "local variable " s
                                                                           " cannot be used in closure declaration")))
                                                        #t)
                                                    #f)))
                               (caddr e)
                               identity))
                             '()))
                        (find-locals-in-method-sig (lambda (methdef)
                                                     (expr-find-all
                                                      (lambda (s) (and (symbol? s)
                                                                       (not (eq? name s))
                                                                       (not (memq s capt-sp))
                                                                       (memq s (lam:sp lam))))
                                                      (caddr methdef)
                                                      identity)))
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
                                              (filter ssavalue? fieldtypes)))
                                (fieldnames (append closure-param-names (filter (lambda (v) (not (is-var-boxed? v lam))) capt-vars))))
                           (if (null? para)
                               (type-for-closure type-name capt-vars '(core Function))
                               (type-for-closure-parameterized type-name para fieldnames capt-vars fieldtypes '(core Function)))))
                        (mk-method ;; expression to make the method
                         (if short '()
                             (let* ((iskw ;; TODO jb/functions need more robust version of this
                                     (contains (lambda (x) (eq? x 'kwftype)) sig))
                                    (renamemap (map cons closure-param-names closure-param-syms))
                                    (arg-defs (replace-vars
                                               (fix-function-arg-type sig type-name iskw namemap closure-param-syms)
                                               renamemap)))
                               (append (map (lambda (gs tvar)
                                              (make-assignment gs `(call (core TypeVar) ',tvar (core Any))))
                                            closure-param-syms closure-param-names)
                                       `((method #f ,(cl-convert arg-defs fname lam namemap defined toplevel interp)
                                                 ,(convert-lambda lam2
                                                                  (if iskw
                                                                      (caddr (lam:args lam2))
                                                                      (car (lam:args lam2)))
                                                                  #f closure-param-names)))))))
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
                                    closure-param-names
                                    (filter identity (map (lambda (v ve)
                                                            (if (is-var-boxed? v lam)
                                                                #f
                                                                `(call (core typeof) ,ve)))
                                                          capt-vars var-exprs)))))
                           `(new ,(if (null? P)
                                      type-name
                                      `(call (core apply_type) ,type-name ,@P))
                                 ,@var-exprs))))
                   (if (pair? moved-vars)
                       (set-car! (lam:vinfo lam)
                                 (filter (lambda (vi)
                                           (not (memq (car vi) moved-vars)))
                                         (car (lam:vinfo lam)))))
                   (if (or exists (and short (pair? alldefs)))
                       `(toplevel-butfirst
                         (null)
                         ,@sp-inits
                         ,@mk-method)
                       (begin
                         (put! defined name #t)
                         `(toplevel-butfirst
                           ,(convert-assignment name mk-closure fname lam interp)
                           ,@typedef
                           ,@(map (lambda (v) `(moved-local ,v)) moved-vars)
                           ,@sp-inits
                           ,@mk-method))))))))
          ((lambda)  ;; happens inside (thunk ...) and generated function bodies
           (for-each (lambda (vi) (vinfo:set-asgn! vi #t))
                     (list-tail (car (lam:vinfo e)) (length (lam:args e))))
           (let ((body (map-cl-convert (cdr (lam:body e)) 'anon
                                       (lambda-optimize-vars! e)
                                       (table)
                                       (table)
                                       (null? (cadr e)) ;; only toplevel thunks have 0 args
                                       interp)))
             `(lambda ,(cadr e)
                (,(clear-capture-bits (car (lam:vinfo e)))
                 () ,@(cddr (lam:vinfo e)))
                (block ,@body))))
          ;; remaining `::` expressions are type assertions
          ((|::|)
           (cl-convert `(call (core typeassert) ,@(cdr e)) fname lam namemap defined toplevel interp))
          ;; remaining `decl` expressions are only type assertions if the
          ;; argument is global or a non-symbol.
          ((decl)
           (cond ((and (symbol? (cadr e))
                       (local-in? (cadr e) lam))
                  '(null))
                 (else
                  (if (or (symbol? (cadr e)) (and (pair? (cadr e)) (eq? (caadr e) 'outerref)))
                      (error "type declarations on global variables are not yet supported"))
                  (cl-convert `(call (core typeassert) ,@(cdr e)) fname lam namemap defined toplevel interp))))
          ;; `with-static-parameters` expressions can be removed now; used only by analyze-vars
          ((with-static-parameters)
           (cl-convert (cadr e) fname lam namemap defined toplevel interp))
          (else
           (cons (car e)
                 (map-cl-convert (cdr e) fname lam namemap defined toplevel interp))))))))

(define (closure-convert e) (cl-convert e #f #f #f #f #f #f))

;; pass 5: convert to linear IR

(define (linearize e)
  (cond ((or (not (pair? e)) (quoted? e)) e)
        ((eq? (car e) 'lambda)
         (set-car! (cdddr e) (compile-body (cadddr e) (append (car (caddr e))
                                                              (cadr (caddr e)))
                                           e)))
        (else (for-each linearize (cdr e))))
  e)

(define (valid-ir-argument? e)
  (or (simple-atom? e) (symbol? e)
      (and (pair? e)
           (memq (car e) '(quote inert top core globalref outerref
                                 slot static_parameter boundscheck)))))

(define (valid-ir-rvalue? lhs e)
  (or (ssavalue? lhs)
      (valid-ir-argument? e)
      (and (symbol? lhs) (pair? e)
           (memq (car e) '(new splatnew the_exception isdefined call invoke foreigncall cfunction gc_preserve_begin copyast)))))

(define (valid-ir-return? e)
  ;; returning lambda directly is needed for @generated
  (or (valid-ir-argument? e) (and (pair? e) (memq (car e) '(lambda)))))

;; this pass behaves like an interpreter on the given code.
;; to perform stateful operations, it calls `emit` to record that something
;; needs to be done. in value position, it returns an expression computing
;; the needed value. in the future, all intermediate values will have
;; numbered slots (or be simple immediate values), and then those will be the
;; only possible returned values.
(define (compile-body e vi lam)
  (let ((code '())            ;; statements (emitted in reverse order)
        (filename #f)
        (first-line #t)
        (current-loc #f)
        (rett #f)
        (global-const-error #f)
        (arg-map #f)          ;; map arguments to new names if they are assigned
        (label-counter 0)     ;; counter for generating label addresses
        (label-map (table))   ;; maps label names to generated addresses
        (label-nesting (table)) ;; exception handler and catch block nesting of each label
        (finally-handler #f)  ;; `(var label map level)` where `map` is a list of `(tag . action)`.
                              ;; To exit the current finally block, set `var` to integer `tag`,
                              ;; jump to `label`, and put `(tag . action)` in the map, where `action`
                              ;; is `(return x)`, `(break x)`, or a call to rethrow.
        (handler-goto-fixups '())  ;; `goto`s that might need `leave` exprs added
        (handler-level 0)     ;; exception handler nesting depth
        (catch-token-stack '())) ;; tokens identifying handler enter for current catch blocks
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
    (define (leave-finally-block action (need-goto #t))
      (let* ((tags (caddr finally-handler))
             (tag  (if (null? tags) 1 (+ 1 (caar tags)))))
        (set-car! (cddr finally-handler) (cons (cons tag action) tags))
        (emit `(= ,(car finally-handler) ,tag))
        (if need-goto
            (begin (emit `(leave ,(+ 1 (- handler-level (cadddr finally-handler)))))
                   (emit `(goto ,(cadr finally-handler)))))
        tag))
    (define (pop-exc-expr src-tokens dest-tokens)
      (if (eq? src-tokens dest-tokens)
          #f
          (let ((restore-token (let loop ((s src-tokens))
                                 (if (not (pair? s))
                                     (error "Attempt to jump into catch block"))
                                 (if (eq? (cdr s) dest-tokens)
                                     (car s)
                                     (loop (cdr s))))))
            `(pop_exception ,restore-token))))
    (define (emit-return x)
      (define (actually-return x)
        (let* ((x   (if rett
                        (compile (convert-for-type-decl x rett) '() #t #f)
                        x))
               (tmp (if (valid-ir-return? x) #f (make-ssavalue))))
          (if tmp (emit `(= ,tmp ,x)))
          (let ((pexc (pop-exc-expr catch-token-stack '())))
            (if pexc (emit pexc)))
          (emit `(return ,(or tmp x)))))
      (if x
          (if (> handler-level 0)
              (let ((tmp (cond ((and (simple-atom? x) (or (not (ssavalue? x)) (not finally-handler))) #f)
                               (finally-handler  (new-mutable-var))
                               (else             (make-ssavalue)))))
                (if tmp (emit `(= ,tmp ,x)))
                (if finally-handler
                    (leave-finally-block `(return ,(or tmp x)))
                    (begin (emit `(leave ,handler-level))
                           (actually-return (or tmp x))))
                (or tmp x))
              (actually-return x))))
    (define (emit-break labl)
      (let ((lvl (caddr labl))
            (dest-tokens (cadddr labl)))
        (let ((pexc (pop-exc-expr catch-token-stack dest-tokens)))
          (if pexc (emit pexc)))
        (if (and finally-handler (> (cadddr finally-handler) lvl))
            (leave-finally-block `(break ,labl))
            (begin
              (if (> handler-level lvl)
                  (emit `(leave ,(- handler-level lvl))))
              (emit `(goto ,(cadr labl)))))))
    (define (new-mutable-var . name)
      (let ((g (if (null? name) (gensy) (named-gensy (car name)))))
        (set-car! (lam:vinfo lam) (append (car (lam:vinfo lam)) `((,g Any 2))))
        g))
    ;; give an error for misplaced top-level-only expressions
    (define (check-top-level e)
      (define (head-to-text h)
        (case h
          ((abstract_type)  "\"abstract type\"")
          ((primitive_type) "\"primitive type\"")
          ((struct_type)    "\"struct\"")
          ((method)         "method definition")
          (else             (string "\"" h "\""))))
      (if (not (null? (cadr lam)))
          (error (string (head-to-text (car e)) " expression not at top level"))))
    ;; evaluate the arguments of a call, creating temporary locations as needed
    (define (compile-args lst break-labels)
      (if (null? lst) '()
          (let ((simple? (every (lambda (x) (or (simple-atom? x) (symbol? x)
                                                (and (pair? x)
                                                     (memq (car x) '(quote inert top core globalref outerref boundscheck)))))
                                lst)))
            (let loop ((lst  lst)
                       (vals '()))
              (if (null? lst)
                  (reverse! vals)
                  (let* ((arg (car lst))
                         (aval (or (compile arg break-labels #t #f)
                                   ;; TODO: argument exprs that don't yield a value?
                                   '(null))))
                    (loop (cdr lst)
                          (cons (if (and (not simple?)
                                         (not (simple-atom? arg))
                                         (not (simple-atom? aval))
                                         (not (and (pair? arg)
                                                   (memq (car arg) '(quote inert top core globalref outerref boundscheck))))
                                         (not (and (symbol? aval) ;; function args are immutable and always assigned
                                                   (memq aval (lam:args lam))))
                                         (not (and (symbol? arg)
                                                   (or (null? (cdr lst))
                                                       (null? vals)))))
                                    (let ((tmp (make-ssavalue)))
                                      (emit `(= ,tmp ,aval))
                                      tmp)
                                    aval)
                                vals))))))))
    (define (compile-cond ex break-labels)
      (let ((cnd (or (compile ex break-labels #t #f)
                     ;; TODO: condition exprs that don't yield a value?
                     '(null))))
        (if (not (valid-ir-argument? cnd))
            (let ((tmp (make-ssavalue)))
              (emit `(= ,tmp ,cnd))
              tmp)
            cnd)))
    (define (emit-assignment lhs rhs)
      (if rhs
          (if (valid-ir-rvalue? lhs rhs)
              (emit `(= ,lhs ,rhs))
              (let ((rr (make-ssavalue)))
                (emit `(= ,rr ,rhs))
                (emit `(= ,lhs ,rr)))))
      #f)
    ;; the interpreter loop. `break-labels` keeps track of the labels to jump to
    ;; for all currently closing break-blocks.
    ;; `value` means we are in a context where a value is required; a meaningful
    ;; value must be returned.
    ;; `tail` means we are in tail position, where a value needs to be `return`ed
    ;; from the current function.
    (define (compile e break-labels value tail)
      (if (or (not (pair? e)) (memq (car e) '(null true false ssavalue quote inert top core copyast the_exception $
                                                   globalref outerref thismodule cdecl stdcall fastcall thiscall llvmcall)))
          (let ((e1 (if (and arg-map (symbol? e))
                        (get arg-map e e)
                        e)))
            (if (and value (or (underscore-symbol? e)
                               (and (pair? e) (or (eq? (car e) 'outerref)
                                                  (eq? (car e) 'globalref))
                                    (underscore-symbol? (cadr e)))))
                (error (string "all-underscore identifier used as rvalue" (format-loc current-loc))))
            (cond (tail  (emit-return e1))
                  (value e1)
                  ((symbol? e1) (emit e1) #f)  ;; keep symbols for undefined-var checking
                  ((and (pair? e1) (eq? (car e1) 'outerref)) (emit e1) #f)  ;; keep globals for undefined-var checking
                  ((and (pair? e1) (eq? (car e1) 'globalref)) (emit e1) #f) ;; keep globals for undefined-var checking
                  (else #f)))
          (case (car e)
            ((call new splatnew foreigncall cfunction)
             (let* ((args
                     (cond ((eq? (car e) 'foreigncall)
                            ;; NOTE: 2nd to 5th arguments of ccall must be left in place
                            ;;       the 1st should be compiled if an atom.
                            (append (if (or (atom? (cadr e))
                                            (let ((fptr (cadr e)))
                                              (not (and (length> fptr 1)
                                                        (eq? (car fptr) 'call)
                                                        (equal? (cadr fptr) '(core tuple))))))
                                        (compile-args (list (cadr e)) break-labels)
                                        (list (cadr e)))
                                    (list-head (cddr e) 4)
                                    (compile-args (list-tail e 6) break-labels)))
                           ;; NOTE: arguments of cfunction must be left in place
                           ;;       except for argument 2 (fptr)
                           ((eq? (car e) 'cfunction)
                            (let ((fptr (car (compile-args (list (caddr e)) break-labels))))
                              (cons (cadr e) (cons fptr (cdddr e)))))
                           ;; TODO: evaluate first argument to cglobal some other way
                           ((and (length> e 2)
                                 (or (eq? (cadr e) 'cglobal)
                                     (equal? (cadr e) '(outerref cglobal))))
                            (list* (cadr e) (caddr e)
                                   (compile-args (cdddr e) break-labels)))
                           (else
                            (compile-args (cdr e) break-labels))))
                    (callex (cons (car e) args)))
               (cond (tail (emit-return callex))
                     (value callex)
                     (else (emit callex)))))
            ((=)
             (let ((lhs (cadr e)))
               (if (and (symbol? lhs) (underscore-symbol? lhs))
                   (compile (caddr e) break-labels value tail)
                   (let* ((rhs (compile (caddr e) break-labels #t #f))
                          (lhs (if (and arg-map (symbol? lhs))
                                   (get arg-map lhs lhs)
                                   lhs)))
                     (if (and value rhs)
                         (let ((rr (if (or (atom? rhs) (ssavalue? rhs) (eq? (car rhs) 'null))
                                       rhs (make-ssavalue))))
                           (if (not (eq? rr rhs))
                               (emit `(= ,rr ,rhs)))
                           (emit `(= ,lhs ,rr))
                           (if tail (emit-return rr))
                           rr)
                         (emit-assignment lhs rhs))))))
            ((block)
             (let* ((last-fname filename)
                    (fnm        (first-non-meta e))
                    (fname      (if (and (length> e 1) (linenum? fnm)
                                         (length> fnm 2))
                                    (caddr fnm)
                                    filename))
                    (file-diff  (not (eq? fname last-fname)))
                    ;; don't need a filename node for start of function
                    (need-meta  (and file-diff last-fname
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
             #f)
            ((unnecessary)
             ;; `unnecessary` marks expressions generated by lowering that
             ;; do not need to be evaluated if their value is unused.
             (if value
                 (compile (cadr e) break-labels value tail)
                 #f))
            ((if elseif)
             (let ((test `(gotoifnot ,(compile-cond (cadr e) break-labels) _))
                   (end-jump `(goto _))
                   (val (if (and value (not tail)) (new-mutable-var) #f)))
               (emit test)
               (let ((v1 (compile (caddr e) break-labels value tail)))
                 (if val (emit-assignment val v1))
                 (if (and (not tail) (or (length> e 3) val))
                     (emit end-jump))
                 (set-car! (cddr test) (make&mark-label))
                 (let ((v2 (if (length> e 3)
                               (compile (cadddr e) break-labels value tail)
                               '(null))))
                   (if val (emit-assignment val v2))
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
               (mark-label endl))
             (if value (compile '(null) break-labels value tail)))
            ((_do_while)
             (let* ((endl (make-label))
                    (topl (make&mark-label)))
               (compile (cadr e) break-labels #f #f)
               (let ((test (compile-cond (caddr e) break-labels)))
                 (emit `(gotoifnot ,test ,endl)))
               (emit `(goto ,topl))
               (mark-label endl))
             (if value (compile '(null) break-labels value tail)))
            ((break-block)
             (let ((endl (make-label)))
               (compile (caddr e)
                        (cons (list (cadr e) endl handler-level catch-token-stack)
                              break-labels)
                        #f #f)
               (mark-label endl))
             (if value (compile '(null) break-labels value tail)))
            ((break)
             (let ((labl (assq (cadr e) break-labels)))
               (if (not labl)
                   (error "break or continue outside loop")
                   (emit-break labl))))
            ((label symboliclabel)
             (if (eq? (car e) 'symboliclabel)
                 (if (has? label-nesting (cadr e))
                     (error (string "label \"" (cadr e) "\" defined multiple times"))
                     (put! label-nesting (cadr e) (list handler-level catch-token-stack))))
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
                     (cons (list code handler-level catch-token-stack (cadr e)) handler-goto-fixups))
               #f))

            ;; exception handlers are lowered using
            ;; (= tok (enter L)) - push handler with catch block at label L, yielding token
            ;; (leave n) - pop N exception handlers
            ;; (pop_exception tok) - pop exception stack back to state of associated enter
            ((trycatch tryfinally)
             (let ((handler-token (make-ssavalue))
                   (catch (make-label))
                   (endl  (make-label))
                   (last-finally-handler finally-handler)
                   (finally           (if (eq? (car e) 'tryfinally) (new-mutable-var) #f))
                   (my-finally-handler #f))
               ;; handler block entry
               (emit `(= ,handler-token (enter ,catch)))
               (set! handler-level (+ handler-level 1))
               (if finally (begin (set! my-finally-handler (list finally endl '() handler-level))
                                  (set! finally-handler my-finally-handler)
                                  (emit `(= ,finally -1))))
               (let* ((v1  (compile (cadr e) break-labels value #f)) ;; emit try block code
                      (val (if (and value (not tail))
                               (new-mutable-var) #f)))
                 ;; handler block postfix
                 (if (and val v1) (emit-assignment val v1))
                 (if tail
                     (begin (if v1 (emit-return v1))
                            (if (not finally) (set! endl #f)))
                     (begin (emit '(leave 1))
                            (emit `(goto ,endl))))
                 (set! handler-level (- handler-level 1))
                 ;; emit either catch or finally block
                 (mark-label catch)
                 (emit `(leave 1))
                 (if finally
                     (begin (leave-finally-block '(call (top rethrow)) #f)
                            (if endl (mark-label endl))
                            (set! finally-handler last-finally-handler)
                            (compile (caddr e) break-labels #f #f)
                            ;; emit actions to be taken at exit of finally block
                            (let loop ((actions (caddr my-finally-handler)))
                              (if (pair? actions)
                                  (let ((skip (if (and tail (null? (cdr actions))
                                                       (eq? (car (cdar actions)) 'return))
                                                  #f
                                                  (make-label))))
                                    (if skip
                                        (let ((tmp (make-ssavalue)))
                                          (emit `(= ,tmp (call (core ===) ,finally ,(caar actions))))
                                          (emit `(gotoifnot ,tmp ,skip))))
                                    (let ((ac (cdar actions)))
                                      (cond ((eq? (car ac) 'return) (emit-return (cadr ac)))
                                            ((eq? (car ac) 'break)  (emit-break (cadr ac)))
                                            (else ;; assumed to be a rethrow
                                             (emit ac))))
                                    (if skip (mark-label skip))
                                    (loop (cdr actions))))))
                     (begin (set! catch-token-stack (cons handler-token catch-token-stack))
                            (let ((v2 (compile (caddr e) break-labels value tail)))
                              (if val (emit-assignment val v2))
                              (if (not tail) (emit `(pop_exception ,handler-token)))
                                             ;; else done in emit-return from compile
                              (if endl (mark-label endl)))
                            (set! catch-token-stack (cdr catch-token-stack))))
                 val)))

            ((newvar)
             ;; avoid duplicate newvar nodes
             (if (and (not (and (pair? code) (equal? (car code) e)))
                      ;; exclude deleted vars
                      (assq (cadr e) (car (lam:vinfo lam))))
                 (emit e)
                 #f))
            ((global) ; keep global declarations as statements
             (if value (error "misplaced \"global\" declaration"))
             (emit e))
            ((local-def) #f)
            ((local) #f)
            ((moved-local)
             (set-car! (lam:vinfo lam) (append (car (lam:vinfo lam)) `((,(cadr e) Any 2))))
             #f)
            ((const)
             (if (local-in? (cadr e) lam)
                 (error (string "unsupported `const` declaration on local variable" (format-loc current-loc)))
                 (if (pair? (cadr lam))
                     ;; delay this error to allow "misplaced struct" errors to happen first
                     (if (not global-const-error)
                         (set! global-const-error current-loc))
                     (emit e))))
            ((isdefined) (if tail (emit-return e) e))
            ((boundscheck) (if tail (emit-return e) e))

            ((method)
             (if (not (null? (cadr lam)))
                 (error (string "Global method definition" (linenode-string current-loc)
                                " needs to be placed at the top level, or use \"eval\".")))
             (if (length> e 2)
                 (let* ((sig (let ((sig (compile (caddr e) break-labels #t #f)))
                               (if (valid-ir-argument? sig)
                                   sig
                                   (let ((l (make-ssavalue)))
                                     (emit `(= ,l ,sig))
                                     l))))
                        (lam (cadddr e))
                        (lam (if (and (pair? lam) (eq? (car lam) 'lambda))
                                 (linearize lam)
                                 (let ((l  (make-ssavalue)))
                                   (emit `(= ,l ,(compile lam break-labels #t #f)))
                                   l))))
                   (emit `(method ,(or (cadr e) '(false)) ,sig ,lam))
                   (if value (compile '(null) break-labels value tail)))
                 (cond (tail  (emit-return e))
                       (value e)
                       (else  (emit e)))))
            ((lambda)
             (let ((temp (linearize e)))
               (cond (tail  (emit-return temp))
                     (value temp)
                     (else  (emit temp)))))

            ;; top level expressions
            ((thunk module)
             (check-top-level e)
             (emit e)
             (if tail (emit-return '(null)))
             '(null))
            ((toplevel-only)
             (check-top-level (cdr e))
             '(null))

            ((toplevel)
             (check-top-level e)
             (let ((val (make-ssavalue)))
               (emit `(= ,val ,e))
               (if tail (emit-return val))
               val))

            ;; other top level expressions
            ((import using export)
             (check-top-level e)
             (emit e)
             (let ((have-ret? (and (pair? code) (pair? (car code)) (eq? (caar code) 'return))))
               (if (and tail (not have-ret?))
                   (emit-return '(null))))
             '(null))

            ((gc_preserve_begin)
             (let ((args (compile-args (cdr e) break-labels)))
               (cons (car e) args)))

            ;; metadata expressions
            ((line meta inbounds loopinfo gc_preserve_end aliasscope popaliasscope)
             (let ((have-ret? (and (pair? code) (pair? (car code)) (eq? (caar code) 'return))))
               (cond ((eq? (car e) 'line)
                      (set! current-loc e)
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

            ;; unsupported assignment operators
            ((≔ ⩴ ≕ :=)
             (error (string "unsupported assignment operator \"" (deparse (car e)) "\"")))

            ((error)
             (error (cadr e)))
            (else
             (error (string "invalid syntax " (deparse e)))))))
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
                      (src-tokens (caddr x))
                      (lab   (cadddr x)))
                  (let ((target-nesting (get label-nesting lab #f)))
                    (if (not target-nesting)
                        (error (string "label \"" lab "\" referenced but not defined")))
                    (let ((target-level (car target-nesting)))
                      (cond ((> target-level hl)
                            (error (string "cannot goto label \"" lab "\" inside try/catch block")))
                            ((= target-level hl)
                             (set-cdr! point (cddr point))) ;; remove empty slot
                            (else
                             (set-car! (cdr point) `(leave ,(- hl target-level))))))
                    (let ((pexc (pop-exc-expr src-tokens (cadr target-nesting))))
                      (if pexc (set-cdr! point (cons pexc (cdr point))))))))
              handler-goto-fixups)
    (if global-const-error
        (error (string "`global const` declaration not allowed inside function" (format-loc global-const-error))))
    (let* ((stmts (reverse! code))
           (di    (definitely-initialized-vars stmts vi))
           (body  (cons 'block (filter (lambda (e)
                                         (not (and (pair? e) (eq? (car e) 'newvar)
                                                   (has? di (cadr e)))))
                                       stmts))))
      (if arg-map
          (insert-after-meta
           body
           (table.foldl (lambda (k v lst) (cons `(= ,v ,k) lst))
                        '() arg-map))
          body))))

(define (for-each-isdefined f e)
  (cond ((or (atom? e) (quoted? e)) #f)
        ((and (pair? e) (eq? (car e) 'isdefined))
         (f (cadr e)))
        (else
         (for-each (lambda (x) (for-each-isdefined f x))
                   (cdr e)))))

;; Find newvar nodes that are unnecessary because (1) the variable is not
;; captured, and (2) the variable is assigned before any branches.
;; This is used to remove newvar nodes that are not needed for re-initializing
;; variables to undefined (see issue #11065).
;; It doesn't look for variable *uses*, because any variables used-before-def
;; that also pass this test are *always* used undefined, and therefore don't need
;; to be *re*-initialized.
;; The one exception to that is `@isdefined`, which can observe an undefined
;; variable without throwing an error.
(define (definitely-initialized-vars stmts vi)
  (let ((vars (table))
        (di   (table)))
    (let loop ((stmts stmts))
      (if (null? stmts)
          di
          (begin
            (let ((e (car stmts)))
              (for-each-isdefined (lambda (x) (if (has? vars x) (del! vars x)))
                                  e)
              (cond ((and (pair? e) (eq? (car e) 'newvar))
                     (let ((vinf (var-info-for (cadr e) vi)))
                       (if (and vinf (not (vinfo:capt vinf)))
                           (put! vars (cadr e) #t))))
                    ((and (pair? e) (or (memq (car e) '(goto gotoifnot))
                                        (and (eq? (car e) '=) (pair? (caddr e))
                                             (eq? (car (caddr e)) 'enter))))
                     (set! vars (table)))
                    ((and (pair? e) (eq? (car e) '=))
                     (if (has? vars (cadr e))
                         (begin (del! vars (cadr e))
                                (put! di (cadr e) #t))))))
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

(define (compact-ir body file line)
  (let ((code         '(block))
        (locs         '(list))
        (linetable    '(list))
        (labltable    (table))
        (ssavtable    (table))
        (reachable    #t)
        (current-loc  0)
        (current-file file)
        (current-line line)
        (locstack     '())
        (i            1))
    (define (emit e)
      (if (and (null? (cdr linetable))
               (not (and (pair? e) (eq? (car e) 'meta))))
          (begin (set! linetable (cons `(line ,line ,file) linetable))
                 (set! current-loc 1)))
      (if (or reachable
              (and (pair? e) (memq (car e) '(meta inbounds gc_preserve_begin gc_preserve_end aliasscope popaliasscope))))
          (begin (set! code (cons e code))
                 (set! i (+ i 1))
                 (set! locs (cons current-loc locs)))))
    (let loop ((stmts (cdr body)))
      (if (pair? stmts)
          (let ((e (car stmts)))
            (cond ((atom? e) (emit e))
                  ((eq? (car e) 'line)
                   (if (and (= current-line 0) (length= e 2) (pair? linetable))
                       ;; (line n) after push_loc just updates the line for the new file
                       (begin (set-car! (cdr (car linetable)) (cadr e))
                              (set! current-line (cadr e)))
                       (begin
                         (set! current-line (cadr e))
                         (if (pair? (cddr e))
                             (set! current-file (caddr e)))
                         (set! linetable (cons (if (null? locstack)
                                                   `(line ,current-line ,current-file)
                                                   `(line ,current-line ,current-file ,(caar locstack)))
                                               linetable))
                         (set! current-loc (- (length linetable) 1)))))
                  ((and (length> e 2) (eq? (car e) 'meta) (eq? (cadr e) 'push_loc))
                   (set! locstack (cons (list current-loc current-line current-file) locstack))
                   (set! current-file (caddr e))
                   (set! current-line 0)
                   (set! linetable (cons `(line ,current-line ,current-file ,current-loc) linetable))
                   (set! current-loc (- (length linetable) 1)))
                  ((and (length= e 2) (eq? (car e) 'meta) (eq? (cadr e) 'pop_loc))
                   (let ((l (car locstack)))
                     (set! locstack (cdr locstack))
                     (set! current-loc (car l))
                     (set! current-line (cadr l))
                     (set! current-file (caddr l))))
                  ((eq? (car e) 'label)
                   (set! reachable #t)
                   (put! labltable (cadr e) i))
                  ((and (assignment? e) (ssavalue? (cadr e)))
                   (let ((idx (and (ssavalue? (caddr e)) (get ssavtable (cadr (caddr e)) #f))))
                     ;; if both lhs and rhs are ssavalues, merge them
                     (if idx
                         (put! ssavtable (cadr (cadr e)) idx)
                         (begin
                           (put! ssavtable (cadr (cadr e)) i)
                           (emit (caddr e))))))
                  (else
                   (emit e)
                   (if (or (eq? (car e) 'goto) (eq? (car e) 'return))
                       (set! reachable #f))))
            (loop (cdr stmts)))))
    (vector (reverse code) (reverse locs) (reverse linetable) ssavtable labltable)))

(define (symbol-to-idx-map lst)
  (let ((tbl (table)))
    (let loop ((xs lst) (i 1))
      (if (pair? xs)
          (begin (put! tbl (car xs) i)
                 (loop (cdr xs) (+ i 1)))))
    tbl))

(define (renumber-lambda lam file line)
  (let* ((stuff (compact-ir (lam:body lam) file line))
         (code (aref stuff 0))
         (locs (aref stuff 1))
         (linetab (aref stuff 2))
         (ssavalue-table (aref stuff 3))
         (label-table (aref stuff 4)))
    (set-car! (cdddr lam) code)
    (define nslots (length (car (lam:vinfo lam))))
    (define slot-table (symbol-to-idx-map (map car (car (lam:vinfo lam)))))
    (define sp-table (symbol-to-idx-map (lam:sp lam)))
    (define (renumber-stuff e)
      (cond ((symbol? e)
             (let ((idx (get slot-table e #f)))
               (if idx
                   `(slot ,idx)
                   (let ((idx (get sp-table e #f)))
                     (if idx
                         `(static_parameter ,idx)
                         e)))))
            ((and (pair? e) (eq? (car e) 'outerref))
             (cadr e))
            ((nospecialize-meta? e)
             ;; convert nospecialize vars to slot numbers
             `(meta ,(cadr e) ,@(map renumber-stuff (cddr e))))
            ((or (atom? e) (quoted? e) (eq? (car e) 'global))
             e)
            ((ssavalue? e)
             (let ((idx (or (get ssavalue-table (cadr e) #f)
                            (error "ssavalue with no def"))))
               `(ssavalue ,idx)))
            ((memq (car e) '(goto enter))
             (list* (car e) (get label-table (cadr e)) (cddr e)))
            ((eq? (car e) 'gotoifnot)
             `(gotoifnot ,(renumber-stuff (cadr e)) ,(get label-table (caddr e))))
            ((eq? (car e) 'lambda)
             (renumber-lambda e 'none 0))
            (else (cons (car e)
                        (map renumber-stuff (cdr e))))))
    (let ((body (renumber-stuff (lam:body lam)))
          (vi   (lam:vinfo lam)))
      (listify-lambda
       `(lambda ,(cadr lam)
          (,(car vi) ,(cadr vi) ,(length (cdr body)) ,(last vi))
          ,body
          ,locs
          ,linetab)))))

(define (compact-and-renumber ex file line)
  (if (atom? ex) ex
      (if (eq? (car ex) 'lambda)
          (renumber-lambda ex
                           (if (null? (cadr ex)) file 'none)
                           (if (null? (cadr ex)) line 0))
          (cons (car ex)
                (map (lambda (e) (compact-and-renumber e file line))
                     (cdr ex))))))

;; expander entry point

(define (julia-expand1 ex file line)
  (compact-and-renumber
   (linearize
    (closure-convert
     (analyze-variables!
      (resolve-scopes ex)))) file line))

(define *current-desugar-loc* #f)

(define (julia-expand0 ex file line)
  (with-bindings ((*current-desugar-loc* `(line ,line ,file)))
   (trycatch (expand-forms ex)
             (lambda (e)
               (if (and (pair? e) (eq? (car e) 'error))
                   ; Add location for desugaring errors. This is approximate:
                   ; - Line number nodes are sparse in the AST
                   ; - Line number nodes apply to the next statement, so
                   ;   tracking by `set!`ing *current-desugar-loc* relies on
                   ;   AST traversal being in program order.
                   (error (string (cadr e) (format-loc *current-desugar-loc*))))
                   (raise e)))))

(define (julia-expand ex (file 'none) (line 0))
  (julia-expand1
   (julia-expand0
    (julia-expand-macroscope ex) file line) file line))
