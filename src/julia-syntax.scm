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
  (or (not (pair? e)) (jlgensym? e) (sym-dot? e) (quoted? e) (equal? e '(null))))

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
                  (let ((g (make-jlgensym)))
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
    `(lambda ,argl ()
             (scope-block ,body))))

;; convert list of names (sl) and list of upper bounds to expressions that
;; construct TypeVars
(define (symbols->typevars sl upperbounds bnd)
  (let ((bnd (if bnd '(true) '())))
    (if (null? upperbounds)
        (map (lambda (x)    `(call (top TypeVar) ',x ,@bnd)) sl)
        (map (lambda (x ub) `(call (top TypeVar) ',x ,ub ,@bnd)) sl upperbounds))))

;; extract type variable name from A<:B expressions
(define (sparam-name sp)
  (cond ((symbol? sp) sp)
        ((and (length= sp 3)
              (eq? (car sp) '|<:|)
              (symbol? (cadr sp)))
         (cadr sp))
        (else (error "malformed type parameter list"))))

;; return (values names bounds) for a series of type var expressions (A<:B)
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

(define (method-expr-name m) (cadr m))

;; extract static parameter names from a (method ...) expression
(define (method-expr-static-parameters m)
  (if (eq? (car (caddr m)) 'block)
      (let ((lst '()))
        (pattern-replace
         (pattern-set
          (pattern-lambda (= v (call (top (-/ TypeVar)) (quote T) y z))
                          (begin (set! lst (cons T lst)) __)))
         (butlast (cdr (caddr m))))
        (reverse! lst))
      '()))

;; expressions of the form a.b.c... where everything is a symbol
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

;; construct the (method ...) expression for one primitive method definition,
;; assuming optional and keyword args are already handled
(define (method-def-expr- name sparams argl body isstaged)
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
                                   (append req opt vararg))))))
   ;; no optional positional args
   (receive
    (names bounds) (sparam-name-bounds sparams '() '())
    (begin
      (let ((anames (llist-vars argl)))
        (if (has-dups (filter (lambda (x) (not (eq? x UNUSED))) anames))
            (error "function argument names not unique"))
        (if (has-dups names)
            (error "function static parameter names not unique"))
        (if (any (lambda (x) (and (not (eq? x UNUSED)) (memq x names))) anames)
            (error "function argument and static parameter names must be distinct")))
      (if (and name (not (sym-ref? name)))
          (error (string "invalid method name \"" (deparse name) "\"")))
      (let* ((iscall (is-call-name? name))
             (name  (if iscall #f name))
             (types (llist-types argl))
             (body  (method-lambda-expr argl body))
             ;; HACK: the typevars need to be bound to jlgensyms, since this code
             ;; might be moved to a different scope by closure-convert.
             (temps (map (lambda (x) (make-jlgensym)) names))
             (renames (map cons names temps))
             (mdef
              (if (null? sparams)
                  `(method ,name (call (top svec) (curly Tuple ,@(dots->vararg types)) (call (top svec)))
                           ,body ,isstaged)
                  `(method ,name
                           (block
                            ,@(map make-assignment temps (symbols->typevars names bounds #t))
                            (call (top svec) (curly Tuple
                                                    ,@(dots->vararg
                                                       (map (lambda (ty)
                                                              (replace-vars ty renames))
                                                            types)))
                                  (call (top svec) ,@temps)))
                           ,body ,isstaged))))
        (if (and iscall (not (null? argl)))
            (let* ((n (arg-name (car argl)))
                   (n (if (hidden-name? n) "" n))
                   (t (deparse (arg-type (car argl)))))
              (syntax-deprecation #f
                                  (string "call(" n "::" t ", ...)")
                                  (string "(" n "::" t ")(...)"))))
        (if (symbol? name)
            `(block (method ,name) ,mdef ,name)  ;; return the function
            mdef))))))

;; keyword default values that can be assigned right away. however, this creates
;; a quasi-bug (part of issue #9535) where it can be hard to predict when a
;; keyword argument will throw an UndefVarError.
(define (const-default? x)
  (or (number? x) (string? x) (char? x) (and (pair? x) (memq (car x) '(quote inert)))
      (eq? x 'true) (eq? x 'false)))

(define (keywords-method-def-expr name sparams argl body isstaged)
  (let* ((kargl (cdar argl))  ;; keyword expressions (= k v)
         (pargl (cdr argl))   ;; positional args
         (body  (if (and (pair? body) (eq? (car body) 'block))
                    body
                    `(block ,body)))
         (ftype (decl-type (car pargl)))
         ;; 1-element list of vararg argument, or empty if none
         (vararg (let ((l (if (null? pargl) '() (last pargl))))
                   (if (vararg? l)
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
          (mangled (symbol (string "#" (if name (undot-name name) 'call) "#"
                                   (string (current-julia-module-counter)))))
          (flags (map (lambda (x) (gensy)) vals)))
      `(block
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

        ;; call with keyword args pre-sorted - original method code goes here
        ,(method-def-expr-
          mangled sparams
          `((|::| ,mangled (call (top typeof) ,mangled)) ,@vars ,@restkw
            ;; strip type off function self argument if not needed for a static param.
            ;; then it is ok for cl-convert to move this definition above the original def.
            ,(if (decl? (car not-optional))
                 (if (any (lambda (sp)
                            (expr-contains-eq (sparam-name sp) (caddr (car not-optional))))
                          positional-sparams)
                     (car not-optional)
                     (decl-var (car not-optional)))
                 (car not-optional))
            ,@(cdr not-optional) ,@vararg)
          `(block
            ,@(if (null? lno) '()
                  ;; TODO jb/functions get a better `name` for functions specified by type
                  (list (append (car lno) (list (undot-name name)))))
            ,@stmts) isstaged)

        ;; call with unsorted keyword args. this sorts and re-dispatches.
        ,(method-def-expr-
          name
          (filter ;; remove sparams that don't occur, to avoid printing the warning twice
           (lambda (s) (let ((name (if (symbol? s) s (cadr s))))
                         (expr-contains-eq name (cons 'list argl))))
           positional-sparams)
          `((|::|
             ;; if there are optional positional args, we need to be able to reference the function name
             ,(if (any kwarg? pargl) (gensy) UNUSED)
             (call (|.| Core 'kwftype) ,ftype)) (:: ,kw (top Array)) ,@pargl ,@vararg)
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
        ,(if (or (not (symbol? name)) (is-call-name? name))
             '(null) name)))))

(define (optional-positional-defs name sparams req opt dfl body isstaged overall-argl)
  ;; prologue includes line number node and eventual meta nodes
  (let ((prologue (if (pair? body)
                      (take-while (lambda (e)
                                    (and (pair? e) (or (eq? (car e) 'line) (eq? (car e) 'meta))))
                                  (cdr body))
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
                             ,@prologue
                             (call ,(arg-name (car req)) ,@(map arg-name (cdr passed)) ,(car vals)))
                           ;; otherwise add all
                           `(block
                             ,@prologue
                             (call ,(arg-name (car req)) ,@(map arg-name (cdr passed)) ,@vals)))))
                 (method-def-expr- name sp passed body #f)))
             (iota (length opt)))
      ,(method-def-expr- name sparams overall-argl body isstaged))))

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
(define (method-def-expr name sparams argl body isstaged)
  (let ((argl (remove-empty-parameters argl)))
    (if (has-parameters? argl)
        ;; has keywords
        (begin (check-kw-args (cdar argl))
               (keywords-method-def-expr name sparams argl body isstaged))
        ;; no keywords
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
                            ,@(map (lambda (p b) `(<: ,p ,b))
                                   params bounds))
                     ,@(map make-decl field-names field-types))
               (block
                ,@locs
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
               (let ((tn (make-jlgensym)))
                 `(block
                   (= ,tn ,Texpr)
                   (new ,tn ,@(map (lambda (fld val)
                                     `(call (top convert)
                                            (call (top fieldtype) ,tn (quote ,fld))
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
          (cons `(call (|::| (curly Type ,name)) ,@sig)
                params)
          (cons `(call (curly (|::| (curly Type ,name)) ,@method-params) ,@sig)
                params))
      (if (null? method-params)
          (cons `(call (curly (|::| (curly Type (curly ,name ,@params)))
                              ,@(map (lambda (p b) `(<: ,p ,b)) params bounds))
                       ,@sig)
                params)
          ;; rename parameters that conflict with user-written method parameters
          (let ((new-params (map (lambda (p) (if (memq p method-params)
                                                 (gensy)
                                                 p))
                                 params)))
            (cons `(call (curly (|::| (curly Type (curly ,name ,@new-params)))
                                ,@(map (lambda (p b) `(<: ,p ,b)) new-params bounds)
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
(define (rewrite-ctor ctor Tname params bounds field-names field-types mutabl)
  (define (ctor-body body type-params)
    (pattern-replace (pattern-set
                      (pattern-lambda
                       (call (-/ new) . args)
                       (new-call Tname type-params params
                                 (map (lambda (a) (ctor-body a type-params)) args)
                                 field-names field-types mutabl))
                      (pattern-lambda
                       (call (curly (-/ new) . p) . args)
                       (new-call Tname p params
                                 (map (lambda (a) (ctor-body a type-params)) args)
                                 field-names field-types mutabl)))
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
         ,@(map make-assignment params (symbols->typevars params bounds #t))
         (composite_type ,name (call (top svec) ,@params)
                         (call (top svec) ,@(map (lambda (x) `',x) field-names))
                         ,super (call (top svec) ,@field-types) ,mut ,min-initialized)))
       ;; "inner" constructors
       (scope-block
        (block
         (global ,name)
         ,@(map (lambda (c)
                  (rewrite-ctor c name params bounds field-names field-types mut))
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
   (params bounds) (sparam-name-bounds params '() '())
   `(block
     (const ,name)
     (scope-block
      (block
       ,@(map (lambda (v) `(local ,v)) params)
       ,@(map make-assignment params (symbols->typevars params bounds #f))
       (abstract_type ,name (call (top svec) ,@params) ,super))))))

(define (bits-def-expr n name params super)
  (receive
   (params bounds) (sparam-name-bounds params '() '())
   `(block
     (const ,name)
     (scope-block
      (block
       ,@(map (lambda (v) `(local ,v)) params)
       ,@(map make-assignment params (symbols->typevars params bounds #f))
       (bits_type ,name (call (top svec) ,@params) ,n ,super))))))

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

(define (expand-function-def e)   ;; handle function or stagedfunction
  (let ((name (cadr e)))
    (cond ((and (length= e 2) (symbol? name))  `(method ,name))
          ((not (pair? name))                  e)
          ((eq? (car name) 'tuple)
           (expand-forms `(-> ,name ,(caddr e))))
          ((eq? (car name) 'call)
           (let* ((head    (cadr name))
                  (argl    (cddr name))
                  (has-sp  (and (pair? head) (eq? (car head) 'curly)))
                  (name    (if has-sp (cadr head) head))
                  (sparams (if has-sp (cddr head) '()))
                  (isstaged (eq? (car e) 'stagedfunction))
                  (adj-decl (lambda (n) (if (and (decl? n) (length= n 2))
                                            `(|::| |#self#| ,(cadr n))
                                            n)))
                  ;; fill in first (closure) argument
                  (farg    (if (decl? name)
                               (adj-decl name)
                               `(|::| |#self#| (call (|.| Core 'Typeof) ,name))))
                  (argl    (fix-arglist
                            (if (and (not (decl? name)) (eq? (undot-name name) 'call))
                                (cons (adj-decl (car argl)) (cdr argl))
                                (arglist-unshift argl farg))
                            (and (not (any kwarg? argl)) (not (and (pair? argl)
                                                                   (pair? (car argl))
                                                                   (eq? (caar argl) 'parameters))))))
                  (name    (if (decl? name) #f name)))
             (expand-forms
              (method-def-expr name sparams argl (caddr e) isstaged))))
          (else e))))

;; handle ( )->( ) function expressions. blocks `(a;b=1)` on the left need to be
;; converted to argument lists with kwargs.
(define (expand-arrow e)
  (let ((a    (cadr e))
        (body (caddr e)))
    (let ((argl (if (pair? a)
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
               (function (call ,name ,@argl) ,body))))))

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
                           (local ,name)
                           (newvar ,name)
                           ,asgn
                           ,blk)))))
               ;; (a, b, c, ...) = rhs
               ((and (pair? (cadar binds))
                     (eq? (caadar binds) 'tuple))
                (let ((vars (lhs-vars (cadar binds))))
                  (loop (cdr binds)
                        `(scope-block
                          (block
                           ,@(map (lambda (v) `(local ,v)) vars)
                           ,@(map (lambda (v) `(newvar ,(decl-var v))) vars)
                           ,(car binds)
                           ,blk)))))
               (else (error "invalid let syntax"))))
             (else (error "invalid let syntax")))))))))

(define (expand-macro-def e)
  (cond ((and (pair? (cadr e))
              (eq? (car (cadr e)) 'call)
              (symbol? (cadr (cadr e))))
         (let ((anames (cddr (cadr e))))
           (expand-forms
            `(function (call ,(symbol (string #\@ (cadr (cadr e))))
                             ,@(map (lambda (v)
                                      (if (symbol? v)
                                          `(|::| ,v (top ANY))
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
                              (and catchb
                                   (not (block-returns? catchb))))
                          (gensy)))
                (retval (if hasret (gensy) #f))
                (bb  (gensy))
                (finally-exception (gensy))
                (val (gensy))) ;; this is jlgensym, but llvm has trouble determining that it dominates all uses
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
                           ,(if catchb
                                `(try ,tryb ,var ,catchb)
                                tryb))
                        #f
                        (= ,err true)))
                  (= ,finally-exception (the_exception))
                  ,finalb
                  (if ,err (ccall 'jl_rethrow_other Void (tuple Any) ,finally-exception))
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
             (if (symbol-like? var)
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
         (params bounds) (sparam-name-bounds params '() '())
         `(block
           (const ,name)
           (= ,name
              (scope-block
               (block
                ,@(map (lambda (v) `(local ,v)) params)
                ,@(map (lambda (l r) (make-assignment l (expand-forms r)))
                       params
                       (symbols->typevars params bounds #t))
                (call (top TypeConstructor)
                      (call (top svec) ,@params)
                      ,(expand-forms type-ex))))))))
      (expand-forms
       `(const (= ,(cadr e) ,(caddr e))))))

;; take apart e.g. `const a::Int = 0` into `const a; a::Int = 0`
(define (expand-const-decl e)
  (if (atom? (cadr e))
      e
      (case (car (cadr e))
        ((global local)
         (expand-forms
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
               ,(expand-forms `(= ,lhs ,rhs))))))
        (else e))))

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

(define (expand-local-or-global-decl e)
  (if (and (symbol? (cadr e)) (length= e 2))
      e
      (expand-forms (expand-decls (car e) (cdr e)))))

(define (assigned-name e)
  (if (and (pair? e) (memq (car e) '(call curly)))
      (assigned-name (cadr e))
      e))

;; local x, y=2, z => local x;local y;local z;y = 2
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
                (tuple ,@(reverse elts)))
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

;; make an expression safe for multiple evaluation
;; for example a[f(x)] => (temp=f(x); a[temp])
;; retuns a pair (expr . assignments)
;; where 'assignments' is a list of needed assignment statements
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

;; lower function call containing keyword arguments
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
         `(call (call (top kwfunc) ,f) (cell1d ,@keyargs) ,f ,@pa)
         (let ((container (make-jlgensym)))
           `(block
             (= ,container (cell1d ,@keyargs))
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
             ,(if (not (null? keys))
                  `(call (call (top kwfunc) ,f) ,container ,f ,@pa)
                  (let* ((expr_stmts (remove-argument-side-effects `(call ,f ,@pa)))
                         (pa         (cddr (car expr_stmts)))
                         (stmts      (cdr expr_stmts)))
                    `(block
                      ,@stmts
                      (if (call (top isempty) ,container)
                          (call ,f ,@pa)
                          (call (call (top kwfunc) ,f) ,container ,f ,@pa)))))))))))

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
(define (expand-update-operator- op lhs rhs declT)
  (let ((e (remove-argument-side-effects lhs)))
    `(block ,@(cdr e)
            ,(if (null? declT)
                 `(= ,(car e) (call ,op ,(car e) ,rhs))
                 `(= ,(car e) (call ,op (:: ,(car e) ,(car declT)) ,rhs))))))

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

(define (lower-update-op e)
  (expand-forms
   (expand-update-operator
    (let ((str (string (car e))))
      (symbol (string.sub str 0 (- (length str) 1))))
    (cadr e)
    (caddr e))))

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
                  (let ((g (make-jlgensym)))
                    `(block (= ,g ,(car tail))
                            (if ,g ,g
                                ,(loop (cdr tail)))))))))))

(define (expand-forms e)
  (if (or (atom? e) (memq (car e) '(quote inert top line module toplevel jlgensym null meta)))
      e
      (let ((ex (get expand-table (car e) #f)))
        (if ex
            (ex e)
            (cons (car e)
                  (map expand-forms (cdr e)))))))

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

;; convert an operator parsed as (op a b) to (call op a b)
(define (syntactic-op-to-call e)
  `(call ,(car e) ,(expand-forms (cadr e)) ,(expand-forms (caddr e))))

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
     (let ((e (flatten-blocks e)))
       (cond ((null? (cdr e)) '(null))
             ((null? (cddr e)) (expand-forms (cadr e)))
             (else
              `(block
                ,.(map (lambda (x)
                         (if (decl? x)
                             `(decl ,@(map expand-forms (cdr x)))
                             (expand-forms x)))
                       (butlast (cdr e)))
                ,(expand-forms (last e)))))))

   '|.|
   (lambda (e)
     `(call (top getfield) ,(expand-forms (cadr e)) ,(expand-forms (caddr e))))

   '|<:| syntactic-op-to-call
   '|>:| syntactic-op-to-call

   'const  expand-const-decl
   'local  expand-local-or-global-decl
   'global expand-local-or-global-decl

   '=
   (lambda (e)
     (define lhs (cadr e))
     (cond
      ((and (pair? lhs)
            (eq? (car lhs) 'call))
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
             (let ((rr (if (symbol-like? rhs) rhs (make-jlgensym))))
               (expand-forms
                `(block ,.(if (eq? rr rhs) '() `((= ,rr ,rhs)))
                        ,@(map (lambda (l) `(= ,l ,rr))
                               lhss)
                        ,rr))))))
      ((symbol-like? lhs)
       `(= ,(cadr e) ,(expand-forms (caddr e))))
      ((atom? lhs)
       (error (string "invalid assignment location \"" (deparse lhs) "\"")))
      (else
       (case (car lhs)
         ((|.|)
          ;; a.b =
          (let ((a   (cadr (cadr e)))
                (b   (caddr (cadr e)))
                (rhs (caddr e)))
            (let ((aa (if (symbol-like? a) a (make-jlgensym)))
                  (bb (if (or (atom? b) (symbol-like? b) (and (pair? b) (quoted? b)))
                          b (make-jlgensym)))
                  (rr (if (or (symbol-like? rhs) (atom? rhs)) rhs (make-jlgensym))))
              `(block
                ,.(if (eq? aa a)   '() `((= ,aa ,(expand-forms a))))
                ,.(if (eq? bb b)   '() `((= ,bb ,(expand-forms b))))
                ,.(if (eq? rr rhs) '() `((= ,rr ,(expand-forms rhs))))
                (call (top setfield!) ,aa ,bb
                      (call (top convert)
                            (call (top fieldtype) (call (top typeof) ,aa) ,bb)
                            ,rr))
                ,rr))))
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
          (let ((a    (cadr lhs))
                (idxs (cddr lhs))
                (rhs  (caddr e)))
            (let* ((reuse (and (pair? a)
                               (contains (lambda (x)
                                           (or (eq? x 'end)
                                               (and (pair? x)
                                                    (eq? (car x) ':))))
                                         idxs)))
                   (arr   (if reuse (make-jlgensym) a))
                   (stmts (if reuse `((= ,arr ,(expand-forms a))) '()))
                   (rrhs (and (pair? rhs) (not (jlgensym? rhs)) (not (quoted? rhs))))
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
                 ,r)))))
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
   (lambda (e) (expand-forms `(call (top apply_type) ,@(cdr e))))

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
                     `(call (top _apply) ,f ,@(tuple-wrap argl '())))))

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
     (if (and (length> e 1) (pair? (cadr e)) (eq? (caadr e) 'parameters))
         (error "unexpected semicolon in tuple"))
     (if (any assignment? (cdr e))
         (error "assignment not allowed inside tuple"))
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

   'generator
   (lambda (e)
     (let ((expr   (cadr e))
           (vars   (map cadr  (cddr e)))
           (ranges (map caddr (cddr e))))
       (let* ((argname (if (and (length= vars 1) (symbol? (car vars)))
                           (car vars)
                           (gensy)))
              (splat (if (eq? argname (car vars))
                         '()
                         `((= (tuple ,@vars) ,argname)))))
         (expand-forms
          `(call (top Generator) (-> ,argname (block ,@splat ,expr))
                 ,(if (length= ranges 1)
                      (car ranges)
                      `(call (top IteratorND) (call (top product) ,@ranges))))))))

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
                  (inbounds true)
                  (call (top setindex!) ,result ,oneresult ,ri)
                  (inbounds pop)
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
        ((decl? e)   (list (decl-var e)))
        ((and (pair? e) (eq? (car e) 'tuple))
         (apply append (map lhs-vars (cdr e))))
        (else '())))

;; pass 2: identify and rename local vars

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
           (if (or (jlgensym? v) (memq v env))
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

(define (find-local-decls  e) (find-decls 'local  e))
(define (find-global-decls e) (find-decls 'global e))

(define (find-locals e env glob)
  (delete-duplicates
   (append! (check-dups (find-local-decls e))
            ;; const decls on non-globals also introduce locals
            (diff (find-decls 'const e) glob)
            (find-assigned-vars e env))))

(define (occurs-outside? sym e excl)
  (cond ((eq? e sym) #t)
        ((not (pair? e)) #f)
        ((eq? e excl) #f)
        ((memq (car e) '(lambda module toplevel quote top line inert)) #f)
        (else (any (lambda (x) (occurs-outside? sym x excl))
                   (cdr e)))))

;; local variable identification and renaming, derived from:
;; 1. (local x) expressions inside this scope-block and lambda
;; 2. (const x) expressions in a scope-block where x is not declared global
;; 3. variables assigned inside this scope-block that don't exist in outer
;;    scopes
;; returns lambdas in the form (lambda (args...) (locals...) body)
(define (resolve-scopes- e env implicitglobals lam renames newlam)
  (cond ((symbol? e) (let ((r (assq e renames)))
                       (if r (cdr r) e)))
        ((or (not (pair? e)) (quoted? e) (eq? (car e) 'toplevel)) e)
        ((eq? (car e) 'local) '(null)) ;; remove local decls
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
         (let* ((blok (cadr e))
                (other-locals (if lam (caddr lam) '()))
                (iglo (find-decls 'implicit-global blok))
                (glob (diff (find-global-decls blok) iglo))
                (vars (find-locals
                       blok
                       ;; being declared global prevents a variable
                       ;; assignment from introducing a local
                       (append env glob implicitglobals iglo)
                       (append glob iglo)))
                (need-rename
                 (if (or newlam (not lam)) '()
                     (receive
                      (conflicted unknown)
                      (separate (lambda (v) (or (memq v env) (memq v other-locals)))
                                vars)
                      (append
                       conflicted
                       (let ((lbod (lam:body lam)))
                         (filter (lambda (v) (occurs-outside? v lbod e))
                                 unknown))))))
                (renamed (map named-gensy need-rename))
                (new-ren (append (map cons need-rename renamed)
                                 (filter (lambda (ren)
                                           (not (memq (car ren) vars)))
                                         renames)))
                (new-env (append vars glob env))
                (new-iglo (append iglo implicitglobals))
                (body (resolve-scopes- blok new-env new-iglo lam new-ren #f))
                (real-new-vars (append (diff vars need-rename) renamed)))
           (for-each (lambda (v)
                       (if (memq v vars)
                           (error (string "variable \"" v "\" declared both local and global"))))
                     glob)
           (if lam
               (set-car! (cddr lam)
                         (append real-new-vars (caddr lam))))
           (insert-after-meta
            (if (and (pair? body) (eq? (car body) 'block))
                body
                `(block ,body))
            (map (lambda (v) `(local ,v)) real-new-vars))))
        ((eq? (car e) 'module)
         (error "module expression not at top level"))
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
  (let* ((args (filter (lambda (v) (not (eq? (arg-name v) UNUSED)))
                       (lam:args e)))
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
         (capt-sp (filter (lambda (v) (and (memq v fv) (not (memq v glo))))
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
;;   (var-info-lst captured-var-infos gensyms static_params)
;; where var-info-lst is a list of var-info records
(define (analyze-vars e env captvars sp)
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
         (let ((vi (var-info-for (method-expr-name e) env)))
           (if vi
               (begin (vinfo:set-asgn! vi #t)
                      ;; note: method defs require a memory loc. (issue #7658)
                      (vinfo:set-sa! vi #f))))
         (if (length= e 2)
             e
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

;; template for generating a closure type with parameters
(define (type-for-closure-parameterized name P fields types super)
  (let ((n (length P)))
    `(thunk
      (lambda ()
        ((,@(map (lambda (p) `(,p Any 18)) P))
         () 0 ())
        (body (global ,name) (const ,name)
              ,@(map (lambda (p) `(= ,p (call (top TypeVar) ',p (top Any) true))) P)
              (composite_type ,name (call (top svec) ,@P)
                              (call (top svec) ,@(map (lambda (v) `',v) fields))
                              ,super
                              (call (top svec) ,@types) #f ,(length fields))
              (return (null)))))))

;; ... and without parameters
(define (type-for-closure name fields super)
  `(thunk (lambda ()
            (() () 0 ())
            (body (global ,name) (const ,name)
                  (composite_type ,name (call (top svec))
                                  (call (top svec) ,@(map (lambda (v) `',v) fields))
                                  ,super
                                  (call (top svec) ,@(map (lambda (v) 'Any) fields))
                                  #f ,(length fields))
                  (return (null))))))

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
      `(call (top typeassert)
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
        (let* ((rhs1 (if (or (jlgensym? rhs0) (simple-atom? rhs0)
                             (equal? rhs0 '(the_exception)))
                         rhs0
                         (make-jlgensym)))
               (rhs  (if (eq? vt 'Any)
                         rhs1
                         (convert-for-type-decl rhs1 (cl-convert vt fname lam #f #f interp))))
               (ex (cond (closed `(call (top setfield!)
                                        ,(if interp
                                             `($ ,var)
                                             `(call (top getfield) ,fname (inert ,var)))
                                        (inert contents)
                                        ,rhs))
                         (capt `(call (top setfield!) ,var (inert contents) ,rhs))
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
                   (pattern-lambda (call (call (top (-/ getfield)) (-/ Core) (quote (-/ Typeof))) name)
                                   (get namemap name __)))
                  (cdddr typapp)))
         (closure-type (if (null? type-sp)
                           typ
                           `(call (top apply_type) ,typ ,@type-sp)))
         (newtypes
          (if iskw
              `(,(car types) ,(cadr types) ,closure-type ,@(cdddr types))
              `(,closure-type ,@(cdr types)))))
    `(call (top svec) (call (top apply_type) Tuple ,@newtypes)
           (call (top svec) ,@(append (cddr (cadddr te)) type-sp)))))

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
                         `((= ,arg (call (top Box) ,arg)))
                         '())))
                 args)))))

;; clear capture bit for vars assigned once at the top, to avoid allocating
;; some unnecessary Boxes.
(define (lambda-optimize-vars! lam)
  (define (expr-uses-var ex v)
    (cond ((assignment? ex) (expr-contains-eq v (caddr ex)))
          ((eq? (car ex) 'method)
           (and (length> ex 2)
                (assq v (cadr (lam:vinfo (cadddr ex))))))
          (else #f)))
  (assert (eq? (car lam) 'lambda))
  (let ((vi (car (lam:vinfo lam))))
    (if (and (any vinfo:capt vi)
             (any vinfo:sa vi))
        (let* ((leading
                (filter (lambda (x) (and (pair? x) (or (eq? (car x) 'method)
                                                       (eq? (car x) '=))))
                        (take-while (lambda (e)
                                      (or (atom? e)
                                          (memq (car e) '(quote top line inert local
                                                                implicit-global global
                                                                const newvar = null method))))
                                    (lam:body lam))))
               (unused (map cadr leading))
               (def (table)))
          ;; TODO: reorder leading statements to put assignments where the RHS is
          ;; `simple-atom?` at the top.
          (for-each (lambda (e)
                      (set! unused (filter (lambda (v) (not (expr-uses-var e v)))
                                           unused))
                      (if (memq (cadr e) unused)
                          (put! def (cadr e) #t)))
                    leading)
          (for-each (lambda (v)
                      (if (and (vinfo:sa v) (has? def (car v)))
                          (set-car! (cddr v) (logand (caddr v) (lognot 5)))))
                    vi)))
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
                                   `($ (call (top QuoteNode) ,e))
                                   `(call (top getfield) ,fname (inert ,e)))))
                   (if (and (vinfo:asgn cv) (vinfo:capt cv))
                       `(call (top getfield) ,access (inert contents))
                       access)))
                (vi
                 (if (and (vinfo:asgn vi) (vinfo:capt vi))
                     `(call (top getfield) ,e (inert contents))
                     e))
                (else e))))
       ((atom? e) e)
       (else
        (case (car e)
          ((quote top line break inert module toplevel null meta) e)
          ((=)
           (let ((var (cadr e))
                 (rhs (cl-convert (caddr e) fname lam namemap toplevel interp)))
             (if (jlgensym? var)
                 `(= ,var ,rhs)
                 (convert-assignment var rhs fname lam interp))))
          ((newvar)
           (let ((vi (assq (cadr e) (car (lam:vinfo lam)))))
             (if (and vi (vinfo:asgn vi) (vinfo:capt vi))
                 `(= ,(cadr e) (call (top Box)))
                 e)))
          ((local)
           (let ((vi (assq (cadr e) (car (lam:vinfo lam)))))
             (if (and vi (vinfo:asgn vi) (vinfo:capt vi)
                      ;; avoid redundant box for vars with newvar nodes
                      (not (any (lambda (x) (and (length= x 2)
                                                 (eq? (car x) 'newvar) (eq? (cadr x) (cadr e))))
                                (lam:body lam))))
                 `(= ,(cadr e) (call (top Box)))
                 `(newvar ,(cadr e)))))
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
                  (local? (and lam (symbol? name)
                               (or (assq name (car  (lam:vinfo lam)))
                                   (assq name (cadr (lam:vinfo lam))))))
                  (sig      (and (not short) (caddr e)))
                  (sp-inits (if (or short (not (eq? (car sig) 'block)))
                                '()
                                (map-cl-convert (butlast (cdr sig))
                                                fname lam namemap toplevel interp)))
                  (sig      (and sig (if (eq? (car sig) 'block)
                                         (last sig)
                                         sig))))
             (if local?
                 (begin (if (memq name (lam:args lam))
                            (error (string "cannot add method to function argument " name)))
                        (if (eqv? (string.char (string name) 0) #\@)
                            (error "macro definition not allowed inside a local scope"))))
             (if (not local?) ;; not a local function; will not be closure converted to a new type
                 (cond (short e)
                       ((null? cvs)
                        `(toplevel-butlast
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
                               (newlam    (renumber-things (renumber-jlgensym (linearize (car exprs)))))
                               (vi        (lam:vinfo newlam))
                               ;; insert `list` expression heads to make the lambda vinfo
                               ;; lists quotable
                               (newlam `(lambda (list ,@(cadr newlam))
                                          (list (list ,@(map (lambda (l) (cons 'list l))
                                                             (car vi)))
                                                (list ,@(cadr vi)) ,(caddr vi) (list ,@(cadddr vi)))
                                          ,@(cdddr newlam))))
                          `(toplevel-butlast
                            ,@top-stmts
                            ,@sp-inits
                            (method ,name ,(cl-convert sig fname lam namemap toplevel interp)
                                    ,(julia-expand-macros `(quote ,newlam))
                                    ,(last e))))))
                 ;; local case - lift to a new type at top level
                 (let* ((exists (get namemap name #f))
                        (tname  (or exists
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
                        (cvs   (delete-duplicates
                                (apply append  ;; merge captured vars from all definitions
                                       cvs
                                       (map (lambda (methdef)
                                              (map car (cadr (lam:vinfo (cadddr methdef)))))
                                            alldefs))))
                        (sps   (delete-duplicates  ;; static params from all definitions
                                (apply append
                                       (if lam2 (lam:sp lam2) '())
                                       (map (lambda (methdef) (lam:sp (cadddr methdef)))
                                            alldefs))))
                        (capt-sp (intersect cvs sps))
                        (capt-vars (diff cvs capt-sp))
                        (method-sp (map (lambda (s) (make-jlgensym)) capt-sp))
                        (typedef  ;; expression to define the type
                         (let* ((fieldtypes (map (lambda (v)
                                                   (if (is-var-boxed? v lam)
                                                       'Any ;; TODO
                                                       (gensy)))
                                                 capt-vars))
                                (para (append capt-sp
                                              (filter (lambda (v) (not (eq? v 'Any))) fieldtypes))))
                           (if (null? para)
                               (type-for-closure tname capt-vars '(top Function))
                               (type-for-closure-parameterized tname para capt-vars fieldtypes '(top Function)))))
                        (mk-closure  ;; expression to make the closure
                         (let* ((var-exprs (map (lambda (v)
                                                  (let ((cv (assq v (cadr (lam:vinfo lam)))))
                                                    (if cv
                                                        (if interp
                                                            `($ (call (top QuoteNode) ,v))
                                                            `(call (top getfield) ,fname (inert ,v)))
                                                        v)))
                                                capt-vars))
                                (P (append
                                    capt-sp
                                    (filter identity (map (lambda (v ve)
                                                            (if (is-var-boxed? v lam)
                                                                #f
                                                                `(call (top typeof) ,ve)))
                                                          capt-vars var-exprs)))))
                           `(new ,(if (null? P)
                                      tname
                                      `(call (top apply_type) ,tname ,@P))
                                 ,@var-exprs)))
                        (iskw ;; TODO jb/functions need more robust version of this
                         (contains (lambda (x) (eq? x 'kwftype)) sig)))
                   `(toplevel-butlast
                     ,@(if exists
                           '()
                           (list
                            (begin (and name (put! namemap name tname))
                                   typedef)))
                     ,@sp-inits
                     ,@(if short '()
                           (map (lambda (gs tvar)
                                  (make-assignment gs `(call (top TypeVar) ',tvar (top Any) true)))
                                method-sp capt-sp))
                     ,@(if short '()
                           `((method #f
                                     ,(cl-convert
                                       (fix-function-arg-type sig tname iskw namemap method-sp)
                                       fname lam namemap toplevel interp)
                                     ,(convert-lambda lam2
                                                      (if iskw
                                                          (caddr (lam:args lam2))
                                                          (car (lam:args lam2)))
                                                      #f capt-sp)
                                     ,(last e))))
                     ,(if exists
                          '(null)
                          (convert-assignment name mk-closure fname lam interp)))))))
          ((lambda)  ;; should only happen inside (thunk ...)
           ;; flattening blocks helps lambda-optimize-vars! work
           (set-car! (cdddr e) (flatten-blocks (lam:body e)))
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
           (cl-convert `(call (top typeassert) ,@(cdr e)) fname lam namemap toplevel interp))
          ;; remaining `decl` expressions are only type assertions if the
          ;; argument is global or a non-symbol.
          ((decl)
           (if (or (assq (cadr e) (car  (lam:vinfo lam)))
                   (assq (cadr e) (cadr (lam:vinfo lam))))
               '(null)
               (cl-convert `(call (top typeassert) ,@(cdr e)) fname lam namemap toplevel interp)))
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
      (if (> handler-level 0)
          (let ((tmp (if (or (simple-atom? x) (jlgensym? x) (equal? x '(null)))
                         #f (make-jlgensym))))
            (if tmp (emit `(= ,tmp ,x)))
            (emit `(leave ,handler-level))
            (emit `(return ,(or tmp x))))
          (emit `(return ,x))))
    (define (new-mutable-var)
      (let ((g (gensy)))
        (set-car! (lam:vinfo lam) (append (car (lam:vinfo lam)) `((,g Any 2))))
        g))
    ;; evaluate the arguments of a call, creating temporary locations as needed
    (define (compile-args lst break-labels)
      (if (null? lst) '()
          (let ((temps? (or *very-linear-mode*
                            (expr-contains-p (lambda (x) (and (assignment? x)
                                                              (symbol? (cadr x))))
                                             (cons 'block (cdr lst))))))
            (let loop ((lst  lst)
                       (vals '()))
              (if (null? lst)
                  (reverse! vals)
                  (let* ((arg (car lst))
                         (aval (compile arg break-labels #t #f)))
                    (loop (cdr lst)
                          (cons (if (and temps?
                                         (not (simple-atom? arg))  (not (jlgensym? arg))
                                         (not (simple-atom? aval)) (not (jlgensym? aval))
                                         (not (and (pair? arg)
                                                   (memq (car arg) '(& quote inert top copyast))))
                                         (not (and (symbol? arg)
                                                   (or (null? (cdr lst))
                                                       (null? vals)))))
                                    (let ((tmp (make-jlgensym)))
                                      (emit `(= ,tmp ,aval))
                                      tmp)
                                    aval)
                                vals))))))))
    (define (compile-cond ex break-labels)
      (let ((cnd (compile ex break-labels #t #f)))
        (if (and *very-linear-mode*
                 (not (or (simple-atom? cnd) (jlgensym? cnd) (symbol? cnd))))
            (let ((tmp (make-jlgensym)))
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
      (if (or (not (pair? e)) (memq (car e) '(null jlgensym quote inert top copyast the_exception $
                                                   cdecl stdcall fastcall thiscall)))
          (cond (tail  (emit-return e))
                (value e)
                ((symbol? e) (emit e) #f)  ;; keep symbols for undefined-var checking
                (else #f))
          (case (car e)
            ((call new)
             (let* ((ccall? (and (eq? (car e) 'call) (equal? (cadr e) '(top ccall))))
                    (args (if ccall?
                              ;; NOTE: first 3 arguments of ccall must be left in place
                              (append (list-head (cdr e) 4)
                                      (compile-args (list-tail e 5) break-labels))
                              (compile-args (cdr e) break-labels)))
                    (callex (cons (car e) args)))
               (cond (tail (emit-return callex))
                     (value callex)
                     ((eq? (car e) 'new) #f)
                     (else (emit callex)))))
            ((=)
             (let ((rhs (compile (caddr e) break-labels #t #f)))
               (if value
                   (let ((rr (if (or (atom? rhs) (jlgensym? rhs) (eq? (car rhs) 'null))
                                 rhs (make-jlgensym))))
                     (if (not (eq? rr rhs))
                         (emit `(= ,rr ,rhs)))
                     (emit `(= ,(cadr e) ,rr))
                     (if tail (emit-return rr))
                     rr)
                   (emit `(= ,(cadr e) ,rhs)))))
            ((block body)
             (let loop ((xs (cdr e)))
               (if (null? (cdr xs))
                   (compile (car xs) break-labels value tail)
                   (begin (compile (car xs) break-labels #f #f)
                          (loop (cdr xs))))))
            ((return)
             (compile (cadr e) break-labels #t #t)
             '(null))
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

            ((type_goto)
             (let ((m (get label-map (cadr e) #f)))
               (if m
                   (emit `(type_goto ,m ,@(cddr e)))
                   (let ((l (make-label)))
                     (put! label-map (cadr e) l)
                     (emit `(type_goto ,l ,@(cddr e)))))))
            ((static_typeof)
             (assert (and value (not tail)))
             e)

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
                 (begin (emit `(method ,(cadr e)
                                       ,(compile (caddr e) break-labels #t #f)
                                       ,(linearize (cadddr e))
                                       ,@(cddddr e)))
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
             (let ((vname (cadr e)))
               (if (var-info-for vname vi)
                   ;; issue #7264
                   (error (string "`global " vname "`: " vname " is local variable in the enclosing scope"))
                   #f)))
            ((local) #f)
            ((implicit-global) #f)
            ((const) (emit e))

            ;; metadata
            ((line meta boundscheck simdloop) (emit e))
            ((inbounds)
             ;; TODO: this should not be here but sometimes ends up in tail position, e.g.
             ;; `f(x) = @inbounds return x`
             (cond (tail  (emit-return e))
                   (value e)
                   (else  (emit e))))
            ;; top level expressions returning values
            ((abstract_type bits_type composite_type thunk toplevel module)
             (if tail (emit-return e) (emit e)))
            ;; other top level expressions
            ((import importall using export)
             (emit e)
             (if tail (emit-return '(null)) '(null)))
            (else
             (error (string "unhandled expr " e))))))
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
           (di    (definitely-initialized-vars stmts vi)))
      (cons 'body (filter (lambda (e)
                            (not (and (pair? e) (eq? (car e) 'newvar)
                                      (has? di (cadr e)))))
                          stmts)))))

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

;; pass 6: renumber jlgensyms to start at 0 in each function

(define (make-gensym-generator)
  (let ((jlgensym-counter 0))
    (lambda ()
      (begin0 `(jlgensym ,jlgensym-counter)
              (set! jlgensym-counter (+ 1 jlgensym-counter))))))

(define (renumber-jlgensym- e tbl next-jlgensym)
  (cond
   ((or (not (pair? e)) (quoted? e)) e)
   ((eq? (car e) 'lambda)
    (let* ((next  (make-gensym-generator))
           (body  (renumber-jlgensym- (lam:body e) (table) next))
           (count (cadr (next)))
           (vi    (caddr e)))
      `(lambda ,(cadr e)
         (,(car vi) ,(cadr vi) ,count ,(last vi))
         ,body)))
   ((jlgensym? e)
    (let ((n (get tbl (cadr e) #f)))
      (if n n
          (let ((n (next-jlgensym))) (put! tbl (cadr e) n) n))))
   (else (map (lambda (x) (renumber-jlgensym- x tbl next-jlgensym)) e))))

(define (renumber-jlgensym e)
  (renumber-jlgensym- e #f error))

(define (label-to-idx-map body)
  (let ((tbl (table)))
    (let loop ((stmts (cdr body))
               (i 1))
      (if (pair? stmts)
          (let ((el (car stmts)))
            (if (and (pair? el) (eq? (car el) 'label))
                (put! tbl (cadr el) i))
            (loop (cdr stmts) (+ i 1)))))
    tbl))

(define (renumber-labels! body label2idx)
  (let loop ((stmts (cdr body)))
    (if (pair? stmts)
        (let ((el (car stmts)))
          (if (pair? el)
              (case (car el)
                ((label goto enter) (set-car! (cdr el) (get label2idx (cadr el))))
                ((gotoifnot)        (set-car! (cddr el) (get label2idx (caddr el))))
                (else #f)))
          (loop (cdr stmts))))))

(define (renumber-things ex)
  (if (atom? ex) ex
      (begin (if (eq? (car ex) 'lambda)
                 (renumber-labels! (lam:body ex) (label-to-idx-map (lam:body ex))))
             (for-each renumber-things (cdr ex))))
  ex)

;; expander entry point

(define (julia-expand1 ex)
  (renumber-things
   (renumber-jlgensym
    (linearize
     (closure-convert
      (analyze-variables!
       (resolve-scopes ex)))))))

(define julia-expand0 expand-forms)

(define (julia-expand ex)
  (julia-expand1
   (julia-expand0
    (julia-expand-macros ex))))
