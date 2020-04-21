;; macro expander

;; backquote expansion

(define splat-token '(__splat__))

(define (bq-expand-arglist lst d)
  (let loop ((lst lst)
             (out '()))
    (if (null? lst)
        (reverse! out)
        (let ((nxt (julia-bq-expand- (car lst) d)))
          (if (and (pair? nxt) (eq? (car nxt) splat-token))
              (loop (cdr lst) (revappend (cdr nxt) out))
              (loop (cdr lst) (cons nxt out)))))))

(define (julia-bq-expand- x d)
  (cond ((or (symbol? x) (ssavalue? x))     (list 'inert x))
        ((atom? x)  x)
        ((memq (car x) '(true false)) x)
        ((and (= d 0) (eq? (car x) '$))
         (if (length= x 2)
             (if (vararg? (cadr x))
                 ;; splice expr ($ (... x))
                 `(... ,(cadr (cadr x)))
                 ;; otherwise normal interpolation
                 (cadr x))
             ;; in e.g. `quote quote $$(x...) end end` multiple expressions can be
             ;; spliced into `$`, which then need to be spliced into the enclosing
             ;; expression in the next stage.
             (cons splat-token (cdr x))))
        ((not (contains (lambda (e) (and (pair? e) (eq? (car e) '$))) x))
         (if (eq? (car x) 'line)
             `(inert ,x)
             `(copyast (inert ,x))))
        (else
         (case (car x)
           ((inert) `(call (core QuoteNode)      ,@(bq-expand-arglist (cdr x) d)))
           ((line)  `(call (core LineNumberNode) ,@(bq-expand-arglist (cdr x) d)))
           ((quote) `(call (core _expr)          ,@(bq-expand-arglist x (+ d 1))))
           (($)     `(call (core _expr)          ,@(bq-expand-arglist x (- d 1))))
           (else    `(call (core _expr)          ,@(bq-expand-arglist x d)))))))

(define (julia-bq-expand x d)
  (let ((e (julia-bq-expand- x d)))
    (if (and (pair? e) (eq? (car e) splat-token))
        '(error "\"...\" expression outside call")
        e)))

;; hygiene

;; return the names of vars introduced by forms, instead of their transformations.
(define vars-introduced-by-patterns
  (pattern-set
   ;; function with static parameters
   (pattern-lambda
    (function (call (curly name . sparams) . argl) body)
    (cons 'varlist (append (safe-llist-positional-args (fix-arglist argl))
                           (typevar-names sparams))))

   ;; function definition
   (pattern-lambda (function (-$ (call name . argl) (|::| (call name . argl) _t)) body)
                   (cons 'varlist (safe-llist-positional-args (fix-arglist (append (self-argname name) argl)))))
   (pattern-lambda (function (where callspec . wheres) body)
                   (let ((others (pattern-expand1 vars-introduced-by-patterns `(function ,callspec ,body))))
                     (cons 'varlist (append (if (and (pair? others) (eq? (car others) 'varlist))
                                                (cdr others)
                                                '())
                                            (typevar-names wheres)))))

   (pattern-lambda (function (tuple . args) body)
                   `(-> (tuple ,@args) ,body))

   ;; expression form function definition
   (pattern-lambda (= (call (curly name . sparams) . argl) body)
                   `(function (call (curly ,name . ,sparams) . ,argl) ,body))
   (pattern-lambda (= (-$ (call name . argl) (|::| (call name . argl) _t)) body)
                   `(function ,(cadr __) ,body))
   (pattern-lambda (= (where callspec . wheres) body)
                   (cons 'function (cdr __)))

   ;; anonymous function
   (pattern-lambda (-> a b)
                   (let ((a (if (and (pair? a)
                                     (eq? (car a) 'tuple))
                                (cdr a)
                                (list a))))
                     (cons 'varlist (safe-llist-positional-args (fix-arglist a)))))

   ;; where
   (pattern-lambda (where ex . vars)
                   (cons 'varlist (typevar-names vars)))
   (pattern-lambda (= (curly ex . vars) rhs)
                   (cons 'varlist (typevar-names vars)))

   ;; let
   (pattern-lambda (let binds ex)
                   (let loop ((binds (let-binds __))
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
                            ((eventually-call? (cadar binds))
                             ;; f()=c
                             (let ((asgn (cadr (julia-expand0 (car binds) 'none 0))))
                               (loop (cdr binds)
                                     (cons (cadr asgn) vars))))
                            ((and (pair? (cadar binds))
                                  (eq? (caadar binds) 'tuple))
                             (loop (cdr binds)
                                   (append (map decl-var (lhs-vars (cadar binds))) vars)))
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
   (pattern-lambda (struct mut spec body)
                   (let ((tn (typedef-expr-name spec))
                         (tv (typedef-expr-tvars spec)))
                     (list* 'varlist (cons (unescape tn) (unescape tn)) '(new . new)
                            (typevar-names tv))))
   (pattern-lambda (abstract spec)
                   (let ((tn (typedef-expr-name spec))
                         (tv (typedef-expr-tvars spec)))
                     (list* 'varlist (cons (unescape tn) (unescape tn))
                            (typevar-names tv))))
   (pattern-lambda (primitive spec nb)
                   (let ((tn (typedef-expr-name spec))
                         (tv (typedef-expr-tvars spec)))
                     (list* 'varlist (cons (unescape tn) (unescape tn))
                            (typevar-names tv))))

   )) ; vars-introduced-by-patterns

(define keywords-introduced-by-patterns
  (pattern-set
   (pattern-lambda (function (call (curly name . sparams) . argl) body)
                   (cons 'varlist (safe-llist-keyword-args (fix-arglist argl))))

   (pattern-lambda (function (-$ (call name . argl) (|::| (call name . argl) _t)) body)
                   (cons 'varlist (safe-llist-keyword-args (fix-arglist argl))))
   (pattern-lambda (function (where callspec . wheres) body)
                   `(function ,callspec ,body))

   (pattern-lambda (= (call (curly name . sparams) . argl) body)
                   `(function (call (curly ,name . ,sparams) . ,argl) ,body))
   (pattern-lambda (= (-$ (call name . argl) (|::| (call name . argl) _t)) body)
                   `(function (call ,name ,@argl) ,body))
   (pattern-lambda (= (where callspec . wheres) body)
                   (cons 'function (cdr __)))
   ))

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

(define (typedef-expr-name e)
  (cond ((atom? e) e)
        ((or (eq? (car e) 'curly) (eq? (car e) '<:)) (typedef-expr-name (cadr e)))
        (else e)))

(define (typedef-expr-tvars e)
  (cond ((atom? e) '())
        ((eq? (car e) '<:) (typedef-expr-tvars (cadr e)))
        ((eq? (car e) 'curly) (cddr e))
        (else '())))

(define (typevar-expr-name e) (car (analyze-typevar e)))

;; get the list of names from a list of `where` variable expressions
(define (typevar-names lst)
  (apply nconc
         (map (lambda (v) (trycatch
                           (list (typevar-expr-name v))
                           (lambda (e) '())))
              lst)))

;; get the name from a function formal argument expression, allowing `(escape x)`
(define (try-arg-name v)
  (cond ((symbol? v) (list v))
        ((atom? v) '())
        (else
         (case (car v)
           ((... kw |::|) (try-arg-name (cadr v)))
           ((escape) (list v))
           ((hygienic-scope) (try-arg-name (cadr v)))
           ((meta)  ;; allow certain per-argument annotations
            (if (nospecialize-meta? v #t)
                (try-arg-name (caddr v))
                '()))
           (else '())))))

;; get names from a formal argument list, specifying whether to include escaped ones
(define (safe-arg-names lst (escaped #f))
  (apply nconc
         (map (lambda (v)
                (let ((vv (try-arg-name v)))
                  (if (eq? escaped (and (pair? vv) (pair? (car vv)) (eq? (caar vv) 'escape)))
                      (if escaped (list (cadar vv)) vv)
                      '())))
              lst)))

;; arg names, looking only at positional args
(define (safe-llist-positional-args lst (escaped #f))
  (receive
   (params normal) (separate (lambda (a) (and (pair? a)
                                              (eq? (car a) 'parameters)))
                             lst)
   (safe-arg-names
    (append normal
            ;; rest keywords name is not a keyword
            (apply append (map (lambda (a) (filter vararg? a))
                               params)))
    escaped)))

;; arg names from keyword arguments, and positional arguments with escaped names
(define (safe-llist-keyword-args lst)
  (let* ((kwargs (apply nconc
                        (map cdr
                             (filter (lambda (a) (and (pair? a) (eq? (car a) 'parameters)))
                                     lst))))
         ;; rest keywords name is not a keyword
         (kwargs (filter (lambda (x) (not (vararg? x))) kwargs)))
    (append
     (safe-arg-names kwargs #f)
     (safe-arg-names kwargs #t)
     ;; count escaped argument names as "keywords" to prevent renaming
     (safe-llist-positional-args lst #t))))

;; argument name for the function itself given `function (f::T)(...)`, otherwise ()
(define (self-argname name)
  (if (and (length= name 3) (eq? (car name) '|::|))
      (list (cadr name))
      '()))

;; resolve-expansion-vars-with-new-env, but turn on `inarg` once we get inside
;; the formal argument list. `e` in general might be e.g. `(f{T}(x)::T) where T`,
;; and we want `inarg` to be true for the `(x)` part.
(define (resolve-in-function-lhs e env m parent-scope inarg)
  (define (recur x) (resolve-in-function-lhs x env m parent-scope inarg))
  (define (other x) (resolve-expansion-vars-with-new-env x env m parent-scope inarg))
  (case (car e)
    ((where) `(where ,(recur (cadr e)) ,@(map other (cddr e))))
    ((|::|)  `(|::| ,(recur (cadr e)) ,(other (caddr e))))
    ((call)  `(call ,(other (cadr e))
                    ,@(map (lambda (x)
                             (resolve-expansion-vars-with-new-env x env m parent-scope #t))
                           (cddr e))))
    (else (other e))))

(define (new-expansion-env-for x env (outermost #f))
  (let ((introduced (pattern-expand1 vars-introduced-by-patterns x)))
    (if (or (atom? x)
            (and (not outermost)
                 (not (and (pair? introduced) (eq? (car introduced) 'varlist)))))
        env
        (let ((globals (find-declared-vars-in-expansion x 'global))
              (vlist (if (and (pair? introduced) (eq? (car introduced) 'varlist))
                         (cdr introduced)
                         '())))
          (receive
           (pairs vnames) (separate pair? vlist)
           (let ((v (diff (delete-duplicates
                           (append! (find-declared-vars-in-expansion x 'local)
                                    (find-assigned-vars-in-expansion x)
                                    vnames))
                          globals)))
             (append!
              pairs
              (filter (lambda (v) (not (assq (car v) env)))
                      (pair-with-gensyms v))
              (map (lambda (v) (cons v v))
                   (keywords-introduced-by x))
              env)))))))

(define (resolve-expansion-vars-with-new-env x env m parent-scope inarg (outermost #f))
  (resolve-expansion-vars-
   x
   (if (and (pair? x) (eq? (car x) 'let))
       ;; let is strange in that it needs both old and new envs within
       ;; the same expression
       env
       (new-expansion-env-for x env outermost))
   m parent-scope inarg))

(define (resolve-expansion-vars- e env m parent-scope inarg)
  (cond ((or (eq? e 'end) (eq? e 'ccall) (eq? e 'cglobal))
         e)
        ((symbol? e)
         (let ((a (assq e env)))
           (if a (cdr a)
               (if m `(globalref ,m ,e)
                   e))))
        ((or (not (pair? e)) (quoted? e))
         e)
        (else
         (case (car e)
           ((ssavalue) e)
           ((escape) (if (null? parent-scope)
              (julia-expand-macroscopes- (cadr e))
              (let* ((scope (car parent-scope))
                     (env (car scope))
                     (m (cadr scope))
                     (parent-scope (cdr parent-scope)))
                (resolve-expansion-vars-with-new-env (cadr e) env m parent-scope inarg))))
           ((global) (let ((arg (cadr e)))
                       (cond ((symbol? arg) e)
                             ((assignment? arg)
                              `(global
                                (= ,(unescape (cadr arg))
                                   ,(resolve-expansion-vars-with-new-env (caddr arg) env m parent-scope inarg))))
                             (else
                              `(global ,(resolve-expansion-vars-with-new-env arg env m parent-scope inarg))))))
           ((using import export meta line inbounds boundscheck loopinfo) (map unescape e))
           ((macrocall) e) ; invalid syntax anyways, so just act like it's quoted.
           ((symboliclabel) e)
           ((symbolicgoto) e)
           ((struct)
            `(struct ,(cadr e) ,(resolve-expansion-vars- (caddr e) env m parent-scope inarg)
                     ;; type has special behavior: identifiers inside are
                     ;; field names, not expressions.
                     ,(map (lambda (x)
                             (cond ((atom? x) x)
                                   ((and (pair? x) (eq? (car x) '|::|))
                                    `(|::| ,(cadr x)
                                      ,(resolve-expansion-vars- (caddr x) env m parent-scope inarg)))
                                   (else
                                    (resolve-expansion-vars-with-new-env x env m parent-scope inarg))))
                           (cadddr e))))

           ((parameters)
            (cons 'parameters
                  (map (lambda (x)
                         ;; `x` by itself after ; means `x=x`
                         (let ((x (if (and (not inarg) (symbol? x))
                                      `(kw ,x ,x)
                                      x)))
                           (resolve-expansion-vars- x env m parent-scope #f)))
                       (cdr e))))

           ((= function)
            (if (and (pair? (cadr e)) (function-def? e))
                ;; in (kw x 1) inside an arglist, the x isn't actually a kwarg
                `(,(car e) ,(resolve-in-function-lhs (cadr e) env m parent-scope inarg)
                  ,(resolve-expansion-vars-with-new-env (caddr e) env m parent-scope inarg))
                `(,(car e) ,@(map (lambda (x)
                                    (resolve-expansion-vars-with-new-env x env m parent-scope inarg))
                                  (cdr e)))))

           ((kw)
            (cond
             ((not (length> e 2)) e)
             ((and (pair? (cadr e))
                   (eq? (caadr e) '|::|))
              `(kw (|::|
                    ,(if inarg
                         (resolve-expansion-vars- (cadr (cadr e)) env m parent-scope inarg)
                         ;; in keyword arg A=B, don't transform "A"
                         (unescape (cadr (cadr e))))
                    ,(resolve-expansion-vars- (caddr (cadr e)) env m parent-scope inarg))
                   ,(resolve-expansion-vars-with-new-env (caddr e) env m parent-scope inarg)))
             (else
              `(kw ,(if inarg
                        (resolve-expansion-vars- (cadr e) env m parent-scope inarg)
                        (unescape (cadr e)))
                   ,(resolve-expansion-vars-with-new-env (caddr e) env m parent-scope inarg)))))

           ((let)
            (let* ((newenv (new-expansion-env-for e env))
                   (body   (resolve-expansion-vars- (caddr e) newenv m parent-scope inarg))
                   (binds  (let-binds e)))
              `(let (block
                     ,@(map
                        (lambda (bind)
                          (if (assignment? bind)
                              (make-assignment
                               ;; expand binds in old env with dummy RHS
                               (cadr (resolve-expansion-vars- (make-assignment (cadr bind) 0)
                                                              newenv m parent-scope inarg))
                               ;; expand initial values in old env
                               (resolve-expansion-vars- (caddr bind) env m parent-scope inarg))
                              bind))
                        binds))
                 ,body)))
           ((hygienic-scope) ; TODO: move this lowering to resolve-scopes, instead of reimplementing it here badly
             (let ((parent-scope (cons (list env m) parent-scope))
                   (body (cadr e))
                   (m (caddr e)))
              (resolve-expansion-vars-with-new-env body env m parent-scope inarg #t)))
           ((tuple)
            (cons (car e)
                  (map (lambda (x)
                         (if (assignment? x)
                             `(= ,(unescape (cadr x))
                                 ,(resolve-expansion-vars-with-new-env (caddr x) env m parent-scope inarg))
                             (resolve-expansion-vars-with-new-env x env m parent-scope inarg)))
                       (cdr e))))

           ;; todo: trycatch
           (else
            (cons (car e)
                  (map (lambda (x)
                         (resolve-expansion-vars-with-new-env x env m parent-scope inarg))
                       (cdr e))))))))

;; decl-var that also identifies f in f()=...
(define (decl-var* e)
  (cond ((not (pair? e))       e)
        ((eq? (car e) 'escape) '())
        ((eq? (car e) 'call)   (decl-var* (cadr e)))
        ((eq? (car e) '=)      (decl-var* (cadr e)))
        ((eq? (car e) 'curly)  (decl-var* (cadr e)))
        ((eq? (car e) '|::|)   (decl-var* (cadr e)))
        ((eq? (car e) 'where)  (decl-var* (cadr e)))
        (else                  (decl-var e))))

(define (decl-vars* e)
  (if (and (pair? e) (eq? (car e) 'tuple))
      (apply append (map decl-vars* (cdr e)))
      (list (decl-var* e))))

;; count hygienic / escape pairs
;; and fold together a list resulting from applying the function to
;; any block at the same hygienic scope
(define (resume-on-escape lam e nblocks)
  (if (or (not (pair? e)) (quoted? e))
      '()
      (cond ((memq (car e) '(lambda module toplevel))
             '())
            ((eq? (car e) 'hygienic-scope)
             (resume-on-escape lam (cadr e) (+ nblocks 1)))
            ((eq? (car e) 'escape)
             (if (= nblocks 0)
                 (lam (cadr e))
                 (resume-on-escape lam (cadr e) (- nblocks 1))))
            (else
             (foldl (lambda (a l) (append! l (resume-on-escape lam a nblocks)))
                    '()
                    (cdr e))))))

(define (find-declared-vars-in-expansion e decl (outer #t))
  (cond ((or (not (pair? e)) (quoted? e)) '())
        ((eq? (car e) 'escape)  '())
        ((eq? (car e) 'hygienic-scope)
         (resume-on-escape (lambda (e) (find-declared-vars-in-expansion e decl outer)) (cadr e) 0))
        ((eq? (car e) decl)     (map decl-var* (cdr e)))
        ((and (not outer) (function-def? e)) '())
        (else
         (apply append! (map (lambda (x)
                               (find-declared-vars-in-expansion x decl #f))
                             e)))))

(define (find-assigned-vars-in-expansion e (outer #t))
  (cond ((or (not (pair? e)) (quoted? e))  '())
        ((eq? (car e) 'escape)  '())
        ((eq? (car e) 'hygienic-scope)
         (resume-on-escape (lambda (e) (find-assigned-vars-in-expansion e outer)) (cadr e) 0))
        ((and (not outer) (function-def? e))
         ;; pick up only function name
         (let ((fname (cond ((eq? (car e) '=) (decl-var* (cadr e)))
                            ((eq? (car e) 'function)
                             (cond ((atom? (cadr e))             (cadr e))
                                   ((eq? (car (cadr e)) 'tuple)  #f)
                                   (else                         (decl-var* (cadr e)))))
                            (else #f))))
           (if (symbol? fname)
               (list fname)
               '())))
        ((and (eq? (car e) '=) (not (function-def? e)))
         (append! (filter symbol? (decl-vars* (cadr e)))
                  (find-assigned-vars-in-expansion (caddr e) #f)))
        ((eq? (car e) 'tuple)
         (apply append! (map (lambda (x)
                               (find-assigned-vars-in-expansion (if (assignment? x)
                                                                    (caddr x)
                                                                    x)
                                                                #f))
                             (cdr e))))
        (else
         (apply append! (map (lambda (x)
                               (find-assigned-vars-in-expansion x #f))
                             (cdr e))))))

(define (keywords-introduced-by e)
  (let ((v (pattern-expand1 keywords-introduced-by-patterns e)))
    (if (and (pair? v) (eq? (car v) 'varlist))
        (cdr v)
        '())))

(define (resolve-expansion-vars e m)
  ;; expand binding form patterns
  ;; keep track of environment, rename locals to gensyms
  ;; and wrap globals in (globalref module var) for macro's home module
  (resolve-expansion-vars-with-new-env e '() m '() #f #t))

(define (julia-expand-quotes e)
  (cond ((not (pair? e)) e)
        ((eq? (car e) 'inert) e)
        ((eq? (car e) 'module) e)
        ((eq? (car e) 'quote)
         (julia-expand-quotes (julia-bq-macro (cadr e))))
        ((not (contains (lambda (e) (and (pair? e) (eq? (car e) 'quote))) (cdr e))) e)
        (else
         (cons (car e) (map julia-expand-quotes (cdr e))))))

(define (julia-expand-macroscopes- e)
  (cond ((not (pair? e)) e)
        ((eq? (car e) 'inert) e)
        ((eq? (car e) 'module) e)
        ((eq? (car e) 'hygienic-scope)
         (let ((form (cadr e)) ;; form is the expression returned from expand-macros
               (modu (caddr e))) ;; m is the macro's def module
           (resolve-expansion-vars form modu)))
        (else
         (map julia-expand-macroscopes- e))))

(define (rename-symbolic-labels- e relabels parent-scope)
  (cond
   ((or (not (pair? e)) (quoted? e)) e)
   ((eq? (car e) 'hygienic-scope)
    (let ((parent-scope (list relabels parent-scope))
          (body (cadr e))
          (m (caddr e)))
      `(hygienic-scope ,(rename-symbolic-labels- (cadr e) (table) parent-scope) ,m)))
   ((and (eq? (car e) 'escape) (not (null? parent-scope)))
    `(escape ,(apply rename-symbolic-labels- (cadr e) parent-scope)))
   ((or (eq? (car e) 'symbolicgoto) (eq? (car e) 'symboliclabel))
    (let* ((s (cadr e))
           (havelabel (if (or (null? parent-scope) (not (symbol? s))) s (get relabels s #f)))
           (newlabel (if havelabel havelabel (named-gensy s))))
      (if (not havelabel) (put! relabels s newlabel))
      `(,(car e) ,newlabel)))
   (else
    (cons (car e)
          (map (lambda (x) (rename-symbolic-labels- x relabels parent-scope))
               (cdr e))))))

(define (rename-symbolic-labels e)
  (rename-symbolic-labels- e (table) '()))

;; macro expander entry point

;; TODO: delete this file and fold this operation into resolve-scopes
(define (julia-expand-macroscope e)
  (julia-expand-macroscopes-
   (rename-symbolic-labels
    (julia-expand-quotes e))))

(define (contains-macrocall e)
  (and (pair? e)
       (contains (lambda (e) (and (pair? e) (eq? (car e) 'macrocall))) e)))

(define (julia-bq-macro x)
  (julia-bq-expand x 0))
