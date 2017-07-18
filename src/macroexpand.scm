;; macro expander

;; backquote expansion

(define (splice-expr? e)
  ;; ($ (tuple (... x)))
  (and (length= e 2)          (eq? (car e)   '$)
       (length= (cadr e) 2)   (eq? (caadr e) 'tuple)
       (vararg? (cadadr e))))

(define (wrap-with-splice x)
  `(call (core _expr) (inert $)
         (call (core _expr) (inert tuple)
               (call (core _expr) (inert |...|) ,x))))

(define (julia-bq-bracket x d)
  (if (splice-expr? x)
      (if (= d 0)
          (cadr (cadr (cadr x)))
          (list 'call '(top vector_any)
                (wrap-with-splice (julia-bq-expand (cadr (cadr (cadr x))) (- d 1)))))
      (list 'call '(top vector_any) (julia-bq-expand x d))))

(define (julia-bq-expand x d)
  (cond ((or (eq? x 'true) (eq? x 'false))  x)
        ((or (symbol? x) (ssavalue? x))     (list 'inert x))
        ((atom? x)  x)
        ((eq? (car x) 'quote)
         `(call (core _expr) (inert quote) ,(julia-bq-expand (cadr x) (+ d 1))))
        ((eq? (car x) '$)
         (if (and (= d 0) (length= x 2))
             (cadr x)
             (if (splice-expr? (cadr x))
                 `(call (core splicedexpr) (inert $)
                        (call (top append_any) ,(julia-bq-bracket (cadr x) (- d 1))))
                 `(call (core _expr) (inert $) ,(julia-bq-expand (cadr x) (- d 1))))))
        ((not (contains (lambda (e) (and (pair? e) (eq? (car e) '$))) x))
         `(copyast (inert ,x)))
        ((not (any splice-expr? x))
         `(call (core _expr) ,.(map (lambda (ex) (julia-bq-expand ex d)) x)))
        (else
         (let loop ((p (cdr x)) (q '()))
           (if (null? p)
               (let ((forms (reverse q)))
                 `(call (core splicedexpr) ,(julia-bq-expand (car x) d)
                        (call (top append_any) ,@forms)))
               (loop (cdr p) (cons (julia-bq-bracket (car p) d) q)))))))

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
                   (cons 'varlist (safe-llist-positional-args (fix-arglist argl))))
   (pattern-lambda (function (where (-$ (call name . argl) (|::| (call name . argl) _t)) . wheres) body)
                   (cons 'varlist (append (safe-llist-positional-args (fix-arglist argl))
                                          (typevar-names wheres))))

   (pattern-lambda (function (tuple . args) body)
                   `(-> (tuple ,@args) ,body))

   ;; expression form function definition
   (pattern-lambda (= (call (curly name . sparams) . argl) body)
                   `(function (call (curly ,name . ,sparams) . ,argl) ,body))
   (pattern-lambda (= (-$ (call name . argl) (|::| (call name . argl) _t)) body)
                   `(function (call ,name ,@argl) ,body))
   (pattern-lambda (= (where (-$ (call name . argl) (|::| (call name . argl) _t)) . wheres) body)
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
                            ((eventually-call (cadar binds))
                             ;; f()=c
                             (let ((asgn (cadr (julia-expand0 (car binds)))))
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
   (pattern-lambda (type mut (<: (curly tn . tvars) super) body)
                   (list* 'varlist (cons (unescape tn) (unescape tn)) '(new . new)
                          (typevar-names tvars)))
   (pattern-lambda (type mut (curly tn . tvars) body)
                   (list* 'varlist (cons (unescape tn) (unescape tn)) '(new . new)
                          (typevar-names tvars)))
   (pattern-lambda (type mut (<: tn super) body)
                   (list 'varlist (cons (unescape tn) (unescape tn)) '(new . new)))
   (pattern-lambda (type mut tn body)
                   (list 'varlist (cons (unescape tn) (unescape tn)) '(new . new)))

   )) ; vars-introduced-by-patterns

(define keywords-introduced-by-patterns
  (pattern-set
   (pattern-lambda (function (call (curly name . sparams) . argl) body)
                   (cons 'varlist (safe-llist-keyword-args (fix-arglist argl))))

   (pattern-lambda (function (-$ (call name . argl) (|::| (call name . argl) _t)) body)
                   (cons 'varlist (safe-llist-keyword-args (fix-arglist argl))))
   (pattern-lambda (function (where (-$ (call name . argl) (|::| (call name . argl) _t)) . wheres) body)
                   (cons 'varlist (safe-llist-keyword-args (fix-arglist argl))))

   (pattern-lambda (= (call (curly name . sparams) . argl) body)
                   `(function (call (curly ,name . ,sparams) . ,argl) ,body))
   (pattern-lambda (= (-$ (call name . argl) (|::| (call name . argl) _t)) body)
                   `(function (call ,name ,@argl) ,body))
   (pattern-lambda (= (where (-$ (call name . argl) (|::| (call name . argl) _t)) . wheres) body)
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
  (cond ((and (symbol? v) (not (eq? v 'true)) (not (eq? v 'false)))
         (list v))
        ((atom? v) '())
        (else
         (case (car v)
           ((... kw |::|) (try-arg-name (cadr v)))
           ((escape) (list v))
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
  (safe-arg-names
   (filter (lambda (a) (not (and (pair? a)
                                 (eq? (car a) 'parameters))))
           lst)
   escaped))

;; arg names from keyword arguments, and positional arguments with escaped names
(define (safe-llist-keyword-args lst)
  (let ((kwargs (apply nconc
                       (map cdr
                            (filter (lambda (a) (and (pair? a) (eq? (car a) 'parameters)))
                                    lst)))))
    (append
     (safe-arg-names kwargs #f)
     (safe-arg-names kwargs #t)
     ;; count escaped argument names as "keywords" to prevent renaming
     (safe-llist-positional-args lst #t))))

;; resolve-expansion-vars-with-new-env, but turn on `inarg` once we get inside
;; the formal argument list. `e` in general might be e.g. `(f{T}(x)::T) where T`,
;; and we want `inarg` to be true for the `(x)` part.
(define (resolve-in-function-lhs e env m inarg)
  (define (recur x) (resolve-in-function-lhs x env m inarg))
  (define (other x) (resolve-expansion-vars-with-new-env x env m inarg))
  (case (car e)
    ((where) `(where ,(recur (cadr e)) ,@(map other (cddr e))))
    ((|::|)  `(|::| ,(recur (cadr e)) ,(other (caddr e))))
    ((call)  `(call ,(other (cadr e))
                    ,@(map (lambda (x)
                             (resolve-expansion-vars-with-new-env x env m #t))
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
                      (append!
                       (pair-with-gensyms v)
                       (map (lambda (v) (cons v v))
                            (diff (keywords-introduced-by x) globals))))
              env)))))))

(define (resolve-expansion-vars-with-new-env x env m inarg (outermost #f))
  (resolve-expansion-vars-
   x
   (if (and (pair? x) (eq? (car x) 'let))
       ;; let is strange in that it needs both old and new envs within
       ;; the same expression
       env
       (new-expansion-env-for x env outermost))
   m inarg))

(define (resolve-expansion-vars- e env m inarg)
  (cond ((or (eq? e 'true) (eq? e 'false) (eq? e 'end) (eq? e 'ccall))
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
           ((escape) (cadr e))
           ((global) (let ((arg (cadr e)))
                       (cond ((symbol? arg) e)
                             ((assignment? arg)
                              `(global
                                (= ,(unescape (cadr arg))
                                   ,(resolve-expansion-vars-with-new-env (caddr arg) env m inarg))))
                             (else
                              `(global ,(resolve-expansion-vars-with-new-env arg env m inarg))))))
           ((using import importall export meta line inbounds boundscheck simdloop) (map unescape e))
           ((macrocall)
            (if (or (eq? (cadr e) '@label) (eq? (cadr e) '@goto)) e
                `(macrocall ,.(map (lambda (x)
                                     (resolve-expansion-vars-with-new-env x env m inarg))
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
            (if (and (pair? (cadr e)) (function-def? e))
                ;; in (kw x 1) inside an arglist, the x isn't actually a kwarg
                `(,(car e) ,(resolve-in-function-lhs (cadr e) env m inarg)
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
                           (unescape (cadr (cadr e))))
                      ,(resolve-expansion-vars- (caddr (cadr e)) env m inarg))
                     ,(resolve-expansion-vars- (caddr e) env m inarg))
                `(kw ,(if inarg
                          (resolve-expansion-vars- (cadr e) env m inarg)
                          (unescape (cadr e)))
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
        ((eq? (car e) '|::|)   (decl-var* (cadr e)))
        ((eq? (car e) 'where)  (decl-var* (cadr e)))
        (else                  (decl-var e))))

(define (decl-vars* e)
  (if (and (pair? e) (eq? (car e) 'tuple))
      (apply append (map decl-vars* (cdr e)))
      (list (decl-var* e))))

(define (function-def? e)
  (and (pair? e) (or (eq? (car e) 'function) (eq? (car e) '->)
                     (and (eq? (car e) '=) (length= e 3)
                          (eventually-call (cadr e))))))

(define (find-declared-vars-in-expansion e decl (outer #t))
  (cond ((or (not (pair? e)) (quoted? e)) '())
        ((eq? (car e) 'escape)  '())
        ((eq? (car e) decl)     (map decl-var* (cdr e)))
        ((and (not outer) (function-def? e)) '())
        (else
         (apply append! (map (lambda (x)
                               (find-declared-vars-in-expansion x decl #f))
                             e)))))

(define (find-assigned-vars-in-expansion e (outer #t))
  (cond ((or (not (pair? e)) (quoted? e))  '())
        ((eq? (car e) 'escape)  '())
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
        (else
         (apply append! (map (lambda (x)
                               (find-assigned-vars-in-expansion x #f))
                             e)))))

(define (keywords-introduced-by e)
  (let ((v (pattern-expand1 keywords-introduced-by-patterns e)))
    (if (and (pair? v) (eq? (car v) 'varlist))
        (cdr v)
        '())))

(define (resolve-expansion-vars e m)
  ;; expand binding form patterns
  ;; keep track of environment, rename locals to gensyms
  ;; and wrap globals in (getfield module var) for macro's home module
  (resolve-expansion-vars-with-new-env e '() m #f #t))

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

;; macro expander entry point

(define (julia-expand-macros e (max-depth -1))
  (cond ((= max-depth 0)   e)
        ((not (pair? e))     e)
        ((eq? (car e) 'quote)
         ;; backquote is essentially a built-in macro at the moment
         (julia-expand-macros (julia-bq-expand (cadr e) 0) max-depth))
        ((eq? (car e) 'inert) e)
        ((eq? (car e) 'macrocall)
         ;; expand macro
         (let ((form (apply invoke-julia-macro (cadr e) (cddr e))))
           (if (not form)
               (error (string "macro \"" (cadr e) "\" not defined")))
           (if (and (pair? form) (eq? (car form) 'error))
               (error (cadr form)))
           (let ((form (car form))
                 (m    (cdr form)))
             ;; m is the macro's def module
             (rename-symbolic-labels
              (julia-expand-macros (resolve-expansion-vars form m) (- max-depth 1))))))
        ((eq? (car e) 'module) e)
        (else
         (map (lambda (ex) (julia-expand-macros ex max-depth)) e))))
