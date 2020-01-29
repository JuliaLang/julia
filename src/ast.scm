;; AST utilities

;; deparser

(define (deparse-arglist l (sep ", "))
  (if (has-parameters? l)
      (string (string.join (map deparse (cdr l)) sep)
              (if (length= (cdr l) 1) "," "")
              (deparse (car l)))
      (string.join (map deparse l) sep)))

(define (deparse-prefix-call head args opn cls)
  (string (if (or (decl? head) (eq? head ':))
              (string "(" (deparse head) ")")
              (deparse head))
          opn (deparse-arglist args) cls))

(define (deparse-generator e)
  (if (eq? (car e) 'flatten)
      (deparse-flatten e '())
      (string (deparse (cadr e)) " for " (deparse-arglist (cddr e) ", "))))

(define (deparse-flatten e iters (flat #t))
  (cond ((and flat (pair? e) (eq? (car e) 'generator))
         (deparse-flatten (cadr e) (cons (deparse-arglist (cddr e) ", ") iters) #f))
        ((and flat (pair? e) (eq? (car e) 'flatten))
         (let ((e (cadr e)))
           (deparse-flatten (cadr e) (cons (deparse-arglist (cddr e) ", ") iters) #t)))
        (else
         (string (deparse e) " for " (string.join (reverse iters) " for ")))))

(define (indented-block lst ilvl)
  (string
   (string.join (map (lambda (ex) (string (string.rep "    " (+ ilvl 1))
                                          (deparse ex (+ ilvl 1))))
                     lst)
                "\n")
   (if (null? lst) "" "\n")))

(define (deparse-block head lst ilvl)
  (string head "\n" (indented-block lst ilvl)
          (string.rep "    " ilvl) "end"))

(define (deparse-colon-dot e)
  (if (dotop? e)
      (string ":" (deparse e))
      (deparse e)))

(define (deparse e (ilvl 0))
  (cond ((or (symbol? e) (number? e)) (string e))
        ((string? e) (print-to-string e))
        ((eq? (typeof e) 'julia_value)
         (let ((s (string e)))
           (if (string.find s "#<julia: ")
               ;; successfully printed as a julia value
               (string.sub s 9 (string.dec s (length s)))
               s)))
        ((char? e) (string "'" e "'"))
        ((atom? e) (string e))
        ((eq? (car e) '|.|)
         (string (deparse (cadr e)) '|.|
                 (cond ((and (pair? (caddr e)) (memq (caaddr e) '(quote inert)))
                        (deparse-colon-dot (cadr (caddr e))))
                       ((and (pair? (caddr e)) (eq? (caaddr e) 'copyast))
                        (deparse-colon-dot (cadr (cadr (caddr e)))))
                       (else
                        (string #\( (deparse (caddr e)) #\))))))
        ((memq (car e) '(... |'|))
         (string (deparse (cadr e)) (car e)))
        ((or (syntactic-op? (car e)) (eq? (car e) '|<:|) (eq? (car e) '|>:|))
         (if (length= e 2)
             (string (car e) (deparse (cadr e)))
             (string (deparse (cadr e)) " " (car e) " " (deparse (caddr e)))))
        (else
         (case (car e)
           ((null)  "nothing")
           ((true)  "true")
           ((false) "false")
           ;; calls and operators
           ((call)
            (cond ((and (eq? (cadr e) ':) (or (length= e 4) (length= e 5)))
                   (string (deparse (caddr e)) ': (deparse (cadddr e))
                           (if (length> e 4)
                               (string ': (deparse (caddddr e)))
                               "")))
                  ((and (length= e 4) (operator? (cadr e)))
                   (string #\( (deparse (caddr e)) " " (cadr e) " " (deparse (cadddr e)) #\) ))
                  (else
                   (deparse-prefix-call (cadr e) (cddr e) #\( #\)))))
           (($ &)          (if (and (pair? (cadr e)) (not (memq (caadr e) '(outerref null true false))))
                               (string (car e) "(" (deparse (cadr e)) ")")
                               (string (car e) (deparse (cadr e)))))
           ((|::|)         (if (length= e 2)
                               (string (car e) (deparse (cadr e)))
                               (string (deparse (cadr e)) (car e) (deparse (caddr e)))))
           ((comparison) (string.join (map deparse (cdr e)) " "))
           ((macrocall) (string (cadr e) " " (deparse-arglist (cddr e) " ")))
           ((kw)        (string (deparse (cadr e)) " = " (deparse (caddr e))))
           ((where)     (string (deparse (cadr e)) " where "
                                (if (length= e 3)
                                    (deparse (caddr e))
                                    (deparse (cons 'braces (cddr e))))))
           ((parameters) (string "; " (deparse-arglist (cdr e))))
           ;; bracket forms
           ((tuple)
            (string #\( (deparse-arglist (cdr e))
                    (if (length= e 2) #\, "")
                    #\)))
           ((ref)   (deparse-prefix-call (cadr e) (cddr e) #\[ #\]))
           ((curly) (deparse-prefix-call (cadr e) (cddr e) #\{ #\}))
           ((vect)  (string #\[ (deparse-arglist (cdr e) ", ") #\]))
           ((vcat)  (string #\[ (deparse-arglist (cdr e) "; ") #\]))
           ((typed_vcat)  (string (deparse (cadr e))
                                  (deparse (cons 'vcat (cddr e)))))
           ((hcat)        (string #\[ (deparse-arglist (cdr e) " ") #\]))
           ((typed_hcat)  (string (deparse (cadr e))
                                  (deparse (cons 'hcat (cddr e)))))
           ((row)        (deparse-arglist (cdr e) " "))
           ((braces)     (string #\{ (deparse-arglist (cdr e) ", ") #\}))
           ((bracescat)  (string #\{ (deparse-arglist (cdr e) "; ") #\}))
           ((string)
            (string #\"
                    (string.join
                     (map (lambda (s)
                            (cond ((string? s) s)
                                  ((symbol? s) (string "$" s))
                                  (else (string "$(" (deparse s) ")"))))
                          (cdr e))
                     "")
                    #\"))
           ;; comprehensions and generators
           ((generator)     (string "(" (deparse-generator e) ")"))
           ((flatten)       (string "(" (deparse-generator e) ")"))
           ((filter)        (string (deparse (caddr e)) " if " (deparse (cadr e))))
           ((comprehension) (string "[ " (deparse-generator (cadr e)) " ]"))
           ((typed_comprehension)
            (string (deparse (cadr e))
                    (deparse (cons 'comprehension (cddr e)))))
           ;; block forms
           ((block)    (deparse-block "begin" (cdr e) ilvl))
           ((toplevel) (string.join (map deparse (cdr e)) "\n"))
           ((function macro for while)
            (define (block-stmts e)
              (if (and (pair? e) (eq? (car e) 'block))
                  (cdr e)
                  (list e)))
            (deparse-block (string (car e) " " (deparse (cadr e)))
                           (block-stmts (caddr e))
                           ilvl))
           ((return)         (string "return " (deparse (cadr e))))
           ((break continue) (string (car e)))
           ((if elseif)
            (define (if-cond e)
              (if (eq? (car e) 'elseif)
                  (caddr (cadr e))
                  (cadr e)))
            (if (length= e 3)
                (deparse-block (string (car e) " " (deparse (if-cond e)))
                               (cdr (caddr e)) ilvl)
                (string (car e) " " (deparse (if-cond e)) "\n"
                        (indented-block (cdr (caddr e)) ilvl)
                        (let ((els (cadddr e)))
                          (if (and (pair? els) (eq? (car els) 'elseif))
                              (deparse els)
                              (deparse-block "else" (cdr els) ilvl))))))
           ((let)
            (deparse-block (string "let " (string.join (map deparse (cdadr e)) ", "))
                           (cdr (caddr e)) ilvl))
           ((try)
            (string "try\n"
                    (indented-block (cdr (cadr e)) ilvl)
                    (if (and (pair? (cadddr e)) (eq? (car (cadddr e)) 'block))
                        (string (string.rep "    " ilvl) "catch"
                                (if (equal? (caddr e) '(false))
                                    ""
                                    (string " " (caddr e)))
                                "\n"
                                (indented-block (cdr (cadddr e)) ilvl))
                        "")
                    (if (length> e 4)
                        (let ((fin (caddddr e)))
                          (if (and (pair? fin) (eq? (car fin) 'block))
                              (string (string.rep "    " ilvl) "finally\n"
                                      (indented-block (cdr fin) ilvl))
                              ""))
                        "")
                    (string.rep "    " ilvl) "end"))
	   ((do)
	    (let ((call (cadr e))
		  (args (cdr (cadr (caddr e))))
		  (body (caddr (caddr e))))
	      (deparse-block (string (deparse call) " do" (if (null? args) "" " ")
				     (deparse-arglist args))
			     (cdr body) ilvl)))
           ((struct)
            (string (if (equal? (cadr e) '(true)) "mutable " "")
                    "struct "
                    (deparse-block (deparse (caddr e)) (cdr (cadddr e)) ilvl)))
           ((abstract)
            (string "abstract type " (deparse (cadr e)) " end"))
           ((primitive)
            (string "primitive type " (deparse (cadr e)) " " (deparse (caddr e)) " end"))
           ((module)
            (string (if (equal? (cadr e) '(true)) "module " "baremodule ")
                    (caddr e) "\n"
                    (string.join (map deparse (cdr (cadddr e))) "\n") "\n"
                    "end"))
           ;; misc syntax forms
           ((import using)
            (define (deparse-path e)
              (cond ((and (pair? e) (eq? (car e) '|.|))
                     (let loop ((lst   (cdr e))
                                (ndots 0))
                       (if (or (null? lst)
                               (not (eq? (car lst) '|.|)))
                           (string (string.rep "." ndots)
                                   (string.join (map deparse lst) "."))
                           (loop (cdr lst) (+ ndots 1)))))
                    ((and (pair? e) (eq? (car e) ':))
                     (string (deparse-path (cadr e)) ": "
                             (string.join (map deparse-path (cddr e)) ", ")))
                    (else
                     (string e))))
            (string (car e) " " (string.join (map deparse-path (cdr e)) ", ")))
           ((global local export) (string (car e) " " (string.join (map deparse (cdr e)) ", ")))
           ((const)        (string "const " (deparse (cadr e))))
           ((top)          (deparse (cadr e)))
           ((core)         (string "Core." (deparse (cadr e))))
           ((globalref)    (string (deparse (cadr e)) "." (deparse-colon-dot (caddr e))))
           ((outerref)     (string (deparse (cadr e))))
           ((ssavalue)     (string "SSAValue(" (cadr e) ")"))
           ((line)         (if (length= e 2)
                               (string "# line " (cadr e))
                               (string "# " (caddr e) ", line " (cadr e))))
           ((copyast)      (deparse (cadr e)))
           ((quote inert)
            (if (and (symbol? (cadr e))
                     (not (memv (string.char (string (cadr e)) 0)
                                '(#\= #\:))))
                (string ":" (deparse (cadr e)))
                (string ":(" (deparse (cadr e)) ")")))
           (else
            (string e))))))

;; custom gensyms

(define *gensyms* '())
(define *current-gensyms* '())
(define *gensy-counter* 1)
(define (gensy)
  (if (null? *current-gensyms*)
      (let ((g (symbol (string "#s" *gensy-counter*))))
        (set! *gensy-counter* (+ *gensy-counter* 1))
        (set! *gensyms* (cons g *gensyms*))
        g)
      (begin0 (car *current-gensyms*)
              (set! *current-gensyms* (cdr *current-gensyms*)))))
(define (named-gensy name)
  (let ((g (symbol (string "#" *gensy-counter* "#" name))))
    (set! *gensy-counter* (+ *gensy-counter* 1))
    g))
(define (reset-gensyms)
  (set! *current-gensyms* *gensyms*))

(define (some-gensym? x)
  (or (gensym? x) (memq x *gensyms*)))

(define make-ssavalue
  (let ((ssavalue-counter 0))
    (lambda ()
      (begin0 `(ssavalue ,ssavalue-counter)
              (set! ssavalue-counter (+ 1 ssavalue-counter))))))

;; predicates and accessors

(define (quoted? e)
  (memq (car e) '(quote top core globalref outerref line break inert meta inbounds loopinfo)))
(define (quotify e) `',e)
(define (unquote e)
  (if (and (pair? e) (memq (car e) '(quote inert)))
      (cadr e)
      e))

(define (lam:args x) (cadr x))
(define (lam:vars x) (llist-vars (lam:args x)))
(define (lam:vinfo x) (caddr x))
(define (lam:body x) (cadddr x))
(define (lam:sp x) (cadddr (lam:vinfo x)))

(define (bad-formal-argument v)
  (error (string #\" (deparse v) #\" " is not a valid function argument name")))

(define (valid-name? s)
  (not (memq s '(ccall cglobal))))

(define (arg-name v)
  (cond ((and (symbol? v) (valid-name? v))
         v)
        ((not (pair? v))
         (bad-formal-argument v))
        (else
         (case (car v)
           ((... kw)
	    (arg-name (cadr v)) ;; to check for errors
	    (decl-var (cadr v)))
           ((|::|)
            (if (not (symbol? (cadr v)))
                (bad-formal-argument (cadr v)))
            (decl-var v))
           ((meta)  ;; allow certain per-argument annotations
            (if (nospecialize-meta? v #t)
                (arg-name (caddr v))
                (bad-formal-argument v)))
           (else (bad-formal-argument v))))))

(define (arg-type v)
  (cond ((symbol? v)  '(core Any))
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
           ((meta)  ;; allow certain per-argument annotations
            (if (nospecialize-meta? v #t)
                (arg-type (caddr v))
                (bad-formal-argument v)))
           (else (bad-formal-argument v))))))

;; convert a lambda list into a list of just symbols
(define (llist-vars lst)
  (map arg-name (filter (lambda (a) (not (and (pair? a)
                                              (eq? (car a) 'parameters))))
                        lst)))

;; get just argument types
(define (llist-types lst) (map arg-type lst))

(define (decl? e)
  (and (pair? e) (eq? (car e) '|::|)))

(define (make-decl n t) `(|::| ,n ,t))

(define (ssavalue? e)
  (and (pair? e) (eq? (car e) 'ssavalue)))

(define (globalref? e)
  (and (pair? e) (eq? (car e) 'globalref)))

(define (symbol-like? e)
  (or (symbol? e) (ssavalue? e)))

(define (simple-atom? x)
  (or (number? x) (string? x) (char? x)
      (and (pair? x) (memq (car x) '(ssavalue null true false)))
      (eq? (typeof x) 'julia_value)))

;; identify some expressions that are safe to repeat
(define (effect-free? e)
  (or (not (pair? e)) (ssavalue? e) (sym-dot? e) (quoted? e) (memq (car e) '(null true false))))

;; get the variable name part of a declaration, x::int => x
(define (decl-var v)
  (if (decl? v) (cadr v) v))

(define (decl-type v)
  (if (decl? v) (caddr v) '(core Any)))

(define (sym-dot? e)
  (and (length= e 3) (eq? (car e) '|.|)
       (symbol-like? (cadr e))))

(define (undot-name e)
  (if (and (pair? e) (eq? (car e) '|.|))
      (cadr (caddr e))
      e))

(define (identifier-name e)
  (cond ((symbol? e)    e)
        ((globalref? e) (caddr e))
        (else           e)))

(define (dotop-named? e) (dotop? (identifier-name e)))

;; convert '.xx to 'xx
(define (undotop op)
  (if (globalref? op)
      `(globalref ,(cadr op) ,(undotop (caddr op)))
      (let ((str (string op)))
        (assert (eqv? (string.char str 0) #\.))
        (symbol (string.sub str 1 (length str))))))

;; raise an error for using .op as a function name
(define (check-dotop e)
  (if (dotop-named? e)
      (error (string "invalid function name \"" (deparse e) "\""))
      (if (pair? e)
          (if (eq? (car e) '|.|)
              (check-dotop (caddr e))
              (if (quoted? e)
                  (check-dotop (cadr e))))))
  e)

(define (vararg? x) (and (pair? x) (eq? (car x) '...)))
(define (vararg-type-expr? x)
  (or (eq? x 'Vararg)
      (and (length> x 1)
           (or (and (eq? (car x) 'curly)
                    (vararg-type-expr? (cadr x)))
               (and (eq? (car x) 'where)
                    (vararg-type-expr? (cadr x)))))))
(define (varargexpr? x)
  (and (pair? x)
       (eq? (car x) '::)
       (vararg-type-expr? (caddr x))))
(define (linenum? x) (and (pair? x) (eq? (car x) 'line)))

(define (make-assignment l r) `(= ,l ,r))
(define (assignment? e) (and (pair? e) (eq? (car e) '=)))
(define (return? e) (and (pair? e) (eq? (car e) 'return)))
(define (complex-return? e) (and (return? e)
                                 (let ((x (cadr e)))
                                   (not (simple-atom? x)))))

(define (eq-sym? a b)
  (or (eq? a b) (and (ssavalue? a) (ssavalue? b) (eqv? (cdr a) (cdr b)))))

(define (blockify e)
  (if (and (pair? e) (eq? (car e) 'block))
      (if (null? (cdr e))
          `(block (null))
          e)
      `(block ,e)))

(define (make-var-info name) (list name '(core Any) 0))
(define vinfo:name car)
(define vinfo:type cadr)
(define (vinfo:set-type! v t) (set-car! (cdr v) t))

(define (vinfo:capt v) (< 0 (logand (caddr v) 1)))
(define (vinfo:asgn v) (< 0 (logand (caddr v) 2)))
(define (vinfo:never-undef v) (< 0 (logand (caddr v) 4)))
(define (vinfo:read v) (< 0 (logand (caddr v) 8)))
(define (vinfo:sa v) (< 0 (logand (caddr v) 16)))
(define (set-bit x b val) (if val (logior x b) (logand x (lognot b))))
;; record whether var is captured
(define (vinfo:set-capt! v c)  (set-car! (cddr v) (set-bit (caddr v) 1 c)))
;; whether var is assigned
(define (vinfo:set-asgn! v a)  (set-car! (cddr v) (set-bit (caddr v) 2 a)))
;; whether the assignments to var are known to dominate its usages
(define (vinfo:set-never-undef! v a) (set-car! (cddr v) (set-bit (caddr v) 4 a)))
;; whether var is ever read
(define (vinfo:set-read! v a) (set-car! (cddr v) (set-bit (caddr v) 8 a)))
;; whether var is assigned once
(define (vinfo:set-sa! v a)    (set-car! (cddr v) (set-bit (caddr v) 16 a)))
;; occurs undef: mask 32
;; whether var is called (occurs in function call head position)
(define (vinfo:set-called! v a)  (set-car! (cddr v) (set-bit (caddr v) 64 a)))

(define var-info-for assq)

(define (assignment-like? e)
  (and (pair? e) (is-prec-assignment? (car e))))

(define (kwarg? e)
  (and (pair? e) (eq? (car e) 'kw)))

(define (nospecialize-meta? e (one #f))
  (and (if one (length= e 3) (length> e 2))
       (eq? (car e) 'meta) (memq (cadr e) '(nospecialize specialize))))

(define (if-generated? e)
  (and (length= e 4) (eq? (car e) 'if) (equal? (cadr e) '(generated))))

(define (generated-meta? e)
  (and (length= e 3) (eq? (car e) 'meta) (eq? (cadr e) 'generated)))

(define (generated_only-meta? e)
  (and (length= e 2) (eq? (car e) 'meta) (eq? (cadr e) 'generated_only)))

(define (function-def? e)
  (and (pair? e) (or (eq? (car e) 'function) (eq? (car e) '->)
                     (and (eq? (car e) '=) (length= e 3)
                          (eventually-call? (cadr e))))))

;; flatten nested expressions with the given head
;; (op (op a b) c) => (op a b c)
(define (flatten-ex head e)
  (if (atom? e)
      e
      (cons (car e)
            (apply append!
                   (map (lambda (x)
                          (if (and (pair? x) (eq? (car x) head))
                              (cdr (flatten-ex head x))
                              (list x)))
                        (cdr e))))))

(define (flatten-blocks e) (flatten-ex 'block e))
