;; AST utilities

;; deparser

(define (deparse-arglist l (sep ",")) (string.join (map deparse l) sep))

(define (deparse e)
  (cond ((or (symbol? e) (number? e)) (string e))
        ((string? e) (print-to-string e))
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
                ((quote inert)
		 (if (symbol? (cadr e))
		     (string ":" (deparse (cadr e)))
		     (string ":(" (deparse (cadr e)) ")")))
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
		((line) (if (length= e 2)
			    (string "# line " (cadr e))
			    (string "# " (caddr e) ", line " (cadr e))))
		((block)
		 (string "begin\n"
			 (string.join (map (lambda (ex) (string "    " (deparse ex)))
					   (cdr e))
				      "\n")
			 "\nend"))
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

(define make-jlgensym
  (let ((jlgensym-counter 0))
    (lambda ()
      (begin0 `(jlgensym ,jlgensym-counter)
              (set! jlgensym-counter (+ 1 jlgensym-counter))))))

;; predicates and accessors

(define (quoted? e) (memq (car e) '(quote top line break inert)))

(define (lam:args x) (cadr x))
(define (lam:vars x) (llist-vars (lam:args x)))
(define (lam:vinfo x) (caddr x))
(define (lam:body x) (cadddr x))

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

;; convert a lambda list into a list of just symbols
(define (llist-vars lst)
  (map arg-name (filter (lambda (a) (not (and (pair? a)
					      (eq? (car a) 'parameters))))
                        lst)))

(define (llist-keywords lst)
  (apply append
         (map (lambda (a) (if (and (pair? a) (eq? (car a) 'parameters))
			      (map arg-name (cdr a))
			      '()))
              lst)))

;; get just argument types
(define (llist-types lst) (map arg-type lst))

(define (decl? e)
  (and (pair? e) (eq? (car e) '|::|)))

(define (make-decl n t) `(|::| ,n ,t))

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

(define (undot-name e)
  (if (and (pair? e) (eq? (car e) '|.|))
      (cadr (caddr e))
      e))

(define (dotop? o) (and (symbol? o) (eqv? (string.char (string o) 0) #\.)))

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

(define (make-assignment l r) `(= ,l ,r))
(define (assignment? e) (and (pair? e) (eq? (car e) '=)))
(define (return? e) (and (pair? e) (eq? (car e) 'return)))

(define (eq-sym? a b)
  (or (eq? a b) (and (jlgensym? a) (jlgensym? b) (eqv? (cdr a) (cdr b)))))

(define (make-var-info name) (list name 'Any 0))
(define vinfo:name car)
(define vinfo:type cadr)
(define (vinfo:set-type! v t) (set-car! (cdr v) t))

(define (vinfo:capt v) (< 0 (logand (caddr v) 1)))
(define (vinfo:asgn v) (< 0 (logand (caddr v) 2)))
(define (vinfo:const v) (< 0 (logand (caddr v) 8)))
(define (vinfo:sa v) (< 0 (logand (caddr v) 16)))
(define (set-bit x b val) (if val (logior x b) (logand x (lognot b))))
;; record whether var is captured
(define (vinfo:set-capt! v c)  (set-car! (cddr v) (set-bit (caddr v) 1 c)))
;; whether var is assigned
(define (vinfo:set-asgn! v a)  (set-car! (cddr v) (set-bit (caddr v) 2 a)))
;; whether var is assigned by an inner function
(define (vinfo:set-iasg! v a)  (set-car! (cddr v) (set-bit (caddr v) 4 a)))
;; whether var is const
(define (vinfo:set-const! v a) (set-car! (cddr v) (set-bit (caddr v) 8 a)))
;; whether var is assigned once
(define (vinfo:set-sa! v a)    (set-car! (cddr v) (set-bit (caddr v) 16 a)))
;; occurs undef: mask 32
;; whether var is called (occurs in function call head position)
(define (vinfo:set-called! v a)  (set-car! (cddr v) (set-bit (caddr v) 64 a)))

(define var-info-for assq)

(define (assignment? e)
  (and (pair? e) (eq? (car e) '=)))

(define (assignment-like? e)
  (and (pair? e) (is-prec-assignment? (car e))))

(define (kwarg? e)
  (and (pair? e) (eq? (car e) 'kw)))
