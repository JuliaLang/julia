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

(define (julia-bq-expand-hygienic x unhygienic)
  (let ((expanded (julia-bq-expand x 0)))
    (if unhygienic expanded `(escape ,expanded))))


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
    (julia-expand-macros- e '() max-depth))

(define (julia-expand-macros- e m max-depth)
  (cond ((= max-depth 0)   e)
        ((not (pair? e)) e)
        ((eq? (car e) 'quote)
         ;; backquote is essentially a built-in unhygienic macro at the moment
         (julia-expand-macros- (julia-bq-expand-hygienic (cadr e) (null? m)) m max-depth))
        ((eq? (car e) 'inert) e)
        ((eq? (car e) 'macrocall)
         ;; expand macro
         (let ((form (apply invoke-julia-macro (if (null? m) 'false (car m)) (cdr e))))
           (if (not form)
               (error (string "macro \"" (cadr e) "\" not defined")))
           (if (and (pair? form) (eq? (car form) 'error))
               (error (cadr form)))
           (let* ((modu (cdr form)) ;; modu is the macro's def module
                  (form (car form)) ;; form is the expression returned from expand-macros
                  (form (julia-expand-macros- (rename-symbolic-labels form) (cons modu m) (- max-depth 1))))
             (if (and (pair? form) (eq? (car form) 'escape))
                 (cadr form) ; immediately fold away (hygienic-scope (escape ...))
                 (if (and (pair? form) (eq? (car form) 'toplevel)) ; swap toplevel and hygienic-scope
                     `(toplevel ,@(map (lambda (e)
                         (if (and (pair? e) (eq? (car e) 'escape))
                             (cadr e)
                             `(hygienic-scope ,e ,modu)))
                         (cdr form)))
                     `(hygienic-scope ,form ,modu))))))
        ((eq? (car e) 'module) e)
        ((eq? (car e) 'toplevel) e)
        ((eq? (car e) 'escape)
         (let ((m (if (null? m) m (cdr m))))
           `(escape ,(julia-expand-macros- (cadr e) m max-depth))))
        (else
         (map (lambda (ex)
                (julia-expand-macros- ex m max-depth))
              e))))
