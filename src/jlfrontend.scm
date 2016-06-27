(load "./flisp/aliases.scm")
(load "utils.scm")
(load "ast.scm")
(load "match.scm")
(load "macroexpand.scm")
(load "julia-parser.scm")
(load "julia-syntax.scm")

;; exception handler for parser. turns known errors into special expressions,
;; and prevents throwing an exception past a C caller.
(define (parser-wrap thk)
  (with-exception-catcher
   (lambda (e)
     (if (and (pair? e) (eq? (car e) 'error))
         (let ((msg (cadr e))
               (pfx "incomplete:"))
           (if (and (string? msg) (>= (string-length msg) (string-length pfx))
                    (equal? pfx
                            (substring msg 0 (string-length pfx))))
               `(incomplete ,msg)
               e))
         (begin
           (newline)
           (display "unexpected error: ")
           (prn e)
           (print-stack-trace (stacktrace))
           '(error "malformed expression"))))
   thk))

;; assigned variables except those marked local or inside inner functions
(define (find-possible-globals- e tab)
  (cond ((atom? e)   tab)
        ((quoted? e) tab)
        (else (case (car e)
                ((=)            (if (not (ssavalue? (cadr e)))
                                    (put! tab (decl-var (cadr e)) #t))
                                (find-possible-globals- (caddr e) tab))
                ((method)       (let ((n (method-expr-name e)))
                                  (if (symbol? n)
                                      (put! tab n #t)
                                      tab)))
                ((lambda)       tab)
                ((local)        tab)
                ((break-block)  (find-possible-globals- (caddr e) tab))
                ((module toplevel) '())
                (else
                 (for-each (lambda (x) (find-possible-globals- x tab))
                           (cdr e))))
              tab)))

(define (find-possible-globals e)
  (table.keys (find-possible-globals- e (table))))

;; this is overwritten when we run in actual julia
(define (defined-julia-global v) #f)

(define (some-gensym? x)
  (or (gensym? x) (memq x *gensyms*)))

;; find variables that should be forced to be global in a toplevel expr
(define (toplevel-expr-globals e)
  (diff
   (delete-duplicates
    (append
     ;; vars assigned at the outer level
     (filter (lambda (x) (not (some-gensym? x))) (find-assigned-vars e '()))
     ;; vars declared const or global outside any scope block
     (find-decls 'const e)
     (find-decls 'global e)
     ;; vars assigned anywhere, if they have been defined as global
     (filter defined-julia-global (find-possible-globals e))))
   (find-decls 'local e)))

;; return a lambda expression representing a thunk for a top-level expression
;; note: expansion of stuff inside module is delayed, so the contents obey
;; toplevel expansion order (don't expand until stuff before is evaluated).
(define (expand-toplevel-expr-- e)
  (let ((ex0 (julia-expand-macros e)))
    (if (and (pair? ex0) (eq? (car ex0) 'toplevel))
        ex0
        (let* ((ex (julia-expand0 ex0))
               (gv (toplevel-expr-globals ex))
               (th (julia-expand1
                    `(lambda () ()
                             (scope-block
                              (block ,@(map (lambda (v) `(implicit-global ,v)) gv)
                                     ,ex))))))
          (if (and (null? (cdadr (caddr th)))
                   (= 0 (cadddr (caddr th))))
              ;; if no locals, return just body of function
              (cadddr th)
              `(thunk ,th))))))

(define *in-expand* #f)

(define (expand-toplevel-expr e)
  (cond ((or (atom? e)
             (and (pair? e)
                  (or (memq (car e) '(toplevel line module import importall using export
                                               error incomplete))
                      (and (eq? (car e) 'global) (every symbol? (cdr e))))))
         e)
        (else
         (let ((last *in-expand*))
           (if (not last)
               (begin (reset-gensyms)
                      (set! *in-expand* #t)))
           (let ((ex (expand-toplevel-expr-- e)))
             (set! *in-expand* last)
             (if (and (length= ex 2) (eq? (car ex) 'body))
                 ;; (body (return x)) => x
                 (cadadr ex)
                 ex))))))

;; construct default definitions of `eval` for non-bare modules
;; called by jl_eval_module_expr
(define (module-default-defs e)
  (jl-expand-to-thunk
   (let ((name (caddr e))
         (body (cadddr e)))
     (let ((loc (cadr body)))
       `(block
         ,(let ((x (if (eq? name 'x) 'y 'x)))
            `(= (call eval ,x)
                (block
                 ,loc
                 (call (core eval) ,name ,x))))
         (= (call eval m x)
            (block
             ,loc
             (call (core eval) m x))))))))

;; parse only, returning end position, no expansion.
(define (jl-parse-one-string s pos0 greedy)
  (let ((inp (open-input-string s)))
    (io.seek inp pos0)
    (let ((expr (parser-wrap (lambda ()
                               (if greedy
                                   (julia-parse inp)
                                   (julia-parse inp parse-atom))))))
      (cons expr (io.pos inp)))))

(define (jl-parse-string s filename)
  (with-bindings ((current-filename (symbol filename)))
    (parser-wrap (lambda ()
                 (let ((inp  (make-token-stream (open-input-string s))))
                   ;; parse all exprs into a (toplevel ...) form
                   (let loop ((exprs '()))
                     ;; delay expansion so macros run in the Task executing
                     ;; the input, not the task parsing it (issue #2378)
                     ;; used to be (expand-toplevel-expr expr)
                     (let ((expr (julia-parse inp)))
                       (if (eof-object? expr)
                           (cond ((null? exprs)     expr)
                                 ((length= exprs 1) (car exprs))
                                 (else (cons 'toplevel (reverse! exprs))))
                           (if (and (pair? expr) (eq? (car expr) 'toplevel))
                               (loop (nreconc (cdr expr) exprs))
                               (loop (cons expr exprs)))))))))))

(define (jl-parse-all io filename)
  (unwind-protect
   (with-bindings ((current-filename (symbol filename)))
    (let ((stream (make-token-stream io)))
      (let loop ((exprs '()))
        (let ((lineno (parser-wrap
                       (lambda ()
                         (skip-ws-and-comments (ts:port stream))
                         (input-port-line (ts:port stream))))))
          (if (pair? lineno)
              (cons 'toplevel (reverse! (cons lineno exprs)))
              (let ((expr (parser-wrap
                           (lambda ()
                             (julia-parse stream)))))
                (if (eof-object? expr)
                    (cons 'toplevel (reverse! exprs))
                    (let* ((iserr (and (pair? expr) (eq? (car expr) 'error)))
			   (next (list* expr
					;; for error, get most recent line number (#16720)
					(if iserr
					    `(line ,(input-port-line io))
					    `(line ,lineno))
					exprs)))
                      (if iserr
                          (cons 'toplevel (reverse! next))
                          (loop next))))))))))
   (io.close io)))

;; parse file-in-a-string
(define (jl-parse-string-stream str filename)
  (jl-parse-all (open-input-string str) filename))

(define (jl-parse-file filename)
  (trycatch
   (jl-parse-all (open-input-file filename) filename)
   (lambda (e) #f)))

(define *depwarn* #t)
(define (jl-parser-depwarn w)
  (let ((prev *depwarn*))
    (set! *depwarn* (eq? w #t))
    prev))

(define *deperror* #f)
(define (jl-parser-deperror e)
  (let ((prev *deperror*))
    (set! *deperror* (eq? e #t))
    prev))

; expand a piece of raw surface syntax to an executable thunk
(define (jl-expand-to-thunk expr)
  (parser-wrap (lambda ()
                 (expand-toplevel-expr expr))))

; macroexpand only
(define (jl-macroexpand expr)
  (reset-gensyms)
  (parser-wrap (lambda ()
                 (julia-expand-macros expr))))

; run whole frontend on a string. useful for testing.
(define (fe str)
  (expand-toplevel-expr (julia-parse str)))
