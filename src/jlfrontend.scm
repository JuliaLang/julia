(load "./flisp/aliases.scm")
(load "./flisp/profile.scm")
(load "utils.scm")
(load "ast.scm")
(load "match.scm")
(load "macroexpand.scm")
(load "julia-parser.scm")
(load "julia-syntax.scm")


;; exception handler to turn known errors into special expressions,
;; to prevent throwing an exception past a C caller.
(define (error-wrap thk)
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
           ;;(newline)
           ;;(display "unexpected error: ")
           ;;(prn e)
           ;;(print-stack-trace (stacktrace))
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
                ((scope-block)
                 ;; TODO: when deprecation for implicit global assignment inside loops
                 ;; is removed, remove this code and just return `tab` in this case
                 (let ((tab2 (table)))
                   (find-possible-globals- (cadr e) tab2)
                   (for-each (lambda (v) (if (has? tab2 v) (del! tab2 v)))
                             (append (find-local-decls (cadr e)) (find-local-def-decls (cadr e))))
                   (for-each (lambda (v) (put! tab v #t))
                             (table.keys tab2))))
                ((break-block)  (find-possible-globals- (caddr e) tab))
                ((module toplevel) '())
                (else
                 (for-each (lambda (x) (find-possible-globals- x tab))
                           (cdr e))))
              tab)))

;; find variables that should be forced to be global in a toplevel expr
(define (find-possible-globals e)
  (table.keys (find-possible-globals- e (table))))

;; this is overwritten when we run in actual julia
(define (defined-julia-global v) #f)

(define (some-gensym? x)
  (or (gensym? x) (memq x *gensyms*)))


;; return a lambda expression representing a thunk for a top-level expression
;; note: expansion of stuff inside module is delayed, so the contents obey
;; toplevel expansion order (don't expand until stuff before is evaluated).
(define (expand-toplevel-expr-- e)
  (let ((ex0 (julia-expand-macroscope e)))
    (if (and (pair? ex0) (eq? (car ex0) 'toplevel))
        ex0
        (let* ((ex (julia-expand0 ex0))
               (lv (find-decls 'local ex))
               (gv (diff (delete-duplicates
                           (append (find-decls 'const ex) ;; convert vars declared const outside any scope block to outer-globals
                                   (find-decls 'global ex) ;; convert vars declared global outside any scope block to outer-globals
                                   ;; vars assigned at the outer level
                                   (filter (lambda (x) (not (some-gensym? x)))
                                           (find-assigned-vars ex '()))))
                         lv))
               ;; vars assigned anywhere, if they have not been explicitly defined
               (existing-gv (filter (lambda (x) (and (not (or (memq x lv) (memq x gv))) (defined-julia-global x)))
                                    (find-possible-globals ex)))
               (th (julia-expand1
                    `(lambda () ()
                             (scope-block
                              (block ,@(map (lambda (v) `(implicit-global ,v)) existing-gv)
                                     ,@(map (lambda (v) `(implicit-global ,v)) gv)
                                     ,ex))))))
          (if (and (null? (cdadr (caddr th)))
                   (and (length= (lam:body th) 2)
                        (let ((retval (cadadr (lam:body th))))
                          (or (and (pair? retval) (eq? (car retval) 'lambda))
                              (simple-atom? retval)))))
              ;; generated functions use the pattern (body (return (lambda ...))), which
              ;; needs to be unwrapped to just the lambda (CodeInfo).
              (cadadr (lam:body th))
              `(thunk ,th))))))

(define *in-expand* #f)

(define (toplevel-only-expr? e)
  (and (pair? e)
       (or (memq (car e) '(toplevel line module import using export
                                    error incomplete))
           (and (eq? (car e) 'global) (every symbol? (cdr e))
                (every (lambda (x) (not (memq x '(true false)))) (cdr e))))))

(define (expand-toplevel-expr e)
  (cond ((or (atom? e) (toplevel-only-expr? e))
         (if (underscore-symbol? e)
             (error "all-underscore identifier used as rvalue"))
         e)
        (else
         (let ((last *in-expand*))
           (if (not last)
               (begin (reset-gensyms)
                      (set! *in-expand* #t)))
           (begin0 (expand-toplevel-expr-- e)
                   (set! *in-expand* last))))))

;; construct default definitions of `eval` for non-bare modules
;; called by jl_eval_module_expr
(define (module-default-defs e)
  (jl-expand-to-thunk
   (let* ((name (caddr e))
          (body (cadddr e))
          (loc  (cadr body))
          (loc  (if (and (pair? loc) (eq? (car loc) 'line))
                    (list loc)
                    '()))
          (x    (if (eq? name 'x) 'y 'x)))
     `(block
       (= (call eval ,x)
          (block
           ,@loc
           (call (core eval) ,name ,x)))
       (= (call include ,x)
          (block
           ,@loc
           (call (top include) ,name ,x)))))))

;; parse only, returning end position, no expansion.
(define (jl-parse-one-string s pos0 greedy)
  (let ((inp (open-input-string s)))
    (io.seek inp pos0)
    (let ((expr (error-wrap (lambda ()
                              (if greedy
                                  (julia-parse inp)
                                  (julia-parse inp parse-atom))))))
      (cons expr (io.pos inp)))))

(define (jl-parse-string s filename)
  (with-bindings ((current-filename (symbol filename)))
    (error-wrap (lambda ()
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
        (let ((lineno (error-wrap
                       (lambda ()
                         (skip-ws-and-comments (ts:port stream))
                         (input-port-line (ts:port stream))))))
          (if (pair? lineno)
              (cons 'toplevel (reverse! (cons lineno exprs)))
              (let ((expr (error-wrap
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

; expand a piece of raw surface syntax to an executable thunk
(define (jl-expand-to-thunk expr)
  (error-wrap (lambda ()
                (expand-toplevel-expr expr))))

(define (jl-expand-to-thunk-stmt expr)
  (jl-expand-to-thunk (if (toplevel-only-expr? expr)
                          expr
                          `(block ,expr (null)))))

(define (jl-expand-macroscope expr)
  (error-wrap (lambda ()
                (julia-expand-macroscope expr))))

; run whole frontend on a string. useful for testing.
(define (fe str)
  (expand-toplevel-expr (julia-parse str)))

(define (profile-e s)
  (with-exception-catcher
   (lambda (e)
           (newline)
           (prn e))
   (lambda () (profile s))))


; --- logging ---
; Utilities for logging messages from the frontend, in a way which can be
; controlled from julia code.

; Log a general deprecation message at line node location `lno`
(define (deprecation-message msg lno)
  (let* ((lf (extract-line-file lno)) (line (car lf)) (file (cadr lf)))
    (frontend-depwarn msg file line)))

; Log a syntax deprecation from line node location `lno`
(define (syntax-deprecation what instead lno)
  (let* ((lf (extract-line-file lno)) (line (car lf)) (file (cadr lf)))
    (deprecation-message (format-syntax-deprecation what instead file line #f) lno)))

; Extract line and file from a line number node, defaulting to (0, none)
; respectively if lno is absent (`#f`) or doesn't contain a file
(define (extract-line-file lno)
  (cond ((or (not lno) (null? lno)) '(0 none))
        ((not (eq? (car lno) 'line)) (error "lno is not a line number node"))
        ((length= lno 2) `(,(cadr lno) none))
        (else (cdr lno))))

(define (format-loc lno)
  (let* ((lf (extract-line-file lno)) (line (car lf)) (file (cadr lf)))
    (format-file-line file line #f)))

(define (format-file-line file line exactloc)
  (if (or (= line 0) (eq? file 'none))
      ""
      (string (if exactloc " at " " around ") file ":" line)))

(define (format-syntax-deprecation what instead file line exactloc)
  (string "Deprecated syntax `" what "`"
          (format-file-line file line exactloc)
          "."
          (if (equal? instead "") ""
              (string #\newline "Use `" instead "` instead."))))

; Corresponds to --depwarn 0="no", 1="yes", 2="error"
(define *depwarn-opt* 1)

; Emit deprecation warning via julia logging layer.
(define (frontend-depwarn msg file line)
  ; (display (string msg "; file = " file "; line = " line #\newline)))
  (case *depwarn-opt*
    (1 (julia-logmsg 1000 'depwarn (symbol (string file line)) file line msg))
    (2 (error msg))))
