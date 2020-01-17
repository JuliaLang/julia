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

;; this is overwritten when we run in actual julia
(define (defined-julia-global v) #f)
(define (julia-current-file) 'none)
(define (julia-current-line) 0)

;; parser entry points

;; parse one expression (if greedy) or atom, returning end position
(define (jl-parse-one s pos0 greedy)
  (let ((inp (open-input-string s)))
    (io.seek inp pos0)
    (let ((expr (error-wrap (lambda ()
                              (if greedy
                                  (julia-parse inp)
                                  (julia-parse inp parse-atom))))))
      (cons expr (io.pos inp)))))

(define (parse-all- io filename)
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
                    (let* ((iserr  (and (pair? expr) (eq? (car expr) 'error)))
                           ;; for error, get most recent line number (#16720)
                           (lineno (if iserr (input-port-line io) lineno))
                           (next   (list* expr
                                          ;; include filename in first line node
                                          (if (null? exprs)
                                              `(line ,lineno ,(symbol filename))
                                              `(line ,lineno))
                                          exprs)))
                      (if iserr
                          (cons 'toplevel (reverse! next))
                          (loop next))))))))))
   (io.close io)))

;; parse all expressions in a string, the same way files are parsed
(define (jl-parse-all str filename)
  (parse-all- (open-input-string str) filename))

(define (jl-parse-file filename)
  (trycatch
   (parse-all- (open-input-file filename) filename)
   (lambda (e) #f)))

;; lowering entry points

;; return a lambda expression representing a thunk for a top-level expression
;; note: expansion of stuff inside module is delayed, so the contents obey
;; toplevel expansion order (don't expand until stuff before is evaluated).
(define (expand-toplevel-expr-- e file line)
  (let ((ex0 (julia-expand-macroscope e)))
    (if (toplevel-only-expr? ex0)
        ex0
        (let* ((ex (julia-expand0 ex0))
               (th (julia-expand1
                    `(lambda () ()
                             (scope-block
                              ,(blockify ex)))
                    file line)))
          (if (and (null? (cdadr (caddr th)))
                   (and (length= (lam:body th) 2)
                        ;; 1-element body might be `return` or `goto` (issue #33227)
                        (return? (cadr (lam:body th)))
                        (let ((retval (cadadr (lam:body th))))
                          (or (and (pair? retval) (eq? (car retval) 'lambda))
                              (simple-atom? retval)))))
              ;; generated functions use the pattern (body (return (lambda ...))), which
              ;; needs to be unwrapped to just the lambda (CodeInfo).
              (cadadr (lam:body th))
              `(thunk ,th))))))

(define (toplevel-only-expr? e)
  (and (pair? e)
       (or (memq (car e) '(toplevel line module import using export
                                    error incomplete))
           (and (memq (car e) '(global const)) (every symbol? (cdr e))))))

(define *in-expand* #f)

(define (expand-toplevel-expr e file line)
  (cond ((or (atom? e) (toplevel-only-expr? e))
         (if (underscore-symbol? e)
             (error "all-underscore identifier used as rvalue"))
         e)
        (else
         (let ((last *in-expand*))
           (if (not last)
               (begin (reset-gensyms)
                      (set! *in-expand* #t)))
           (begin0 (expand-toplevel-expr-- e file line)
                   (set! *in-expand* last))))))

;; used to collect warnings during lowering, which are usually discarded
;; unless logging is requested
(define lowering-warning (lambda lst (void)))

;; expand a piece of raw surface syntax to an executable thunk

(define (expand-to-thunk- expr file line)
  (error-wrap (lambda ()
                (expand-toplevel-expr expr file line))))

(define (expand-to-thunk-stmt- expr file line)
  (expand-to-thunk- (if (toplevel-only-expr? expr)
                        expr
                        `(block ,expr (null)))
                    file line))

(define (jl-expand-to-thunk-warn expr file line stmt)
  (let ((warnings '()))
    (with-bindings
     ((lowering-warning (lambda lst (set! warnings (cons lst warnings)))))
     (begin0
      (if stmt
          (expand-to-thunk-stmt- expr file line)
          (expand-to-thunk- expr file line))
      (for-each (lambda (args) (apply julia-logmsg args))
                (reverse warnings))))))

(define (jl-expand-to-thunk expr file line)
  (expand-to-thunk- expr file line))

(define (jl-expand-to-thunk-stmt expr file line)
  (expand-to-thunk-stmt- expr file line))

(define (jl-expand-macroscope expr)
  (error-wrap (lambda ()
                (julia-expand-macroscope expr))))

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
           (call (top include) ,name ,x)))))
   'none 0))

; run whole frontend on a string. useful for testing.
(define (fe str)
  (expand-toplevel-expr (julia-parse str) 'none 0))

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

(define *scopewarn-opt* 1)

; Corresponds to --depwarn 0="no", 1="yes", 2="error"
(define *depwarn-opt* 1)

; Emit deprecation warning via julia logging layer.
(define (frontend-depwarn msg file line)
  ; (display (string msg "; file = " file "; line = " line #\newline)))
  (case *depwarn-opt*
    (1 (julia-logmsg 1000 'depwarn (symbol (string file line)) file line msg))
    (2 (error msg))))
