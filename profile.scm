(let ((*profiles* (table)))
  (set! profile
	(lambda (s)
	  (let ((f (top-level-value s)))
	    (put! *profiles* s (cons 0 0))
	    (set-top-level-value! s
	     (lambda args
	       (define tt (get *profiles* s))
	       (define count (car tt))
	       (define time  (cdr tt))
	       (define t0 (time.now))
	       (define v (apply f args))
	       (set-cdr! tt (+ time (- (time.now) t0)))
	       (set-car! tt (+ count 1))
	       v)))))
  (set! show-profiles
	(lambda ()
	  (define pr (filter (lambda (x) (> (cadr x) 0))
			     (table.pairs *profiles*)))
	  (define width (+ 4
			   (apply max
				  (map (lambda (x)
					 (length (string x)))
				       (cons 'Function
					     (map car pr))))))
	  (princ (string.rpad "Function" width #\ )
		 "#Calls     Time (seconds)")
	  (newline)
	  (princ (string.rpad "--------" width #\ )
		 "------     --------------")
	  (newline)
	  (for-each
	   (lambda (p)
	     (princ (string.rpad (string (caddr p)) width #\ )
		    (string.rpad (string (cadr p)) 11 #\ )
		    (car p))
	     (newline))
	   (simple-sort (map (lambda (l) (reverse (to-proper l)))
			     pr)))))
  (set! clear-profiles
	(lambda ()
	  (for-each (lambda (k)
		      (put! *profiles* k (cons 0 0)))
		    (table.keys *profiles*)))))

(for-each profile
	  '(syntactic-op? syntactic-unary-op? newline?
	    identifier-char? opchar? operator? skip-to-eol
	    accum-tok accum-tok-eager read-number read-operator
	    skip-ws-and-comments next-token peek-token require-token
	    parse-LtoR parse-RtoL parse-cond parse-call parse-atom
	    julia-parse-file julia-parse
	    quoted? lam:args lam:vars lam:vinfo lam:body
	    fsig-to-lambda-list arg-name llist-vars llist-types
	    decl-var decl-type to-LFF expand-and-or
	    remove-argument-side-effects expand-update-operator
	    expand-compare-chain process-indexes function-expr
	    symbols->typevars generic-function-def-expr
	    struct-def-expr type-def-expr index-p
	    identify-locals declared-local-vars
	    rename-vars flatten-scopes make-var-info vinfo:name
	    vinfo:type vinfo:capt vinfo:set-type! vinfo:set-capt!
	    var-info-for lambda-all-vars fix-seq-type analyze-vars
	    analyze-variables closure-convert goto-form
	    expand-backquote julia-expand-backquote
	    julia-expand closure-convert- pattern-expand
))
