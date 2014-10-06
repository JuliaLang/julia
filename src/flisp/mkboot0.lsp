; -*- scheme -*-

(if (not (bound? 'top-level-value)) (set! top-level-value %eval))
(if (not (bound? 'set-top-level-value!)) (set! set-top-level-value! set))
(if (not (bound? 'eof-object?)) (set! eof-object? (lambda (x) #f)))

;(load "compiler.lsp")

(define (compile-file inf)
  (let ((in  (file inf :read)))
    (let next ((E (read in)))
      (if (not (io.eof? in))
	  (begin (print (compile-thunk (expand E)))
		 (princ "\n")
		 (next (read in)))))
    (io.close in)))

(define (do-boot0)
  (for-each (lambda (file)
	      (compile-file file))
	    (cdr *argv*)))

(do-boot0)
