; definitions of standard scheme procedures in terms of femtolisp procedures

(define-macro (begin0 first . rest)
  `(prog1 ,first ,@rest))

(define vector-ref aref)
(define vector-set! aset!)
(define vector-length length)

(define char=? eqv?)
(define char<? <)
(define char>? >)
(define char<=? <=)
(define char>=? >=)
(define (char-whitespace? c) (not (not (string.find *whitespace* c))))
(define (char-numeric? c) (not (not (string.find "0123456789" c))))

(define string-append string)
(define string-length string.count)
(define string->symbol symbol)
(define (symbol->string s) (string s))

(define (list->string l) (apply string l))
(define (string->list s)
  (let loop ((i (sizeof s))
	     (l '()))
    (if (= i 0)
	l
	(let ((newi (string.dec s i)))
	  (loop newi (cons (string.char s newi) l))))))

(define (substring s start end)
  (string.sub s (string.inc s 0 start) (string.inc s 0 end)))

(define (port? x) (iostream? x))
(define (read-char (s *input-stream*)) (io.getc s))
(define (peek-char (s *input-stream*)) (io.peekc s))
(define (write-char c (s *output-stream*)) (io.putc s c))

(define (port-eof? p) (io.eof? p))
(define (open-input-string str)
  (let ((b (buffer)))
    (io.write b str)
    (io.seek b 0)
    b))
(define (open-output-string) (buffer))
(define (open-string-output-port)
  (let ((b (buffer)))
    (values b (lambda () (io.tostring! b)))))

(define (get-output-string b)
  (let ((p (io.pos b)))
    (io.seek b 0)
    (let ((s (io.readall b)))
      (io.seek b p)
      (if (eof-object? s) "" s))))

(define (open-input-file name) (file name :read))
(define (open-output-file name) (file name :write :create))

(define (current-input-port (p *input-stream*))
  (set! *input-stream* p))
(define (current-output-port (p *output-stream*))
  (set! *output-stream* p))

(define (display x (port *output-stream*))
  (with-output-to port (princ x))
  #t)

; --- gambit

(define (with-exception-catcher hand thk)
  (trycatch (thk)
	    (lambda (e) (hand e))))

(define make-table table)
(define table-ref get)
(define table-set! put!)
