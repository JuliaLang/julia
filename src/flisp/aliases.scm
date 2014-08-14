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
(define string-index string.find)
(define string-map string.map)
(define (string-drop s n) (string.tail s n))
(define string-ref string.char)
(define (string-contains s1 s2) (string.find s1 s2))
(define string->symbol symbol)
(define symbol->string string)
(define str string)
(define char->integer fixnum)
(define string-join string.join)

(define (list->string l) (apply string l))
(define (string->list s)
  (do ((i (sizeof s) i)
       (l '() (cons (string.char s i) l)))
      ((= i 0) l)
    (set! i (string.dec s i))))

(define (substring s start end)
  (string.sub s (string.inc s 0 start) (string.inc s 0 end)))

(define port? iostream?)
(define input-port? iostream?)
(define output-port? iostream?)
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

(define get-output-string io.tostring!)

(define (open-input-file name) (file name :read))
(define (open-output-file name) (file name :write :create))

(define (call-with-output-string proc)
  (let ((b (buffer)))
    (proc b)
    (io.tostring! b)))

(define (current-input-port (p *input-stream*))
  (set! *input-stream* p))
(define (current-output-port (p *output-stream*))
  (set! *output-stream* p))
(define (current-error-port) *stderr*)

(define (display x (port *output-stream*))
  (with-output-to port (princ x))
  #t)

(define (with-exception-handler hand thk)
  (trycatch (thk)
	    (lambda (e) (hand e))))

(define make-hash-table table)
(define hash-table-ref get)
(define hash-table-set! put!)
(define hash-table-exists? has?)
(define hash-table-ref/default get)

(define compare-strs compare)
(define compare-nums compare)

(define (last lst)
  (if (null? (cdr lst))
      (car lst)
      (last (cdr lst))))
