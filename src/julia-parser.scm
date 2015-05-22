;; Operator precedence table, lowest at top

;; note: there are some strange-looking things in here because
;; the way the lexer works, every prefix of an operator must also
;; be an operator.
(define prec-assignment
  '(= := += -= *= /= //= .//= .*= ./= |\\=| |.\\=| ^= .^= ÷= .÷= %= .%= |\|=| &= $= => <<= >>= >>>= ~ |.+=| |.-=|))
(define prec-conditional '(?))
(define prec-lazy-or     '(|\|\||))
(define prec-lazy-and    '(&&))
(define prec-arrow       '(-- --> ← → ↔ ↚ ↛ ↠ ↣ ↦ ↮ ⇎ ⇏ ⇒ ⇔ ⇴ ⇶ ⇷ ⇸ ⇹ ⇺ ⇻ ⇼ ⇽ ⇾ ⇿ ⟵ ⟶ ⟷ ⟷ ⟹ ⟺ ⟻ ⟼ ⟽ ⟾ ⟿ ⤀ ⤁ ⤂ ⤃ ⤄ ⤅ ⤆ ⤇ ⤌ ⤍ ⤎ ⤏ ⤐ ⤑ ⤔ ⤕ ⤖ ⤗ ⤘ ⤝ ⤞ ⤟ ⤠ ⥄ ⥅ ⥆ ⥇ ⥈ ⥊ ⥋ ⥎ ⥐ ⥒ ⥓ ⥖ ⥗ ⥚ ⥛ ⥞ ⥟ ⥢ ⥤ ⥦ ⥧ ⥨ ⥩ ⥪ ⥫ ⥬ ⥭ ⥰ ⧴ ⬱ ⬰ ⬲ ⬳ ⬴ ⬵ ⬶ ⬷ ⬸ ⬹ ⬺ ⬻ ⬼ ⬽ ⬾ ⬿ ⭀ ⭁ ⭂ ⭃ ⭄ ⭇ ⭈ ⭉ ⭊ ⭋ ⭌ ￩ ￫))
(define prec-comparison
  '(> < >= ≥ <= ≤ == === ≡ != ≠ !== ≢ |.>| |.<| |.>=| |.≥| |.<=| |.≤| |.==| |.!=| |.≠| |.=| |.!| |<:| |>:| ∈ ∉ ∋ ∌ ⊆ ⊈ ⊂ ⊄ ⊊ ∝ ∊ ∍ ∥ ∦ ∷ ∺ ∻ ∽ ∾ ≁ ≃ ≄ ≅ ≆ ≇ ≈ ≉ ≊ ≋ ≌ ≍ ≎ ≐ ≑ ≒ ≓ ≔ ≕ ≖ ≗ ≘ ≙ ≚ ≛ ≜ ≝ ≞ ≟ ≣ ≦ ≧ ≨ ≩ ≪ ≫ ≬ ≭ ≮ ≯ ≰ ≱ ≲ ≳ ≴ ≵ ≶ ≷ ≸ ≹ ≺ ≻ ≼ ≽ ≾ ≿ ⊀ ⊁ ⊃ ⊅ ⊇ ⊉ ⊋ ⊏ ⊐ ⊑ ⊒ ⊜ ⊩ ⊬ ⊮ ⊰ ⊱ ⊲ ⊳ ⊴ ⊵ ⊶ ⊷ ⋍ ⋐ ⋑ ⋕ ⋖ ⋗ ⋘ ⋙ ⋚ ⋛ ⋜ ⋝ ⋞ ⋟ ⋠ ⋡ ⋢ ⋣ ⋤ ⋥ ⋦ ⋧ ⋨ ⋩ ⋪ ⋫ ⋬ ⋭ ⋲ ⋳ ⋴ ⋵ ⋶ ⋷ ⋸ ⋹ ⋺ ⋻ ⋼ ⋽ ⋾ ⋿ ⟈ ⟉ ⟒ ⦷ ⧀ ⧁ ⧡ ⧣ ⧤ ⧥ ⩦ ⩧ ⩪ ⩫ ⩬ ⩭ ⩮ ⩯ ⩰ ⩱ ⩲ ⩳ ⩴ ⩵ ⩶ ⩷ ⩸ ⩹ ⩺ ⩻ ⩼ ⩽ ⩾ ⩿ ⪀ ⪁ ⪂ ⪃ ⪄ ⪅ ⪆ ⪇ ⪈ ⪉ ⪊ ⪋ ⪌ ⪍ ⪎ ⪏ ⪐ ⪑ ⪒ ⪓ ⪔ ⪕ ⪖ ⪗ ⪘ ⪙ ⪚ ⪛ ⪜ ⪝ ⪞ ⪟ ⪠ ⪡ ⪢ ⪣ ⪤ ⪥ ⪦ ⪧ ⪨ ⪩ ⪪ ⪫ ⪬ ⪭ ⪮ ⪯ ⪰ ⪱ ⪲ ⪳ ⪴ ⪵ ⪶ ⪷ ⪸ ⪹ ⪺ ⪻ ⪼ ⪽ ⪾ ⪿ ⫀ ⫁ ⫂ ⫃ ⫄ ⫅ ⫆ ⫇ ⫈ ⫉ ⫊ ⫋ ⫌ ⫍ ⫎ ⫏ ⫐ ⫑ ⫒ ⫓ ⫔ ⫕ ⫖ ⫗ ⫘ ⫙ ⫷ ⫸ ⫹ ⫺ ⊢ ⊣))
(define prec-pipe        '(|\|>| |<\||))
(define prec-colon       '(: |..|))
(define prec-plus        '(+ - ⊕ ⊖ ⊞ ⊟ |.+| |.-| |\|| ∪ ∨ $ ⊔ ± ∓ ∔ ∸ ≂ ≏ ⊎ ⊻ ⊽ ⋎ ⋓ ⧺ ⧻ ⨈ ⨢ ⨣ ⨤ ⨥ ⨦ ⨧ ⨨ ⨩ ⨪ ⨫ ⨬ ⨭ ⨮ ⨹ ⨺ ⩁ ⩂ ⩅ ⩊ ⩌ ⩏ ⩐ ⩒ ⩔ ⩖ ⩗ ⩛ ⩝ ⩡ ⩢ ⩣))
(define prec-bitshift    '(<< >> >>> |.<<| |.>>| |.>>>|))
(define prec-times       '(* / |./| ÷ % ⋅ ∘ × |.%| |.*| |\\| |.\\| & ∩ ∧ ⊗ ⊘ ⊙ ⊚ ⊛ ⊠ ⊡ ⊓ ∗ ∙ ∤ ⅋ ≀ ⊼ ⋄ ⋆ ⋇ ⋉ ⋊ ⋋ ⋌ ⋏ ⋒ ⟑ ⦸ ⦼ ⦾ ⦿ ⧶ ⧷ ⨇ ⨰ ⨱ ⨲ ⨳ ⨴ ⨵ ⨶ ⨷ ⨸ ⨻ ⨼ ⨽ ⩀ ⩃ ⩄ ⩋ ⩍ ⩎ ⩑ ⩓ ⩕ ⩘ ⩚ ⩜ ⩞ ⩟ ⩠ ⫛ ⊍))
(define prec-rational    '(// .//))
(define prec-power       '(^ |.^| ↑ ↓ ⇵ ⟰ ⟱ ⤈ ⤉ ⤊ ⤋ ⤒ ⤓ ⥉ ⥌ ⥍ ⥏ ⥑ ⥔ ⥕ ⥘ ⥙ ⥜ ⥝ ⥠ ⥡ ⥣ ⥥ ⥮ ⥯ ￪ ￬))
(define prec-decl        '(|::|))
(define prec-dot         '(|.|))

(define prec-names '(prec-assignment
                     prec-conditional prec-lazy-or prec-lazy-and prec-arrow prec-comparison
                     prec-pipe prec-colon prec-plus prec-bitshift prec-times prec-rational
                     prec-power prec-decl prec-dot))

(define (Set l)
  ;; construct a length-specialized membership tester
  (cond ((length= l 1)
         (eval `(lambda (x)
                  (,(if (symbol? (car l)) 'eq? 'eqv?) x (quote ,(car l))))))
        ((not (length> l 8))
         (eval `(lambda (x)
                  (not (not (,(if (every symbol? l) 'memq 'memv) x (quote ,l)))))))
        (else
         (let ((t (table)))
           (for-each (lambda (x) (put! t x #t)) l)
           (lambda (x)
             (has? t x))))))

;; for each prec-x generate an is-prec-x? procedure
(for-each (lambda (name)
            (eval `(define ,(symbol (string "is-" name "?")) (Set ,name))))
          prec-names)

;; hash table of binary operators -> precedence
(define prec-table (let ((t (table)))
                     (define (pushprec L prec)
                       (if (not (null? L))
                           (begin
                             (for-each (lambda (x) (put! t x prec)) (car L))
                             (pushprec (cdr L) (+ prec 1)))))
                     (pushprec (map eval prec-names) 1)
                     t))
(define (operator-precedence op) (get prec-table op 0))

(define unary-ops '(+ - ! ¬ ~ |<:| |>:| √ ∛ ∜))

; operators that are both unary and binary
(define unary-and-binary-ops '(+ - $ & ~))

; operators that are special forms, not function names
(define syntactic-operators
  '(= := += -= *= /= //= .//= .*= ./= |\\=| |.\\=| ^= .^= ÷= .÷= %= .%= |\|=| &= $= =>
      <<= >>= >>>= -> --> |\|\|| && |.| ... |.+=| |.-=|))
(define syntactic-unary-operators '($ & |::|))

(define syntactic-op? (Set syntactic-operators))
(define syntactic-unary-op? (Set syntactic-unary-operators))

(define trans-op (string->symbol ".'"))
(define ctrans-op (string->symbol "'"))
(define vararg-op (string->symbol "..."))

(define operators (list* '~ '! '¬ '-> '√ '∛ '∜ ctrans-op trans-op vararg-op
                         (delete-duplicates
                          (apply append (map eval prec-names)))))

(define op-chars
  (delete-duplicates
   (apply append
          (map string->list (map symbol->string operators)))))

;; characters that can be in an operator
(define opchar? (Set op-chars))
;; characters that can follow . in an operator
(define (dot-opchar? c) (and (char? c) (string.find ".*^/\\+-'<>!=%≥≤≠÷" c)))
(define operator? (Set operators))

(define reserved-words '(begin while if for try return break continue
                         stagedfunction function macro quote let local global const
                         abstract typealias type bitstype immutable ccall do
                         module baremodule using import export importall))

(define (assignment? e)
  (and (pair? e) (eq? (car e) '=)))

(define (assignment-like? e)
  (and (pair? e) (is-prec-assignment? (car e))))

(define (kwarg? e)
  (and (pair? e) (eq? (car e) 'kw)))

(define (dict-literal? l)
  (and (length= l 3) (eq? (car l) '=>)))

;; Parser state variables

; disable range colon for parsing ternary conditional operator
(define range-colon-enabled #t)
; in space-sensitive mode "x -y" is 2 expressions, not a subtraction
(define space-sensitive #f)
(define inside-vec #f)
; treat 'end' like a normal symbol instead of a reserved word
(define end-symbol #f)
; treat newline like ordinary whitespace instead of as a potential separator
(define whitespace-newline #f)

(define current-filename 'none)

(define-macro (with-normal-ops . body)
  `(with-bindings ((range-colon-enabled #t)
                   (space-sensitive #f))
                  ,@body))

(define-macro (without-range-colon . body)
  `(with-bindings ((range-colon-enabled #f))
                  ,@body))

(define-macro (with-space-sensitive . body)
  `(with-bindings ((space-sensitive #t)
                   (whitespace-newline #f))
                  ,@body))

(define-macro (with-inside-vec . body)
  `(with-bindings ((space-sensitive #t)
                   (inside-vec #t)
                   (whitespace-newline #f))
                  ,@body))

(define-macro (with-end-symbol . body)
  `(with-bindings ((end-symbol #t))
                  ,@body))

(define-macro (with-whitespace-newline . body)
  `(with-bindings ((whitespace-newline #t))
                  ,@body))

(define-macro (without-whitespace-newline . body)
  `(with-bindings ((whitespace-newline #f))
                  ,@body))

;; --- lexer ---

(define special-char?
  (let ((chrs (string->list "()[]{},;\"`@")))
    (lambda (c) (memv c chrs))))
(define (newline? c) (eqv? c #\newline))

(define (skip-to-eol port)
  (let ((c (peek-char port)))
    (cond ((eof-object? c)    c)
          ((eqv? c #\newline) c)
          (else               (read-char port)
                              (skip-to-eol port)))))

(define (read-operator port c)
  (if (and (eqv? c #\*) (eqv? (peek-char port) #\*))
      (error "use \"^\" instead of \"**\""))
  (if (or (eof-object? (peek-char port)) (not (opchar? (peek-char port))))
      (symbol (string c)) ; 1-char operator
      (let ((str (let loop ((str (string c))
                            (c   (peek-char port)))
                   (if (and (not (eof-object? c)) (opchar? c))
                       (let* ((newop (string str c))
                              (opsym (string->symbol newop)))
                         (if (operator? opsym)
                             (begin (read-char port)
                                    (loop newop (peek-char port)))
                             str))
                       str))))
        (if (equal? str "--")
            (syntax-deprecation-warning port str ""))
        (string->symbol str))))

(define (accum-digits c pred port lz)
  (if (and (not lz) (eqv? c #\_))
      (cons "_" #f)
      (let loop ((str '())
                 (c c))
        (if (eqv? c #\_)
            (begin (read-char port)
                   (let ((c (peek-char port)))
                     (if (and (not (eof-object? c)) (pred c))
                         (loop str c)
                         (begin
                           (io.ungetc port #\_)
                           (cons (list->string (reverse str)) #t)))))
            (if (and (not (eof-object? c)) (pred c))
                (begin (read-char port)
                       (loop (cons c str) (peek-char port)))
                (cons (list->string (reverse str)) #t))))))

(define (char-hex? c)
  (or (char-numeric? c)
      (and (>= c #\a) (<= c #\f))
      (and (>= c #\A) (<= c #\F))))

(define (char-oct? c)
  (and (>= c #\0) (<= c #\7)))

(define (char-bin? c)
  (or (eqv? c #\0)
      (eqv? c #\1)))

(define (string-to-number s r is-float32)
  (let ((ans (if is-float32
                 (float (string->number
                         (string.map (lambda (c) (if (eqv? c #\f) #\e c)) s)
                         r))
                 (string->number s r))))
    (and ans
         (if (or (= ans +inf.0) (= ans -inf.0))
             (error (string "overflow in numeric constant \"" s "\""))
             ans))))

(define (read-number port leadingdot neg)
  (let ((str  (open-output-string))
        (pred char-numeric?)
        (is-float32-literal #f)
        (is-hex-float-literal #f)
        (leadingzero #f))
    (define (allow ch)
      (let ((c (peek-char port)))
        (and (eqv? c ch)
             (begin (write-char (read-char port) str) #t))))
    (define (disallow-dot)
      (if (eqv? (peek-char port) #\.)
          (begin (read-char port)
                 (if (dot-opchar? (peek-char port))
                     (io.ungetc port #\.)
                     (error (string "invalid numeric constant \""
                                    (get-output-string str) #\. "\""))))))
    (define (read-digs lz)
      (let ((D (accum-digits (peek-char port) pred port lz)))
        (let ((d  (car D))
              (ok (cdr D)))
          (if (not ok)
              (begin (display d str)
                     (error (string "invalid numeric constant \""
                                    (get-output-string str) "\""))))
          (and (not (equal? d ""))
               (not (eof-object? d))
               (display d str)
               #t))))
    (if neg (write-char #\- str))
    (if leadingdot
        (write-char #\. str)
        (if (eqv? (peek-char port) #\0)
            (begin (write-char (read-char port) str)
                   (set! leadingzero #t)
                   (cond ((allow #\x)
                          (begin (set! leadingzero #f)
                                 (set! pred char-hex?)))
                         ((allow #\o)
                          (begin (set! leadingzero #f)
                                 (set! pred char-oct?)))
                         ((allow #\b)
                          (begin (set! leadingzero #f)
                                 (set! pred char-bin?)))))
            (allow #\.)))
    (read-digs leadingzero)
    (if (eqv? (peek-char port) #\.)
        (begin (read-char port)
               (if (dot-opchar? (peek-char port))
                   (io.ungetc port #\.)
                   (begin (write-char #\. str)
                          (read-digs #f)
                          (if (eq? pred char-hex?)
                              (set! is-hex-float-literal #t))
                          (disallow-dot)))))
    (let* ((c    (peek-char port))
           (ispP (or (eqv? c #\p) (eqv? c #\P))))
      (if (or (and is-hex-float-literal (or ispP (error "hex float literal must contain \"p\" or \"P\"")))
              (and (eq? pred char-hex?) ispP)
              (memv c '(#\e #\E #\f)))
          (begin (read-char port)
                 (let ((d (peek-char port)))
                   (if (and (not (eof-object? d))
                            (or (char-numeric? d) (eqv? d #\+) (eqv? d #\-)))
                       (begin (set! is-float32-literal (eqv? c #\f))
                              (set! is-hex-float-literal ispP)
                              (write-char c str)
                              (write-char (read-char port) str)
                              (read-digs #f)
                              (disallow-dot))
                       (io.ungetc port c))))
          ;; disallow digits after binary or octal literals, e.g., 0b12
          (if (and (or (eq? pred char-bin?) (eq? pred char-oct?))
                   (not (eof-object? c))
                   (char-numeric? c))
              (error (string "invalid numeric constant \""
                             (get-output-string str) c "\"")))))
    (let* ((s (get-output-string str))
           (r (cond ((eq? pred char-hex?) 16)
                    ((eq? pred char-oct?) 8)
                    ((eq? pred char-bin?) 2)
                    (else 10)))
           (n (string-to-number
               ;; for an unsigned literal starting with -, remove the - and
               ;; parse instead as a call to unary -
               (if (and neg (not (= r 10)) (not is-hex-float-literal))
                   (string.sub s 1)
                   s)
               r is-float32-literal)))
      ;; n is #f for integers > typemax(UInt64)
      (cond (is-hex-float-literal (double n))
            ((eq? pred char-hex?) (fix-uint-neg neg (sized-uint-literal n s 4)))
            ((eq? pred char-oct?) (fix-uint-neg neg (sized-uint-oct-literal n s)))
            ((eq? pred char-bin?) (fix-uint-neg neg (sized-uint-literal n s 1)))
            (is-float32-literal   (float n))
            (n (if (and (integer? n) (> n 9223372036854775807))
                   `(macrocall @int128_str ,s)
                   n))
            ((within-int128? s) `(macrocall @int128_str ,s))
            (else `(macrocall @big_str ,s))))))

(define (fix-uint-neg neg n)
  (if neg
      (if (large-number? n)
          `(call - ,(maybe-negate '- n))
          `(call - ,n))
      n))

(define (sized-uint-literal n s b)
  (let* ((i (if (eqv? (string.char s 0) #\-) 3 2))
         (l (* (- (length s) i) b)))
    (cond ((<= l 8)   (uint8  n))
          ((<= l 16)  (uint16 n))
          ((<= l 32)  (uint32 n))
          ((<= l 64)  (uint64 n))
          ((<= l 128) `(macrocall @uint128_str ,s))
          (else       (error "Hex or binary literal too large for UInt128")))))

(define (sized-uint-oct-literal n s)
  (if (string.find s "o0")
      (sized-uint-literal n s 3)
      (if n
          (cond ((< n 256)        (uint8  n))
                ((< n 65536)      (uint16 n))
                ((< n 4294967296) (uint32 n))
                (else             (uint64 n)))
          (if (oct-within-uint128? s)
              `(macrocall @uint128_str ,s)
              (error "Octal literal too large for UInt128")))))

(define (strip-leading-0s s)
  (define (loop i)
    (if (eqv? (string.char s i) #\0)
        (loop (+ i 1))
        (string.tail s i)))
  (if (eqv? (string.char s 0) #\-)
      (string #\- (loop 1))
      (loop 0)))

(define (compare-num-strings s1 s2)
  (let ((s1 (strip-leading-0s s1))
        (s2 (strip-leading-0s s2)))
    (if (= (string-length s1) (string-length s2))
        (compare s1 s2)
        (compare (string-length s1) (string-length s2)))))

(define (oct-within-uint128? s)
  (let ((s (if (eqv? (string.char s 0) #\-)
               (string.tail s 1)
               s)))
    (>= 0 (compare-num-strings s "0o3777777777777777777777777777777777777777777"))))

(define (within-int128? s)
  (if (eqv? (string.char s 0) #\-)
      (>= 0 (compare-num-strings s "-170141183460469231731687303715884105728"))
      (>= 0 (compare-num-strings s "170141183460469231731687303715884105727"))))

(define (large-number? t)
  (and (pair? t)
       (eq? (car t) 'macrocall)
       (memq (cadr t) '(@int128_str @uint128_str @big_str))))

; skip to end of comment, starting at #:  either #...<eol> or #= .... =#.
(define (skip-comment port)
  (define (skip-multiline-comment port count)
    (let ((c (read-char port)))
      (if (eof-object? c)
          (error "incomplete: unterminated multi-line comment #= ... =#") ; NOTE: changing this may affect code in base/client.jl
          (begin (if (eqv? c #\=)
                     (let ((c (peek-char port)))
                       (if (eqv? c #\#)
                           (begin
                             (read-char port)
                             (if (> count 1)
                                 (skip-multiline-comment port (- count 1))))
                           (skip-multiline-comment port count)))
                     (if (eqv? c #\#)
                         (skip-multiline-comment port
                                                 (if (eqv? (peek-char port) #\=)
                                                     (begin (read-char port)
                                                            (+ count 1))
                                                     count))
                         (skip-multiline-comment port count)))))))

  (read-char port) ; read # that was already peeked
  (if (eqv? (peek-char port) #\=)
      (begin (read-char port) ; read initial =
             (skip-multiline-comment port 1))
      (skip-to-eol port)))

(define (skip-ws-and-comments port)
  (skip-ws port #t)
  (if (eqv? (peek-char port) #\#)
      (begin (skip-comment port)
             (skip-ws-and-comments port)))
  #t)

(define (zero-width-space? c)
  (memv c '(#\u200b #\u2060 #\ufeff)))

(define (default-ignorable-char? c)
  (or (zero-width-space? c)
      (and (char>=? c #\u200c) (char<=? c #\u200f))
      (memv c '(#\u00ad #\u2061 #\u115f))))

(define (next-token port s)
  (aset! s 2 (eq? (skip-ws port whitespace-newline) #t))
  (let ((c (peek-char port)))
    (cond ((or (eof-object? c) (newline? c))  (read-char port))

          ((special-char? c)    (read-char port))

          ((char-numeric? c)    (read-number port #f #f))

          ((eqv? c #\#)         (skip-comment port) (next-token port s))

          ;; . is difficult to handle; it could start a number or operator
          ((and (eqv? c #\.)
                (let ((c (read-char port))
                      (nextc (peek-char port)))
                  (cond ((eof-object? nextc)
                         '|.|)
                        ((char-numeric? nextc)
                         (read-number port #t #f))
                        ((opchar? nextc)
                         (let ((op (read-operator port c)))
                           (if (and (eq? op '..) (opchar? (peek-char port)))
                               (error (string "invalid operator \"" op (peek-char port) "\"")))
                           op))
                        (else '|.|)))))

          ((opchar? c)  (read-operator port (read-char port)))

          ((identifier-start-char? c) (accum-julia-symbol c port))

          (else
           (read-char port)
           (if (default-ignorable-char? c)
               (error (string "invisible character \\u" (number->string (fixnum c) 16)))
               (error (string "invalid character \"" c "\"")))))))

;; --- token stream ---

(define (make-token-stream s) (vector #f s #t #f))
(define-macro (ts:port s)       `(aref ,s 1))
(define-macro (ts:last-tok s)   `(aref ,s 0))
(define-macro (ts:set-tok! s t) `(aset! ,s 0 ,t))
(define-macro (ts:space? s)     `(aref ,s 2))
(define-macro (ts:pbtok s)      `(aref ,s 3))
(define (ts:put-back! s t)
  (if (ts:pbtok s)
      (error "too many pushed-back tokens (internal error)")
      (aset! s 3 t)))

(define (peek-token s)
  (or (ts:pbtok s)
      (ts:last-tok s)
      (begin (ts:set-tok! s (next-token (ts:port s) s))
             (ts:last-tok s))))

(define (require-token s)
  (let ((t (or (ts:pbtok s) (ts:last-tok s) (next-token (ts:port s) s))))
    (if (eof-object? t)
        (error "incomplete: premature end of input") ; NOTE: changing this may affect code in base/client.jl
        (if (newline? t)
            (begin (take-token s)
                   (require-token s))
            (begin (if (not (ts:pbtok s)) (ts:set-tok! s t))
                   t)))))

(define (take-token s)
  (or
   (begin0 (ts:pbtok s)
           (aset! s 3 #f))
   (begin0 (ts:last-tok s)
           (ts:set-tok! s #f))))

;; --- misc ---

(define (syntax-deprecation-warning s what instead)
  (if *depwarn*
    (io.write
     *stderr*
     (string
      #\newline "WARNING: deprecated syntax \"" what "\""
      (if (eq? current-filename 'none)
          ""
          (string " at " current-filename ":" (input-port-line (if (port? s) s (ts:port s)))))
      "."
      (if (equal? instead "")
          ""
          (string #\newline "Use \"" instead "\" instead."))
      #\newline))))

;; --- parser ---

; parse left-to-right binary operator
; produces structures like (+ (+ (+ 2 3) 4) 5)
(define-macro (parse-LtoR s down ops)
  `(let loop ((ex (,down ,s))
              (t  (peek-token ,s)))
     (if (not (,ops t))
         ex
         (begin (take-token ,s)
                (if (or (syntactic-op? t) (eq? t 'in) (eq? t '|::|))
                    (loop (list t ex (,down ,s)) (peek-token ,s))
                    (loop (list 'call t ex (,down ,s)) (peek-token ,s)))))))

; parse right-to-left binary operator
; produces structures like (= a (= b (= c d)))
(define (parse-RtoL s down ops)
  (let loop ((ex  (down s))
             (t   (peek-token s))
             (spc (ts:space? s)))
    (if (not (ops t))
        ex
        (begin (take-token s)
               (cond ((and space-sensitive spc (memq t unary-and-binary-ops)
                           (not (eqv? (peek-char (ts:port s)) #\ )))
                      (ts:put-back! s t)
                      ex)
                     ((syntactic-op? t)
                      (list t ex (parse-RtoL s down ops)))
                     ((eq? t '~)
                      (let ((args (parse-chain s down '~)))
                        (if (ops (peek-token s))
                            `(macrocall @~ ,ex ,@(butlast args)
                                        ,(loop (last args)
                                               (peek-token s)
                                               (ts:space? s)))
                            `(macrocall @~ ,ex ,@args))))
                     (else
                      (list 'call t ex (parse-RtoL s down ops))))))))

(define (parse-cond s)
  (let ((ex (parse-or s)))
    (cond ((eq? (peek-token s) '?)
           (begin (take-token s)
                  (let ((then (without-range-colon (parse-eq* s))))
                    (if (not (eq? (take-token s) ':))
                        (error "colon expected in \"?\" expression")
                        (list 'if ex then (parse-eq* s))))))
          #;((string? ex)
           (let loop ((args (list ex)))
             (let ((next (peek-token s)))
               (if (or (eof-object? next) (closing-token? next)
                       (newline? next))
                   `(call (top string) ,@(reverse args))
                   (loop (cons (parse-or s) args))))))
          (else ex))))

(define (invalid-initial-token? tok)
  (or (eof-object? tok)
      (memv tok '(#\) #\] #\} else elseif catch finally =))))

(define (line-number-node s)
  `(line ,(input-port-line (ts:port s))))

(define (line-number-filename-node s)
  `(line ,(input-port-line (ts:port s)) ,current-filename))

;; insert line/file for short-form function defs, otherwise leave alone
(define (short-form-function-loc ex lno)
  (if (and (pair? ex)
           (eq? (car ex) '=)
           (pair? (cadr ex))
           (eq? (caadr ex) 'call))
      `(= ,(cadr ex) (block (line ,lno ,current-filename) ,(caddr ex)))
      ex))

; parse a@b@c@... as (@ a b c ...) for some operator @
; op: the operator to look for
; head: the expression head to yield in the result, e.g. "a;b" => (block a b)
; closers: a list of tokens that will stop the process
;          however, this doesn't consume the closing token, just looks at it
; allow-empty: if true will ignore runs of the operator, like a@@@@b
; ow, my eyes!!
(define (parse-Nary s down ops head closers allow-empty)
  (if (invalid-initial-token? (require-token s))
      (error (string "unexpected \"" (peek-token s) "\"")))
  (if (memv (require-token s) closers)
      (list head)  ; empty block
      (let loop ((ex
                  ;; in allow-empty mode skip leading runs of operator
                  (if (and allow-empty (memv (require-token s) ops))
                      '()
                      (if (memv #\newline ops)
                          (let ((loc (line-number-node s)))
                            ;; note: line-number must happen before (down s)
                            (list (down s) loc))
                          (list (down s)))))
                 (first? #t)
                 (t (peek-token s)))
        (if (not (memv t ops))
            (begin
              (if (not (or (eof-object? t) (eqv? t #\newline) (memv #\, ops)
                           (memv t closers)))
                  (error (string "extra token \"" t "\" after end of expression")))
              (if (or (null? ex) (pair? (cdr ex)) (not first?))
                  ;; () => (head)
                  ;; (ex2 ex1) => (head ex1 ex2)
                  ;; (ex1) if operator appeared => (head ex1) (handles "x;")
                  (cons head (reverse ex))
                  ;; (ex1) => ex1
                  (car ex)))
            (begin (take-token s)
                   ;; allow input to end with the operator, as in a;b;
                   (if (or (eof-object? (peek-token s))
                           (memv (peek-token s) closers)
                           (and allow-empty
                                (memv (peek-token s) ops))
                           (and (equal? ops '(#\,))
                                (eq? (peek-token s) '=)))
                       (loop ex #f (peek-token s))
                       (if (memv #\newline ops)
                           (let ((loc (line-number-node s)))
                             (loop (list* (down s) loc ex) #f (peek-token s)))
                           (loop (cons (down s) ex) #f (peek-token s)))))))))

; parse ranges and postfix ...
; colon is strange; 3 arguments with 2 colons yields one call:
; 1:2   => (: 1 2)
; 1:2:3 => (: 1 2 3)
; 1:    => (: 1 :)
; 1:2:  => (: 1 2 :)
;; not enabled:
;;; :2    => (: 2)
;;; :1:2  => (: (: 1 2))
;;; :1:   => (: (: 1 :))
; a simple state machine is up to the task.
; we will leave : expressions as a syntax form, not a call to ':',
; so they can be processed by syntax passes.
(define (parse-range s)
  (let loop ((ex (parse-expr s))
             (first? #t))
    (let* ((t   (peek-token s))
           (spc (ts:space? s)))
      (cond ((and first? (eq? t '|..|))
             (take-token s)
             `(call ,t ,ex ,(parse-expr s)))
            ((and range-colon-enabled (eq? t ':))
             (take-token s)
             (if (and space-sensitive spc
                      (or (peek-token s) #t) (not (ts:space? s)))
                 ;; "a :b" in space sensitive mode
                 (begin (ts:put-back! s ':)
                        ex)
                 (let ((argument
                        (cond ((closing-token? (peek-token s))
                               (error  (string "missing last argument in \""
                                               (deparse ex) ":\" range expression ")))
                              ((newline? (peek-token s))
                               (error "line break in \":\" expression"))
                              (else
                               (parse-expr s)))))
                   (if (and (not (ts:space? s))
                            (or (eq? argument '<) (eq? argument '>)))
                       (error (string "\":" argument "\" found instead of \""
                                      argument ":\"")))
                   (if first?
                       (loop (list t ex argument) #f)
                       (loop (append ex (list argument)) #t)))))
            ((eq? t '...)
             (take-token s)
             (list '... ex))
            (else ex)))))

; the principal non-terminals follow, in increasing precedence order

(define (parse-block s) (parse-Nary s parse-eq '(#\newline #\;) 'block
                                    '(end else elseif catch finally) #t))

;; ";" at the top level produces a sequence of top level expressions
(define (parse-stmts s)
  (let ((ex (parse-Nary s parse-eq '(#\;) 'toplevel '(#\newline) #t)))
    ;; check for unparsed junk after an expression
    (let ((t (peek-token s)))
      (if (not (or (eof-object? t) (eqv? t #\newline) (eq? t #f)))
          (error (string "extra token \"" t "\" after end of expression"))))
    ex))

(define (parse-eq s)
  (let ((lno (input-port-line (ts:port s))))
    (short-form-function-loc
     (parse-RtoL s parse-comma is-prec-assignment?) lno)))

; parse-eq* is used where commas are special, for example in an argument list
(define (parse-eq* s)
  (let ((lno (input-port-line (ts:port s))))
    (short-form-function-loc
     (parse-RtoL s parse-cond is-prec-assignment?) lno)))

; parse-comma is needed for commas outside parens, for example a = b,c
(define (parse-comma s) (parse-Nary s parse-cond  '(#\,) 'tuple '() #f))
(define (parse-or s)    (parse-LtoR s parse-and   is-prec-lazy-or?))
(define (parse-and s)   (parse-LtoR s parse-arrow is-prec-lazy-and?))
(define (parse-arrow s) (parse-RtoL s parse-comparison  is-prec-arrow?))

;; parse left to right chains of a certain binary operator
;; returns a list of arguments
(define (parse-chain s down op)
  (let loop ((chain (list (down s))))
    (let* ((t   (peek-token s))
           (spc (ts:space? s)))
      (if (not (eq? t op))
          (reverse! chain)
          (begin
            (take-token s)
            (cond ((and space-sensitive spc (memq t unary-and-binary-ops)
                        (not (eqv? (peek-char (ts:port s)) #\ )))
                   ;; here we have "x -y"
                   (ts:put-back! s t)
                   (reverse! chain))
                  (else
                   (loop (cons (down s) chain)))))))))

;; parse left to right, combining chains of a certain operator into 1 call
;; e.g. a+b+c => (call + a b c)
(define (parse-with-chains s down ops chain-op)
  (let loop ((ex (down s)))
    (let* ((t   (peek-token s))
           (spc (ts:space? s)))
      (if (not (ops t))
          ex
          (begin
            (take-token s)
            (cond ((and space-sensitive spc (memq t unary-and-binary-ops)
                        (not (eqv? (peek-char (ts:port s)) #\ )))
                   ;; here we have "x -y"
                   (ts:put-back! s t)
                   ex)
                  ((eq? t chain-op)
                   (loop (list* 'call t ex
                                (parse-chain s down t))))
                  (else
                   (loop (list 'call t ex (down s))))))))))

(define (parse-expr s) (parse-with-chains s parse-shift is-prec-plus? '+))

(define (parse-shift s) (parse-LtoR s parse-term is-prec-bitshift?))

(define (parse-term s) (parse-with-chains s parse-rational is-prec-times? '*))

(define (parse-rational s) (parse-LtoR s parse-unary is-prec-rational?))

(define (parse-pipes s)    (parse-LtoR s parse-range is-prec-pipe?))

(define is-in? (Set '(in)))
(define (parse-in s)       (parse-LtoR s parse-pipes is-in?))

(define (parse-comparison s)
  (let loop ((ex (parse-in s))
             (first #t))
    (let ((t (peek-token s)))
      (if (not (is-prec-comparison? t))
          ex
          (begin (take-token s)
                 (if first
                     (loop (list 'comparison ex t (parse-range s)) #f)
                     (loop (append ex (list t (parse-range s))) #f)))))))

; flag an error for tokens that cannot begin an expression
(define (closing-token? tok)
  (or (eof-object? tok)
      (and (eq? tok 'end) (not end-symbol))
      (memv tok '(#\, #\) #\] #\} #\; else elseif catch finally))))

(define (maybe-negate op num)
  (if (eq? op '-)
      (if (large-number? num)
          (if (eqv? (caddr num) "-170141183460469231731687303715884105728")
              `(macrocall @big_str "170141183460469231731687303715884105728")
              `(,(car num) ,(cadr num) ,(string.tail (caddr num) 1)))
          (if (= num -9223372036854775808)
              `(macrocall @int128_str "9223372036854775808")
              (- num)))
      num))

; given an expression and the next token, is there a juxtaposition
; operator between them?
(define (juxtapose? expr t)
  (and (or (number? expr)
           (large-number? expr)
	   (not (number? t))    ;; disallow "x.3" and "sqrt(2)2"
	   ;; to allow x'y as a special case
	   #;(and (pair? expr) (memq (car expr) '(|'| |.'|))
		(not (memv t '(#\( #\[ #\{))))
	   )
       (not (operator? t))
       (not (memq t reserved-words))
       (not (closing-token? t))
       (not (newline? t))
       (not (and (pair? expr) (syntactic-unary-op? (car expr))))))

(define (parse-juxtapose ex s)
  (let ((next (peek-token s)))
    ;; numeric literal juxtaposition is a unary operator
    (cond ((and (juxtapose? ex next)
                (not (ts:space? s)))
           (begin
             #;(if (and (number? ex) (= ex 0))
                 (error "juxtaposition with literal \"0\""))
             `(call * ,ex ,(parse-unary s))))
          (else ex))))

(define (invalid-identifier-name? ex)
  ;; TODO: remove this hack when we remove the special Dict syntax
  (or (and (not (eq? ex '=>)) (syntactic-op? ex))
      (eq? ex '....)))

(define (parse-unary s)
  (let ((t (require-token s)))
    (if (closing-token? t)
        (error (string "unexpected " t)))
    ;; TODO: ? should probably not be listed here except for the syntax hack in osutils.jl
    (cond ((and (operator? t) (not (memq t '(: |'| ?))) (not (syntactic-unary-op? t))
		(not (invalid-identifier-name? t)))
           (let* ((op  (take-token s))
                  (nch (peek-char (ts:port s))))
             (if (and (or (eq? op '-) (eq? op '+))
                      (or (and (char? nch) (char-numeric? nch))
                          (and (eqv? nch #\.) (read-char (ts:port s)))))
                 (let ((num
                        (parse-juxtapose
                         (read-number (ts:port s) (eqv? nch #\.) (eq? op '-))
                         s)))
                   (if (is-prec-power? (peek-token s))
                       ;; -2^x parsed as (- (^ 2 x))
                       (begin (ts:put-back! s (maybe-negate op num))
                              (list 'call op (parse-factor s)))
                       num))
                 (let ((next (peek-token s)))
                   (cond ((or (closing-token? next) (newline? next) (eq? next '=))
                          op)  ; return operator by itself, as in (+)
                         ((or (eqv? next #\{)  ;; this case is +{T}(x::T) = ...
			      (and (not (memq op unary-ops))
				   (eqv? next #\( )))
                          (ts:put-back! s op)
                          (parse-factor s))
			 ((not (memq op unary-ops))
			  (error (string "\"" op "\" is not a unary operator")))
                         (else
                          (let ((arg (parse-unary s)))
                            (if (and (pair? arg)
                                     (eq? (car arg) 'tuple))
                                (list* 'call op (cdr arg))
                                (list  'call op arg)))))))))
          (else
           (parse-juxtapose (parse-factor s) s)))))

; handle ^ and .^
(define (parse-factor-h s down ops)
  (let ((ex (down s))
        (t (peek-token s)))
    (cond ((not (ops t))
           ex)
          (else
           (list 'call
                 (take-token s) ex (parse-factor-h s parse-unary ops))))))

; -2^3 is parsed as -(2^3), so call parse-decl for the first argument,
; and parse-unary from then on (to handle 2^-3)
(define (parse-factor s)
  (parse-factor-h s parse-decl is-prec-power?))

(define (parse-decl s)
  (let loop ((ex (parse-call s)))
    (let ((t (peek-token s)))
      (case t
        ((|::|) (take-token s)
         (loop (list t ex (parse-call s))))
        ((->)   (take-token s)
         ;; -> is unusual: it binds tightly on the left and
         ;; loosely on the right.
         (let ((lno (line-number-filename-node s)))
           `(-> ,ex (block ,lno ,(parse-eq* s)))))
        (else
         ex)))))

;; convert (comparison a <: b) to (<: a b)
(define (subtype-syntax e)
  (if (and (pair? e) (eq? (car e) 'comparison)
           (length= e 4) (eq? (caddr e) '|<:|))
      `(<: ,(cadr e) ,(cadddr e))
      e))

(define (parse-unary-prefix s)
  (let ((op (peek-token s)))
    (if (syntactic-unary-op? op)
        (begin (take-token s)
               (cond ((closing-token? (peek-token s))  op)
                     ((memq op '(& |::|))  (list op (parse-call s)))
                     (else                 (list op (parse-unary-prefix s)))))
        (parse-atom s))))

;; parse function call, indexing, dot, and transpose expressions
;; also handles looking for syntactic reserved words
(define (parse-call s)
  (let ((ex (parse-unary-prefix s)))
    (if (memq ex reserved-words)
        (parse-resword s ex)
        (parse-call-chain s ex #f))))

(define (deprecated-dict-replacement ex)
  (if (dict-literal? ex)
      (string "Dict{" (deparse (cadr ex)) #\, (deparse (caddr ex)) "}")
      "Dict"))

(define (parse-call-chain s ex one-call)
  (let loop ((ex ex))
    (let ((t (peek-token s)))
      (if (or (and space-sensitive (ts:space? s)
                   (memv t '(#\( #\[ #\{ |'| #\")))
              (and (or (number? ex)  ;; 2(...) is multiply, not call
                       (large-number? ex))
                   (eqv? t #\()))
          ex
          (case t
            ((#\( )   (take-token s)
             (let ((c
                    (let ((al (parse-arglist s #\) )))
                      (receive
                       (params args) (separate (lambda (x)
                                                 (and (pair? x)
                                                      (eq? (car x) 'parameters)))
                                               al)
                       (if (eq? (peek-token s) 'do)
                           (begin
                             (take-token s)
                             `(call ,ex ,@params ,(parse-do s) ,@args))
                           `(call ,ex ,@al))))))
               (if one-call
                   c
                   (loop c))))
            ((#\[ )   (take-token s)
             ;; ref is syntax, so we can distinguish
             ;; a[i] = x  from
             ;; ref(a,i) = x
             (let ((al (with-end-symbol (parse-cat s #\] (dict-literal? ex)))))
               (if (null? al)
                   (if (dict-literal? ex)
                       (begin
                         (syntax-deprecation-warning
                          s (string #\( (deparse ex) #\) "[]")
                          (string (deprecated-dict-replacement ex) "()"))
                         (loop (list 'typed_dict ex)))
                       (loop (list 'ref ex)))
                   (case (car al)
                     ((dict)
                      (if (dict-literal? ex)
                          (begin (syntax-deprecation-warning
                                  s (string #\( (deparse ex) #\) "[a=>b, ...]")
                                  (string (deprecated-dict-replacement ex) "(a=>b, ...)"))
                                 (loop (list* 'typed_dict ex (cdr al))))
                          (loop (list* 'ref ex (cdr al)))))
                     ((vect)  (loop (list* 'ref ex (cdr al))))
                     ((hcat)  (loop (list* 'typed_hcat ex (cdr al))))
                     ((vcat)
                      (loop (list* 'typed_vcat ex (cdr al))))
                     ((macrocall) ; comprehensions
                      (case (cadadr al)
                        ((@comprehension) (loop (list* 'macrocall '(top @typed_comprehension) ex (cddr al))))
                        ((@dict_comprehension) (loop (list* 'macrocall '(top @typed_dict_comprehension) ex (cddr al))))
                        (else (error "unknown parse-comprehension result (internal error)"))))
                     (else (error "unknown parse-cat result (internal error)"))))))
            ((|.|) (take-token s)
             (loop
              (cond ((eqv? (peek-token s) #\()
                     `(|.| ,ex ,(parse-atom s)))
                    ((eq? (peek-token s) '$)
		     (take-token s)
                     (let ((dollarex (parse-atom s)))
                       `(|.| ,ex ($ (call (top Expr) (quote quote)
                                          ,dollarex)))))
                    (else
                     (let ((name (parse-atom s)))
                       (if (and (pair? name) (eq? (car name) 'macrocall))
                           `(macrocall (|.| ,ex (quote ,(cadr name)))
                                       ,@(cddr name))
                           `(|.| ,ex (quote ,name))))))))
            ((|.'| |'|)
	     (if (ts:space? s)
		 (error (string "space not allowed before \"" t "\"")))
	     (take-token s)
             (loop (list t ex)))
            ((#\{ )   (take-token s)
             (loop (list* 'curly ex
                          (map subtype-syntax (parse-arglist s #\} )))))
            ((#\")
             (if (and (symbol? ex) (not (operator? ex))
                      (not (ts:space? s)))
                 ;; custom prefixed string literals, x"s" => @x_str "s"
                 (let* ((str (begin (take-token s)
                                    (parse-string-literal s #t)))
                        (nxt (peek-token s))
                        (macname (symbol (string #\@ ex '_str)))
                        (macstr (if (triplequote-string-literal? str) str (cadr str))))
                   (if (and (symbol? nxt) (not (operator? nxt))
                            (not (ts:space? s)))
                       ;; string literal suffix, "s"x
                       (loop `(macrocall ,macname ,macstr
                                         ,(string (take-token s))))
                       (loop `(macrocall ,macname ,macstr))))
                 ex))
            (else ex))))))

(define expect-end-current-line 0)

(define (expect-end- s word)
  (let ((t (peek-token s)))
    (cond ((eq? t 'end) (take-token s))
          ((eof-object? t)
           (error (string "incomplete: \"" word "\" at " ; NOTE: changing this may affect code in base/client.jl
                          current-filename ":" expect-end-current-line
                          " requires end")))
          (else
           (error (string "\"" word "\" at "
                          current-filename ":" expect-end-current-line
                          " expected \"end\", got \"" t "\""))))))

(define (parse-subtype-spec s)
  (subtype-syntax (parse-comparison s)))

; parse expressions or blocks introduced by syntactic reserved words
(define (parse-resword s word)
  (define (expect-end s) (expect-end- s word))
  (with-bindings ((expect-end-current-line (input-port-line (ts:port s))))
  (with-normal-ops
  (without-whitespace-newline
  (case word
    ((begin quote)
     (let ((loc  (begin (skip-ws-and-comments (ts:port s))
                        (line-number-filename-node s)))
           (blk  (parse-block s)))
       (expect-end s)
       (let ((blk  (if (and (length> blk 1)
                            (pair? (cadr blk)) (eq? (caadr blk) 'line))
                       (list* 'block loc (cddr blk))
                       blk)))
         (if (eq? word 'quote)
             (list 'quote blk)
             blk))))
    ((while)  (begin0 (list 'while (parse-cond s) (parse-block s))
                      (expect-end s)))
    ((for)
     (let* ((ranges (parse-comma-separated-iters s))
            (body   (parse-block s)))
       (expect-end s)
       `(for ,(if (length= ranges 1) (car ranges) (cons 'block ranges))
             ,body)))

    ((if)
     (let* ((test (parse-cond s))
            (then (if (memq (require-token s) '(else elseif))
                      '(block)
                      (parse-block s)))
            (nxt  (require-token s)))
       (take-token s)
       (case nxt
         ((end)     (list 'if test then))
         ((elseif)
          (if (newline? (peek-token s))
              (error (string "missing condition in \"elseif\" at " current-filename
                             ":" (- (input-port-line (ts:port s)) 1))))
          `(if ,test ,then
               ;; line number for elseif condition
               (block ,(line-number-node s)
                      ,(parse-resword s 'if))))
         ((else)
          (if (eq? (peek-token s) 'if)
              (error "use \"elseif\" instead of \"else if\""))
          (begin0 (list 'if test then (parse-block s))
                  (expect-end s)))
         (else      (error (string "unexpected \"" nxt "\""))))))
    ((let)
     (let ((binds (if (memv (peek-token s) '(#\newline #\;))
                      '()
                      (parse-comma-separated-assignments s))))
       (if (not (or (eof-object? (peek-token s))
                    (memv (peek-token s) '(#\newline #\; end))))
           (error "let variables should end in \";\" or newline"))
       (let ((ex (parse-block s)))
         (expect-end s)
         `(let ,ex ,@binds))))
    ((global local)
     (let* ((lno (input-port-line (ts:port s)))
            (const (and (eq? (peek-token s) 'const)
                        (take-token s)))
            (expr  (cons word
                         (map (lambda (x)
                                (short-form-function-loc x lno))
                              (parse-comma-separated-assignments s)))))
       (if const
           `(const ,expr)
           expr)))
    ((stagedfunction function macro)
     (if (eq? word 'stagedfunction) (syntax-deprecation-warning s "stagedfunction" "@generated function"))
     (let* ((paren (eqv? (require-token s) #\())
            (sig   (parse-call s))
            (def   (if (or (symbol? sig)
                           (and (pair? sig) (eq? (car sig) '|::|)
                                (symbol? (cadr sig))))
                       (if paren
                           ;; in "function (x)" the (x) is a tuple
                           `(tuple ,sig)
                           ;; function foo  =>  syntax error
                           (error (string "expected \"(\" in \"" word "\" definition")))
                       (if (not (and (pair? sig)
                                     (or (eq? (car sig) 'call)
                                         (eq? (car sig) 'tuple))))
                           (error (string "expected \"(\" in \"" word "\" definition"))
                           sig)))
            (loc   (begin (if (not (eq? (peek-token s) 'end))
                              ;; if ends on same line, don't skip the following newline
                              (skip-ws-and-comments (ts:port s)))
                          (line-number-filename-node s)))
            (body  (parse-block s)))
       (expect-end s)
       (add-filename-to-block! body loc)
       (list word def body)))
    ((abstract)
     (list 'abstract (parse-subtype-spec s)))
    ((type immutable)
     (let ((immu? (eq? word 'immutable)))
       (if (memq (peek-token s) reserved-words)
	   (error (string "invalid type name \"" (take-token s) "\"")))
       (let ((sig (parse-subtype-spec s)))
         (begin0 (list 'type (if (eq? word 'type) #t #f)
                       sig (parse-block s))
                 (expect-end s)))))
    ((bitstype)
     (list 'bitstype (with-space-sensitive (parse-cond s))
           (parse-subtype-spec s)))
    ((typealias)
     (let ((lhs (parse-call s)))
       (if (and (pair? lhs) (eq? (car lhs) 'call))
           ;; typealias X (...) is tuple type alias, not call
           (list 'typealias (cadr lhs) (cons 'tuple (cddr lhs)))
           (list 'typealias lhs (parse-arrow s)))))
    ((try)
     (let ((try-block (if (memq (require-token s) '(catch finally))
                          '(block)
                          (parse-block s))))
       (let loop ((nxt    (require-token s))
                  (catchb #f)
                  (catchv #f)
                  (finalb #f))
         (take-token s)
         (cond
          ((eq? nxt 'end)
           (list* 'try try-block catchv
                  ;; default to empty catch block in `try ... end`
                  (or catchb (if finalb #f '(block)))
                  (if finalb (list finalb) '())))
          ((and (eq? nxt 'catch)
                (not catchb))
           (let ((nl (memv (peek-token s) '(#\newline #\;))))
             (if (eqv? (peek-token s) #\;)
                 (take-token s))
             (if (memq (require-token s) '(end finally))
                 (loop (require-token s)
                       '(block)
                       #f
                       finalb)
                 (let* ((var (parse-eq* s))
                        (var? (and (not nl) (or (symbol? var) (and (length= var 2) (eq? (car var) '$)))))
                        (catch-block (if (eq? (require-token s) 'finally)
                                         '(block)
                                         (parse-block s))))
                   (loop (require-token s)
                         (if var?
                             catch-block
                             `(block ,var ,@(cdr catch-block)))
                         (and var? var)
                         finalb)))))
          ((and (eq? nxt 'finally)
                (not finalb))
           (let ((fb (if (eq? (require-token s) 'catch)
                         '(block)
                         (parse-block s))))
             (loop (require-token s)
                   catchb
                   catchv
                   fb)))
          (else    (error (string "unexpected \"" nxt "\"")))))))
    ((return)          (let ((t (peek-token s)))
                         (if (or (eqv? t #\newline) (closing-token? t))
                             (list 'return '(null))
                             (list 'return (parse-eq s)))))
    ((break continue)  (list word))
    ((const)
     (let ((assgn (parse-eq s)))
       (if (not (and (pair? assgn)
                     (or (eq? (car assgn) '=)
                         (eq? (car assgn) 'global)
                         (eq? (car assgn) 'local))))
           (error "expected assignment after \"const\"")
           `(const ,assgn))))
    ((module baremodule)
     (let* ((name (parse-unary-prefix s))
            (body (parse-block s)))
       (expect-end s)
       (list 'module (eq? word 'module) name
             (if (eq? word 'module)
                 (list* 'block
                        ;; add definitions for module-local eval
                        (let ((x (if (eq? name 'x) 'y 'x)))
                          `(= (call eval ,x)
                              (call (|.| (top Core) 'eval) ,name ,x)))
                        `(= (call eval m x)
                            (call (|.| (top Core) 'eval) m x))
                        (cdr body))
                 body))))
    ((export)
     (let ((es (map macrocall-to-atsym
                    (parse-comma-separated s parse-atom))))
       (if (not (every symbol? es))
           (error "invalid \"export\" statement"))
       `(export ,@es)))
    ((import using importall)
     (let ((imports (parse-imports s word)))
       (if (length= imports 1)
           (car imports)
           (cons 'toplevel imports))))
    ((ccall)
     (if (not (eqv? (peek-token s) #\())
         (error "invalid \"ccall\" syntax")
         (begin
           (take-token s)
           (let ((al (parse-arglist s #\))))
             (if (and (length> al 1)
                      (memq (cadr al) '(cdecl stdcall fastcall thiscall)))
                 ;; place (callingconv) at end of arglist
                 `(ccall ,(car al) ,@(cddr al) (,(cadr al)))
                 `(ccall ,.al))))))
    ((do)
     (error "invalid \"do\" syntax"))
    (else (error "unhandled reserved word")))))))

(define (add-filename-to-block! body loc)
  (if (and (length> body 1)
           (pair? (cadr body))
           (eq? (caadr body) 'line))
      (set-car! (cdr body) loc))
  body)

(define (parse-do s)
  (with-bindings ((expect-end-current-line (input-port-line (ts:port s))))
  (without-whitespace-newline
   (let* ((doargs (if (eqv? (peek-token s) #\newline)
                      '()
                      (parse-comma-separated s parse-range)))
          (loc (line-number-filename-node s)))
     `(-> (tuple ,@doargs)
          ,(begin0 (add-filename-to-block! (parse-block s) loc)
                   (expect-end- s 'do)))))))

(define (macrocall-to-atsym e)
  (if (and (pair? e) (eq? (car e) 'macrocall))
      (cadr e)
      e))

(define (parse-imports s word)
  (let* ((first (parse-import s word))
         (next  (peek-token s))
         (from  (and (eq? next ':) (not (ts:space? s))))
         (done  (cond ((or from (eqv? next #\,))
                       (begin (take-token s) #f))
                      ((memv next '(#\newline #\;)) #t)
                      ((eof-object? next) #t)
                      (else #f)))
         (rest  (if done
                    '()
                    (parse-comma-separated s (lambda (s)
                                               (parse-import s word))))))
    (if from
        (map (lambda (x)
               (cons (car x) (append (cdr first) (cdr x))))
             rest)
        (cons first rest))))

(define (parse-import-dots s)
  (let loop ((l '())
             (t (peek-token s)))
    (cond ((eq? t '|.|)
           (begin (take-token s)
                  (loop (list* '|.| l) (peek-token s))))
          ((eq? t '..)
           (begin (take-token s)
                  (loop (list* '|.| '|.| l) (peek-token s))))
          ((eq? t '...)
           (begin (take-token s)
                  (loop (list* '|.| '|.| '|.| l) (peek-token s))))
          ((eq? t '....)
           (begin (take-token s)
                  (loop (list* '|.| '|.| '|.| '|.| l) (peek-token s))))
          (else
           (cons (macrocall-to-atsym (parse-atom s)) l)))))

(define (parse-import s word)
  (let loop ((path (parse-import-dots s)))
    (if (not (symbol? (car path)))
        (error (string "invalid \"" word "\" statement: expected identifier")))
    (let ((nxt (peek-token s)))
      (cond
       ((eq? nxt '|.|)
        (take-token s)
        (loop (cons (macrocall-to-atsym (parse-atom s)) path)))
       ((or (memv nxt '(#\newline #\; #\, :))
            (eof-object? nxt))
        `(,word ,@(reverse path)))
       ((eqv? (string.sub (string nxt) 0 1) ".")
        (take-token s)
        (loop (cons (symbol (string.sub (string nxt) 1))
                    path)))
       (else
        (error (string "invalid \"" word "\" statement")))))))

; parse comma-separated assignments, like "i=1:n,j=1:m,..."
(define (parse-comma-separated s what)
  (let loop ((exprs '()))
    (let ((r (what s)))
      (case (peek-token s)
        ((#\,)  (take-token s) (loop (cons r exprs)))
        (else   (reverse! (cons r exprs)))))))

(define (parse-comma-separated-assignments s)
  (parse-comma-separated s parse-eq*))

; as above, but allows both "i=r" and "i in r"
(define (parse-comma-separated-iters s)
  (let loop ((ranges '()))
    (let ((r (parse-eq* s)))
      (let ((r (cond ((and (pair? r) (eq? (car r) '=))
                      r)
                     ((eq? r ':)
                      r)
                     ((and (pair? r) (eq? (car r) 'in))
                      `(= ,(cadr r) ,(caddr r)))
                     (else
                      (error "invalid iteration specification")))))
        (case (peek-token s)
          ((#\,)  (take-token s) (loop (cons r ranges)))
          (else   (reverse! (cons r ranges))))))))

(define (parse-space-separated-exprs s)
  (with-space-sensitive
   (let loop ((exprs '()))
     (if (or (closing-token? (peek-token s))
             (newline? (peek-token s))
             (and inside-vec (eq? (peek-token s) 'for)))
         (reverse! exprs)
         (let ((e (parse-eq s)))
           (case (peek-token s)
             ((#\newline)   (reverse! (cons e exprs)))
             (else          (loop (cons e exprs)))))))))

(define (has-parameters? lst)
  (and (pair? lst) (pair? (car lst)) (eq? (caar lst) 'parameters)))

(define (to-kws lst)
  (map (lambda (x) (if (assignment? x)
                       `(kw ,@(cdr x))
                       x))
       lst))

; handle function call argument list, or any comma-delimited list.
; . an extra comma at the end is allowed
; . expressions after a ; are enclosed in (parameters ...)
; . an expression followed by ... becomes (... x)
(define (parse-arglist s closer)
  (with-normal-ops
   (with-whitespace-newline
    (parse-arglist- s closer))))
(define (parse-arglist- s closer)
  (let loop ((lst '()))
    (let ((t (require-token s)))
      (if (equal? t closer)
          (begin (take-token s)
                 (if (eqv? closer #\) )
                     ;; (= x y) inside function call is keyword argument
                     (to-kws (reverse lst))
                     (reverse lst)))
          (if (equal? t #\;)
              (begin (take-token s)
                     (if (equal? (peek-token s) closer)
                         ;; allow f(a, b; )
                         (loop lst)
                         (let ((params (loop '()))
                               (lst    (if (eqv? closer #\) )
                                           (to-kws (reverse lst))
                                           (reverse lst))))
                           (cons (cons 'parameters params)
                                 lst))))
              (let* ((nxt (parse-eq* s))
                     (c (require-token s)))
                (cond ((eqv? c #\,)
                       (begin (take-token s) (loop (cons nxt lst))))
                      ((eqv? c #\;)          (loop (cons nxt lst)))
                      ((equal? c closer)     (loop (cons nxt lst)))
                      ;; newline character isn't detectable here
                      #;((eqv? c #\newline)
                       (error "unexpected line break in argument list"))
                      ((memv c '(#\] #\}))
                       (error (string "unexpected \"" c "\" in argument list")))
                      (else
                       (error (string "missing comma or " closer
                                      " in argument list"))))))))))

(define (parse-vect s first closer)
  (let loop ((lst '())
             (nxt first))
    (let ((t (require-token s)))
      (if (eqv? t closer)
          (begin (take-token s)
                 (cons 'vect (reverse (cons nxt lst))))
          (case t
            ((#\,)
             (take-token s)
             (if (eqv? (require-token s) closer)
                 ;; allow ending with ,
                 (begin (take-token s)
                        (cons 'vect (reverse (cons nxt lst))))
                 (loop (cons nxt lst) (parse-eq* s))))
            ((#\;)
             (if (eqv? (require-token s) closer)
                 (loop lst nxt)
                 (let ((params (parse-arglist s closer)))
                   `(vcat ,@params ,@(reverse lst) ,nxt))))
            ((#\] #\})
             (error (string "unexpected \"" t "\"")))
            (else
             (error "missing separator in array expression")))))))

(define (parse-dict s first closer)
  (let ((v (parse-vect s first closer)))
    (if (any dict-literal? (cdr v))
        (if (every dict-literal? (cdr v))
            `(dict ,@(cdr v))
            (error "invalid dict literal")))))

(define (parse-comprehension s first closer)
  (let ((r (parse-comma-separated-iters s)))
    (if (not (eqv? (require-token s) closer))
        (error (string "expected " closer))
        (take-token s))
    `(macrocall (top ,(symbol "@comprehension")) ,first ,@r)))

(define (parse-dict-comprehension s first closer)
  (let ((c (parse-comprehension s first closer)))
    (if (dict-literal? (caddr c))
        `(macrocall (top ,(symbol "@dict_comprehension")) ,@(cddr c))
        (error "invalid dict comprehension"))))

(define (parse-matrix s first closer gotnewline)
  (define (fix head v) (cons head (reverse v)))
  (define (update-outer v outer)
    (cond ((null? v)       outer)
          ((null? (cdr v)) (cons (car v) outer))
          (else            (cons (fix 'row v) outer))))
  (define semicolon (eqv? (peek-token s) #\;))
  (let loop ((vec   (list first))
             (outer '()))
    (let ((t  (if (or (eqv? (peek-token s) #\newline) gotnewline)
                  #\newline
                  (require-token s))))
      (if (eqv? t closer)
          (begin (take-token s)
                 (if (pair? outer)
                     (fix 'vcat (update-outer vec outer))
                     (if (or (null? vec) (null? (cdr vec)))
                         (fix 'vect vec)     ; [x]   => (vect x)
                         (fix 'hcat vec))))  ; [x y] => (hcat x y)
          (case t
            ((#\; #\newline)
             (or gotnewline (take-token s))
             (set! gotnewline #f)
             (loop '() (update-outer vec outer)))
            ((#\,)
             (error "unexpected comma in matrix expression"))
            ((#\] #\})
             (error (string "unexpected \"" t "\"")))
            ((for)
             (if (and (not semicolon)
                      (length= outer 1)
                      (null? vec))
                 (begin (take-token s)
                        (parse-comprehension s (car outer) closer))
                 (error "invalid comprehension syntax")))
            (else
             (loop (cons (parse-eq* s) vec) outer)))))))

(define (peek-non-newline-token s)
  (let loop ((t (peek-token s)))
    (if (newline? t)
        (begin (take-token s)
               (loop (peek-token s)))
        t)))

(define (parse-cat s closer . isdict)
  (with-normal-ops
   (with-inside-vec
    (if (eqv? (require-token s) closer)
        (begin (take-token s)
               '())
        (let ((first (parse-eq* s)))
          (if (and (dict-literal? first)
                   (or (null? isdict) (car isdict)))
              (case (peek-non-newline-token s)
                ((for)
                 (take-token s)
                 (parse-dict-comprehension s first closer))
                (else
                 (if (or (null? isdict) (not (car isdict)))
                     (syntax-deprecation-warning s "[a=>b, ...]" "Dict(a=>b, ...)"))
                 (parse-dict s first closer)))
              (let ((t (peek-token s)))
                (cond ((or (eqv? t #\,) (eqv? t closer))
                       (parse-vect s first closer))
                      ((eq? t 'for)
                       (take-token s)
                       (parse-comprehension s first closer))
                      ((eqv? t #\newline)
                       (take-token s)
                       (if (memv (peek-token s) (list #\, closer))
                           (parse-vect s first closer)
                           (parse-matrix s first closer #t)))
                      (else
                       (parse-matrix s first closer #f))))))))))

; for sequenced evaluation inside expressions: e.g. (a;b, c;d)
(define (parse-stmts-within-expr s)
  (parse-Nary s parse-eq* '(#\;) 'block '(#\, #\) ) #t))

(define (parse-tuple s first)
  (let loop ((lst '())
             (nxt first))
    (case (require-token s)
      ((#\))
       (take-token s)
       (cons 'tuple (reverse (cons nxt lst))))
      ((#\,)
       (take-token s)
       (if (eqv? (require-token s) #\))
           ;; allow ending with ,
           (begin (take-token s)
                  (cons 'tuple (reverse (cons nxt lst))))
           (loop (cons nxt lst) (parse-eq* s))))
      ((#\;)
       (error "unexpected semicolon in tuple"))
      #;((#\newline)
      (error "unexpected line break in tuple"))
      ((#\] #\})
       (error (string "unexpected \"" (peek-token s) "\" in tuple")))
      (else
       (error "missing separator in tuple")))))

(define (not-eof-2 c)
  (if (eof-object? c)
      (error "incomplete: invalid \"`\" syntax") ; NOTE: changing this may affect code in base/client.jl
      c))

(define (parse-backquote s)
  (let ((b (open-output-string))
        (p (ts:port s)))
    (let loop ((c (read-char p)))
      (if (eqv? c #\`)
          #t
          (begin (if (eqv? c #\\)
                     (let ((nextch (read-char p)))
                       (if (eqv? nextch #\`)
                           (write-char nextch b)
                           (begin (write-char #\\ b)
                                  (write-char (not-eof-2 nextch) b))))
                     (write-char (not-eof-2 c) b))
                 (loop (read-char p)))))
    (let ((str (io.tostring! b)))
      `(macrocall @cmd ,str))))

(define (not-eof-3 c)
  (if (eof-object? c)
      (error "incomplete: invalid string syntax") ; NOTE: changing this may affect code in base/client.jl
      c))

(define (take-char p)
  (begin (read-char p) p))

(define (parse-string-literal s custom)
  (let ((p (ts:port s)))
    (if (eqv? (peek-char p) #\")
        (if (eqv? (peek-char (take-char p)) #\")
            (parse-string-literal- 'triple_quoted_string 2 (take-char p) s custom)
            '(single_quoted_string ""))
        (parse-string-literal- 'single_quoted_string 0 p s custom))))

(define (parse-interpolate s)
  (let* ((p (ts:port s))
         (c (peek-char p)))
    (cond ((identifier-start-char? c)
           (parse-atom s))
          ((eqv? c #\()
           (read-char p)
           (let ((ex (parse-eq* s))
                 (t (require-token s)))
             (cond ((eqv? t #\) )
                    (take-token s)
                    ex)
                   (else (error "invalid interpolation syntax")))))
          (else (error (string "invalid interpolation syntax: \"$" c "\""))))))

(define (tostr custom io)
  (if custom
      (io.tostring! io)
      (let ((str (unescape-string (io.tostring! io))))
        (if (not (string.isutf8 str))
            (error "invalid UTF-8 sequence")
            str))))

;; custom = custom string literal
;; when custom is #t, unescape only \\ and \"
;; otherwise do full unescaping, and parse interpolations too
(define (parse-string-literal- head n p s custom)
  (let loop ((c (read-char p))
             (b (open-output-string))
             (e (list head))
             (quotes 0))
    (cond
      ((eqv? c #\")
       (if (< quotes n)
           (loop (read-char p) b e (+ quotes 1))
           (reverse (cons (tostr custom b) e))))

      ((= quotes 1)
       (if (not custom) (write-char #\\ b))
       (write-char #\" b)
       (loop c b e 0))

      ((= quotes 2)
       (if (not custom) (write-char #\\ b))
       (write-char #\" b)
       (if (not custom) (write-char #\\ b))
       (write-char #\" b)
       (loop c b e 0))

      ((eqv? c #\\)
       (let ((nxch (not-eof-3 (read-char p))))
         (if (or (not custom)
                 (not (or (eqv? nxch #\") #;(eqv? nxch #\\))))
             (write-char #\\ b))
         (write-char nxch b)
         (loop (read-char p) b e 0)))

      ((and (eqv? c #\$) (not custom))
       (let ((ex (parse-interpolate s)))
         (loop (read-char p)
               (open-output-string)
               (list* ex (tostr custom b) e)
               0)))

      (else
       (write-char (not-eof-3 c) b)
       (loop (read-char p) b e 0)))))

(define (interpolate-string-literal? s) (length> s 2))
(define (triplequote-string-literal? s) (eqv? (car s) 'triple_quoted_string))

(define (not-eof-1 c)
  (if (eof-object? c)
      (error "incomplete: invalid character literal") ; NOTE: changing this may affect code in base/client.jl
      c))

(define (unescape-string s)
  (with-exception-catcher
   (lambda (e) (error "invalid escape sequence"))
   (lambda ()
     ;; process escape sequences using lisp read
     (read (open-input-string (string #\" s #\"))))))

; parse numbers, identifiers, parenthesized expressions, lists, vectors, etc.
(define (parse-atom s)
  (let ((ex (parse-atom- s)))
    (if (invalid-identifier-name? ex)
        (error (string "invalid identifier name \"" ex "\"")))
    ex))

(define (parse-atom- s)
  (let ((t (require-token s)))
    (cond ((or (string? t) (number? t) (large-number? t)) (take-token s))

          ;; char literal
          ((eq? t '|'|)
           (take-token s)
           (let ((firstch (read-char (ts:port s))))
             (if (eqv? firstch #\')
              (error "invalid character literal")
              (if (and (not (eqv? firstch #\\))
                       (not (eof-object? firstch))
                       (eqv? (peek-char (ts:port s)) #\'))
               ;; easy case: 1 character, no \
               (begin (read-char (ts:port s)) firstch)
               (let ((b (open-output-string)))
                 (let loop ((c firstch))
                   (if (eqv? c #\')
                       #t
                       (begin (write-char (not-eof-1 c) b)
                              (if (eqv? c #\\)
                                  (write-char
                                   (not-eof-1 (read-char (ts:port s))) b))
                              (loop (read-char (ts:port s))))))
                 (let ((str (unescape-string (io.tostring! b))))
                   (if (= (length str) 1)
                       ;; one byte, e.g. '\xff'. maybe not valid UTF-8, but we
                       ;; want to use the raw value as a codepoint in this case.
                       (wchar (aref str 0))
                       (if (or (not (= (string-length str) 1))
                               (not (string.isutf8 str)))
                           (error "invalid character literal")
                           (string.char str 0)))))))))

          ;; symbol/expression quote
          ((eq? t ':)
           (take-token s)
           (let ((nxt (peek-token s)))
             (if (and (closing-token? nxt)
                      (or (not (symbol? nxt))
                          (ts:space? s)))
                 ':
		 (if (ts:space? s)
		     (error "space not allowed after \":\" used for quoting")
		     (list 'quote (parse-atom- s))))))

          ;; misplaced =
          ((eq? t '=) (error "unexpected \"=\""))

          ;; identifier
          ((symbol? t) (take-token s))

          ;; parens or tuple
          ((eqv? t #\( )
           (take-token s)
           (with-normal-ops
           (with-whitespace-newline
           (cond
            ((eqv? (require-token s) #\) )
             ;; empty tuple ()
             (begin (take-token s) '(tuple)))
            ((syntactic-op? (peek-token s))
             ;; allow (=) etc.
             (let ((tok (take-token s)))
               (if (not (eqv? (require-token s) #\) ))
                   (error (string "invalid identifier name \"" tok "\""))
                   (take-token s))
               tok))
            (else
             ;; here we parse the first subexpression separately, so
             ;; we can look for a comma to see if it's a tuple.
             ;; this lets us distinguish (x) from (x,)
             (let* ((ex (parse-eq* s))
                    (t  (require-token s)))
               (cond ((eqv? t #\) )
                      (take-token s)
                      (if (and (pair? ex) (eq? (car ex) '...))
                          ;; (ex...)
                          `(tuple ,ex)
                          ;; value in parentheses (x)
                          ex))
                     ((eqv? t #\, )
                      ;; tuple (x,) (x,y) (x...) etc.
                      (parse-tuple s ex))
                     ((eqv? t #\;)
                      ;; parenthesized block (a;b;c)
                      (take-token s)
                      (if (eqv? (require-token s) #\))
                          ;; (ex;)
                          (begin (take-token s) `(block ,ex))
                          (let* ((blk (parse-stmts-within-expr s))
                                 (tok (require-token s)))
                            (if (eqv? tok #\,)
                                (error "unexpected comma in statement block"))
                            (if (not (eqv? tok #\)))
                                (error "missing separator in statement block"))
                            (take-token s)
                            `(block ,ex ,blk))))
                     #;((eqv? t #\newline)
                        (error "unexpected line break in tuple"))
                     ((memv t '(#\] #\}))
                      (error (string "unexpected \"" t "\" in tuple")))
                     (else
                      (error "missing separator in tuple")))))))))

          ;; cell expression
          ((eqv? t #\{ )
           (take-token s)
           (if (eqv? (require-token s) #\})
               (begin (syntax-deprecation-warning s "{}" "[]")
                      (take-token s)
                      '(cell1d))
               (let ((vex (parse-cat s #\} #t)))
                 (if (null? vex)
                     (begin (syntax-deprecation-warning s "{}" "[]")
                            '(cell1d))
                     (case (car vex)
                       ((vect)
                        (syntax-deprecation-warning s "{a,b, ...}" "Any[a,b, ...]")
                        `(cell1d ,@(cdr vex)))
                       ((comprehension)
                        (syntax-deprecation-warning s "{a for a in b}" "Any[a for a in b]")
                        `(typed_comprehension (top Any) ,@(cdr vex)))
                       ((dict_comprehension)
                        (syntax-deprecation-warning s "{a=>b for (a,b) in c}" "Dict{Any,Any}([a=>b for (a,b) in c])")
                        `(typed_dict_comprehension (=> (top Any) (top Any)) ,@(cdr vex)))
                       ((dict)
                        (syntax-deprecation-warning s "{a=>b, ...}" "Dict{Any,Any}(a=>b, ...)")
                        `(typed_dict (=> (top Any) (top Any)) ,@(cdr vex)))
                       ((hcat)
                        (syntax-deprecation-warning s "{a b ...}" "Any[a b ...]")
                        `(cell2d 1 ,(length (cdr vex)) ,@(cdr vex)))
                       (else  ; (vcat ...)
                        (if (and (pair? (cadr vex)) (eq? (caadr vex) 'row))
                            (let ((nr (length (cdr vex)))
                                  (nc (length (cdadr vex))))
                              ;; make sure all rows are the same length
                              (if (not (every
                                        (lambda (x)
                                          (and (pair? x)
                                               (eq? (car x) 'row)
                                               (length= (cdr x) nc)))
                                        (cddr vex)))
                                  (error "inconsistent shape in cell expression"))
                              (begin
                                (syntax-deprecation-warning s "{a b; c d}" "Any[a b; c d]")
                                `(cell2d ,nr ,nc
                                         ,@(apply append
                                                  ;; transpose to storage order
                                                  (apply map list
                                                         (map cdr (cdr vex)))))))
                            (if (any (lambda (x) (and (pair? x)
                                                      (eq? (car x) 'row)))
                                     (cddr vex))
                                (error "inconsistent shape in cell expression")
                                (begin
                                  (syntax-deprecation-warning s "{a,b, ...}" "Any[a,b, ...]")
                                  `(cell1d ,@(cdr vex)))))))))))

          ;; cat expression
          ((eqv? t #\[ )
           (take-token s)
           (let ((vex (parse-cat s #\])))
             (if (null? vex) '(vect) vex)))

          ;; string literal
          ((eqv? t #\")
           (take-token s)
           (let ((ps (parse-string-literal s #f)))
             (if (triplequote-string-literal? ps)
                 `(macrocall @mstr ,@(cdr ps))
                 (if (interpolate-string-literal? ps)
                     `(string ,@(filter (lambda (s)
                                          (not (and (string? s)
                                                    (= (length s) 0))))
                                        (cdr ps)))
                     (cadr ps)))))

          ;; macro call
          ((eqv? t #\@)
           (take-token s)
           (with-space-sensitive
            (let* ((head (parse-unary-prefix s))
                   (t    (peek-token s)))
              (if (ts:space? s)
                  `(macrocall ,(macroify-name head)
                              ,@(parse-space-separated-exprs s))
                  (let ((call (parse-call-chain s head #t)))
                    (if (and (pair? call) (eq? (car call) 'call))
                        `(macrocall ,(macroify-name (cadr call)) ,@(cddr call))
                        `(macrocall ,(macroify-name call)
                                    ,@(parse-space-separated-exprs s))))))))

          ;; command syntax
          ((eqv? t #\`)
           (take-token s)
           (parse-backquote s))

          (else (error (string "invalid syntax: \"" (take-token s) "\""))))))

(define (valid-modref? e)
  (and (length= e 3) (eq? (car e) '|.|) (pair? (caddr e))
       (eq? (car (caddr e)) 'quote) (symbol? (cadr (caddr e)))
       (or (symbol? (cadr e))
           (valid-modref? (cadr e)))))

(define (macroify-name e)
  (cond ((symbol? e)  (symbol (string #\@ e)))
        ((valid-modref? e)  `(|.| ,(cadr e)
                              (quote ,(macroify-name (cadr (caddr e))))))
        (else (error (string "invalid macro use \"@(" (deparse e) ")\"" )))))

; --- main entry point ---

;; can optionally specify which grammar production to parse.
;; default is parse-stmts.
(define (julia-parse s . production)
  (cond ((string? s)
         (apply julia-parse (make-token-stream (open-input-string s))
                production))
        ((port? s)
         (apply julia-parse (make-token-stream s) production))
        ((eof-object? s)
         s)
        (else
         ;; as a special case, allow early end of input if there is
         ;; nothing left but whitespace
         (skip-ws-and-comments (ts:port s))
         (let skip-loop ((tok (peek-token s)))
           (if (or (eqv? tok #\newline) )
               (begin (take-token s) (skip-loop (peek-token s)))))
         (if (eof-object? (peek-token s))
             (eof-object)
             ((if (null? production) parse-stmts (car production))
              s)))))
