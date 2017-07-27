;; Operator precedence table, lowest at top

; for most operators X there is a .X "elementwise" equivalent
(define (add-dots ops) (append! ops (map (lambda (op) (symbol (string "." op))) ops)))

;; note: there are some strange-looking things in here because
;; the way the lexer works, every prefix of an operator must also
;; be an operator.
(define prec-assignment
  (append! (add-dots '(= += -= *= /= //= |\\=| ^= ÷= %= <<= >>= >>>= |\|=| &= ⊻=))
           '(:= => ~ $=)))
(define prec-conditional '(?))
(define prec-arrow       (append!
                          '(-- -->)
                          (add-dots '(← → ↔ ↚ ↛ ↠ ↣ ↦ ↮ ⇎ ⇏ ⇒ ⇔ ⇴ ⇶ ⇷ ⇸ ⇹ ⇺ ⇻ ⇼ ⇽ ⇾ ⇿ ⟵ ⟶ ⟷ ⟷ ⟹ ⟺ ⟻ ⟼ ⟽ ⟾ ⟿ ⤀ ⤁ ⤂ ⤃ ⤄ ⤅ ⤆ ⤇ ⤌ ⤍ ⤎ ⤏ ⤐ ⤑ ⤔ ⤕ ⤖ ⤗ ⤘ ⤝ ⤞ ⤟ ⤠ ⥄ ⥅ ⥆ ⥇ ⥈ ⥊ ⥋ ⥎ ⥐ ⥒ ⥓ ⥖ ⥗ ⥚ ⥛ ⥞ ⥟ ⥢ ⥤ ⥦ ⥧ ⥨ ⥩ ⥪ ⥫ ⥬ ⥭ ⥰ ⧴ ⬱ ⬰ ⬲ ⬳ ⬴ ⬵ ⬶ ⬷ ⬸ ⬹ ⬺ ⬻ ⬼ ⬽ ⬾ ⬿ ⭀ ⭁ ⭂ ⭃ ⭄ ⭇ ⭈ ⭉ ⭊ ⭋ ⭌ ￩ ￫))))
(define prec-lazy-or     '(|\|\||))
(define prec-lazy-and    '(&&))
(define prec-comparison
  (append! '(|<:| |>:| in isa)
           (add-dots '(> < >= ≥ <= ≤ == === ≡ != ≠ !== ≢ ∈ ∉ ∋ ∌ ⊆ ⊈ ⊂ ⊄ ⊊ ∝ ∊ ∍ ∥ ∦ ∷ ∺ ∻ ∽ ∾ ≁ ≃ ≄ ≅ ≆ ≇ ≈ ≉ ≊ ≋ ≌ ≍ ≎ ≐ ≑ ≒ ≓ ≔ ≕ ≖ ≗ ≘ ≙ ≚ ≛ ≜ ≝ ≞ ≟ ≣ ≦ ≧ ≨ ≩ ≪ ≫ ≬ ≭ ≮ ≯ ≰ ≱ ≲ ≳ ≴ ≵ ≶ ≷ ≸ ≹ ≺ ≻ ≼ ≽ ≾ ≿ ⊀ ⊁ ⊃ ⊅ ⊇ ⊉ ⊋ ⊏ ⊐ ⊑ ⊒ ⊜ ⊩ ⊬ ⊮ ⊰ ⊱ ⊲ ⊳ ⊴ ⊵ ⊶ ⊷ ⋍ ⋐ ⋑ ⋕ ⋖ ⋗ ⋘ ⋙ ⋚ ⋛ ⋜ ⋝ ⋞ ⋟ ⋠ ⋡ ⋢ ⋣ ⋤ ⋥ ⋦ ⋧ ⋨ ⋩ ⋪ ⋫ ⋬ ⋭ ⋲ ⋳ ⋴ ⋵ ⋶ ⋷ ⋸ ⋹ ⋺ ⋻ ⋼ ⋽ ⋾ ⋿ ⟈ ⟉ ⟒ ⦷ ⧀ ⧁ ⧡ ⧣ ⧤ ⧥ ⩦ ⩧ ⩪ ⩫ ⩬ ⩭ ⩮ ⩯ ⩰ ⩱ ⩲ ⩳ ⩴ ⩵ ⩶ ⩷ ⩸ ⩹ ⩺ ⩻ ⩼ ⩽ ⩾ ⩿ ⪀ ⪁ ⪂ ⪃ ⪄ ⪅ ⪆ ⪇ ⪈ ⪉ ⪊ ⪋ ⪌ ⪍ ⪎ ⪏ ⪐ ⪑ ⪒ ⪓ ⪔ ⪕ ⪖ ⪗ ⪘ ⪙ ⪚ ⪛ ⪜ ⪝ ⪞ ⪟ ⪠ ⪡ ⪢ ⪣ ⪤ ⪥ ⪦ ⪧ ⪨ ⪩ ⪪ ⪫ ⪬ ⪭ ⪮ ⪯ ⪰ ⪱ ⪲ ⪳ ⪴ ⪵ ⪶ ⪷ ⪸ ⪹ ⪺ ⪻ ⪼ ⪽ ⪾ ⪿ ⫀ ⫁ ⫂ ⫃ ⫄ ⫅ ⫆ ⫇ ⫈ ⫉ ⫊ ⫋ ⫌ ⫍ ⫎ ⫏ ⫐ ⫑ ⫒ ⫓ ⫔ ⫕ ⫖ ⫗ ⫘ ⫙ ⫷ ⫸ ⫹ ⫺ ⊢ ⊣))))
(define prec-pipe        (add-dots '(|\|>| |<\||)))
(define prec-colon       '(: |..|))
(define prec-plus        (append! '($)
                          (add-dots '(+ - |\|| ⊕ ⊖ ⊞ ⊟ |++| ∪ ∨ ⊔ ± ∓ ∔ ∸ ≂ ≏ ⊎ ⊻ ⊽ ⋎ ⋓ ⧺ ⧻ ⨈ ⨢ ⨣ ⨤ ⨥ ⨦ ⨧ ⨨ ⨩ ⨪ ⨫ ⨬ ⨭ ⨮ ⨹ ⨺ ⩁ ⩂ ⩅ ⩊ ⩌ ⩏ ⩐ ⩒ ⩔ ⩖ ⩗ ⩛ ⩝ ⩡ ⩢ ⩣))))
(define prec-bitshift    (add-dots '(<< >> >>>)))
(define prec-times       (add-dots '(* / ÷ % & ⋅ ∘ × |\\| ∩ ∧ ⊗ ⊘ ⊙ ⊚ ⊛ ⊠ ⊡ ⊓ ∗ ∙ ∤ ⅋ ≀ ⊼ ⋄ ⋆ ⋇ ⋉ ⋊ ⋋ ⋌ ⋏ ⋒ ⟑ ⦸ ⦼ ⦾ ⦿ ⧶ ⧷ ⨇ ⨰ ⨱ ⨲ ⨳ ⨴ ⨵ ⨶ ⨷ ⨸ ⨻ ⨼ ⨽ ⩀ ⩃ ⩄ ⩋ ⩍ ⩎ ⩑ ⩓ ⩕ ⩘ ⩚ ⩜ ⩞ ⩟ ⩠ ⫛ ⊍ ▷ ⨝ ⟕ ⟖ ⟗)))
(define prec-rational    (add-dots '(//)))
;; `where`
;; unary
(define prec-power       (add-dots '(^ ↑ ↓ ⇵ ⟰ ⟱ ⤈ ⤉ ⤊ ⤋ ⤒ ⤓ ⥉ ⥌ ⥍ ⥏ ⥑ ⥔ ⥕ ⥘ ⥙ ⥜ ⥝ ⥠ ⥡ ⥣ ⥥ ⥮ ⥯ ￪ ￬)))
(define prec-decl        '(|::|))
;; `where` occurring after `::`
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
        ((and (every symbol? l) (not (length> l 20)))
         (eval `(lambda (x)
                  (not (not (memq x (quote ,l)))))))
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

(define unary-ops (append! '(|<:| |>:|)
                           (add-dots '(+ - ! ~ ¬ √ ∛ ∜))))

(define unary-op? (Set unary-ops))

; operators that are both unary and binary
(define unary-and-binary-ops '(+ - $ & ~ |.+| |.-|))

; operators that are special forms, not function names
(define syntactic-operators
  (append! (add-dots '(= += -= *= /= //= |\\=| ^= ÷= %= <<= >>= >>>= |\|=| &= ⊻=))
           '(:= --> $= && |\|\|| |.| ... ->)))
(define syntactic-unary-operators '($ & |::|))

(define syntactic-op? (Set syntactic-operators))
(define syntactic-unary-op? (Set syntactic-unary-operators))

(define (symbol-or-interpolate? ex)
  (or (symbol? ex)
      (and (pair? ex)
           (eq? '$ (car ex)))))

(define trans-op (string->symbol ".'"))
(define ctrans-op (string->symbol "'"))
(define vararg-op (string->symbol "..."))

(define (is-word-operator? op)
  (every identifier-start-char? (string->list (symbol->string op))))

(define operators
  (filter (lambda (x) (not (is-word-operator? x)))
          (delete-duplicates
           (list* '-> ctrans-op trans-op vararg-op
                  (append unary-ops (apply append (map eval prec-names)))))))

(define op-chars
  (delete-duplicates
   (apply append
          (map string->list (map symbol->string operators)))))

;; characters that can be in an operator
(define opchar? (Set op-chars))
;; characters that can follow . in an operator
(define dot-opchar? (Set
                     (delete-duplicates
                      (map (lambda (op) (string.char (string op) 1))
                           (cons `|..| (filter dotop? operators))))))
(define operator? (Set operators))

(define initial-reserved-words '(begin while if for try return break continue
                         function macro quote let local global const do
                         struct
                         abstract typealias bitstype type immutable  ;; to be deprecated
                         module baremodule using import export importall))

(define initial-reserved-word? (Set initial-reserved-words))

(define reserved-words (append initial-reserved-words '(end else catch finally true false))) ;; todo: make this more complete

(define reserved-word? (Set reserved-words))

(define closing-token?
  (let ((closer? (Set '(else elseif catch finally #\, #\) #\] #\} #\;))))
    (lambda (tok)
      (or (and (eq? tok 'end) (not end-symbol))
          (closer? tok)
          (eof-object? tok)))))

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
; enable parsing `where` with high precedence
(define where-enabled #t)

(define current-filename 'none)

(define-macro (with-normal-ops . body)
  `(with-bindings ((range-colon-enabled #t)
                   (space-sensitive #f)
                   (where-enabled #t))
                  ,@body))

(define-macro (with-normal-context . body)
  `(with-bindings ((range-colon-enabled #t)
                   (space-sensitive #f)
                   (where-enabled #t)
                   (inside-vec #f)
                   (end-symbol #f)
                   (whitespace-newline #f))
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
            (error (string "invalid operator \"" str "\"")))
        (string->symbol str))))

(define (accum-digits c pred port _-digit-sep)
  (let loop ((str '())
             (c c))
    (if (and _-digit-sep (eqv? c #\_))
        (begin (read-char port)
               (let ((c (peek-char port)))
                 (if (and (not (eof-object? c)) (pred c))
                     (loop str c)
                     (begin
                       (io.ungetc port #\_)
                       (list->string (reverse str))))))
        (if (and (not (eof-object? c)) (pred c))
            (begin (read-char port)
                   (loop (cons c str) (peek-char port)))
            (list->string (reverse str))))))

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

(define (numchk n s)
  (or n (error (string "invalid numeric constant \"" s "\""))))

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
    (define (read-digs lz _-digit-sep)
      (let ((c (peek-char port)))
        (if (and (not lz) _-digit-sep (eqv? c #\_))
            (error (string "invalid numeric constant \""
                           (get-output-string str) c "\"")))
        (let ((d (accum-digits c pred port _-digit-sep)))
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
    (read-digs leadingzero #t)
    (if (eqv? (peek-char port) #\.)
        (begin (read-char port)
               (if (dot-opchar? (peek-char port))
                   (begin
                     (if (not (eqv? (peek-char port) #\.))
                         (let ((num (get-output-string str)))
                           (syntax-deprecation port
                                               (string num #\. (peek-char port))
                                               (string num " ." (peek-char port)))))
                     (io.ungetc port #\.))
                   (begin (write-char #\. str)
                          (read-digs #f #t)
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
                              (read-digs #t #f)
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
      (if (and (eqv? #\. (string.char s (string.dec s (length s))))
               (let ((nxt (peek-char port)))
                 (and (not (eof-object? nxt))
                      (or (identifier-start-char? nxt)
                          (memv nxt '(#\( #\[ #\{ #\@ #\` #\~ #\"))))))
          (error (string "numeric constant \"" s "\" cannot be implicitly multiplied because it ends with \".\"")))
      ;; n is #f for integers > typemax(UInt64)
      (cond (is-hex-float-literal (numchk n s) (double n))
            ((eq? pred char-hex?) (fix-uint-neg neg (sized-uint-literal n s 4)))
            ((eq? pred char-oct?) (fix-uint-neg neg (sized-uint-oct-literal n s)))
            ((eq? pred char-bin?) (fix-uint-neg neg (sized-uint-literal n s 1)))
            (is-float32-literal   (numchk n s) (float n))
            (n (if (and (integer? n) (> n 9223372036854775807))
                   `(macrocall @int128_str (null) ,s)
                   n))
            ((within-int128? s) `(macrocall @int128_str (null) ,s))
            (else `(macrocall @big_str (null) ,s))))))

(define (fix-uint-neg neg n)
  (if neg
      (if (large-number? n)
          `(call - ,(maybe-negate '- n))
          `(call - ,n))
      n))

(define (sized-uint-literal n s b)
  (let* ((i (if (eqv? (string.char s 0) #\-) 3 2))
         (l (* (- (length s) i) b)))
    (cond ((<= l 8)   (numchk n s) (uint8  n))
          ((<= l 16)  (numchk n s) (uint16 n))
          ((<= l 32)  (numchk n s) (uint32 n))
          ((<= l 64)  (numchk n s) (uint64 n))
          ((<= l 128) `(macrocall @uint128_str (null) ,s))
          (else       (error "Hex or binary literal too large for UInt128")))))

(define (sized-uint-oct-literal n s)
  (if (string.find s "o0")
      (sized-uint-literal n s 3)
      (if n
          (cond ((< n 256)        (uint8  n))
                ((< n 65536)      (uint16 n))
                ((< n 4294967296) (uint32 n))
                (else             (uint64 n)))
          (begin (if (equal? s "0o") (numchk n s))
                 (if (oct-within-uint128? s)
                     `(macrocall @uint128_str (null) ,s)
                     (error "Octal literal too large for UInt128"))))))

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

;; skip to end of comment, starting at #:  either #...<eol> or #= .... =#.
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
    (cond ((or (eof-object? c) (eqv? c #\newline))  (read-char port))

          ((identifier-start-char? c)     (accum-julia-symbol c port))

          ((string.find "()[]{},;\"`@" c) (read-char port))

          ((string.find "0123456789" c)   (read-number port #f #f))

          ((eqv? c #\#)                   (skip-comment port) (next-token port s))

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

          (else
           (read-char port)
           (if (default-ignorable-char? c)
               (error (string "invisible character \\u" (number->string (fixnum c) 16)))
               (error (string "invalid character \"" c "\"")))))))

;; --- token stream ---

(define (make-token-stream s) (vector #f s #t #f #f))
(define-macro (ts:port s)       `(aref ,s 1))
(define-macro (ts:last-tok s)   `(aref ,s 0))
(define-macro (ts:set-tok! s t) `(aset! ,s 0 ,t))
(define-macro (ts:pbtok s)      `(aref ,s 3))
(define (ts:space? s)           (aref s (if (ts:pbtok s) 4 2)))
(define (ts:put-back! s t spc)
  (if (ts:pbtok s)
      (error "too many pushed-back tokens (internal error)")
      (begin (aset! s 3 t)
             (aset! s 4 spc))))

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

(define (syntax-deprecation s what instead)
  (if (or *depwarn* *deperror*)
      (let ((msg (string
                  #\newline
                  (if *deperror* "ERROR:" "WARNING:") " deprecated syntax \"" what "\""
                  (if (or (not s) (eq? current-filename 'none))
                      ""
                      (string " at " current-filename ":" (input-port-line (if (port? s) s (ts:port s)))))
                  "."
                  (if (equal? instead "")
                      ""
                      (string #\newline "Use \"" instead "\" instead."))
                  #\newline)))
        (if *deperror*
            (error msg)
            (io.write *stderr* msg)))))

;; --- parser ---

;; parse left-to-right binary operator
;; produces structures like (+ (+ (+ 2 3) 4) 5)
(define-macro (parse-LtoR s down ops)
  `(let loop ((ex (,down ,s))
              (t  (peek-token ,s)))
     (if (,ops t)
         (begin (take-token ,s)
                (loop (list 'call t ex (,down ,s)) (peek-token ,s)))
         ex)))

;; parse right-to-left binary operator
;; produces structures like (= a (= b (= c d)))
(define-macro (parse-RtoL s down ops syntactic self)
  `(let* ((ex (,down ,s))
          (t  (peek-token ,s)))
     (if (,ops t)
         (begin (take-token ,s)
                (if ,syntactic
                    (list       t ex (,self ,s))
                    (list 'call t ex (,self ,s))))
         ex)))

(define (line-number-node s)
  `(line ,(input-port-line (ts:port s)) ,current-filename))

;; parse a@b@c@... as (@ a b c ...) for some operator @
;; ops: operators to look for
;; head: the expression head to yield in the result, e.g. "a;b" => (block a b)
;; closer?: predicate to identify tokens that stop parsing
;;          however, this doesn't consume the closing token, just looks at it
;; ow, my eyes!!
(define (parse-Nary s down ops head closer? add-linenums)
  (let ((t (require-token s)))
    (if (closer? t)
        (if add-linenums    ;; empty block
            (list head (line-number-node s))
            (list head))
        (let loop ((ex
                    ;; skip leading runs of operator
                    (if (memv t ops)
                        (if add-linenums
                            (list (line-number-node s))
                            '())
                        (if add-linenums
                            (let ((loc (line-number-node s)))
                              ;; note: line-number must happen before (down s)
                              (list (down s) loc))
                            (list (down s)))))
                   (first? #t)
                   (t (peek-token s)))
          (if (not (memv t ops))
              (begin
                (if (not (or (eof-object? t) (eqv? t #\newline) (closer? t)))
                    (error (string "extra token \"" t "\" after end of expression")))
                (if (or (null? ex) (pair? (cdr ex)) (not first?))
                    ;; () => (head)
                    ;; (ex2 ex1) => (head ex1 ex2)
                    ;; (ex1) if operator appeared => (head ex1) (handles "x;")
                    (cons head (reverse! ex))
                    ;; (ex1) => ex1
                    (car ex)))
              (begin (take-token s)
                     ;; allow input to end with the operator, as in a;b;
                     (if (or (eof-object? (peek-token s))
                             (closer? (peek-token s))
                             (memv (peek-token s) ops))
                         (loop ex #f (peek-token s))
                         (if (and add-linenums
                                  (not (and (pair? (car ex))
                                            (eq? (caar ex) 'line))))
                             (let ((loc (line-number-node s)))
                               (loop (list* (down s) loc ex) #f (peek-token s)))
                             (loop (cons (down s) ex) #f (peek-token s))))))))))

;; the principal non-terminals follow, in increasing precedence order

(define (parse-block s (down parse-eq))
  (parse-Nary s down '(#\newline #\;) 'block
              (lambda (x) (memq x '(end else elseif catch finally))) #t))

;; ";" at the top level produces a sequence of top level expressions
(define (parse-stmts s)
  (let ((ex (parse-Nary s (lambda (s) (parse-docstring s parse-eq))
                        '(#\;) 'toplevel (lambda (x) (eqv? x #\newline)) #f)))
    ;; check for unparsed junk after an expression
    (let ((t (peek-token s)))
      (if (not (or (eof-object? t) (eqv? t #\newline) (eq? t #f)))
          (error (string "extra token \"" t "\" after end of expression"))))
    ex))

(define (parse-eq s) (parse-assignment s parse-comma))

;; symbol tokens that do not simply parse to themselves when appearing alone as
;; an element of an argument list
(define non-standalone-symbol-token?
  (Set (append operators reserved-words '(.... mutable primitive))))

; parse-eq* is used where commas are special, for example in an argument list
(define (parse-eq* s)
  (let* ((t   (peek-token s))
         (spc (ts:space? s)))
    ;; optimization: skip checking the whole precedence stack if we have a simple
    ;; token followed by a common closing token
    (if (or (number? t) (and (symbol? t) (not (non-standalone-symbol-token? t))))
        (begin (take-token s)
               (let ((nxt (peek-token s)))
                 (if (or (eqv? nxt #\,) (eqv? nxt #\) ) (eqv? nxt #\}) (eqv? nxt #\]))
                     t
                     (begin (ts:put-back! s t spc)
                            (parse-assignment s parse-cond)))))
        (parse-assignment s parse-cond))))

(define (eventually-call ex)
  (and (pair? ex)
       (or (eq? (car ex) 'call)
           (and (or (eq? (car ex) 'where) (eq? (car ex) '|::|))
                (eventually-call (cadr ex))))))

(define (short-form-function-loc ex lno)
  (if (eventually-call (cadr ex))
      `(= ,(cadr ex) (block (line ,lno ,current-filename) ,(caddr ex)))
      ex))

(define (parse-assignment s down)
  (let loop ((ex (down s))
             (t  (peek-token s)))
    (if (not (is-prec-assignment? t))
        ex
        (begin
          (take-token s)
          (cond ((eq? t '~)
                 (if (and space-sensitive (ts:space? s)
                          (not (eqv? (peek-char (ts:port s)) #\ )))
                     (begin (ts:put-back! s t (ts:space? s))
                            ex)
                     (list 'call t ex (parse-assignment s down))))
                ((eq? t '=>)  ;; ~ and => are the only non-syntactic assignment-precedence operators
                 (list 'call t ex (parse-assignment s down)))
                ((eq? t '=)
                 ;; insert line/file for short-form function defs, otherwise leave alone
                 (let ((lno (input-port-line (ts:port s))))
                   (short-form-function-loc
                    (list t ex (parse-assignment s down)) lno)))
                (else
                 (list t ex (parse-assignment s down))))))))

; parse-comma is needed for commas outside parens, for example a = b,c
(define (parse-comma s)
  (let loop ((ex     (list (parse-cond s)))
             (first? #t)
             (t      (peek-token s)))
    (if (not (eqv? t #\,))
        (if (or (pair? (cdr ex)) (not first?))
            ;; () => (tuple)
            ;; (ex2 ex1) => (tuple ex1 ex2)
            ;; (ex1,) => (tuple ex1)
            (cons 'tuple (reverse! ex))
            ;; (ex1) => ex1
            (car ex))
        (begin (take-token s)
               (if (or (eof-object? (peek-token s)) (eq? (peek-token s) '=))
                   (loop ex #f (peek-token s))
                   (loop (cons (parse-cond s) ex) #f (peek-token s)))))))

(define (parse-cond s)
  (let ((ex (parse-arrow s)))
    (cond ((eq? (peek-token s) '?)
           (begin (if (not (ts:space? s))
                      (syntax-deprecation s (string (deparse ex) "?") (string (deparse ex) " ?")))
                  (take-token s) ; take the ?
                  (let ((t (with-whitespace-newline (without-range-colon (peek-token s)))))
                    (if (not (ts:space? s))
                        (syntax-deprecation s (string (deparse ex) " ?" (deparse t)) (string (deparse ex) " ? " (deparse t)))))
                  (let ((then (without-range-colon (parse-eq* s))))
                    (if (not (eq? (peek-token s) ':))
                        (error "colon expected in \"?\" expression"))
                    (if (not (ts:space? s))
                        (syntax-deprecation s (string (deparse ex) " ? " (deparse then) ":") (string (deparse ex) " ? " (deparse then) " :")))
                    (take-token s) ; take the :
                    (let ((t (with-whitespace-newline (peek-token s))))
                      (if (not (ts:space? s))
                          (syntax-deprecation s (string (deparse ex) " ? " (deparse then) " :" t) (string (deparse ex) " ? " (deparse then) " : " t))))
                    (list 'if ex then (parse-eq* s)))))
          (else ex))))

(define (parse-arrow s) (parse-RtoL s parse-or         is-prec-arrow? (eq? t '-->) parse-arrow))
(define (parse-or s)    (parse-RtoL s parse-and        is-prec-lazy-or? #t parse-or))
(define (parse-and s)   (parse-RtoL s parse-comparison is-prec-lazy-and? #t parse-and))

(define (parse-comparison s)
  (let loop ((ex (parse-pipes s))
             (first #t))
    (let ((t (peek-token s)))
      (cond ((is-prec-comparison? t)
             (begin (take-token s)
                    (if first
                        (loop (list 'comparison ex t (parse-pipes s)) #f)
                        (loop (append ex (list t (parse-pipes s))) #f))))
            (first ex)
            ((length= ex 4)
             ;; only a single comparison; special chained syntax not required
             (let ((op   (caddr ex))
                   (arg1 (cadr ex))
                   (arg2 (cadddr ex)))
               (if (or (eq? op '|<:|) (eq? op '|>:|))
                   `(,op ,arg1 ,arg2)
                   `(call ,op ,arg1 ,arg2))))
            (else ex)))))

(define (parse-pipes s) (parse-LtoR s parse-range is-prec-pipe?))

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
                 (begin (ts:put-back! s ': spc)
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

;; parse left to right chains of a certain binary operator
;; returns a list of arguments
(define (parse-chain s down op)
  (let loop ((chain (list (down s))))
    (let ((t (peek-token s)))
      (if (not (eq? t op))
          (reverse! chain)
          (let ((spc (ts:space? s)))
            (take-token s)
            (cond ((and space-sensitive spc (memq t unary-and-binary-ops)
                        (not (eqv? (peek-char (ts:port s)) #\ )))
                   ;; here we have "x -y"
                   (ts:put-back! s t spc)
                   (reverse! chain))
                  (else
                   (loop (cons (down s) chain)))))))))

;; parse left to right, combining chains of a certain operator into 1 call
;; e.g. a+b+c => (call + a b c)
(define (parse-with-chains s down ops chain-ops)
  (let loop ((ex (down s)))
    (let ((t (peek-token s)))
      (if (not (ops t))
          ex
          (let ((spc (ts:space? s)))
            (take-token s)
            (cond ((and space-sensitive spc (memq t unary-and-binary-ops)
                        (not (eqv? (peek-char (ts:port s)) #\ )))
                   ;; here we have "x -y"
                   (ts:put-back! s t spc)
                   ex)
                  ((memq t chain-ops)
                   (loop (list* 'call t ex
                                (parse-chain s down t))))
                  (else
                   (loop (list 'call t ex (down s))))))))))

(define (parse-with-chains-warn s down ops chain-ops)
  (let loop ((ex (down s))
             (got #f))
    (let ((t (peek-token s)))
      (if (not (ops t))
          (cons ex got)
          (let ((spc (ts:space? s)))
            (take-token s)
            (cond ((and space-sensitive spc (memq t unary-and-binary-ops)
                        (not (eqv? (peek-char (ts:port s)) #\ )))
                   ;; here we have "x -y"
                   (ts:put-back! s t spc)
                   (cons ex got))
                  ((memq t chain-ops)
                   (loop (list* 'call t ex
                                (parse-chain s down t))
                         #t))
                  (else
                   (loop (list 'call t ex (down s))
                         got))))))))

(define (parse-expr s)     (parse-with-chains s parse-shift         is-prec-plus? '(+ ++)))

(define (bitshift-warn s)
  (syntax-deprecation s (string "call to `*` inside call to bitshift operator")
                      "parenthesized call to `*`"))

(define (parse-shift s)    #;(parse-LtoR        s parse-term          is-prec-bitshift?)
  (let loop ((ex (parse-term s))
             (t  (peek-token s))
             (warn1 #f))
    (let ((ex (car ex))
          (warn (cdr ex)))
      (if (is-prec-bitshift? t)
          (begin (if warn (bitshift-warn s))
                 (take-token s)
                 (let ((nxt (parse-term s)))
                   (loop (cons (list 'call t ex (car nxt)) (cdr nxt)) (peek-token s) (cdr nxt))))
          (begin (if warn1 (bitshift-warn s))
                 ex)))))

(define (parse-term s)     (parse-with-chains-warn s parse-rational      is-prec-times? '(*)))
(define (parse-rational s) (parse-LtoR        s parse-unary-subtype is-prec-rational?))

;; parse `<: A where B` as `<: (A where B)` (issue #21545)
(define (parse-unary-subtype s)
  (let* ((op  (require-token s))
         (spc (ts:space? s)))
    (if (or (eq? op '|<:|) (eq? op '|>:|))
        (begin (take-token s)
               (let ((next (peek-token s)))
                 (cond ((or (closing-token? next) (newline? next) (eq? next '=))
                        op)  ; return operator by itself, as in (<:)
                       ;; parse <:{T}(x::T) or <:(x::T) like other unary operators
                       ((or (eqv? next #\{) (eqv? next #\( ))
                        (ts:put-back! s op spc)
                        (parse-where s parse-unary))
                       (else
                        (let ((arg (parse-where s parse-unary)))
                          (if (and (pair? arg) (eq? (car arg) 'tuple))
                              (cons op (cdr arg))
                              (list op arg)))))))
        (parse-where s parse-unary))))

(define (parse-where-chain s first)
  (with-bindings ((where-enabled #f))
   (let loop ((ex first)
              (t 'where))
     (if (eq? t 'where)
         (begin (take-token s)
                (let ((var (parse-comparison s)))
                  (loop (if (and (pair? var) (eq? (car var) 'cell1d))
                            (list* 'where ex (cdr var))  ;; form `x where {T,S}`
                            (list 'where ex var))
                        (peek-token s))))
         ex))))

(define (parse-where s down)
  ;; `where` needs to be below unary for `+(x::T,y::T) where {T} = ...` to work
  (let ((ex (down s)))
    (if (and where-enabled
             (eq? (peek-token s) 'where))
        (parse-where-chain s ex)
        ex)))

(define (maybe-negate op num)
  (if (eq? op '-)
      (if (large-number? num)
          (if (eqv? (cadddr num) "-170141183460469231731687303715884105728")
              `(macrocall @big_str (null) "170141183460469231731687303715884105728")
              `(,(car num) ,(cadr num) ,(caddr num) ,(string.tail (cadddr num) 1)))
          (if (= num -9223372036854775808)
              `(macrocall @int128_str (null) "9223372036854775808")
              (- num)))
      num))

;; operators handled by parse-unary at the start of an expression
(define initial-operator?
  ;; TODO: ? should probably not be listed here except for the syntax hack in osutils.jl
  (Set (diff operators (append '(: |'| ?) syntactic-unary-operators syntactic-operators))))

(define (parse-unary s)
  (let* ((op  (require-token s))
         (spc (ts:space? s)))
    (if (initial-operator? op)
        (begin
          (take-token s)
          (if (or (eq? op '-) (eq? op '+))
              (let ((nch (peek-char (ts:port s))))
                (if (or (and (char? nch) (char-numeric? nch))
                        (and (eqv? nch #\.) (read-char (ts:port s))))
                    (let ((num (parse-juxtapose
                                (read-number (ts:port s) (eqv? nch #\.) (eq? op '-))
                                s)))
                      (if (is-prec-power? (peek-token s))
                          ;; -2^x parsed as (- (^ 2 x))
                          (begin (ts:put-back! s (maybe-negate op num) spc)
                                 (list 'call op (parse-factor s)))
                          num))
                    (parse-unary-call s op #t spc)))
              (parse-unary-call s op (unary-op? op) spc)))
        (parse-juxtapose (parse-factor s) s))))

(define (parse-unary-call s op un spc)
  (let ((next (peek-token s)))
    (cond ((or (closing-token? next) (newline? next) (eq? next '=))
           op)  ; return operator by itself, as in (+)
          ((or (eqv? next #\{)  ;; this case is +{T}(x::T) = ...
               (and (not un) (eqv? next #\( )))
           (ts:put-back! s op spc)
           (parse-factor s))
          ((not un)
           (error (string "\"" op "\" is not a unary operator")))
          (else
           (let* ((arg  (parse-unary s))
                  (args (if (and (pair? arg) (eq? (car arg) 'tuple))
                            (cons op (cdr arg))
                            (list op arg))))
             (if (or (eq? op '|<:|) (eq? op '|>:|))
                 args
                 (cons 'call args)))))))

;; given an expression and the next token, is there a juxtaposition
;; operator between them?
(define (juxtapose? s expr t)
  (and (or (number? expr)
           (large-number? expr)
           (and (not (number? t))    ;; disallow "x.3" and "sqrt(2)2"
                (not (eqv? t #\@)))  ;; disallow "x@time"
           ;; to allow x'y as a special case
           #;(and (pair? expr) (memq (car expr) '(|'| |.'|))
                (not (memv t '(#\( #\[ #\{))))
           )
       (not (ts:space? s))
       (not (operator? t))
       (not (closing-token? t))
       (not (newline? t))
       (or (not (string? expr))  ;; issue #20575
           (error "cannot juxtapose string literal"))
       (not (initial-reserved-word? t))
       (not (and (pair? expr) (syntactic-unary-op? (car expr))))
       ;; TODO: this would disallow juxtaposition with 0, which is ambiguous
       ;; with e.g. hex literals `0x...`. however this is used for `0im`, which
       ;; we might not want to break.
       #;(or (not (and (eq? expr 0)
                     (symbol? t)))
           (error (string "invalid numeric constant \"" expr t "\"")))))

(define (parse-juxtapose ex s)
  (let ((next (peek-token s)))
    ;; numeric literal juxtaposition is a unary operator
    (cond ((juxtapose? s ex next)
           (begin
             #;(if (and (number? ex) (= ex 0))
                 (error "juxtaposition with literal \"0\""))
             `(call * ,ex ,(parse-unary s))))
          (else ex))))

;; handle ^ and .^
;; -2^3 is parsed as -(2^3), so call parse-decl for the first argument,
;; and parse-unary from then on (to handle 2^-3)
(define (parse-factor s)       (parse-RtoL s parse-decl  is-prec-power? #f parse-factor-after))
(define (parse-factor-after s) (parse-RtoL s parse-unary is-prec-power? #f parse-factor-after))

(define (parse-decl s)
  (let loop ((ex (parse-call s)))
    (let ((t (peek-token s)))
      (case t
        ((|::|) (take-token s)
         (loop (list t ex (parse-where s parse-call))))
        ((->)   (take-token s)
         ;; -> is unusual: it binds tightly on the left and
         ;; loosely on the right.
         (let ((lno (line-number-node s)))
           `(-> ,ex (block ,lno ,(parse-eq* s)))))
        (else
         ex)))))

;; parse function call, indexing, dot, and transpose expressions
;; also handles looking for syntactic reserved words
(define (parse-call s)
  (let ((ex (parse-unary-prefix s)))
    (if (or (initial-reserved-word? ex) (eq? ex 'mutable) (eq? ex 'primitive))
        (parse-resword s ex)
        (parse-call-chain s ex #f))))

(define (parse-unary-prefix s)
  (let ((op (peek-token s)))
    (if (syntactic-unary-op? op)
        (begin (take-token s)
               (cond ((and (memq op '(& $))
                           (let ((next (peek-token s)))
                             (or (closing-token? next) (newline? next))))
                      op)
                     ((memq op '(& |::|))  (list op (parse-where s parse-call)))
                     (else                 (list op (parse-unary-prefix s)))))
        (parse-atom s))))

(define (parse-def s is-func)
  (let* ((ex (parse-unary-prefix s))
         (sig (if (or (and is-func (reserved-word? ex)) (initial-reserved-word? ex))
                  (error (string "invalid name \"" ex "\""))
                  (parse-call-chain s ex #f)))
         (decl-sig
          (if (and is-func (eq? (peek-token s) '|::|))
              (begin (take-token s)
                     `(|::| ,sig ,(parse-call s)))
              sig)))
    (if (eq? (peek-token s) 'where)
        (parse-where-chain s decl-sig)
        decl-sig)))

(define (disallowed-space ex t)
  (error (string "space before \"" t "\" not allowed in \""
                 (deparse ex) " " (deparse t) "\"")))

;; string macro suffix for given delimiter t
(define (macsuffix t)
  (case t
    ((#\") '_str)
    ((#\`) '_cmd)))

(define (parse-call-chain s ex macrocall?)
  (let loop ((ex ex))
    (let ((t (peek-token s)))
      (if (or (and space-sensitive (ts:space? s)
                   (memv t '(#\( #\[ #\{ |'| #\" #\`)))
              (and (or (number? ex)  ;; 2(...) is multiply, not call
                       (large-number? ex))
                   (eqv? t #\()))
          ex
          (case t
            ((#\( )
             (if (ts:space? s) (disallowed-space ex t))
             (take-token s)
             (let ((c (let ((al (parse-arglist s #\) )))
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
               (if macrocall?
                   (map (lambda (x)  ;; parse `a=b` as `=` instead of `kw` in macrocall
                          (if (and (pair? x) (eq? (car x) 'kw))
                              `(= ,@(cdr x))
                              x))
                        c)
                   (loop c))))
            ((#\[ )
             (if (ts:space? s) (disallowed-space ex t))
             (take-token s)
             ;; ref is syntax, so we can distinguish
             ;; a[i] = x  from
             ;; ref(a,i) = x
             (let* ((es end-symbol)
                    (al (with-end-symbol (parse-cat s #\] es))))
               (if (null? al)
                   (loop (list 'ref ex))
                   (case (car al)
                     ((vect)  (loop (list* 'ref ex (cdr al))))
                     ((hcat)  (loop (list* 'typed_hcat ex (cdr al))))
                     ((vcat)
                      (loop (list* 'typed_vcat ex (cdr al))))
                     ((comprehension)
                      (loop (list* 'typed_comprehension ex (cdr al))))
                     (else (error "unknown parse-cat result (internal error)"))))))
            ((|.|)
             (if (ts:space? s) (disallowed-space ex t))
             (take-token s)
             (loop
              (cond ((eqv? (peek-token s) #\()
                     (begin
                       (take-token s)
                       `(|.| ,ex (tuple ,@(parse-arglist s #\) )))))
                    ((eqv? (peek-token s) ':)
                     (begin
                       (take-token s)
                       `(|.| ,ex (quote ,(parse-atom s)))))
                    ((eq? (peek-token s) '$)
                     (take-token s)
                     (let ((dollarex (parse-atom s)))
                       `(|.| ,ex (inert ($ ,dollarex)))))
                    (else
                     (let ((name (parse-atom s)))
                       (if (and (pair? name) (eq? (car name) 'macrocall))
                           `(macrocall (|.| ,ex (quote ,(cadr name))) ; move macrocall outside by rewriting A.@B as @A.B
                                       ,@(cddr name))
                           `(|.| ,ex (quote ,name))))))))
            ((|.'| |'|)
             (if (ts:space? s)
                 (error (string "space not allowed before \"" t "\"")))
             (take-token s)
             (loop (list t ex)))
            ((#\{ )
             (if (ts:space? s) (disallowed-space ex t))
             (take-token s)
             (loop (list* 'curly ex (parse-arglist s #\} ))))
            ((#\" #\`)
             (if (and (or (symbol? ex) (valid-modref? ex))
                      (not (operator? ex))
                      (not (ts:space? s)))
                 ;; custom string and command literals; x"s" => @x_str "s"
                 (let* ((startloc  (line-number-node s))
                        (macstr (begin (take-token s)
                                       (parse-raw-literal s t)))
                        (nxt (peek-token s))
                        (macname (macroify-name ex (macsuffix t))))
                   (if (and (symbol? nxt) (not (operator? nxt))
                            (not (ts:space? s)))
                       ;; string literal suffix, "s"x
                       (loop `(macrocall ,macname ,startloc ,macstr
                                         ,(string (take-token s))))
                       (loop `(macrocall ,macname ,startloc ,macstr))))
                 ex))
            (else ex))))))

(define expect-end-current-line 0)

(define (expect-end s word)
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
  (parse-comparison s))

(define (valid-func-sig? paren sig)
  (and (pair? sig)
       (or (eq? (car sig) 'call)
           (eq? (car sig) 'tuple)
           (and paren (eq? (car sig) 'block))
           (and (eq? (car sig) '|::|)
                (pair? (cadr sig))
                (eq? (car (cadr sig)) 'call))
           (and (eq? (car sig) 'where)
                (valid-func-sig? paren (cadr sig))))))

(define (unwrap-where x)
  (if (and (pair? x) (eq? (car x) 'where))
      (unwrap-where (cadr x))
      x))

(define (rewrap-where x w)
  (if (and (pair? w) (eq? (car w) 'where))
      (list 'where (rewrap-where x (cadr w)) (caddr w))
      x))

(define (parse-struct-def s mut? word)
  (if (reserved-word? (peek-token s))
      (error (string "invalid type name \"" (take-token s) "\"")))
  (let ((sig (parse-subtype-spec s)))
    (begin0 (list 'type (if mut? 'true 'false) sig (parse-block s))
            (expect-end s word))))

;; consume any number of line endings from a token stream
(define (take-lineendings s)
  (let ((nt (peek-token s)))
    (if (or (newline? nt) (eqv? nt #\;))
        (begin (take-token s)
               (take-lineendings s))
        s)))

;; parse expressions or blocks introduced by syntactic reserved words
(define (parse-resword s word)
  (with-bindings
   ((expect-end-current-line (input-port-line (ts:port s))))
   (with-normal-context
    (begin
     (case word
       ((begin quote)
        (let ((loc  (begin (skip-ws-and-comments (ts:port s))
                           (line-number-node s)))
              (blk  (parse-block s (lambda (s) (parse-docstring s parse-eq)))))
          (expect-end s word)
          (let ((blk  (if (and (length> blk 1)
                               (pair? (cadr blk)) (eq? (caadr blk) 'line))
                          (list* 'block loc (cddr blk))
                          blk)))
            (if (eq? word 'quote)
                (list 'quote blk)
                blk))))
       ((while)  (begin0 (list 'while (parse-cond s) (parse-block s))
                         (expect-end s word)))
       ((for)
        (let* ((ranges (parse-comma-separated-iters s))
               (body   (parse-block s)))
          (expect-end s word)
          `(for ,(if (length= ranges 1) (car ranges) (cons 'block ranges))
                ,body)))

       ((if)
        (if (newline? (peek-token s))
            (error (string "missing condition in \"if\" at " current-filename
                           ":" (- (input-port-line (ts:port s)) 1))))
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
                     (expect-end s word)))
            (else      (error (string "unexpected \"" nxt "\""))))))
       ((let)
        (let ((binds (if (memv (peek-token s) '(#\newline #\;))
                         '()
                         (parse-comma-separated-assignments s))))
          (if (not (or (eof-object? (peek-token s))
                       (memv (peek-token s) '(#\newline #\; end))))
              (error "let variables should end in \";\" or newline"))
          (let ((ex (parse-block s)))
            (expect-end s word)
            ;; don't need line info in an empty let block
            (if (and (length= ex 2) (pair? (cadr ex)) (eq? (caadr ex) 'line))
                `(let (block) ,@binds)
                `(let ,ex ,@binds)))))

       ((global local)
        (let* ((const (and (eq? (peek-token s) 'const)
                           (take-token s)))
               (assgn (parse-eq s))
               (expr  (if (and (pair? assgn) (eq? (car assgn) 'tuple))
                          (cons word (cdr assgn))
                          (list word assgn))))
          (if const
              `(const ,expr)
              expr)))
       ((const)
        (let ((assgn (parse-eq s)))
          (if (not (and (pair? assgn)
                        (or (eq? (car assgn) '=)
                            (eq? (car assgn) 'global)
                            (eq? (car assgn) 'local))))
              (error "expected assignment after \"const\"")
              `(const ,assgn))))

       ((function macro)
        (let* ((paren (eqv? (require-token s) #\())
               (sig   (parse-def s (not (eq? word 'macro)))))
          (if (and (eq? word 'function) (not paren) (symbol-or-interpolate? sig))
              (begin (if (not (eq? (require-token s) 'end))
                         (error (string "expected \"end\" in definition of function \"" sig "\"")))
                     (take-token s)
                     `(function ,sig))
              (let* ((usig (unwrap-where sig))
                     (def  (if (or (symbol? usig)
                                   (and (pair? usig) (eq? (car usig) '|::|)
                                        (symbol? (cadr usig))))
                               (if paren
                                   ;; in "function (x)" the (x) is a tuple
                                   (rewrap-where `(tuple ,usig) sig)
                                   ;; function foo  =>  syntax error
                                   (error (string "expected \"(\" in " word " definition")))
                               (if (not (valid-func-sig? paren sig))
                                   (error (string "expected \"(\" in " word " definition"))
                                   sig)))
                     (body (parse-block s)))
                (expect-end s word)
                (list word def body)))))

       ((abstract)
        (let ((ty (eq? (peek-token s) 'type)))
          (if ty
              (take-token s))
          (let ((spec (parse-subtype-spec s)))
            (if (not ty)
                (syntax-deprecation s (string "abstract " (deparse spec))
                                    (string "abstract type " (deparse spec) " end")))
            (begin0 (list 'abstract spec)
                    (if ty (expect-end (take-lineendings s) "abstract type"))))))
       ((struct)
        (begin (take-token s)
               (parse-struct-def s #f word)))
       ((mutable)
        (if (not (eq? (peek-token s) 'struct))
            (parse-call-chain s word #f)
            (begin (take-token s)
                   (parse-struct-def s #t word))))
       ((primitive)
        (if (not (eq? (peek-token s) 'type))
            (parse-call-chain s word #f)
            (begin (take-token s)
                   (let* ((spec (with-space-sensitive (parse-subtype-spec s)))
                          (nb   (with-space-sensitive (parse-cond s))))
                     (begin0 (list 'bitstype nb spec)
                             (expect-end (take-lineendings s) "primitive type"))))))
       ;; deprecated type keywords
       ((type)
        (syntax-deprecation s "type" "mutable struct")  ;; retain in 0.7
        (parse-struct-def s #t word))
       ((immutable)
        (syntax-deprecation s "immutable" "struct")  ;; retain in 0.7
        (parse-struct-def s #f word))
       ((bitstype)
        (let* ((nb   (with-space-sensitive (parse-cond s)))
               (spec (parse-subtype-spec s)))
          (syntax-deprecation s (string "bitstype " (deparse nb) " " (deparse spec))
                              (string "primitive type " (deparse spec) " " (deparse nb) " end"))
          (list 'bitstype nb spec)))
       ((typealias)
        (let ((lhs (with-space-sensitive (parse-call s)))
              (rhs (parse-where s parse-call)))
          (syntax-deprecation s (string "typealias " (deparse lhs) " " (deparse rhs))
                              (string (if (symbol? lhs) "const " "")
                                      (deparse lhs) " = " (deparse rhs)))
          (list 'typealias lhs rhs)))

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
              (list* 'try try-block (or catchv 'false)
                     ;; default to empty catch block in `try ... end`
                     (or catchb (if finalb 'false '(block)))
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
                    (let* ((loc (line-number-node s))
                           (var (if nl #f (parse-eq* s)))
                           (var? (and (not nl) (or (and (symbol? var) (not (eq? var 'false))
                                                        (not (eq? var 'true)))
                                                   (and (length= var 2) (eq? (car var) '$)))))
                           (catch-block (if (eq? (require-token s) 'finally)
                                            `(block ,(line-number-node s))
                                            (parse-block s))))
                      (loop (require-token s)
                            (if (or var? (not var))
                                catch-block
                                `(block ,loc ,var
                                        ,@(if (and (length= catch-block 2)
                                                   (pair? (cadr catch-block))
                                                   (eq? (caadr catch-block) 'line))
                                              '()
                                              (cdr catch-block))))
                            (if var? var 'false)
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
       ((break continue)
        (let ((t (peek-token s)))
          (if (or (eof-object? t)
                  (and (eq? t 'end) (not end-symbol))
                  (memv t '(#\newline #\; #\) :)))
              (list word)
              (error (string "unexpected \"" t "\" after " word)))))

       ((module baremodule)
        (let* ((name (parse-unary-prefix s))
               (loc  (line-number-node s))
               (body (parse-block s (lambda (s) (parse-docstring s parse-eq)))))
          (expect-end s word)
          (list 'module (if (eq? word 'module) 'true 'false) name
                `(block ,loc ,@(cdr body)))))
       ((export)
        (let ((es (map macrocall-to-atsym
                       (parse-comma-separated s parse-unary-prefix))))
          (if (not (every symbol-or-interpolate? es))
              (error "invalid \"export\" statement"))
          `(export ,@es)))
       ((import using importall)
        (let ((imports (parse-imports s word)))
          (if (length= imports 1)
              (car imports)
              (cons 'toplevel imports))))
       ((do)
        (error "invalid \"do\" syntax"))
       (else (error "unhandled reserved word")))))))

(define (parse-do s)
  (with-bindings
   ((expect-end-current-line (input-port-line (ts:port s))))
   (without-whitespace-newline
    (let ((doargs (if (memv (peek-token s) '(#\newline #\;))
                      '()
                      (parse-comma-separated s parse-range))))
      `(-> (tuple ,@doargs)
           ,(begin0 (parse-block s)
                    (expect-end s 'do)))))))

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
                      ((or (eq? next '|.|)
                           (eqv? (string.sub (string next) 0 1) ".")) #f)
                      (else #t)))
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
             (t (require-token s)))  ;; skip newlines
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
           (cons (macrocall-to-atsym (parse-unary-prefix s)) l)))))

(define (parse-import s word)
  (let loop ((path (parse-import-dots s)))
    (if (not (symbol-or-interpolate? (car path)))
        (error (string "invalid \"" word "\" statement: expected identifier")))
    (let ((nxt (peek-token s)))
      (cond
       ((eq? nxt '|.|)
        (if (ts:space? s) (disallowed-space word nxt))
        (take-token s)
        (loop (cons (macrocall-to-atsym (parse-unary-prefix s)) path)))
       ((or (memv nxt '(#\newline #\; #\, :))
            (eof-object? nxt))
        `(,word ,@(reverse path)))
       ((eqv? (string.sub (string nxt) 0 1) ".")
        (take-token s)
        (loop (cons (symbol (string.sub (string nxt) 1))
                    path)))
       (else
        `(,word ,@(reverse path)))))))

;; parse comma-separated assignments, like "i=1:n,j=1:m,..."
(define (parse-comma-separated s what)
  (let loop ((exprs '()))
    (let ((r (what s)))
      (case (peek-token s)
        ((#\,)
         (take-token s)
         (loop (cons r exprs)))
        (else   (reverse! (cons r exprs)))))))

(define (parse-comma-separated-assignments s)
  (parse-comma-separated s parse-eq*))

;; as above, but allows both "i=r" and "i in r"
(define (parse-iteration-spec s)
  (let* ((lhs (parse-pipes s))
         (t   (peek-token s)))
    (cond ((memq t '(= in ∈))
           (take-token s)
           (let* ((rhs (parse-pipes s))
                  (t   (peek-token s)))
             #;(if (not (or (closing-token? t) (newline? t)))
                 ;; should be: (error "invalid iteration specification")
                 (syntax-deprecation s (string "for " (deparse `(= ,lhs ,rhs)) " " t)
                                     (string "for " (deparse `(= ,lhs ,rhs)) "; " t)))
             `(= ,lhs ,rhs)))
          ((and (eq? lhs ':) (closing-token? t))
           ':)
          (else (error "invalid iteration specification")))))

(define (parse-comma-separated-iters s)
  (let loop ((ranges '()))
    (let ((r (parse-iteration-spec s)))
      (case (peek-token s)
        ((#\,)  (take-token s) (loop (cons r ranges)))
        (else   (reverse! (cons r ranges)))))))

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

;; handle function call argument list, or any comma-delimited list.
;; . an extra comma at the end is allowed
;; . expressions after a ; are enclosed in (parameters ...)
;; . an expression followed by ... becomes (... x)
(define (parse-arglist s closer)
(with-normal-ops
 (with-whitespace-newline
  (let loop ((lst '()))
    (let ((t (require-token s)))
      (if (eqv? t closer)
          (begin (take-token s)
                 (if (eqv? closer #\) )
                     ;; (= x y) inside function call is keyword argument
                     (to-kws (reverse! lst))
                     (reverse! lst)))
          (if (eqv? t #\;)
              (begin (take-token s)
                     (if (eqv? (peek-token s) closer)
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
                       (take-token s)
                       (loop (cons nxt lst)))
                      ((eqv? c #\;)     (loop (cons nxt lst)))
                      ((eqv? c closer)  (loop (cons nxt lst)))
                      ((eq? c 'for)
                       (expect-space-before s 'for)
                       (take-token s)
                       (loop (cons (parse-generator s nxt) lst)))
                      ;; newline character isn't detectable here
                      #;((eqv? c #\newline)
                      (error "unexpected line break in argument list"))
                      ((or (eqv? c #\]) (eqv? c #\}))
                       (error (string "unexpected \"" c "\" in argument list")))
                      (else
                       (error (string "missing comma or " closer
                                      " in argument list"))))))))))))

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

(define (parse-generator s first)
  (let ((iters (parse-comma-separated-iters s)))
    (let ((iters (if (eq? (peek-token s) 'if)
                     (begin (take-token s)
                            (list `(filter ,(parse-cond s) ,@iters)))
                     iters)))
      (if (eq? (peek-token s) 'for)
          (begin (expect-space-before s 'for)
                 (take-token s)
                 `(flatten (generator ,(parse-generator s first) ,@iters)))
          `(generator ,first ,@iters)))))

(define (parse-comprehension s first closer)
  (with-whitespace-newline
   (let ((gen (parse-generator s first)))
     (if (not (eqv? (require-token s) closer))
         (error (string "expected \"" closer "\""))
         (take-token s))
     `(comprehension ,gen))))

(define (parse-matrix s first closer gotnewline last-end-symbol)
  (define (fix head v) (cons head (reverse v)))
  (define (update-outer v outer)
    (cond ((null? v)       outer)
          ((null? (cdr v)) (cons (car v) outer))
          (else            (cons (fix 'row v) outer))))
  (define semicolon (eqv? (peek-token s) #\;))
  ;; if a [ ] expression is a cat expression, `end` is not special
  (with-bindings ((end-symbol last-end-symbol))
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
                 (begin (expect-space-before s 'for)
                        (take-token s)
                        (parse-comprehension s (car outer) closer))
                 (error "invalid comprehension syntax")))
            (else
             (if (and (pair? vec) (not (ts:space? s)))
                 (error (string "expected separator between arguments to \"[ ]\"; got \""
                                (deparse (car vec)) t "\"")))
             (loop (cons (parse-eq* s) vec) outer))))))))

(define (peek-non-newline-token s)
  (let loop ((t (peek-token s)))
    (if (newline? t)
        (begin (take-token s)
               (loop (peek-token s)))
        t)))

(define (expect-space-before s t)
  (if (not (ts:space? s))
      (error (string "expected space before \"" t "\""))))

(define (parse-cat s closer last-end-symbol)
  (with-normal-ops
   (with-inside-vec
    (if (eqv? (require-token s) closer)
        (begin (take-token s)
               '())
        (let* ((first (parse-eq* s))
               (t (peek-token s)))
          (cond ((or (eqv? t #\,) (eqv? t closer))
                 (parse-vect s first closer))
                ((eq? t 'for)
                 (expect-space-before s 'for)
                 (take-token s)
                 (parse-comprehension s first closer))
                ((eqv? t #\newline)
                 (take-token s)
                 (if (memv (peek-token s) (list #\, closer))
                     (parse-vect s first closer)
                     (parse-matrix s first closer #t last-end-symbol)))
                (else
                 (parse-matrix s first closer #f last-end-symbol))))))))

(define (kw-to-= e) (if (kwarg? e) (cons '= (cdr e)) e))
(define (=-to-kw e) (if (assignment? e) (cons 'kw (cdr e)) e))

;; translate nested (parameters ...) expressions to a statement block if possible
;; this allows us to first parse tuples using parse-arglist
(define (parameters-to-block e)
  (if (and (pair? e) (eq? (car e) 'parameters))
      (cond ((length= e 1) '())
            ((length= e 2) (parameters-to-block (cadr e)))
            ((length= e 3)
             (let ((fst (cadr e))
                   (snd (caddr e)))
               (if (and (pair? fst) (eq? (car fst) 'parameters))
                   (let ((rec (parameters-to-block fst))
                         (snd (parameters-to-block snd)))
                     (and rec snd
                          (cons (car snd) rec)))
                   #f)))
            (else #f))
      (list (kw-to-= e))))

;; convert an arglist to a tuple or block expr
;; leading-semi? means we saw (; ...)
;; comma? means there was a comma after the first expression
(define (arglist-to-tuple leading-semi? comma? args . first)
  (if (and (pair? first) (null? args) (not leading-semi?) (not comma?))
      `(block ,@first)  ;; this case is (x;)
      (or (and (not comma?) (length= args 1) (pair? (car args)) (eq? (caar args) 'parameters)
               (let ((blk (parameters-to-block (car args))))
                 (and blk (or (and (not leading-semi?)
                                   `(block ,@first ,@blk))
                              (and (null? first) (null? blk)
                                   `(block))))))  ;; all semicolons inside ()
          (and (null? first) (null? args) (not comma?)
               `(block))  ;; this case is (;)
          (if (and (pair? args) (pair? (car args)) (eq? (caar args) 'parameters))
              `(tuple ,(car args) ,@first ,@(map kw-to-= (cdr args)))
              `(tuple ,@first ,@(map kw-to-= args))))))

(define (not-eof-for delim c)
  (if (eof-object? c)
      ;; NOTE: changing this may affect code in base/client.jl
      (error (case delim
                   ((#\`) "incomplete: invalid \"`\" syntax")
                   ((#\") "incomplete: invalid string syntax")))
      c))

(define (take-char p)
  (begin (read-char p) p))

;; map the first element of lst
(define (map-first f lst)
  (if (null? lst) ()
    (cons (f (car lst)) (cdr lst))))

;; map the elements of lst where (pred index) is true
;; e.g., (map-at odd? (lambda (x) 0) '(a b c d)) -> '(a 0 c 0)
(define (map-at pred f lst)
  (define (map-at- pred f lst i r)
    (if (null? lst) (reverse r)
        (let* ((x (car lst))
               (y (if (pred i) (f x) x)))
          (map-at- pred f (cdr lst) (+ i 1) (cons y r)))))
  (map-at- pred f lst 0 ()))

(define (parse-raw-literal s delim)
  (car (parse-string-literal s delim #t)))

(define (parse-string-literal s delim raw)
  (let ((p (ts:port s)))
    (if (eqv? (peek-char p) delim)
        (if (eqv? (peek-char (take-char p)) delim)
            (map-first strip-leading-newline
                       (dedent-triplequoted-string
                        (parse-string-literal- 2 (take-char p) s delim raw)))
            (list ""))
        (parse-string-literal- 0 p s delim raw))))

(define (strip-leading-newline s)
  (let ((n (sizeof s)))
    (cond
     ((and (> n 0) (eqv? (string.char s 0) #\newline))
      (string.tail s 1))
     ((and (> n 1) (eqv? (string.char s 0) #\return)
           (eqv? (string.char s 1) #\newline))
      (string.tail s 2))
     (else s))))

(define (dedent-triplequoted-string lst)
  (let ((prefix (triplequoted-string-indentation lst)))
    (if (length> prefix 0)
        (map-at even?
                (lambda (s)
                  (string-replace s
                                  (list->string (cons #\newline prefix))
                                  #\newline))
                lst)
        lst)))

(define (triplequoted-string-indentation lst)
  (longest-common-prefix
   (apply append (map (lambda (s) (if (string? s)
                                      (triplequoted-string-indentation- s)
                                      ()))
                      lst))))

(define (triplequoted-string-indentation- s)
  (let ((p (open-input-string s)))
    (let loop ((c (read-char p))
               (state 0)
               (prefix ())
               (prefixes ()))
      (cond
       ((eqv? c #\newline)
        (loop (read-char p) 1 () prefixes))
       ((eqv? state 0)
        (if (eof-object? c) prefixes
            (loop (read-char p) 0 () prefixes)))
       ((memv c '(#\space #\tab))
        (loop (read-char p) 2 (cons c prefix) prefixes))
       (else
        (loop (read-char p) 0 () (cons (reverse prefix) prefixes)))))))

;; return the longest common prefix of the elements of l
;; e.g., (longest-common-prefix ((1 2) (1 4))) -> (1)
(define (longest-common-prefix l)
  (let ((len (length l)))
    (cond
     ((= len 0) ())
     ((= len 1) (car l))
     (else (longest-common-prefix
            (cons (longest-common-prefix2 (car l) (cadr l))
                  (cddr l)))))))

;; return the longest common prefix of lists a & b
(define (longest-common-prefix2 a b)
  (longest-common-prefix2- a b ()))

(define (longest-common-prefix2- a b p)
  (if (and (length> a 0)
           (length> b 0)
           (eqv? (car a) (car b)))
      (longest-common-prefix2- (cdr a) (cdr b) (cons (car a) p))
      (reverse p)))

(define (string-split s sep)
  (string-split- s sep 0 ()))

(define (string-split- s sep start splits)
  (let ((i (string.find s sep start)))
    (if i
        (string-split- s sep (+ i (sizeof sep)) (cons (string.sub s start i) splits))
        (reverse (cons (string.sub s start (sizeof s)) splits)))))

;; replace all occurrences of a in s with b
(define (string-replace s a b)
  (string.join (string-split s a) b))

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

(define (tostr raw io)
  (if raw
      (io.tostring! io)
      (let ((str (unescape-string (io.tostring! io))))
        (if (not (string.isutf8 str))
            (error "invalid UTF-8 sequence")
            str))))

;; raw = raw string literal
;; when raw is #t, unescape only \\ and delimiter
;; otherwise do full unescaping, and parse interpolations too
(define (parse-string-literal- n p s delim raw)
  (let loop ((c (read-char p))
             (b (open-output-string))
             (e ())
             (quotes 0))
    (cond
      ((eqv? c delim)
       (if (< quotes n)
           (loop (read-char p) b e (+ quotes 1))
           (reverse (cons (tostr raw b) e))))

      ((= quotes 1)
       (if (not raw) (write-char #\\ b))
       (write-char delim b)
       (loop c b e 0))

      ((= quotes 2)
       (if (not raw) (write-char #\\ b))
       (write-char delim b)
       (if (not raw) (write-char #\\ b))
       (write-char delim b)
       (loop c b e 0))

      ((eqv? c #\\)
       (let ((nxch (not-eof-for delim (read-char p))))
         (if (or (not raw)
                 (not (or (eqv? nxch delim) #;(eqv? nxch #\\))))
             (write-char #\\ b))
         (write-char nxch b)
         (loop (read-char p) b e 0)))

      ((and (eqv? c #\$) (not raw))
       (let ((ex (parse-interpolate s)))
         (loop (read-char p)
               (open-output-string)
               (list* ex (tostr raw b) e)
               0)))

      ; convert literal \r and \r\n in strings to \n (issue #11988)
      ((eqv? c #\return) ; \r
       (begin
         (if (eqv? (peek-char p) #\linefeed) ; \r\n
             (read-char p))
         (write-char #\newline b)
         (loop (read-char p) b e 0)))

      (else
       (write-char (not-eof-for delim c) b)
       (loop (read-char p) b e 0)))))

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

(define-macro (check-identifier ex)
  `(begin (if (or (syntactic-op? ,ex) (eq? ,ex '....))
              (error (string "invalid identifier name \"" ,ex "\"")))
          (if (eq? ,ex '?)
              (syntax-deprecation s "`?` used as an identifier" "")))) ; merge with above check in v1.0

;; parse numbers, identifiers, parenthesized expressions, lists, vectors, etc.
(define (parse-atom s (checked #t))
  (let ((t (require-token s)))
    (cond ;; char literal
          ((eq? t '|'|)
           (take-token s)
           (let ((firstch (read-char (ts:port s))))
               (if (and (not (eqv? firstch #\\))
                        (not (eof-object? firstch))
                        (eqv? (peek-char (ts:port s)) #\'))
                   ;; easy case: 1 character, no \
                   (begin (read-char (ts:port s)) firstch)
                   (let ((b (open-output-string)))
                     (let loop ((c firstch))
                       (if (eqv? c #\')
                           #t
                           (begin (if (eqv? c #\")
                                      (error "invalid character literal") ;; issue 14683
                                      #t)
                                  (write-char (not-eof-1 c) b)
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
                               (string.char str 0))))))))

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
                     (list 'quote (parse-atom s #f))))))

          ;; misplaced =
          ((eq? t '=) (error "unexpected \"=\""))

          ;; identifier
          ((symbol? t)
           (if checked
               (begin (check-identifier t)
                      (if (closing-token? t)
                          (error (string "unexpected \"" (take-token s) "\"")))))
           (take-token s))

          ;; parens or tuple
          ((eqv? t #\( )
           (take-token s)
           (with-bindings ((range-colon-enabled #t)
                           (space-sensitive #f)
                           (where-enabled #t)
                           (whitespace-newline #t))
            (let ((nxt (require-token s)))
             (cond
              ((eqv? nxt #\) )
               ;; empty tuple ()
               (begin (take-token s) '(tuple)))
              ((syntactic-op? nxt)
               ;; allow (=) etc.
               (let ((tok (take-token s)))
                 (if (not (eqv? (require-token s) #\) ))
                     (error (string "invalid identifier name \"" tok "\""))
                     (take-token s))
                 (if checked (check-identifier tok))
                 tok))
              ;; allow :(::) as a special case
              ((and (not checked) (eq? nxt '|::|)
                    (let ((spc (ts:space? s)))
                      (or (and (take-token s) (eqv? (require-token s) #\) ))
                          (and (ts:put-back! s '|::| spc) #f))))
               (take-token s)  ;; take #\)
               '|::|)
              ((eqv? nxt #\;)
               (arglist-to-tuple #t #f (parse-arglist s #\) )))
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
                       ((eq? t 'for)
                        (expect-space-before s 'for)
                        (take-token s)
                        (let ((gen (parse-generator s ex)))
                          (if (eqv? (require-token s) #\) )
                              (take-token s)
                              (error "expected \")\""))
                          gen))
                       (else
                        ;; tuple (x,) (x,y) (x...) etc.
                        (if (eqv? t #\, )
                            (take-token s)
                            (if (not (eqv? t #\;))
                                (error "missing comma or ) in argument list")))
                        (arglist-to-tuple #f (eqv? t #\,) (parse-arglist s #\) ) ex)))))))))

          ;; cat expression
          ((eqv? t #\[ )
           (take-token s)
           (let ((vex (parse-cat s #\] end-symbol)))
             (if (null? vex) '(vect) vex)))

          ((eqv? t #\{ )
           (take-token s)
           (if (eqv? (require-token s) #\})
               (begin (take-token s)
                      '(cell1d))
               (let ((vex (parse-cat s #\} end-symbol)))
                 (if (null? vex)
                     '(cell1d)
                     (case (car vex)
                       ((vect) `(cell1d ,@(cdr vex)))
                       ((hcat) `(cell2d 1 ,(length (cdr vex)) ,@(cdr vex)))
                       ((comprehension)      (error "{a for a in b} syntax is discontinued"))
                       (else
                        (if (and (pair? (cadr vex)) (eq? (caadr vex) 'row))
                            (let ((nr (length (cdr vex)))
                                  (nc (length (cdadr vex))))
                              (begin
                                `(cell2d ,nr ,nc
                                         ,@(apply append
                                                  ;; transpose to storage order
                                                  (apply map list
                                                         (map cdr (cdr vex)))))))
                            `(cell1d ,@(cdr vex)))))))))

          ;; string literal
          ((eqv? t #\")
           (take-token s)
           (let ((ps (parse-string-literal s #\" #f)))
             (if (length> ps 1)
                 `(string ,@(filter (lambda (s)
                                      (not (and (string? s)
                                                (= (length s) 0))))
                                    ps))
                 (car ps))))

          ;; macro call
          ((eqv? t #\@)
           (take-token s)
           (let ((nxt (peek-token s)))
             (if (ts:space? s)
                 (disallowed-space '@ nxt)))
           (with-space-sensitive
            (let ((startloc  (line-number-node s))
                  (head (if (eq? (peek-token s) '|.|)
                            (begin (take-token s) '__dot__)
                            (parse-unary-prefix s))))
              (peek-token s)
              (if (ts:space? s)
                  `(macrocall ,(macroify-name head)
                              ,startloc
                              ,@(parse-space-separated-exprs s))
                  (let ((call (parse-call-chain s head #t)))
                    (if (and (pair? call) (eq? (car call) 'call))
                        `(macrocall ,(macroify-name (cadr call))
                                    ,startloc
                                    ,@(cddr call))
                        `(macrocall ,(macroify-name call)
                                    ,startloc
                                    ,@(parse-space-separated-exprs s))))))))
          ;; command syntax
          ((eqv? t #\`)
           (take-token s)
           `(macrocall @cmd ,(line-number-node s) ,(parse-raw-literal s #\`)))

          ((or (string? t) (number? t) (large-number? t)) (take-token s))

          ((closing-token? t) (error (string "unexpected \"" (take-token s) "\"")))

          (else (error (string "invalid syntax: \"" (take-token s) "\""))))))

(define (valid-modref? e)
  (and (length= e 3) (eq? (car e) '|.|) (pair? (caddr e))
       (eq? (car (caddr e)) 'quote) (symbol? (cadr (caddr e)))
       (or (symbol? (cadr e))
           (valid-modref? (cadr e)))))

(define (macroify-name e . suffixes)
  (cond ((symbol? e) (symbol (apply string #\@ e suffixes)))
        ((valid-modref? e)
         `(|.| ,(cadr e)
               (quote ,(apply macroify-name (cadr (caddr e)) suffixes))))
        (else (error (string "invalid macro use \"@(" (deparse e) ")\"" )))))

(define (simple-string-literal? e) (string? e))

(define (doc-string-literal? e)
  (or (simple-string-literal? e)
      (and (pair? e) (eq? 'string (car e))) ; string interpolation
      (and (length= e 3) (eq? (car e) 'macrocall)
           (simple-string-literal? (caddr e))
           (eq? (cadr e) '@doc_str))
      (and (length= e 4) (eq? (car e) 'macrocall)
           (simple-string-literal? (cadddr e))
           (eq? (cadr e) '@doc_str))))

(define (parse-docstring s production)
  (let ((startloc (line-number-node s)) ; be sure to use the line number from the head of the docstring
        (ex (production s)))
    (if (and (doc-string-literal? ex)
             (let loop ((t (peek-token s)))
               (cond ((closing-token? t) #f)
                     ((newline? t) (take-token s) (loop (peek-token s)))
                     (else #t))))
        `(macrocall (core @doc) ,startloc ,ex ,(production s))
        ex)))

;; --- main entry point ---

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
