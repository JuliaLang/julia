;
; Emacs mode for Julia
;

(defvar julia-mode-hook nil)

(add-to-list 'auto-mode-alist '("\\.jl\\'" . julia-mode))

(defvar julia-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?_ "w" table)   ; underscores in words
    (modify-syntax-entry ?@ "w" table)
    (modify-syntax-entry ?# "<" table)   ; #  single-line comment start
    (modify-syntax-entry ?\n ">" table)  ; \n single-line comment end
    (modify-syntax-entry ?\{ "(} " table)
    (modify-syntax-entry ?\} "){ " table)
    (modify-syntax-entry ?\[ "(] " table)
    (modify-syntax-entry ?\] ")[ " table)
    (modify-syntax-entry ?\( "() " table)
    (modify-syntax-entry ?\) ")( " table)
    ;(modify-syntax-entry ?\\ "." table)  ; \ is an operator outside quotes
    (modify-syntax-entry ?'  "." table)  ; character quote or transpose 
    ;(modify-syntax-entry ?\" "." table)
    (modify-syntax-entry ?? "." table)
    (modify-syntax-entry ?$ "." table)
    (modify-syntax-entry ?& "." table)
    (modify-syntax-entry ?* "." table)
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?% "." table)
    table)
  "Syntax table for julia-mode")

;; syntax table that holds within strings
(defvar julia-mode-string-syntax-table
  (let ((table (make-syntax-table)))
    table)
  "Syntax table for julia-mode")

;; disable " inside char quote
(defvar julia-mode-char-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\" "." table)
    table)
  "Syntax table for julia-mode")

(defconst julia-string-regex
  "\"[^\"]*?\\(\\(\\\\\\\\\\)*\\\\\"[^\"]*?\\)*\"")

(defconst julia-char-regex
  "\\(\\s(\\|\\s-\\|-\\|[,%=<>\\+*/?&|$!\\^~\\\\;:]\\|^\\)\\('\\(\\([^']*?[^\\\\]\\)\\|\\(\\\\\\\\\\)\\)'\\)")

(defconst julia-unquote-regex
  "\\(\\s(\\|\\s-\\|-\\|[,%=<>\\+*/?&|!\\^~\\\\;:]\\|^\\)\\($[a-zA-Z0-9_]+\\)")

(defconst julia-forloop-in-regex
  "for +.*[^ 	].* \\(in\\)\\(\\s-\\|$\\)+")

(defconst julia-font-lock-keywords
  (list '("\\<\\(\\|Uint\\(8\\|16\\|32\\|64\\|128\\)\\|Int\\(8\\|16\\|32\\|64\\|128\\)\\|Integer\\|FloatingPoint\\|Float32\\|Float64\\|Complex128\\|Complex64\\|ComplexPair\\|Bool\\|Char\\|Number\\|Real\\|Int\\|Uint\\|Array\\|DArray\\|AbstractArray\\|AbstractVector\\|AbstractMatrix\\|AbstractSparseMatrix\\|SubArray\\|StridedArray\\|StridedVector\\|StridedMatrix\\|VecOrMat\\|StridedVecOrMat\\|Range\\|Range1\\|SparseMatrixCSC\\|Tuple\\|NTuple\\|Symbol\\|Function\\|Vector\\|Matrix\\|Union\\|Type\\|Any\\|Complex\\|None\\|String\\|Ptr\\|Void\\|Exception\\|Task\\|Signed\\|Unsigned\\|Associative\\|Dict\\|IO\\|IOStream\\|Ranges\\|Rational\\|Regex\\|RegexMatch\\|Set\\|IntSet\\|ASCIIString\\|UTF8String\\|ByteString\\|Expr\\|WeakRef\\|Nothing\\|ObjectIdDict\\|SubString\\)\\>" .
      font-lock-type-face)
    (cons
     (concat "\\<\\("
         (mapconcat
          'identity
          '("if" "else" "elseif" "while" "for" "begin" "end" "quote"
            "try" "catch" "return" "local" "abstract" "function" "macro" "ccall"
	    "finally" "typealias" "break" "continue" "type" "global" "@\\w+"
	    "module" "using" "import" "export" "const" "let" "bitstype" "do"
	    "baremodule" "importall")
          "\\|") "\\)\\>")
     'font-lock-keyword-face)
    '("\\<\\(true\\|false\\|C_NULL\\|Inf\\|NaN\\|Inf32\\|NaN32\\|nothing\\)\\>" . font-lock-constant-face)
    (list julia-unquote-regex 2 'font-lock-constant-face)
    (list julia-char-regex 2 'font-lock-string-face)
    (list julia-forloop-in-regex 1 'font-lock-keyword-face)
    ;(list julia-string-regex 0 'font-lock-string-face)
))

(defconst julia-block-start-keywords
  (list "if" "while" "for" "begin" "try" "function" "type" "let" "macro"
	"quote" "do"))

(defconst julia-block-other-keywords
  (list "else" "elseif"))

(defconst julia-block-end-keywords
  (list "end" "else" "elseif" "catch" "finally"))

(defun julia-member (item lst)
  (if (null lst)
      nil
    (or (equal item (car lst))
	(julia-member item (cdr lst)))))

(if (not (fboundp 'evenp))
    (defun evenp (x) (zerop (% x 2))))

(defun julia-find-comment-open (p0)
  (if (< (point) p0)
      nil
    (if (and (equal (char-after (point)) ?#)
	     (evenp (julia-strcount
		     (buffer-substring p0 (point)) ?\")))
	t
      (if (= (point) p0)
	  nil
	(progn (backward-char 1)
	       (julia-find-comment-open p0))))))

(defun julia-in-comment ()
  (save-excursion
    (julia-find-comment-open (line-beginning-position))))

(defun julia-strcount (str chr)
  (let ((i 0)
	(c 0))
    (while (< i (length str))
      (if (equal (elt str i) chr)
	  (setq c (+ c 1)))
      (setq i (+ i 1)))
    c))

(defun julia-in-brackets ()
  (let ((before (buffer-substring (line-beginning-position) (point))))
    (> (julia-strcount before ?[)
       (julia-strcount before ?]))))

(defun julia-at-keyword (kw-list)
  ; not a keyword if used as a field name, X.word, or quoted, :word
  (and (or (= (point) 1)
	   (and (not (equal (char-before (point)) ?.))
		(not (equal (char-before (point)) ?:))))
       (not (julia-in-comment))
       (not (julia-in-brackets))
       (julia-member (current-word t) kw-list)))

;; if backward-sexp gives an error, move back 1 char to move over the '('
(defun julia-safe-backward-sexp ()
  (if (condition-case nil (backward-sexp) (error t))
      (error2nil (backward-char))))

; get the position of the last open block
(defun julia-last-open-block-pos (min)
  (let ((count 0))
    (while (not (or (> count 0) (<= (point) min)))
      (julia-safe-backward-sexp)
      (setq count
	    (cond ((julia-at-keyword julia-block-start-keywords)
		   (+ count 1))
		  ((and (equal (current-word t) "end")
			(not (julia-in-comment)) (not (julia-in-brackets)))
		   (- count 1))
		  (t count))))
    (if (> count 0)
	(point)
      nil)))

; get indent for last open block
(defun julia-last-open-block (min)
  (let ((pos (julia-last-open-block-pos min)))
    (and pos
	 (progn
	   (goto-char pos)
	   (+ julia-basic-offset (current-indentation))))))

(defmacro error2nil (body) `(condition-case nil ,body (error nil)))

(defun julia-paren-indent ()
  (let* ((p (parse-partial-sexp (save-excursion
				  ;; only indent by paren if the last open
				  ;; paren is closer than the last open
				  ;; block
				  (or (julia-last-open-block-pos (point-min))
				      (point-min)))
				(progn (beginning-of-line)
				       (point))))
         (pos (cadr p)))
    (if (or (= 0 (car p)) (null pos))
        nil
      (progn (goto-char pos) (+ 1 (current-column))))))

(defun julia-indent-line ()
  "Indent current line of julia code"
  (interactive)
;  (save-excursion
    (end-of-line)
    (indent-line-to
     (or (save-excursion (error2nil (julia-paren-indent)))
         (save-excursion
           (let ((endtok (progn
                           (beginning-of-line)
                           (forward-to-indentation 0)
                           (julia-at-keyword julia-block-end-keywords))))
             (error2nil (+ (julia-last-open-block (point-min))
                           (if endtok (- julia-basic-offset) 0)))))
	 ;; previous line ends in =
	 (save-excursion
	   (if (and (not (equal (point-min) (line-beginning-position)))
		    (progn
		      (forward-line -1)
		      (end-of-line) (backward-char 1)
		      (equal (char-after (point)) ?=)))
	       (+ julia-basic-offset (current-indentation))
	     nil))
	 ;; take same indentation as previous line
	 (save-excursion (forward-line -1)
			 (current-indentation))
         0))
    (when (julia-at-keyword julia-block-end-keywords)
      (forward-word 1)))

(defun julia-mode ()
  "Major mode for editing julia code"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table julia-mode-syntax-table)
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-start-skip) "#+\\s-*")
  (set (make-local-variable 'font-lock-defaults) '(julia-font-lock-keywords))
;  (set (make-local-variable 'font-lock-syntactic-keywords)
;      (list
;       (list "\\(\\\\\\)\\s-*\".*?\"" 1 julia-mode-char-syntax-table)))
  (set (make-local-variable 'font-lock-syntactic-keywords)
       (list
	(list julia-char-regex 2
	      julia-mode-char-syntax-table)
;        (list julia-string-regex 0
;              julia-mode-string-syntax-table)
))
  (set (make-local-variable 'indent-line-function) 'julia-indent-line)
  (set (make-local-variable 'julia-basic-offset) 4)
  (setq indent-tabs-mode nil)
  (setq major-mode 'julia-mode)
  (setq mode-name "julia")
  (run-hooks 'julia-mode-hook))

(provide 'julia-mode)
