;
; Emacs mode for Julia
;

(defvar julia-mode-hook nil)

(add-to-list 'auto-mode-alist '("\\.j\\'\\|\\.jl\\'" . julia-mode))

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
  "for +[^ 	]+ +.*\\(in\\)\\(\\s-\\|$\\)+")

(defconst julia-font-lock-keywords
  (list '("\\<\\(\\|Uint\\(8\\|16\\|32\\|64\\)\\|Int\\(8\\|16\\|32\\|64\\)\\|Integer\\|Float\\|Float32\\|Float64\\|Complex128\\|Complex64\\|ComplexNum\\|Bool\\|Char\\|Number\\|Scalar\\|Real\\|Int\\|Uint\\|Array\\|DArray\\|AbstractArray\\|AbstractVector\\|AbstractMatrix\\|SubArray\\|StridedArray\\|StridedVector\\|StridedMatrix\\|VecOrMat\\|StridedVecOrMat\\|Range\\|Range1\\|SparseMatrixCSC\\|Tuple\\|NTuple\\|Buffer\\|Size\\|Index\\|Symbol\\|Function\\|Vector\\|Matrix\\|Union\\|Type\\|Any\\|Complex\\|None\\|String\\|Ptr\\|Void\\|Exception\\|PtrInt\\|Long\\|Ulong\\)\\>" .
      font-lock-type-face)
    (cons
     (concat "\\<\\("
         (mapconcat
          'identity
          '("if" "else" "elseif" "while" "for" "begin" "end" "quote"
            "try" "catch" "return" "local" "abstract" "function" "macro" "ccall"
	    "typealias" "break" "continue" "type" "global" "@\\w+"
	    "module" "import" "export" "const" "let" "bitstype")
          "\\|") "\\)\\>")
     'font-lock-keyword-face)
    '("\\<\\(true\\|false\\|C_NULL\\|Inf\\|NaN\\|Inf32\\|NaN32\\)\\>" . font-lock-constant-face)
    (list julia-unquote-regex 2 'font-lock-constant-face)
    (list julia-char-regex 2 'font-lock-string-face)
    (list julia-forloop-in-regex 1 'font-lock-keyword-face)
    ;(list julia-string-regex 0 'font-lock-string-face)
))

(defconst julia-block-start-keywords
  (list "if" "while" "for" "begin" "try" "function" "type" "let" "macro"
	"quote"))

(defconst julia-block-other-keywords
  (list "else" "elseif"))

(defconst julia-block-end-keywords
  (list "end" "else" "elseif" "catch"))

(defun member (item lst)
  (if (null lst)
      nil
    (or (equal item (car lst))
	(member item (cdr lst)))))

; TODO: skip keywords inside strings

; fixme? is the line-beginning-position necessary?
; Currently you are fine with this kind of comment:
;    # Here's a comment with the word "for" in it
; but not with this:
;      x = 5   # Here's another comment with the word "for" in it
(defun in-comment ()
  (equal (char-after (+ (line-beginning-position) (current-indentation)))
	 ?#))

(defun strcount (str chr)
  (let ((i 0)
	(c 0))
    (while (< i (length str))
      (if (equal (elt str i) chr)
	  (setq c (+ c 1)))
      (setq i (+ i 1)))
    c))

(defun in-brackets ()
  (let ((before (buffer-substring (line-beginning-position) (point))))
    (> (strcount before ?[)
       (strcount before ?]))))

(defun at-keyword (kw-list)
  ; not a keyword if used as a field name, X.word, or quoted, :word
  (and (or (= (point) 1)
	   (and (not (equal (char-before (point)) ?.))
		(not (equal (char-before (point)) ?:))))
       (not (in-comment))
       (not (in-brackets))
       (member (current-word) kw-list)))

; get the position of the last open block
(defun last-open-block-pos (min)
  (let ((count 0))
    (while (not (or (> count 0) (<= (point) min)))
      (backward-word 1)
      (setq count
	    (cond ((at-keyword julia-block-start-keywords)
		   (+ count 1))
		  ((and (equal (current-word) "end")
			(not (in-comment)) (not (in-brackets)))
		   (- count 1))
		  (t count))))
    (if (> count 0)
	(point)
      nil)))

; get indent for last open block
(defun last-open-block (min)
  (let ((pos (last-open-block-pos min)))
    (and pos
	 (progn
	   (goto-char pos)
	   (+ julia-basic-offset (current-indentation))))))

; return indent implied by a special form opening on the previous line, if any
(defun form-indent ()
  (forward-line -1)
  (end-of-line)
  (backward-sexp)
  (if (at-keyword julia-block-other-keywords)
      (+ julia-basic-offset (current-indentation))
    (if (char-equal (char-after (point)) ?\()
        (progn
          (backward-word 1)
          (let ((cur (current-indentation)))
            (if (at-keyword julia-block-start-keywords)
                (+ julia-basic-offset cur)
              nil)))
      nil)))

;(defun far-back ()
;  (max (point-min) (- (point) 2000)))

(defmacro error2nil (body) `(condition-case nil ,body (error nil)))

(defun paren-indent ()
  (let* ((p (parse-partial-sexp (save-excursion
				  ;; only indent by paren if the last open
				  ;; paren is closer than the last open
				  ;; block
				  (or (last-open-block-pos (point-min))
				      (point-min)))
				(progn (beginning-of-line)
				       (point))))
         (pos (cadr p)))
    (if (or (= 0 (car p)) (null pos))
        nil
      (progn (goto-char pos) (+ 1 (current-column))))))
;  (forward-line -1)
;  (end-of-line)
;  (let ((pos (condition-case nil
;                (scan-lists (point) -1 1)
;              (error nil))))
;   (if pos
;       (progn (goto-char pos) (+ 1 (current-column)))
;     nil)))

(defun julia-indent-line ()
  "Indent current line of julia code"
  (interactive)
;  (save-excursion
    (end-of-line)
    (indent-line-to
     (or (save-excursion (error2nil (form-indent)))
         (save-excursion (error2nil (paren-indent)))
         (save-excursion
           (let ((endtok (progn
                           (beginning-of-line)
                           (forward-to-indentation 0)
                           (at-keyword julia-block-end-keywords))))
             (error2nil (+ (last-open-block (point-min))
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
    (when (at-keyword julia-block-end-keywords)
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
