;
; Emacs mode for Julia
;

(defvar julia-mode-hook nil)

(add-to-list 'auto-mode-alist '("\\.j\\'\\|\\.jl\\'" . julia-mode))

(defvar julia-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?_ "w" table)   ; underscores in words
    (modify-syntax-entry ?# "<" table)   ; #  single-line comment start
    (modify-syntax-entry ?\n ">" table)  ; \n single-line comment end
    (modify-syntax-entry ?\{ "(} " table)
    (modify-syntax-entry ?\} "){ " table)
    (modify-syntax-entry ?\[ "(] " table)
    (modify-syntax-entry ?\] ")[ " table)
    (modify-syntax-entry ?\( "() " table)
    (modify-syntax-entry ?\) ")( " table)
    (modify-syntax-entry ?\\ "." table)  ; \ is an operator outside quotes
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

; syntax table that holds within strings (uses default emacs behavior)
(defvar julia-mode-string-syntax-table
  (let ((table (make-syntax-table)))
    table)
  "Syntax table for julia-mode")

(defconst julia-font-lock-keywords
  (list '("\\<\\(true\\|false\\|\\|Uint\\(8\\|16\\|32\\|64\\)\\|Int\\(8\\|16\\|32\\|64\\)\\|Float\\|Float32\\|Float64\\|Bool\\|Number\\|Scalar\\|Real\\|Int\\|Tensor\\|Array\\|Tuple\\|NTuple\\|Buffer\\|Size\\|Index\\|Symbol\\|Function\\|Vector\\|Matrix\\|Union\\|Type\\|Any\\|Complex\\|Bottom\\|Ptr\\|Void\\)\\>" .
      font-lock-type-face)
    (cons
     (concat "\\<\\("
         (mapconcat
          'identity
          '("if" "else" "elseif" "while" "for" "begin" "end" "do"
            "try" "catch" "return" "local" "type" "function"
	    "typealias" "break" "continue" "struct" "global"
	    "module" "import" "export" "const")
          "\\|") "\\)\\>")
     'font-lock-keyword-face)
    '("\\\\\\s-*\".*?\"" . font-lock-string-face)))

(defconst julia-block-start-keywords
  (list "if" "while" "for" "begin" "try" "function" "struct"))

(defconst julia-block-other-keywords
  (list "else" "elseif"))

(defconst julia-block-end-keywords
  (list "end" "else" "elseif" "catch"))

(defun member (item lst)
  (if (null lst)
      nil
    (or (equal item (car lst))
	(member item (cdr lst)))))

; TODO: skip keywords inside strings and comments

(defun at-keyword (kw-list)
  ; not a keyword if used as a field name, X.word
  (and (or (= (point) 1)
	   (not (equal (char-before (point)) ?.)))
       (member (current-word) kw-list)))

; get the column of the last open block
(defun last-open-block (min count)
  (cond ((> count 0) (+ 4 (current-indentation)))
        ((<= (point) min) nil)
        (t (backward-word 1)
           (cond ((at-keyword julia-block-start-keywords)
                  (last-open-block min (+ count 1)))
                 ((equal (current-word) "end")
                  (last-open-block min (- count 1)))
                 (t (last-open-block min count))))))

; return indent implied by a special form opening on the previous line, if any
(defun form-indent ()
  (forward-line -1)
  (end-of-line)
  (backward-sexp)
  (if (at-keyword julia-block-other-keywords)
      (+ 4 (current-indentation))
    (if (char-equal (char-after (point)) ?\()
        (progn
          (backward-word 1)
          (let ((cur (current-indentation)))
            (if (at-keyword julia-block-start-keywords)
                (+ 4 cur)
              nil)))
      nil)))

;(defun far-back ()
;  (max (point-min) (- (point) 2000)))

(defmacro error2nil (body) `(condition-case nil ,body (error nil)))

(defun paren-indent ()
  (let* ((p (parse-partial-sexp (point-min) (progn (beginning-of-line)
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
             (error2nil (+ (last-open-block (point-min) 0)
                           (if endtok -4 0)))))
; take same indentation as previous line
;      (save-excursion (beginning-of-line)
;                      (forward-line -1)
;                      (forward-to-indentation 0)
;                      (current-column))
		 (save-excursion
		   (if (and (not (equal (point-min) (line-beginning-position)))
					(progn
					  (forward-line -1)
					  (end-of-line) (backward-char 1)
					  (equal (char-after (point)) ?=)))
			   4 nil))
         0))
	(when (at-keyword julia-block-end-keywords)
	  (forward-word 1)))

(defun julia-mode ()
  "Major mode for editing julia code"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table julia-mode-syntax-table)
  (set (make-local-variable 'font-lock-defaults) '(julia-font-lock-keywords))
;  (set (make-local-variable 'font-lock-syntactic-keywords)
;      (list
;       (list "\\(\\\\\\)\\s-*\".*?\"" 1 julia-mode-char-syntax-table)))
  (set (make-local-variable 'font-lock-syntactic-keywords)
       (list
        (list "\\(\"\\(.\\|\\s-\\)*?[^\\\\]\"\\|\"\"\\)" 0
              julia-mode-string-syntax-table)))
  (set (make-local-variable 'indent-line-function) 'julia-indent-line)
  (setq indent-tabs-mode nil)
  (setq major-mode 'julia-mode)
  (setq mode-name "julia")
  (run-hooks 'julia-mode-hook))

(provide 'julia-mode)
