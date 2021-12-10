#-------------------------------------------------------------------------------

# Parser Utils

function TODO(str)
    error("TODO: $str")
end

# Placeholder - bump an identifier or literal in place of a production we
# haven't implemented yet.
function bumpTODO(ps::ParseState)
    if peek(ps) == K"Identifier" || isliteral(peek(ps))
        bump(ps)
    else
        error("bumpTODO - got unexpected $(peek(ps))")
    end
end

function is_closing_token(ps::ParseState, tok)
    k = kind(tok)
    return k in (K"else", K"elseif", K"catch", K"finally",
                 K",", K")", K"]", K"}", K";",
                 K"EndMarker") || (k == K"end" && !ps.end_symbol)
end

function has_whitespace_prefix(tok::SyntaxToken)
    tok.had_whitespace
end

#-------------------------------------------------------------------------------
# Parser
#
# The definitions and top-level comments here were automatically generated to
# match the structure of Julia's official flisp-based parser.
#
# This is to make both codebases mutually understandable and make porting
# changes simple.


# parse left-to-right binary operator
# produces structures like (+ (+ (+ 2 3) 4) 5)
#
# flisp: (define-macro (parse-LtoR s down ops)
function parse_LtoR(ps::ParseState, down, is_op)
    mark = position(ps)
    down(ps)
    while is_op(peek(ps))
        bump(ps)
        down(ps)
        emit(ps, mark, K"call", INFIX_FLAG)
    end
end

# parse right-to-left binary operator
# produces structures like (= a (= b (= c d)))
#        (define-macro (parse-RtoL s down ops syntactic self)
# flisp: 
function parse_RtoL(ps::ParseState, down, is_op, syntactic, self)
    mark = position(ps)
    down(ps)
    k = peek(ps)
    if is_op(k)
        if (syntactic isa Bool && syntactic) || syntactic(k)
            bump(ps, TRIVIA_FLAG)
            self(ps)
            emit(ps, mark, k)
        else
            bump(ps)
            self(ps)
            emit(ps, mark, K"call", INFIX_FLAG)
        end
    end
end

# parse block-like structures
#
# `delimiters` are a set of token kinds acting as delimiters; `closing_tokens`
# stop the parsing.
#
# Returns true if the block was nontrivial and a node needs to be emitted by
# the caller.
#
# flisp: (define (parse-Nary s down ops head closer? add-linenums)
function parse_Nary(ps::ParseState, down, delimiters, closing_tokens)
    bump_newlines(ps)
    k = peek(ps)
    if k in closing_tokens
        return true
    end
    # Skip leading operator
    n_delims = 0
    if k in delimiters
        bump(ps, TRIVIA_FLAG)
        n_delims += 1
    else
        down(ps)
    end
    while peek(ps) in delimiters
        bump(ps, TRIVIA_FLAG)
        n_delims += 1
        k = peek(ps)
        if k == K"EndMarker" || k in closing_tokens
            break
        elseif k in delimiters
            # ignore empty delimited sections
            # a;;;b  ==>  (block a b) 
            continue
        end
        down(ps)
    end
    return n_delims != 0
end

# the principal non-terminals follow, in increasing precedence order

# Parse a newline or semicolon-delimited list of expressions. 
# Repeated delimiters are allowed but ignored
# (a;b;c)     ==>  (block a b c)
# (a;;;b;;)   ==>  (block a b)
# ===
# begin
#   a
#   b
# end
# ==> (block a b)
#
# flisp: (define (parse-block s (down parse-eq))
function parse_block(ps::ParseState, down=parse_eq)
    mark = position(ps)
    if parse_Nary(ps, down, (K"NewlineWs", K";"),
                  (K"end", K"else", K"elseif", K"catch", K"finally"))
        emit(ps, mark, K"block")
    end
end

# ";" at the top level produces a sequence of top level expressions
#
# a;b;c   ==>  (toplevel a b c)
# a;;;b;; ==>  (toplevel a b)
#
# flisp: (define (parse-stmts s)
function parse_stmts(ps::ParseState)
    mark = position(ps)
    do_emit = parse_Nary(ps, parse_docstring, (K";",), (K"NewlineWs",))
    # check for unparsed junk after an expression
    junk_mark = position(ps)
    while peek(ps) ∉ (K"EndMarker", K"NewlineWs")
        # Error recovery
        bump(ps)
    end
    if junk_mark != position(ps)
        emit(ps, junk_mark, K"Error",
             error="Extra tokens after end of expression")
    end
    if do_emit
        emit(ps, mark, K"toplevel")
    end
end

# flisp: (define (parse-eq s) (parse-assignment s parse-comma))
function parse_eq(ps::ParseState)
    parse_assignment(ps, parse_comma)
end

# parse_eq_star is used where commas are special, for example in an argument list
#
# flisp: (define (parse-eq* s)
function parse_eq_star(ps::ParseState)
    k = peek(ps)
    k2 = peek(ps,2)
    if (isliteral(k) || k == K"Identifier") && k2 in (K",", K")", K"}", K"]")
        # optimization: skip checking the whole precedence stack if we have a
        # simple token followed by a common closing token
        bump(ps)
    else
        parse_assignment(ps, parse_pair)
    end
end

# flisp: (define (eventually-call? ex)
function is_eventually_call(ex)
    TODO("is_eventually_call unimplemented")
end

# flisp: (define (short-form-function-loc ex lno)
function short_form_function_loc(ex, lno)
    TODO("short_form_function_loc unimplemented")
end

# flisp: (define (parse-assignment s down)
function parse_assignment(ps::ParseState, down)
    mark = position(ps)
    down(ps)
    k = peek(ps)
    if !is_prec_assignment(k)
        return
    end
    if k == K"~"
        bump(ps)
        if ps.space_sensitive # && ...
            # Prefix operator ~x ?
            TODO("parse_assignment... ~ not implemented")
        else
            parse_assignment(ps, down)
            # ~ is the only non-syntactic assignment-precedence operator.
            emit(ps, mark, K"call", INFIX_FLAG)
        end
    else
        bump(ps, TRIVIA_FLAG)
        parse_assignment(ps, down)
        emit(ps, mark, k)
    end
end

# parse-comma is needed for commas outside parens, for example a = b,c
#
# flisp: (define (parse-comma s)
function parse_comma(ps::ParseState)
    mark = position(ps)
    n_commas = 0
    parse_pair(ps)
    first = true
    while true
        if peek(ps) != K","
            if !first || n_commas > 0
                # FIXME: is use of n_commas correct here? flisp comments say:
                # () => (tuple)
                # (ex2 ex1) => (tuple ex1 ex2)
                # (ex1,) => (tuple ex1)
                emit(ps, mark, K"tuple")
            end
            return
        end
        first = false
        bump(ps, TRIVIA_FLAG)
        n_commas += 1
        if peek(ps) == K"="
            # Test:
            # x, = ...
            continue
        end
        parse_pair(ps)
    end
end

# flisp: (define (parse-pair s) (parse-RtoL s parse-cond is-prec-pair? #f parse-pair))
function parse_pair(ps::ParseState)
    parse_RtoL(ps, parse_cond, is_prec_pair, false, parse_pair)
end

# flisp: (define (parse-cond s)
function parse_cond(ps::ParseState)
    mark = position(ps)
    parse_arrow(ps)
    t = peek_token(ps)
    if kind(t) != K"?"
        return
    end
    cond_flags = EMPTY_FLAGS
    if !t.had_whitespace
        # a? b : c
        emit_diagnostic(ps, error="space required before `?` operator")
        cond_flags |= ERROR_FLAG
    end
    bump(ps, TRIVIA_FLAG) # ?
    t = peek_token(ps, skip_newlines=true)
    if !t.had_whitespace
        # a ?b : c
        emit_diagnostic(ps, error="space required after `?` operator")
        cond_flags |= ERROR_FLAG
    end
    parse_eq_star(ParseState(ps, range_colon_enabled=false))
    t = peek_token(ps)
    if kind(t) != K":"
        # a ? b:   ==>   (if-e a b)
        emit(ps, mark, K"if", cond_flags,
             error="colon expected in `?` expression")
        return
    end
    if !t.had_whitespace
        # a ? b: c
        emit_diagnostic(ps, error="space required before `:` in `?` expression")
        cond_flags |= ERROR_FLAG
    end
    bump(ps, TRIVIA_FLAG) # :
    t = peek_token(ps, skip_newlines=true)
    if !t.had_whitespace
        # a ? b :c
        emit_diagnostic(ps, error="space required after `:` in `?` expression")
        cond_flags |= ERROR_FLAG
    end
    parse_eq_star(ps)
    emit(ps, mark, K"if", cond_flags)
end

# Parse arrows
# x → y     ==>  (call-i x → y)
# x <--> y  ==>  (call-i x <--> y)
# x --> y   ==>  (x --> y)           # The only syntactic arrow
#
# flisp: (define (parse-arrow s) (parse-RtoL s parse-or         is-prec-arrow? (eq? t '-->) parse-arrow))
function parse_arrow(ps::ParseState)
    parse_RtoL(ps, parse_or, is_prec_arrow, ==(K"-->"), parse_arrow)
end

# x || y || z   ==>   (call-i x || (call-i y || z))
#
# flisp: (define (parse-or s)    (parse-RtoL s parse-and        is-prec-lazy-or? #t parse-or))
function parse_or(ps::ParseState)
    parse_RtoL(ps, parse_and, is_prec_lazy_or, true, parse_or)
end

# flisp: (define (parse-and s)   (parse-RtoL s parse-comparison is-prec-lazy-and? #t parse-and))
function parse_and(ps::ParseState)
    parse_RtoL(ps, parse_comparison, is_prec_lazy_and, true, parse_and)
end

# Parse comparison chains like
# x > y        ==> (call-i > x y)
# x < y < z    ==> (comparison x < y < z)
# x == y < z   ==> (comparison x == y < z)
#
# flisp: (define (parse-comparison s)
function parse_comparison(ps::ParseState)
    mark = position(ps)
    parse_pipe_lt(ps)
    n_comparisons = 0
    op_pos = 0
    initial_kind = peek(ps)
    while is_prec_comparison(peek(ps))
        n_comparisons += 1
        op_pos = bump(ps)
        parse_pipe_lt(ps)
    end
    if n_comparisons == 1
        if initial_kind in (K"<:", K">:")
            # Type comparisons are syntactic and have their kind encoded in the head
            # x <: y  ==>  (<: x y)
            # x >: y  ==>  (>: x y)
            reset_token!(ps, op_pos, flags=TRIVIA_FLAG)
            emit(ps, mark, initial_kind)
        else
            emit(ps, mark, K"call", INFIX_FLAG)
        end
    elseif n_comparisons > 1
        emit(ps, mark, K"comparison", INFIX_FLAG)
    end
end

# x |> y |> z  ==>  ((x |> y) |> z)
# flisp: (define (parse-pipe< s) (parse-RtoL s parse-pipe> is-prec-pipe<? #f parse-pipe<))
function parse_pipe_lt(ps::ParseState)
    parse_RtoL(ps, parse_pipe_gt, is_prec_pipe_lt, false, parse_pipe_lt)
end

# x <| y <| z  ==>  (x <| (y <| z))
# flisp: (define (parse-pipe> s) (parse-LtoR s parse-range is-prec-pipe>?))
function parse_pipe_gt(ps::ParseState)
    parse_LtoR(ps, parse_range, is_prec_pipe_gt)
end

# parse ranges and postfix ...
# colon is strange; 3 arguments with 2 colons yields one call:
# 1:2       ==> (call-i 1 : 2)
# 1:2:3     ==> (call-i 1 : 2 3)
# Chaining gives
# a:b:c:d:e ==> (call-i (call-i a : b c) : d e)
#
# flisp: (define (parse-range s)
function parse_range(ps::ParseState)
    mark = position(ps)
    parse_expr(ps)
    initial_kind = peek(ps)
    if initial_kind != K":" && is_prec_colon(initial_kind)
        # a..b     ==>   (call-i a .. b)
        # a … b    ==>   (call-i a … b)
        bump(ps)
        parse_expr(ps)
        emit(ps, mark, K"call", INFIX_FLAG)
    elseif initial_kind == K":" && ps.range_colon_enabled
        # a ? b : c     ==>   (if a b c)
        # a ? b : c:d   ==>   (if a b (call-i c : d))
        n_colons = 0
        while peek(ps) == K":"
            if ps.space_sensitive &&
                    peek_token(ps).had_whitespace &&
                    !peek_token(ps, 2).had_whitespace
                # Tricky cases in space sensitive mode
                # [1 :a]      ==>  (vcat 1 (quote a))
                # [1 2:3 :a]  ==>  (vcat 1 (call-i 2 : 3) (quote a))
                break
            end
            n_colons += 1
            bump(ps, n_colons == 1 ? EMPTY_FLAGS : TRIVIA_FLAG)
            t2 = peek_token(ps)
            if is_closing_token(ps, kind(t2))
                # 1: }    ==>  (call-i-e 1 :)
                # 1:2: }  ==>  (call-i-e 1 : 2)
                emit(ps, mark, K"call", INFIX_FLAG,
                     error="missing last argument in range expression")
                emit_diagnostic(ps, error="found unexpected closing token")
                return
            end
            if t2.had_newline
                # Error message for people coming from python
                # ===
                # 1:
                # 2
                # ==> (call-i-e 1 :)
                emit(ps, mark, K"call", INFIX_FLAG|ERROR_FLAG)
                emit_diagnostic(ps, error="line break after `:` in range expression")
                return
            elseif kind(t2) in (K"<", K">") && !t2.had_whitespace
                # :> and :< are not operators
                ks = untokenize(kind(t2))
                emit_diagnostic(ps, error="Invalid `:$ks` found - did you mean `$ks:`?")
            end
            parse_expr(ps)
            if n_colons == 2
                emit(ps, mark, K"call", INFIX_FLAG)
                n_colons = 0
            end
        end
        if n_colons > 0
            emit(ps, mark, K"call", INFIX_FLAG)
        end
    end

    # x...     ==>  (... x)
    # x:y...   ==>  (... (call-i x : y))
    # x..y...  ==>  (... (call-i x .. y))   # flisp parser fails here
    if peek(ps) == K"..."
        bump(ps, TRIVIA_FLAG)
        emit(ps, mark, K"...")
    end
end

# parse left to right chains of a given binary operator
#
# flisp: (define (parse-chain s down op)
function parse_chain(ps::ParseState, down, op_kind)
    while (t = peek_token(ps); kind(t) == op_kind)
        if ps.space_sensitive && t.had_whitespace &&
            is_both_unary_and_binary(kind(t)) &&
            !peek_token(ps, 2).had_whitespace
            # [x +y]  ==>  (hcat x (call + y))
            break
        end
        bump(ps, TRIVIA_FLAG)
        down(ps)
    end
end

# Parse left to right, combining any of `chain_ops` into one call
# a - b - c  ==>  (call-i (call-i a - b) - c)
#
# flisp: parse-with-chains
function parse_with_chains(ps::ParseState, down, is_op, chain_ops)
    mark = position(ps)
    down(ps)
    while (t = peek_token(ps); is_op(kind(t)))
        if ps.space_sensitive && t.had_whitespace &&
            is_both_unary_and_binary(kind(t)) &&
            !peek_token(ps, 2).had_whitespace
            # The following is two elements of a hcat
            # [x+y +z]   ==>  (hcat (call-i x + y) (call + z))
            # Conversely
            # [x+y+z]    ==>  (hcat (call-i x + y z))
            # [x+y + z]  ==>  (hcat (call-i x + y z))
            break
        end
        bump(ps)
        down(ps)
        if kind(t) in chain_ops && !is_decorated(t)
            # a + b + c    ==>  (call-i a + b c)
            # a +₁ b +₁ c  ==>  (call-i (call-i a +₁ b) +₁ c)
            # a .+ b .+ c  ==>  (call-i (call-i a .+ b) .+ c)
            parse_chain(ps, down, kind(t))
        end
        emit(ps, mark, K"call", INFIX_FLAG)
    end
end

# flisp: parse-expr
function parse_expr(ps::ParseState)
    parse_with_chains(ps, parse_term, is_prec_plus, (K"+", K"++"))
end

# flisp: parse-term
function parse_term(ps::ParseState)
    parse_with_chains(ps, parse_rational, is_prec_times, (K"*",))
end

# flisp: parse-rational
function parse_rational(ps::ParseState)
    parse_LtoR(ps, parse_shift, is_prec_rational)
end

# flisp: parse-shift
function parse_shift(ps::ParseState)
    parse_LtoR(ps, parse_unary_subtype, is_prec_bitshift)
end

# parse `<: A where B` as `<: (A where B)` (issue #21545)
#
# flisp: parse-unary-subtype
function parse_unary_subtype(ps::ParseState)
    bumpTODO(ps)
    #TODO("parse_unary_subtype unimplemented")
end

# flisp: (define (parse-where-chain s first)
function parse_where_chain(ps::ParseState, first)
    TODO("parse_where_chain unimplemented")
end

# flisp: (define (parse-where s down)
function parse_where(ps::ParseState, down)
    TODO("parse_where unimplemented")
end

# given an expression and the next token, is there a juxtaposition
# operator between them?
#
# flisp: (define (juxtapose? s expr t)
function is_juxtapose(s, expr, t)
    TODO("is_juxtapose unimplemented")
end

# flisp: (define (parse-juxtapose s)
function parse_juxtapose(ps::ParseState)
    TODO("parse_juxtapose unimplemented")
end

# flisp: (define (maybe-negate op num)
function maybe_negate(op, num)
    TODO("maybe_negate unimplemented")
end

# operators handled by parse-unary at the start of an expression

# flisp: (define (parse-unary s)
function parse_unary(ps::ParseState)
    TODO("parse_unary unimplemented")
end

# flisp: (define (fix-syntactic-unary e)
function fix_syntactic_unary(e)
    TODO("fix_syntactic_unary unimplemented")
end

# flisp: (define (parse-unary-call s op un spc)
function parse_unary_call(ps::ParseState, op, un, spc)
    TODO("parse_unary_call unimplemented")
end

# handle ^ and .^
# -2^3 is parsed as -(2^3), so call parse-decl for the first argument,
# and parse-unary from then on (to handle 2^-3)
#
# flisp: (define (parse-factor s)
function parse_factor(ps::ParseState)
    TODO("parse_factor unimplemented")
end

# flisp: (define (parse-factor-with-initial-ex s ex0 (tok #f))
function parse_factor_with_initial_ex(ps::ParseState, ex0; tok=false)
    TODO("parse_factor_with_initial_ex unimplemented")
end

# flisp: (define (parse-factor-after s) (parse-RtoL s parse-juxtapose is-prec-power? #f parse-factor-after))
function parse_factor_after(ps::ParseState)
    parse_RtoL(ps, parse_juxtapose, is_prec_power, false, parse_factor_after)
end

# flisp: (define (parse-decl s)
function parse_decl(ps::ParseState)
    TODO("parse_decl unimplemented")
end

# flisp: (define (parse-decl-with-initial-ex s ex)
function parse_decl_with_initial_ex(ps::ParseState, ex)
    TODO("parse_decl_with_initial_ex unimplemented")
end

# parse function call, indexing, dot, and transpose expressions
# also handles looking for syntactic reserved words
#
# flisp: (define (parse-call s)
function parse_call(ps::ParseState)
    TODO("parse_call unimplemented")
end

# flisp: (define (parse-call-with-initial-ex s ex tok)
function parse_call_with_initial_ex(ps::ParseState, ex, tok)
    TODO("parse_call_with_initial_ex unimplemented")
end

# flisp: (define (parse-unary-prefix s)
function parse_unary_prefix(ps::ParseState)
    TODO("parse_unary_prefix unimplemented")
end

# flisp: (define (parse-def s is-func anon)
function parse_def(ps::ParseState, is_func, anon)
    TODO("parse_def unimplemented")
end

# flisp: (define (disallowed-space-error lno ex t)
function disallowed_space_error(lno, ex, t)
    TODO("disallowed_space_error unimplemented")
end

# flisp: (define (disallow-space s ex t)
function disallow_space(s, ex, t)
    TODO("disallow_space unimplemented")
end

# string macro suffix for given delimiter t
#
# flisp: (define (macsuffix t)
function macsuffix(t)
    TODO("macsuffix unimplemented")
end

# flisp: (define (parse-call-chain s ex macrocall?)
function parse_call_chain(ps::ParseState, ex, is_macrocall)
    TODO("parse_call_chain unimplemented")
end

# flisp: (define (expect-end s word)
function expect_end(s, word)
    TODO("expect_end unimplemented")
end

# flisp: (define (expect-end-error t word)
function expect_end_error(t, word)
    TODO("expect_end_error unimplemented")
end

# flisp: (define (parse-subtype-spec s)
function parse_subtype_spec(ps::ParseState)
    TODO("parse_subtype_spec unimplemented")
end

# flisp: (define (valid-func-sig? paren sig)
function is_valid_func_sig(paren, sig)
    TODO("is_valid_func_sig unimplemented")
end

# flisp: (define (valid-1arg-func-sig? sig)
function is_valid_1arg_func_sig(sig)
    TODO("is_valid_1arg_func_sig unimplemented")
end

# flisp: (define (unwrap-where x)
function unwrap_where(x)
    TODO("unwrap_where unimplemented")
end

# flisp: (define (rewrap-where x w)
function rewrap_where(x, w)
    TODO("rewrap_where unimplemented")
end

# flisp: (define (parse-struct-def s mut? word)
function parse_struct_def(ps::ParseState, is_mut, word)
    TODO("parse_struct_def unimplemented")
end

# consume any number of line endings from a token stream
#
# flisp: (define (take-lineendings s)
function take_lineendings(s)
    TODO("take_lineendings unimplemented")
end

# parse expressions or blocks introduced by syntactic reserved words
#
# flisp: (define (parse-resword s word)
function parse_resword(ps::ParseState, word)
    TODO("parse_resword unimplemented")
end

# flisp: (define (parse-do s)
function parse_do(ps::ParseState)
    TODO("parse_do unimplemented")
end

# flisp: (define (macrocall-to-atsym e)
function macrocall_to_atsym(e)
    TODO("macrocall_to_atsym unimplemented")
end

# flisp: (define (parse-imports s word)
function parse_imports(ps::ParseState, word)
    TODO("parse_imports unimplemented")
end

# flisp: (define (parse-macro-name s)
function parse_macro_name(ps::ParseState)
    TODO("parse_macro_name unimplemented")
end

# flisp: (define (parse-atsym s)
function parse_atsym(ps::ParseState)
    TODO("parse_atsym unimplemented")
end

# flisp: (define (parse-import-dots s)
function parse_import_dots(ps::ParseState)
    TODO("parse_import_dots unimplemented")
end

# flisp: (define (parse-import-path s word)
function parse_import_path(ps::ParseState, word)
    TODO("parse_import_path unimplemented")
end

# flisp: (define (parse-import s word from)
function parse_import(ps::ParseState, word, from)
    TODO("parse_import unimplemented")
end

# parse comma-separated assignments, like "i=1:n,j=1:m,..."
#
# flisp: (define (parse-comma-separated s what)
function parse_comma_separated(ps::ParseState, what)
    TODO("parse_comma_separated unimplemented")
end

# flisp: (define (parse-comma-separated-assignments s)
function parse_comma_separated_assignments(ps::ParseState)
    TODO("parse_comma_separated_assignments unimplemented")
end

# as above, but allows both "i=r" and "i in r"
#
# flisp: (define (parse-iteration-spec s)
function parse_iteration_spec(ps::ParseState)
    TODO("parse_iteration_spec unimplemented")
end

# flisp: (define (parse-comma-separated-iters s)
function parse_comma_separated_iters(ps::ParseState)
    TODO("parse_comma_separated_iters unimplemented")
end

# flisp: (define (parse-space-separated-exprs s)
function parse_space_separated_exprs(ps::ParseState)
    TODO("parse_space_separated_exprs unimplemented")
end

# flisp: (define (has-parameters? lst)
function is_has_parameters(lst)
    TODO("is_has_parameters unimplemented")
end

# flisp: (define (to-kws lst)
function to_kws(lst)
    TODO("to_kws unimplemented")
end

# like parse-arglist, but with `for` parsed as a generator
#
# flisp: (define (parse-call-arglist s closer)
function parse_call_arglist(ps::ParseState, closer)
    TODO("parse_call_arglist unimplemented")
end

# handle function call argument list, or any comma-delimited list.
# . an extra comma at the end is allowed
# . expressions after a ; are enclosed in (parameters ...)
# . an expression followed by ... becomes (... x)
#
# flisp: (define (parse-arglist s closer (add-linenums #f))
function parse_arglist(ps::ParseState, closer; add_linenums=false)
    TODO("parse_arglist unimplemented")
end

# flisp: (define (parse-vect s first closer)
function parse_vect(ps::ParseState, first, closer)
    TODO("parse_vect unimplemented")
end

# flisp: (define (parse-generator s first)
function parse_generator(ps::ParseState, first)
    TODO("parse_generator unimplemented")
end

# flisp: (define (parse-comprehension s first closer)
function parse_comprehension(ps::ParseState, first, closer)
    TODO("parse_comprehension unimplemented")
end

# flisp: (define (parse-array s first closer gotnewline last-end-symbol)
function parse_array(ps::ParseState, first, closer, gotnewline, last_end_symbol)
    TODO("parse_array unimplemented")
end

# flisp: (define (expect-space-before s t)
function expect_space_before(s, t)
    TODO("expect_space_before unimplemented")
end

# Parse syntax inside of `[]` or `{}`
#
# flisp: (define (parse-cat s closer last-end-symbol)
function parse_cat(ps::ParseState, closer, last_end_symbol)
    TODO("parse_cat unimplemented")
    ps = ParseState(ps0, range_colon_enabled=true,
                    space_sensitive=true,
                    where_enabled=true,
                    whitespace_newline=false,
                    for_generator=true)
    if require_token(ps) == closer
        take_token!(ps)
        return 
    end
end

# flisp: (define (kw-to-= e) (if (kwarg? e) (cons '= (cdr e)) e))
function kw_to_equals(e)
    TODO("kw_to_equals unimplemented")
end
# flisp: (define (=-to-kw e) (if (assignment? e) (cons 'kw (cdr e)) e))
function equals_to_kw(e)
    TODO("equals_to_kw unimplemented")
end

# translate nested (parameters ...) expressions to a statement block if possible
# this allows us to first parse tuples using parse-arglist
#
# flisp: (define (parameters-to-block e)
function parameters_to_block(e)
    TODO("parameters_to_block unimplemented")
end

# flisp: (define (rm-linenums e)
function rm_linenums(e)
    TODO("rm_linenums unimplemented")
end

# convert an arglist to a tuple or block expr
# leading-semi? means we saw (; ...)
# comma? means there was a comma after the first expression
#
# flisp: (define (arglist-to-tuple s leading-semi? comma? args . first)
function arglist_to_tuple(s, is_leading_semi, is_comma, args, _, first)
    TODO("arglist_to_tuple unimplemented")
end

# flisp: (define (tuple-to-arglist e)
function tuple_to_arglist(e)
    TODO("tuple_to_arglist unimplemented")
end

# flisp: (define (parse-paren s (checked #t)) (car (parse-paren- s checked)))
function parse_paren(ps::ParseState; checked=true)
    TODO("parse_paren unimplemented")
end

# return (expr . arglist) where arglist is #t iff this isn't just a parenthesized expr
#
# flisp: (define (parse-paren- s checked)
function parse_paren_(ps::ParseState, checked)
    TODO("parse_paren_ unimplemented")
end

# flisp: (define (not-eof-for delim c)
function not_eof_for(delim, c)
    TODO("not_eof_for unimplemented")
end

# flisp: (define (take-char p)
function take_char(p)
    TODO("take_char unimplemented")
end

# map the first element of lst
#
# flisp: (define (map-first f lst)
function map_first(f, lst)
    TODO("map_first unimplemented")
end

# map the elements of lst where (pred index) is true
# e.g., (map-at odd? (lambda (x) 0) '(a b c d)) -> '(a 0 c 0)
#
# flisp: (define (map-at pred f lst)
function map_at(pred, f, lst)
    TODO("map_at unimplemented")
end

# flisp: (define (parse-raw-literal s delim)
function parse_raw_literal(ps::ParseState, delim)
    TODO("parse_raw_literal unimplemented")
end

# flisp: (define (unescape-parsed-string-literal strs)
function unescape_parsed_string_literal(strs)
    TODO("unescape_parsed_string_literal unimplemented")
end

# flisp: (define (strip-escaped-newline s raw)
function strip_escaped_newline(s, raw)
    TODO("strip_escaped_newline unimplemented")
end

# remove `\` followed by a newline
#
# flisp: (define (strip-escaped-newline- s)
function strip_escaped_newline_(s)
    TODO("strip_escaped_newline_ unimplemented")
end

# flisp: (define (parse-string-literal s delim raw)
function parse_string_literal(ps::ParseState, delim, raw)
    TODO("parse_string_literal unimplemented")
end

# flisp: (define (strip-leading-newline s)
function strip_leading_newline(s)
    TODO("strip_leading_newline unimplemented")
end

# flisp: (define (dedent-triplequoted-string lst)
function dedent_triplequoted_string(lst)
    TODO("dedent_triplequoted_string unimplemented")
end

# flisp: (define (triplequoted-string-indentation lst)
function triplequoted_string_indentation(lst)
    TODO("triplequoted_string_indentation unimplemented")
end

# flisp: (define (triplequoted-string-indentation- s)
function triplequoted_string_indentation_(s)
    TODO("triplequoted_string_indentation_ unimplemented")
end

# return the longest common prefix of the elements of l
# e.g., (longest-common-prefix ((1 2) (1 4))) -> (1)
#
# flisp: (define (longest-common-prefix l)
function longest_common_prefix(l)
    TODO("longest_common_prefix unimplemented")
end

# return the longest common prefix of lists a & b
#
# flisp: (define (longest-common-prefix2 a b)
function longest_common_prefix2(a, b)
    TODO("longest_common_prefix2 unimplemented")
end

# flisp: (define (longest-common-prefix2- a b p)
function longest_common_prefix2_(a, b, p)
    TODO("longest_common_prefix2_ unimplemented")
end

# flisp: (define (string-split s sep)
function string_split(s, sep)
    TODO("string_split unimplemented")
end

# flisp: (define (string-split- s sep start splits)
function string_split_(s, sep, start, splits)
    TODO("string_split_ unimplemented")
end

# replace all occurrences of a in s with b
#
# flisp: (define (string-replace s a b)
function string_replace(s, a, b)
    TODO("string_replace unimplemented")
end

# flisp: (define (ends-interpolated-atom? c)
function is_ends_interpolated_atom(c)
    TODO("is_ends_interpolated_atom unimplemented")
end

# flisp: (define (parse-interpolate s)
function parse_interpolate(ps::ParseState)
    TODO("parse_interpolate unimplemented")
end

# raw = raw string literal
# when raw is #t, unescape only \\ and delimiter
# otherwise do full unescaping, and parse interpolations too
#
# flisp: (define (parse-string-literal- n p s delim raw)
function parse_string_literal_(n, p, s, delim, raw)
    TODO("parse_string_literal_ unimplemented")
end

# flisp: (define (not-eof-1 c)
function not_eof_1(c)
    TODO("not_eof_1 unimplemented")
end

# flisp: (define (unescape-string s)
function unescape_string_(s)
    TODO("unescape_string_ unimplemented")
end

# parse numbers, identifiers, parenthesized expressions, lists, vectors, etc.
#
# flisp: (define (parse-atom s (checked #t))
function parse_atom(ps::ParseState; checked=true)
    TODO("parse_atom unimplemented")
end

# flisp: (define (valid-modref? e)
function is_valid_modref(e)
    TODO("is_valid_modref unimplemented")
end

# flisp: (define (macroify-name e . suffixes)
function macroify_name(e, _, suffixes)
    TODO("macroify_name unimplemented")
end

# flisp: (define (macroify-call s call startloc)
function macroify_call(s, call, startloc)
    TODO("macroify_call unimplemented")
end

# flisp: (define (called-macro-name e)
function called_macro_name(e)
    TODO("called_macro_name unimplemented")
end

# flisp: (define (maybe-docstring s e)
function maybe_docstring(s, e)
    TODO("maybe_docstring unimplemented")
end

# flisp: (define (simple-string-literal? e) (string? e))
function is_simple_string_literal(e)
    TODO("is_simple_string_literal unimplemented")
end

# flisp: (define (doc-string-literal? s e)
function is_doc_string_literal(s, e)
    TODO("is_doc_string_literal unimplemented")
end

# Parse docstrings attached by a space or single newline
# flisp: (define (parse-docstring s production)
function parse_docstring(ps::ParseState, down=parse_eq)
    mark = position(ps)
    # TODO? This is not quite equivalent to the flisp parser which accepts
    # more than just a string. For example:
    #! ("doc") foo  ==>  (macrocall core_@doc "doc" foo)
    maybe_doc = peek(ps) in (K"String", K"TripleString")
    atdoc_mark = bump_invisible(ps)
    down(ps)
    if maybe_doc
        is_doc = true
        k = peek(ps)
        if is_closing_token(ps, k)
            is_doc = false
        elseif k == K"NewlineWs"
            k2 = peek(ps, 2)
            if is_closing_token(ps, k2) || k2 == K"NewlineWs"
                is_doc = false
            else
                # Allow a single newline
                # ===
                # "doc"
                # foo
                # ===> (macrocall core_@doc "doc" foo)
                bump(ps, TRIVIA_FLAG) # NewlineWs
            end
        end
        if is_doc
            reset_token!(ps, atdoc_mark, kind=K"core_@doc")
            down(ps)
            emit(ps, mark, K"macrocall")
        end
    end
end


"""
    parse_all(input)

Parse a sequence of top level statements.

`input` may be a `ParseStream` or other input source which will be passed to
the `ParseStream` constructor. The `ParseStream` is returned.

flisp: parse-all
"""
function parse_all(stream::ParseStream)
    ps = ParseState(stream)
    mark = position(ps)
    while true
        if peek(ps, skip_newlines=true) == K"EndMarker"
            # As a special case, allow early end of input if there is
            # nothing left but whitespace
            # ===
            # # a
            #
            # #= b =#  # c
            # ==> (toplevel)
            bump(ps, skip_newlines=true)
            break
        else
            parse_stmts(ps)
        end
    end
    emit(ps, mark, K"toplevel")
    return ps.stream
end

function parse_all(code, args...)
    stream = ParseStream(code)
    return parse_all(ParseState(stream), args...)
end

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------


#=

# Parse numbers, identifiers, parenthesized expressions, lists, vectors, etc.
function parse_atom(ps::ParseState; checked::Bool=true)::GreenNode
    tok = require_token(ps)
    tok_kind = kind(tok)
    # TODO: Reorder these to put most likely tokens first
    if tok_kind == K":" # symbol/expression quote
        take_token!(ps)
        next = peek_token(ps)
        if is_closing_token(ps, next) && (kind(next) != K"Keyword" ||
                                          has_whitespace_prefix(next))
            return GreenNode(tok)
        elseif has_whitespace_prefix(next)
            error("whitespace not allowed after \":\" used for quoting")
        elseif kind(next) == K"NewlineWs"
            error("newline not allowed after \":\" used for quoting")
        else
            # Being inside quote makes `end` non-special again. issue #27690
            ps1 = ParseState(ps, end_symbol=false)
            return GreenNode(K"quote", parse_atom(ps1, checked=false))
        end
    elseif tok_kind == K"=" # misplaced =
        error("unexpected `=`")
    elseif tok_kind == K"Identifier"
        if checked
            TODO("Checked identifier names")
        end
        take_token!(ps)
        return GreenNode(tok)
    elseif tok_kind == K"VarIdentifier"
        take_token!(ps)
        return GreenNode(tok)
    elseif tok_kind == K"(" # parens or tuple
        take_token!(ps)
        return parse_paren(ps, checked)
    elseif tok_kind == K"[" # cat expression
        # NB: Avoid take_token! here? It's better to not consume tokens early
        # take_token!(ps)
        vex = parse_cat(ps, tok, K"]", ps.end_symbol)
    elseif tok_kind == K"{" # cat expression
        take_token!(ps)
        TODO("""parse_cat(ps, K"}", )""")
    elseif tok_kind == K"`"
        TODO("(macrocall (core @cmd) ...)")
        # return Expr(:macrocall, Expr(:core, Symbol("@cmd")),
    elseif isliteral(tok_kind)
        take_token!(ps)
        return GreenNode(tok)
    elseif is_closing_token(tok)
        error("unexpected: $tok")
    else
        error("invalid syntax: `$tok`")
    end
end

#-------------------------------------------------------------------------------

=#

