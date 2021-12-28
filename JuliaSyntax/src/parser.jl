#-------------------------------------------------------------------------------
# Parser Utils

# Bump an expected closing token.  If not found, discard unexpected tokens
# until we find it or another closing token.
#
# Crude recovery heuristic: bump any tokens which aren't block or bracket
# closing tokens.
function bump_closing_token(ps, closing_kind)
    # TODO: Refactor with recover() ?
    bump_trivia(ps)
    if peek(ps) == closing_kind
        bump(ps, TRIVIA_FLAG)
        return
    end
    # We didn't find the closing token. Read ahead in the stream
    mark = position(ps)
    while true
        k = peek(ps)
        if is_closing_token(ps, k) && !(k in (K",", K";"))
            break
        end
        bump(ps)
    end
    # mark as trivia => ignore in AST.
    emit(ps, mark, K"error", TRIVIA_FLAG,
         error="Expected `$(untokenize(closing_kind))` but got unexpected tokens")
    if peek(ps) == closing_kind
        bump(ps, TRIVIA_FLAG)
    end
end

# Read tokens until we find an expected closing token.
# Bump the big pile of resulting tokens as a single nontrivia error token
function recover(is_closer::Function, ps, flags=EMPTY_FLAGS; mark = position(ps), error="unexpected tokens")
    while true
        k = peek(ps)
        if k == K"EndMarker"
            bump_invisible(ps, K"error", TRIVIA_FLAG,
                           error="premature end of input")
            break
        elseif is_closer(ps, k)
            break
        end
        bump(ps)
    end
    emit(ps, mark, K"error", flags, error=error)
end

# flisp: disallow-space
function bump_disallowed_space(ps)
    if peek_token(ps).had_whitespace
        bump_trivia(ps, skip_newlines=false, error="whitespace is not allowed here")
    end
end

function TODO(str)
    error("TODO: $str")
end

#-------------------------------------------------------------------------------
# Parsing-specific predicates on tokens/kinds
#
# All these take either a raw kind or a token.

function is_closing_token(ps::ParseState, k)
    k = kind(k)
    return k in (K"else", K"elseif", K"catch", K"finally",
                 K",", K")", K"]", K"}", K";",
                 K"EndMarker") || (k == K"end" && !ps.end_symbol)
end

function is_closer_or_newline(ps::ParseState, k)
    is_closing_token(ps,k) || k == K"NewlineWs"
end

# Closing token which isn't a keyword
function is_non_keyword_closer(k)
    kind(k) in (K",", K")", K"]", K"}", K";", K"EndMarker")
end

function is_initial_reserved_word(ps::ParseState, k)
    k = kind(k)
    is_iresword = k in (
        K"begin", K"while", K"if", K"for", K"try", K"return", K"break",
        K"continue", K"function", K"macro", K"quote", K"let", K"local",
        K"global", K"const", K"do", K"struct", K"module", K"baremodule",
        K"using", K"import", K"export")
    # `begin` means firstindex(a) inside a[...]
    return is_iresword && !(k == K"begin" && ps.end_symbol)
end

function is_contextural_keyword(k)
    kind(k) ∈ (K"mutable", K"primitive", K"abstract")
end

function is_reserved_word(k)
    k = kind(k)
    iskeyword(k) && !is_contextural_keyword(k)
end

# Return true if the next word (or word pair) is reserved, introducing a
# syntactic structure.
function peek_initial_reserved_words(ps::ParseState)
    k = peek(ps)
    if is_initial_reserved_word(ps, k)
        return true
    elseif is_contextural_keyword(k)
        k2 = peek(ps,2)
        return (k == K"mutable"   && k2 == K"struct") ||
               (k == K"primitive" && k2 == K"type")   ||
               (k == K"abstract"  && k2 == K"type")
    else
        return false
    end
end

function is_block_form(k)
    kind(k) in (K"block", K"quote", K"if", K"for", K"while",
                K"let", K"function", K"macro", K"abstract",
                K"primitive", K"struct", K"try", K"module")
end

function is_syntactic_operator(k)
    k = kind(k)
    return k in (K"&&", K"||", K".", K"...", K"->") ||
           (is_prec_assignment(k) && k != K"~")
end

function is_syntactic_unary_op(k)
    kind(k) in (K"$", K"&", K"::")
end

function is_type_operator(k)
    kind(k) in (K"<:", K">:")
end

function is_unary_op(k)
    kind(k) in (
        K"<:", K">:",  # TODO: dotop disallowed ?
        K"+", K"-", K"!", K"~", K"¬", K"√", K"∛", K"∜", K"⋆", K"±", K"∓" # dotop allowed
    )
end

# Operators which are both unary and binary
function is_both_unary_and_binary(k)
    # TODO: Do we need to check dotop as well here?
    kind(k) in (K"$", K"&", K"~",             # <- dotop disallowed?
                K"+", K"-", K"⋆", K"±", K"∓") # dotop allowed
end

# operators handled by parse_unary at the start of an expression
function is_initial_operator(k)
    k = kind(k)
    # TODO(jb): `?` should probably not be listed here except for the syntax hack in osutils.jl
    isoperator(k)                      &&
    !(k in (K":", K"'", K".'", K"?"))  &&
    !is_syntactic_unary_op(k)          &&
    !is_syntactic_operator(k)
end

# flisp: invalid-identifier?
function is_valid_identifier(k)
    k = kind(k)
    # TODO: flisp also had K"...." disallowed. But I don't know what that's
    # for! Tokenize doesn't have an equivalent here.
    !(is_syntactic_operator(k) || k in (K"?", K".'"))
end

#-------------------------------------------------------------------------------
# Parser
#
# The definitions and top-level comments here were copied to match the
# structure of Julia's official flisp-based parser.
#
# This is to make both codebases mutually understandable and make porting
# changes simple.


# parse left-to-right binary operator
# produces structures like (+ (+ (+ 2 3) 4) 5)
#
# flisp: parse-LtoR
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
#
# flisp: parse-RtoL
function parse_RtoL(ps::ParseState, down, is_op, syntactic, self)
    mark = position(ps)
    down(ps)
    k = peek(ps)
    if is_op(k)
        if syntactic isa Bool ? syntactic : syntactic(k)
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
# flisp: parse-Nary
function parse_Nary(ps::ParseState, down, delimiters, closing_tokens)
    bump_trivia(ps)
    k = peek(ps)
    if k in closing_tokens
        return true
    end
    # Skip leading delimiter
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
# flisp: parse-block
function parse_block(ps::ParseState, down=parse_eq, mark=position(ps),
                     consume_end=false)
    parse_block_inner(ps::ParseState, down)
    emit(ps, mark, K"block")
end

# Parse a block, but leave emitting the block up to the caller.
function parse_block_inner(ps::ParseState, down)
    parse_Nary(ps, down, (K"NewlineWs", K";"),
               (K"end", K"else", K"elseif", K"catch", K"finally"))
end

# ";" at the top level produces a sequence of top level expressions
#
# a;b;c   ==>  (toplevel a b c)
# a;;;b;; ==>  (toplevel a b)
#
# flisp: parse-stmts
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
        emit(ps, junk_mark, K"error",
             error="Extra tokens after end of expression")
    end
    if do_emit
        emit(ps, mark, K"toplevel")
    end
end

# flisp: parse-eq
function parse_eq(ps::ParseState)
    parse_assignment(ps, parse_comma, false)
end

# parse_eq_star is used where commas are special, for example in an argument list
#
# If an `(= x y)` node was emitted, returns the position of that node in the
# output list so that it can be changed to `(kw x y)` later if necessary.
#
# flisp: parse-eq*
function parse_eq_star(ps::ParseState, equals_is_kw=false)
    k = peek(ps)
    k2 = peek(ps,2)
    if (isliteral(k) || is_identifier(k)) && k2 in (K",", K")", K"}", K"]")
        # optimization: skip checking the whole precedence stack if we have a
        # simple token followed by a common closing token
        bump(ps)
        return NO_POSITION
    else
        return parse_assignment(ps, parse_pair, equals_is_kw)
    end
end

# flisp: eventually-call?
function is_eventually_call(ex)
    TODO("is_eventually_call unimplemented")
end

# a = b  ==>  (= a b)
#
# flisp: parse-assignment
function parse_assignment(ps::ParseState, down, equals_is_kw::Bool)
    mark = position(ps)
    down(ps)
    k = peek(ps)
    if !is_prec_assignment(k)
        return NO_POSITION
    end
    if k == K"~"
        bump(ps)
        if ps.space_sensitive # && ...
            # Prefix operator ~x ?
            TODO("parse_assignment... ~ not implemented")
        else
            # ~ is the only non-syntactic assignment-precedence operator.
            # a ~ b  ==>  (call-i a ~ b)
            parse_assignment(ps, down, equals_is_kw)
            emit(ps, mark, K"call", INFIX_FLAG)
        end
        return NO_POSITION
    else
        # a += b  ==>  (+= a b)
        # FIXME:
        # a .= b  ==>  (.= a b)
        bump(ps, TRIVIA_FLAG)
        parse_assignment(ps, down, equals_is_kw)
        result_k = (k == K"=" && equals_is_kw) ? K"kw" : k
        equals_pos = emit(ps, mark, result_k)
        return k == K"=" ? equals_pos : NO_POSITION
    end
end

# parse_comma is needed for commas outside parens, for example a = b,c
#
# flisp: parse-comma
function parse_comma(ps::ParseState, do_emit=true)
    mark = position(ps)
    n_commas = 0
    parse_pair(ps)
    while true
        if peek(ps) != K","
            if do_emit && n_commas >= 1
                # FIXME: is use of n_commas correct here? flisp comments say:
                # () => (tuple)
                # (ex2 ex1) => (tuple ex1 ex2)
                # (ex1,) => (tuple ex1)
                emit(ps, mark, K"tuple")
            end
            return n_commas
        end
        bump(ps, TRIVIA_FLAG)
        n_commas += 1
        if peek_token(ps) == K"="
            # Allow trailing comma before `=`
            # x, = xs  ==>  (tuple x)
            continue
        end
        parse_pair(ps)
    end
end

# flisp: parse-pair
function parse_pair(ps::ParseState)
    parse_RtoL(ps, parse_cond, is_prec_pair, false, parse_pair)
end

# Parse short form conditional expression
# a ? b : c ==> (if a b c)
#
# flisp: parse-cond
function parse_cond(ps::ParseState)
    mark = position(ps)
    parse_arrow(ps)
    t = peek_token(ps)
    if kind(t) != K"?"
        return
    end
    if !t.had_whitespace
        # a? b : c  => (if a (error-t) b c)
        bump_invisible(ps, K"error", TRIVIA_FLAG,
                       error="space required before `?` operator")
    end
    bump(ps, TRIVIA_FLAG) # ?
    t = peek_token(ps)
    if !t.had_whitespace
        # a ?b : c
        bump_invisible(ps, K"error", TRIVIA_FLAG,
                       error="space required after `?` operator")
    end
    parse_eq_star(ParseState(ps, range_colon_enabled=false))
    t = peek_token(ps)
    if !t.had_whitespace
        # a ? b: c  ==>  (if a [ ] [?] [ ] b (error-t) [:] [ ] c)
        bump_invisible(ps, K"error", TRIVIA_FLAG,
                       error="space required before `:` in `?` expression")
    end
    if kind(t) == K":"
        bump(ps, TRIVIA_FLAG)
    else
        # a ? b c  ==>  (if a b (error) c)
        bump_invisible(ps, K"error", TRIVIA_FLAG, error="`:` expected in `?` expression")
    end
    t = peek_token(ps)
    if !t.had_whitespace
        # a ? b :c  ==>  (if a [ ] [?] [ ] b [ ] [:] (error-t) c)
        bump_invisible(ps, K"error", TRIVIA_FLAG,
                       error="space required after `:` in `?` expression")
    end
    parse_eq_star(ps)
    emit(ps, mark, K"if")
end

# Parse arrows
# x → y     ==>  (call-i x → y)
# x <--> y  ==>  (call-i x <--> y)
# x --> y   ==>  (--> x y)           # The only syntactic arrow
#
# flisp: parse-arrow
function parse_arrow(ps::ParseState)
    parse_RtoL(ps, parse_or, is_prec_arrow, ==(K"-->"), parse_arrow)
end

# x || y || z   ==>   (|| x (|| y z))
#
# flisp: parse-or
function parse_or(ps::ParseState)
    parse_RtoL(ps, parse_and, is_prec_lazy_or, true, parse_or)
end

# x && y && z   ==>   (&& x (&& y z))
#
# flisp: parse-and
function parse_and(ps::ParseState)
    parse_RtoL(ps, parse_comparison, is_prec_lazy_and, true, parse_and)
end

# Parse comparison chains like
# x > y        ==> (call-i x > y)
# x < y < z    ==> (comparison x < y < z)
# x == y < z   ==> (comparison x == y < z)
#
# flisp: parse-comparison
function parse_comparison(ps::ParseState)
    mark = position(ps)
    parse_pipe_lt(ps)
    n_comparisons = 0
    op_pos = NO_POSITION
    initial_kind = peek(ps)
    while is_prec_comparison(peek(ps))
        n_comparisons += 1
        op_pos = bump(ps)
        parse_pipe_lt(ps)
    end
    if n_comparisons == 1
        if is_type_operator(initial_kind)
            # Type comparisons are syntactic
            # x <: y  ==>  (<: x y)
            # x >: y  ==>  (>: x y)
            reset_node!(ps, op_pos, flags=TRIVIA_FLAG)
            emit(ps, mark, initial_kind)
        else
            emit(ps, mark, K"call", INFIX_FLAG)
        end
    elseif n_comparisons > 1
        emit(ps, mark, K"comparison")
    end
end

# x <| y <| z  ==>  (call-i x <| (call-i y <| z))
# flisp: parse-pipe<
function parse_pipe_lt(ps::ParseState)
    parse_RtoL(ps, parse_pipe_gt, is_prec_pipe_lt, false, parse_pipe_lt)
end

# x |> y |> z  ==>  (call-i (call-i x |> y) |> z)
# flisp: parse-pipe>
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
# flisp: parse-range
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
            t2 = peek_token(ps,2)
            if kind(t2) in (K"<", K">") && !t2.had_whitespace
                # Error heuristic: we found `:>` or `:<` which are invalid lookalikes
                # for `<:` and `>:`. Attempt to recover by treating them as a
                # comparison operator.
                # a :> b   ==>  (call-i a (error : >) b)
                bump_trivia(ps, skip_newlines=false)
                emark = position(ps)
                bump(ps) # K":"
                ks = untokenize(peek(ps))
                bump(ps) # K"<" or K">"
                emit(ps, emark, K"error",
                     error="Invalid `:$ks` found, maybe replace with `$ks:`")
                parse_expr(ps)
                emit(ps, mark, K"call", INFIX_FLAG)
                break
            end
            n_colons += 1
            bump(ps, n_colons == 1 ? EMPTY_FLAGS : TRIVIA_FLAG)
            t = peek_token(ps)
            if is_closing_token(ps, kind(t))
                # 1: }    ==>  (call-i 1 : (error))
                # 1:2: }  ==>  (call-i 1 : 2 (error))
                bump_invisible(ps, K"error",
                               error="missing last argument in range expression")
                emit(ps, mark, K"call", INFIX_FLAG)
                emit_diagnostic(ps, error="found unexpected closing token")
                return
            end
            if t.had_newline
                # Error message for people coming from python
                # ===
                # 1:
                # 2
                # ==> (call-i 1 : (error))
                emit_diagnostic(ps, whitespace=true,
                                error="line break after `:` in range expression")
                bump_invisible(ps, K"error")
                emit(ps, mark, K"call", INFIX_FLAG)
                return
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
# flisp: parse-chain
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
            parse_chain(ps, down, kind(t))
        end
        # a +₁ b +₁ c  ==>  (call-i (call-i a +₁ b) +₁ c)
        # a .+ b .+ c  ==>  (call-i (call-i a .+ b) .+ c)
        emit(ps, mark, K"call", INFIX_FLAG)
    end
end

# a - b - c  ==>  (call-i (call-i a - b) - c)
# a + b + c  ==>  (call-i a + b c)
#
# flisp: parse-expr
function parse_expr(ps::ParseState)
    parse_with_chains(ps, parse_term, is_prec_plus, (K"+", K"++"))
end

# a * b * c  ==>  (call-i a * b c)
#
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
    k = peek(ps, skip_newlines=true)
    if k == K"EndMarker"
        parse_atom(ps)
        return 
    elseif k in (K"<:", K">:")
        # FIXME add test cases
        k2 = peek(ps, 2)
        if is_closing_token(ps, k2) || k2 in (K"NewlineWs", K"=")
            # return operator by itself, as in (<:)
            bump(ps)
            return
        end
        if k2 in (K"{", K"(")
            # parse <:{T}(x::T) or <:(x::T) like other unary operators
            parse_where(ps, parse_juxtapose)
        else
            TODO("parse_unary_subtype")
            parse_where(ps, parse_juxtapose)
            if peek_behind(ps) == K"tuple"
                # Argh
            end
        end
    else
        parse_where(ps, parse_juxtapose)
    end
end

# flisp: parse-where-chain
function parse_where_chain(ps0::ParseState, mark)
    ps = ParseState(ps0, where_enabled=false)
    while peek(ps) == K"where"
        bump(ps, TRIVIA_FLAG) # where
        k = peek(ps)
        if k == K"{"
            # x where {T,S}  ==>  (where x T S)
            ckind, cflags = parse_cat(ps, K"}", ps.end_symbol)
            if ckind != K"vect"
                # Various nonsensical forms permitted here
                # x where {T S}  ==>  (where x (bracescat (row T S)))
                # x where {y for y in ys}  ==>  (where x (braces (generator y (= y ys))))
                emit_braces(ps, mark, ckind, cflags)
            end
            emit(ps, mark, K"where")
        else
            parse_comparison(ps)
            emit(ps, mark, K"where")
        end
    end
end

# flisp: parse-where
function parse_where(ps::ParseState, down)
    # `where` needs to be below unary for the following to work
    # +(x::T,y::T) where {T} = x
    mark = position(ps)
    down(ps)
    if ps.where_enabled && peek(ps) == K"where"
        parse_where_chain(ps, mark)
    end
end

# given the previous expression kind and the next token, is there a
# juxtaposition operator between them?
#
# flisp: juxtapose?
function is_juxtapose(ps, prev_k, t)
    k = kind(t)

    return !t.had_whitespace                         &&
    (is_number(prev_k) ||
        (!is_number(k) &&  # disallow "x.3" and "sqrt(2)2"
         k != K"@"     &&  # disallow "x@time"
         !(is_block_form(prev_k)         ||
           is_syntactic_unary_op(prev_k) ||
           is_initial_reserved_word(ps, prev_k) )))  &&
    (!isoperator(k) || is_radical_op(k))             &&
    !is_closing_token(ps, k)                         &&
    !is_initial_reserved_word(ps, k)
end

# Juxtoposition. Ugh!
#
# 2x       ==>  (call-i 2 * x)
# 2(x)     ==>  (call-i 2 * x)
# (2)(3)x  ==>  (call-i 2 * 3 x)
# (x-1)y   ==>  (call-i (call-i x - 1) * y)
#
# flisp: parse-juxtapose
function parse_juxtapose(ps::ParseState)
    mark = position(ps)
    parse_unary(ps)
    n_terms = 1
    while true
        prev_kind = peek_behind(ps)
        t = peek_token(ps)
        if !is_juxtapose(ps, prev_kind, t)
            break
        end
        if n_terms == 1
            bump_invisible(ps, K"*")
        end
        if is_string(prev_kind) || is_string(t)
            # issue #20575
            #
            # "a""b"  ==>  (call-i "a" * (error) "b")
            # "a"x    ==>  (call-i "a" * (error) x)
            bump_invisible(ps, K"error", TRIVIA_FLAG,
                           error="cannot juxtapose string literal")
        end
        if is_radical_op(t)
            parse_unary(ps)
        else
            parse_factor(ps)
        end
        n_terms += 1
    end
    if n_terms > 1
        emit(ps, mark, K"call", INFIX_FLAG)
    end
end

# Deal with numeric literal prefixes and unary calls
#
# flisp: parse-unary
function parse_unary(ps::ParseState)
    mark = position(ps)
    bump_trivia(ps)
    k = peek(ps)
    if !is_initial_operator(k)
        parse_factor(ps)
        return
    end
    if k in (K"-", K"+")
        t2 = peek_token(ps, 2)
        if !t2.had_whitespace && kind(t2) in (K"Integer", K"Float")
            k3 = peek(ps, 3)
            if is_prec_power(k3) || k3 in (K"[", K"{")
                # `[`, `{` (issue #18851) and `^` have higher precedence than
                # unary negation
                # -2^x      ==>  (call - (call-i 2 ^ x))
                # -2[1, 3]  ==>  (call - (ref 2 1 3))
                bump(ps)
                parse_factor(ps)
                emit(ps, mark, K"call")
            else
                # We have a signed numeric literal. Glue the operator to the
                # next token to create a signed literal:
                # +2    ==>  +2
                # -2*x  ==>  (call-i -2 * x)
                bump_glue(ps, kind(t2), EMPTY_FLAGS, 2)
            end
            return
        end
    end
    parse_unary_call(ps)
end

# Parse calls to unary operators and prefix calls involving arbitrary operators
# with bracketed arglists (as opposed to infix notation)
#
# +a      ==>  (call + a)
# +(a,b)  ==>  (call + a b)
#
# flisp: parse-unary-call
function parse_unary_call(ps::ParseState)
    mark = position(ps)
    op_t = peek_token(ps)
    op_k = kind(op_t)
    op_node_kind = is_type_operator(op_k) ? op_k : K"call"
    op_tok_flags = is_type_operator(op_t) ? TRIVIA_FLAG : EMPTY_FLAGS
    t2 = peek_token(ps, 2)
    k2 = kind(t2)
    if is_closing_token(ps, k2) || k2 in (K"NewlineWs", K"=")
        if is_dotted(op_t)
            # standalone dotted operators are parsed as (|.| op)
            # .+  ==>  (. +)
            bump_trivia(ps)
            bump_split(ps, 1,
                       K".", TRIVIA_FLAG,
                       op_k, EMPTY_FLAGS)
            emit(ps, mark, K".")
        else
            # return operator by itself, as in
            # (+)  ==>  +
            bump(ps)
        end
    elseif k2 == K"{" || (!is_unary_op(op_k) && k2 == K"(")
        # this case is +{T}(x::T) = ...
        parse_factor(ps)
    elseif k2 == K"("
        # Cases like +(a,b) and +(a)
        #
        # Bump the operator
        bump(ps, op_tok_flags)

        # Setup possible whitespace error between operator and (
        ws_mark = position(ps)
        bump_trivia(ps)
        ws_mark_end = position(ps) # FIXME - 1
        ws_error_pos = emit(ps, ws_mark, K"TOMBSTONE")

        mark_before_paren = position(ps)
        bump(ps, TRIVIA_FLAG) # (
        # There's two tricky parts for unary-prefixed parenthesized expressions
        # like `+(a,b)`
        #
        # 1. The ambiguity between a function call arglist or a block. The
        #    flisp parser resolves in favor of a block if there's no initial
        #    commas before semicolons:
        #
        #    Function calls:
        #    +(a,b)   ==>  (call + a b)
        #    +(a=1,)  ==>  (call + (kw a 1))
        #
        #    Not function calls:
        #    +(a;b)   ==>  (call + (block a b))
        #    +(a=1)   ==>  (call + (= a 1))
        #
        # But this heuristic fails in some cases so here we use a simpler rule:
        # if there were any commas, it was a function call. Then we also parse
        # things like the following in a useful way:
        #
        #    +(a;b,c)  ==>  (call + (tuple a (parameters b c)))
        #
        is_call = false
        is_block = false
        parse_brackets(ps, K")") do had_commas,  num_semis, num_subexprs
            is_call = had_commas
            is_block = !is_call && num_semis > 0
            bump_closing_token(ps, K")")
            return (needs_parameters=is_call,
                    eq_is_kw_before_semi=is_call,
                    eq_is_kw_after_semi=is_call)
        end

        if is_call && t2.had_whitespace
            reset_node!(ps, ws_error_pos, kind=K"error")
            emit_diagnostic(ps, ws_mark, ws_mark_end,
                error="whitespace not allowed between prefix function call and argument list")
        end

        # 2. The precedence between unary + and any following infix ^ depends
        #    on whether the parens are a function call or not
        if is_call
            # Prefix operator call
            # +(a,b)^2  ==>  (call-i (call + a b) ^ 2)
            emit(ps, mark, op_node_kind)
            parse_factor_with_initial_ex(ps, mark)
        else
            if is_block
                emit(ps, mark_before_paren, K"block")
            end
            # Not a prefix operator call
            # +(a)^2    ==>  (call + (call-i ^ a 2))
            parse_factor_with_initial_ex(ps, mark_before_paren)
            emit(ps, mark, op_node_kind)
        end
    elseif !is_unary_op(op_k)
        emit_diagnostic(ps, error="expected a unary operator")
    else
        bump(ps, op_tok_flags)
        parse_unary(ps)
        emit(ps, mark, op_node_kind)
    end
end

# handle ^ and .^
# -2^3 is parsed as -(2^3), so call parse-decl for the first argument,
# and parse-unary from then on (to handle 2^-3)
#
# flisp: parse-factor
function parse_factor(ps::ParseState)
    if peek_initial_reserved_words(ps)
        parse_resword(ps)
    else
        mark = position(ps)
        parse_unary_prefix(ps)
        parse_factor_with_initial_ex(ps, mark)
    end
end

# flisp: parse-factor-with-initial-ex
function parse_factor_with_initial_ex(ps::ParseState, mark)
    parse_call_chain(ps, mark)
    parse_decl_with_initial_ex(ps, mark)
    if is_prec_power(peek(ps))
        bump(ps)
        parse_factor_after(ps)
        emit(ps, mark, K"call", INFIX_FLAG)
    end
end

# flisp: parse-factor-after
function parse_factor_after(ps::ParseState)
    parse_RtoL(ps, parse_juxtapose, is_prec_power, false, parse_factor_after)
end

# Parse type declarations and lambda syntax
# a::b      ==>   (:: a b)
# a->b      ==>   (-> a b)
#
# flisp: parse-decl
function parse_decl(ps::ParseState)
    mark = position(ps)
    parse_call(ps)
    parse_decl_with_initial_ex(ps, mark)
end

# flisp: parse-decl-with-initial-ex
function parse_decl_with_initial_ex(ps::ParseState, mark)
    while peek(ps) == K"::"
        # a::b::c   ==>   (:: (:: a b) c)
        bump(ps, TRIVIA_FLAG)
        parse_where(ps, parse_call)
        emit(ps, mark, K"::")
    end
    if peek(ps) == K"->"
        # a::b->c   ==>   (-> (:: a b) c)
        bump(ps, TRIVIA_FLAG)
        # -> is unusual: it binds tightly on the left and
        # loosely on the right.
        parse_eq_star(ps)
        emit(ps, mark, K"->")
    end
end

# parse function call, indexing, dot, and transpose expressions
# also handles looking for syntactic reserved words
#
# flisp: parse-call
function parse_call(ps::ParseState)
    if peek_initial_reserved_words(ps)
        parse_resword(ps)
    else
        mark = position(ps)
        # f(x)   ==>  (call f x)
        # $f(x)  ==>  (call ($ f) x)
        parse_unary_prefix(ps)
        parse_call_chain(ps, mark)
    end
end

# parse syntactic unary operators
#
# &a   ==>  (& a)
# ::a  ==>  (:: a)
# $a   ==>  ($ a)
#
# flisp: parse-unary-prefix
function parse_unary_prefix(ps::ParseState)
    mark = position(ps)
    k = peek(ps)
    if is_syntactic_unary_op(k)
        k2 = peek(ps, 2)
        if k in (K"&", K"$") && (is_closing_token(ps, k2) || k2 == K"NewlineWs")
            # &)   ==>  &
            # $\n  ==>  $
            bump(ps)
        else
            bump(ps, TRIVIA_FLAG)
            if k in (K"&", K"::")
                # &a   ==>  (& a)
                parse_where(ps, parse_call)
            else
                # $a   ==>  ($ a)
                # $$a  ==>  ($ ($ a))
                # $&a  ==>  ($ (& a))
                parse_unary_prefix(ps)
            end
            emit(ps, mark, k)
        end
    else
        parse_atom(ps)
    end
end

# Parse a symbol or interpolation syntax (a restricted version of
# parse_unary_prefix)
function parse_identifier_or_interpolate(ps::ParseState, outermost=true)
    mark = position(ps)
    if peek(ps) == K"$"
        bump(ps, TRIVIA_FLAG)
        # $a   ==>  ($ a)
        # $$a  ==>  ($ ($ a))
        parse_identifier_or_interpolate(ps, false)
        emit(ps, mark, K"$")
    else
        parse_atom(ps)
        if outermost && !is_identifier(peek_behind(ps))
            emit(ps, mark, K"error",
                 error="Expected identifier or interpolation syntax")
        end
    end
end

function parse_export_symbol(ps::ParseState)
    bump_trivia(ps)
    if peek(ps) == K"@"
        # export @a  ==>  (export @a)
        # export a, \n @b  ==>  (export a @b)
        bump(ps, TRIVIA_FLAG)
        parse_macro_name(ps, remap_kind=true)
    else
        # export a  ==>  (export a)
        # export \n a  ==>  (export a)
        # export $a, $(a*b)  ==>  (export ($ a) ($ (call * a b)))
        parse_identifier_or_interpolate(ps)
    end
end

# Emit an error if the call chain syntax is not a valid module reference
function emit_modref_error(ps, mark)
    emit(ps, mark, K"error", error="not a valid module reference")
end

function finish_macroname(ps, mark, is_valid_modref, macro_name_position,
                          name_kind=nothing)
    if is_valid_modref
        if isnothing(name_kind)
            name_kind = macro_name_kind(peek_behind(ps, macro_name_position))
        end
        reset_node!(ps, macro_name_position, kind = name_kind)
    else
        emit(ps, mark, K"error", error="not a valid module reference")
    end
end

# Parses a chain of sufficies at function call precedence, leftmost binding
# tightest.
# f(a,b)    ==> (call f a b)
# f(a).g(b) ==> (call (. (call f a) (quote g)) b)
#
# flisp: parse-call-chain, parse-call-with-initial-ex
function parse_call_chain(ps::ParseState, mark, is_macrocall=false)
    if is_number(peek_behind(ps)) && peek(ps) == K"("
        # juxtaposition with numbers is multiply, not call
        # 2(x) ==> (* 2 x)
        return
    end
    # source range of the @-prefixed part of a macro
    macro_atname_range = nothing
    kb = peek_behind(ps)
    is_valid_modref = is_identifier(kb) || kb == K"."
    # We record the last component of chains of dot-separated identifiers so we
    # know which identifier was the macro name.
    macro_name_position = position(ps) # points to same output span as peek_behind
    while true
        this_iter_valid_modref = false
        t = peek_token(ps)
        k = kind(t)
        if (ps.space_sensitive && t.had_whitespace &&
                # TODO: Is `'` adjoint or Char here?
                k in (K"(", K"[", K"{", K"\\", K"'", K"Char", K"String", K"TripleString"))
            break
        end
        if k == K"("
            if is_macrocall
                # a().@x(y)  ==> (macrocall (error (. (call a) (quote x))) y)
                finish_macroname(ps, mark, is_valid_modref, macro_name_position)
            end
            # f(a,b)  ==>  (call f a b)
            bump_disallowed_space(ps)
            bump(ps, TRIVIA_FLAG)
            # Keyword arguments depends on call vs macrocall
            #  foo(a=1)  ==>  (call foo (kw a 1))
            # @foo(a=1)  ==>  (macrocall @foo (= a 1))
            parse_call_arglist(ps, K")", is_macrocall)
            emit(ps, mark, is_macrocall ? K"macrocall" : K"call")
            if peek(ps) == K"do"
                # f(x) do y body end  ==>  (do (call :f :x) (-> (tuple :y) (block :body)))
                bump(ps, TRIVIA_FLAG)
                parse_do(ps)
                emit(ps, mark, K"do")
            end
            if is_macrocall
                break
            end
        elseif is_macrocall && (t.had_whitespace || is_closing_token(ps, k))
            # a().@x y  ==> (macrocall (error (. (call a) (quote x))) y)
            finish_macroname(ps, mark, is_valid_modref, macro_name_position)
            with_space_sensitive(ps) do ps
                # Space separated macro arguments
                # @foo a b      ==> (macrocall @foo a b)
                # A.@foo a b    ==> (macrocall (. A (quote @foo)) a b)
                # @A.foo a b    ==> (macrocall (. A (quote @foo)) a b)
                n_args = parse_space_separated_exprs(ps)
                is_doc_macro = peek_behind_str(ps, macro_name_position, "doc")
                if is_doc_macro && n_args == 1
                    # Parse extended @doc args on next line
                    # @doc x\ny      ==>  (macrocall @doc x y)
                    # A.@doc x\ny    ==>  (macrocall (. A (quote @doc)) doc x y)
                    # @A.doc x\ny    ==>  (macrocall (. A (quote @doc)) doc x y)
                    # @doc x y\nz    ==>  (macrocall @doc x y)
                    #
                    # Excluded cases
                    # @doc x\n\ny    ==>  (macrocall @doc x)
                    # @doc x\nend    ==>  (macrocall @doc x)
                    k2 = peek(ps, 2)
                    if peek(ps) == K"NewlineWs" && !is_closing_token(ps, k2) &&
                            k2 != K"NewlineWs"
                        bump(ps) # newline
                        parse_eq(ps)
                    end
                end
                emit(ps, mark, K"macrocall")
            end
            break
        elseif k == K"["
            if is_macrocall
                # a().@x[1]  ==> FIXME
                finish_macroname(ps, mark, is_valid_modref, macro_name_position)
            end
            bump_disallowed_space(ps)
            bump(ps, TRIVIA_FLAG)
            ckind, cflags = parse_cat(ParseState(ps, end_symbol=true),
                                      K"]", ps.end_symbol)
            # a[i]    ==>  (ref a i)
            # a[i,j]  ==>  (ref a i j)
            # T[x for x in xs]  ==>  (typed_comprehension T (generator x (= x xs)))
            # TODO: other test cases
            outk = ckind == K"vect"          ? K"ref"                  :
                   ckind == K"hcat"          ? K"typed_hcat"           :
                   ckind == K"vcat"          ? K"typed_vcat"           :
                   ckind == K"comprehension" ? K"typed_comprehension"  :
                   ckind == K"ncat"          ? K"typed_ncat"           :
                   error("Unrecognized kind in parse_cat")
            emit(ps, mark, outk, cflags)
            if is_macrocall
                emit(ps, mark, K"macrocall")
                break
            end
        elseif k == K"."
            bump_disallowed_space(ps)
            if peek(ps, 2) == K"'"
                emark = position(ps)
                bump(ps)
                bump(ps)
                "f.'"  =>  "f (error . ')"
                emit(ps, emark, K"error", TRIVIA_FLAG,
                     error="the .' operator is discontinued")
                is_valid_modref = false
                continue
            end
            if !isnothing(macro_atname_range)
                # Allow `@` in macrocall only in first and last position
                # A.B.@x  ==>  (macrocall (. (. A (quote B)) (quote @x)))
                # @A.B.x  ==>  (macrocall (. (. A (quote B)) (quote @x)))
                # A.@B.x  ==>  (macrocall (. (. A (error) B) (quote @x)))
                emit_diagnostic(ps, macro_atname_range,
                    error="`@` must appear on first or last macro name component")
                bump(ps, TRIVIA_FLAG, error="Unexpected `.` after macro name")
            else
                bump(ps, TRIVIA_FLAG)
            end
            k = peek(ps)
            if k == K"("
                if is_macrocall
                    bump_invisible(ps, K"error")
                    emit_diagnostic(ps, mark,
                                    error="dot call syntax not supported for macros")
                end
                # Keyword params always use kw inside tuple in dot calls
                # f.(a,b)   ==>  (. f (tuple a b))
                # f.(a=1)   ==>  (. f (tuple (kw a 1)))
                bump_disallowed_space(ps)
                m = position(ps)
                bump(ps, TRIVIA_FLAG)
                parse_call_arglist(ps, K")", is_macrocall)
                emit(ps, m, K"tuple")
                emit(ps, mark, K".")
            elseif k == K":"
                # A.:+  ==>  (. A (quote +))
                m = position(ps)
                bump(ps, TRIVIA_FLAG)
                bump_disallowed_space(ps)
                parse_atom(ps, false)
                emit(ps, m, K"quote")
                emit(ps, mark, K".")
            elseif k == K"$"
                # f.$x      ==>  (. f (inert ($ x)))
                # f.$(x+y)  ==>  (. f (inert ($ (call + x y))))
                m = position(ps)
                bump(ps, TRIVIA_FLAG)
                parse_atom(ps)
                emit(ps, m, K"$")
                emit(ps, m, K"inert")
                emit(ps, mark, K".")
                # Syntax extension: We could allow interpolations like A.$B.@C
                # to parse in the module reference path. But disallow this for
                # now for simplicity and for compatibility with the flisp parser.
            elseif k == K"@"
                # A macro call after some prefix A has been consumed
                # A.@x    ==>  (macrocall (. A (quote @x)))
                # A.@x a  ==>  (macrocall (. A (quote @x)) a)
                m = position(ps)
                if is_macrocall
                    # @A.B.@x a  ==>  (macrocall (error (. A (quote x))) a)
                    bump(ps, TRIVIA_FLAG, error="repeated `@` in macro module path")
                else
                    bump(ps, TRIVIA_FLAG)
                    is_macrocall = true
                end
                parse_macro_name(ps)
                macro_name_position = position(ps)
                macro_atname_range = (m, macro_name_position)
                emit(ps, m, K"quote")
                emit(ps, mark, K".")
                this_iter_valid_modref = true
            else
                # Field/property syntax
                # f.x.y ==> (. (. f (quote x)) (quote y))
                m = position(ps)
                parse_atom(ps, false)
                macro_name_position = position(ps)
                emit(ps, m, K"quote")
                emit(ps, mark, K".")
                this_iter_valid_modref = true
            end
        elseif k == K"'"
            if !is_suffixed(t)
                # f'  ==> (' f)
                bump(ps, TRIVIA_FLAG)
                emit(ps, mark, k)
            else
                # f'ᵀ ==> (call 'ᵀ f)
                bump(ps)
                emit(ps, mark, K"call", INFIX_FLAG)
            end
        elseif k == K"{"
            # Type parameter curlies and macro calls
            if is_macrocall
                # a().@x{y}  ==> (macrocall (error (. (call a) (quote x))) (braces y))
                finish_macroname(ps, mark, is_valid_modref, macro_name_position)
            end
            m = position(ps)
            bump_disallowed_space(ps)
            bump(ps, TRIVIA_FLAG)
            parse_call_arglist(ps, K"}", is_macrocall)
            if is_macrocall
                # @S{a,b} ==> (macrocall S (braces a b))
                emit(ps, m, K"braces")
                emit(ps, mark, K"macrocall")
                # Extension
                #if ps.julia_version < v"1.5"
                #    emit(ps, mark, K"error",
                #         error="", min_version=v"1.5")
                #end
                break
            else
                # S{a,b} ==> (curly S a b)
                emit(ps, mark, K"curly")
            end
        elseif k in (K"String", K"TripleString", K"Cmd", K"TripleCmd") &&
                !t.had_whitespace && is_valid_modref
            # Custom string and command literals
            # x"str" ==> (macrocall x_str "str")
            # x`str` ==> (macrocall x_cmd "str")

            # Use a special token kind for string and cmd macro names so the
            # names can be expanded later as necessary.
            mackind = is_string(k) ? K"StringMacroName" : K"CmdMacroName"
            finish_macroname(ps, mark, is_valid_modref, macro_name_position, mackind)
            bump(ps)
            t = peek_token(ps)
            k = kind(t)
            if !t.had_whitespace && (k == K"Identifier" || iskeyword(k) || is_number(k))
                # Macro sufficies can include keywords and numbers
                # x"s"y ==> (macrocall x_str "s" "y")
                # x"s"end ==> (macrocall x_str "s" "end")
                # x"s"2 ==> (macrocall x_str "s" 2)
                # x"s"10.0 ==> (macrocall x_str "s" 10.0)
                suffix_kind = (k == K"Identifier" || iskeyword(k)) ?
                              K"UnquotedString" : k
                bump(ps, remap_kind=suffix_kind)
            end
            emit(ps, mark, K"macrocall")
        else
            break
        end
        is_valid_modref &= this_iter_valid_modref
    end
end

# flisp: parse-subtype-spec
function parse_subtype_spec(ps::ParseState)
    k = peek(ps)
    if is_reserved_word(k)
        # Recovery
        # struct try end  ==>  (struct false (error try) (block))
        bump(ps, error="Invalid type name `$(untokenize(k))`")
        m = position(ps)
        if is_prec_comparison(peek(ps))
            bump(ps)
            parse_pipe_lt(ps)
            emit(ps, m, K"error", TRIVIA_FLAG)
        end
    else
        # Wart: why isn't the flisp parser more strict here?
        # <: is the only operator which isn't a syntax error, but
        # parse_subtype_spec allows all sorts of things.
        parse_comparison(ps)
    end
end

# Parse struct definitions. The caller must arrange for the next tokens to be
# `struct` or `mutable struct`.
#
# flisp: parse-struct-def
function parse_struct_def(ps::ParseState)
    mark = position(ps)
    is_mutable = peek(ps) == K"mutable"
    if is_mutable
        bump(ps, TRIVIA_FLAG)
    end
    @assert peek(ps) == K"struct"
    bump(ps, TRIVIA_FLAG)
    k = peek(ps)
    if is_reserved_word(k)
        bump(ps, error="Invalid type name `$(untokenize(k))`")
    end
end

# parse expressions or blocks introduced by syntactic reserved words.
#
# The caller should use peek_initial_reserved_words to determine whether
# to call parse_resword, or whether contextural keywords like `mutable` are
# simple identifiers.
#
# flisp: parse-resword
function parse_resword(ps::ParseState)
    ps = normal_context(ps)
    mark = position(ps)
    word = peek(ps)
    if word in (K"begin", K"quote")
        # begin end         ==>  (block)
        # begin a ; b end   ==>  (block a b)
        # begin\na\nb\nend  ==>  (block a b)
        bump(ps, TRIVIA_FLAG)
        parse_block_inner(ps, parse_docstring)
        bump_closing_token(ps, K"end")
        emit(ps, mark, K"block")
        if word == K"quote"
            # quote end       ==>  (quote (block))
            # quote body end  ==>  (quote (block body))
            emit(ps, mark, K"quote")
        end
    elseif word == K"while"
        # while cond body end  ==>  (while cond (block body))
        # ===
        # while x < y
        #     a
        #     b
        # end
        # ==> (while (call < x y) (block a b))
        bump(ps, TRIVIA_FLAG)
        parse_cond(ps)
        parse_block(ps)
        bump_closing_token(ps, K"end")
        emit(ps, mark, K"while")
    elseif word == K"for"
        # for x in xs end  ==>  (for (= x xs) (block))
        # ===
        # for x in xs, y in ys
        #     a
        #     b
        # end
        # ==> (for (block (= x xs) (= y ys)) (block a b))
        bump(ps, TRIVIA_FLAG)
        m = position(ps)
        n_subexprs = parse_comma_separated(ps, parse_iteration_spec)
        if n_subexprs > 1
            emit(ps, m, K"block")
        end
        parse_block(ps)
        bump_closing_token(ps, K"end")
        emit(ps, mark, K"for")
    elseif word == K"let"
        bump(ps, TRIVIA_FLAG)
        if peek(ps) ∉ (K"NewlineWs", K";")
            # let x=1\n end    ==>  (let (= x 1) (block))
            m = position(ps)
            n_subexprs = parse_comma_separated(ps, parse_eq_star)
            kb = peek_behind(ps)
            # Wart: This ugly logic seems unfortunate. Why not always emit a block?
            # let x=1 ; end   ==>  (let (= x 1) (block))
            # let x::1 ; end  ==>  (let (:: x 1) (block))
            # let x ; end     ==>  (let x (block))
            if n_subexprs > 1 || !(kb in (K"Identifier", K"=", K"::"))
                # let x=1,y=2 ; end  ==>  (let (block (= x 1) (= y 2) (block)))
                # let x+=1 ; end     ==>  (let (block (+= x 1)) (block))
                emit(ps, m, K"block")
            end
        else
            # let end           ==>  (let (block) (block))
            # let ; end         ==>  (let (block) (block))
            # let ; body end    ==>  (let (block) (block body))
            bump_invisible(ps, K"block")
        end
        k = peek(ps)
        if k in (K"NewlineWs", K";")
            bump(ps, TRIVIA_FLAG)
        elseif k == K"end"
            # pass
        else
            recover(is_closer_or_newline, ps, TRIVIA_FLAG,
                    error="let variables should end in `;` or newline")
        end
        # let\na\nb\nend    ==>  (let (block) (block a b))
        parse_block(ps)
        bump_closing_token(ps, K"end")
        emit(ps, mark, K"let")
    elseif word == K"if"
        parse_if_elseif(ps)
    elseif word in (K"const", K"global", K"local")
        parse_const_local_global(ps)
    elseif word in (K"function", K"macro")
        parse_function(ps)
    elseif word == K"abstract"
        # Abstract type definitions
        # abstract type A end             ==>  (abstract A)
        # abstract type \n\n A \n\n end   ==>  (abstract A)
        # abstract type A <: B end        ==>  (abstract (<: A B))
        # abstract type A <: B{T,S} end   ==>  (abstract (<: A (curly B T S)))
        # Oddities allowed by parser
        # abstract type A < B end         ==>  (abstract (call < A B))
        bump(ps, TRIVIA_FLAG)
        @assert peek(ps) == K"type"
        bump(ps, TRIVIA_FLAG)
        parse_subtype_spec(ps)
        bump_closing_token(ps, K"end")
        emit(ps, mark, K"abstract")
    elseif word in (K"struct", K"mutable")
        # struct A <: B \n a::X \n end  ==>  (struct false (<: A B) (block (:: a X)))
        if word == K"mutable"
            # mutable struct A end  ==>  (struct true A (block))
            bump(ps, remap_kind=K"true")
        else
            # struct A end  ==>  (struct false A (block))
            bump_invisible(ps, K"false")
        end
        @assert peek(ps) == K"struct"
        bump(ps, TRIVIA_FLAG)
        parse_subtype_spec(ps)
        parse_block(ps)
        bump_closing_token(ps, K"end")
        emit(ps, mark, K"struct")
    elseif word == K"primitive"
        # primitive type A 32 end             ==> (primitive A 32)
        # primitive type A <: B \n 8 \n end   ==> (primitive (<: A B) 8)
        bump(ps, TRIVIA_FLAG)
        @assert peek(ps) == K"type"
        bump(ps, TRIVIA_FLAG)
        parse_subtype_spec(ps)
        parse_cond(ps)
        bump_closing_token(ps, K"end")
        emit(ps, mark, K"primitive")
    elseif word == K"try"
        parse_try(ps)
    elseif word == K"return"
        bump(ps, TRIVIA_FLAG)
        k = peek(ps)
        if k == K"NewlineWs" || is_closing_token(ps, k)
            # return\nx   ==>  (return nothing)
            # return)     ==>  (return nothing)
            bump_invisible(ps, K"NothingLiteral")
        else
            # return x    ==>  (return x)
            # return x,y  ==>  (return (tuple x y))
            parse_eq(ps)
        end
        emit(ps, mark, K"return")
    elseif word in (K"break", K"continue")
        bump(ps, TRIVIA_FLAG)
        k = peek(ps)
        if !(k in (K"NewlineWs", K";", K")", K":", K"EndMarker") || (k == K"end" && !ps.end_symbol))
            recover(is_closer_or_newline, ps, TRIVIA_FLAG,
                    error="unexpected token after $(untokenize(word))")
        end
    elseif word in (K"module", K"baremodule")
        # module A end  ==> (module true A (block))
        # baremodule A end ==> (module false A (block))
        bump(ps, remap_kind= (word == K"module") ? K"true" : K"false")
        if is_reserved_word(peek(ps))
            # module do \n end  ==>  (module true (error do) (block))
            bump(ps, error="Invalid module name")
        else
            # module $A end  ==>  (module true ($ A) (block))
            parse_unary_prefix(ps)
        end
        # module A \n a \n b \n end  ==>  (module true A (block a b))
        parse_block(ps)
        bump_closing_token(ps, K"end")
        emit(ps, mark, K"module")
    elseif word == K"export"
        # export a
        # export a, b,
        bump(ps, TRIVIA_FLAG)
        parse_comma_separated(ps, parse_export_symbol)
        emit(ps, mark, K"export")
    elseif word in (K"import", K"using")
        TODO("parse_resword - $word")
    elseif word == K"do"
        bump(ps, TRIVIA_FLAG, error="invalid `do` syntax")
    else
        error("unhandled reserved word")
    end
end

# Parse if-elseif-else-end expressions
#
# if a xx elseif b yy else zz end   ==>  (if a (block xx) (elseif (block b) (block yy) (block zz)))
function parse_if_elseif(ps, is_elseif=false, is_elseif_whitespace_err=false)
    mark = position(ps)
    word = peek(ps)
    if is_elseif_whitespace_err
        # Only get here on recovery from error case - pretend we're parsing elseif.
        word = K"elseif"
    else
        bump(ps, TRIVIA_FLAG)
    end
    cond_mark = position(ps)
    if peek(ps) in (K"NewlineWs", K"end")
        # if end      ==>  (if (error) (block))
        # if \n end   ==>  (if (error) (block))
        bump_trivia(ps, error="missing condition in `$(untokenize(word))`")
    else
        # if a end      ==>  (if a (block))
        # if a xx end   ==>  (if a (block xx))
        parse_cond(ps)
    end
    if is_elseif
        # Wart: `elseif` condition is in a block but not `if` condition
        emit(ps, cond_mark, K"block")
    end
    # if a \n\n xx \n\n end   ==>  (if a (block xx))
    parse_block(ps)
    bump_trivia(ps)
    k = peek(ps)
    if k == K"elseif"
        # if a xx elseif b yy end   ==>  (if a (block xx) (elseif (block b) (block yy)))
        parse_if_elseif(ps, true)
    elseif k == K"else"
        emark = position(ps)
        bump(ps, TRIVIA_FLAG)
        if peek(ps) == K"if"
            # User wrote `else if` by mistake ?
            # if a xx else if b yy end  ==>  (if a (block xx) (else (error if) (block b) (block yy)))
            bump(ps, TRIVIA_FLAG)
            emit(ps, emark, K"error", TRIVIA_FLAG,
                 error="use `elseif` instead of `else if`")
            parse_if_elseif(ps, true, true)
        else
            # if a xx else yy end   ==>  (if a (block xx) (block yy))
            parse_block(ps)
        end
    end
    if !is_elseif
        bump_closing_token(ps, K"end")
    end
    emit(ps, mark, word)
end

function parse_const_local_global(ps)
    mark = position(ps)
    scope_mark = mark
    has_const = false
    scope_k = K"Nothing"
    k = peek(ps)
    if k in (K"global", K"local")
        # global x = 1  ==>  (global (= x 1))
        # local x = 1   ==>  (local (= x 1))
        scope_k = k
        bump(ps, TRIVIA_FLAG)
        if peek(ps) == K"const"
            # global const x = 1  ==>  (const (global (= x 1)))
            # local const x = 1   ==>  (const (local (= x 1)))
            has_const = true
            bump(ps, TRIVIA_FLAG)
        end
    else
        has_const = true
        # const x = 1          ==>  (const (= x 1))
        # const x,y = 1,2      ==>  (const (= (tuple x y) (tuple 1 2)))
        bump(ps, TRIVIA_FLAG)
        k = peek(ps)
        if k in (K"global", K"local")
            # const global x = 1   ==>  (const (global (= x 1)))
            # const local x = 1    ==>  (const (local (= x 1)))
            scope_k = k
            scope_mark = position(ps)
            bump(ps, TRIVIA_FLAG)
        end
    end
    # Like parse_eq, but specialized for error recovery:
    beforevar_mark = position(ps)
    n_commas = parse_comma(ps, false)
    t = peek_token(ps)
    if is_prec_assignment(t) && !is_decorated(t)
        if n_commas >= 1
            emit(ps, beforevar_mark, K"tuple")
        end
        bump(ps, TRIVIA_FLAG)
        parse_comma(ps)
        emit(ps, beforevar_mark, K"=")
    elseif has_const
        # const x  ==> (const (error x))
        # Recovery heuristic
        recover(ps, mark=beforevar_mark,
                error="Expected assignment after `const`") do ps, k
            k == K"NewlineWs" || (k != K"," && is_closing_token(ps, k))
        end
    else
        # global x    ==>  (global x)
        # local x     ==>  (local x)
        # global x,y  ==>  (global x y)
    end
    if scope_k != K"Nothing"
        emit(ps, scope_mark, scope_k)
    end
    if has_const
        emit(ps, mark, K"const")
    end
end

# Parse function and macro definitions
function parse_function(ps::ParseState)
    mark = position(ps)
    word = peek(ps)
    is_func = word == K"function"
    bump(ps, TRIVIA_FLAG)
    bump_trivia(ps)

    def_mark = position(ps)
    k = peek(ps)
    if k == K"("
        # Wart: flisp parser parses anon function arguments as tuples, roughly
        # like `parse_paren(ps)`, but the code to disambiguate those cases
        # is kind of awful.
        #
        # It seems much more consistent to treat them as function argument lists:
        # function (x,y) end   ==>  (function (tuple x y) (block))
        # function (x=1) end   ==>  (function (tuple (kw x 1)) (block))
        # function (;x=1) end  ==>  (function (tuple (parameters (kw x 1))) (block))
        bump(ps, TRIVIA_FLAG)
        parse_call_arglist(ps, K")", false)
        emit(ps, def_mark, K"tuple")
        # function (x) body end   ==>  (function (tuple x) (block body))
        #
        # Wart: flisp parser allows the following but it's invalid syntax in lowering
        # macro (x) end   !=>  (macro (tuple x) (block))
        # Fix is simple:
        if !is_func
            # macro (x) end   ==>  (macro (error (tuple x)) (block))
            emit(ps, def_mark, K"error", error="Expected macro name")
        end
    else
        if iskeyword(k)
            # Forbid things like
            # function begin() end  ==>  (function (call (error begin)) (block))
            # macro begin() end  ==>  (macro (call (error begin)) (block))
            bump(ps, error="invalid $(untokenize(word)) name")
            parse_call_chain(ps, def_mark)
        else
            # function f() end  ==>  (function (call f) (block))
            # function \n f() end  ==>  (function (call f) (block))
            # function $f() end  ==>  (function (call ($ f)) (block))
            parse_unary_prefix(ps)
        end
        parse_call_chain(ps, def_mark)
    end
    if is_func && peek(ps) == K"::"
        # Return type
        # function f()::T    end   ==>  (function (:: (call f) T) (block))
        # function f()::g(T) end   ==>  (function (:: (call f) (call g T)) (block))
        bump(ps, TRIVIA_FLAG)
        parse_call(ps)
        emit(ps, def_mark, K"::")
    end
    if peek(ps) == K"where"
        # function f() where {T} end   ==>  (function (where (call f) T) (block))
        # function f() where T   end   ==>  (function (where (call f) T) (block))
        parse_where_chain(ps, def_mark)
    end

    # The function body
    # function f() \n a \n b end  ==> (function (call f) (block a b))
    # function f() end            ==> (function (call f) (block))
    parse_block(ps)
    bump_closing_token(ps, K"end")
    emit(ps, mark, word)
end

# Parse a try block
#
# try \n x \n catch e \n y \n finally \n z end  ==>  (try (block x) e (block y) false (block z))
#v1.8: try \n x \n catch e \n y \n else z finally \n w end  ==>  (try (block x) e (block y) (block z) (block w))
#
# flisp: embedded in parse_resword
function parse_try(ps)
    mark = position(ps)
    bump(ps, TRIVIA_FLAG)
    parse_block(ps)
    has_catch = false
    has_else = false
    has_finally = false
    bump_trivia(ps)
    flags = EMPTY_FLAGS
    bump_trivia(ps)
    if peek(ps) == K"catch"
        has_catch = true
        parse_catch(ps)
    else
        bump_invisible(ps, K"false")
        bump_invisible(ps, K"false")
    end
    bump_trivia(ps)
    if peek(ps) == K"else"
        # catch-else syntax: https://github.com/JuliaLang/julia/pull/42211
        #
        #v1.8: try catch ; else end ==> (try (block) false (block) (block) false)
        has_else = true
        else_mark = position(ps)
        bump(ps, TRIVIA_FLAG)
        parse_block(ps)
        if !has_catch
            #v1.8: try else end ==> (try (block) false false (error (block)) false)
            emit(ps, else_mark, K"error", error="Expected `catch` before `else`")
        end
        if ps.julia_version < v"1.8"
            #v1.7: try catch ; else end ==> (try (block) false (block) (error (block)) false)
            emit(ps, else_mark, K"error",
                 error="`else` in `try` requires at least Julia 1.8")
        end
    else
        bump_invisible(ps, K"false")
    end
    bump_trivia(ps)
    if peek(ps) == K"finally"
        # try x finally y end  ==>  (try (block x) false false false (block y))
        has_finally = true
        bump(ps, TRIVIA_FLAG)
        parse_block(ps)
    else
        bump_invisible(ps, K"false")
    end
    # Wart: the flisp parser allows finally before catch, the *opposite* order
    # in which these blocks execute.
    bump_trivia(ps)
    if !has_catch && peek(ps) == K"catch"
        # try x finally y catch e z end  ==>  (try (block x) false false false (block y) e (block z))
        flags |= TRY_CATCH_AFTER_FINALLY_FLAG
        m = position(ps)
        parse_catch(ps)
        emit_diagnostic(ps, m, position(ps),
                        warning="`catch` after `finally` will execute out of order")
    end
    bump_closing_token(ps, K"end")
    emit(ps, mark, K"try", flags)
end

function parse_catch(ps::ParseState)
    bump(ps, TRIVIA_FLAG)
    k = peek(ps)
    if k in (K";", K"NewlineWs") || is_closing_token(ps, k)
        # try x catch ; y end  ==>  (try (block x) false (block y) false false)
        # try x catch \n y end  ==>  (try (block x) false (block y) false false)
        bump_invisible(ps, K"false")
        bump(ps, TRIVIA_FLAG)
    else
        # try x catch e y end  ==>  (try (block x) e (block y) false false)
        parse_identifier_or_interpolate(ps)
    end
    parse_block(ps)
end

# flisp: parse-do
function parse_do(ps::ParseState)
    ps = normal_context(ps)
    mark = position(ps)
    if peek(ps) in (K"NewlineWs", K";")
        # f() do\nend        ==>  (do (call f) (-> (tuple) (block)))
        # f() do ; body end  ==>  (do (call f) (-> (tuple) (block body)))
        # this trivia needs to go into the tuple due to the way position()
        # works.
        bump(ps, TRIVIA_FLAG)
    else
        # f() do x, y\n body end  ==>  (do (call f) (-> (tuple x y) (block body)))
        parse_comma_separated(ps, parse_range)
    end
    emit(ps, mark, K"tuple")
    parse_block(ps)
    bump_closing_token(ps, K"end")
    emit(ps, mark, K"->")
end

# flisp: parse-imports
function parse_imports(ps::ParseState, word)
    TODO("parse_imports unimplemented")
end

function macro_name_kind(k)
    return k == K"Identifier"    ? K"MacroName"    :
           k == K"."             ? K"@."           :
           k == K"VarIdentifier" ? K"VarMacroName" :
           error("Unrecognized source kind for macro name $k")
end

# If remap_kind is false, the kind will be remapped by parse_call_chain after
# it discovers the macro name component of the module path.
#
# flisp: parse-macro-name
function parse_macro_name(ps::ParseState; remap_kind=false)
    bump_disallowed_space(ps)
    if peek(ps) == K"."
        # @. y  ==>  (macrocall (quote @__dot__) y)
        bump(ps)
    else
        parse_atom(ps, false)
    end
    if remap_kind
        reset_node!(ps, position(ps), kind=macro_name_kind(peek_behind(ps)))
    end
end

# flisp: parse-atsym
function parse_atsym(ps::ParseState)
    TODO("parse_atsym unimplemented")
end

# flisp: parse-import-dots
function parse_import_dots(ps::ParseState)
    TODO("parse_import_dots unimplemented")
end

# flisp: parse-import-path
function parse_import_path(ps::ParseState, word)
    TODO("parse_import_path unimplemented")
end

# flisp: parse-import
function parse_import(ps::ParseState, word, from)
    TODO("parse_import unimplemented")
end

# parse comma-separated assignments, like "i=1:n,j=1:m,..."
#
# flisp: parse-comma-separated
function parse_comma_separated(ps::ParseState, down)
    n_subexprs = 0
    while true
        down(ps)
        n_subexprs += 1
        if peek(ps) == K","
            bump(ps, TRIVIA_FLAG)
        else
            break
        end
    end
    return n_subexprs
end

# flisp: parse-comma-separated-assignments
function parse_comma_separated_assignments(ps::ParseState)
    TODO("parse_comma_separated_assignments unimplemented")
end

# FIXME(sschaub): for backwards compatibility, allows newline before =/in/∈
# in generator expressions. See issue #37393
function peek_skip_newline_in_gen(ps::ParseState, n=1)
    k = peek(ps, n)
    if ps.for_generator && k == K"NewlineWs"
        k = peek(ps, n+1)
    end
    return k
end

# parse comma-separated "assignment" but allowing `in` and `∈` as assignment operators
#
# i = rhs   ==>  (= i rhs)
# i in rhs  ==>  (= i rhs)
# i ∈ rhs   ==>  (= i rhs)
#
# i = 1:10       ==>  (= i (call : 1 10))
# (i,j) in iter  ==>  (= (tuple i j) iter)
#
# flisp: parse-iteration-spec
function parse_iteration_spec(ps::ParseState)
    mark = position(ps)
    k = peek(ps)
    # Handle `outer` contextual keyword
    is_outer_kw = k == K"outer" && !(peek_skip_newline_in_gen(ps, 2) in (K"=", K"in", K"∈"))
    if is_outer_kw
        # outer i = rhs  ==>  (= (outer i) rhs)
        bump(ps, TRIVIA_FLAG)
    end
    with_space_sensitive(parse_pipe_lt, ps)
    if is_outer_kw
        emit(ps, mark, K"outer")
    end
    if peek_skip_newline_in_gen(ps) in (K"=", K"in", K"∈")
        bump(ps, TRIVIA_FLAG)
        parse_pipe_lt(ps)
    else
        # Recovery heuristic
        recover(ps, error="invalid iteration spec: expected one of `=` `in` or `∈`") do ps, k
            k in (K",", K"NewlineWs") || is_closing_token(ps, k)
        end
        # TODO: or try parse_pipe_lt ???
    end
    emit(ps, mark, K"=")
end

# flisp: parse-comma-separated-iters
function parse_comma_separated_iters(ps::ParseState)
    # FIXME REmove?
    parse_comma_separated(ps, parse_iteration_spec)
end

# flisp: parse-space-separated-exprs
function parse_space_separated_exprs(ps::ParseState)
    with_space_sensitive(ps) do ps
        n_sep = 0
        while true
            k = peek(ps)
            if is_closing_token(ps, k) || k == K"NewlineWs" ||
                    (ps.for_generator && k == K"for")
                break
            end
            parse_eq(ps)
            n_sep += 1
        end
        return n_sep
    end
end

# like parse-arglist, but with `for` parsed as a generator
#
# flisp: parse-call-arglist
function parse_call_arglist(ps::ParseState, closer, is_macrocall)
    ps = ParseState(ps, for_generator=true)

    parse_brackets(ps, closer) do _, _, _
        bump_closing_token(ps, closer)
        return (needs_parameters=true,
                eq_is_kw_before_semi=!is_macrocall,
                eq_is_kw_after_semi=true)
    end
end

# Parse the suffix of comma-separated array expressions such as
# [x, suffix].  Consumes `closer`, but does not emit the AST node for the
# surrounding brackets.
#
# flisp: parse-vect
function parse_vect(ps::ParseState, closer)
    # [x, y]        ==>  (vect x y)
    # [x, y]        ==>  (vect x y)
    # [x,y ; z]     ==>  (vect x y (parameters z))
    # [x=1, y=2]    ==>  (vect (= x 1) (= y 2))
    # [x=1, ; y=2]  ==>  (vect (= x 1) (parameters (= y 2)))
    parse_brackets(ps, closer) do _, _, _
        bump_closing_token(ps, closer)
        return (needs_parameters=true,
                eq_is_kw_before_semi=false,
                eq_is_kw_after_semi=false)
    end
    return (K"vect", EMPTY_FLAGS)
end

# Flattened generators are hard because the Julia AST doesn't respect a key
# rule we normally expect: that the children of an AST node are a contiguous
# range in the source text. This is because the `for`s in
# `[xy for x in xs for y in ys]` are parsed in the normal order of a for as
#
# (flatten
#  (generator
#   (generator
#    xy
#    y in ys)
#   x in xs))
#
# A reasonable way to deal with this is to emit only the flatten:
#
# (flatten xy (= x xs) (= y ys))
#
# then reconstruct the nested generators when converting to Expr.
#
# flisp: parse-generator
function parse_generator(ps::ParseState, mark, flatten=false)
    # (x for x in xs) ==>  (generator x (= x xs))
    t = peek_token(ps)
    if !t.had_whitespace
        # [(x)for x in xs]  ==>  (comprehension (generator x (error) (= x xs)))
        bump_invisible(ps, K"error", TRIVIA_FLAG,
                       error="Expected space before `for` in generator")
    end
    @assert kind(t) == K"for"
    bump(ps, TRIVIA_FLAG)
    filter_mark = position(ps)
    parse_comma_separated_iters(ps)
    if peek(ps) == K"if"
        bump(ps, TRIVIA_FLAG)
        parse_cond(ps)
        emit(ps, filter_mark, K"filter")
    end
    t = peek_token(ps)
    if kind(t) == K"for"
        # [xy for x in xs for y in ys]   ==>  (comprehension (flatten xy (= x xs) (= y ys)))
        parse_generator(ps, mark, true)
        emit(ps, mark, K"flatten")
    elseif !flatten
        emit(ps, mark, K"generator")
    end
end

# flisp: parse-comprehension
function parse_comprehension(ps::ParseState, mark, closer)
    ps = ParseState(ps, whitespace_newline=true,
                    space_sensitive=false)
    parse_generator(ps, mark)
    bump_closing_token(ps, closer)
    return (K"comprehension", EMPTY_FLAGS)
end

# Parse array concatenation syntax with multiple semicolons
#
# Normal matrix construction syntax
# [x y ; z w]     ==>  (vcat (row x y) (row z w))
# [x y ; z w ; a b]  ==>  (vcat (row x y) (row z w) (row a b))
# [x ; y ; z]     ==>  (vcat x y z)
# [x;]            ==>  (vcat x)
# [x y]           ==>  (hcat x y)
#
# Mismatched rows
# [x y ; z]     ==>  (vcat (row x y) z)
#
# Double semicolon with spaces allowed (only) for line continuation
# [x y ;;\n z w]  ==>  (hcat x y z w)
# [x y ;; z w]    ==>  (hcat x y (error) z w)
#
# Single elements in rows
# [x ; y ;; z ]  ==>  (ncat 2 (nrow 1 x y) z)
# [x  y ;;; z ]  ==>  (ncat 3 (row x y) z)
#
# Higher dimensional ncat
# Row major
# [x y ; z w ;;; a b ; c d]  ==>
#     (ncat 3 (nrow 1 (row x y) (row z w)) (nrow 1 (row a b) (row c d)))
# Column major
# [x ; y ;; z ; w ;;; a ; b ;; c ; d]  ==>
#     (ncat 3 (nrow 2 (nrow 1 x y) (nrow 1 z w)) (nrow 2 (nrow 1 a b) (nrow 1 c d)))
#
# flisp: parse-array
function parse_array(ps::ParseState, mark, closer, end_is_symbol)
    ps = ParseState(ps, end_symbol=end_is_symbol)

    # Outer array parsing loop - parse chain of separators with descending
    # precedence such as
    # [a ; b ;; c ;;; d ;;;; e] ==> (ncat-4 (ncat-3 (ncat-2 (ncat-1 a b) c) d) e)
    #
    # Ascending and equal precedence is handled by parse_array_inner.
    #
    # This is a variant of a Pratt parser, but we have a separate outer loop
    # because there's no minimum precedence/binding power - you can always get
    # a lower binding power by adding more semicolons.
    #
    # For an excellent overview of Pratt parsing, see
    # https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
    (dim, binding_power) = parse_array_separator(ps)
    while true
        (next_dim, next_bp) = parse_array_inner(ps, binding_power)
        if next_bp == typemin(Int)
            break
        end
        if binding_power == 0
            emit(ps, mark, K"row")
        else
            emit(ps, mark, K"nrow", numeric_flags(dim))
        end
        dim = next_dim
        binding_power = next_bp
    end
    bump_closing_token(ps, closer)
    return binding_power == -1 ? (K"vcat", EMPTY_FLAGS) :
           binding_power ==  0 ? (K"hcat", EMPTY_FLAGS) :
           (K"ncat", numeric_flags(dim))
end

# Parse equal and ascending precedence chains of array concatenation operators
# (semicolons, newlines and whitespace). Invariants:
#
# * The caller must have already consumed
#   - The left hand side
#   - The concatenation operator, providing the current binding_power.
#   So eg, we're here in the input stream
#                |
#          [a ;; b ; c ]
#          [a ;; ]
#
# * The caller must call emit() to delimit the AST node for this binding power.
#
function parse_array_inner(ps, binding_power)
    mark = NO_POSITION
    dim = -1
    bp = binding_power
    while true
        if bp < binding_power
            return (dim, bp)
        end
        # Allow trailing separators
        # [a ;] ==> (vcat a)
        # [a ; b;;] ==> (ncat-2 (nrow-1 a b))
        if is_closing_token(ps, peek(ps))
            return (typemin(Int), typemin(Int))
        end
        if bp == binding_power
            # Parse one expression
            mark = position(ps)
            parse_eq_star(ps)
            (next_dim, next_bp) = parse_array_separator(ps)
        else # bp > binding_power
            # Recurse to parse a separator with greater binding power. Eg:
            # [a ;; b ; c ]
            #       |   ^------ the next input is here
            #       '---------- the mark is here
            (next_dim, next_bp) = parse_array_inner(ps, bp)
            if bp == 0
                emit(ps, mark, K"row")
            else
                emit(ps, mark, K"nrow", numeric_flags(dim))
            end
        end
        dim, bp = next_dim, next_bp
    end
end

# Parse a separator in an array concatenation
#
# Here we aim to identify:
# * Dimension on which the next separator acts
# * Binding power (precedence) of the separator, where whitespace binds
#   tightest:  ... < `;;;` < `;;` < `;`,`\n` < whitespace. We choose binding
#   power of 0 for whitespace and negative numbers for other separators.
function parse_array_separator(ps)
    t = peek_token(ps)
    k = kind(t)
    if k == K";"
        n_semis = 1
        while true
            bump(ps, TRIVIA_FLAG)
            t = peek_token(ps)
            if kind(t) != K";" || t.had_whitespace
                break
            end
            n_semis += 1
        end
        # FIXME - following is ncat, not line continuation
        # [a ;; \n c]
        if n_semis == 2 && peek(ps) == K"NewlineWs"
            # Line continuation
            # [a b ;; \n \n c]
            # TODO: Should this only consume a single newline?
            while peek(ps) == K"NewlineWs"
                bump(ps, TRIVIA_FLAG)
            end
            return (2, 0)
        else
            return (n_semis, -n_semis)
        end
    elseif k == K"NewlineWs"
        bump_trivia(ps)
        # Newlines separate the first dimension
        return (1, -1)
    else
        if t.had_whitespace && !is_closing_token(ps, k)
            return (2, 0)
        else
            return (typemin(Int), typemin(Int))
        end
    end
end

# Parse array concatenation/construction/indexing syntax inside of `[]` or `{}`.
#
# flisp: parse-cat
function parse_cat(ps::ParseState, closer, end_is_symbol)
    ps = ParseState(ps, range_colon_enabled=true,
                    space_sensitive=true,
                    where_enabled=true,
                    whitespace_newline=false,
                    for_generator=true)
    k = peek(ps, skip_newlines=true)
    if k == closer
        # []  ==>  (vect)
        return parse_vect(ps, closer)
    end
    mark = position(ps)
    parse_eq_star(ps)
    k = peek(ps, skip_newlines=true)
    if k in (K",", closer)
        if k == K","
            # [x,]  ==>  (vect x)
            bump(ps, TRIVIA_FLAG)
        end
        # [x]      ==>  (vect x)
        # [x \n ]  ==>  (vect x)
        parse_vect(ps, closer)
    elseif k == K"for"
        # [x for x in xs]  ==>  (comprehension (generator x (= x xs)))
        # [x \n\n for x in xs]  ==>  (comprehension (generator x (= x xs)))
        parse_comprehension(ps, mark, closer)
    else
        # [x y]  ==>  (hcat x y)
        # and other forms; See parse_array.
        parse_array(ps, mark, closer, end_is_symbol)
    end
end

# Parse un-prefixed parenthesized syntax. This is hard because parentheses are
# *very* overloaded!
#
# flisp: parse-paren / parse-paren-
function parse_paren(ps::ParseState, check_identifiers=true)
    ps = ParseState(ps, range_colon_enabled=true,
                    space_sensitive=false,
                    where_enabled=true,
                    whitespace_newline=true)
    mark = position(ps)
    @assert peek(ps) == K"("
    bump(ps, TRIVIA_FLAG) # K"("
    after_paren_mark = position(ps)
    k = peek(ps)
    if k == K")"
        # ()  ==>  (tuple)
        bump(ps, TRIVIA_FLAG)
        emit(ps, mark, K"tuple")
    elseif is_syntactic_operator(k)
        # allow :(=) etc in unchecked contexts, eg quotes
        # :(=)  ==>  (quote =)
        if check_identifiers && !is_valid_identifier(k)
            bump(ps, error="invalid identifier")
        else
            bump(ps)
        end
        bump_closing_token(ps, K")")
    elseif !check_identifiers && k == K"::" && peek(ps, 2, skip_newlines=true) == K")"
        # allow :(::) as a special case
        # :(::)   ==>  (quote ::)
        bump(ps)
        bump(ps, TRIVIA_FLAG, skip_newlines=true)
    else
        # Deal with all other cases of tuple or block syntax via the generic
        # parse_brackets
        initial_semi = peek(ps) == K";"
        is_tuple = false
        is_block = false
        parse_brackets(ps, K")") do had_commas, num_semis, num_subexprs
            # Parentheses used for grouping
            # (a * b)     ==>  (call-i * a b)
            # (a=1)       ==>  (= a 1)
            # (x)         ==>  x
            is_tuple = had_commas ||
                       (initial_semi && (num_semis == 1 || num_subexprs > 0))
            is_block = num_semis > 0
            bump_closing_token(ps, K")")
            return (needs_parameters=is_tuple,
                    eq_is_kw_before_semi=false,
                    eq_is_kw_after_semi=is_tuple)
        end
        if is_tuple
            # Tuple syntax with commas
            # (x,)        ==>  (tuple x)
            # (x,y)       ==>  (tuple x y)
            # (x=1, y=2)  ==>  (tuple (= x 1) (= y 2))
            #
            # Named tuple with initial semicolon
            # (;)         ==>  (tuple (parameters))
            # (; a=1)     ==>  (tuple (parameters (kw a 1)))
            #
            # Extra credit: nested parameters and frankentuples
            # (; a=1; b=2)    ==> (tuple (parameters (kw a 1) (parameters (kw b 2))))
            # (a; b; c,d)     ==> (tuple a (parameters b (parameters c d)))
            # (a=1, b=2; c=3) ==> (tuple (= a 1) (= b 2) (parameters (kw c 3)))
            emit(ps, mark, K"tuple")
        elseif is_block
            # Blocks
            # (;;)        ==>  (block)
            # (a=1;)      ==>  (block (= a 1))
            # (a;b;;c)    ==>  (block a b c)
            # (a=1; b=2)  ==>  (block (= a 1) (= b 2))
            emit(ps, mark, K"block")
        end
    end
end

# Handle bracketed syntax inside any of () [] or {} where there's a mixture
# of commas and semicolon delimiters.
#
# For parentheses this is hard because there's various ambiguities depending on
# context. In general (X; Y) is difficult when X and Y are subexpressions
# possibly containing `,` and `=`.
#
# For example, (a=1; b=2) could be seen to parse four different ways!
#
# Function args:   (kw a 1) (parameters (kw b 2))
# Tuple-like:      (=  a 1) (parameters (kw b 2)) 
# Block:           (=  a 1)             (=  b 2)  
# [] vect-like:    (=  a 1) (parameters (=  b 2)) 
#
# Expressions (X; Y; Z) with more semicolons are also allowed by the flisp
# parser and generally parse as nested parameters blocks. This is invalid Julia
# syntax so the parse tree is pretty strange in these cases!  Some macros
# probably use it though.  Example:
#
# (a,b=1; c,d=2; e,f=3)  ==>  (tuple a (= b 1) (parameters c (kw d 2) (parameters e (kw f 3))))
#
# Deciding which of these representations to use depends on both the prefix
# context and the contained expressions. To distinguish between blocks vs
# tuples we use the presence of `,` within the `;`-delimited sections: If
# there's commas, it's a tuple, otherwise a block.
#
# flisp: parts of parse-paren- and parse-arglist
function parse_brackets(after_parse::Function,
                        ps::ParseState, closing_kind)
    ps = ParseState(ps, range_colon_enabled=true,
                    space_sensitive=false,
                    where_enabled=true,
                    whitespace_newline=true)
    params_marks = ParseStreamPosition[]
    eq_positions = ParseStreamPosition[]
    last_eq_before_semi = 0
    num_subexprs = 0
    num_semis = 0
    had_commas = false
    while true
        bump_trivia(ps)
        k = peek(ps)
        if k == closing_kind
            break
        elseif k == K";"
            # Start of parameters list
            # a, b; c d  ==>  a b (parameters c d)
            push!(params_marks, position(ps))
            if num_semis == 0
                last_eq_before_semi = length(eq_positions)
            end
            num_semis += 1
            bump(ps, TRIVIA_FLAG)
            bump_trivia(ps)
        else
            num_subexprs += 1
            mark = position(ps)
            eq_pos = parse_eq_star(ps)
            if eq_pos != NO_POSITION
                push!(eq_positions, eq_pos)
            end
            t = peek_token(ps, skip_newlines=true)
            k = kind(t)
            bump_trivia(ps)
            if k == K","
                had_commas = true
                bump(ps, TRIVIA_FLAG)
            elseif k == K";" || k == closing_kind
                # Handled above
                continue
            elseif k == K"for"
                # Generator syntax
                parse_generator(ps, mark)
            else
                k_str = untokenize(k)
                ck_str = untokenize(closing_kind)
                if is_closing_token(ps, k)
                    emit_diagnostic(ps, error="unexpected `$k_str` in bracketed list")
                else
                    emit_diagnostic(ps, error="missing comma or $ck_str in bracketed list")
                end
                # Recovery done after loop
                break
            end
        end
    end
    actions = after_parse(had_commas, num_semis, num_subexprs)
    if num_semis == 0
        last_eq_before_semi = length(eq_positions)
    end
    # Turn any K"=" into K"kw" as necessary
    if actions.eq_is_kw_before_semi
        # f(a=1)   ==>   (call f (kw a 1))
        for i=1:last_eq_before_semi
            reset_node!(ps, eq_positions[i], kind=K"kw")
        end
    end
    if actions.eq_is_kw_after_semi
        for i = last_eq_before_semi+1:length(eq_positions)
            reset_node!(ps, eq_positions[i], kind=K"kw")
        end
    end
    # Emit nested parameter nodes if necessary
    if actions.needs_parameters
        for mark in Iterators.reverse(params_marks)
            emit(ps, mark, K"parameters")
        end
    end
end

# flisp: parse-raw-literal
function parse_raw_literal(ps::ParseState, delim)
    TODO("parse_raw_literal unimplemented")
end

# flisp: unescape-parsed-string-literal
function unescape_parsed_string_literal(strs)
    TODO("unescape_parsed_string_literal unimplemented")
end

# flisp: strip-escaped-newline
function strip_escaped_newline(s, raw)
    TODO("strip_escaped_newline unimplemented")
end

# remove `\` followed by a newline
#
# flisp: strip-escaped-newline-
function strip_escaped_newline_(s)
    TODO("strip_escaped_newline_ unimplemented")
end

# flisp: parse-string-literal
function parse_string_literal(ps::ParseState, delim, raw)
    TODO("parse_string_literal unimplemented")
end

# flisp: strip-leading-newline
function strip_leading_newline(s)
    TODO("strip_leading_newline unimplemented")
end

# flisp: dedent-triplequoted-string
function dedent_triplequoted_string(lst)
    TODO("dedent_triplequoted_string unimplemented")
end

# flisp: triplequoted-string-indentation
function triplequoted_string_indentation(lst)
    TODO("triplequoted_string_indentation unimplemented")
end

# flisp: triplequoted-string-indentation-
function triplequoted_string_indentation_(s)
    TODO("triplequoted_string_indentation_ unimplemented")
end

# return the longest common prefix of the elements of l
# e.g., (longest-common-prefix ((1 2) (1 4))) -> (1)
#
# flisp: longest-common-prefix
function longest_common_prefix(l)
    TODO("longest_common_prefix unimplemented")
end

# return the longest common prefix of lists a & b
#
# flisp: longest-common-prefix2
function longest_common_prefix2(a, b)
    TODO("longest_common_prefix2 unimplemented")
end

# flisp: longest-common-prefix2-
function longest_common_prefix2_(a, b, p)
    TODO("longest_common_prefix2_ unimplemented")
end

# flisp: string-split
function string_split(s, sep)
    TODO("string_split unimplemented")
end

# flisp: string-split-
function string_split_(s, sep, start, splits)
    TODO("string_split_ unimplemented")
end

# replace all occurrences of a in s with b
#
# flisp: string-replace
function string_replace(s, a, b)
    TODO("string_replace unimplemented")
end

# flisp: ends-interpolated-atom?
function is_ends_interpolated_atom(c)
    TODO("is_ends_interpolated_atom unimplemented")
end

# flisp: parse-interpolate
function parse_interpolate(ps::ParseState)
    TODO("parse_interpolate unimplemented")
end

# raw = raw string literal
# when raw is #t, unescape only \\ and delimiter
# otherwise do full unescaping, and parse interpolations too
#
# flisp: parse-string-literal-
function parse_string_literal_(n, p, s, delim, raw)
    TODO("parse_string_literal_ unimplemented")
end

# flisp: unescape-string
function unescape_string_(s)
    TODO("unescape_string_ unimplemented")
end

# parse numbers, identifiers, parenthesized expressions, lists, vectors, etc.
#
# If `check_identifiers` is true, identifiers are disallowed from being one of
# the syntactic operators or closing tokens.
#
# flisp: parse-atom
function parse_atom(ps::ParseState, check_identifiers=true)
    bump_trivia(ps)
    mark = position(ps)
    leading_kind = peek(ps)
    # TODO: Reorder to put most likely tokens first?
    if leading_kind == K":"
        # symbol/expression quote
        # :foo  =>  (quote foo)
        t = peek_token(ps, 2)
        k = kind(t)
        if is_closing_token(ps, k) && (!iskeyword(k) || t.had_whitespace)
            # : is a literal colon in some circumstances
            # :)     ==>  :
            # : end  ==>  :
            bump(ps) # K":"
            return
        end
        bump(ps, TRIVIA_FLAG) # K":"
        if t.had_whitespace
            # : a  ==> (quote (error-t) a))
            # ===
            # :
            # a
            # ==> (quote (error))
            bump_trivia(ps, error="whitespace not allowed after `:` used for quoting")
            # Heuristic recovery
            if kind(t) == K"NewlineWs"
                bump_invisible(ps, K"error")
            else
                bump(ps)
            end
        else
            # Being inside quote makes keywords into identifiers at at the
            # first level of nesting
            # :end ==> (quote end)
            # :(end) ==> (quote (error end))
            # Being inside quote makes end non-special again (issue #27690)
            # a[:(end)]  ==>  (ref a (quote (error-t end)))
            parse_atom(ParseState(ps, end_symbol=false), false)
        end
        emit(ps, mark, K"quote")
    elseif leading_kind == K"="
        bump(ps, TRIVIA_FLAG, error="unexpected `=`")
    elseif leading_kind == K"Identifier"
        bump(ps)
    elseif leading_kind == K"VarIdentifier"
        bump(ps)
        t = peek_token(ps)
        if !t.had_whitespace && !(isoperator(kind(t)) || is_non_keyword_closer(t))
            bump(ps, error="suffix not allowed after var\"...\" syntax")
        end
    elseif isoperator(leading_kind)
        # Operators and keywords are generally turned into identifiers if used
        # as atoms.
        if check_identifiers && is_syntactic_operator(leading_kind)
            bump(ps, error="invalid identifier")
        else
            bump(ps)
        end
    elseif iskeyword(leading_kind)
        if check_identifiers && is_closing_token(ps, leading_kind)
            # :(end)  ==>  (quote (error end))
            bump(ps, error="invalid identifier")
        else
            # :end  ==>  (quote end)
            bump(ps, remap_kind=K"Identifier")
        end
    elseif leading_kind == K"(" # parens or tuple
        parse_paren(ps, check_identifiers)
    elseif leading_kind == K"[" # cat expression
        bump(ps, TRIVIA_FLAG)
        ckind, cflags = parse_cat(ps, K"]", ps.end_symbol)
        emit(ps, mark, ckind, cflags)
    elseif leading_kind == K"{" # cat expression
        bump(ps, TRIVIA_FLAG)
        ckind, cflags = parse_cat(ps, K"}", ps.end_symbol)
        emit_braces(ps, mark, ckind, cflags)
    elseif is_string(leading_kind)
        bump(ps)
        # FIXME parse_string_literal(ps)
    elseif leading_kind == K"@" # macro call
        bump(ps, TRIVIA_FLAG)
        parse_macro_name(ps)
        parse_call_chain(ps, mark, true)
    elseif leading_kind in (K"Cmd", K"TripleCmd")
        bump_invisible(ps, K"core_@cmd")
        bump(ps)
        emit(ps, mark, K"macrocall")
    elseif isliteral(leading_kind)
        bump(ps)
    elseif is_closing_token(ps, leading_kind)
        # Leave closing token in place for other productions to 
        # recover with (??)
        msg = leading_kind == K"EndMarker" ?
              "premature end of input" :
              "unexpected closing token"
        emit_diagnostic(ps, error=msg)
    else
        bump(ps, error="invalid syntax atom")
    end
end

function emit_braces(ps, mark, ckind, cflags)
    if ckind == K"hcat"
        # {x y}  ==>  (bracescat (row x y))
        emit(ps, K"row", mark, cflags)
    elseif ckind == K"ncat"
        # {x ;;; y}  ==>  (bracescat (nrow-3 x y))
        emit(ps, K"nrow", mark, cflags)
    end
    outk = ckind in (K"vect", K"comprehension") ? K"braces" : K"bracescat"
    emit(ps, mark, outk)
end

# Parse docstrings attached by a space or single newline
# "doc" foo  ==>  
#
# flisp: parse-docstring
function parse_docstring(ps::ParseState, down=parse_eq)
    mark = position(ps)
    # TODO? This is not quite equivalent to the flisp parser which accepts
    # more than just a string. For example:
    #! ("doc") foo  ==>  (macrocall core_@doc "doc" foo)
    # TODO: Also, all these TOMBSTONEs seem kind of inefficient. Perhaps we can
    # improve things?
    maybe_doc = is_string(peek(ps))
    atdoc_mark = bump_invisible(ps, K"TOMBSTONE")
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
                # ==> (macrocall core_@doc "doc" foo)
                bump(ps, TRIVIA_FLAG) # NewlineWs
            end
        end
        if is_doc
            reset_node!(ps, atdoc_mark, kind=K"core_@doc")
            down(ps)
            emit(ps, mark, K"macrocall")
        end
    end
end


#-------------------------------------------------------------------------------
# Parser entry points

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
    return stream
end

function parse_all(code, args...)
    stream = ParseStream(code)
    return parse_all(stream, args...)
end

