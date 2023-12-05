"""
    ParseState(stream::ParseStream)

ParseState is an internal data structure wrapping `ParseStream` to carry parser
context as we recursively descend into the parse tree. For example, normally
`x -y` means `(x) - (y)`, but when parsing matrix literals we're in
`space_sensitive` mode, and `[x -y]` means [(x) (-y)].
"""
struct ParseState
    stream::ParseStream

    # Disable range colon for parsing ternary conditional operator
    range_colon_enabled::Bool
    # In space-sensitive mode "x -y" is 2 expressions, not a subtraction
    space_sensitive::Bool
    # Seeing `for` stops parsing macro arguments and makes a generator
    for_generator::Bool
    # Treat 'end' like a normal symbol instead of a reserved word
    end_symbol::Bool
    # Treat newline like ordinary whitespace instead of as a potential separator
    whitespace_newline::Bool
    # Enable parsing `where` with high precedence
    where_enabled::Bool
end

# Normal context
function ParseState(stream::ParseStream)
    ParseState(stream, true, false, false, false, false, true)
end

function ParseState(ps::ParseState; range_colon_enabled=nothing,
                    space_sensitive=nothing, for_generator=nothing,
                    end_symbol=nothing, whitespace_newline=nothing,
                    where_enabled=nothing)
    ParseState(ps.stream,
        range_colon_enabled === nothing ? ps.range_colon_enabled : range_colon_enabled,
        space_sensitive === nothing ? ps.space_sensitive : space_sensitive,
        for_generator === nothing ? ps.for_generator : for_generator,
        end_symbol === nothing ? ps.end_symbol : end_symbol,
        whitespace_newline === nothing ? ps.whitespace_newline : whitespace_newline,
        where_enabled === nothing ? ps.where_enabled : where_enabled)
end

# Functions to change parse state

function normal_context(ps::ParseState)
    ParseState(ps,
               range_colon_enabled=true,
               space_sensitive=false,
               where_enabled=true,
               for_generator=false,
               end_symbol=false,
               whitespace_newline=false)
end

function with_space_sensitive(ps::ParseState)
    ParseState(ps,
               space_sensitive=true,
               whitespace_newline=false)
end

# Convenient wrappers for ParseStream

function Base.peek(ps::ParseState, n=1; skip_newlines=nothing)
    skip_nl = isnothing(skip_newlines) ? ps.whitespace_newline : skip_newlines
    peek(ps.stream, n; skip_newlines=skip_nl)
end

function peek_token(ps::ParseState, n=1; skip_newlines=nothing)
    skip_nl = isnothing(skip_newlines) ? ps.whitespace_newline : skip_newlines
    peek_token(ps.stream, n, skip_newlines=skip_nl)
end

function peek_full_token(ps::ParseState, n=1; skip_newlines=nothing, kws...)
    skip_nl = isnothing(skip_newlines) ? ps.whitespace_newline : skip_newlines
    peek_full_token(ps.stream, n; skip_newlines=skip_nl, kws...)
end

function peek_behind(ps::ParseState, args...; kws...)
    peek_behind(ps.stream, args...; kws...)
end

function peek_behind_pos(ps::ParseState, args...; kws...)
    peek_behind_pos(ps.stream, args...; kws...)
end

function bump(ps::ParseState, flags=EMPTY_FLAGS; skip_newlines=nothing, kws...)
    skip_nl = isnothing(skip_newlines) ? ps.whitespace_newline : skip_newlines
    bump(ps.stream, flags; skip_newlines=skip_nl, kws...)
end

function bump_trivia(ps::ParseState, args...; kws...)
    bump_trivia(ps.stream, args...; kws...)
end

function bump_invisible(ps::ParseState, args...; kws...)
    bump_invisible(ps.stream, args...; kws...)
end

function bump_glue(ps::ParseState, args...; kws...)
    bump_glue(ps.stream, args...; kws...)
end

function bump_split(ps::ParseState, args...; kws...)
    bump_split(ps.stream, args...; kws...)
end

function reset_node!(ps::ParseState, args...; kws...)
    reset_node!(ps.stream, args...; kws...)
end

function steal_token_bytes!(ps::ParseState, args...)
    steal_token_bytes!(ps.stream, args...)
end

function Base.position(ps::ParseState, args...)
    position(ps.stream, args...)
end

function emit(ps::ParseState, args...; kws...)
    emit(ps.stream, args...; kws...)
end

function emit_diagnostic(ps::ParseState, args...; kws...)
    emit_diagnostic(ps.stream, args...; kws...)
end

function unsafe_textbuf(ps::ParseState)
    unsafe_textbuf(ps.stream)
end

function first_child_position(ps::ParseState, pos::ParseStreamPosition)
    first_child_position(ps.stream, pos)
end

#-------------------------------------------------------------------------------
# Parser Utils

# Bump an expected closing token.  If not found, discard unexpected tokens
# until we find it or another closing token.
#
# Crude recovery heuristic: bump any tokens which aren't block or bracket
# closing tokens.
function bump_closing_token(ps, closing_kind, alternative_closer_hint=nothing)
    # todo: Refactor with recover() ?
    if peek(ps) == closing_kind
        bump_trivia(ps)
        bump(ps, TRIVIA_FLAG)
        return
    end
    errmsg = "Expected `$(untokenize(closing_kind))`"
    if !isnothing(alternative_closer_hint)
        errmsg *= alternative_closer_hint
    end
    # We didn't find the closing token. Read ahead in the stream
    mark = position(ps)
    emit_diagnostic(ps, mark, mark, error=errmsg)
    while true
        k = peek(ps)
        if is_closing_token(ps, k) && !(k in KSet", ;")
            break
        end
        bump(ps)
    end
    # mark as trivia => ignore in AST.
    emit(ps, mark, K"error", TRIVIA_FLAG)
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

@noinline function min_supported_version_err(ps, mark, message, min_ver)
    major, minor = ps.stream.version
    msg = "$message not supported in Julia version $major.$minor < $(min_ver.major).$(min_ver.minor)"
    emit(ps, mark, K"error", error=msg)
end

# Emit an error if the version is less than `min_ver`
function min_supported_version(min_ver, ps, mark, message)
    if ps.stream.version < (min_ver.major, min_ver.minor)
        min_supported_version_err(ps, mark, message, min_ver)
    end
end

# flisp: disallow-space
function bump_disallowed_space(ps)
    if preceding_whitespace(peek_token(ps))
        bump_trivia(ps, TRIVIA_FLAG, skip_newlines=false,
                    error="whitespace is not allowed here")
    end
end

function bump_semicolon_trivia(ps)
    while peek(ps) in KSet"; NewlineWs"
        bump(ps, TRIVIA_FLAG)
    end
end

#-------------------------------------------------------------------------------
# Parsing-specific predicates on tokens/kinds
#
# All these take either a raw kind or a token.

function is_plain_equals(t)
    kind(t) == K"=" && !is_decorated(t)
end

function is_closing_token(ps::ParseState, k)
    k = kind(k)
    return k in KSet"else elseif catch finally , ) ] } ; EndMarker" ||
        (k == K"end" && !ps.end_symbol)
end

function is_block_continuation_keyword(ps::ParseState, k)
    is_block_continuation_keyword(k) && !(ps.end_symbol && k == K"end")
end

function is_closer_or_newline(ps::ParseState, k)
    is_closing_token(ps,k) || k == K"NewlineWs"
end

function is_initial_reserved_word(ps::ParseState, k)
    k = kind(k)
    is_iresword = k in KSet"begin while if for try return break continue function
                            macro quote let local global const do struct module
                            baremodule using import export"
    # `begin` means firstindex(a) inside a[...]
    return is_iresword && !(k == K"begin" && ps.end_symbol)
end

function is_reserved_word(k)
    k = kind(k)
    is_keyword(k) && !is_contextual_keyword(k)
end

# Return true if the next word (or word pair) is reserved, introducing a
# syntactic structure.
function peek_initial_reserved_words(ps::ParseState)
    k = peek(ps)
    if is_initial_reserved_word(ps, k)
        return true
    elseif is_contextual_keyword(k)
        k2 = peek(ps, 2, skip_newlines=false)
        return (k == K"mutable"   && k2 == K"struct") ||
               (k == K"primitive" && k2 == K"type")   ||
               (k == K"abstract"  && k2 == K"type")
    else
        return false
    end
end

function is_block_form(k)
    kind(k) in KSet"block quote if for while let function macro
                    abstract primitive struct try module"
end

function is_syntactic_operator(k)
    k = kind(k)
    # TODO: Do we need to disallow dotted and suffixed forms here?
    # The lexer itself usually disallows such tokens, so it's not clear whether
    # we need to handle them. (Though note `.->` is a token...)
    return k in KSet"&& || . ... ->" || (is_prec_assignment(k) && k != K"~")
end

function is_syntactic_unary_op(k)
    kind(k) in KSet"$ & ::"
end

function is_type_operator(t)
    kind(t) in KSet"<: >:" && !is_dotted(t)
end

function is_unary_op(t)
    k = kind(t)
    !is_suffixed(t) && (
        (k in KSet"<: >:" && !is_dotted(t)) ||
        k in KSet"+ - ! ~ ¬ √ ∛ ∜ ⋆ ± ∓" # dotop allowed
    )
end

# Operators that are both unary and binary
function is_both_unary_and_binary(t)
    k = kind(t)
    # Preventing is_suffixed here makes this consistent with the flisp parser.
    # But is this by design or happenstance?
    !is_suffixed(t) && (
        k in KSet"+ - ⋆ ± ∓" || (k in KSet"$ & ~" && !is_dotted(t))
    )
end

function is_string_macro_suffix(k)
    k == K"Identifier" || is_keyword(k) || is_word_operator(k) || is_number(k)
end

# flisp: invalid-identifier?
function is_valid_identifier(k)
    k = kind(k)
    !(is_syntactic_operator(k) || k in KSet"? .'")
end

# The expression is a call after stripping `where` and `::`
function was_eventually_call(ps::ParseState)
    stream = ps.stream
    p = peek_behind_pos(ps)
    while true
        b = peek_behind(stream, p)
        if b.kind == K"call"
            return true
        elseif b.kind == K"where" || b.kind == K"parens" ||
                (b.kind == K"::" && has_flags(b.flags, INFIX_FLAG))
            p = first_child_position(ps, p)
        else
            return false
        end
    end
end

function bump_dotsplit(ps, flags=EMPTY_FLAGS;
                       emit_dot_node::Bool=false, remap_kind::Kind=K"None")
    t = peek_token(ps)
    if is_dotted(t)
        bump_trivia(ps)
        mark = position(ps)
        k = remap_kind != K"None" ? remap_kind : kind(t)
        pos = bump_split(ps, (1, K".", TRIVIA_FLAG), (0, k, flags))
        if emit_dot_node
            pos = emit(ps, mark, K".")
        end
    else
        if remap_kind != K"None"
            pos = bump(ps, remap_kind=remap_kind)
        else
            pos = bump(ps)
        end
    end
    return pos
end

#-------------------------------------------------------------------------------
# Parser
#
# The definitions and top-level comments here were copied to match the
# structure of Julia's previous flisp-based parser to make both codebases
# mutually understandable and make porting changes simple.
#
# The `parse_*` functions are listed here roughly in order of increasing
# precedence (lowest to highest binding power). A few helper functions are
# interspersed.

# parse left-to-right binary operator
# produces structures like (+ (+ (+ 2 3) 4) 5)
#
# flisp: parse-LtoR
function parse_LtoR(ps::ParseState, down, is_op)
    mark = position(ps)
    down(ps)
    while is_op(peek(ps))
        t = peek_token(ps)
        bump_dotsplit(ps)
        down(ps)
        emit(ps, mark, is_dotted(t) ? K"dotcall" : K"call", INFIX_FLAG)
    end
end

# parse right-to-left binary operator
# produces structures like (= a (= b (= c d)))
#
# flisp: parse-RtoL
function parse_RtoL(ps::ParseState, down, is_op, self)
    mark = position(ps)
    down(ps)
    t = peek_token(ps)
    if is_op(kind(t))
        bump_dotsplit(ps)
        self(ps)
        emit(ps, mark, is_dotted(t) ? K"dotcall" : K"call", INFIX_FLAG)
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
    n_delims = 0
    if k in delimiters
        # allow leading delimiters
        # ; a  ==>  (block a)
    else
        # a ; b  ==>  (block a b)
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

# Parse a sequence of top level statements separated by newlines, all wrapped
# in a toplevel node.
#
#   a \n b ==>  (toplevel a b)
#
# Note that parse_stmts can also emit toplevel nodes for semicolon-separated
# statements, so it's possible for these to be nested one level deep.
#
#   a;b \n c;d  ==>  (toplevel (toplevel a b) (toplevel c d))
function parse_toplevel(ps::ParseState)
    mark = position(ps)
    while true
        if peek(ps, skip_newlines=true) == K"EndMarker"
            # Allow end of input if there is nothing left but whitespace
            # a \n \n ==> (toplevel a)
            # Empty files
            #  ==> (toplevel)
            bump_trivia(ps)
            break
        else
            parse_stmts(ps)
        end
    end
    emit(ps, mark, K"toplevel")
    nothing
end

# Parse a newline or semicolon-delimited list of expressions.
# Repeated delimiters are allowed but ignored
# a;b;c     ==>  (block a b c)
# a;;;b;;   ==>  (block a b)
# ;a        ==>  (block a)
# \n a      ==>  (block a)
# a \n b    ==>  (block a b)
#
# flisp: parse-block
function parse_block(ps::ParseState, down=parse_eq, mark=position(ps))
    parse_block_inner(ps::ParseState, down)
    emit(ps, mark, K"block")
end

# Parse a block, but leave emitting the block up to the caller.
function parse_block_inner(ps::ParseState, down)
    parse_Nary(ps, down, KSet"NewlineWs ;", KSet"end else elseif catch finally")
end

# ";" at the top level produces a sequence of top level expressions
#
# a;b;c   ==>  (toplevel a b c)
# a;;;b;; ==>  (toplevel a b)
# "x" a ; "y" b ==>  (toplevel (doc (string "x") a) (doc (string "y") b))
#
# flisp: parse-stmts
function parse_stmts(ps::ParseState)
    mark = position(ps)
    do_emit = parse_Nary(ps, parse_public, (K";",), (K"NewlineWs",))
    # check for unparsed junk after an expression
    junk_mark = position(ps)
    while peek(ps) ∉ KSet"EndMarker NewlineWs"
        # Error recovery
        bump(ps)
    end
    if junk_mark != position(ps)
        # x y  ==>  x (error-t y)
        emit(ps, junk_mark, K"error", TRIVIA_FLAG,
             error="extra tokens after end of expression")
    end
    if do_emit
        emit(ps, mark, K"toplevel", TOPLEVEL_SEMICOLONS_FLAG)
    end
end

# Parse `public foo, bar`
#
# We *only* call this from toplevel contexts (file and module level) for
# compatibility. In the future we should probably make public a full fledged
# keyword like `export`.
function parse_public(ps::ParseState)
    if ps.stream.version >= (1, 11) && peek(ps) == K"public"
        if peek(ps, 2) ∈ KSet"( = ["
            # this branch is for compatibility with use of public as a non-keyword.
            # it should be removed at some point.
            emit_diagnostic(ps, warning="using public as an identifier is deprecated")
        else
            return parse_resword(ps)
        end
    end
    parse_docstring(ps)
end

# Parse docstrings attached by a space or single newline
#
# flisp: parse-docstring
function parse_docstring(ps::ParseState, down=parse_eq)
    mark = position(ps)
    down(ps)
    if peek_behind(ps).kind == K"string"
        is_doc = true
        k = peek(ps)
        if is_closing_token(ps, k)
            # "notdoc" ] ==> (string "notdoc")
            is_doc = false
        elseif k == K"NewlineWs"
            k2 = peek(ps, 2)
            if is_closing_token(ps, k2) || k2 == K"NewlineWs"
                # "notdoc" \n]      ==> (string "notdoc")
                # "notdoc" \n\n foo ==> (string "notdoc")
                is_doc = false
            else
                # Allow a single newline
                # "doc" \n foo ==> (doc (string "doc") foo)
                bump(ps, TRIVIA_FLAG) # NewlineWs
            end
        else
            # "doc" foo    ==> (doc (string "doc") foo)
            # "doc $x" foo ==> (doc (string "doc " x) foo)
            # Allow docstrings with embedded trailing whitespace trivia
            # """\n doc\n """ foo ==> (doc (string-s "doc\n") foo)
        end
        if is_doc
            down(ps)
            emit(ps, mark, K"doc")
        end
    end
end

# Parse assignments with comma separated lists on each side
# a = b         ==>  (= a b)
# a .= b        ==>  (.= a b)
# a += b        ==>  (+= a b)
# a .+= b       ==>  (.+= a b)
# a, b = c, d   ==>  (= (tuple a b) (tuple c d))
# x, = xs       ==>  (= (tuple x) xs)
#
# flisp: parse-eq
function parse_eq(ps::ParseState)
    parse_assignment(ps, parse_comma)
end

# parse_eq_star is used where commas are special, for example in an argument list
#
# flisp: parse-eq*
function parse_eq_star(ps::ParseState)
    k = peek(ps)
    k2 = peek(ps,2)
    if (is_literal(k) || k == K"Identifier") && k2 in KSet", ) } ]"
        # optimization: skip checking the whole precedence stack if we have a
        # simple token followed by a common closing token
        bump(ps)
    else
        parse_assignment(ps, parse_pair)
    end
end

# a = b  ==>  (= a b)
#
# flisp: parse-assignment
function parse_assignment(ps::ParseState, down)
    mark = position(ps)
    down(ps)
    parse_assignment_with_initial_ex(ps, mark, down)
end

function parse_assignment_with_initial_ex(ps::ParseState, mark, down::T) where {T} # where => specialize on `down`
    t = peek_token(ps)
    k = kind(t)
    if !is_prec_assignment(k)
        return
    end
    if k == K"~"
        if ps.space_sensitive && preceding_whitespace(t) && !preceding_whitespace(peek_token(ps, 2))
            # Unary ~ in space sensitive context is not assignment precedence
            # [a ~b]  ==>  (hcat a (call-pre ~ b))
            return
        end
        # ~ is the only non-syntactic assignment-precedence operator.
        # a ~ b      ==>  (call-i a ~ b)
        # a .~ b     ==>  (dotcall-i a ~ b)
        # [a ~ b c]  ==>  (hcat (call-i a ~ b) c)
        # [a~b]      ==>  (vect (call-i a ~ b))
        bump_dotsplit(ps)
        bump_trivia(ps)
        parse_assignment(ps, down)
        emit(ps, mark, is_dotted(t) ? K"dotcall" : K"call", INFIX_FLAG)
    else
        # a += b  ==>  (+= a b)
        # a .= b  ==>  (.= a b)
        bump(ps, TRIVIA_FLAG)
        bump_trivia(ps)
        parse_assignment(ps, down)
        emit(ps, mark, k, flags(t))
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
                emit(ps, mark, K"tuple")
            end
            return n_commas
        end
        bump(ps, TRIVIA_FLAG)
        n_commas += 1
        if is_plain_equals(peek_token(ps))
            # Allow trailing comma before `=`
            # x, = xs  ==>  (tuple x)
            continue
        end
        parse_pair(ps)
    end
end

# flisp: parse-pair
# a => b  ==>  (call-i a => b)
# a .=> b ==>  (dotcall-i a => b)
function parse_pair(ps::ParseState)
    parse_RtoL(ps, parse_cond, is_prec_pair, parse_pair)
end

# Parse short form conditional expression
# a ? b : c ==> (? a b c)
#
# flisp: parse-cond
function parse_cond(ps::ParseState)
    mark = position(ps)
    parse_arrow(ps)
    t = peek_token(ps)
    if kind(t) != K"?"
        return
    end
    if !preceding_whitespace(t)
        # a? b : c  => (? a (error-t) b c)
        bump_invisible(ps, K"error", TRIVIA_FLAG,
                       error="space required before `?` operator")
    end
    bump(ps, TRIVIA_FLAG) # ?
    t = peek_token(ps)
    if !preceding_whitespace(t)
        # a ?b : c  ==>  (? a (error-t) b c)
        bump_invisible(ps, K"error", TRIVIA_FLAG,
                       error="space required after `?` operator")
    end
    parse_eq_star(ParseState(ps, range_colon_enabled=false))
    t = peek_token(ps)
    if !preceding_whitespace(t)
        # a ? b: c  ==>  (? a b (error-t) c)
        bump_invisible(ps, K"error", TRIVIA_FLAG,
                       error="space required before `:` in `?` expression")
    end
    if kind(t) == K":"
        bump(ps, TRIVIA_FLAG)
    else
        # a ? b c  ==>  (? a b (error-t) c)
        bump_invisible(ps, K"error", TRIVIA_FLAG, error="`:` expected in `?` expression")
    end
    t = peek_token(ps; skip_newlines = true)
    if !preceding_whitespace(t)
        # a ? b :c  ==>  (? a b (error-t) c)
        bump_invisible(ps, K"error", TRIVIA_FLAG,
                       error="space required after `:` in `?` expression")
    end

    # FIXME: This is a very specific case. Error recovery should be handled more
    # generally elsewhere.
    if is_block_continuation_keyword(ps, kind(t))
        # a "continuaton keyword" is likely to belong to the surrounding code, so
        # we abort early

        # if true; x ? true elseif true end  ==> (if true (block (if x true (error-t) (error-t))) (elseif true (block)))
        # if true; x ? true end  ==> (if true (block (if x true (error-t) (error-t))))
        # if true; x ? true\n end  ==> (if true (block (if x true (error-t) (error-t))))
        # if true; x ? true : elseif true end  ==> (if true (block (if x true (error-t))) (elseif true (block)))
        bump_invisible(ps, K"error", TRIVIA_FLAG, error="unexpected `$(kind(t))`")
        emit(ps, mark, K"if")
        return
    else
        # A[x ? y : end] ==> (ref A (? x y end))
    end
    parse_eq_star(ps)
    emit(ps, mark, K"?")
end

# Parse arrows.  Like parse_RtoL, but specialized for --> syntactic operator
#
# flisp: parse-arrow
function parse_arrow(ps::ParseState)
    mark = position(ps)
    parse_or(ps)
    t = peek_token(ps)
    k = kind(t)
    if is_prec_arrow(k)
        if kind(t) == K"-->" && !is_decorated(t)
            # x --> y   ==>  (--> x y)           # The only syntactic arrow
            bump(ps, TRIVIA_FLAG)
            parse_arrow(ps)
            emit(ps, mark, k, flags(t))
        else
            # x → y     ==>  (call-i x → y)
            # x <--> y  ==>  (call-i x <--> y)
            # x .--> y  ==>  (dotcall-i x --> y)
            # x -->₁ y  ==>  (call-i x -->₁ y)
            bump_dotsplit(ps)
            parse_arrow(ps)
            emit(ps, mark, is_dotted(t) ? K"dotcall" : K"call", INFIX_FLAG)
        end
    end
end

# Like parse_RtoL, but specialized for the version test of dotted operators.
function parse_lazy_cond(ps::ParseState, down, is_op, self)
    mark = position(ps)
    down(ps)
    t = peek_token(ps)
    k = kind(t)
    if is_op(k)
        bump(ps, TRIVIA_FLAG)
        self(ps)
        emit(ps, mark, k, flags(t))
        if is_dotted(t)
            min_supported_version(v"1.7", ps, mark, "dotted operators `.||` and `.&&`")
        end
    end
end

# x || y || z   ==>   (|| x (|| y z))
#v1.6: x .|| y  ==>   (error (.|| x y))
#v1.7: x .|| y  ==>   (.|| x y)
#
# flisp: parse-or
function parse_or(ps::ParseState)
    parse_lazy_cond(ps, parse_and, is_prec_lazy_or, parse_or)
end

# x && y && z   ==>   (&& x (&& y z))
#v1.6: x .&& y  ==>   (error (.&& x y))
#v1.7: x .&& y  ==>   (.&& x y)
#
# flisp: parse-and
function parse_and(ps::ParseState)
    parse_lazy_cond(ps, parse_comparison, is_prec_lazy_and, parse_and)
end

# Parse binary comparisons and comparison chains
#
# flisp: parse-comparison
function parse_comparison(ps::ParseState, subtype_comparison=false)
    mark = position(ps)
    if subtype_comparison && is_reserved_word(peek(ps))
        # Recovery
        # struct try end  ==>  (struct (error (try)) (block))
        name = untokenize(peek(ps))
        bump(ps)
        emit(ps, mark, K"error", error="Invalid type name `$name`")
    else
        parse_pipe_lt(ps)
    end
    n_comparisons = 0
    op_pos = NO_POSITION
    op_dotted = false
    initial_tok = peek_token(ps)
    while (t = peek_token(ps); is_prec_comparison(t))
        n_comparisons += 1
        op_dotted = is_dotted(t)
        op_pos = bump_dotsplit(ps, emit_dot_node=true)
        parse_pipe_lt(ps)
    end
    if n_comparisons == 1
        if is_type_operator(initial_tok)
            # Type comparisons are syntactic
            # x <: y  ==>  (<: x y)
            # x >: y  ==>  (>: x y)
            reset_node!(ps, op_pos, flags=TRIVIA_FLAG)
            emit(ps, mark, kind(initial_tok))
        else
            # Normal binary comparisons
            # x < y    ==>  (call-i x < y)
            # x .< y   ==>  (dotcall-i x < y)
            if op_dotted
                # x .<: y  ==>  (dotcall-i x <: y)
                reset_node!(ps, op_pos, kind=K"TOMBSTONE", flags=TRIVIA_FLAG)
            end
            emit(ps, mark, is_dotted(initial_tok) ? K"dotcall" : K"call", INFIX_FLAG)
        end
    elseif n_comparisons > 1
        # Comparison chains
        # x < y < z    ==> (comparison x < y < z)
        # x == y < z   ==> (comparison x == y < z)
        # x .< y .< z  ==> (comparison x (. <) y (. <) z)
        # x .< y < z   ==> (comparison x (. <) y < z)
        emit(ps, mark, K"comparison")
    end
end

# x <| y <| z  ==>  (call-i x <| (call-i y <| z))
# flisp: parse-pipe<
function parse_pipe_lt(ps::ParseState)
    parse_RtoL(ps, parse_pipe_gt, is_prec_pipe_lt, parse_pipe_lt)
end

# x |> y |> z  ==>  (call-i (call-i x |> y) |> z)
# x .|> y      ==>  (dotcall-i x |> y)
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
    parse_invalid_ops(ps)
    initial_tok = peek_token(ps)
    initial_kind = kind(initial_tok)
    if initial_kind != K":" && is_prec_colon(initial_kind)
        # a..b     ==>   (call-i a .. b)
        # a … b    ==>   (call-i a … b)
        # a .… b    ==>  (dotcall-i a … b)
        bump_dotsplit(ps)
        parse_invalid_ops(ps)
        emit(ps, mark, is_dotted(initial_tok) ? K"dotcall" : K"call", INFIX_FLAG)
    elseif initial_kind == K":" && ps.range_colon_enabled
        # a ? b : c:d   ==>   (? a b (call-i c : d))
        n_colons = 0
        while peek(ps) == K":"
            if ps.space_sensitive &&
                    preceding_whitespace(peek_token(ps)) &&
                    !preceding_whitespace(peek_token(ps, 2))
                # Tricky cases in space sensitive mode
                # [1 :a]      ==>  (hcat 1 (quote-: a))
                # [1 2:3 :a]  ==>  (hcat 1 (call-i 2 : 3) (quote-: a))
                break
            end
            t2 = peek_token(ps,2)
            if kind(t2) in KSet"< >" && !preceding_whitespace(t2)
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
                parse_invalid_ops(ps)
                emit(ps, mark, K"call", INFIX_FLAG)
                break
            end
            n_colons += 1
            bump(ps, n_colons == 1 ? EMPTY_FLAGS : TRIVIA_FLAG)
            had_newline = peek(ps) == K"NewlineWs"
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
            if had_newline
                # Error message for people coming from python
                # 1:\n2   ==> (call-i 1 : (error))
                # (1:\n2) ==> (parens (call-i 1 : 2))
                emit_diagnostic(ps, whitespace=true,
                                error="line break after `:` in range expression")
                bump_invisible(ps, K"error")
                emit(ps, mark, K"call", INFIX_FLAG)
                return
            end
            parse_invalid_ops(ps)
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

# Parse invalid binary operators
#
# Having this is unnecessary, but it improves error messages and the
# error-containing parse tree.
#
# a--b  ==>  (call-i a (error) b)
function parse_invalid_ops(ps::ParseState)
    mark = position(ps)
    parse_expr(ps)
    while (t = peek_token(ps); kind(t) in KSet"ErrorInvalidOperator Error**")
        bump_trivia(ps)
        bump_dotsplit(ps)
        parse_expr(ps)
        emit(ps, mark, is_dotted(t) ? K"dotcall" : K"call", INFIX_FLAG)
    end
end

# a - b - c  ==>  (call-i (call-i a - b) - c)
# a + b + c  ==>  (call-i a + b c)
# a .+ b     ==>  (dotcall-i a + b)
#
# flisp: parse-expr
function parse_expr(ps::ParseState)
    parse_with_chains(ps, parse_term, is_prec_plus, KSet"+ ++")
end

# a * b * c  ==>  (call-i a * b c)
#
# flisp: parse-term
function parse_term(ps::ParseState)
    parse_with_chains(ps, parse_rational, is_prec_times, KSet"*")
end

# Parse left to right, combining any of `chain_ops` into one call
#
# flisp: parse-with-chains
function parse_with_chains(ps::ParseState, down, is_op, chain_ops)
    mark = position(ps)
    down(ps)
    while (t = peek_token(ps); is_op(kind(t)))
        if ps.space_sensitive && preceding_whitespace(t) &&
                is_both_unary_and_binary(t) &&
                !preceding_whitespace(peek_token(ps, 2))
            # The following is two elements of a hcat
            # [x +y]     ==>  (hcat x (call-pre + y))
            # [x+y +z]   ==>  (hcat (call-i x + y) (call-pre + z))
            # Conversely the following are infix calls
            # [x +₁y]    ==>  (vect (call-i x +₁ y))
            # [x+y+z]    ==>  (vect (call-i x + y z))
            # [x+y + z]  ==>  (vect (call-i x + y z))
            break
        end
        bump_dotsplit(ps)
        down(ps)
        if kind(t) in chain_ops && !is_decorated(t)
            # a + b + c    ==>  (call-i a + b c)
            # a + b .+ c   ==>  (dotcall-i (call-i a + b) + c)
            parse_chain(ps, down, kind(t))
        end
        # a +₁ b +₁ c  ==>  (call-i (call-i a +₁ b) +₁ c)
        # a .+ b .+ c  ==>  (dotcall-i (dotcall-i a + b) + c)
        emit(ps, mark, is_dotted(t) ? K"dotcall" : K"call", INFIX_FLAG)
    end
end

# parse left to right chains of a given binary operator
#
# flisp: parse-chain
function parse_chain(ps::ParseState, down, op_kind)
    while (t = peek_token(ps); kind(t) == op_kind && !is_decorated(t))
        if ps.space_sensitive && preceding_whitespace(t) &&
            is_both_unary_and_binary(t) &&
            !preceding_whitespace(peek_token(ps, 2))
            # [x +y]  ==>  (hcat x (call-pre + y))
            break
        end
        bump(ps, TRIVIA_FLAG)
        down(ps)
    end
end

# flisp: parse-rational
# x // y // z  ==>  (call-i (call-i x // y) // z)
function parse_rational(ps::ParseState)
    parse_LtoR(ps, parse_shift, is_prec_rational)
end

# flisp: parse-shift
# x >> y >> z  ==>  (call-i (call-i x >> y) >> z)
function parse_shift(ps::ParseState)
    parse_LtoR(ps, parse_unary_subtype, is_prec_bitshift)
end

# parse `<: A where B` as `<: (A where B)` (issue #21545)
#
# flisp: parse-unary-subtype
function parse_unary_subtype(ps::ParseState)
    t = peek_token(ps)
    if is_type_operator(t)
        k2 = peek(ps, 2)
        if is_closing_token(ps, k2) || k2 in KSet"NewlineWs ="
            # return operator by itself
            # <: )  ==>  <:
            # <: \n ==>  <:
            # <: =  ==>  <:
            bump(ps)
        elseif k2 in KSet"{ ("
            # parse <:{T}(x::T) or <:(x::T) like other unary operators
            # <:{T}(x::T)  ==>  (call (curly <: T) (:: x T))
            # <:(x::T)     ==>  (<:-pre (parens (:: x T)))
            parse_where(ps, parse_juxtapose)
        else
            # <: x          ==>  (<:-pre x)
            # <: A where B  ==>  (<:-pre (where A B))
            # <: <: x       ==>  (<:-pre (<:-pre x))
            mark = position(ps)
            bump(ps, TRIVIA_FLAG)
            parse_unary_subtype(ps)
            emit(ps, mark, kind(t), PREFIX_OP_FLAG)
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
        bump_trivia(ps)
        k = peek(ps)
        if k == K"{"
            # x where \n {T}  ==>  (where x (braces T))
            # x where {T,S}  ==>  (where x (braces T S))
            # Also various nonsensical forms permitted
            # x where {T S}  ==>  (where x (bracescat (row T S)))
            # x where {y for y in ys}  ==>  (where x (braces (generator y (= y ys))))
            m = position(ps)
            bump(ps, TRIVIA_FLAG)
            ckind, cflags = parse_cat(ps, K"}", ps.end_symbol)
            emit_braces(ps, m, ckind, cflags)
            emit(ps, mark, K"where")
        else
            # x where T     ==>  (where x T)
            # x where \n T  ==>  (where x T)
            # x where T<:S  ==>  (where x (<: T S))
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

# Juxtaposition. Kinda ugh but soo useful for units and Field identities like `im`
#
# flisp: parse-juxtapose
function parse_juxtapose(ps::ParseState)
    mark = position(ps)
    parse_unary(ps)
    n_terms = 1
    while true
        t = peek_token(ps)
        k = kind(t)
        prev_k = peek_behind(ps).kind
        is_juxtapose = false
        if !preceding_whitespace(t) &&
                (is_number(prev_k) ||
                    (!is_number(k) &&  # disallow "x.3" and "f(2)2"
                     k != K"@"     &&  # disallow "x@y"
                     !(is_block_form(prev_k)         ||
                       is_syntactic_unary_op(prev_k) ||
                       is_initial_reserved_word(ps, prev_k) )))  &&
                (!is_operator(k) || is_radical_op(k))            &&
                !is_closing_token(ps, k)
            if prev_k == K"string" || is_string_delim(t)
                bump_invisible(ps, K"error", TRIVIA_FLAG,
                               error="cannot juxtapose string literal")
                # JuliaLang/julia#20575
                # Error, but assume juxtapose for recovery
                # "a""b"  ==>  (juxtapose (string "a") (error-t) (string "b"))
                # "a"x    ==>  (juxtapose (string "a") (error-t) x)
                # "$y"x   ==>  (juxtapose (string y) (error-t) x)
                # "a"begin end  ==> (juxtapose (string \"a\") (error-t) (block))
                is_juxtapose = true
            elseif !is_initial_reserved_word(ps, k)
                # 2x       ==>  (juxtapose 2 x)
                # 2(x)     ==>  (juxtapose 2 (parens x))
                # (2)(3)x  ==>  (juxtapose (parens 2) (parens 3) x)
                # (x-1)y   ==>  (juxtapose (parens (call-i x - 1)) y)
                # x'y      ==>  (juxtapose (call-post x ') y)
                # 1√x      ==>  (juxtapose 1 (call-pre √ x))
                is_juxtapose = true
            end
        end
        if !is_juxtapose
            # x.3       ==>  x
            # f(2)2     ==>  (call f 2)
            # x' y      ==>  (call-post x ')
            # x 'y      ==>  x
            # x@y       ==>  x
            break
        end
        if is_radical_op(t)
            parse_unary(ps)
        else
            parse_factor(ps)
        end
        n_terms += 1
    end
    if n_terms > 1
        emit(ps, mark, K"juxtapose")
    end
end

# Parse numeric literal prefixes, calls to unary operators and prefix
# calls involving arbitrary operators with bracketed arglists (as opposed to
# infix notation)
#
# flisp: parse-unary, parse-unary-call
function parse_unary(ps::ParseState)
    mark = position(ps)
    bump_trivia(ps)
    op_t = peek_token(ps)
    op_k = kind(op_t)
    if (
            !is_operator(op_k)           ||
            is_word_operator(op_k)       ||
            (op_k in KSet": ' .'")       ||
            (is_syntactic_unary_op(op_k) && !is_dotted(op_t)) ||
            is_syntactic_operator(op_k)
        )
        # `op_t` is not an initial operator
        # :T      ==>  (quote-: T)
        # in::T   ==>  (:: in T)
        # isa::T  ==>  (:: isa T)
        parse_factor(ps)
        return
    end
    t2 = peek_token(ps, 2)
    k2 = kind(t2)
    if op_k in KSet"- +" && !is_decorated(op_t)
        if !preceding_whitespace(t2) && (k2 in KSet"Integer Float Float32" ||
                                         (op_k == K"+" && k2 in KSet"BinInt HexInt OctInt"))

            k3 = peek(ps, 3)
            if is_prec_power(k3) || k3 in KSet"[ {"
                # `[`, `{` (issue #18851) and `^` have higher precedence than
                # unary negation
                # -2^x      ==>  (call-pre - (call-i 2 ^ x))
                # -2[1, 3]  ==>  (call-pre - (ref 2 1 3))
                bump(ps)
                parse_factor(ps)
                emit(ps, mark, K"call", PREFIX_OP_FLAG)
            else
                # We have a signed numeric literal. Glue the operator to the
                # next token to create a signed literal:
                # -2      ==>  -2
                # +2.0    ==>  2.0
                # -1.0f0  ==>  -1.0f0
                # -2*x    ==>  (call-i -2 * x)
                # +0xff   ==>  0xff
                bump_glue(ps, kind(t2), EMPTY_FLAGS)
            end
            return
        end
    end
    if is_closing_token(ps, k2) || k2 in KSet"NewlineWs ="
        # Standalone operators parsed as `op` or `(. op)`
        # +)   ==>  +
        # +\n  ==>  +
        # + =  ==>  +
        # .+   ==>  (. +)
        # .&   ==>  (. &)
        parse_atom(ps)
    elseif k2 == K"{" || (!is_unary_op(op_t) && k2 == K"(")
        # Call with type parameters or non-unary prefix call
        # +{T}(x::T)  ==>  (call (curly + T) (:: x T))
        # *(x)  ==>  (call * x)
        # .*(x) ==>  (call .* x)
        parse_factor(ps)
    elseif k2 == K"("
        # Cases like +(a;b) are ambiguous: are they prefix calls to + with b as
        # a keyword argument, or is `a;b` a block?  We resolve this with a
        # simple heuristic: if there were any commas (or an initial splat), it
        # was a function call.
        #
        # (The flisp parser only considers commas before `;` and thus gets this
        # last case wrong)
        op_pos = bump_dotsplit(ps, emit_dot_node=true)

        space_before_paren = preceding_whitespace(t2)
        if space_before_paren
            # Setup possible whitespace error between operator and (
            ws_mark = position(ps)
            bump_trivia(ps)
            ws_error_pos = emit(ps, ws_mark, K"TOMBSTONE")
            ws_mark_end = position(ps)
        end

        mark_before_paren = position(ps)
        bump(ps, TRIVIA_FLAG) # (
        initial_semi = peek(ps, skip_newlines=true) == K";"
        opts = parse_brackets(ps, K")") do had_commas, had_splat, num_semis, num_subexprs
            is_paren_call = had_commas || had_splat               ||
                            (initial_semi && num_subexprs > 0)    ||
                            (initial_semi && num_semis == 1)      ||
                            (num_semis == 0 && num_subexprs == 0)
            return (needs_parameters=is_paren_call,
                    is_paren_call=is_paren_call,
                    is_block=!is_paren_call && num_semis > 0)
        end

        # The precedence between unary + and any following infix ^ depends on
        # whether the parens are a function call or not
        if opts.is_paren_call
            if space_before_paren
                # Whitespace not allowed before prefix function call bracket
                # + (a,b)   ==> (call + (error) a b)
                reset_node!(ps, ws_error_pos, kind=K"error")
                emit_diagnostic(ps, ws_mark, ws_mark_end,
                                error="whitespace not allowed between prefix function call and argument list")
            end
            # Prefix function calls for operators which are both binary and unary
            # +(a,b)    ==>  (call + a b)
            # +(a=1,)   ==>  (call + (= a 1))
            # +(a...)   ==>  (call + (... a))
            # +(a;b,c)  ==>  (call + a (parameters b c))
            # +(;a)     ==>  (call + (parameters a))
            # +()       ==>  (call +)
            # Prefix calls have higher precedence than ^
            # +(a,b)^2  ==>  (call-i (call + a b) ^ 2)
            # +(a,b)(x)^2  ==>  (call-i (call (call + a b) x) ^ 2)
            if is_type_operator(op_t)
                # <:(a,)  ==>  (<: a)
                emit(ps, mark, op_k)
                reset_node!(ps, op_pos, flags=TRIVIA_FLAG)
            else
                emit(ps, mark, K"call")
            end
            parse_call_chain(ps, mark)
            parse_factor_with_initial_ex(ps, mark)
        else
            # Unary function calls with brackets as grouping, not an arglist
            # .+(a)    ==>  (dotcall-pre + (parens a))
            if opts.is_block
                # +(a;b)   ==>  (call-pre + (block-p a b))
                emit(ps, mark_before_paren, K"block", PARENS_FLAG)
            else
                emit(ps, mark_before_paren, K"parens")
            end
            # Not a prefix operator call but a block; `=` is not `kw`
            # +(a=1)  ==>  (call-pre + (parens (= a 1)))
            # Unary operators have lower precedence than ^
            # +(a)^2  ==>  (call-pre + (call-i (parens a) ^ 2))
            # .+(a)^2  ==>  (dotcall-pre + (call-i (parens a) ^ 2))
            # +(a)(x,y)^2  ==>  (call-pre + (call-i (call (parens a) x y) ^ 2))
            parse_call_chain(ps, mark_before_paren)
            parse_factor_with_initial_ex(ps, mark_before_paren)
            if is_type_operator(op_t)
                # <:(a)  ==>  (<:-pre (parens a))
                emit(ps, mark, op_k, PREFIX_OP_FLAG)
                reset_node!(ps, op_pos, flags=TRIVIA_FLAG)
            else
                if is_dotted(op_t)
                    emit(ps, mark, K"dotcall", PREFIX_OP_FLAG)
                    reset_node!(ps, op_pos, kind=K"TOMBSTONE")
                else
                    emit(ps, mark, K"call", PREFIX_OP_FLAG)
                end
            end
        end
    else
        if is_unary_op(op_t)
            # Normal unary calls
            # +x  ==>  (call-pre + x)
            # √x  ==>  (call-pre √ x)
            # .~x ==>  (dotcall-pre ~ x)
            # Things which are not quite negative literals
            # -0x1 ==> (call-pre - 0x01)
            # - 2  ==> (call-pre - 2)
            # .-2  ==> (dotcall-pre - 2)
            op_pos = bump_dotsplit(ps, EMPTY_FLAGS)
        else
            # /x     ==>  (call-pre (error /) x)
            # +₁ x   ==>  (call-pre (error +₁) x)
            # .<: x  ==>  (dotcall-pre (error (. <:)) x)
            bump_dotsplit(ps, EMPTY_FLAGS, emit_dot_node=true)
            op_pos = emit(ps, mark, K"error", error="not a unary operator")
        end
        parse_unary(ps)
        if is_type_operator(op_t)
            reset_node!(ps, op_pos, flags=TRIVIA_FLAG)
            emit(ps, mark, op_k, PREFIX_OP_FLAG)
        else
            emit(ps, mark, is_dotted(op_t) ? K"dotcall" : K"call", PREFIX_OP_FLAG)
        end
    end
end

# handle ^ and .^
#
# x^y    ==>  (call-i x ^ y)
# x^y^z  ==>  (call-i x ^ (call-i y ^ z))
# x .^ y ==>  (dotcall-i x ^ y)
# begin x end::T  ==>  (:: (block x) T)
#
# flisp: parse-factor
function parse_factor(ps::ParseState)
    mark = position(ps)
    parse_call(ps)
    parse_factor_with_initial_ex(ps, mark)
end

# flisp: parse-factor-with-initial-ex
function parse_factor_with_initial_ex(ps::ParseState, mark)
    parse_decl_with_initial_ex(ps, mark)
    if (t = peek_token(ps); is_prec_power(kind(t)))
        bump_dotsplit(ps)
        parse_factor_after(ps)
        emit(ps, mark, is_dotted(t) ? K"dotcall" : K"call", INFIX_FLAG)
    end
end

# flisp: parse-factor-after
function parse_factor_after(ps::ParseState)
    parse_RtoL(ps, parse_juxtapose, is_prec_power, parse_factor_after)
end

# Parse type declarations and lambda syntax
# a::b      ==>   (::-i a b)
# a->b      ==>   (-> a b)
#
# flisp: parse-decl-with-initial-ex
function parse_decl_with_initial_ex(ps::ParseState, mark)
    while peek(ps) == K"::"
        # a::b::c   ==>   (::-i (::-i a b) c)
        bump(ps, TRIVIA_FLAG)
        parse_where(ps, parse_call)
        emit(ps, mark, K"::", INFIX_FLAG)
    end
    if peek(ps) == K"->"
        # x -> y    ==>  (-> x y)
        # a::b->c   ==>  (-> (::-i a b) c)
        bump(ps, TRIVIA_FLAG)
        # -> is unusual: it binds tightly on the left and loosely on the right.
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
# ::a  ==>  (::-pre a)
# $a   ==>  ($ a)
#
# flisp: parse-unary-prefix
function parse_unary_prefix(ps::ParseState)
    mark = position(ps)
    t = peek_token(ps)
    k = kind(t)
    if is_syntactic_unary_op(k) && !is_dotted(t)
        k2 = peek(ps, 2)
        if k in KSet"& $" && (is_closing_token(ps, k2) || k2 == K"NewlineWs")
            # &)   ==>  &
            # $\n  ==>  $
            bump(ps)
        else
            bump(ps, TRIVIA_FLAG)
            if k in KSet"& ::"
                # &a   ==>  (& a)
                parse_where(ps, parse_call)
            else
                # $a   ==>  ($ a)
                # $$a  ==>  ($ ($ a))
                # $&a  ==>  ($ (& a))
                parse_unary_prefix(ps)
            end
            # Only need PREFIX_OP_FLAG for ::
            f = k == K"::" ? PREFIX_OP_FLAG : EMPTY_FLAGS
            emit(ps, mark, k, f)
        end
    else
        # .&(x,y)  ==>  (call .& x y)
        parse_atom(ps)
    end
end

# Parses a chain of sufficies at function call precedence, leftmost binding
# tightest. This handles
#  * Bracketed calls like a() b[] c{}
#  * Field access like a.b.c
#    - Various dotted syntax like f.() and f.:x
#  * Adjoint suffix like a'
#  * String macros like a"str" b"""str""" c`str` d```str```
#
# f(a).g(b) ==> (call (. (call f a) g) b)
#
# flisp: parse-call-chain, parse-call-with-initial-ex
function parse_call_chain(ps::ParseState, mark, is_macrocall=false)
    if is_number(peek_behind(ps).kind) && peek(ps) == K"("
        # juxtaposition with numbers is multiply, not call
        # 2(x) ==> (* 2 x)
        return
    end
    # source range of the @-prefixed part of a macro
    macro_atname_range = nothing
    # $A.@x  ==>  (macrocall (. ($ A) @x))
    maybe_strmac = true
    # We record the last component of chains of dot-separated identifiers so we
    # know which identifier was the macro name.
    macro_name_position = position(ps) # points to same output span as peek_behind
    while true
        maybe_strmac_1 = false
        t = peek_token(ps)
        k = kind(t)
        if !is_macrocall && ps.space_sensitive && preceding_whitespace(t) &&
                k in KSet"( [ { \" \"\"\" ` ```"
            # [f (x)]  ==>  (hcat f (parens x))
            # [f x]    ==>  (hcat f x)
            break
        elseif is_macrocall && (preceding_whitespace(t) || !(k in KSet"( [ { ' ."))
            # Macro calls with space-separated arguments
            # @foo a b    ==> (macrocall @foo a b)
            # @foo (x)    ==> (macrocall @foo (parens x))
            # @foo (x,y)  ==> (macrocall @foo (tuple-p x y))
            # [@foo x]    ==> (vect (macrocall @foo x))
            # [@foo]      ==> (vect (macrocall @foo))
            # @var"#" a   ==> (macrocall (var @#) a)
            # A.@x y      ==> (macrocall (. A @x) y)
            # A.@var"#" a ==> (macrocall (. A (var @#)) a)
            # @+x y       ==> (macrocall @+ x y)
            # A.@.x       ==> (macrocall (. A @.) x)
            fix_macro_name_kind!(ps, macro_name_position)
            let ps = with_space_sensitive(ps)
                # Space separated macro arguments
                # A.@foo a b    ==> (macrocall (. A @foo) a b)
                # @A.foo a b    ==> (macrocall (. A @foo) a b)
                n_args = parse_space_separated_exprs(ps)
                is_doc_macro = peek_behind(ps, macro_name_position).orig_kind == K"doc"
                if is_doc_macro && n_args == 1
                    # Parse extended @doc args on next line
                    # @doc x\ny      ==>  (macrocall @doc x y)
                    # A.@doc x\ny    ==>  (macrocall (. A @doc) doc x y)
                    # @A.doc x\ny    ==>  (macrocall (. A @doc) doc x y)
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
        elseif k == K"("
            # f(a,b)  ==>  (call f a b)
            # f(a=1; b=2) ==> (call f (= a 1) (parameters (= b 2)))
            # f(a; b; c)  ==> (call f a (parameters b) (parameters c))
            # (a=1)()  ==>  (call (parens (= a 1)))
            # f (a)    ==>  (call f (error-t) a)
            bump_disallowed_space(ps)
            bump(ps, TRIVIA_FLAG)
            parse_call_arglist(ps, K")")
            if peek(ps) == K"do"
                # f(x) do y body end  ==>  (call f x (do (tuple y) (block body)))
                parse_do(ps)
            end
            emit(ps, mark, is_macrocall ? K"macrocall" : K"call",
                 is_macrocall ? PARENS_FLAG : EMPTY_FLAGS)
            if is_macrocall
                # @x(a, b)   ==>  (macrocall-p @x a b)
                # A.@x(y)    ==>  (macrocall-p (. A @x) y)
                # A.@x(y).z  ==>  (. (macrocall-p (. A @x) y) z)
                fix_macro_name_kind!(ps, macro_name_position)
                is_macrocall = false
                macro_atname_range = nothing
            end
        elseif k == K"["
            m = position(ps)
            # a [i]  ==>  (ref a (error-t) i)
            bump_disallowed_space(ps)
            bump(ps, TRIVIA_FLAG)
            ckind, cflags = parse_cat(ParseState(ps, end_symbol=true),
                                      K"]", ps.end_symbol)
            if is_macrocall
                # @S[a,b]  ==>  (macrocall @S (vect a b))
                # @S[a b]  ==>  (macrocall @S (hcat a b))
                # @S[a; b] ==>  (macrocall @S (vcat a b))
                # A.@S[a]  ==>  (macrocall (. A @S) (vect a))
                # @S[a].b  ==>  (. (macrocall @S (vect a)) b)
                #v1.7: @S[a ;; b]  ==>  (macrocall @S (ncat-2 a b))
                #v1.6: @S[a ;; b]  ==>  (macrocall @S (error (ncat-2 a b)))
                fix_macro_name_kind!(ps, macro_name_position)
                emit(ps, m, ckind, cflags)
                check_ncat_compat(ps, m, ckind)
                emit(ps, mark, K"macrocall")
                is_macrocall = false
                macro_atname_range = nothing
            else
                # a[i]    ==>  (ref a i)
                # a[i,j]  ==>  (ref a i j)
                # (a=1)[] ==>  (ref (parens (= a 1)))
                # a[end]  ==>  (ref a end)
                # T[x   y]  ==>  (typed_hcat T x y)
                # T[x ; y]  ==>  (typed_vcat T x y)
                # T[a b; c d]  ==>  (typed_vcat T (row a b) (row c d))
                # T[x for x in xs]  ==>  (typed_comprehension T (generator x (= x xs)))
                #v1.8: T[a ; b ;; c ; d]  ==>  (typed_ncat-2 T (nrow-1 a b) (nrow-1 c d))
                outk = ckind == K"vect"          ? K"ref"                  :
                       ckind == K"hcat"          ? K"typed_hcat"           :
                       ckind == K"vcat"          ? K"typed_vcat"           :
                       ckind == K"comprehension" ? K"typed_comprehension"  :
                       ckind == K"ncat"          ? K"typed_ncat"           :
                       internal_error("unrecognized kind in parse_cat ", ckind)
                emit(ps, mark, outk, cflags)
                check_ncat_compat(ps, mark, ckind)
            end
        elseif k == K"."
            # x .y  ==>  (. x (error-t) y)
            bump_disallowed_space(ps)
            emark = position(ps)
            if !isnothing(macro_atname_range)
                # Allow `@` in macrocall only in first and last position
                # A.B.@x  ==>  (macrocall (. (. A B) @x))
                # @A.B.x  ==>  (macrocall (. (. A B) @x))
                # A.@B.x  ==>  (macrocall (. (. A B (error-t)) @x))
                emit_diagnostic(ps, macro_atname_range...,
                    error="`@` must appear on first or last macro name component")
                bump(ps, TRIVIA_FLAG, error="Unexpected `.` after macro name")
            else
                bump(ps, TRIVIA_FLAG)
            end
            k = peek(ps)
            if k == K"("
                if is_macrocall
                    # @M.(x)  ==> (macrocall (dotcall @M (error-t) x))
                    bump_invisible(ps, K"error", TRIVIA_FLAG)
                    emit_diagnostic(ps, mark,
                                    error="dot call syntax not supported for macros")
                end
                # f.(a,b)   ==>  (dotcall f a b)
                # f. (x)    ==>  (dotcall f (error-t) x)
                bump_disallowed_space(ps)
                bump(ps, TRIVIA_FLAG)
                parse_call_arglist(ps, K")")
                emit(ps, mark, K"dotcall")
            elseif k == K":"
                # A.:+  ==>  (. A (quote-: +))
                # A.: +  ==>  (. A (error-t) (quote-: +))
                m = position(ps)
                bump(ps, TRIVIA_FLAG)
                bump_disallowed_space(ps)
                parse_atom(ps, false)
                emit(ps, m, K"quote", COLON_QUOTE)
                emit(ps, mark, K".")
            elseif k == K"$"
                # f.$x      ==>  (. f ($ x))
                # f.$(x+y)  ==>  (. f ($ (call + x y)))
                # A.$B.@x   ==>  (macrocall (. (. A ($ B)) @x))
                # @A.$x a   ==>  (macrocall (. A (error x)) a)
                m = position(ps)
                bump(ps, TRIVIA_FLAG)
                parse_atom(ps)
                emit(ps, m, K"$")
                macro_name_position = position(ps)
                emit(ps, mark, K".")
            elseif k == K"@"
                # A macro call after some prefix A has been consumed
                # A.@x    ==>  (macrocall (. A @x))
                # A.@x a  ==>  (macrocall (. A @x) a)
                m = position(ps)
                if is_macrocall
                    # @A.B.@x a ==> (macrocall (. (. A B) (error-t) @x) a)
                    bump(ps, TRIVIA_FLAG, error="repeated `@` in macro module path")
                else
                    bump(ps, TRIVIA_FLAG)
                    is_macrocall = true
                end
                parse_macro_name(ps)
                macro_name_position = position(ps)
                macro_atname_range = (m, position(ps))
                emit(ps, mark, K".")
            elseif k == K"'"
                # TODO: Reclaim dotted postfix operators :-)
                # f.'  =>  f (error-t ')
                bump(ps)
                emit(ps, emark, K"error", TRIVIA_FLAG,
                     error="the .' operator for transpose is discontinued")
            else
                # Field/property syntax
                # f.x.y ==> (. (. f x) y)
                parse_atom(ps, false)
                macro_name_position = position(ps)
                maybe_strmac_1 = true
                emit(ps, mark, K".")
            end
        elseif k == K"'" && !preceding_whitespace(t)
            # f'  ==> (call-post f ')
            # f'ᵀ ==> (call-post f 'ᵀ)
            bump(ps)
            emit(ps, mark, K"call", POSTFIX_OP_FLAG)
        elseif k == K"{"
            # Type parameter curlies and macro calls
            m = position(ps)
            # S {a} ==> (curly S (error-t) a)
            bump_disallowed_space(ps)
            bump(ps, TRIVIA_FLAG)
            parse_call_arglist(ps, K"}")
            if is_macrocall
                # @S{a,b} ==> (macrocall S (braces a b))
                # A.@S{a}  ==> (macrocall (. A @S) (braces a))
                # @S{a}.b  ==> (. (macrocall @S (braces a)) b)
                fix_macro_name_kind!(ps, macro_name_position)
                emit(ps, m, K"braces")
                emit(ps, mark, K"macrocall")
                min_supported_version(v"1.6", ps, mark, "macro call without space before `{}`")
                is_macrocall = false
                macro_atname_range = nothing
            else
                # S{a,b} ==> (curly S a b)
                emit(ps, mark, K"curly")
            end
        elseif k in KSet" \" \"\"\" ` ``` " &&
                !preceding_whitespace(t) && maybe_strmac &&
                (# Must mirror the logic in lex_quote() for consistency
                 origk = peek_behind(ps, macro_name_position).orig_kind;
                 origk == K"Identifier" || is_contextual_keyword(origk) || is_word_operator(origk))
            # Custom string and command literals
            # x"str" ==> (macrocall @x_str (string-r "str"))
            # x`str` ==> (macrocall @x_cmd (cmdstring-r "str"))
            # x""    ==> (macrocall @x_str (string-r ""))
            # x``    ==> (macrocall @x_cmd (cmdstring-r ""))
            # Triple quoted procesing for custom strings
            # r"""\nx"""          ==> (macrocall @r_str (string-s-r "x"))
            # r"""\n x\n y"""     ==> (macrocall @r_str (string-s-r "x\n" "y"))
            # r"""\n x\\n y"""    ==> (macrocall @r_str (string-s-r "x\\\n" "y"))
            #
            # Use a special token kind for string and cmd macro names so the
            # names can be expanded later as necessary.
            outk = is_string_delim(k) ? K"StringMacroName" : K"CmdMacroName"
            fix_macro_name_kind!(ps, macro_name_position, outk)
            parse_string(ps, true)
            t = peek_token(ps)
            k = kind(t)
            if !preceding_whitespace(t) && is_string_macro_suffix(k)
                # Macro sufficies can include keywords and numbers
                # x"s"y    ==> (macrocall @x_str (string-r "s") "y")
                # x"s"end  ==> (macrocall @x_str (string-r "s") "end")
                # x"s"in   ==> (macrocall @x_str (string-r "s") "in")
                # x"s"2    ==> (macrocall @x_str (string-r "s") 2)
                # x"s"10.0 ==> (macrocall @x_str (string-r "s") 10.0)
                suffix_kind = (k == K"Identifier" || is_keyword(k) ||
                               is_word_operator(k)) ? K"String" : k
                bump(ps, remap_kind=suffix_kind)
            end
            emit(ps, mark, K"macrocall")
        else
            break
        end
        maybe_strmac = maybe_strmac_1
    end
end

# Parse the `A<:B` part of type definitions like `struct A<:B end`
#
# flisp: parse-subtype-spec
function parse_subtype_spec(ps::ParseState)
    # Wart: why isn't the flisp parser more strict here?
    # <: is the only operator which isn't a syntax error, but
    # parse_comparison allows all sorts of things.
    parse_comparison(ps, true)
end

# flisp: parse-struct-field
function parse_struct_field(ps::ParseState)
    mark = position(ps)
    const_field = peek(ps) == K"const"
    if const_field
        bump(ps, TRIVIA_FLAG)
    end
    parse_eq(ps)
    if const_field
        # Const fields https://github.com/JuliaLang/julia/pull/43305
        #v1.8: struct A const a end  ==>  (struct A (block (const x)))
        #v1.7: struct A const a end  ==>  (struct A (block (error (const x))))
        emit(ps, mark, K"const")
        min_supported_version(v"1.8", ps, mark, "`const` struct field")
    end
end

# parse expressions or blocks introduced by syntactic reserved words.
#
# The caller should use peek_initial_reserved_words to determine whether
# to call parse_resword, or whether contextual keywords like `mutable` are
# simple identifiers.
#
# flisp: parse-resword
function parse_resword(ps::ParseState)
    # In normal_context
    # begin f() where T = x end  ==>  (block (= (where (call f) T) x))
    ps = normal_context(ps)
    bump_trivia(ps)
    mark = position(ps)
    word = peek(ps)
    if word in KSet"begin quote"
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
        # while x < y \n a \n b \n end ==> (while (call-i x < y) (block a b))
        bump(ps, TRIVIA_FLAG)
        parse_cond(ps)
        parse_block(ps)
        bump_closing_token(ps, K"end")
        emit(ps, mark, K"while")
    elseif word == K"for"
        # for x in xs end  ==>  (for (= x xs) (block))
        # for x in xs, y in ys \n a \n end ==> (for (cartesian_iterator (= x xs) (= y ys)) (block a))
        bump(ps, TRIVIA_FLAG)
        parse_iteration_specs(ps)
        parse_block(ps)
        bump_closing_token(ps, K"end")
        emit(ps, mark, K"for")
    elseif word == K"let"
        bump(ps, TRIVIA_FLAG)
        m = position(ps)
        if peek(ps) in KSet"NewlineWs ;"
            # let end           ==>  (let (block) (block))
            # let ; end         ==>  (let (block) (block))
            # let ; body end    ==>  (let (block) (block body))
        else
            # let x=1\n end     ==>  (let (block (= x 1)) (block))
            # let x=1 ; end     ==>  (let (block (= x 1)) (block))
            # let x::1 ; end    ==>  (let (block (::-i x 1)) (block))
            # let x ; end       ==>  (let (block x) (block))
            # let x=1,y=2 ; end ==>  (let (block (= x 1) (= y 2) (block)))
            # let x+=1 ; end    ==>  (let (block (+= x 1)) (block))
            parse_comma_separated(ps, parse_eq_star)
        end
        emit(ps, m, K"block")
        k = peek(ps)
        if k in KSet"NewlineWs ;"
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
    elseif word in KSet"global local"
        # global x   ==>  (global x)
        # local x    ==>  (local x)
        bump(ps, TRIVIA_FLAG)
        const_mark = nothing
        if peek(ps) == K"const"
            const_mark = position(ps)
            bump(ps, TRIVIA_FLAG)
        end
        had_assignment = parse_global_local_const_vars(ps)
        if !isnothing(const_mark)
            # global const x = 1  ==>  (global (const (= x 1)))
            # local const x = 1   ==>  (local (const (= x 1)))
            emit(ps, const_mark, K"const")
            if !had_assignment
                # global const x  ==>  (global (error (const x)))
                emit(ps, mark, K"error", error="expected assignment after `const`")
            end
        end
        emit(ps, mark, word)
    elseif word == K"const"
        # const x = 1  ==>  (const (= x 1))
        bump(ps, TRIVIA_FLAG)
        scope_mark = nothing
        scope_k = peek(ps)
        if scope_k in KSet"local global"
            scope_mark = position(ps)
            bump(ps, TRIVIA_FLAG)
        end
        had_assignment = parse_global_local_const_vars(ps)
        if !isnothing(scope_mark)
            # const global x = 1  ==>  (const (global (= x 1)))
            # const local x = 1   ==>  (const (local (= x 1)))
            emit(ps, scope_mark, scope_k)
        end
        emit(ps, mark, K"const")
        if !had_assignment
            # const x .= 1  ==>  (error (const (.= x 1)))
            emit(ps, mark, K"error", error="expected assignment after `const`")
        end
    elseif word in KSet"function macro"
        bump(ps, TRIVIA_FLAG)
        bump_trivia(ps)
        has_body = parse_function_signature(ps, word == K"function")
        if has_body
            # The function body
            # function f() \n a \n b end  ==> (function (call f) (block a b))
            # function f() end            ==> (function (call f) (block))
            parse_block(ps)
            bump_closing_token(ps, K"end")
            emit(ps, mark, word)
        else
            # Function/macro definition with no methods
            # function f end       ==> (function f)
            # (function f \n end)  ==> (parens (function f))
            # function f \n\n end  ==> (function f)
            # function $f end      ==> (function ($ f))
            # macro f end          ==> (macro f)
            bump(ps, TRIVIA_FLAG, skip_newlines=true)
            emit(ps, mark, word)
        end
    elseif word == K"abstract"
        # Abstract type definitions
        # abstract type A end             ==>  (abstract A)
        # abstract type A ; end             ==>  (abstract A)
        # abstract type \n\n A \n\n end   ==>  (abstract A)
        # abstract type A <: B end        ==>  (abstract (<: A B))
        # abstract type A <: B{T,S} end   ==>  (abstract (<: A (curly B T S)))
        # Oddities allowed by parser
        # abstract type A < B end         ==>  (abstract (call-i A < B))
        bump(ps, TRIVIA_FLAG)
        @check peek(ps) == K"type"
        bump(ps, TRIVIA_FLAG)
        parse_subtype_spec(ps)
        bump_semicolon_trivia(ps)
        bump_closing_token(ps, K"end")
        emit(ps, mark, K"abstract")
    elseif word in KSet"struct mutable"
        # struct A <: B \n a::X \n end  ==>  (struct (<: A B) (block (::-i a X)))
        # struct A \n a \n b \n end  ==>  (struct A (block a b))
        #v1.7: struct A const a end  ==>  (struct A (block (error (const a))))
        #v1.8: struct A const a end  ==>  (struct A (block (const a)))
        is_mut = word == K"mutable"
        if is_mut
            # mutable struct A end  ==>  (struct-mut A (block))
            bump(ps, TRIVIA_FLAG)
        else
            # struct A end  ==>  (struct A (block))
        end
        @check peek(ps) == K"struct"
        bump(ps, TRIVIA_FLAG)
        parse_subtype_spec(ps)
        parse_block(ps, parse_struct_field)
        bump_closing_token(ps, K"end")
        emit(ps, mark, K"struct", is_mut ? MUTABLE_FLAG : EMPTY_FLAGS)
    elseif word == K"primitive"
        # primitive type A 32 end             ==> (primitive A 32)
        # primitive type A 32 ; end           ==> (primitive A 32)
        # primitive type A $N end             ==> (primitive A ($ N))
        # primitive type A <: B \n 8 \n end   ==> (primitive (<: A B) 8)
        bump(ps, TRIVIA_FLAG)
        @check peek(ps) == K"type"
        bump(ps, TRIVIA_FLAG)
        let ps = with_space_sensitive(ps)
            parse_subtype_spec(ps)
            parse_cond(ps)
        end
        bump_semicolon_trivia(ps)
        bump_closing_token(ps, K"end")
        emit(ps, mark, K"primitive")
    elseif word == K"try"
        parse_try(ps)
    elseif word == K"return"
        bump(ps, TRIVIA_FLAG)
        k = peek(ps)
        if k == K"NewlineWs" || is_closing_token(ps, k)
            # return\nx   ==>  (return)
            # return)     ==>  (return)
        else
            # return x    ==>  (return x)
            # return x,y  ==>  (return (tuple x y))
            parse_eq(ps)
        end
        emit(ps, mark, K"return")
    elseif word in KSet"break continue"
        # break     ==>  (break)
        # continue  ==>  (continue)
        bump(ps, TRIVIA_FLAG)
        emit(ps, mark, word)
        k = peek(ps)
        if !(k in KSet"NewlineWs ; ) : EndMarker" || (k == K"end" && !ps.end_symbol))
            recover(is_closer_or_newline, ps, TRIVIA_FLAG,
                    error="unexpected token after $(untokenize(word))")
        end
    elseif word in KSet"module baremodule"
        # module A end  ==> (module A (block))
        # baremodule A end ==> (module-bare A (block))
        bump(ps, TRIVIA_FLAG)
        if is_reserved_word(peek(ps))
            # module do \n end  ==>  (module (error do) (block))
            bump(ps, error="Invalid module name")
        else
            # module $A end  ==>  (module ($ A) (block))
            parse_unary_prefix(ps)
        end
        # module A \n a \n b \n end  ==>  (module A (block a b))
        # module A \n "x"\na \n end  ==>  (module A (block (doc (string "x") a)))
        parse_block(ps, parse_public)
        bump_closing_token(ps, K"end")
        emit(ps, mark, K"module",
             word == K"baremodule" ? BARE_MODULE_FLAG : EMPTY_FLAGS)
    elseif word in KSet"export public"
        # export a         ==>  (export a)
        # export @a        ==>  (export @a)
        # export a, \n @b  ==>  (export a @b)
        # export +, ==     ==>  (export + ==)
        # export \n a      ==>  (export a)
        # export \$a, \$(a*b) ==> (export (\$ a) (\$ (parens (call-i a * b))))
        bump(ps, TRIVIA_FLAG)
        parse_comma_separated(ps, x->parse_atsym(x, false))
        emit(ps, mark, word)
    elseif word in KSet"import using"
        parse_imports(ps)
    elseif word == K"do"
        bump(ps, TRIVIA_FLAG, error="invalid `do` syntax")
    else
        internal_error("unhandled reserved word ", word)
    end
end

# Parse if-elseif-else-end expressions
#
# if a xx elseif b yy else zz end   ==>  (if a (block xx) (elseif b (block yy) (block zz)))
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
    if peek(ps) in KSet"NewlineWs end"
        # if end      ==>  (if (error) (block))
        # if \n end   ==>  (if (error) (block))
        bump_trivia(ps, error="missing condition in `$(untokenize(word))`")
    else
        # if a end      ==>  (if a (block))
        # if a xx end   ==>  (if a (block xx))
        parse_cond(ps)
    end
    # if a \n\n xx \n\n end   ==>  (if a (block xx))
    parse_block(ps)
    bump_trivia(ps)
    k = peek(ps)
    if k == K"elseif"
        # if a xx elseif b yy end   ==>  (if a (block xx) (elseif b (block yy)))
        parse_if_elseif(ps, true)
    elseif k == K"else"
        emark = position(ps)
        bump(ps, TRIVIA_FLAG)
        if peek(ps) == K"if"
            # Recovery: User wrote `else if` by mistake ?
            # if a xx else if b yy end  ==>  (if a (block xx) (error-t) (elseif b (block yy)))
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

# Like parse_assignment, but specialized so that we can omit the
# tuple when there's commas but no assignment.
function parse_global_local_const_vars(ps)
    mark = position(ps)
    n_commas = parse_comma(ps, false)
    t = peek_token(ps)
    if is_prec_assignment(t)
        if n_commas >= 1
            # const x,y = 1,2  ==>  (const (= (tuple x y) (tuple 1 2)))
            emit(ps, mark, K"tuple")
        end
        # const x = 1   ==>  (const (= x 1))
        # global x ~ 1  ==>  (global (call-i x ~ 1))
        # global x += 1 ==>  (global (+= x 1))
        parse_assignment_with_initial_ex(ps, mark, parse_comma)
    else
        # global x,y   ==>  (global x y)
    end
    return kind(t) == K"=" && !is_dotted(t)
end

# Parse function and macro definitions
function parse_function_signature(ps::ParseState, is_function::Bool)
    is_anon_func = false
    parsed_call = false
    needs_parse_call = true

    mark = position(ps)
    if !is_function
        # Parse macro name
        parse_unary_prefix(ps)
        kb = peek_behind(ps).orig_kind
        if is_initial_reserved_word(ps, kb)
            # macro while(ex) end  ==> (macro (call (error while) ex) (block))
            emit(ps, mark, K"error", error="invalid macro name")
        else
            # macro f()     end  ==>  (macro (call f) (block))
            # macro (:)(ex) end  ==>  (macro (call (parens :) ex) (block))
            # macro (type)(ex) end  ==>  (macro (call (parens type) ex) (block))
            # macro $f()    end  ==>  (macro (call ($ f)) (block))
            # macro ($f)()  end  ==>  (macro (call (parens ($ f))) (block))
        end
    else
        if peek(ps) != K"("
            # function f() end  ==> (function (call f))
            parse_unary_prefix(ps)
        else
            # When an initial parenthesis is present, we need to distinguish
            # between
            # * The function name in parens, followed by (args...)
            # * An anonymous function argument list in parens
            # * The whole function declaration, in parens
            bump(ps, TRIVIA_FLAG)
            is_empty_tuple = peek(ps, skip_newlines=true) == K")"
            opts = parse_brackets(ps, K")") do _, _, _, _
                _parsed_call = was_eventually_call(ps)
                _needs_parse_call = peek(ps, 2) ∈ KSet"( ."
                _is_anon_func = !_needs_parse_call && !_parsed_call
                return (needs_parameters = _is_anon_func,
                        is_anon_func     = _is_anon_func,
                        parsed_call      = _parsed_call,
                        needs_parse_call = _needs_parse_call)
            end
            is_anon_func = opts.is_anon_func
            parsed_call = opts.parsed_call
            needs_parse_call = opts.needs_parse_call
            if is_anon_func
                # function (x) body end ==>  (function (tuple-p x) (block body))
                # function (x::f()) end ==>  (function (tuple-p (::-i x (call f))) (block))
                # function (x,y) end    ==>  (function (tuple-p x y) (block))
                # function (x=1) end    ==>  (function (tuple-p (= x 1)) (block))
                # function (;x=1) end   ==>  (function (tuple-p (parameters (= x 1))) (block))
                emit(ps, mark, K"tuple", PARENS_FLAG)
            elseif is_empty_tuple
                # Weird case which is consistent with parse_paren but will be
                # rejected in lowering
                # function ()(x) end  ==> (function (call (tuple-p) x) (block))
                emit(ps, mark, K"tuple", PARENS_FLAG)
            else
                # function (A).f() end  ==> (function (call (. (parens A) f)) (block))
                # function (:)() end    ==> (function (call (parens :)) (block))
                # function (x::T)() end ==> (function (call (parens (::-i x T))) (block))
                # function (::T)() end  ==> (function (call (parens (::-pre T))) (block))
                # function (:*=(f))() end  ==> (function (call (parens (call (quote-: *=) f))) (block))
                emit(ps, mark, K"parens", PARENS_FLAG)
            end
        end
        if !is_anon_func
            kb = peek_behind(ps).orig_kind
            if is_reserved_word(kb)
                # function begin() end  ==>  (function (call (error begin)) (block))
                emit(ps, mark, K"error", error="invalid function name")
            else
                # function f() end     ==>  (function (call f) (block))
                # function type() end  ==>  (function (call type) (block))
                # function \n f() end  ==>  (function (call f) (block))
                # function $f() end    ==>  (function (call ($ f)) (block))
                # function (::Type{T})(x) end ==> (function (call (parens (::-pre (curly Type T))) x) (block))
            end
        end
    end
    if peek(ps, skip_newlines=true) == K"end" && !is_anon_func && !parsed_call
        return false
    end
    if needs_parse_call
        # Parse function argument list
        # function f(x,y)  end    ==>  (function (call f x y) (block))
        # function f{T}()  end    ==>  (function (call (curly f T)) (block))
        # function A.f()   end    ==>  (function (call (. A f)) (block))
        parse_call_chain(ps, mark)
        if peek_behind(ps).kind != K"call"
            # function f body end  ==>  (function (error f) (block body))
            emit(ps, mark, K"error",
                 error="Invalid signature in $(is_function ? "function" : "macro") definition")
        end
    end
    if is_function && peek(ps) == K"::"
        # Function return type
        # function f()::T    end   ==>  (function (::-i (call f) T) (block))
        # function f()::g(T) end   ==>  (function (::-i (call f) (call g T)) (block))
        bump(ps, TRIVIA_FLAG)
        parse_call(ps)
        emit(ps, mark, K"::", INFIX_FLAG)
    end
    if peek(ps) == K"where"
        # Function signature where syntax
        # function f() where {T} end   ==>  (function (where (call f) (braces T)) (block))
        # function f() where T   end   ==>  (function (where (call f) T) (block))
        parse_where_chain(ps, mark)
    end
    # function f()::S where T end ==> (function (where (::-i (call f) S) T) (block))
    #
    # Ugly cases for compat where extra parentheses existed and we've
    # already parsed at least the call part of the signature
    #
    # function (f() where T) end         ==> (function (where (call f) T) (block))
    # function (f()) where T end         ==> (function (where (call f) T) (block))
    # function (f() where T) where U end ==> (function (where (where (call f) T) U) (block))
    # function (f()::S) end              ==> (function (parens (::-i (call f) S)) (block))
    # function ((f()::S) where T) end    ==> (function (where (parens (::-i (call f) S)) T) (block))
    #
    # TODO: Warn for use of parens? The precedence of `::` and
    # `where` don't work inside parens so this is a bit of a syntax
    # oddity/aberration.
    return true
end

# Parse a try block
#
# try \n x \n catch e \n y \n finally \n z end  ==>  (try (block x) (catch e (block y)) (finally (block z)))
#v1.8: try \n x \n catch e \n y \n else z finally \n w end  ==>  (try (block x) (catch e (block y)) (else (block z)) (finally (block w)))
#
# flisp: embedded in parse_resword
function parse_try(ps)
    mark = position(ps)
    bump(ps, TRIVIA_FLAG)
    parse_block(ps)
    has_catch = false
    has_finally = false
    bump_trivia(ps)
    if peek(ps) == K"catch"
        has_catch = true
        parse_catch(ps)
    end
    bump_trivia(ps)
    if peek(ps) == K"else"
        # catch-else syntax: https://github.com/JuliaLang/julia/pull/42211
        #
        #v1.8: try catch ; else end ==> (try (block) (catch false (block)) (else (block)))
        else_mark = position(ps)
        bump(ps, TRIVIA_FLAG)
        parse_block(ps)
        if !has_catch
            #v1.8: try else x finally y end ==> (try (block) (else (error (block x))) (finally (block y)))
            emit(ps, else_mark, K"error", error="Expected `catch` before `else`")
        end
        #v1.7: try catch ; else end ==> (try (block) (catch false (block)) (else (error (block))))
        min_supported_version(v"1.8", ps, else_mark, "`else` after `catch`")
        emit(ps, else_mark, K"else")
    end
    bump_trivia(ps)
    if peek(ps) == K"finally"
        finally_mark = position(ps)
        # try x finally y end  ==>  (try (block x) (finally (block y)))
        has_finally = true
        bump(ps, TRIVIA_FLAG)
        parse_block(ps)
        emit(ps, finally_mark, K"finally")
    end
    # Wart: the flisp parser allows finally before catch, the *opposite* order
    # in which these blocks execute.
    bump_trivia(ps)
    if !has_catch && peek(ps) == K"catch"
        # try x finally y catch e z end  ==>  (try (block x) (finally (block y)) (catch e (block z)))
        m = position(ps)
        parse_catch(ps)
        emit_diagnostic(ps, m,
            warning="`catch` after `finally` will execute out of order")
    end
    missing_recovery = !has_catch && !has_finally
    if missing_recovery
        # try x end  ==>  (try (block x) (error-t))
        bump_invisible(ps, K"error", TRIVIA_FLAG)
    end
    bump_closing_token(ps, K"end")
    emit(ps, mark, K"try")
    if missing_recovery
        emit_diagnostic(ps, mark, error="try without catch or finally")
    end
end

function parse_catch(ps::ParseState)
    mark = position(ps)
    bump(ps, TRIVIA_FLAG)
    k = peek(ps)
    if k in KSet"NewlineWs ;" || is_closing_token(ps, k)
        # try x catch end      ==>  (try (block x) (catch false (block)))
        # try x catch ; y end  ==>  (try (block x) (catch false (block y)))
        # try x catch \n y end ==>  (try (block x) (catch false (block y)))
        bump_invisible(ps, K"false")
    else
        # try x catch e y end   ==>  (try (block x) (catch e (block y)))
        # try x catch $e y end  ==>  (try (block x) (catch ($ e) (block y)))
        m = position(ps)
        parse_eq_star(ps)
        if !(peek_behind(ps).kind in KSet"Identifier var $")
            # try x catch e+3 y end  ==>  (try (block x) (catch (error (call-i e + 3)) (block y)))
            emit(ps, m, K"error", error="a variable name is expected after `catch`")
        end
    end
    parse_block(ps)
    emit(ps, mark, K"catch")
end

# flisp: parse-do
function parse_do(ps::ParseState)
    mark = position(ps)
    bump(ps, TRIVIA_FLAG) # do
    ps = normal_context(ps)
    m = position(ps)
    if peek(ps) in KSet"NewlineWs ;"
        # f() do\nend        ==>  (call f (do (tuple) (block)))
        # f() do ; body end  ==>  (call f (do (tuple) (block body)))
        # this trivia needs to go into the tuple due to the way position()
        # works.
        bump(ps, TRIVIA_FLAG)
    else
        # f() do x, y\n body end  ==>  (call f (do (tuple x y) (block body)))
        parse_comma_separated(ps, parse_range)
    end
    emit(ps, m, K"tuple")
    parse_block(ps)
    bump_closing_token(ps, K"end")
    emit(ps, mark, K"do")
end

function _is_valid_macro_name(peektok)
    return !is_error(peektok.kind) && (peektok.is_leaf || peektok.kind == K"var")
end

function fix_macro_name_kind!(ps::ParseState, macro_name_position, name_kind=nothing)
    k = peek_behind(ps, macro_name_position).kind
    if k == K"var"
        macro_name_position = first_child_position(ps, macro_name_position)
        k = peek_behind(ps, macro_name_position).kind
    elseif k == K"parens"
        # @(A) x  ==>  (macrocall (parens @A) x)
        macro_name_position = first_child_position(ps, macro_name_position)
        if macro_name_position == NO_POSITION
            return
        end
        k = peek_behind(ps, macro_name_position).kind
    elseif k == K"error"
        # Error already reported in parse_macro_name
        return
    end
    if isnothing(name_kind)
        name_kind = _is_valid_macro_name(peek_behind(ps, macro_name_position)) ?
                    K"MacroName" : K"error"
        if name_kind == K"error"
            # TODO: This isn't quite accurate
            emit_diagnostic(ps, macro_name_position, macro_name_position,
                            error="invalid macro name")
        end
    end
    reset_node!(ps, macro_name_position, kind=name_kind)
end

# If remap_kind is false, the kind will be remapped by parse_call_chain after
# it discovers which component of the macro's module path is the macro name.
#
# flisp: parse-macro-name
function parse_macro_name(ps::ParseState)
    # @! x   ==>  (macrocall @! x)
    # @.. x  ==>  (macrocall @.. x)
    # @$ x   ==>  (macrocall @$ x)
    # @var"#" x   ==>  (macrocall (var @#) x)
    bump_disallowed_space(ps)
    mark = position(ps)
    parse_atom(ps, false)
    b = peek_behind(ps, skip_parens=false)
    if b.kind == K"parens"
        emit_diagnostic(ps, mark,
            warning="parenthesizing macro names is unnecessary")
    elseif !_is_valid_macro_name(b)
        # @[x] y z  ==>  (macrocall (error (vect x)) y z)
        emit(ps, mark, K"error", error="invalid macro name")
    end
end

# Parse an identifier, interpolation or @-prefixed symbol
#
# flisp: parse-atsym
function parse_atsym(ps::ParseState, allow_quotes=true)
    bump_trivia(ps)
    if peek(ps) == K"@"
        # export @a       ==>  (export @a)
        # export @var"'"  ==>  (export (var @'))
        # export a, \n @b ==>  (export a @b)
        bump(ps, TRIVIA_FLAG)
        parse_macro_name(ps)
        fix_macro_name_kind!(ps, position(ps))
    else
        # export a  ==>  (export a)
        # export \n a  ==>  (export a)
        # export $a, $(a*b)  ==>  (export ($ a) (parens ($ (call * a b))))
        # export (x::T) ==> (export (error (parens (::-i x T))))
        # export outer  ==> (export outer)
        # export ($f)   ==> (export ($ f))
        mark = position(ps)
        if allow_quotes && peek(ps) == K":" && !is_closing_token(ps, peek(ps,2))
            # import A.:+  ==>  (import (importpath A (quote-: +)))
            emit_diagnostic(ps, warning="quoting with `:` is not required here")
        end
        parse_unary_prefix(ps)
        pos = position(ps)
        warn_parens = false
        if peek_behind(ps, pos).kind == K"parens"
            # import A.(:+)  ==>  (import (importpath A (parens (quote-: +))))
            pos = first_child_position(ps, pos)
            warn_parens = true
        end
        if allow_quotes && peek_behind(ps, pos).kind == K"quote"
            pos = first_child_position(ps, pos)
            if peek_behind(ps, pos).kind == K"parens"
                # import A.:(+)  ==>  (import (importpath A (quote-: (parens +))))
                pos = first_child_position(ps, pos)
                warn_parens = true
            end
        end
        if warn_parens
            emit_diagnostic(ps, mark, warning="parentheses are not required here")
        end
        b = peek_behind(ps, pos)
        ok = (b.is_leaf  && (b.kind == K"Identifier" || is_operator(b.kind))) ||
             (!b.is_leaf && b.kind in KSet"$ var")
        if !ok
            emit(ps, mark, K"error", error="expected identifier")
        end
    end
end

# Parse import and using syntax
#
# flisp: parse-imports
function parse_imports(ps::ParseState)
    mark = position(ps)
    word = peek(ps)
    @check word in KSet"import using"
    bump(ps, TRIVIA_FLAG)
    emark = position(ps)
    initial_as = parse_import(ps, word, false)
    t = peek_token(ps)
    k = kind(t)
    has_import_prefix = false  # true if we have `prefix:` in `import prefix: stuff`
    has_comma = false
    if k == K":" && !preceding_whitespace(t)
        bump(ps, TRIVIA_FLAG)
        has_import_prefix = true
        if initial_as
            # import A as B: x  ==>  (import (: (error (as (importpath A) B)) (importpath x)))
            emit(ps, emark, K"error", error="`as` before `:` in import/using")
        end
    elseif k == K","
        bump(ps, TRIVIA_FLAG)
        has_comma = true
    end
    if has_import_prefix || has_comma
        # import A, y      ==>  (import (importpath A) (importpath y))
        # import A: x, y   ==>  (import (: (importpath A) (importpath x) (importpath y)))
        # import A: +, ==  ==>  (import (: (importpath A) (importpath +) (importpath ==)))
        has_import_prefix_ = has_import_prefix
        parse_comma_separated(ps, ps1->parse_import(ps1, word, has_import_prefix_))
        if peek(ps) == K":"
            # Error recovery
            # import A: x, B: y ==> (import (: (importpath A) (importpath x) (importpath B) (error-t (importpath y))))
            emark = position(ps)
            bump(ps, TRIVIA_FLAG)
            parse_comma_separated(ps, ps1->parse_import(ps1, word, has_import_prefix_))
            emit(ps, emark, K"error", TRIVIA_FLAG,
                 error="`:` can only be used when importing a single module. Split imports into multiple lines")
        end
    end
    if has_import_prefix
        # import A: x  ==>  (import (: (importpath A) (importpath x)))
        emit(ps, mark, K":")
    end
    # using  A  ==>  (using (importpath A))
    # import A  ==>  (import (importpath A))
    emit(ps, mark, word)
end

# Parse individual module path and renaming with `as`
#
# flisp: parse-import
function parse_import(ps::ParseState, word, has_import_prefix)
    mark = position(ps)
    parse_import_path(ps)
    # import A: x, y   ==>  (import (: (importpath A) (importpath x) (importpath y)))
    if peek(ps) == K"as"
        # import A as B     ==>  (import (as (importpath A) B))
        # import A: x as y  ==>  (import (: (importpath A) (as (importpath x) y)))
        # using  A: x as y  ==>  (using (: (importpath A) (as (importpath x) y)))
        bump(ps, TRIVIA_FLAG)
        parse_atsym(ps)
        emit(ps, mark, K"as")
        if word == K"using" && !has_import_prefix
            # using A as B     ==>  (using (error (as (importpath A) B)))
            # using A, B as C  ==>  (using (importpath A) (error (as (importpath B) C)))
            emit(ps, mark, K"error",
                 error="`using` with `as` renaming requires a `:` and context module")
        end
        #v1.5: import A as B     ==>  (import (error (as (importpath A) B)))
        min_supported_version(v"1.6", ps, mark, "`import ... as`")
        return true
    else
        return false
    end
end

# flisp: parse-import-path
function parse_import_path(ps::ParseState)
    mark = position(ps)
    bump_trivia(ps)
    # The tokenizer produces conjoined dotted tokens .. and ...
    # When parsing import we must split these into single dots
    # import .A     ==> (import (importpath . A))
    # import ..A    ==> (import (importpath . . A))
    # import ...A   ==> (import (importpath . . . A))
    # import ....A  ==> (import (importpath . . . . A))
    # Dots with spaces are allowed (a misfeature?)
    # import . .A    ==> (import (importpath . . A))
    first_dot = true
    while true
        t = peek_token(ps)
        k = kind(t)
        if !first_dot && preceding_whitespace(t)
            emit_diagnostic(ps, whitespace=true,
                            warning="space between dots in import path")
        end
        if k == K"."
            bump(ps)
        elseif k == K".."
            bump_split(ps, (1,K".",EMPTY_FLAGS), (1,K".",EMPTY_FLAGS))
        elseif k == K"..."
            bump_split(ps, (1,K".",EMPTY_FLAGS), (1,K".",EMPTY_FLAGS), (1,K".",EMPTY_FLAGS))
        else
            break
        end
        first_dot = false
    end
    if is_dotted(peek_token(ps))
        # Modules with operator symbol names
        # import .⋆  ==>  (import (importpath . ⋆))
        bump_trivia(ps)
        bump_split(ps, (1,K".",EMPTY_FLAGS), (1,peek(ps),EMPTY_FLAGS))
    else
        # import @x     ==>  (import (importpath @x))
        # import $A     ==>  (import (importpath ($ A)))
        parse_atsym(ps)
    end
    while true
        t = peek_token(ps)
        k = kind(t)
        if k == K"."
            # import A.B    ==>  (import (importpath A B))
            # import $A.@x  ==>  (import (importpath ($ A) @x))
            # import A.B.C  ==>  (import (importpath A B C))
            bump_disallowed_space(ps)
            bump(ps, TRIVIA_FLAG)
            parse_atsym(ps)
        elseif is_dotted(t)
            # Resolve tokenization ambiguity: In imports, dots are part of the
            # path, not operators
            # import A.==   ==>  (import (importpath A ==))
            # import A.⋆.f  ==>  (import (importpath A ⋆ f))
            if preceding_whitespace(t)
                # Whitespace in import path allowed but discouraged
                # import A .==  ==>  (import (importpath A ==))
                emit_diagnostic(ps, whitespace=true,
                                warning="space between dots in import path")
            end
            bump_trivia(ps)
            bump_split(ps, (1,K".",TRIVIA_FLAG), (1,k,EMPTY_FLAGS))
        elseif k == K"..."
            # Import the .. operator
            # import A...  ==>  (import (importpath A ..))
            bump_split(ps, (1,K".",TRIVIA_FLAG), (2,K"..",EMPTY_FLAGS))
        elseif k in KSet"NewlineWs ; , : EndMarker"
            # import A; B  ==>  (import (importpath A))
            break
        else
            # Could we emit a more comprehensible error here?
            break
        end
    end
    emit(ps, mark, K"importpath")
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
# (i,j) in iter  ==>  (= (tuple-p i j) iter)
#
# flisp: parse-iteration-spec
function parse_iteration_spec(ps::ParseState)
    mark = position(ps)
    k = peek(ps)
    # Handle `outer` contextual keyword
    parse_pipe_lt(with_space_sensitive(ps))
    if peek_behind(ps).orig_kind == K"outer"
        if peek_skip_newline_in_gen(ps) in KSet"= in ∈"
            # Not outer keyword
            # outer = rhs        ==>  (= outer rhs)
            # outer <| x = rhs   ==>  (= (call-i outer <| x) rhs)
        else
            # outer i = rhs      ==>  (= (outer i) rhs)
            # outer (x,y) = rhs  ==>  (= (outer (tuple-p x y)) rhs)
            reset_node!(ps, position(ps), kind=K"outer", flags=TRIVIA_FLAG)
            parse_pipe_lt(ps)
            emit(ps, mark, K"outer")
        end
    end
    if peek_skip_newline_in_gen(ps) in KSet"= in ∈"
        bump(ps, TRIVIA_FLAG)
        parse_pipe_lt(ps)
    else
        # Recovery heuristic
        recover(ps, error="invalid iteration spec: expected one of `=` `in` or `∈`") do ps, k
            k in KSet", NewlineWs" || is_closing_token(ps, k)
        end
        # Or try parse_pipe_lt ???
    end
    emit(ps, mark, K"=")
end

# Parse an iteration spec, or a comma separate list of such for for loops and
# generators
function parse_iteration_specs(ps::ParseState)
    mark = position(ps)
    n_iters = parse_comma_separated(ps, parse_iteration_spec)
    if n_iters > 1
        emit(ps, mark, K"cartesian_iterator")
    end
end

# flisp: parse-space-separated-exprs
function parse_space_separated_exprs(ps::ParseState)
    ps = with_space_sensitive(ps)
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

# like parse-arglist, but with `for` parsed as a generator
#
# flisp: parse-call-arglist
function parse_call_arglist(ps::ParseState, closer)
    ps = ParseState(ps, for_generator=true)

    parse_brackets(ps, closer) do _, _, _, _
        return (needs_parameters=true,)
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
    parse_brackets(ps, closer) do _, _, _, _
        return (needs_parameters=true,)
    end
    return (K"vect", EMPTY_FLAGS)
end

# Parse generators
#
# We represent generators quite differently from `Expr`:
# * Cartesian products of iterators are grouped within cartesian_iterator
#   nodes, as in the short form of `for` loops.
# * The `generator` kind is used for both cartesian and flattened generators
#
# (x for a in as for b in bs) ==> (parens (generator x (= a as) (= b bs)))
# (x for a in as, b in bs) ==> (parens (generator x (cartesian_iterator (= a as) (= b bs))))
# (x for a in as, b in bs if z)  ==> (parens (generator x (filter (cartesian_iterator (= a as) (= b bs)) z)))
#
# flisp: parse-generator
function parse_generator(ps::ParseState, mark)
    while (t = peek_token(ps); kind(t) == K"for")
        if !preceding_whitespace(t)
            # ((x)for x in xs)  ==>  (parens (generator (parens x) (error) (= x xs)))
            bump_invisible(ps, K"error", TRIVIA_FLAG,
                           error="Expected space before `for` in generator")
        end
        bump(ps, TRIVIA_FLAG)
        iter_mark = position(ps)
        parse_iteration_specs(ps)
        if peek(ps) == K"if"
            # (x for a in as if z) ==> (parens (generator x (filter (= a as) z)))
            bump(ps, TRIVIA_FLAG)
            parse_cond(ps)
            emit(ps, iter_mark, K"filter")
        end
    end
    emit(ps, mark, K"generator")
end

# flisp: parse-comprehension
function parse_comprehension(ps::ParseState, mark, closer)
    # [x for a in as] ==> (comprehension (generator x a in as))
    ps = ParseState(ps, whitespace_newline=true,
                    space_sensitive=false,
                    end_symbol=false)
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
# Single elements in rows
#v1.7: [x ; y ;; z ]  ==>  (ncat-2 (nrow-1 x y) z)
#v1.7: [x  y ;;; z ]  ==>  (ncat-3 (row x y) z)
#
# Higher dimensional ncat
# Row major
#v1.7: [x y ; z w ;;; a b ; c d]  ==>
#     (ncat-3 (nrow-1 (row x y) (row z w)) (nrow-1 (row a b) (row c d)))
# Column major
#v1.7: [x ; y ;; z ; w ;;; a ; b ;; c ; d]  ==>
#     (ncat-3 (nrow-2 (nrow-1 x y) (nrow-1 z w)) (nrow-2 (nrow-1 a b) (nrow-1 c d)))
#
# flisp: parse-array
function parse_array(ps::ParseState, mark, closer, end_is_symbol)
    ps = ParseState(ps, end_symbol=end_is_symbol)

    array_order = Ref(:unknown)
    # Outer array parsing loop - parse chain of separators with descending
    # precedence such as
    #v1.7: [a ; b ;; c ;;; d ;;;; e] ==> (ncat-4 (ncat-3 (ncat-2 (ncat-1 a b) c) d) e)
    #
    # Ascending and equal precedence is handled by parse_array_inner.
    #
    # This is a variant of a Pratt parser, but we have a separate outer loop
    # because there's no minimum precedence/binding power - you can always get
    # a lower binding power by adding more semicolons.
    #
    # For an excellent overview of Pratt parsing, see
    # https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
    (dim, binding_power) = parse_array_separator(ps, array_order)
    if binding_power == typemin(Int)
        # [x@y  ==>  (hcat x (error-t ✘ y))
        bump_closing_token(ps, closer)
        return (K"hcat", EMPTY_FLAGS)
    end
    while true
        (next_dim, next_bp) = parse_array_inner(ps, binding_power, array_order)
        if next_bp == typemin(Int)
            break
        end
        if binding_power == 0
            emit(ps, mark, K"row")
        else
            emit(ps, mark, K"nrow", set_numeric_flags(dim))
        end
        dim = next_dim
        binding_power = next_bp
    end
    bump_closing_token(ps, closer)
    return binding_power == -1 ? (K"vcat", EMPTY_FLAGS) :
           binding_power ==  0 ? (K"hcat", EMPTY_FLAGS) :
           (K"ncat", set_numeric_flags(dim))
end

# Parse equal and ascending precedence chains of array concatenation operators -
# semicolons, newlines and whitespace. Invariants:
#
# * The caller must have already consumed
#   - The left hand side
#   - The concatenation operator, providing `binding_power`.
#   So eg, we're here in the input stream, either at an element or closing token
#                |
#          [a ;; b ; c ]
#          [a ;; ]
#
# * The caller must call emit() to delimit the AST node for this binding power.
#
function parse_array_inner(ps, binding_power, array_order)
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
            (next_dim, next_bp) = parse_array_separator(ps, array_order)
        else # bp > binding_power
            # Recurse to parse a separator with greater binding power. Eg:
            # [a ;; b ; c ]
            #       |   ^------ the next input is here
            #       '---------- the mark is here
            (next_dim, next_bp) = parse_array_inner(ps, bp, array_order)
            if bp == 0
                emit(ps, mark, K"row")
            else
                emit(ps, mark, K"nrow", set_numeric_flags(dim))
            end
        end
        dim, bp = next_dim, next_bp
    end
end

# Parse a separator in an array concatenation
#
# Here we return a tuple (dim, binding_power) containing
# * Dimension on which the next separator acts
# * Binding power (precedence) of the separator, where whitespace binds
#   tightest:  ... < `;;;` < `;;` < `;`,`\n` < whitespace. We choose binding
#   power of 0 for whitespace and negative numbers for other separators.
#
function parse_array_separator(ps, array_order)
    sep_mismatch_err = "cannot mix space and ;; separators in an array expression, except to wrap a line"
    mark = position(ps)
    t = peek_token(ps, skip_newlines=true)
    if kind(t) == K";"
        # Newlines before semicolons are not significant
        # [a \n ;]     ==> (vcat a)
        bump_trivia(ps)
        n_semis = 1
        while true
            bump(ps, TRIVIA_FLAG)
            t = peek_token(ps)
            if kind(t) != K";"
                break
            end
            if preceding_whitespace(t)
                bump_disallowed_space(ps)
            end
            n_semis += 1
        end
        had_newline = peek(ps) == K"NewlineWs"
        # Newlines after semicolons are not significant
        # [a ; \n]     ==> (vcat a)
        # [a ; \n\n b] ==> (vcat a b)
        #v1.7: [a ;; \n b]  ==> (ncat-2 a b)
        bump_trivia(ps)
        if n_semis == 2
            if array_order[] === :row_major
                if had_newline
                    # In hcat with spaces as separators, `;;` is a line
                    # continuation character
                    #v1.7: [a b ;; \n c]  ==>  (hcat a b c)
                    #v1.7: [a b \n ;; c]  ==>  (ncat-2 (row a b (error-t)) c)
                    return (2, 0)
                else
                    # Can't mix spaces and multiple ;;
                    #v1.7:  [a b ;; c]  ==>  (ncat-2 (row a b (error-t)) c)
                    emit(ps, mark, K"error", TRIVIA_FLAG, error=sep_mismatch_err)
                end
            else
                array_order[] = :column_major
            end
        end
        return (n_semis, -n_semis)
    end
    t = peek_token(ps)
    k = kind(t)
    if k == K"NewlineWs"
        bump_trivia(ps)
        if peek(ps) == K"]"
            # Linebreaks not significant before closing `]`
            # [a b\n\n]  ==>  (hcat a b)
            return (typemin(Int), typemin(Int))
        else
            # Treat a linebreak prior to a value as a semicolon (ie, separator
            # for the first dimension) if no previous semicolons observed
            # [a \n b]  ==> (vcat a b)
            return (1, -1)
        end
    elseif k == K","
        # Treat `,` as semicolon for the purposes of recovery
        # [a; b, c] ==> (vcat a b (error-t) c)
        bump(ps, TRIVIA_FLAG, error="unexpected comma in array expression")
        return (1, -1)
    else
        if preceding_whitespace(t) && !is_closing_token(ps, k)
            if array_order[] === :column_major
                # Can't mix multiple ;'s and spaces
                #v1.7:  [a ;; b c]  ==>  (ncat-2 a (row b (error-t) c))
                bump_trivia(ps, TRIVIA_FLAG, error=sep_mismatch_err)
            else
                array_order[] = :row_major
            end
            return (2, 0)
        else
            # Something else; use typemin to exit array parsing
            return (typemin(Int), typemin(Int))
        end
    end
end

# Parse array concatenation/construction/indexing syntax inside of `[]` or `{}`.
# The opening bracket has been consumed.
#
# flisp: parse-cat
function parse_cat(ps::ParseState, closer, end_is_symbol)
    ps = ParseState(ps, range_colon_enabled=true,
                    space_sensitive=true,
                    where_enabled=true,
                    whitespace_newline=false,
                    for_generator=true)
    k = peek(ps, skip_newlines=true)
    mark = position(ps)
    if k == closer
        # []  ==>  (vect)
        return parse_vect(ps, closer)
    elseif k == K";"
        #v1.8: [;]           ==>  (ncat-1)
        #v1.8: [;;]          ==>  (ncat-2)
        #v1.8: [\n  ;; \n ]  ==>  (ncat-2)
        #v1.7: [;;]          ==>  (ncat-2 (error))
        bump_trivia(ps)
        dim, _ = parse_array_separator(ps, Ref(:unknown))
        min_supported_version(v"1.8", ps, mark, "empty multidimensional array syntax")
        bump_closing_token(ps, closer)
        return (K"ncat", set_numeric_flags(dim))
    end
    parse_eq_star(ps)
    k = peek(ps, skip_newlines=true)
    if k == K"," || (is_closing_token(ps, k) && k != K";")
        if k == K","
            # [x,]  ==>  (vect x)
            bump(ps, TRIVIA_FLAG; skip_newlines = true)
        end
        # [x]      ==>  (vect x)
        # [x \n ]  ==>  (vect x)
        # [x       ==>  (vect x (error-t))
        parse_vect(ps, closer)
    elseif k == K"for"
        # [x for a in as]  ==>  (comprehension (generator x (= a as)))
        # [x \n\n for a in as]  ==>  (comprehension (generator x (= a as)))
        parse_comprehension(ps, mark, closer)
    else
        # [x y]  ==>  (hcat x y)
        # and other forms; See parse_array.
        parse_array(ps, mark, closer, end_is_symbol)
    end
end

function check_ncat_compat(ps, mark, k)
    # https://github.com/JuliaLang/julia/pull/33697
    if k == K"ncat"
        min_supported_version(v"1.7", ps, mark, "multidimensional array syntax")
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
    @check peek(ps) == K"("
    bump(ps, TRIVIA_FLAG) # K"("
    after_paren_mark = position(ps)
    k = peek(ps)
    if k == K")"
        # ()  ==>  (tuple-p)
        bump(ps, TRIVIA_FLAG)
        emit(ps, mark, K"tuple", PARENS_FLAG)
    elseif is_syntactic_operator(k)
        # allow :(=) etc in unchecked contexts, eg quotes
        # :(=)  ==>  (quote-: (parens =))
        parse_atom(ps, check_identifiers)
        bump_closing_token(ps, K")")
        emit(ps, mark, K"parens")
    elseif !check_identifiers && k == K"::" &&
            peek(ps, 2, skip_newlines=true) == K")"
        # allow :(::) as a special case
        # :(::)  ==>  (quote-: (parens ::))
        bump(ps)
        bump(ps, TRIVIA_FLAG, skip_newlines=true)
        emit(ps, mark, K"parens")
    else
        # Deal with all other cases of tuple or block syntax via the generic
        # parse_brackets
        initial_semi = peek(ps) == K";"
        opts = parse_brackets(ps, K")") do had_commas, had_splat, num_semis, num_subexprs
            is_tuple = had_commas || (had_splat && num_semis >= 1) ||
                       (initial_semi && (num_semis == 1 || num_subexprs > 0))
            return (needs_parameters=is_tuple,
                    is_tuple=is_tuple,
                    is_block=num_semis > 0)
        end
        if opts.is_tuple
            # Tuple syntax with commas
            # (x,)        ==>  (tuple-p x)
            # (x,y)       ==>  (tuple-p x y)
            # (x=1, y=2)  ==>  (tuple-p (= x 1) (= y 2))
            #
            # Named tuple with initial semicolon
            # (;)         ==>  (tuple-p (parameters))
            # (; a=1)     ==>  (tuple-p (parameters (= a 1)))
            #
            # Extra credit: nested parameters and frankentuples
            # (x...;)         ==> (tuple-p (... x) (parameters))
            # (x...; y)       ==> (tuple-p (... x) (parameters y))
            # (; a=1; b=2)    ==> (tuple-p (parameters (= a 1)) (parameters (= b 2)))
            # (a; b; c,d)     ==> (tuple-p a (parameters b) (parameters c d))
            # (a=1, b=2; c=3) ==> (tuple-p (= a 1) (= b 2) (parameters (= c 3)))
            emit(ps, mark, K"tuple", PARENS_FLAG)
        elseif opts.is_block
            # Blocks
            # (;;)        ==>  (block-p)
            # (a=1;)      ==>  (block-p (= a 1))
            # (a;b;;c)    ==>  (block-p a b c)
            # (a=1; b=2)  ==>  (block-p (= a 1) (= b 2))
            emit(ps, mark, K"block", PARENS_FLAG)
        else
            # Parentheses used for grouping
            # (a * b)     ==>  (parens (call-i * a b))
            # (a=1)       ==>  (parens (= a 1))
            # (x)         ==>  (parens x)
            # (a...)      ==>  (parens (... a))
            emit(ps, mark, K"parens")
        end
    end
end

# Handle bracketed syntax inside any of () [] or {} where there's a mixture
# of commas and semicolon delimiters.
#
# For parentheses this is tricky because there's various cases to disambiguate,
# depending on outside context and the content of the brackets (number of
# semicolons, presence of commas or splats). The `after_parse` function must be
# provided by the caller to disambiguate these cases.
#
# Expressions (X; Y; Z) with more semicolons are also allowed by the flisp
# parser and generally parse as nested parameters blocks. This is invalid Julia
# syntax so the parse tree is pretty strange in these cases!  Some macros
# probably use it though.  Example:
#
# (a,b=1; c,d=2; e,f=3)  ==>  (tuple-p a (= b 1) (parameters c (= d 2)) (parameters e (= f 3)))
#
# flisp: parts of parse-paren- and parse-arglist
function parse_brackets(after_parse::Function,
                        ps::ParseState, closing_kind)
    ps = ParseState(ps, range_colon_enabled=true,
                    space_sensitive=false,
                    where_enabled=true,
                    whitespace_newline=true)
    params_positions = acquire_positions(ps.stream)
    last_eq_before_semi = 0
    num_subexprs = 0
    num_semis = 0
    had_commas = false
    had_splat = false
    param_start = nothing
    while true
        k = peek(ps)
        if k == closing_kind
            break
        elseif k == K";"
            # Start of parameters list
            # a, b; c d  ==>  a b (parameters c d)
            if !isnothing(param_start)
                push!(params_positions, emit(ps, param_start, K"TOMBSTONE"))
            end
            num_semis += 1
            param_start = position(ps)
            bump(ps, TRIVIA_FLAG)
            bump_trivia(ps)
        elseif is_closing_token(ps, k)
            # Error; handled below in bump_closing_token
            break
        else
            mark = position(ps)
            parse_eq_star(ps)
            num_subexprs += 1
            if num_subexprs == 1
                had_splat = peek_behind(ps).kind == K"..."
            end
            t = peek_token(ps, skip_newlines=true)
            k = kind(t)
            if k == K","
                had_commas = true
                bump(ps, TRIVIA_FLAG)
            elseif k == K";" || k == closing_kind
                # Handled above
                continue
            elseif k == K"for"
                # Generator syntax
                # (x for a in as)       ==>  (parens (generator x (= a as)))
                # (x \n\n for a in as)  ==>  (parens (generator x (= a as)))
                parse_generator(ps, mark)
            else
                # Error - recovery done when consuming closing_kind
                break
            end
        end
    end
    if !isnothing(param_start) && position(ps) != param_start
        push!(params_positions, emit(ps, param_start, K"TOMBSTONE"))
    end
    opts = after_parse(had_commas, had_splat, num_semis, num_subexprs)
    # Emit nested parameter nodes if necessary
    if opts.needs_parameters
        for pos in params_positions
            reset_node!(ps, pos, kind=K"parameters")
        end
    end
    release_positions(ps.stream, params_positions)
    bump_closing_token(ps, closing_kind, " or `,`")
    return opts
end

_is_indentation(b::UInt8) = (b == u8" " || b == u8"\t")

# Parse a string, embedded interpolations and deindent triple quoted strings
# by marking indentation characters as whitespace trivia.
#
# flisp: parse-string-literal-, parse-interpolate
function parse_string(ps::ParseState, raw::Bool)
    mark = position(ps)
    delim_k = peek(ps)
    triplestr = delim_k in KSet"\"\"\" ```"
    string_chunk_kind = delim_k in KSet"\" \"\"\"" ? K"String" : K"CmdString"
    indent_ref_i = 0
    indent_ref_len = typemax(Int)
    indent_chunks = acquire_positions(ps.stream)
    txtbuf = unsafe_textbuf(ps)
    chunk_flags = raw ? RAW_STRING_FLAG : EMPTY_FLAGS
    bump(ps, TRIVIA_FLAG)
    first_chunk = true
    n_nontrivia_chunks = 0
    removed_initial_newline = false
    had_interpolation = false
    prev_chunk_newline = false
    while true
        t = peek_full_token(ps)
        k = kind(t)
        if k == K"$"
            if raw
                # FIXME: This case is actually a tokenization error:
                # The `K"$"` token should not occur when a raw string
                # is being parsed, but this would require the lexer to know
                # about the parse state. (see also parse_atom)
                break
            end
            if prev_chunk_newline
                # """\n$x\n a"""  ==>  (string-s x "\n" " a")
                indent_ref_i = first_byte(t)
                indent_ref_len = 0
            end
            bump(ps, TRIVIA_FLAG)
            k = peek(ps)
            if k == K"("
                # "a $(x + y) b"  ==>  (string "a " (parens (call-i x + y)) " b")
                # "hi$("ho")"     ==>  (string "hi" (parens (string "ho")))
                m = position(ps)
                bump(ps, TRIVIA_FLAG)
                opts = parse_brackets(ps, K")") do had_commas, had_splat, num_semis, num_subexprs
                    return (needs_parameters=false,
                            simple_interp=!had_commas && num_semis == 0 && num_subexprs == 1)
                end
                if !opts.simple_interp || peek_behind(ps, skip_parens=false).kind == K"generator"
                    # "$(x,y)" ==> (string (parens (error x y)))
                    emit(ps, m, K"error", error="invalid interpolation syntax")
                end
                emit(ps, m, K"parens")
            elseif k == K"var"
                # var identifiers disabled in strings
                # "$var"  ==>  (string var)
                bump(ps, remap_kind=K"Identifier")
            elseif k == K"Identifier" || is_keyword(k) || is_word_operator(k)
                # "a $foo b"  ==> (string "a " foo " b")
                # "$outer"    ==> (string outer)
                # "$in"       ==> (string in)
                parse_atom(ps)
            else
                bump_invisible(ps, K"error",
                    error="identifier or parenthesized expression expected after \$ in string")
            end
            first_chunk = false
            n_nontrivia_chunks += 1
            had_interpolation = true
            prev_chunk_newline = false
        elseif k == string_chunk_kind
            if triplestr && first_chunk && span(t) <= 2 &&
                    begin
                        s = span(t)
                        b = txtbuf[last_byte(t)]
                        # Test whether the string is a single logical newline
                        (s == 1 && (b == u8"\n" || b == u8"\r")) ||
                        (s == 2 && (txtbuf[first_byte(t)] == u8"\r" && b == u8"\n"))
                    end
                # First line of triple string is a newline only: mark as trivia.
                # """\nx"""    ==> (string-s "x")
                # """\n\nx"""  ==> (string-s "\n" "x")
                bump(ps, TRIVIA_FLAG)
                first_chunk = false
                prev_chunk_newline = true
            else
                if triplestr
                    # Triple-quoted dedenting:
                    # Various newlines (\n \r \r\n) and whitespace (' ' \t)
                    # """\n x\n y"""      ==> (string-s "x\n" "y")
                    # ```\n x\n y```      ==> (macrocall :(Core.var"@cmd") (cmdstring-s-r "x\n" "y"))
                    # """\r x\r y"""      ==> (string-s "x\n" "y")
                    # """\r\n x\r\n y"""  ==> (string-s "x\n" "y")
                    # Spaces or tabs or mixtures acceptable
                    # """\n\tx\n\ty"""    ==> (string-s "x\n" "y")
                    # """\n \tx\n \ty"""  ==> (string-s "x\n" "y")
                    #
                    # Mismatched tab vs space not deindented
                    # Find minimum common prefix in mismatched whitespace
                    # """\n\tx\n y"""     ==> (string-s "\tx\n" " y")
                    # """\n x\n  y"""   ==> (string-s "x\n" " y")
                    # """\n  x\n y"""   ==> (string-s " x\n" "y")
                    # """\n \tx\n  y""" ==> (string-s "\tx\n" " y")
                    # """\n  x\n \ty""" ==> (string-s " x\n" "\ty")
                    #
                    # Empty lines don't affect dedenting
                    # """\n x\n\n y"""    ==> (string-s "x\n" "\n" "y")
                    # Non-empty first line doesn't participate in deindentation
                    # """ x\n y"""    ==> (string-s " x\n" "y")
                    #
                    # Dedenting and interpolations
                    # """\n  $a\n  $b"""    ==> (string-s a "\n" b)
                    # """\n  $a \n  $b"""   ==> (string-s a " \n" b)
                    # """\n  $a\n  $b\n"""  ==> (string-s "  " a "\n" "  " b "\n")
                    #
                    if prev_chunk_newline && (b = txtbuf[first_byte(t)];
                                              b != u8"\n" && b != u8"\r")
                        # Compute length of longest common prefix of mixed
                        # spaces and tabs, in bytes
                        #
                        # Initial whitespace is never regarded as indentation
                        # in any triple quoted string chunk, as it's always
                        # preceded in the source code by a visible token of
                        # some kind; either a """ delimiter or $()
                        # interpolation.
                        if indent_ref_i == 0
                            # No indentation found yet. Find indentation we'll
                            # use as a reference
                            i = first_byte(t) - 1
                            while i < last_byte(t) && _is_indentation(txtbuf[i+1])
                                i += 1
                            end
                            indent_ref_i = first_byte(t)
                            indent_ref_len = i - first_byte(t) + 1
                        else
                            # Matching the current indentation with reference,
                            # shortening length if necessary.
                            j = 0
                            while j < span(t) && j < indent_ref_len
                                if txtbuf[j + first_byte(t)] != txtbuf[j + indent_ref_i]
                                    break
                                end
                                j += 1
                            end
                            indent_ref_len = min(indent_ref_len, j)
                        end
                        # Prepare a place for indentiation trivia, if necessary
                        push!(indent_chunks, bump_invisible(ps, K"TOMBSTONE"))
                    end
                    b = txtbuf[last_byte(t)]
                    prev_chunk_newline = b == UInt8('\n') || b == UInt8('\r')
                end
                bump(ps, chunk_flags)
                first_chunk = false
                n_nontrivia_chunks += 1
            end
        elseif  k == K"ErrorInvalidInterpolationTerminator" ||
                k == K"ErrorBidiFormatting" ||
                k == K"ErrorInvalidUTF8"
            # Treat these errors as string chunks
            bump(ps)
            n_nontrivia_chunks += 1
        else
            break
        end
    end
    had_end_delim = peek(ps) == delim_k
    if triplestr && prev_chunk_newline && had_end_delim
        # Newline at end of string
        # """\n x\n y\n"""    ==> (string-s " x\n" " y\n")
        indent_ref_len = 0
    end
    if triplestr && indent_ref_len > 0
        for pos in indent_chunks
            reset_node!(ps, pos, kind=K"Whitespace", flags=TRIVIA_FLAG)
            rhs_empty = steal_token_bytes!(ps, pos, indent_ref_len)
            if rhs_empty
                # Empty chunks after dedent are removed
                # """\n \n """        ==> (string-s "\n")
                n_nontrivia_chunks -= 1
            end
        end
    end
    release_positions(ps.stream, indent_chunks)
    if had_end_delim
        if n_nontrivia_chunks == 0
            # Empty strings, or empty after triple quoted processing
            # "" ==> (string "")
            # """\n  """ ==> (string-s "")
            bump_invisible(ps, string_chunk_kind, chunk_flags)
        end
        bump(ps, TRIVIA_FLAG)
    else
        # Missing delimiter recovery
        # "str   ==> (string "str" (error-t))
        bump_invisible(ps, K"error", TRIVIA_FLAG, error="unterminated string literal")
    end
    # String interpolations
    # "$x$y$z"  ==> (string x y z)
    # "$(x)"    ==> (string (parens x))
    # "$x"      ==> (string x)
    # """$x"""  ==> (string-s x)
    #
    # Strings with embedded whitespace trivia
    # "a\\\nb"      ==> (string "a" "b")
    # "a\\\rb"      ==> (string "a" "b")
    # "a\\\r\nb"    ==> (string "a" "b")
    # "a\\\n \tb"   ==> (string "a" "b")
    #
    # Strings with only a single valid string chunk
    # "str"     ==> (string "str")
    # "a\\\n"   ==> (string "a")
    # "a\\\r"   ==> (string "a")
    # "a\\\r\n" ==> (string "a")
    string_kind = delim_k in KSet"\" \"\"\"" ? K"string" : K"cmdstring"
    str_flags = (triplestr ? TRIPLE_STRING_FLAG : EMPTY_FLAGS) |
                (raw       ? RAW_STRING_FLAG : EMPTY_FLAGS)
    emit(ps, mark, string_kind, str_flags)
end

function emit_braces(ps, mark, ckind, cflags)
    if ckind == K"hcat"
        # {x y}  ==>  (bracescat (row x y))
        emit(ps, mark, K"row", cflags)
    elseif ckind == K"ncat"
        # {x ;;; y}  ==>  (bracescat (nrow-3 x y))
        emit(ps, mark, K"nrow", cflags)
    end
    check_ncat_compat(ps, mark, ckind)
    outk = ckind in KSet"vect comprehension" ? K"braces" : K"bracescat"
    emit(ps, mark, outk)
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
    # todo: Reorder to put most likely tokens first?
    if is_error(leading_kind)
        # Errors for bad tokens are emitted in validate_tokens() rather than
        # here.
        bump(ps)
    elseif leading_kind == K"'"
        # char literal
        bump(ps, TRIVIA_FLAG)
        k = peek(ps)
        if k == K"'"
            # ''  ==>  (char (error))
            bump_invisible(ps, K"error", error="empty character literal")
            bump(ps, TRIVIA_FLAG)
        elseif k == K"EndMarker"
            # '   ==>  (char (error))
            bump_invisible(ps, K"error", error="unterminated character literal")
        else
            if k == K"Char"
                bump(ps)
            elseif is_error(k)
                bump(ps)
            else
                # FIXME: This case is actually a tokenization error.
                # Make a best-effort attempt to workaround this for now by
                # remapping the kind. This needs to be fixed by rewinding the
                # tokenizer's buffer and re-tokenizing the next token as a
                # char. (A lot of work for a very obscure edge case)
                #
                # x in'c'  ==>  (call-i x in (char 'c'))
                bump(ps, remap_kind=K"Char")
            end
            if peek(ps) == K"'"
                # 'a'         ==>  (char 'a')
                # 'α'         ==>  (char 'α')
                # '\xce\xb1'  ==>  (char 'α')
                bump(ps, TRIVIA_FLAG)
            else
                # 'a  ==>  (char 'a' (error-t))
                bump_invisible(ps, K"error", TRIVIA_FLAG,
                               error="unterminated character literal")
            end
        end
        emit(ps, mark, K"char")
    elseif leading_kind == K"Char"
        # FIXME: This is a tokenization error and should be preceeded with
        # K"'". However this workaround is better than emitting a bare Char.
        bump(ps, remap_kind=K"Identifier")
    elseif leading_kind == K":"
        # symbol/expression quote
        # :foo  ==>  (quote-: foo)
        t = peek_token(ps, 2)
        k = kind(t)
        if is_closing_token(ps, k) && (!is_keyword(k) || preceding_whitespace(t))
            # : is a literal colon in some circumstances
            # :)     ==>  :
            # : end  ==>  :
            bump(ps) # K":"
            return
        end
        bump(ps, TRIVIA_FLAG) # K":"
        if preceding_whitespace(t)
            # : foo   ==>  (quote-: (error-t) foo)
            # :\nfoo  ==>  (quote-: (error-t) foo)
            bump_trivia(ps, TRIVIA_FLAG,
                        error="whitespace not allowed after `:` used for quoting")
        end
        # Being inside quote makes keywords into identifiers at the
        # first level of nesting
        # :end ==> (quote-: end)
        # :(end) ==> (quote-: (parens (error-t)))
        # Being inside quote makes end non-special again (issue #27690)
        # a[:(end)]  ==>  (ref a (quote-: (error-t end)))
        parse_atom(ParseState(ps, end_symbol=false), false)
        emit(ps, mark, K"quote", COLON_QUOTE)
    elseif check_identifiers && leading_kind == K"=" && is_plain_equals(peek_token(ps))
        # =   ==> (error =)
        bump(ps, error="unexpected `=`")
    elseif leading_kind == K"Identifier"
        # xx  ==>  xx
        # x₁  ==>  x₁
        bump(ps)
    elseif is_word_operator(leading_kind)
        # where=1 ==> (= where 1)
        bump(ps, remap_kind=K"Identifier")
    elseif is_operator(leading_kind)
        # +     ==>  +
        # .+    ==>  (. +)
        # .=    ==>  (. =)
        bump_dotsplit(ps, emit_dot_node=true)
        if check_identifiers && !is_valid_identifier(leading_kind)
            # +=   ==>  (error +=)
            # ?    ==>  (error ?)
            # .+=  ==>  (error (. +=))
            emit(ps, mark, K"error", error="invalid identifier")
        else
            # Quoted syntactic operators allowed
            # :+=  ==>  (quote-: +=)
        end
    elseif is_keyword(leading_kind)
        if leading_kind == K"var" && (t = peek_token(ps,2);
                                      kind(t) == K"\"" && !preceding_whitespace(t))
            # var"x"     ==> (var x)
            # Raw mode unescaping
            # var""     ==> (var )
            # var"\""   ==> (var ")
            # var"\\""  ==> (var \")
            # var"\\x"  ==> (var \\x)
            #
            # NB: Triple quoted var identifiers are not implemented, but with
            # the complex deindentation rules they seem like a misfeature
            # anyway, maybe?
            # var"""x""" !=> x
            bump(ps, TRIVIA_FLAG)
            bump(ps, TRIVIA_FLAG)
            if peek(ps) == K"String"
                bump(ps, RAW_STRING_FLAG; remap_kind=K"Identifier")
            else
                bump_invisible(ps, K"Identifier", RAW_STRING_FLAG)
            end
            if peek(ps) == K"\""
                bump(ps, TRIVIA_FLAG)
            else
                bump_invisible(ps, K"error", TRIVIA_FLAG,
                               error="unterminated `var\"\"` identifier")
            end
            t = peek_token(ps)
            k = kind(t)
            if preceding_whitespace(t) || is_operator(k) ||
                    k in KSet"( ) [ ] { } , ; @ EndMarker"
                # var"x"+  ==>  x
                # var"x")  ==>  x
                # var"x"(  ==>  x
            elseif is_string_macro_suffix(k)
                # var"x"end  ==>  (var x (error-t))
                # var"x"1    ==>  (var x (error-t))
                # var"x"y    ==>  (var x (error-t))
                bump(ps, TRIVIA_FLAG, error="suffix not allowed after `var\"...\"` syntax")
            elseif k == K"`" || k == K"\"" || k == K"\"\"\"" || k == K"```"
                # Disallow `var"#""str". To allow this we'd need to fix `raw`
                # detection in lex_quote to be consistent with the parser.
                bump_invisible(ps, K"error", TRIVIA_FLAG,
                               error="`var\"...\"` syntax not supported as string macro name")
            end
            emit(ps, mark, K"var")
        elseif check_identifiers && is_closing_token(ps, leading_kind)
            # :(end)  ==>  (quote-: (error end))
            bump(ps, error="invalid identifier")
        else
            # Remap keywords to identifiers.
            # :end  ==>  (quote-: end)
            # :<:   ==> (quote-: <:)
            bump(ps, remap_kind=K"Identifier")
        end
    elseif leading_kind == K"(" # parens or tuple
        parse_paren(ps, check_identifiers)
    elseif leading_kind == K"[" # cat expression
        bump(ps, TRIVIA_FLAG)
        ckind, cflags = parse_cat(ps, K"]", ps.end_symbol)
        emit(ps, mark, ckind, cflags)
        check_ncat_compat(ps, mark, ckind)
    elseif leading_kind == K"{" # cat expression
        bump(ps, TRIVIA_FLAG)
        ckind, cflags = parse_cat(ps, K"}", ps.end_symbol)
        emit_braces(ps, mark, ckind, cflags)
    elseif leading_kind == K"@" # macro call
        # Macro names can be keywords
        # @end x  ==> (macrocall @end x)
        bump(ps, TRIVIA_FLAG)
        parse_macro_name(ps)
        parse_call_chain(ps, mark, true)
    elseif is_string_delim(leading_kind)
        parse_string(ps, false)
    elseif leading_kind in KSet"` ```"
        # ``          ==>  (macrocall core_@cmd (cmdstring-r ""))
        # `cmd`       ==>  (macrocall core_@cmd (cmdstring-r "cmd"))
        # ```cmd```   ==>  (macrocall core_@cmd (cmdstring-s-r "cmd"))
        bump_invisible(ps, K"core_@cmd")
        parse_string(ps, true)
        emit(ps, mark, K"macrocall")
    elseif is_literal(leading_kind)
        # 42   ==>  42
        bump(ps)
    elseif is_closing_token(ps, leading_kind)
        # Leave closing token in place for other productions to
        # recover with
        # )  ==>  error
        msg = leading_kind == K"EndMarker" ?
              "premature end of input" :
              "unexpected `$(untokenize(leading_kind))`"
        emit_diagnostic(ps, error=msg)
        bump_invisible(ps, K"error")
    else
        bump(ps, error="invalid syntax atom")
    end
end
