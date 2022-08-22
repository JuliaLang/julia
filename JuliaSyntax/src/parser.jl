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

function textbuf(ps::ParseState)
    textbuf(ps.stream)
end

#-------------------------------------------------------------------------------
# Parser Utils

# Bump an expected closing token.  If not found, discard unexpected tokens
# until we find it or another closing token.
#
# Crude recovery heuristic: bump any tokens which aren't block or bracket
# closing tokens.
function bump_closing_token(ps, closing_kind)
    # todo: Refactor with recover() ?
    bump_trivia(ps)
    if peek(ps) == closing_kind
        bump(ps, TRIVIA_FLAG)
        return
    end
    # We didn't find the closing token. Read ahead in the stream
    mark = position(ps)
    while true
        k = peek(ps)
        if is_closing_token(ps, k) && !(k in KSet", ;")
            break
        end
        bump(ps)
    end
    # mark as trivia => ignore in AST.
    emit(ps, mark, K"error", TRIVIA_FLAG,
         error="Expected `$(untokenize(closing_kind))`")
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

# Like @assert, but always enabled and calls internal_error()
macro check(ex, msgs...)
    msg = isempty(msgs) ? ex : msgs[1]
    if isa(msg, AbstractString)
        msg = msg
    elseif !isempty(msgs) && (isa(msg, Expr) || isa(msg, Symbol))
        msg = :(string($(esc(msg))))
    else
        msg = string(msg)
    end
    return :($(esc(ex)) ? nothing : internal_error($msg))
end

# Parser internal error, used as an assertion failure for cases we expect can't
# happen.
@noinline function internal_error(strs...)
    error("Internal error: ", strs...)
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
        k2 = peek(ps,2)
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

# operators handled by parse_unary at the start of an expression
function is_initial_operator(t)
    k = kind(t)
    # TODO(jb): `?` should probably not be listed here except for the syntax hack in osutils.jl
    is_operator(k)             &&
    !is_word_operator(k)       &&
    !(k in KSet": ' .' ?")     &&
    !(is_syntactic_unary_op(k) && !is_dotted(t)) &&
    !is_syntactic_operator(k)
end

# flisp: invalid-identifier?
function is_valid_identifier(k)
    k = kind(k)
    !(is_syntactic_operator(k) || k in KSet"? .'")
end

#-------------------------------------------------------------------------------
# Parser
#
# The definitions and top-level comments here were copied to match the
# structure of Julia's official flisp-based parser.
#
# This is to make both codebases mutually understandable and make porting
# changes simple.
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
        bump(ps)
        down(ps)
        emit(ps, mark, K"call", INFIX_FLAG)
    end
end

# parse right-to-left binary operator
# produces structures like (= a (= b (= c d)))
#
# flisp: parse-RtoL
function parse_RtoL(ps::ParseState, down, is_op, self)
    mark = position(ps)
    down(ps)
    k = peek(ps)
    if is_op(k)
        bump(ps)
        self(ps)
        emit(ps, mark, K"call", INFIX_FLAG)
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
            bump_trivia(ps, skip_newlines=true)
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
function parse_block(ps::ParseState, down=parse_eq, mark=position(ps),
                     consume_end=false)
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
# "x" a ; "y" b ==>  (toplevel (macrocall core_@doc "x" a) (macrocall core_@doc "y" b))
#
# flisp: parse-stmts
function parse_stmts(ps::ParseState)
    mark = position(ps)
    do_emit = parse_Nary(ps, parse_docstring, (K";",), (K"NewlineWs",))
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
        emit(ps, mark, K"toplevel")
    end
end

# Parse docstrings attached by a space or single newline
# "doc" foo  ==>  (macrocall core_@doc "doc" foo)
#
# flisp: parse-docstring
function parse_docstring(ps::ParseState, down=parse_eq)
    mark = position(ps)
    atdoc_mark = bump_invisible(ps, K"TOMBSTONE")
    down(ps)
    if peek_behind(ps).kind in KSet"String string"
        is_doc = true
        k = peek(ps)
        if is_closing_token(ps, k)
            # "notdoc" ] ==> "notdoc"
            is_doc = false
        elseif k == K"NewlineWs"
            k2 = peek(ps, 2)
            if is_closing_token(ps, k2) || k2 == K"NewlineWs"
                # "notdoc" \n]      ==> "notdoc"
                # "notdoc" \n\n foo ==> "notdoc"
                is_doc = false
            else
                # Allow a single newline
                # "doc" \n foo ==> (macrocall core_@doc "doc" foo)
                bump(ps, TRIVIA_FLAG) # NewlineWs
            end
        else
            # "doc" foo    ==> (macrocall core_@doc "doc" foo)
            # "doc $x" foo ==> (macrocall core_@doc (string "doc " x) foo)
            # Allow docstrings with embedded trailing whitespace trivia
            # """\n doc\n """ foo ==> (macrocall core_@doc "doc\n" foo)
        end
        if is_doc
            reset_node!(ps, atdoc_mark, kind=K"core_@doc")
            down(ps)
            emit(ps, mark, K"macrocall")
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
    if (is_literal(k) || k == K"Identifier") && k2 in KSet", ) } ]"
        # optimization: skip checking the whole precedence stack if we have a
        # simple token followed by a common closing token
        bump(ps)
        return NO_POSITION
    else
        return parse_assignment(ps, parse_pair, equals_is_kw)
    end
end

# a = b  ==>  (= a b)
#
# flisp: parse-assignment
function parse_assignment(ps::ParseState, down, equals_is_kw::Bool)
    mark = position(ps)
    down(ps)
    parse_assignment_with_initial_ex(ps, mark, down, equals_is_kw)
end

function parse_assignment_with_initial_ex(ps::ParseState, mark, down, equals_is_kw::Bool)
    t = peek_token(ps)
    k = kind(t)
    if !is_prec_assignment(k)
        return NO_POSITION
    end
    if k == K"~"
        if ps.space_sensitive && !preceding_whitespace(peek_token(ps, 2))
            # Unary ~ in space sensitive context is not assignment precedence
            # [a ~b]  ==>  (hcat a (call ~ b))
            return NO_POSITION
        end
        # ~ is the only non-syntactic assignment-precedence operator.
        # a ~ b      ==>  (call-i a ~ b)
        # [a ~ b c]  ==>  (hcat (call-i a ~ b) c)
        bump(ps)
        parse_assignment(ps, down, equals_is_kw)
        emit(ps, mark, K"call", INFIX_FLAG)
        return NO_POSITION
    else
        # a += b  ==>  (+= a b)
        # a .= b  ==>  (.= a b)
        bump(ps, TRIVIA_FLAG)
        parse_assignment(ps, down, equals_is_kw)
        plain_eq = is_plain_equals(t)
        equals_pos = emit(ps, mark, plain_eq && equals_is_kw ? K"kw" : k, flags(t))
        return plain_eq ? equals_pos : NO_POSITION
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
function parse_pair(ps::ParseState)
    parse_RtoL(ps, parse_cond, is_prec_pair, parse_pair)
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
    if !preceding_whitespace(t)
        # a? b : c  => (if a (error-t) b c)
        bump_invisible(ps, K"error", TRIVIA_FLAG,
                       error="space required before `?` operator")
    end
    bump(ps, TRIVIA_FLAG) # ?
    t = peek_token(ps)
    if !preceding_whitespace(t)
        # a ?b : c
        bump_invisible(ps, K"error", TRIVIA_FLAG,
                       error="space required after `?` operator")
    end
    parse_eq_star(ParseState(ps, range_colon_enabled=false))
    t = peek_token(ps)
    if !preceding_whitespace(t)
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
    if !preceding_whitespace(t)
        # a ? b :c  ==>  (if a [ ] [?] [ ] b [ ] [:] (error-t) c)
        bump_invisible(ps, K"error", TRIVIA_FLAG,
                       error="space required after `:` in `?` expression")
    end
    parse_eq_star(ps)
    emit(ps, mark, K"if")
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
            # x .--> y  ==>  (call-i x .--> y)
            # x -->₁ y  ==>  (call-i x -->₁ y)
            bump(ps)
            parse_arrow(ps)
            emit(ps, mark, K"call", INFIX_FLAG)
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
        # struct try end  ==>  (struct false (error (try)) (block))
        name = untokenize(peek(ps))
        bump(ps)
        emit(ps, mark, K"error", error="Invalid type name `$name`")
    else
        parse_pipe_lt(ps)
    end
    n_comparisons = 0
    op_pos = NO_POSITION
    initial_tok = peek_token(ps)
    while is_prec_comparison(peek(ps))
        n_comparisons += 1
        op_pos = bump(ps)
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
            # x .<: y  ==>  (call-i x .<: y)
            emit(ps, mark, K"call", INFIX_FLAG)
        end
    elseif n_comparisons > 1
        # Comparison chains
        # x < y < z    ==> (comparison x < y < z)
        # x == y < z   ==> (comparison x == y < z)
        emit(ps, mark, K"comparison")
    end
end

# x <| y <| z  ==>  (call-i x <| (call-i y <| z))
# flisp: parse-pipe<
function parse_pipe_lt(ps::ParseState)
    parse_RtoL(ps, parse_pipe_gt, is_prec_pipe_lt, parse_pipe_lt)
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
                    preceding_whitespace(peek_token(ps)) &&
                    !preceding_whitespace(peek_token(ps, 2))
                # Tricky cases in space sensitive mode
                # [1 :a]      ==>  (hcat 1 (quote a))
                # [1 2:3 :a]  ==>  (hcat 1 (call-i 2 : 3) (quote a))
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
                parse_expr(ps)
                emit(ps, mark, K"call", INFIX_FLAG)
                break
            end
            n_colons += 1
            bump(ps, n_colons == 1 ? EMPTY_FLAGS : TRIVIA_FLAG)
            had_newline = peek(ps, skip_newlines=false) == K"NewlineWs"
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
                # 1:\n2 ==> (call-i 1 : (error))
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

# a - b - c  ==>  (call-i (call-i a - b) - c)
# a + b + c  ==>  (call-i a + b c)
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
            # [x +y]     ==>  (hcat x (call + y))
            # [x+y +z]   ==>  (hcat (call-i x + y) (call + z))
            # Conversely the following are infix calls
            # [x +₁y]    ==>  (vect (call-i x +₁ y))
            # [x+y+z]    ==>  (vect (call-i x + y z))
            # [x+y + z]  ==>  (vect (call-i x + y z))
            break
        end
        bump(ps)
        down(ps)
        if kind(t) in chain_ops && !is_decorated(t)
            # a + b + c    ==>  (call-i a + b c)
            # a + b .+ c   ==>  (call-i (call-i a + b) .+ c)
            parse_chain(ps, down, kind(t))
        end
        # a +₁ b +₁ c  ==>  (call-i (call-i a +₁ b) +₁ c)
        # a .+ b .+ c  ==>  (call-i (call-i a .+ b) .+ c)
        emit(ps, mark, K"call", INFIX_FLAG)
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
            # [x +y]  ==>  (hcat x (call + y))
            break
        end
        bump(ps, TRIVIA_FLAG)
        down(ps)
    end
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
    if k in KSet"<: >:"
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
            # <:(x::T)     ==>  (<: (:: x T))
            parse_where(ps, parse_juxtapose)
        else
            # <: A where B  ==>  (<: (where A B))
            mark = position(ps)
            bump(ps, TRIVIA_FLAG)
            parse_where(ps, parse_juxtapose)
            # Flisp parser handled this, but I don't know how it can happen...
            @check peek_behind(ps).kind != K"tuple"
            emit(ps, mark, k)
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
        bump_trivia(ps, skip_newlines=true)
        k = peek(ps)
        if k == K"{"
            m = position(ps)
            bump(ps, TRIVIA_FLAG)
            # x where \n {T}  ==>  (where x T)
            # x where {T,S}  ==>  (where x T S)
            ckind, cflags = parse_cat(ps, K"}", ps.end_symbol)
            if ckind != K"vect"
                # Various nonsensical forms permitted here
                # x where {T S}  ==>  (where x (bracescat (row T S)))
                # x where {y for y in ys}  ==>  (where x (braces (generator y (= y ys))))
                emit_braces(ps, m, ckind, cflags)
            end
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

# given the previous expression kind and the next token, is there a
# juxtaposition operator between them?
#
# flisp: juxtapose?
function is_juxtapose(ps, prev_k, t)
    k = kind(t)

    # Not juxtaposition - parse_juxtapose will consume only the first token.
    # x.3       ==>  x
    # sqrt(2)2  ==>  (call sqrt 2)
    # x' y      ==>  x
    # x 'y      ==>  x

    return !preceding_whitespace(t)                         &&
    (is_number(prev_k) ||
        (!is_number(k) &&  # disallow "x.3" and "sqrt(2)2"
         k != K"@"     &&  # disallow "x@time"
         !(is_block_form(prev_k)         ||
           is_syntactic_unary_op(prev_k) ||
           is_initial_reserved_word(ps, prev_k) )))  &&
    # https://github.com/JuliaLang/julia/issues/16356
    # 0xenomorph  ==>  0x0e
    !(prev_k in KSet"BinInt HexInt OctInt" && (k == K"Identifier" || is_keyword(k))) &&
    (!is_operator(k) || is_radical_op(k))            &&
    !is_closing_token(ps, k)                         &&
    !is_initial_reserved_word(ps, k)
end

# Juxtoposition. Ugh!
#
# 2x       ==>  (call-i 2 * x)
# 2(x)     ==>  (call-i 2 * x)
# (2)(3)x  ==>  (call-i 2 * 3 x)
# (x-1)y   ==>  (call-i (call-i x - 1) * y)
# x'y      ==>  x
#
# flisp: parse-juxtapose
function parse_juxtapose(ps::ParseState)
    mark = position(ps)
    parse_unary(ps)
    n_terms = 1
    while true
        prev_kind = peek_behind(ps).kind
        t = peek_token(ps)
        if !is_juxtapose(ps, prev_kind, t)
            break
        end
        if n_terms == 1
            bump_invisible(ps, K"*")
        end
        if prev_kind == K"String" || is_string_delim(t)
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
    t = peek_token(ps)
    k = kind(t)
    if !is_initial_operator(t)
        # :T      ==>  (quote T)
        # in::T   ==>  (:: in T)
        # isa::T  ==>  (:: isa T)
        parse_factor(ps)
        return
    end
    if k in KSet"- +"
        t2 = peek_token(ps, 2)
        if !preceding_whitespace(t2) && kind(t2) in KSet"Integer Float"
            k3 = peek(ps, 3)
            if is_prec_power(k3) || k3 in KSet"[ {"
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
    op_node_kind = is_type_operator(op_t) ? op_k : K"call"
    op_tok_flags = is_type_operator(op_t) ? TRIVIA_FLAG : EMPTY_FLAGS
    t2 = peek_token(ps, 2)
    k2 = kind(t2)
    if is_closing_token(ps, k2) || k2 in KSet"NewlineWs ="
        if is_dotted(op_t)
            # Standalone dotted operators are parsed as (|.| op)
            # .+    ==>  (. +)
            # .+\n  ==>  (. +)
            # .+ =  ==>  (. +)
            # .+)   ==>  (. +)
            # .&    ==>  (. &)
            bump_trivia(ps)
            bump_split(ps, (1, K".", TRIVIA_FLAG), (0, op_k, EMPTY_FLAGS))
            emit(ps, mark, K".")
        else
            # Standalone non-dotted operators
            # +)  ==>  +
            bump(ps)
        end
    elseif k2 == K"{" || (!is_unary_op(op_t) && k2 == K"(")
        # Call with type parameters or non-unary prefix call
        # +{T}(x::T)  ==>  (call (curly + T) (:: x T))
        # *(x)  ==>  (call * x)
        parse_factor(ps)
    elseif k2 == K"("
        # Cases like +(a;b) are ambiguous: are they prefix calls to + with b as
        # a keyword argument, or is `a;b` a block?  We resolve this with a
        # simple heuristic: if there were any commas (or an initial splat), it
        # was a function call.
        #
        # (The flisp parser only considers commas before `;` and thus gets this
        # last case wrong)
        bump(ps, op_tok_flags)

        # Setup possible whitespace error between operator and (
        ws_mark = position(ps)
        bump_trivia(ps)
        ws_mark_end = position(ps)
        ws_error_pos = emit(ps, ws_mark, K"TOMBSTONE")

        mark_before_paren = position(ps)
        bump(ps, TRIVIA_FLAG) # (
        initial_semi = peek(ps) == K";"
        is_call = Ref(false)
        is_block = Ref(false)
        parse_brackets(ps, K")") do had_commas, had_splat, num_semis, num_subexprs
            is_call[] = had_commas || had_splat || initial_semi
            is_block[] = !is_call[] && num_semis > 0
            return (needs_parameters=is_call[],
                    eq_is_kw_before_semi=is_call[],
                    eq_is_kw_after_semi=is_call[])
        end

        # The precedence between unary + and any following infix ^ depends on
        # whether the parens are a function call or not
        if is_call[]
            if preceding_whitespace(t2)
                # Whitespace not allowed before prefix function call bracket
                # + (a,b)   ==> (call + (error) a b)
                reset_node!(ps, ws_error_pos, kind=K"error")
                emit_diagnostic(ps, ws_mark, ws_mark_end,
                                error="whitespace not allowed between prefix function call and argument list")
            end
            # Prefix function calls for operators which are both binary and unary
            # +(a,b)    ==>  (call + a b)
            # +(a=1,)   ==>  (call + (kw a 1))
            # +(a...)   ==>  (call + (... a))
            # +(a;b,c)  ==>  (call + a (parameters b c))
            # +(;a)     ==>  (call + (parameters a))
            # Prefix calls have higher precedence than ^
            # +(a,b)^2  ==>  (call-i (call + a b) ^ 2)
            # +(a,b)(x)^2  ==>  (call-i (call (call + a b) x) ^ 2)
            emit(ps, mark, op_node_kind)
            parse_call_chain(ps, mark)
            parse_factor_with_initial_ex(ps, mark)
        else
            # Unary function calls with brackets as grouping, not an arglist
            if is_block[]
                # +(a;b)   ==>  (call + (block a b))
                emit(ps, mark_before_paren, K"block")
            end
            # Not a prefix operator call but a block; `=` is not `kw`
            # +(a=1)  ==>  (call + (= a 1))
            # Unary operators have lower precedence than ^
            # +(a)^2  ==>  (call + (call-i a ^ 2))
            # +(a)(x,y)^2  ==>  (call + (call-i (call a x y) ^ 2))
            parse_call_chain(ps, mark_before_paren)
            parse_factor_with_initial_ex(ps, mark_before_paren)
            emit(ps, mark, op_node_kind)
        end
    else
        if is_unary_op(op_t)
            # Normal unary calls
            # +x  ==>  (call + x)
            # √x  ==>  (call √ x)
            # ±x  ==>  (call ± x)
            bump(ps, op_tok_flags)
        else
            # /x     ==>  (call (error /) x)
            # +₁ x   ==>  (call (error +₁) x)
            # .<: x  ==>  (call (error .<:) x)
            bump(ps, error="not a unary operator")
        end
        parse_unary(ps)
        emit(ps, mark, op_node_kind)
    end
end

# handle ^ and .^
#
# x^y    ==>  (call-i x ^ y)
# x^y^z  ==>  (call-i x ^ (call-i y ^ z))
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
    if is_prec_power(peek(ps))
        bump(ps)
        parse_factor_after(ps)
        emit(ps, mark, K"call", INFIX_FLAG)
    end
end

# flisp: parse-factor-after
function parse_factor_after(ps::ParseState)
    parse_RtoL(ps, parse_juxtapose, is_prec_power, parse_factor_after)
end

# Parse type declarations and lambda syntax
# a::b      ==>   (:: a b)
# a->b      ==>   (-> a b)
#
# flisp: parse-decl-with-initial-ex
function parse_decl_with_initial_ex(ps::ParseState, mark)
    while peek(ps) == K"::"
        # a::b::c   ==>   (:: (:: a b) c)
        bump(ps, TRIVIA_FLAG)
        parse_where(ps, parse_call)
        emit(ps, mark, K"::")
    end
    if peek(ps) == K"->"
        # x -> y    ==>  (-> x y)
        # a::b->c   ==>  (-> (:: a b) c)
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
# ::a  ==>  (:: a)
# $a   ==>  ($ a)
#
# flisp: parse-unary-prefix
function parse_unary_prefix(ps::ParseState)
    mark = position(ps)
    k = peek(ps)
    if is_syntactic_unary_op(k)
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
            emit(ps, mark, k)
        end
    else
        parse_atom(ps)
    end
end

# Parse a symbol or interpolation syntax
function parse_identifier_or_interpolate(ps::ParseState)
    mark = position(ps)
    parse_unary_prefix(ps)
    b = peek_behind(ps)
    # export (x::T) ==> (export (error (:: x T)))
    # export outer  ==> (export outer)
    # export ($f)   ==> (export ($ f))
    ok = (b.is_leaf  && (b.kind == K"Identifier" || is_operator(b.kind))) ||
         (!b.is_leaf && b.kind == K"$")
    if !ok
        emit(ps, mark, K"error", error="Expected identifier")
    end
end

function finish_macroname(ps, mark, valid_macroname, macro_name_position,
                          name_kind=nothing)
    if valid_macroname
        if isnothing(name_kind)
            name_kind = macro_name_kind(peek_behind(ps, macro_name_position).kind)
        end
        reset_node!(ps, macro_name_position, kind = name_kind)
    else
        emit(ps, mark, K"error", error="not a valid macro name or macro module path")
    end
end

# Parses a chain of sufficies at function call precedence, leftmost binding
# tightest.
# f(a,b)    ==> (call f a b)
# f(a).g(b) ==> (call (. (call f a) (quote g)) b)
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
    # $A.@x  ==>  (macrocall (. ($ A) (quote @x)))
    valid_macroname = peek_behind(ps, skip_trivia=false).kind in KSet"Identifier . $"
    # We record the last component of chains of dot-separated identifiers so we
    # know which identifier was the macro name.
    macro_name_position = position(ps) # points to same output span as peek_behind
    while true
        this_iter_valid_macroname = false
        t = peek_token(ps)
        k = kind(t)
        if is_macrocall && (preceding_whitespace(t) || is_closing_token(ps, k))
            # Macro calls with space-separated arguments
            # @foo a b    ==> (macrocall @foo a b)
            # @foo (x)    ==> (macrocall @foo x)
            # @foo (x,y)  ==> (macrocall @foo (tuple x y))
            # a().@x y    ==> (macrocall (error (. (call a) (quote x))) y)
            # [@foo "x"]  ==> (vect (macrocall @foo "x"))
            finish_macroname(ps, mark, valid_macroname, macro_name_position)
            let ps = with_space_sensitive(ps)
                # Space separated macro arguments
                # A.@foo a b    ==> (macrocall (. A (quote @foo)) a b)
                # @A.foo a b    ==> (macrocall (. A (quote @foo)) a b)
                n_args = parse_space_separated_exprs(ps)
                is_doc_macro = peek_behind(ps, macro_name_position).orig_kind == K"doc"
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
        elseif (ps.space_sensitive && preceding_whitespace(t) &&
                k in KSet"( [ { \ Char \" \"\"\" ` ```")
            # [f (x)]  ==>  (hcat f x)
            # [f "x"]  ==>  (hcat f "x")
            break
        elseif k == K"("
            if is_macrocall
                # a().@x(y)  ==> (macrocall (error (. (call a) (quote x))) y)
                finish_macroname(ps, mark, valid_macroname, macro_name_position)
            end
            # f(a,b)  ==>  (call f a b)
            # f (a)  ==>  (call f (error-t) a b)
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
        elseif k == K"["
            if is_macrocall
                # a().@x[1]  ==> (macrocall (ref (error (. (call a) (quote x))) 1))
                finish_macroname(ps, mark, valid_macroname, macro_name_position)
            end
            # a [i]  ==>  (ref a (error-t) i)
            bump_disallowed_space(ps)
            bump(ps, TRIVIA_FLAG)
            ckind, cflags = parse_cat(ParseState(ps, end_symbol=true),
                                      K"]", ps.end_symbol)
            # a[i]    ==>  (ref a i)
            # a[i,j]  ==>  (ref a i j)
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
            if is_macrocall
                emit(ps, mark, K"macrocall")
                break
            end
        elseif k == K"."
            # x .y  ==>  (. x (error-t) (quote y))
            bump_disallowed_space(ps)
            if peek(ps, 2) == K"'"
                # f.'  =>  f (error-t . ')
                emark = position(ps)
                bump(ps)
                bump(ps)
                emit(ps, emark, K"error", TRIVIA_FLAG,
                     error="the .' operator for transpose is discontinued")
                valid_macroname = false
                continue
            end
            if !isnothing(macro_atname_range)
                # Allow `@` in macrocall only in first and last position
                # A.B.@x  ==>  (macrocall (. (. A (quote B)) (quote @x)))
                # @A.B.x  ==>  (macrocall (. (. A (quote B)) (quote @x)))
                # A.@B.x  ==>  (macrocall (. (. A (error-t) B) (quote @x)))
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
                # f. (x)    ==>  (. f (error-t) (tuple x))
                bump_disallowed_space(ps)
                m = position(ps)
                bump(ps, TRIVIA_FLAG)
                parse_call_arglist(ps, K")", is_macrocall)
                emit(ps, m, K"tuple")
                emit(ps, mark, K".")
            elseif k == K":"
                # A.:+  ==>  (. A (quote +))
                # A.: +  ==>  (. A (error-t) (quote +))
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
                # A.$B.@x  ==>  (macrocall (. (. A (inert ($ B))) (quote @x)))
                this_iter_valid_macroname = true
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
                this_iter_valid_macroname = true
            else
                # Field/property syntax
                # f.x.y ==> (. (. f (quote x)) (quote y))
                m = position(ps)
                parse_atom(ps, false)
                macro_name_position = position(ps)
                emit(ps, m, K"quote")
                emit(ps, mark, K".")
                this_iter_valid_macroname = true
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
                finish_macroname(ps, mark, valid_macroname, macro_name_position)
            end
            m = position(ps)
            # S {a} ==> (curly S (error-t) a)
            bump_disallowed_space(ps)
            bump(ps, TRIVIA_FLAG)
            parse_call_arglist(ps, K"}", is_macrocall)
            if is_macrocall
                # @S{a,b} ==> (macrocall S (braces a b))
                emit(ps, m, K"braces")
                emit(ps, mark, K"macrocall")
                min_supported_version(v"1.6", ps, mark, "macro call without space before `{}`")
                break
            else
                # S{a,b} ==> (curly S a b)
                emit(ps, mark, K"curly")
            end
        elseif k in KSet" \" \"\"\" ` ``` " &&
                !preceding_whitespace(t) && valid_macroname
            # Custom string and command literals
            # x"str" ==> (macrocall @x_str "str")
            # x`str` ==> (macrocall @x_cmd "str")
            # x""    ==> (macrocall @x_str "")
            # x``    ==> (macrocall @x_cmd "")
            # Triple quoted procesing for custom strings
            # r"""\nx""" ==> (macrocall @r_str "x")
            # r"""\n x\n y"""     ==> (macrocall @r_str (string-sr "x\n" "y"))
            # r"""\n x\\n y"""    ==> (macrocall @r_str (string-sr "x\\\n" "y"))
            #
            # Use a special token kind for string and cmd macro names so the
            # names can be expanded later as necessary.
            outk = is_string_delim(k) ? K"StringMacroName" : K"CmdMacroName"
            finish_macroname(ps, mark, valid_macroname, macro_name_position, outk)
            parse_string(ps, true)
            t = peek_token(ps)
            k = kind(t)
            if !preceding_whitespace(t) && (k == K"Identifier" || is_keyword(k) || is_word_operator(k) || is_number(k))
                # Macro sufficies can include keywords and numbers
                # x"s"y    ==> (macrocall @x_str "s" "y")
                # x"s"end  ==> (macrocall @x_str "s" "end")
                # x"s"in   ==> (macrocall @x_str "s" "in")
                # x"s"2    ==> (macrocall @x_str "s" 2)
                # x"s"10.0 ==> (macrocall @x_str "s" 10.0)
                suffix_kind = (k == K"Identifier" || is_keyword(k) ||
                               is_word_operator(k)) ? K"String" : k
                bump(ps, remap_kind=suffix_kind)
            end
            emit(ps, mark, K"macrocall")
        else
            break
        end
        valid_macroname &= this_iter_valid_macroname
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
        # for x in xs, y in ys \n a \n end ==> (for (block (= x xs) (= y ys)) (block a))
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
        if peek(ps) ∉ KSet"NewlineWs ;"
            # let x=1\n end    ==>  (let (= x 1) (block))
            m = position(ps)
            n_subexprs = parse_comma_separated(ps, parse_eq_star)
            kb = peek_behind(ps).kind
            # Wart: This ugly logic seems unfortunate. Why not always emit a block?
            # let x=1 ; end   ==>  (let (= x 1) (block))
            # let x::1 ; end  ==>  (let (:: x 1) (block))
            # let x ; end     ==>  (let x (block))
            if n_subexprs > 1 || !(kb in KSet"Identifier = ::")
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
    elseif word in KSet"const global local"
        parse_const_local_global(ps)
    elseif word in KSet"function macro"
        parse_function(ps)
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
        # struct A <: B \n a::X \n end  ==>  (struct false (<: A B) (block (:: a X)))
        if word == K"mutable"
            # mutable struct A end  ==>  (struct true A (block))
            bump(ps, TRIVIA_FLAG)
            bump_invisible(ps, K"true")
        else
            # struct A end  ==>  (struct false A (block))
            bump_invisible(ps, K"false")
        end
        @check peek(ps) == K"struct"
        bump(ps, TRIVIA_FLAG)
        parse_subtype_spec(ps)
        parse_block(ps)
        bump_closing_token(ps, K"end")
        emit(ps, mark, K"struct")
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
            # return\nx   ==>  (return nothing)
            # return)     ==>  (return nothing)
            bump_invisible(ps, K"nothing")
        else
            # return x    ==>  (return x)
            # return x,y  ==>  (return (tuple x y))
            parse_eq(ps)
        end
        emit(ps, mark, K"return")
    elseif word in KSet"break continue"
        # break     ==>  (break)
        # continue  ==>  (continue)
        bump(ps)
        k = peek(ps)
        if !(k in KSet"NewlineWs ; ) : EndMarker" || (k == K"end" && !ps.end_symbol))
            recover(is_closer_or_newline, ps, TRIVIA_FLAG,
                    error="unexpected token after $(untokenize(word))")
        end
    elseif word in KSet"module baremodule"
        # module A end  ==> (module true A (block))
        # baremodule A end ==> (module false A (block))
        bump(ps, TRIVIA_FLAG)
        bump_invisible(ps, (word == K"module") ? K"true" : K"false")
        if is_reserved_word(peek(ps))
            # module do \n end  ==>  (module true (error do) (block))
            bump(ps, error="Invalid module name")
        else
            # module $A end  ==>  (module true ($ A) (block))
            parse_unary_prefix(ps)
        end
        # module A \n a \n b \n end  ==>  (module true A (block a b))
        # module A \n "x"\na \n end  ==>  (module true A (block (core_@doc "x" a)))
        parse_block(ps, parse_docstring)
        bump_closing_token(ps, K"end")
        emit(ps, mark, K"module")
    elseif word == K"export"
        # export a         ==>  (export a)
        # export @a        ==>  (export @a)
        # export a, \n @b  ==>  (export a @b)
        # export +, ==     ==>  (export + ==)
        # export \n a      ==>  (export a)
        # export \$a, \$(a*b) ==> (export (\$ a) (\$ (call-i a * b)))
        bump(ps, TRIVIA_FLAG)
        parse_comma_separated(ps, parse_atsym)
        emit(ps, mark, K"export")
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

function parse_const_local_global(ps)
    mark = position(ps)
    scope_mark = mark
    has_const = false
    scope_k = K"None"
    k = peek(ps)
    if k in KSet"global local"
        # global x  ==>  (global x)
        # local x   ==>  (local x)
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
        bump(ps, TRIVIA_FLAG)
        k = peek(ps)
        if k in KSet"global local"
            # const global x = 1   ==>  (const (global (= x 1)))
            # const local x = 1    ==>  (const (local (= x 1)))
            scope_k = k
            scope_mark = position(ps)
            bump(ps, TRIVIA_FLAG)
        end
    end
    # Like parse_assignment, but specialized so that we can omit the
    # tuple when there's commas but no assignment.
    beforevar_mark = position(ps)
    n_commas = parse_comma(ps, false)
    t = peek_token(ps)
    has_assignment = is_prec_assignment(t)
    if n_commas >= 1 && (has_assignment || has_const)
        # const x,y = 1,2  ==>  (const (= (tuple x y) (tuple 1 2)))
        # Maybe nonsensical? But this is what the flisp parser does.
        #v1.8: const x,y  ==>  (const (tuple x y))
        emit(ps, beforevar_mark, K"tuple")
    end
    if has_assignment
        # const x = 1   ==>  (const (= x 1))
        # global x ~ 1  ==>  (global (call-i x ~ 1))
        # global x += 1 ==>  (global (+= x 1))
        parse_assignment_with_initial_ex(ps, beforevar_mark, parse_comma, false)
    else
        # global x    ==>  (global x)
        # local x     ==>  (local x)
        # global x,y  ==>  (global x y)
    end
    if has_const && (!has_assignment || is_dotted(t))
        # Const fields https://github.com/JuliaLang/julia/pull/43305
        #v1.8: const x     ==>  (const x)
        #v1.8: const x::T  ==>  (const (:: x T))
        # Disallowed const forms on <= 1.7
        #v1.7: const x       ==> (const (error x))
        #v1.7: const x .= 1  ==>  (const (error (.= x 1)))
        min_supported_version(v"1.8", ps, beforevar_mark,
                              "`const` struct field without assignment")
    end
    if scope_k != K"None"
        emit(ps, scope_mark, scope_k)
    end
    if has_const
        # TODO: Normalize `global const` during Expr conversion rather than here?
        emit(ps, mark, K"const")
    end
end

# Parse function and macro definitions
function parse_function(ps::ParseState)
    mark = position(ps)
    word = peek(ps)
    @check word in KSet"macro function"
    is_function = word == K"function"
    is_anon_func = false
    bump(ps, TRIVIA_FLAG)
    bump_trivia(ps)

    def_mark = position(ps)
    if !is_function
        # Parse macro name
        parse_identifier_or_interpolate(ps)
        kb = peek_behind(ps).orig_kind
        if is_initial_reserved_word(ps, kb)
            # macro while(ex) end  ==> (macro (call (error while) ex) (block))
            emit(ps, def_mark, K"error", error="Invalid macro name")
        else
            # macro f()     end  ==>  (macro (call f) (block))
            # macro (:)(ex) end  ==>  (macro (call : ex) (block))
            # macro (type)(ex) end  ==>  (macro (call type ex) (block))
            # macro $f()    end  ==>  (macro (call ($ f)) (block))
            # macro ($f)()  end  ==>  (macro (call ($ f)) (block))
        end
    else
        if peek(ps) == K"("
            bump(ps, TRIVIA_FLAG)
            # When an initial parenthesis is present, we might either have the
            # function name or the argument list in an anonymous function. We
            # use parse_brackets directly here (rather than dispatching to it
            # via parse_atom) so we can distinguish these two cases by peeking
            # at the following parenthesis, if present.
            #
            # The flisp parser disambiguates this case quite differently,
            # producing less consistent syntax for anonymous functions.
            is_anon_func_ = Ref(is_anon_func)
            parse_brackets(ps, K")") do _, _, _, _
                is_anon_func_[] = peek(ps, 2) != K"("
                return (needs_parameters     = is_anon_func_[],
                        eq_is_kw_before_semi = is_anon_func_[],
                        eq_is_kw_after_semi  = is_anon_func_[])
            end
            is_anon_func = is_anon_func_[]
            if is_anon_func
                # function (x) body end ==>  (function (tuple x) (block body))
                # function (x,y) end    ==>  (function (tuple x y) (block))
                # function (x=1) end    ==>  (function (tuple (kw x 1)) (block))
                # function (;x=1) end   ==>  (function (tuple (parameters (kw x 1))) (block))
                emit(ps, def_mark, K"tuple")
            else
                # function (:)() end    ==> (function (call :) (block))
                # function (x::T)() end ==> (function (call (:: x T)) (block))
                # function (::T)() end  ==> (function (call (:: T)) (block))
            end
        else
            parse_unary_prefix(ps)
        end
        if !is_anon_func
            kb = peek_behind(ps).orig_kind
            if is_reserved_word(kb)
                # function begin() end  ==>  (function (call (error begin)) (block))
                emit(ps, def_mark, K"error", error="Invalid function name")
            else
                # function f() end     ==>  (function (call f) (block))
                # function type() end  ==>  (function (call type) (block))
                # function \n f() end  ==>  (function (call f) (block))
                # function $f() end    ==>  (function (call ($ f)) (block))
                # function (:)() end  ==>  (function (call :) (block))
                # function (::Type{T})(x) end ==> (function (call (:: (curly Type T)) x) (block))
            end
        end
    end
    if peek(ps, skip_newlines=true) == K"end" && !is_anon_func
        # Function/macro definition with no methods
        # function f end       ==> (function f)
        # (function f \n end)  ==> (function f)
        # function f \n\n end  ==> (function f)
        # function $f end      ==> (function ($ f))
        # macro f end          ==> (macro f)
        bump(ps, TRIVIA_FLAG, skip_newlines=true)
        emit(ps, mark, word)
        return
    end
    if !is_anon_func
        # Parse function argument list
        # function f(x,y)  end    ==>  (function (call f x y) (block))
        # function f{T}()  end    ==>  (function (call (curly f T)) (block))
        # function A.f()   end    ==>  (function (call (. A (quote f))) (block))
        parse_call_chain(ps, def_mark)
        if peek_behind(ps).kind != K"call"
            # function f body end  ==>  (function (error f) (block body))
            emit(ps, def_mark, K"error",
                 error="Invalid signature in $(untokenize(word)) definition")
        end
    end
    if is_function && peek(ps) == K"::"
        # Function return type
        # function f()::T    end   ==>  (function (:: (call f) T) (block))
        # function f()::g(T) end   ==>  (function (:: (call f) (call g T)) (block))
        bump(ps, TRIVIA_FLAG)
        parse_call(ps)
        emit(ps, def_mark, K"::")
    end
    if peek(ps) == K"where"
        # Function signature where syntax
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
        #v1.7: try catch ; else end ==> (try (block) false (block) (error (block)) false)
        min_supported_version(v"1.8", ps, else_mark, "`else` after `catch`")
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
        # try x finally y catch e z end  ==>  (try-f (block x) false false false (block y) e (block z))
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
    if k in KSet"NewlineWs ;" || is_closing_token(ps, k)
        # try x catch end      ==>  (try (block x) false (block) false false)
        # try x catch ; y end  ==>  (try (block x) false (block y) false false)
        # try x catch \n y end ==>  (try (block x) false (block y) false false)
        bump_invisible(ps, K"false")
    else
        # try x catch e y end   ==>  (try (block x) e (block y) false false)
        # try x catch $e y end  ==>  (try (block x) ($ e) (block y) false false)
        parse_identifier_or_interpolate(ps)
    end
    parse_block(ps)
end

# flisp: parse-do
function parse_do(ps::ParseState)
    ps = normal_context(ps)
    mark = position(ps)
    if peek(ps) in KSet"NewlineWs ;"
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

function macro_name_kind(k)
    return k == K"Identifier"    ? K"MacroName"    :
           k == K"."             ? K"@."           :
           internal_error("unrecognized source kind for macro name ", k)
end

# If remap_kind is false, the kind will be remapped by parse_call_chain after
# it discovers which component of the macro's module path is the macro name.
#
# flisp: parse-macro-name
function parse_macro_name(ps::ParseState)
    bump_disallowed_space(ps)
    mark = position(ps)
    k = peek(ps)
    if k == K"."
        # @. y  ==>  (macrocall @__dot__ y)
        bump(ps)
    else
        # @! x   ==>  (macrocall @! x)
        # @.. x  ==>  (macrocall @.. x)
        # @$ x   ==>  (macrocall @$ x)
        let ps = with_space_sensitive(ps)
            parse_atom(ps, false)
        end
    end
end

# Parse an identifier, interpolation of @-prefixed symbol
#
# flisp: parse-atsym
function parse_atsym(ps::ParseState)
    bump_trivia(ps)
    if peek(ps) == K"@"
        # export @a  ==>  (export @a)
        # export a, \n @b  ==>  (export a @b)
        bump(ps, TRIVIA_FLAG)
        parse_macro_name(ps)
        reset_node!(ps, position(ps), kind=macro_name_kind(peek_behind(ps).kind))
    else
        # export a  ==>  (export a)
        # export \n a  ==>  (export a)
        # export $a, $(a*b)  ==>  (export ($ a) ($ (call * a b)))
        parse_identifier_or_interpolate(ps)
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
            # import A as B: x  ==>  (import (: (error (as (. A) B)) (. x)))
            emit(ps, emark, K"error", error="`as` before `:` in import/using")
        end
    elseif k == K","
        bump(ps, TRIVIA_FLAG)
        has_comma = true
    end
    if has_import_prefix || has_comma
        # import A, y      ==>  (import (. A) (. y))
        # import A: x, y   ==>  (import (: (. A) (. x) (. y)))
        # import A: +, ==  ==>  (import (: (. A) (. +) (. ==)))
        has_import_prefix_ = has_import_prefix
        parse_comma_separated(ps, ps1->parse_import(ps1, word, has_import_prefix_))
        if peek(ps) == K":"
            # Error recovery
            # import A: x, B: y ==> (import (: (. A) (. x) (. B) (error-t (. y))))
            emark = position(ps)
            bump(ps, TRIVIA_FLAG)
            parse_comma_separated(ps, ps1->parse_import(ps1, word, has_import_prefix_))
            emit(ps, emark, K"error", TRIVIA_FLAG,
                 error="`:` can only be used when importing a single module. Split imports into multiple lines")
        end
    end
    if has_import_prefix
        # import A: x  ==>  (import (: (. A) (. x)))
        emit(ps, mark, K":")
    end
    # using  A  ==>  (using (. A))
    # import A  ==>  (import (. A))
    emit(ps, mark, word)
end

# Parse individual module path and renaming with `as`
#
# flisp: parse-import
function parse_import(ps::ParseState, word, has_import_prefix)
    mark = position(ps)
    parse_import_path(ps)
    # import A: x, y   ==>  (import (: (. A) (. x) (. y)))
    if peek(ps) == K"as"
        # import A as B     ==>  (import (as (. A) B))
        # import A: x as y  ==>  (import (: (. A) (as (. x) y)))
        # using  A: x as y  ==>  (using (: (. A) (as (. x) y)))
        bump(ps, TRIVIA_FLAG)
        parse_atsym(ps)
        emit(ps, mark, K"as")
        if word == K"using" && !has_import_prefix
            # using A as B     ==>  (using (error (as (. A) B)))
            # using A, B as C  ==>  (using (. A) (error (as (. B) C)))
            emit(ps, mark, K"error",
                 error="`using` with `as` renaming requires a `:` and context module")
        end
        #v1.5: import A as B     ==>  (import (error (as (. A) B)))
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
    # import .A     ==> (import (. . A))
    # import ..A    ==> (import (. . . A))
    # import ...A   ==> (import (. . . . A))
    # import ....A  ==> (import (. . . . . A))
    # Dots with spaces are allowed (a misfeature?)
    # import . .A    ==> (import (. . . A))
    first_dot = true
    while true
        m = position(ps)
        bump_trivia(ps)
        m2 = position(ps)
        k = peek(ps)
        if k == K"."
            bump(ps)
        elseif k == K".."
            bump_split(ps, (1,K".",EMPTY_FLAGS), (1,K".",EMPTY_FLAGS))
        elseif k == K"..."
            bump_split(ps, (1,K".",EMPTY_FLAGS), (1,K".",EMPTY_FLAGS), (1,K".",EMPTY_FLAGS))
        else
            break
        end
        if !first_dot && m != m2
            emit_diagnostic(ps, m, m2, warning="space between dots in import path")
        end
        first_dot = false
    end
    if is_dotted(peek_token(ps))
        # Modules with operator symbol names
        # import .⋆  ==>  (import (. . ⋆))
        bump_trivia(ps)
        bump_split(ps, (1,K".",EMPTY_FLAGS), (1,peek(ps),EMPTY_FLAGS))
    else
        # import @x     ==>  (import (. @x))
        # import $A     ==>  (import (. ($ A)))
        parse_atsym(ps)
    end
    while true
        t = peek_token(ps)
        k = kind(t)
        if k == K"."
            # import A.B    ==>  (import (. A B))
            # import $A.@x  ==>  (import (. ($ A) @x))
            # import A.B.C  ==>  (import (. A B C))
            bump_disallowed_space(ps)
            bump(ps, TRIVIA_FLAG)
            parse_atsym(ps)
        elseif is_dotted(t)
            # Resolve tokenization ambiguity: In imports, dots are part of the
            # path, not operators
            # import A.==   ==>  (import (. A ==))
            # import A.⋆.f  ==>  (import (. A ⋆ f))
            if preceding_whitespace(t)
                # Whitespace in import path allowed but discouraged
                # import A .==  ==>  (import (. A ==))
                emit_diagnostic(ps, whitespace=true,
                                warning="space between dots in import path")
            end
            bump_trivia(ps)
            bump_split(ps, (1,K".",TRIVIA_FLAG), (1,k,EMPTY_FLAGS))
        # elseif k == K".."
        #     # The flisp parser does this, but it's nonsense?
        #     # import A..  !=>  (import (. A .))
        #     bump_split(ps, (1,K".",TRIVIA_FLAG), (1,K".",EMPTY_FLAGS))
        elseif k == K"..."
            # Import the .. operator
            # import A...  ==>  (import (. A ..))
            bump_split(ps, (1,K".",TRIVIA_FLAG), (2,K"..",EMPTY_FLAGS))
        elseif k in KSet"NewlineWs ; , : EndMarker"
            # import A; B  ==>  (import (. A))
            break
        else
            # Could we emit a more comprehensible error here?
            break
        end
    end
    emit(ps, mark, K".")
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
# (i,j) in iter  ==>  (= (tuple i j) iter)
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
            # outer (x,y) = rhs  ==>  (= (outer (tuple x y)) rhs)
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
function parse_call_arglist(ps::ParseState, closer, is_macrocall)
    ps = ParseState(ps, for_generator=true)

    parse_brackets(ps, closer) do _, _, _, _
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
    parse_brackets(ps, closer) do _, _, _, _
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
# We deal with this by only emitting the flatten:
#
# (flatten xy (= x xs) (= y ys))
#
# then reconstructing the nested flattens and generators when converting to Expr.
#
# [x for a = as for b = bs if cond1 for c = cs if cond2] ==> (comprehension (flatten x (= a as) (filter (= b bs) cond1) (filter (= c cs) cond2)))
#
# flisp: parse-generator
function parse_generator(ps::ParseState, mark, flatten=false)
    t = peek_token(ps)
    if !preceding_whitespace(t)
        # [(x)for x in xs]  ==>  (comprehension (generator x (error) (= x xs)))
        bump_invisible(ps, K"error", TRIVIA_FLAG,
                       error="Expected space before `for` in generator")
    end
    @check kind(t) == K"for"
    bump(ps, TRIVIA_FLAG)
    filter_mark = position(ps)
    parse_comma_separated(ps, parse_iteration_spec)
    if peek(ps) == K"if"
        # (a for x in xs if cond) ==> (generator a (filter (= x xs) cond))
        bump(ps, TRIVIA_FLAG)
        parse_cond(ps)
        emit(ps, filter_mark, K"filter")
    end
    t = peek_token(ps)
    if kind(t) == K"for"
        # (xy for x in xs for y in ys)  ==> (flatten xy (= x xs) (= y ys))
        # (xy for x in xs for y in ys for z in zs)  ==> (flatten xy (= x xs) (= y ys) (= z zs))
        parse_generator(ps, mark, true)
        if !flatten
            emit(ps, mark, K"flatten")
        end
    elseif !flatten
        # (x for a in as)  ==>  (generator x (= a as))
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
        # Treat a linebreak prior to a value as a semicolon (ie, separator for
        # the first dimension) if no previous semicolons observed
        # [a \n b]  ==> (vcat a b)
        return (1, -1)
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
            bump(ps, TRIVIA_FLAG)
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
        is_tuple = Ref(false)
        is_block = Ref(false)
        parse_brackets(ps, K")") do had_commas, had_splat, num_semis, num_subexprs
            is_tuple[] = had_commas || (had_splat && num_semis >= 1) ||
                       (initial_semi && (num_semis == 1 || num_subexprs > 0))
            is_block[] = num_semis > 0
            return (needs_parameters=is_tuple[],
                    eq_is_kw_before_semi=false,
                    eq_is_kw_after_semi=is_tuple[])
        end
        if is_tuple[]
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
            # (x...;)         ==> (tuple (... x) (parameters))
            # (x...; y)       ==> (tuple (... x) (parameters y))
            # (; a=1; b=2)    ==> (tuple (parameters (kw a 1) (parameters (kw b 2))))
            # (a; b; c,d)     ==> (tuple a (parameters b (parameters c d)))
            # (a=1, b=2; c=3) ==> (tuple (= a 1) (= b 2) (parameters (kw c 3)))
            emit(ps, mark, K"tuple")
        elseif is_block[]
            # Blocks
            # (;;)        ==>  (block)
            # (a=1;)      ==>  (block (= a 1))
            # (a;b;;c)    ==>  (block a b c)
            # (a=1; b=2)  ==>  (block (= a 1) (= b 2))
            emit(ps, mark, K"block")
        else
            # Parentheses used for grouping
            # (a * b)     ==>  (call-i * a b)
            # (a=1)       ==>  (= a 1)
            # (x)         ==>  x
            # (a...)      ==>  (... a)
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
    params_marks = acquire_positions(ps.stream)
    eq_positions = acquire_positions(ps.stream)
    last_eq_before_semi = 0
    num_subexprs = 0
    num_semis = 0
    had_commas = false
    had_splat = false
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
        elseif k == K","
            had_commas = true
            bump(ps, TRIVIA_FLAG)
        elseif is_closing_token(ps, k)
            # Error; handled below in bump_closing_token
            break
        else
            mark = position(ps)
            eq_pos = parse_eq_star(ps)
            num_subexprs += 1
            if num_subexprs == 1
                had_splat = peek_behind(ps).kind == K"..."
            end
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
                # (x for a in as)       ==>  (generator x (= a as))
                # (x \n\n for a in as)  ==>  (generator x (= a as))
                parse_generator(ps, mark)
            else
                # Error - recovery done when consuming closing_kind
                break
            end
        end
    end
    actions = after_parse(had_commas, had_splat, num_semis, num_subexprs)
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
    release_positions(ps.stream, params_marks)
    release_positions(ps.stream, eq_positions)
    bump_closing_token(ps, closing_kind)
end

is_indentation(b::UInt8) = (b == UInt8(' ') || b == UInt8('\t'))

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
    buf = textbuf(ps)
    str_flags = (triplestr ? TRIPLE_STRING_FLAG : EMPTY_FLAGS) |
                (raw       ? RAW_STRING_FLAG : EMPTY_FLAGS)
    bump(ps, TRIVIA_FLAG)
    first_chunk = true
    n_valid_chunks = 0
    removed_initial_newline = false
    had_interpolation = false
    prev_chunk_newline = false
    while true
        t = peek_full_token(ps)
        k = kind(t)
        if k == K"$"
            @assert !raw  # The lexer detects raw strings separately
            bump(ps, TRIVIA_FLAG)
            k = peek(ps)
            if k == K"("
                # "a $(x + y) b"  ==> (string "a " (call-i x + y) " b")
                m = position(ps)
                parse_atom(ps)
                # https://github.com/JuliaLang/julia/pull/38692
                prev = peek_behind(ps)
                if prev.kind == string_chunk_kind
                    # Wrap interpolated literal strings in (string) so we can
                    # distinguish them from the surrounding text (issue #38501)
                    # "hi$("ho")"      ==>  (string "hi" (string "ho"))
                    # "hi$("""ho""")"  ==>  (string "hi" (string-s "ho"))
                    emit(ps, m, K"string", prev.flags)
                end
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
            n_valid_chunks += 1
            had_interpolation = true
            prev_chunk_newline = false
        elseif k == string_chunk_kind
            if triplestr && first_chunk && span(t) <= 2 &&
                    begin
                        s = span(t)
                        b = buf[last_byte(t)]
                        # Test whether the string is a single logical newline
                        (s == 1 && (b == UInt8('\n') || b == UInt8('\r'))) ||
                        (s == 2 && (buf[first_byte(t)] == UInt8('\r') && b == UInt8('\n')))
                    end
                # First line of triple string is a newline only: mark as trivia.
                # """\nx"""    ==> "x"
                # """\n\nx"""  ==> (string-s "\n" "x")
                bump(ps, TRIVIA_FLAG)
                first_chunk = false
                prev_chunk_newline = true
            else
                if triplestr
                    # Triple-quoted dedenting:
                    # Various newlines (\n \r \r\n) and whitespace (' ' \t)
                    # """\n x\n y"""      ==> (string-s "x\n" "y")
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
                    if prev_chunk_newline && (b = buf[first_byte(t)];
                                              b != UInt8('\n') && b != UInt8('\r'))
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
                            while i < last_byte(t) && is_indentation(buf[i+1])
                                i += 1
                            end
                            indent_ref_i = first_byte(t)
                            indent_ref_len = i - first_byte(t) + 1
                        else
                            # Matching the current indentation with reference,
                            # shortening length if necessary.
                            j = 0
                            while j < span(t) && j < indent_ref_len
                                if buf[j + first_byte(t)] != buf[j + indent_ref_i]
                                    break
                                end
                                j += 1
                            end
                            indent_ref_len = min(indent_ref_len, j)
                        end
                        # Prepare a place for indentiation trivia, if necessary
                        push!(indent_chunks, bump_invisible(ps, K"TOMBSTONE"))
                    end
                    b = buf[last_byte(t)]
                    prev_chunk_newline = b == UInt8('\n') || b == UInt8('\r')
                end
                bump(ps, str_flags)
                first_chunk = false
                n_valid_chunks += 1
            end
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
                n_valid_chunks -= 1
            end
        end
    end
    release_positions(ps.stream, indent_chunks)
    if had_end_delim
        if n_valid_chunks == 0
            # Empty strings, or empty after triple quoted processing
            # "" ==> ""
            # """\n  """ ==> ""
            bump_invisible(ps, string_chunk_kind, str_flags)
        end
        bump(ps, TRIVIA_FLAG)
    else
        # Missing delimiter recovery
        # "str   ==> "str" (error)
        bump_invisible(ps, K"error", TRIVIA_FLAG, error="Unterminated string literal")
    end
    if n_valid_chunks > 1 || had_interpolation
        # String interpolations
        # "$x$y$z"  ==> (string x y z)
        # "$(x)"    ==> (string x)
        # "$x"      ==> (string x)
        # """$x"""  ==> (string-s x)
        #
        # Strings with embedded whitespace trivia
        # "a\\\nb"      ==> (string "a" "b")
        # "a\\\rb"      ==> (string "a" "b")
        # "a\\\r\nb"    ==> (string "a" "b")
        # "a\\\n \tb"   ==> (string "a" "b")
        emit(ps, mark, K"string", str_flags)
    else
        # Strings with only a single valid string chunk
        # "str" ==> "str"
        # "a\\\n"   ==> "a"
        # "a\\\r"   ==> "a"
        # "a\\\r\n" ==> "a"
    end
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
    if leading_kind == K":"
        # symbol/expression quote
        # :foo  ==>  (quote foo)
        # : foo  ==>  (quote (error-t) foo)
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
            # : a  ==> (quote (error-t) a))
            # ===
            # :
            # a
            # ==> (quote (error))
            bump_trivia(ps, TRIVIA_FLAG,
                        error="whitespace not allowed after `:` used for quoting")
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
            # :(end) ==> (quote (error (end)))
            # Being inside quote makes end non-special again (issue #27690)
            # a[:(end)]  ==>  (ref a (quote (error-t end)))
            parse_atom(ParseState(ps, end_symbol=false), false)
        end
        emit(ps, mark, K"quote")
    elseif leading_kind == K"=" && is_plain_equals(peek_token(ps))
        # =   ==> (error =)
        bump(ps, error="unexpected `=`")
    elseif leading_kind == K"Identifier"
        # xx  ==>  xx
        # x₁  ==>  x₁
        bump(ps)
    elseif is_operator(leading_kind)
        if check_identifiers && is_syntactic_operator(leading_kind)
            # +=   ==>  (error +=)
            # .+=  ==>  (error .+=)
            bump(ps, error="invalid identifier")
        else
            # +     ==>  +
            # ~     ==>  ~
            # Quoted syntactic operators allowed
            # :+=   ==>  (quote +=)
            # :.=   ==>  (quote .=)
            # Remap the kind here to K"Identifier", as operators parsed in this
            # branch should be in "identifier-like" positions (I guess this is
            # correct? is it convenient?)
            bump(ps, remap_kind=K"Identifier")
        end
    elseif is_keyword(leading_kind)
        if leading_kind == K"var" && (t = peek_token(ps,2);
                                      kind(t) == K"\"" && !preceding_whitespace(t))
            # var"x"     ==> x
            # Raw mode unescaping
            # var""     ==>
            # var"\""   ==> "
            # var"\\""  ==> \"
            # var"\\x"  ==> \\x
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
                               error="Unterminated string literal")
            end
            t = peek_token(ps)
            k = kind(t)
            if preceding_whitespace(t) || is_operator(k) ||
                    k in KSet"( ) [ ] { } , ; @ EndMarker"
                # var"x"+  ==>  x
                # var"x")  ==>  x
                # var"x"(  ==>  x
            else
                # var"x"end  ==>  (error (end))
                # var"x"1    ==>  (error 1)
                # var"x"y    ==>  (error y)
                bump(ps, error="suffix not allowed after var\"...\" syntax")
            end
        elseif check_identifiers && is_closing_token(ps, leading_kind)
            # :(end)  ==>  (quote (error end))
            bump(ps, error="invalid identifier")
        else
            # Remap keywords to identifiers.
            # :end  ==>  (quote end)
            # :<:   ==> (quote <:)
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
        # @. x y  ==> (macrocall @__dot__ x y)
        bump(ps, TRIVIA_FLAG)
        parse_macro_name(ps)
        parse_call_chain(ps, mark, true)
    elseif is_string_delim(leading_kind)
        parse_string(ps, false)
    elseif leading_kind in KSet"` ```"
        # ``          ==>  (macrocall core_@cmd "")
        # `cmd`       ==>  (macrocall core_@cmd "cmd")
        # ```cmd```   ==>  (macrocall core_@cmd "cmd"-s)
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
              "unexpected closing token"
        bump_invisible(ps, K"error", error=msg)
    else
        bump(ps, error="invalid syntax atom")
    end
end
