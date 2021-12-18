#-------------------------------------------------------------------------------
"""
`SyntaxToken` covers a contiguous range of the source text which contains a
token *relevant for parsing*. Syntax trivia (comments and whitespace) is dealt
with separately, though `SyntaxToken` does include some minimal information
about whether these were present.

This does not include tokens include
* Whitespace
* Comments

Note that "triviality" of tokens is context-dependent in general. For example,
the parentheses in `(1+2)*3` are important for parsing but are irrelevant after
the abstract syntax tree is constructed.
"""
struct SyntaxToken
    raw::RawToken
    # Flags for leading whitespace
    had_whitespace::Bool
    had_newline::Bool
end

function Base.show(io::IO, tok::SyntaxToken)
    range = string(lpad(first_byte(tok), 3), ":", rpad(last_byte(tok), 3))
    print(io, rpad(range, 17, " "), rpad(kind(tok), 15, " "))
end

kind(tok::SyntaxToken) = tok.raw.kind
first_byte(tok::SyntaxToken) = tok.raw.startbyte + 1
last_byte(tok::SyntaxToken) = tok.raw.endbyte + 1
span(tok::SyntaxToken) = last_byte(tok) - first_byte(tok) + 1

is_dotted(tok::SyntaxToken)    = tok.raw.dotop
is_suffixed(tok::SyntaxToken)  = tok.raw.suffix
is_decorated(tok::SyntaxToken) = is_dotted(tok) || is_suffixed(tok)

Base.:(~)(tok::SyntaxToken, k::Kind) = kind(tok) == k
Base.:(~)(k::Kind, tok::SyntaxToken) = kind(tok) == k

#-------------------------------------------------------------------------------

# Range in the source text which will become a node in the tree. Can be either
# a token (leaf node of the tree) or an interior node, depending on how nodes
# overlap.
struct TaggedRange
    head::SyntaxHead
    first_byte::Int
    last_byte::Int
end

function TaggedRange(raw::RawToken, flags::RawFlags)
    TaggedRange(SyntaxHead(raw.kind, flags), raw.startbyte + 1, raw.endbyte + 1)
end

head(text_span::TaggedRange)       = text_span.head
kind(text_span::TaggedRange)       = kind(text_span.head)
flags(text_span::TaggedRange)      = flags(text_span.head)
first_byte(text_span::TaggedRange) = text_span.first_byte
last_byte(text_span::TaggedRange)  = text_span.last_byte
span(text_span::TaggedRange)       = last_byte(text_span) - first_byte(text_span) + 1

struct Diagnostic
    text_span::TaggedRange
    message::String
end

function show_diagnostic(io::IO, diagnostic::Diagnostic, code)
    printstyled(io, "Error: ", color=:light_red)
    print(io, diagnostic.message, ":\n")
    p = first_byte(diagnostic.text_span)
    q = last_byte(diagnostic.text_span)
    if !isvalid(code, q)
        # Transform byte range into valid text range
        q = prevind(code, q)
    end
    if q < p || (p == q && code[p] == '\n')
        # An empty or invisible range!  We expand it symmetrically to make it
        # visible.
        p = max(firstindex(code), prevind(code, p))
        q = min(lastindex(code), nextind(code, q))
    end
    print(io, code[1:prevind(code, p)])
    _printstyled(io, code[p:q]; color=(100,40,40))
    print(io, code[nextind(code, q):end], '\n')
end

struct ParseStreamPosition
    input_byte::Int    # Index of next byte in input
    output_index::Int  # Index of last span in output
end

const NO_POSITION = ParseStreamPosition(0,0)

#-------------------------------------------------------------------------------
"""
ParseStream provides an IO interface for the parser. It
- Wraps the lexer with a lookahead buffer
- Removes whitespace and comment tokens, shifting them into the output implicitly

This is simililar in spirit to rust-analyzer's
[TextTreeSink](https://github.com/rust-analyzer/rust-analyzer/blob/4691a0647b2c96cc475d8bbe7c31fe194d1443e7/crates/syntax/src/parsing/text_tree_sink.rs)
"""
mutable struct ParseStream
    lexer::Tokenize.Lexers.Lexer{IOBuffer,RawToken}
    lookahead::Vector{SyntaxToken}
    spans::Vector{TaggedRange}
    diagnostics::Vector{Diagnostic}
    # First byte of next token
    next_byte::Int
    # Counter for number of peek()s we've done without making progress via a bump()
    peek_count::Int
end

function ParseStream(code)
    lexer = Tokenize.tokenize(code, RawToken)
    ParseStream(lexer,
                Vector{SyntaxToken}(),
                Vector{TaggedRange}(),
                Vector{Diagnostic}(),
                1,
                0)
end

function Base.show(io::IO, mime::MIME"text/plain", stream::ParseStream)
    println(io, "ParseStream at position $(stream.next_byte)")
end

function show_diagnostics(io::IO, stream::ParseStream, code)
    for d in stream.diagnostics
        show_diagnostic(io, d, code)
    end
end

#-------------------------------------------------------------------------------
# Stream input interface - the peek_* family of functions

# Buffer up until the next non-whitespace token.
# This can buffer more than strictly necessary when newlines are significant,
# but this is not a big problem.
function _buffer_lookahead_tokens(stream::ParseStream)
    had_whitespace = false
    had_newline    = false
    while true
        raw = Tokenize.Lexers.next_token(stream.lexer)
        k = TzTokens.exactkind(raw)
        was_whitespace = k in (K"Whitespace", K"Comment", K"NewlineWs")
        was_newline    = k == K"NewlineWs"
        had_whitespace |= was_whitespace
        had_newline    |= was_newline
        push!(stream.lookahead, SyntaxToken(raw, had_whitespace, had_newline))
        if !was_whitespace
            break
        end
    end
end

# Find the index of the first nontrivia token in the lookahead buffer.
#
# TODO: Store this as part of _buffer_lookahead_tokens to avoid redoing this
# work all the time!
function _lookahead_index(stream::ParseStream, n::Integer, skip_newlines::Bool)
    i = 1
    while true
        if i > length(stream.lookahead)
            _buffer_lookahead_tokens(stream)
        end
        k = kind(stream.lookahead[i])
        is_skipped =  k ∈ (K"Whitespace", K"Comment") ||
                     (k == K"NewlineWs" && skip_newlines)
        if !is_skipped
            if n == 1
                return i
            end
            n -= 1
        end
        i += 1
    end
end

"""
    peek_token(stream [, n=1])

Look ahead in the stream `n` tokens, returning a SyntaxToken
"""
function peek_token(stream::ParseStream, n::Integer=1, skip_newlines=false)
    stream.peek_count += 1
    if stream.peek_count > 100_000
        error("The parser seems stuck at byte $(position(stream))")
    end
    stream.lookahead[_lookahead_index(stream, n, skip_newlines)]
end

"""
    peek_token(stream [, n=1])

Look ahead in the stream `n` tokens, returning a Kind
"""
function peek(stream::ParseStream, n::Integer=1, skip_newlines=false)
    kind(peek_token(stream, n, skip_newlines))
end

"""
Return true if the next token equals the string `str`

This is a hack (ideally the tokenizer would provide tokens for any
identifiers which need special treatment) But occasionally the parser needs
access to interpret normal identifiers as contextural keywords or other special
syntactic constructs.

For example, the special parsing rules for `@doc` line contination :-/
"""
function peek_equal_to(stream::ParseStream, str::String)
    t = peek_token(stream)
    if span(t) != ncodeunits(str)
        return false
    end
    # Humongous but should-be-allocation-free hack: peek at the underlying data
    # buffer. TODO: Attach the code string to the stream so we don't have to
    # dig into the lexer?
    buf = stream.lexer.io.data
    cbuf = codeunits(str)
    for i = 1:span(t)
        if buf[first_byte(t) + i - 1] != cbuf[i]
            return false
        end
    end
    return true
end

"""
Return the kind of the previous non-trivia span which was inserted.

Looking backward is a bit hacky but can be handy on occasion.
"""
function peek_behind(stream::ParseStream)
    for i = length(stream.spans):-1:1
        s = stream.spans[i]
        if !istrivia(head(s))
            return kind(s)
        end
    end
    return K"Nothing"
end

#-------------------------------------------------------------------------------
# Stream output interface - the `bump_*` and `emit_*` family of functions
#
# Though note bump() really does both input and output

# Bump the next `n` tokens
# flags and remap_kind are applied to any non-trivia tokens
function _bump_n(stream::ParseStream, n::Integer, flags, remap_kind=K"Nothing")
    if n <= 0
        return
    end
    for i=1:n
        tok = stream.lookahead[i]
        k = kind(tok)
        if k == K"EndMarker"
            break
        end
        is_trivia = k ∈ (K"Whitespace", K"Comment", K"NewlineWs")
        f = is_trivia ? TRIVIA_FLAG : flags
        k = (is_trivia || remap_kind == K"Nothing") ? k : remap_kind
        span = TaggedRange(SyntaxHead(k, f), first_byte(tok), last_byte(tok))
        push!(stream.spans, span)
    end
    Base._deletebeg!(stream.lookahead, n)
    stream.next_byte = last_byte(last(stream.spans)) + 1
    # Defuse the time bomb
    stream.peek_count = 0
end

"""
    bump(stream [, flags=EMPTY_FLAGS];
         skip_newlines=false, error, remap_kind)

Shift the current token from the input to the output, adding the given flags.
"""
function bump(stream::ParseStream, flags=EMPTY_FLAGS; skip_newlines=false,
              error=nothing, remap_kind=K"Nothing")
    emark = position(stream)
    _bump_n(stream, _lookahead_index(stream, 1, skip_newlines), flags, remap_kind)
    if !isnothing(error)
        emit(stream, emark, K"error", TRIVIA_FLAG, error=error)
    end
    # Return last token location in output if needed for reset_node!
    return position(stream)
end

"""
Bump comments and whitespace tokens preceding the next token

**Skips newlines** by default.  Set skip_newlines=false to avoid that.
"""
function bump_trivia(stream::ParseStream; skip_newlines=true, error=nothing)
    emark = position(stream)
    _bump_n(stream, _lookahead_index(stream, 1, skip_newlines) - 1, EMPTY_FLAGS)
    if !isnothing(error)
        emit(stream, emark, K"error", TRIVIA_FLAG, error=error)
    end
    return position(stream)
end

"""
Bump an invisible zero-width token into the output

This is useful when surrounding syntax implies the presence of a token.  For
example, `2x` means `2*x` via the juxtoposition rules.
"""
function bump_invisible(stream::ParseStream, kind, flags=EMPTY_FLAGS;
                        error=nothing)
    emit(stream, position(stream), kind, flags, error=error)
    return position(stream)
end

"""
Bump several tokens, gluing them together into a single token

This is for use in special circumstances where the parser needs to resolve
lexing ambiguities. There's no special whitespace handling — bump any
whitespace if necessary with bump_trivia.
"""
function bump_glue(stream::ParseStream, kind, flags, num_tokens)
    span = TaggedRange(SyntaxHead(kind, flags),
                       first_byte(stream.lookahead[1]),
                       last_byte(stream.lookahead[num_tokens]))
    Base._deletebeg!(stream.lookahead, num_tokens)
    push!(stream.spans, span)
    return position(stream)
end

"""
Bump a token, splitting it into two pieces.

Wow, this is a hack! It helps resolves the occasional lexing ambiguities. For
example whether .+ should be a single token or a composite (. +)
"""
function bump_split(stream::ParseStream, num_bytes, kind1, flags1, kind2, flags2)
    tok = popfirst!(stream.lookahead)
    push!(stream.spans, TaggedRange(SyntaxHead(kind1, flags1),
                                    first_byte(tok), first_byte(tok)+num_bytes-1))
    push!(stream.spans, TaggedRange(SyntaxHead(kind2, flags2),
                                    first_byte(tok)+num_bytes, last_byte(tok)))
    nothing # position(stream) is ambiguous here, as it involves two spans
end

"""
Reset kind or flags of an existing node in the output stream

This is a hack, but necessary on some occasions
* When some trailing syntax may change the kind or flags of the token
* When an invisible token might be required - see bump_invisible with K"TOMBSTONE"
"""
function reset_node!(stream::ParseStream, mark::ParseStreamPosition;
                     kind=nothing, flags=nothing)
    text_span = stream.spans[mark.output_index]
    k = isnothing(kind)  ? (@__MODULE__).kind(text_span)  : kind
    f = isnothing(flags) ? (@__MODULE__).flags(text_span) : flags
    stream.spans[mark.output_index] =
        TaggedRange(SyntaxHead(k, f), first_byte(text_span), last_byte(text_span))
end

function Base.position(stream::ParseStream)
    ParseStreamPosition(stream.next_byte, lastindex(stream.spans))
end

"""
    emit(stream, mark, kind, flags = EMPTY_FLAGS; error=nothing)

Emit a new text span into the output which covers source bytes from `mark` to
the end of the most recent token which was `bump()`'ed. The starting `mark`
should be a previous return value of `position()`.
"""
function emit(stream::ParseStream, mark::ParseStreamPosition, kind::Kind,
              flags::RawFlags = EMPTY_FLAGS; error=nothing)
    text_span = TaggedRange(SyntaxHead(kind, flags), mark.input_byte, stream.next_byte-1)
    if !isnothing(error)
        push!(stream.diagnostics, Diagnostic(text_span, error))
    end
    push!(stream.spans, text_span)
    return position(stream)
end

"""
Emit a diagnostic at the position of the next token

If `whitespace` is true, the diagnostic is positioned on the whitespace before
the next token. Otherwise it's positioned at the next token as returned by `peek()`.

FIXME: Rename? This doesn't emit normal tokens into the output event list!
"""
function emit_diagnostic(stream::ParseStream, mark=nothing, end_mark=nothing;
                         error, whitespace=false)
    i = _lookahead_index(stream, 1, true)
    begin_tok_i = i
    end_tok_i = i
    if whitespace
        # It's the whitespace which is the error. Find the range of the current
        # whitespace.
        begin_tok_i = 1
        end_tok_i = is_whitespace(stream.lookahead[i]) ? i : max(1, i-1)
    end
    first_byte = isnothing(mark) ?
        first_byte(stream.lookahead[begin_tok_i]) : mark.input_byte
    last_byte = isnothing(end_mark) ?
        last_byte(stream.lookahead[end_tok_i]) : end_mark.input_byte
    # It's a bit weird to require supplying a SyntaxHead here...
    text_span = TaggedRange(SyntaxHead(K"error", EMPTY_FLAGS), first_byte, last_byte)
    push!(stream.diagnostics, Diagnostic(text_span, error))
    return nothing
end

function emit_diagnostic(stream::ParseStream, r::NTuple{2,ParseStreamPosition}; kws...)
    emit_diagnostic(stream, first(r), last(r); kws...)
end

#-------------------------------------------------------------------------------
# Tree construction from the list of text spans held by ParseStream
#
# Note that this is largely independent of GreenNode, and could easily be
# made completely independent with a tree builder interface.

function _push_node!(stack, text_span::TaggedRange, children=nothing)
    if isnothing(children)
        node = GreenNode(head(text_span), span(text_span))
        push!(stack, (text_span=text_span, node=node))
    else
        node = GreenNode(head(text_span), span(text_span), children)
        push!(stack, (text_span=text_span, node=node))
    end
end

function to_raw_tree(st; wrap_toplevel_as_kind=nothing)
    stack = Vector{@NamedTuple{text_span::TaggedRange,node::GreenNode}}()
    for text_span in st.spans
        if kind(text_span) == K"TOMBSTONE"
            # Ignore invisible tokens which were created but never finalized.
            # See bump_invisible()
            continue
        end

        if isempty(stack) || first_byte(text_span) > last_byte(stack[end].text_span)
            # A leaf node (span covering a single token):
            # [a][b][stack[end]]
            #                   [text_span]
            _push_node!(stack, text_span)
            continue
        end
        # An interior node, span covering multiple tokens:
        #
        # [a][b][stack[end]]
        #    [    text_span]
        j = length(stack)
        while j > 1 && first_byte(text_span) <= first_byte(stack[j-1].text_span)
            j -= 1
        end
        children = [stack[k].node for k = j:length(stack)]
        resize!(stack, j-1)
        _push_node!(stack, text_span, children)
    end
    # show(stdout, MIME"text/plain"(), stack[1].node)
    if length(stack) == 1
        return only(stack).node
    elseif !isnothing(wrap_toplevel_as_kind)
        # Mostly for debugging
        children = [x.node for x in stack]
        return GreenNode(SyntaxHead(wrap_toplevel_as_kind, EMPTY_FLAGS), children...)
    else
        error("Found multiple nodes at top level")
    end
end

#-------------------------------------------------------------------------------
"""
ParseState carries parser context as we recursively descend into the parse
tree. For example, normally `x -y` means `(x) - (y)`, but when parsing matrix
literals we're in `space_sensitive` mode, and `[x -y]` means [(x) (-y)].
"""
struct ParseState
    stream::ParseStream
    # Vesion of Julia we're parsing this code for. May be different from VERSION!
    julia_version::VersionNumber

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
function ParseState(stream::ParseStream; julia_version=VERSION)
    ParseState(stream, julia_version, true, false, true, false, false, false)
end

function ParseState(ps::ParseState; range_colon_enabled=nothing,
                    space_sensitive=nothing, for_generator=nothing,
                    end_symbol=nothing, whitespace_newline=nothing,
                    where_enabled=nothing)
    ParseState(ps.stream, ps.julia_version,
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
               where_enabled=false,
               for_generator=false,
               end_symbol=false,
               whitespace_newline=false)
end

function with_space_sensitive(f::Function, ps::ParseState)
    f(ParseState(ps,
                 space_sensitive=true,
                 whitespace_newline=false))
end

# Convenient wrappers for ParseStream

function peek(ps::ParseState, n=1; skip_newlines=nothing)
    skip_nl = isnothing(skip_newlines) ? ps.whitespace_newline : skip_newlines
    peek(ps.stream, n, skip_nl)
end

function peek_token(ps::ParseState, n=1; skip_newlines=nothing)
    skip_nl = isnothing(skip_newlines) ? ps.whitespace_newline : skip_newlines
    peek_token(ps.stream, n, skip_nl)
end

function peek_equal_to(ps::ParseState, args...)
    peek_equal_to(ps.stream, args...)
end

function peek_behind(ps::ParseState)
    peek_behind(ps.stream)
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

function Base.position(ps::ParseState, args...)
    position(ps.stream, args...)
end

function emit(ps::ParseState, args...; kws...)
    emit(ps.stream, args...; kws...)
end

function emit_diagnostic(ps::ParseState, args...; kws...)
    emit_diagnostic(ps.stream, args...; kws...)
end

