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

Base.:(~)(tok::SyntaxToken, k::Kind) = kind(tok) == k
Base.:(~)(k::Kind, tok::SyntaxToken) = kind(tok) == k

#-------------------------------------------------------------------------------

struct TextSpan
    head::SyntaxHead
    first_byte::Int
    last_byte::Int
end

function TextSpan(raw::RawToken, flags::RawFlags)
    TextSpan(SyntaxHead(raw.kind, flags), raw.startbyte + 1, raw.endbyte + 1)
end

head(text_span::TextSpan)       = text_span.head
kind(text_span::TextSpan)       = kind(text_span.head)
flags(text_span::TextSpan)      = flags(text_span.head)
first_byte(text_span::TextSpan) = text_span.first_byte
last_byte(text_span::TextSpan)  = text_span.last_byte
span(text_span::TextSpan)       = last_byte(text_span) - first_byte(text_span) + 1

struct Diagnostic
    text_span::TextSpan
    message::String
end

function show_diagnostic(io::IO, diagnostic::Diagnostic, code)
    printstyled(io, "Error: ", color=:light_red)
    print(io, diagnostic.message, ":\n")
    p = first_byte(diagnostic.text_span)
    q = last_byte(diagnostic.text_span)
    print(io, code[1:p-1])
    _printstyled(io, code[p:q]; color=(100,40,40))
    print(io, code[q+1:end], '\n')
end

#-------------------------------------------------------------------------------
"""
ParseStream provides an IO interface for the parser. It
- Wraps the lexer from Tokenize.jl with a short lookahead buffer
- Removes whitespace and comment tokens, shifting them into the output implicitly

This is simililar to rust-analyzer's
[TextTreeSink](https://github.com/rust-analyzer/rust-analyzer/blob/4691a0647b2c96cc475d8bbe7c31fe194d1443e7/crates/syntax/src/parsing/text_tree_sink.rs)
"""
mutable struct ParseStream
    lexer::Tokenize.Lexers.Lexer{IOBuffer,RawToken}
    lookahead::Vector{SyntaxToken}
    lookahead_trivia::Vector{TextSpan}
    spans::Vector{TextSpan}
    diagnostics::Vector{Diagnostic}
    # First byte of next token
    next_byte::Int
end

function ParseStream(code)
    lexer = Tokenize.tokenize(code, RawToken)
    ParseStream(lexer,
                Vector{SyntaxToken}(),
                Vector{TextSpan}(),
                Vector{TextSpan}(),
                Vector{Diagnostic}(),
                1)
end

function Base.show(io::IO, mime::MIME"text/plain", stream::ParseStream)
    println(io, "ParseStream at position $(stream.next_byte)")
end

function _read_token(stream::ParseStream)
    had_whitespace = false
    had_newline = false
    while true
        raw = Tokenize.Lexers.next_token(stream.lexer)
        k = TzTokens.exactkind(raw)
        if k in (K"Whitespace", K"Comment", K"NewlineWs")
            had_whitespace = true
            had_newline = k == K"NewlineWs"
            push!(stream.lookahead_trivia, TextSpan(raw, TRIVIA_FLAG))
            continue
        end
        return SyntaxToken(raw, had_whitespace, had_newline)
    end
end

"""
    peek_token(stream [, n=1])

Look ahead in the stream `n` tokens, returning a SyntaxToken
"""
function peek_token(stream::ParseStream, n::Integer=1)
    if length(stream.lookahead) < n
        for i=1:(n-length(stream.lookahead))
            push!(stream.lookahead, _read_token(stream))
        end
    end
    return stream.lookahead[n]
end

"""
    peek_token(stream [, n=1])

Look ahead in the stream `n` tokens, returning a Kind
"""
function peek(stream::ParseStream, n::Integer=1)
    kind(peek_token(stream, n))
end

"""
    bump(stream [, flags=EMPTY_FLAGS])

Shift the current token into the output as a new text span with the given
`flags`.
"""
function bump(stream::ParseStream, flags=EMPTY_FLAGS)
    tok = isempty(stream.lookahead) ?
          _read_token(stream) :
          popfirst!(stream.lookahead)  # TODO: use a circular buffer?
    # Bump trivia tokens into output
    while !isempty(stream.lookahead_trivia) &&
            first_byte(first(stream.lookahead_trivia)) <= first_byte(tok)
        trivia_span = popfirst!(stream.lookahead_trivia)
        push!(stream.spans, trivia_span)
    end
    span = TextSpan(SyntaxHead(kind(tok), flags), first_byte(tok), last_byte(tok))
    push!(stream.spans, span)
    stream.next_byte = last_byte(tok) + 1
    nothing
end

function Base.position(stream::ParseStream)
    return stream.next_byte
end

"""
    emit(stream, start_position, kind [, flags = EMPTY_FLAGS])

Emit a new text span into the output which covers source bytes from
`start_position` to the end of the most recent token which was `bump()`'ed.
The `start_position` of the span should be a previous return value of
`position()`.
"""
function emit(stream::ParseStream, start_position::Integer, kind::Kind,
              flags::RawFlags = EMPTY_FLAGS; error=nothing)
    if !isnothing(error)
        flags |= ERROR_FLAG
    end
    text_span = TextSpan(SyntaxHead(kind, flags), start_position, stream.next_byte-1)
    if !isnothing(error)
        push!(stream.diagnostics, Diagnostic(text_span, error))
    end
    push!(stream.spans, text_span)
    return nothing
end


# Tree construction from the list of text spans held by ParseStream
#
# Note that this is largely independent of GreenNode, and could easily be
# made completely independent with a tree builder interface.

function _push_node!(stack, text_span::TextSpan, children=nothing)
    if isnothing(children)
        node = GreenNode(head(text_span), span(text_span))
        push!(stack, (text_span=text_span, node=node))
    else
        node = GreenNode(head(text_span), span(text_span), children)
        push!(stack, (text_span=text_span, node=node))
    end
end

function to_raw_tree(st)
    stack = Vector{@NamedTuple{text_span::TextSpan,node::GreenNode}}()
    _push_node!(stack, st.spans[1])
    for i = 2:length(st.spans)
        text_span = st.spans[i]

        if first_byte(text_span) > last_byte(stack[end].text_span)
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
        while j > 1 && first_byte(text_span) < first_byte(stack[j].text_span)
            j -= 1
        end
        children = [stack[k].node for k = j:length(stack)]
        resize!(stack, j-1)
        _push_node!(stack, text_span, children)
    end
    return only(stack).node
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

peek(ps::ParseState, args...) = peek(ps.stream, args...)
bump(ps::ParseState, args...) = bump(ps.stream, args...)
emit(ps::ParseState, args...) = emit(ps.stream, args...)
