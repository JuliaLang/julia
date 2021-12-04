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
    range = string(lpad(start_byte(tok), 3), ":", rpad(end_byte(tok), 3))
    print(io, rpad(range, 17, " "), rpad(kind(tok), 15, " "))
end

kind(tok::SyntaxToken) = tok.raw.kind
start_byte(tok::SyntaxToken) = tok.raw.startbyte + 1
end_byte(tok::SyntaxToken) = tok.raw.endbyte + 1
span(tok::SyntaxToken) = end_byte(tok) - start_byte(tok) + 1


#-------------------------------------------------------------------------------

"""
ParseStream provides an IO interface for the parser. It
- Wraps the lexer from Tokenize.jl with a short lookahead buffer
- Removes whitespace and comment tokens, shifting them into the output implicitly
- Provides a begin_node/end_node interface to emit the parsed tree structure

This is simililar to rust-analyzer's
[TextTreeSink](https://github.com/rust-analyzer/rust-analyzer/blob/4691a0647b2c96cc475d8bbe7c31fe194d1443e7/crates/syntax/src/parsing/text_tree_sink.rs)
"""
mutable struct ParseStream
    lexer::Tokenize.Lexers.Lexer{IOBuffer,RawToken}
    lookahead::Vector{SyntaxToken}
    trivia_buf::Vector{SyntaxToken}
    current_end_byte::Int  # Byte index of the last *consumed* token
    pending_node_stack::Vector{Tuple{Vector{RawSyntaxNode},Int}}
end

function ParseStream(code)
    lexer = Tokenize.tokenize(code, RawToken)
    ParseStream(lexer, SyntaxToken[], SyntaxToken[], 0, Vector{RawSyntaxNode}[])
end

function Base.show(io::IO, mime::MIME"text/plain", stream::ParseStream)
    print(io, ParseStream, ":\n  lexer = ")
    show(io, mime, stream.lexer)
end

# Iterator interface
#=
Base.IteratorSize(::Type{ParseStream}) = Base.SizeUnknown()
Base.IteratorEltype(::Type{ParseStream}) = Base.HasEltype()
Base.eltype(::Type{ParseStream}) = SyntaxToken

function Base.iterate(stream::ParseStream, end_state=false)
    end_state && return nothing
    t = peek(stream)
    bump!()
    return t, kind(t) == K"EndMarker"
end
=#

# Read one nontrivia token; shift trivia into stream.trivia_buf
function _read_token(stream::ParseStream)
    had_whitespace = false
    had_newline = false
    while true
        raw = Tokenize.Lexers.next_token(stream.lexer)
        k = TzTokens.exactkind(raw)
        if k in (K"Whitespace", K"Comment", K"NewlineWs")
            had_whitespace = true
            had_newline = k == K"NewlineWs"
            push!(stream.trivia_buf, SyntaxToken(raw, false, false))
            continue
        end
        return SyntaxToken(raw, had_whitespace, had_newline)
    end
end

"""
    peek(stream [, n=1])

Look ahead in the stream `n` tokens.
"""
function peek(stream::ParseStream, n::Integer=1)
    if length(stream.lookahead) < n
        for i=1:(n-length(stream.lookahead))
            push!(stream.lookahead, _read_token(stream))
        end
    end
    return stream.lookahead[n]
end

function _current_node_children(stream::ParseStream)
    last(stream.pending_node_stack)[1]
end

"""
    bump!(stream)

Remove next token from the stream and add it as a syntax leaf to the current
output node.

`bump!` returns `nothing` to make synchronization with the output stream
clearer. To see token values use `peek()`.
"""
function bump!(stream::ParseStream, flags::_RawFlags=EMPTY_FLAGS)
    if isempty(stream.pending_node_stack)
        error("Cannot bump! stream outside begin_node-end_node pair because this would loose input tokens")
    end
    tok = isempty(stream.lookahead) ?
          _read_token(stream) :
          popfirst!(stream.lookahead)  # TODO: use a circular buffer?
    while true
        if #==# isempty(stream.trivia_buf) ||
                start_byte(first(stream.trivia_buf)) > start_byte(tok)
            break
        end
        t = popfirst!(stream.trivia_buf)
        trivia_node = RawSyntaxNode(kind(t), span(t), TRIVIA_FLAG)
        push!(_current_node_children(stream), trivia_node)
    end
    node = RawSyntaxNode(kind(tok), span(tok), flags)
    push!(_current_node_children(stream), node)
    stream.current_end_byte = end_byte(tok)
    nothing
end


#-------------------------------------------------------------------------------
# ParseStream tree output interface

function begin_node(stream::ParseStream)
    # TODO: Add a trivia heuristic here and in end_node so that whitespace and
    # comments attach to nodes more usefully. May need some hint from the
    # parser (eg, designating nodes which tend to be "block" vs "inline"?) for
    # this to work well.
    node_begin_byte = stream.current_end_byte + 1
    push!(stream.pending_node_stack, (RawSyntaxNode[], node_begin_byte))
    nothing
end

function end_node(stream::ParseStream, k::Kind, flags::_RawFlags=EMPTY_FLAGS)
    children, node_begin_byte = pop!(stream.pending_node_stack)
    span = stream.current_end_byte - node_begin_byte + 1
    node = RawSyntaxNode(k, span, flags, children)
    if !isempty(stream.pending_node_stack)
        push!(last(stream.pending_node_stack)[1], node)
    end
    return node
end
