#-------------------------------------------------------------------------------
# Flags hold auxiliary information about tokens/nonterminals which the Kind
# doesn't capture in a nice way.
#
# TODO: Use `primitive type SyntaxFlags 16 end` rather than an alias?
const RawFlags = UInt16
const EMPTY_FLAGS = RawFlags(0)

# Set for tokens or ranges which are syntax trivia after parsing
const TRIVIA_FLAG = RawFlags(1<<0)

"""
Set for nodes that are non-terminals
"""
const NON_TERMINAL_FLAG = RawFlags(1<<7)

function remove_flags(n::RawFlags, fs...)
    RawFlags(n & ~(RawFlags((|)(fs...))))
end

"""
    has_flags(x, test_flags)

Return true if any of `test_flags` are set.
"""
has_flags(flags::RawFlags, test_flags) = (flags & test_flags) != 0

#-------------------------------------------------------------------------------
"""
    SyntaxHead(kind, flags)

A `SyntaxHead` combines the [`Kind`](@ref) of a syntactic construct with a set
of flags. The kind defines the broad "type" of the syntactic construct, while
the flag bits compactly store more detailed information about the construct.
"""
struct SyntaxHead
    kind::Kind
    flags::RawFlags
end

kind(head::SyntaxHead) = head.kind

"""
    flags(x)

Return the flag bits of a syntactic construct. Prefer to query these with the
predicates `is_trivia`, `is_prefix_call`, `is_infix_op_call`,
`is_prefix_op_call`, `is_postfix_op_call`, `is_dotted`, `is_suffixed`,
`is_decorated`.

Or extract numeric portion of the flags with `numeric_flags`.
"""
flags(head::SyntaxHead) = head.flags

function Base.summary(head::SyntaxHead)
    untokenize(head, unique=false, include_flag_suff=false)
end

#-------------------------------------------------------------------------------
# Generic interface for types `T` which have kind and flags. Either:
# 1. Define kind(::T) and flags(::T), or
# 2. Define head(::T) to return a type like `SyntaxKind` for which `kind` and
#    `flags` are defined
kind(x)  = kind(head(x))
flags(x) = flags(head(x))

# Predicates based on flags()
has_flags(x, test_flags) = has_flags(flags(x), test_flags)
call_type_flags(x) = call_type_flags(flags(x))

"""
    is_trivia(x)

Return true for "syntax trivia": tokens in the tree which are either largely
invisible to the parser (eg, whitespace) or implied by the structure of the AST
(eg, reserved words).
"""
is_trivia(x) = has_flags(x, TRIVIA_FLAG)

#-------------------------------------------------------------------------------
"""
`SyntaxToken` is a token covering a contiguous byte range in the input text.

We record only the `next_byte` here (the index of the next byte *after* the
token) to avoid duplication of data between neighbouring tokens. This is more
useful than recording the first byte, as it allows an initial fixed sentinel
token to be used for recording the first byte of the first real token.
"""
struct SyntaxToken
    head::SyntaxHead
    orig_kind::Kind
    preceding_whitespace::Bool
    next_byte::UInt32
end

function Base.show(io::IO, tok::SyntaxToken)
    print(io, rpad(untokenize(tok.head, unique=false), 15), " |", tok.next_byte)
end

head(tok::SyntaxToken) = tok.head
preceding_whitespace(tok::SyntaxToken) = tok.preceding_whitespace


#-------------------------------------------------------------------------------

"""
    RawGreenNode(head::SyntaxHead, byte_span::UInt32, orig_kind::Kind) # Terminal
    RawGreenNode(head::SyntaxHead, byte_span::UInt32, nchildren::UInt32) # Non-terminal

A "green tree" is a lossless syntax tree which overlays all the source text.
The most basic properties of a green tree are that:

* Nodes cover a contiguous span of bytes in the text
* Sibling nodes are ordered in the same order as the text

As implementation choices, we choose that:

* Nodes are immutable and don't know their parents or absolute position, so can
  be cached and reused
* Nodes are homogeneously typed at the language level so they can be stored
  concretely, with the `head` defining the node type. Normally this would
  include a "syntax kind" enumeration, but it can also include flags and record
  information the parser knew about the layout of the child nodes.
* For simplicity and uniformity, leaf nodes cover a single token in the source.
  This is like rust-analyzer, but different from Roslyn where leaves can
  include syntax trivia.
* The parser produces a single buffer of `RawGreenNode` which encodes the tree.
  There are higher level accessors, which make working with this tree easier.
"""
struct RawGreenNode
    head::SyntaxHead                  # Kind,flags
    byte_span::UInt32                 # Number of bytes covered by this range
    # If NON_TERMINAL_FLAG is set, this is the total number of child nodes
    # Otherwise this is a terminal node (i.e. a token) and this is orig_kind
    node_span_or_orig_kind::UInt32

    # Constructor for terminal nodes (tokens)
    function RawGreenNode(head::SyntaxHead, byte_span::Integer, orig_kind::Kind)
        @assert (flags(head) & NON_TERMINAL_FLAG) == 0
        new(head, UInt32(byte_span), UInt32(reinterpret(UInt16, orig_kind)))
    end

    # Constructor for non-terminal nodes - automatically sets NON_TERMINAL_FLAG
    function RawGreenNode(head::SyntaxHead, byte_span::Integer, node_span::Integer)
        h = SyntaxHead(kind(head), flags(head) | NON_TERMINAL_FLAG)
        new(h, UInt32(byte_span), UInt32(node_span))
    end

    global reset_node
    function reset_node(node::RawGreenNode, kind, flags)
        new(_reset_node_head(node, kind, flags),
            getfield(node, :byte_span),
            getfield(node, :node_span_or_orig_kind))
    end
end

function _reset_node_head(node, k, f)
    if !isnothing(f)
        f = RawFlags(f)
        @assert (f & NON_TERMINAL_FLAG) == 0
        f |= flags(node) & NON_TERMINAL_FLAG
    else
        f = flags(node)
    end
    h = SyntaxHead(isnothing(k) ? kind(node) : k, f)
end

Base.summary(node::RawGreenNode) = summary(node.head)
function Base.show(io::IO, node::RawGreenNode)
    print(io, summary(node), " (", node.byte_span, " bytes,")
    if is_terminal(node)
        print(io, " orig_kind=", node.orig_kind, ")")
    else
        print(io, " ", node.node_span, " children)")
    end
end

function Base.getproperty(rgn::RawGreenNode, name::Symbol)
    if name === :node_span
        has_flags(getfield(rgn, :head), NON_TERMINAL_FLAG) || return UInt32(0) # Leaf nodes have no children
        return getfield(rgn, :node_span_or_orig_kind)
    elseif name === :orig_kind
        has_flags(getfield(rgn, :head), NON_TERMINAL_FLAG) && error("Cannot access orig_kind for non-terminal node")
        return Kind(getfield(rgn, :node_span_or_orig_kind))
    end
    getfield(rgn, name)
end

head(range::RawGreenNode) = range.head

# Helper functions for unified output
is_terminal(node::RawGreenNode) = !has_flags(node.head, NON_TERMINAL_FLAG)
is_non_terminal(node::RawGreenNode) = has_flags(node.head, NON_TERMINAL_FLAG)

#-------------------------------------------------------------------------------
struct ParseStreamPosition
    """
    The current position in the byte stream, i.e. the byte at `byte_index` is
    the first byte of the next token to be parsed.
    """
    byte_index::UInt32
    """
    The total number of nodes (terminal + non-terminal) in the output so far.
    """
    node_index::UInt32
end

const NO_POSITION = ParseStreamPosition(0, 0)

#-------------------------------------------------------------------------------
"""
    ParseStream(text::AbstractString,          index::Integer=1; version=VERSION)
    ParseStream(text::IO;                                        version=VERSION)
    ParseStream(text::Vector{UInt8},           index::Integer=1; version=VERSION)
    ParseStream(ptr::Ptr{UInt8}, len::Integer, index::Integer=1; version=VERSION)

Construct a `ParseStream` from input which may come in various forms:
* An string (zero copy for `String` and `SubString`)
* An `IO` object (zero copy for `IOBuffer`). The `IO` object must be seekable.
* A buffer of bytes (zero copy). The caller is responsible for preserving
  buffers passed as `(ptr,len)`.

A byte `index` may be provided as the position to start parsing.

ParseStream provides an IO interface for the parser which provides lexing of
the source text input into tokens, manages insignificant whitespace tokens on
behalf of the parser, and stores output tokens and tree nodes in a pair of
output arrays.

`version` (default `VERSION`) may be used to set the syntax version to
any Julia version `>= v"1.0"`. We aim to parse all Julia syntax which has been
added after v"1.0", emitting an error if it's not compatible with the requested
`version`.
"""
mutable struct ParseStream
    # `textbuf` is a buffer of UTF-8 encoded text of the source code. This is a
    # natural representation as we desire random access and zero-copy parsing
    # of UTF-8 text from various containers, and unsafe_wrap(Vector{UInt8},
    # ...) allows us to use a Vector here.
    #
    # We want `ParseStream` to be concrete so that all `parse_*` functions only
    # need to be compiled once. Thus `textbuf` must not be parameterized here.
    textbuf::Vector{UInt8}
    # GC root for the object which owns the memory in `textbuf`. `nothing` if
    # the `textbuf` owner was unknown (eg, ptr,length was passed)
    text_root::Any
    # Lexer, transforming the input bytes into a token stream
    lexer::Tokenize.Lexer{IOBuffer}
    # Lookahead buffer for already lexed tokens
    lookahead::Vector{SyntaxToken}
    lookahead_index::Int
    # Pool of stream positions for use as working space in parsing
    position_pool::Vector{Vector{ParseStreamPosition}}
    output::Vector{RawGreenNode}
    # Current byte position in the output (the next byte to be written)
    next_byte::Int
    # Parsing diagnostics (errors/warnings etc)
    diagnostics::Vector{Diagnostic}
    # Counter for number of peek()s we've done without making progress via a bump()
    peek_count::Int
    # (major,minor) version of Julia we're parsing this code for.
    # May be different from VERSION!
    version::Tuple{Int,Int}

    function ParseStream(text_buf::Vector{UInt8}, text_root, next_byte::Integer,
                         version::VersionNumber)
        io = IOBuffer(text_buf)
        seek(io, next_byte-1)
        lexer = Tokenize.Lexer(io)
        # To avoid keeping track of the exact Julia development version where new
        # features were added or comparing prerelease strings, we treat prereleases
        # or dev versions as the release version using only major and minor version
        # numbers. This means we're inexact for old dev versions but that seems
        # like an acceptable tradeoff.
        ver = (version.major, version.minor)
        # Initial sentinel node (covering all ignored bytes before the first token)
        sentinel = RawGreenNode(SyntaxHead(K"TOMBSTONE", EMPTY_FLAGS), next_byte-1, K"TOMBSTONE")
        new(text_buf,
            text_root,
            lexer,
            Vector{SyntaxToken}(),
            1,
            Vector{Vector{ParseStreamPosition}}(),
            RawGreenNode[sentinel],
            next_byte,  # Initialize next_byte from the parameter
            Vector{Diagnostic}(),
            0,
            ver)
    end
end

function ParseStream(text::Vector{UInt8}, index::Integer=1; version=VERSION)
    ParseStream(text, text, index, version)
end

# Buffer with unknown owner. Not exactly recommended, but good for C interop
function ParseStream(ptr::Ptr{UInt8}, len::Integer, index::Integer=1; version=VERSION)
    ParseStream(unsafe_wrap(Vector{UInt8}, ptr, len), nothing, index, version)
end

# Buffers originating from strings
function ParseStream(text::String, index::Integer=1; version=VERSION)
    ParseStream(unsafe_wrap(Vector{UInt8}, text),
                text, index, version)
end
function ParseStream(text::SubString{String}, index::Integer=1; version=VERSION)
    # See also IOBuffer(SubString("x"))
    ParseStream(unsafe_wrap(Vector{UInt8}, pointer(text), sizeof(text)),
                text, index, version)
end
function ParseStream(text::AbstractString, index::Integer=1; version=VERSION)
    ParseStream(String(text), index; version=version)
end

# IO-based cases
# TODO: switch ParseStream to use a Memory internally on newer versions of Julia
VERSION < v"1.11.0-DEV.753" && function ParseStream(io::IOBuffer; version=VERSION)
    ParseStream(io.data, io, position(io)+1, version)
end
function ParseStream(io::Base.GenericIOBuffer; version=VERSION)
    textbuf = unsafe_wrap(Vector{UInt8}, pointer(io.data), length(io.data))
    ParseStream(textbuf, io, position(io)+1, version)
end
function ParseStream(io::IO; version=VERSION)
    textbuf = read(io)
    ParseStream(textbuf, textbuf, 1, version)
end

function Base.show(io::IO, mime::MIME"text/plain", stream::ParseStream)
    println(io, "ParseStream at position $(stream.next_byte)")
end

function show_diagnostics(io::IO, stream::ParseStream)
    show_diagnostics(io, stream.diagnostics, SourceFile(stream))
end

# We manage a pool of stream positions as parser working space
function acquire_positions(stream)
    if isempty(stream.position_pool)
        return Vector{ParseStreamPosition}()
    end
    pop!(stream.position_pool)
end

function release_positions(stream, positions)
    empty!(positions)
    push!(stream.position_pool, positions)
end

#-------------------------------------------------------------------------------
# Return true when a terminal (token) was emitted last at stream position `pos`
function token_is_last(stream, pos)
    # In the unified structure, check if the node at pos is a terminal
    return pos.node_index > 0 && pos.node_index <= length(stream.output) &&
           is_terminal(stream.output[pos.node_index])
end

function lookahead_token_first_byte(stream, i)
    i == 1 ? _next_byte(stream) : stream.lookahead[i-1].next_byte
end

function lookahead_token_last_byte(stream, i)
    stream.lookahead[i].next_byte - 1
end

#-------------------------------------------------------------------------------
# Stream input interface - the peek_* family of functions

# Buffer several tokens ahead
function _buffer_lookahead_tokens(lexer, lookahead)
    had_whitespace = false
    token_count = 0
    while true
        raw = Tokenize.next_token(lexer)
        k = kind(raw)
        was_whitespace = is_whitespace(k)
        had_whitespace |= was_whitespace
        f = EMPTY_FLAGS
        raw.suffix     && (f |= SUFFIXED_FLAG)
        push!(lookahead, SyntaxToken(SyntaxHead(k, f), k,
                                     had_whitespace, raw.endbyte + 2))
        token_count += 1
        if k == K"EndMarker"
            break
        end
        if !was_whitespace
            # Buffer tokens in batches for lookahead. Generally we want a
            # moderate-size buffer to make sure we hit the fast path of peek(),
            # but not too large to avoid (a) polluting the processor cache and
            # (b) doing unnecessary work when not parsing the whole input.
            had_whitespace = false
            if token_count > 100
                break
            end
        end
    end
end

# Return the index of the next byte of the input
function _next_byte(stream)
    stream.next_byte
end

# Find the index of the next nontrivia token
@inline function _lookahead_index(stream::ParseStream, n::Integer, skip_newlines::Bool)
    # Much of the time we'll be peeking ahead a single token and have one or
    # zero whitespace tokens before the next token. The following code is an
    # unrolled optimized version for that fast path. Empirically it seems we
    # only hit the slow path about 5% of the time here.
    i = stream.lookahead_index
    @inbounds if n == 1 && i+2 <= length(stream.lookahead)
        if skip_newlines
            k = kind(stream.lookahead[i])
            if !(k == K"Whitespace" || k == K"Comment" || k == K"NewlineWs")
                return i
            end
            i += 1
            k = kind(stream.lookahead[i])
            if !(k == K"Whitespace" || k == K"Comment" || k == K"NewlineWs")
                return i
            end
        else
            k = kind(stream.lookahead[i])
            if !(k == K"Whitespace" || k == K"Comment")
                return i
            end
            i += 1
            k = kind(stream.lookahead[i])
            if !(k == K"Whitespace" || k == K"Comment")
                return i
            end
        end
    end
    # Fall through to the general case
    return __lookahead_index(stream, n, skip_newlines)
end

@noinline function __lookahead_index(stream, n, skip_newlines)
    i = stream.lookahead_index
    while true
        if i+1 > length(stream.lookahead)
            n_to_delete = stream.lookahead_index-1
            if n_to_delete > 0.9*length(stream.lookahead)
                Base._deletebeg!(stream.lookahead, n_to_delete)
                i -= n_to_delete
                stream.lookahead_index = 1
            end
            _buffer_lookahead_tokens(stream.lexer, stream.lookahead)
            continue
        end
        k = @inbounds kind(stream.lookahead[i])
        if !((k == K"Whitespace" || k == K"Comment") ||
             (k == K"NewlineWs" && skip_newlines))
            if n == 1
                return i
            end
            n -= 1
        end
        i += 1
    end
end

@noinline function _parser_stuck_error(stream)
    # Optimization: emit unlikely errors in a separate function
    error("The parser seems stuck at byte $(stream.next_byte)")
end

"""
    peek(stream::ParseStream [, n=1]; skip_newlines=false)

Look ahead in the stream `n` tokens, returning the token kind. Comments and
non-newline whitespace are skipped automatically. Whitespace containing a
single newline is returned as kind `K"NewlineWs"` unless `skip_newlines` is
true.
"""
function Base.peek(stream::ParseStream, n::Integer=1;
                   skip_newlines::Bool=false, skip_whitespace=true)
    kind(peek_token(stream, n; skip_newlines=skip_newlines, skip_whitespace=skip_whitespace))
end

"""
    peek_token(stream [, n=1])

Like `peek`, but return the full token information rather than just the kind.
"""
function peek_token(stream::ParseStream, n::Integer=1;
                    skip_newlines=false, skip_whitespace=true)
    stream.peek_count += 1
    if stream.peek_count > 100_000
        _parser_stuck_error(stream)
    end
    i = _lookahead_index(stream, n, skip_newlines)
    if !skip_whitespace
        i = stream.lookahead_index
    end
    return @inbounds stream.lookahead[i]
end


struct FullToken
    head::SyntaxHead
    first_byte::UInt32
    last_byte::UInt32
end

head(t::FullToken) = t.head
byte_range(t::FullToken) = t.first_byte:t.last_byte
span(t::FullToken) = 1 + last_byte(t) - first_byte(t)

function peek_full_token(stream::ParseStream, n::Integer=1;
                         skip_newlines=false, skip_whitespace=true)
    stream.peek_count += 1
    if stream.peek_count > 100_000
        _parser_stuck_error(stream)
    end
    i = _lookahead_index(stream, n, skip_newlines)
    if !skip_whitespace
        i = stream.lookahead_index
    end
    t = stream.lookahead[i]

    FullToken(head(t), lookahead_token_first_byte(stream, i),
              lookahead_token_last_byte(stream, i))
end

"""
    peek_behind(ps; skip_trivia=true, skip_parens=true)
    peek_behind(ps, pos::ParseStreamPosition)

Return information about a span which was previously inserted into the output,
defaulting to the most previous nontrivia node when `skip_trivia` is true, or
at the provided position `pos`.

Retroactively inspecting or modifying the parser's output can be confusing, so
using this function should be avoided where possible.
"""
function peek_behind(stream::ParseStream, pos::ParseStreamPosition)
    if pos.node_index > 0 && pos.node_index <= length(stream.output)
        node = stream.output[pos.node_index]
        if is_terminal(node)
            return (kind=kind(node),
                    flags=flags(node),
                    orig_kind=node.orig_kind,
                    is_leaf=true)
        else
            return (kind=kind(node),
                    flags=flags(node),
                    orig_kind=K"None",
                    is_leaf=false)
        end
    else
        return (kind=K"None",
                flags=EMPTY_FLAGS,
                orig_kind=K"None",
                is_leaf=true)
    end
end

"""
    first_child_position(stream::ParseStream, pos::ParseStreamPosition)

Find the first non-trivia child of this node (in the GreenTree/RedTree sense) and return
its position.
"""
function first_child_position(stream::ParseStream, pos::ParseStreamPosition)
    output = stream.output
    @assert pos.node_index > 0
    cursor = RedTreeCursor(GreenTreeCursor(output, pos.node_index), pos.byte_index-UInt32(1))
    candidate = nothing
    for child in reverse(cursor)
        is_trivia(child) && continue
        candidate = child
    end

    candidate !== nothing && return ParseStreamPosition(candidate.byte_end+UInt32(1), candidate.green.position)

    # No children found - return the first non-trivia *token* (even if it
    # is the child of a non-terminal trivia node (e.g. an error)).
    byte_end = pos.byte_index
    for i in pos.node_index-1:-1:(pos.node_index - treesize(cursor))
        node = output[i]
        if is_terminal(node)
            if !is_trivia(node)
                return ParseStreamPosition(byte_end, i)
            end
            byte_end -= node.byte_span
        end
    end

    # Still none found. Return a sentinel value
    return ParseStreamPosition(0, 0)
end

"""
        first_child_position(stream::ParseStream, pos::ParseStreamPosition)

    Find the last non-trivia child of this node (in the GreenTree/RedTree sense) and
    return its position (i.e. the position as if that child had been the last thing parsed).
"""
function last_child_position(stream::ParseStream, pos::ParseStreamPosition)
    output = stream.output
    @assert pos.node_index > 0
    cursor = RedTreeCursor(GreenTreeCursor(output, pos.node_index), pos.byte_index-1)
    candidate = nothing
    for child in reverse(cursor)
        is_trivia(child) && continue
        return ParseStreamPosition(child.byte_end+UInt32(1), child.green.position)
    end
    return ParseStreamPosition(0, 0)
end

# Get last position in stream "of interest", skipping
# * parens nodes
# * deleted tokens (TOMBSTONE)
# * whitespace (if skip_trivia=true)
function peek_behind_pos(stream::ParseStream; skip_trivia::Bool=true,
                         skip_parens::Bool=true)
    # Work backwards through the output
    node_idx = length(stream.output)
    byte_idx = stream.next_byte

    # Skip parens nodes if requested
    if skip_parens
        while node_idx > 0
            node = stream.output[node_idx]
            if is_non_terminal(node) && kind(node) == K"parens"
                node_idx -= 1
            else
                break
            end
        end
    end

    # Skip trivia if requested
    while node_idx > 0
        node = stream.output[node_idx]
        if kind(node) == K"TOMBSTONE" || (skip_trivia && is_trivia(node))
            byte_idx -= node.byte_span
            # If this is a non-terminal node, skip its children without
            # subtracting their byte_spans, as they're already included in the parent
            if is_non_terminal(node)
                node_idx -= (1 + node.node_span)
            else
                node_idx -= 1
            end
        else
            break
        end
    end

    return ParseStreamPosition(byte_idx, node_idx)
end

function peek_behind(stream::ParseStream; kws...)
    peek_behind(stream, peek_behind_pos(stream; kws...))
end

#-------------------------------------------------------------------------------
# Stream output interface - the `bump_*` and `emit_*` family of functions
#
# Though note bump() really does both input and output

# Bump up until the `n`th token
# flags and remap_kind are applied to any non-trivia tokens
function _bump_until_n(stream::ParseStream, n::Integer, new_flags, remap_kind=K"None")
    if n < stream.lookahead_index
        return
    end
    for i in stream.lookahead_index:n
        tok = stream.lookahead[i]
        k = kind(tok)
        if k == K"EndMarker"
            break
        end
        f = new_flags | flags(tok)
        is_trivia = is_whitespace(k)
        is_trivia && (f |= TRIVIA_FLAG)
        outk = (is_trivia || remap_kind == K"None") ? k : remap_kind
        h = SyntaxHead(outk, f)

        # Calculate byte span for this token
        if i == stream.lookahead_index
            # First token in this batch - calculate span from current stream position
            prev_byte = stream.next_byte
        else
            # Subsequent tokens - use previous token's next_byte
            prev_byte = stream.lookahead[i-1].next_byte
        end
        byte_span = Int(tok.next_byte) - Int(prev_byte)

        # Create terminal RawGreenNode
        node = RawGreenNode(h, byte_span, kind(tok))
        push!(stream.output, node)

        # Update next_byte
        stream.next_byte += byte_span
    end
    stream.lookahead_index = n + 1
    # Defuse the time bomb
    stream.peek_count = 0
end

"""
    bump(stream [, flags=EMPTY_FLAGS];
         skip_newlines=false, error, remap_kind)

Copy the current token from the input stream to the output. Adds the given
flags to the output token (normally this would be the default `EMPTY_FLAGS` or
`TRIVIA_FLAG`).

Keyword arguments:
* `skip_newlines` - if `true`, newlines are treated as whitespace.
* `error` - if set, emit an error for this token
* `remap_kind` - the kind of the token in the output token stream if it needs
                 to be modified.
"""
function bump(stream::ParseStream, flags=EMPTY_FLAGS; skip_newlines=false,
              error=nothing, remap_kind::Kind=K"None")
    emark = position(stream)
    _bump_until_n(stream, _lookahead_index(stream, 1, skip_newlines), flags, remap_kind)
    if !isnothing(error)
        emit(stream, emark, K"error", flags, error=error)
    end
    # Return last token location in output if needed for reset_node!
    return position(stream)
end

"""
Bump comments and whitespace tokens preceding the next token

**Skips newlines** by default.  Set skip_newlines=false to avoid that.
"""
function bump_trivia(stream::ParseStream, flags=EMPTY_FLAGS;
                     skip_newlines=true, error=nothing)
    emark = position(stream)
    _bump_until_n(stream, _lookahead_index(stream, 1, skip_newlines) - 1, EMPTY_FLAGS)
    if !isnothing(error)
        emit(stream, emark, K"error", flags, error=error)
    end
    return position(stream)
end

"""
Bump an invisible zero-width token into the output

This is useful when surrounding syntax implies the presence of a token.  For
example, `2x` means `2*x` via the juxtaposition rules.
"""
function bump_invisible(stream::ParseStream, kind, flags=EMPTY_FLAGS;
                        error=nothing)
    b = stream.next_byte
    h = SyntaxHead(kind, flags)
    # Zero-width token
    node = RawGreenNode(h, 0, kind)
    push!(stream.output, node)
    # No need to update next_byte for zero-width token
    if !isnothing(error)
        emit_diagnostic(stream, b:b-1, error=error)
    end
    stream.peek_count = 0
    return position(stream)
end

"""
Bump several tokens, gluing them together into a single token

This is for use in special circumstances where the parser needs to resolve
lexing ambiguities. There's no special whitespace handling — bump any
whitespace if necessary with bump_trivia.
"""
function bump_glue(stream::ParseStream, kind, flags)
    i = stream.lookahead_index
    h = SyntaxHead(kind, flags)
    # Calculate byte span for glued tokens
    start_byte = stream.next_byte
    end_byte = stream.lookahead[i+1].next_byte
    byte_span = end_byte - start_byte

    node = RawGreenNode(h, byte_span, kind)
    push!(stream.output, node)
    stream.next_byte += byte_span
    stream.lookahead_index += 2
    stream.peek_count = 0
    return position(stream)
end

"""
Reset kind or flags of an existing node in the output stream

This is a hack, but in some limited occasions the trailing syntax may change
the kind or flags of a token in a way which would require unbounded lookahead
in a recursive descent parser. Modifying the output with reset_node! is useful
in those cases.
"""
function reset_node!(stream::ParseStream, pos::ParseStreamPosition;
                     kind=nothing, flags=nothing)
    node = stream.output[pos.node_index]
    stream.output[pos.node_index] = reset_node(node, kind, flags)
end

"""
Move `numbytes` from the range at output position `pos+1` to the output
position `pos`. If the donor range becomes empty, mark it dead with
K"TOMBSTONE" and return `true`, otherwise return `false`.

Hack alert! This is used only for managing the complicated rules related to
dedenting triple quoted strings.
"""
function steal_token_bytes!(stream::ParseStream, pos::ParseStreamPosition, numbytes)
    i = pos.node_index
    t1 = stream.output[i]
    t2 = stream.output[i+1]
    @assert is_terminal(t1) && is_terminal(t2)

    stream.output[i] = RawGreenNode(t1.head, t1.byte_span + numbytes,
                                    t1.orig_kind)

    t2_is_empty = t2.byte_span == numbytes
    head2 = t2_is_empty ? SyntaxHead(K"TOMBSTONE", EMPTY_FLAGS) : t2.head
    stream.output[i+1] = RawGreenNode(head2, t2.byte_span - numbytes,
                                      t2.orig_kind)
    return t2_is_empty
end

# Get position of last item emitted into the output stream
function Base.position(stream::ParseStream)
    byte_idx = stream.next_byte
    node_idx = length(stream.output)

    ParseStreamPosition(byte_idx, node_idx)
end

"""
    emit(stream, mark, kind, flags = EMPTY_FLAGS; error=nothing)

Emit a new non-terminal node into the output which covers source bytes from `mark` to
the end of the most recent token which was `bump()`'ed. The starting `mark`
should be a previous return value of `position()`. The emitted node will have
its `node_span` set to the number of nodes emitted since `mark`.
"""
function emit(stream::ParseStream, mark::ParseStreamPosition, kind::Kind,
              flags::RawFlags = EMPTY_FLAGS; error=nothing)
    # Calculate byte span from mark position to current
    mark_byte = mark.byte_index
    current_byte = stream.next_byte
    byte_span = current_byte - mark_byte

    # Calculate node span (number of children, exclusive of the node itself)
    node_span = length(stream.output) - mark.node_index

    # Create non-terminal RawGreenNode
    node = RawGreenNode(SyntaxHead(kind, flags), byte_span, node_span)

    if !isnothing(error)
        emit_diagnostic(stream, mark_byte:current_byte-1, error=error)
    end

    push!(stream.output, node)
    # Note: emit() for non-terminals doesn't advance next_byte
    # because it's a range over already-emitted tokens
    return position(stream)
end

function emit_diagnostic(stream::ParseStream, byterange::AbstractUnitRange; kws...)
    emit_diagnostic(stream.diagnostics, byterange; kws...)
    return nothing
end

"""
Emit a diagnostic at the position of the next token

If `whitespace` is true, the diagnostic is positioned on the whitespace before
the next token. Otherwise it's positioned at the next token as returned by `peek()`.
"""
function emit_diagnostic(stream::ParseStream; whitespace=false, kws...)
    i = _lookahead_index(stream, 1, true)
    begin_tok_i = i
    end_tok_i = i
    if whitespace
        # It's the whitespace which is the error. Find the range of the current
        # whitespace.
        begin_tok_i = stream.lookahead_index
        end_tok_i = is_whitespace(stream.lookahead[i]) ?
                    i : max(stream.lookahead_index, i - 1)
    end
    fbyte = lookahead_token_first_byte(stream, begin_tok_i)
    lbyte = lookahead_token_last_byte(stream, end_tok_i)
    emit_diagnostic(stream, fbyte:lbyte; kws...)
    return nothing
end

function emit_diagnostic(stream::ParseStream, mark::ParseStreamPosition; trim_whitespace=true, kws...)
    # Find the byte range from mark to current position
    start_byte = mark.byte_index
    end_byte = stream.next_byte - 1

    if trim_whitespace
        # TODO: Implement whitespace trimming for unified output
        # This would require scanning the output array
    end

    emit_diagnostic(stream, start_byte:end_byte; kws...)
end

function emit_diagnostic(stream::ParseStream, mark::ParseStreamPosition,
                         end_mark::ParseStreamPosition; kws...)
    emit_diagnostic(stream, mark.byte_index:end_mark.byte_index-1; kws...)
end

function emit_diagnostic(diagnostics::AbstractVector{Diagnostic},
                         byterange::AbstractUnitRange; kws...)
    push!(diagnostics, Diagnostic(first(byterange), last(byterange); kws...))
end

# Tree construction from the list of text ranges held by ParseStream

# API for extracting results from ParseStream

function sourcetext(stream::ParseStream; steal_textbuf=false)
    Base.depwarn("Use of `sourcetext(::ParseStream)` is deprecated. Use `SourceFile(stream)` instead", :sourcetext)
    root = stream.text_root
    # The following kinda works but makes the return type of this method type
    # unstable. (Also codeunit(root) == UInt8 doesn't imply UTF-8 encoding?)
    # if root isa AbstractString && codeunit(root) == UInt8
    #     return root
    str = if root isa String || root isa SubString
        root
    elseif steal_textbuf
        String(stream.textbuf)
    else
        # Safe default for other cases is to copy the buffer. Technically this
        # could possibly be avoided in some situations, but might have side
        # effects such as mutating stream.text_root or stealing the storage of
        # stream.textbuf
        String(copy(stream.textbuf))
    end
    SubString(str, first_byte(stream), thisind(str, last_byte(stream)))
end

function SourceFile(stream::ParseStream; kws...)
    fbyte = first_byte(stream)
    lbyte = last_byte(stream)
    if !isempty(stream.diagnostics)
        lbyte = max(lbyte, maximum(last_byte(d) for d in stream.diagnostics))
    end
    # See also sourcetext()
    srcroot = stream.text_root
    str = if srcroot isa String
        SubString(srcroot, fbyte, thisind(srcroot, lbyte))
    elseif srcroot isa SubString{String}
        SubString(srcroot, fbyte, thisind(srcroot, lbyte))
    else
        SubString(String(stream.textbuf[fbyte:lbyte]))
    end
    return SourceFile(str; first_index=first_byte(stream), kws...)
end

"""
    unsafe_textbuf(stream)

Return the `Vector{UInt8}` text buffer being parsed by this `ParseStream`.

!!! warning
    The caller must hold a reference to `stream` while using textbuf
"""
unsafe_textbuf(stream) = stream.textbuf

first_byte(stream::ParseStream) = first(stream.output).byte_span + 1 # After sentinel
last_byte(stream::ParseStream) = stream.next_byte - 1
any_error(stream::ParseStream) = any_error(stream.diagnostics)

# Return last non-whitespace byte which was parsed
function last_non_whitespace_byte(stream::ParseStream)
    byte_pos = stream.next_byte
    for i = length(stream.output):-1:1
        node = stream.output[i]
        if is_terminal(node)
            if kind(node) in KSet"Comment Whitespace NewlineWs ErrorEofMultiComment" || kind(node) == K"error" && node.byte_span == 0
                byte_pos -= node.byte_span
            else
                return byte_pos - 1
            end
        end
    end
    return first_byte(stream) - 1
end

function Base.empty!(stream::ParseStream)
    # Keep only the sentinel
    if !isempty(stream.output) && kind(stream.output[1]) == K"TOMBSTONE"
        resize!(stream.output, 1)
    else
        empty!(stream.output)
        # Restore sentinel node
        push!(stream.output, RawGreenNode(SyntaxHead(K"TOMBSTONE", EMPTY_FLAGS), 0, K"TOMBSTONE"))
    end
    # Reset next_byte to initial position
    stream.next_byte = 1
end
