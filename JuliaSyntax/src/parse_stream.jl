#-------------------------------------------------------------------------------
# Flags hold auxilary information about tokens/nonterminals which the Kind
# doesn't capture in a nice way.
#
# TODO: Use `primitive type SyntaxFlags 16 end` rather than an alias?
const RawFlags = UInt16
const EMPTY_FLAGS = RawFlags(0)
# Applied to tokens which are syntax trivia after parsing
const TRIVIA_FLAG = RawFlags(1<<0)

# Record whether operators are dotted
const DOTOP_FLAG = RawFlags(1<<1)
# Record whether operator has a suffix
const SUFFIXED_FLAG        = RawFlags(1<<2)

# Distinguish various syntaxes which are mapped to K"call"
const PREFIX_CALL_FLAG = RawFlags(0<<3)
const INFIX_FLAG       = RawFlags(1<<3)
const PREFIX_OP_FLAG   = RawFlags(2<<3)
const POSTFIX_OP_FLAG  = RawFlags(3<<3)

# The next two bits could overlap with the previous two if necessary
# Set when kind == K"String" was triple-delimited as with """ or ```
const TRIPLE_STRING_FLAG = RawFlags(1<<5)
# Set when a string or identifier needs "raw string" unescaping
const RAW_STRING_FLAG = RawFlags(1<<6)

# Token-only flag
# Record whether a token had preceding whitespace
const PRECEDING_WHITESPACE_FLAG = RawFlags(1<<7)

# Flags holding the dimension of an nrow or other UInt8 not held in the source
const NUMERIC_FLAGS = RawFlags(RawFlags(0xff)<<8)
# Todo ERROR_FLAG = 0x8000 ?

function set_numeric_flags(n::Integer)
    f = RawFlags((n << 8) & NUMERIC_FLAGS)
    if numeric_flags(f) != n
        error("Numeric flags unable to hold large integer $n")
    end
    f
end

function call_type_flags(f::RawFlags)
    f & 0b11000
end

function numeric_flags(f::RawFlags)
    Int((f >> 8) % UInt8)
end

function remove_flags(n::RawFlags, fs...)
    RawFlags(n & ~(RawFlags((|)(fs...))))
end

# Return true if any of `test_flags` are set
has_flags(flags::RawFlags, test_flags) = (flags & test_flags) != 0

#-------------------------------------------------------------------------------
struct SyntaxHead
    kind::Kind
    flags::RawFlags
end

kind(head::SyntaxHead) = head.kind
flags(head::SyntaxHead) = head.flags

function Base.summary(head::SyntaxHead)
    untokenize(head, unique=false, include_flag_suff=false)
end

function untokenize(head::SyntaxHead; unique=true, include_flag_suff=true)
    str = is_error(kind(head)) ? "error" : untokenize(kind(head); unique=unique)::String
    if is_dotted(head)
        str = "."*str
    end
    # Ignore some flags:
    # DOTOP_FLAG is represented above with . prefix
    # PRECEDING_WHITESPACE_FLAG relates to the environment of this token
    suffix_flags = remove_flags(flags(head), DOTOP_FLAG, PRECEDING_WHITESPACE_FLAG)
    if include_flag_suff && suffix_flags != EMPTY_FLAGS
        str = str*"-"
        is_trivia(head)  && (str = str*"t")
        is_infix_op_call(head)          && (str = str*"i")
        is_prefix_op_call(head)  && (str = str*"pre")
        is_postfix_op_call(head) && (str = str*"post")
        has_flags(head, TRIPLE_STRING_FLAG) && (str = str*"s")
        has_flags(head, RAW_STRING_FLAG) && (str = str*"r")
        is_suffixed(head) && (str = str*"S")
        n = numeric_flags(head)
        n != 0 && (str = str*string(n))
    end
    str
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

is_trivia(x) = has_flags(x, TRIVIA_FLAG)
is_prefix_call(x)     = call_type_flags(x) == PREFIX_CALL_FLAG
is_infix_op_call(x)   = call_type_flags(x) == INFIX_FLAG
is_prefix_op_call(x)  = call_type_flags(x) == PREFIX_OP_FLAG
is_postfix_op_call(x) = call_type_flags(x) == POSTFIX_OP_FLAG
is_dotted(x) = has_flags(x, DOTOP_FLAG)
is_suffixed(x) = has_flags(x, SUFFIXED_FLAG)
is_decorated(x) = is_dotted(x) || is_suffixed(x)
preceding_whitespace(x) = has_flags(x, PRECEDING_WHITESPACE_FLAG)
numeric_flags(x) = numeric_flags(flags(x))

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
    next_byte::UInt32
end

function SyntaxToken(head::SyntaxHead, next_byte::Integer)
    SyntaxToken(head, kind(head), next_byte)
end

function Base.show(io::IO, tok::SyntaxToken)
    print(io, rpad(untokenize(tok.head, unique=false), 15), " |", tok.next_byte)
end

head(tok::SyntaxToken) = tok.head


#-------------------------------------------------------------------------------

"""
Range in the source text which will become a node in the tree. Can be either a
token (leaf node of the tree) or an interior node, depending on how the
start_mark compares to previous nodes.
"""
struct TaggedRange
    head::SyntaxHead # Kind,flags
    # The following field is used for one of two things:
    # - For leaf nodes it's an index in the tokens array
    # - For non-leaf nodes it points to the index of the first child
    first_token::UInt32
    last_token::UInt32
end

head(range::TaggedRange) = range.head

#-------------------------------------------------------------------------------
struct ParseStreamPosition
    token_index::UInt32  # Index of last token in output
    range_index::UInt32
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
    # Buffer of finalized tokens
    tokens::Vector{SyntaxToken}
    # Parser output as an ordered sequence of ranges, parent nodes after children.
    ranges::Vector{TaggedRange}
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
        # or dev versons as the release version using only major and minor version
        # numbers. This means we're inexact for old dev versions but that seems
        # like an acceptable tradeoff.
        ver = (version.major, version.minor)
        # Initial sentinel token containing the first byte of the first real token.
        sentinel = SyntaxToken(SyntaxHead(K"TOMBSTONE", EMPTY_FLAGS),
                               K"TOMBSTONE", next_byte)
        new(text_buf,
            text_root,
            lexer,
            Vector{SyntaxToken}(),
            1,
            Vector{Vector{ParseStreamPosition}}(),
            SyntaxToken[sentinel],
            Vector{TaggedRange}(),
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
function ParseStream(text::SubString, index::Integer=1; version=VERSION)
    # See also IOBuffer(SubString("x"))
    ParseStream(unsafe_wrap(Vector{UInt8}, pointer(text), sizeof(text)),
                text, index, version)
end
function ParseStream(text::AbstractString, index::Integer=1; version=VERSION)
    ParseStream(String(text), index; version=version)
end

# IO-based cases
function ParseStream(io::IOBuffer; version=VERSION)
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
    println(io, "ParseStream at position $(_next_byte(stream))")
end

function show_diagnostics(io::IO, stream::ParseStream, code)
    show_diagnostics(io, stream.diagnostics, code)
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
# Return true when a token was emitted last at stream position `pos`
function token_is_last(stream, pos)
    return pos.range_index == 0 ||
           pos.token_index > stream.ranges[pos.range_index].last_token
end

# Compute the first byte of a token at given index `i`
function token_first_byte(stream, i)
    stream.tokens[i-1].next_byte
end

function token_last_byte(stream::ParseStream, i)
    stream.tokens[i].next_byte - 1
end

function token_span(stream::ParseStream, i)
    stream.tokens[i].next_byte - stream.tokens[i-1].next_byte
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
        was_whitespace = k in (K"Whitespace", K"Comment", K"NewlineWs")
        had_whitespace |= was_whitespace
        f = EMPTY_FLAGS
        had_whitespace && (f |= PRECEDING_WHITESPACE_FLAG)
        raw.dotop      && (f |= DOTOP_FLAG)
        raw.suffix     && (f |= SUFFIXED_FLAG)
        push!(lookahead, SyntaxToken(SyntaxHead(k, f), raw.endbyte + 2))
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
    last(stream.tokens).next_byte
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
    error("The parser seems stuck at byte $(_next_byte(stream))")
end

"""
    peek(stream [, n=1]; skip_newlines=false)

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
    return @inbounds head(stream.lookahead[i])
end


struct FullToken
    head::SyntaxHead
    first_byte::UInt32
    last_byte::UInt32
end

head(t::FullToken) = t.head
first_byte(t::FullToken) = t.first_byte
last_byte(t::FullToken) = t.last_byte
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
    peek_behind(ps; skip_trivia=true)
    peek_behind(ps, pos::ParseStreamPosition)

Return information about a span which was previously inserted into the output,
defaulting to the most previous nontrivia node when `skip_trivia` is true, or
at the provided position `pos`.

Retroactively inspecting or modifying the parser's output can be confusing, so
using this function should be avoided where possible.
"""
function peek_behind(stream::ParseStream, pos::ParseStreamPosition)
    if token_is_last(stream, pos) && !isempty(stream.tokens)
        t = stream.tokens[pos.token_index]
        return (kind=kind(t),
                flags=flags(t),
                orig_kind=t.orig_kind,
                is_leaf=true)
    elseif !isempty(stream.ranges)
        r = stream.ranges[pos.range_index]
        return (kind=kind(r),
                flags=flags(r),
                orig_kind=K"None",
                is_leaf=false)
    else
        internal_error("Can't peek behind at start of stream")
    end
end

function peek_behind(stream::ParseStream; skip_trivia::Bool=true)
    pos = position(stream)
    if !skip_trivia || !token_is_last(stream, pos)
        return peek_behind(stream, pos)
    else
        token_index = lastindex(stream.tokens)
        range_index = lastindex(stream.ranges)
        last_token_in_nonterminal = isempty(stream.ranges) ? 0 :
                                    stream.ranges[range_index].last_token
        while token_index > last_token_in_nonterminal
            t = stream.tokens[token_index]
            if !is_trivia(t) && kind(t) != K"TOMBSTONE"
                break
            end
            token_index -= 1
        end
        if token_index > 0
            return peek_behind(stream, ParseStreamPosition(token_index, range_index))
        else
            internal_error("Can't peek behind at start of stream")
        end
    end
end

#-------------------------------------------------------------------------------
# Stream output interface - the `bump_*` and `emit_*` family of functions
#
# Though note bump() really does both input and output

# Bump up until the `n`th token
# flags and remap_kind are applied to any non-trivia tokens
function _bump_until_n(stream::ParseStream, n::Integer, flags, remap_kind=K"None")
    if n < stream.lookahead_index
        return
    end
    for i in stream.lookahead_index:n
        tok = stream.lookahead[i]
        k = kind(tok)
        if k == K"EndMarker"
            break
        end
        f = flags | remove_flags((@__MODULE__).flags(tok), PRECEDING_WHITESPACE_FLAG)
        is_trivia = k ∈ (K"Whitespace", K"Comment", K"NewlineWs")
        is_trivia && (f |= TRIVIA_FLAG)
        outk = (is_trivia || remap_kind == K"None") ? k : remap_kind
        h = SyntaxHead(outk, f)
        push!(stream.tokens, SyntaxToken(h, kind(tok), tok.next_byte))
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
example, `2x` means `2*x` via the juxtoposition rules.
"""
function bump_invisible(stream::ParseStream, kind, flags=EMPTY_FLAGS;
                        error=nothing)
    b = _next_byte(stream)
    h = SyntaxHead(kind, flags)
    push!(stream.tokens, SyntaxToken(h, b))
    if !isnothing(error)
        _emit_diagnostic(stream, b, b-1, error=error)
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
function bump_glue(stream::ParseStream, kind, flags, num_tokens)
    i = stream.lookahead_index
    h = SyntaxHead(kind, flags)
    push!(stream.tokens, SyntaxToken(h, stream.lookahead[i+1].next_byte))
    stream.lookahead_index += num_tokens
    stream.peek_count = 0
    return position(stream)
end

"""
    bump_split(stream, token_spec1, [token_spec2 ...])

Bump the next token, splitting it into several pieces

Tokens are defined by a number of `token_spec` of shape `(nbyte, kind, flags)`.
The number of input bytes of the last spec is taken from the remaining bytes of
the input token, with the associated `nbyte` ignored.

This is a hack which helps resolves the occasional lexing ambiguity. For
example
* Whether .+ should be a single token or the composite (. +) which is used for
  standalone operators.
* Whether ... is splatting (most of the time) or three . tokens in import paths

TODO: Are these the only cases?  Can we replace this general utility with a
simpler one which only splits preceding dots?
"""
function bump_split(stream::ParseStream, split_spec...)
    tok = stream.lookahead[stream.lookahead_index]
    stream.lookahead_index += 1
    b = _next_byte(stream)
    for (i, (nbyte, k, f)) in enumerate(split_spec)
        h = SyntaxHead(k, f)
        b = (i == length(split_spec)) ? tok.next_byte : b + nbyte
        push!(stream.tokens, SyntaxToken(h, kind(tok), b))
    end
    stream.peek_count = 0
    # Returning position(stream) like the other bump* methods would be
    # ambiguous here; return nothing instead.
    nothing
end

function _reset_node_head(x, k, f)
    h = SyntaxHead(isnothing(k) ? kind(x)  : k,
                   isnothing(f) ? flags(x) : f)
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
    if token_is_last(stream, pos)
        t = stream.tokens[pos.token_index]
        stream.tokens[pos.token_index] = SyntaxToken(_reset_node_head(t, kind, flags),
                                                     t.orig_kind, t.next_byte)
    else
        r = stream.ranges[pos.range_index]
        stream.ranges[pos.range_index] = TaggedRange(_reset_node_head(r, kind, flags),
                                                     r.first_token, r.last_token)
    end
end

"""
Move `numbytes` from the range at output position `pos+1` to the output
position `pos`. If the donor range becomes empty, mark it dead with
K"TOMBSTONE" and return `true`, otherwise return `false`.

Hack alert! This is used only for managing the complicated rules related to
dedenting triple quoted strings.
"""
function steal_token_bytes!(stream::ParseStream, pos::ParseStreamPosition, numbytes)
    i = pos.token_index
    t1 = stream.tokens[i]
    t2 = stream.tokens[i+1]

    t1_next_byte = t1.next_byte + numbytes
    stream.tokens[i] = SyntaxToken(t1.head, t1.orig_kind, t1_next_byte)

    t2_is_empty = t1_next_byte == t2.next_byte
    head2 = t2_is_empty ? SyntaxHead(K"TOMBSTONE", EMPTY_FLAGS) : t2.head
    stream.tokens[i+1] = SyntaxToken(head2, t2.orig_kind, t2.next_byte)
    return t2_is_empty
end

function Base.position(stream::ParseStream)
    ParseStreamPosition(lastindex(stream.tokens), lastindex(stream.ranges))
end

"""
    emit(stream, mark, kind, flags = EMPTY_FLAGS; error=nothing)

Emit a new text span into the output which covers source bytes from `mark` to
the end of the most recent token which was `bump()`'ed. The starting `mark`
should be a previous return value of `position()`.
"""
function emit(stream::ParseStream, mark::ParseStreamPosition, kind::Kind,
              flags::RawFlags = EMPTY_FLAGS; error=nothing)
    first_token = mark.token_index + 1
    range = TaggedRange(SyntaxHead(kind, flags), first_token, length(stream.tokens))
    if !isnothing(error)
        # The first child must be a leaf, otherwise ranges would be improperly
        # nested.
        fbyte = token_first_byte(stream, first_token)
        lbyte = token_last_byte(stream, lastindex(stream.tokens))
        _emit_diagnostic(stream, fbyte, lbyte, error=error)
    end
    push!(stream.ranges, range)
    return position(stream)
end

function _emit_diagnostic(stream::ParseStream, fbyte, lbyte; kws...)
    push!(stream.diagnostics, Diagnostic(fbyte, lbyte; kws...))
    return nothing
end

"""
Emit a diagnostic at the position of the next token

If `whitespace` is true, the diagnostic is positioned on the whitespace before
the next token. Otherwise it's positioned at the next token as returned by `peek()`.

TODO: Rename? This doesn't emit normal tokens into the output event list!
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
    _emit_diagnostic(stream, fbyte, lbyte; kws...)
    return nothing
end

function emit_diagnostic(stream::ParseStream, mark::ParseStreamPosition; kws...)
    _emit_diagnostic(stream, token_first_byte(stream, mark.token_index),
                     _next_byte(stream) - 1; kws...)
end

function emit_diagnostic(stream::ParseStream, r::NTuple{2,ParseStreamPosition}; kws...)
    emit_diagnostic(stream, first(r), last(r); kws...)
end

function emit_diagnostic(stream::ParseStream, mark::ParseStreamPosition,
                         end_mark::ParseStreamPosition; kws...)
    fbyte = token_first_byte(stream, mark.token_index)
    lbyte = token_first_byte(stream, end_mark.token_index) - 1
    _emit_diagnostic(stream, fbyte, lbyte; kws...)
end

#-------------------------------------------------------------------------------
# Tree construction from the list of text ranges held by ParseStream

# API for extracting results from ParseStream

"""
    build_tree(::Type{NodeType}, stream::ParseStream;
               wrap_toplevel_as_kind=nothing, kws...)

Construct a tree with `NodeType` nodes from a ParseStream using depth-first
traversal. `NodeType` must have the constructors

    NodeType(head::SyntaxHead, span::Integer)
    NodeType(head::SyntaxHead, span::Integer, children::Vector{NodeType})

A single node which covers the input is expected, but if the ParseStream has
multiple nodes at the top level, `wrap_toplevel_as_kind` may be used to wrap
them in a single node.

The tree here is constructed depth-first, but it would also be possible to use
a bottom-up tree builder interface similar to rust-analyzer. (In that case we'd
traverse the list of ranges backward rather than forward.)
"""
function build_tree(::Type{NodeType}, stream::ParseStream;
                    wrap_toplevel_as_kind=nothing, kws...) where NodeType
    stack = Vector{NamedTuple{(:first_token,:node),Tuple{Int,NodeType}}}()

    tokens = stream.tokens
    ranges = stream.ranges
    i = firstindex(tokens)
    j = firstindex(ranges)
    while true
        last_token = j <= lastindex(ranges) ?
                     ranges[j].last_token : lastindex(tokens)
        # Process tokens to nodes for all tokens used by the next internal node
        while i <= last_token
            t = tokens[i]
            if kind(t) == K"TOMBSTONE"
                i += 1
                continue # Ignore removed tokens
            end
            node = NodeType(head(t), token_span(stream, i))
            push!(stack, (first_token=i, node=node))
            i += 1
        end
        if j > lastindex(ranges)
            break
        end
        # Process internal nodes which end at the current position
        while j <= lastindex(ranges)
            r = ranges[j]
            if r.last_token != last_token
                break
            end
            if kind(r) == K"TOMBSTONE"
                j += 1
                continue
            end
            # Collect children from the stack for this internal node
            k = length(stack) + 1
            while k > 1 && r.first_token <= stack[k-1].first_token
                k -= 1
            end
            children = (stack[n].node for n = k:length(stack))
            node = NodeType(head(r), children)
            resize!(stack, k-1)
            push!(stack, (first_token=r.first_token, node=node))
            j += 1
        end
    end
    if length(stack) == 1
        return only(stack).node
    elseif !isnothing(wrap_toplevel_as_kind)
        # Mostly for debugging
        children = (x.node for x in stack)
        return NodeType(SyntaxHead(wrap_toplevel_as_kind, EMPTY_FLAGS), children)
    else
        error("Found multiple nodes at top level")
    end
end


"""
    sourcetext(stream::ParseStream; steal_textbuf=true)

Return the source text being parsed by this `ParseStream` as a UTF-8 encoded
string.

If `steal_textbuf==true`, this is permitted to steal the content of the
stream's text buffer. Note that this leaves the `ParseStream` in an invalid
state for further parsing.
"""
function sourcetext(stream::ParseStream; steal_textbuf=false)
    root = stream.text_root
    if root isa AbstractString && codeunit(root) == UInt8
        return root
    elseif steal_textbuf
        return String(stream.textbuf)
    else
        # Safe default for other cases is to copy the buffer. Technically this
        # could possibly be avoided in some situations, but might have side
        # effects such as mutating stream.text_root or stealing the storage of
        # stream.textbuf
        return String(copy(stream.textbuf))
    end
end

"""
    textbuf(stream)

Return the `Vector{UInt8}` text buffer being parsed by this `ParseStream`.
"""
textbuf(stream) = stream.textbuf

first_byte(stream::ParseStream) = first(stream.tokens).next_byte # Use sentinel token
last_byte(stream::ParseStream) = _next_byte(stream)-1
any_error(stream::ParseStream) = any_error(stream.diagnostics)

function Base.empty!(stream::ParseStream)
    t = last(stream.tokens)
    empty!(stream.tokens)
    # Restore sentinel token
    push!(stream.tokens, SyntaxToken(SyntaxHead(K"TOMBSTONE",EMPTY_FLAGS),
                                     K"TOMBSTONE", t.next_byte))
    empty!(stream.ranges)
end
