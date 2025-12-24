# Token flags - may be set for operator kinded tokens
# Operator has a suffix
const SUFFIXED_FLAG = RawFlags(1<<2)

# Set for K"call", K"dotcall" or any syntactic operator heads
# Distinguish various syntaxes which are mapped to K"call"
const PREFIX_CALL_FLAG = RawFlags(0<<3)
const INFIX_FLAG       = RawFlags(1<<3)
const PREFIX_OP_FLAG   = RawFlags(2<<3)
const POSTFIX_OP_FLAG  = RawFlags(3<<3)

# The following flags are quite head-specific and may overlap with numeric flags

"""
Set when K"string" or K"cmdstring" was triple-delimited as with \"\"\" or ```
"""
const TRIPLE_STRING_FLAG = RawFlags(1<<8)

"""
Set when a K"string", K"cmdstring" or K"Identifier" needs raw string unescaping
"""
const RAW_STRING_FLAG = RawFlags(1<<9)

"""
Set for K"tuple", K"block" or K"macrocall" which are delimited by parentheses
"""
const PARENS_FLAG = RawFlags(1<<8)

"""
Set for various delimited constructs when they contains a trailing comma. For
example, to distinguish `(a,b,)` vs `(a,b)`, and `f(a)` vs `f(a,)`. Kinds where
this applies are: `tuple call dotcall macrocall vect curly braces <: >:`.
"""
const TRAILING_COMMA_FLAG = RawFlags(1<<9)

"""
Set for K"quote" for the short form `:x` as opposed to long form `quote x end`
"""
const COLON_QUOTE = RawFlags(1<<8)

"""
Set for K"toplevel" which is delimited by parentheses
"""
const TOPLEVEL_SEMICOLONS_FLAG = RawFlags(1<<8)

"""
Set for K"function" in short form definitions such as `f() = 1`
"""
const SHORT_FORM_FUNCTION_FLAG = RawFlags(1<<8)

"""
Set for K"struct" when mutable
"""
const MUTABLE_FLAG = RawFlags(1<<8)

"""
Set for K"module" when it's not bare (`module`, not `baremodule`)
"""
const BARE_MODULE_FLAG = RawFlags(1<<8)

# Flags holding the dimension of an nrow or other UInt8 not held in the source
# TODO: Given this is only used for nrow/ncat, we could actually use all the flags?
const NUMERIC_FLAGS = RawFlags(RawFlags(0xff)<<8)

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

flags(tok::SyntaxToken) = remove_flags(flags(tok.head), NUMERIC_FLAGS)

"""
    is_prefix_call(x)

Return true for normal prefix function call syntax such as the `f` call node
parsed from `f(x)`.
"""
is_prefix_call(x)     = call_type_flags(x) == PREFIX_CALL_FLAG

"""
    is_infix_op_call(x)

Return true for infix operator calls such as the `+` call node parsed from
`x + y`.
"""
is_infix_op_call(x)   = call_type_flags(x) == INFIX_FLAG

"""
    is_prefix_op_call(x)

Return true for prefix operator calls such as the `+` call node parsed from `+x`.
"""
is_prefix_op_call(x)  = call_type_flags(x) == PREFIX_OP_FLAG

"""
    is_postfix_op_call(x)

Return true for postfix operator calls such as the `'ᵀ` call node parsed from `x'ᵀ`.
"""
is_postfix_op_call(x) = call_type_flags(x) == POSTFIX_OP_FLAG


"""
    is_suffixed(x)

Return true for operators which have suffixes, such as `+₁`
"""
is_suffixed(x) = has_flags(x, SUFFIXED_FLAG)


"""
    numeric_flags(x)

Return the number attached to a `SyntaxHead`. This is only for kinds `K"nrow"`
and `K"ncat"`, for now.
"""
numeric_flags(x) = numeric_flags(flags(x))

function untokenize(head::SyntaxHead; unique=true, include_flag_suff=true)
    str = (is_error(kind(head)) ? untokenize(kind(head); unique=false) :
           untokenize(kind(head); unique=unique))::String
    if include_flag_suff
        is_trivia(head)  && (str = str*"-t")
        is_infix_op_call(head)   && (str = str*"-i")
        is_prefix_op_call(head)  && (str = str*"-pre")
        is_postfix_op_call(head) && (str = str*"-post")

        k = kind(head)
        # Handle numeric flags for nrow/ncat nodes
        if k in KSet"nrow ncat typed_ncat"
            n = numeric_flags(head)
            n != 0 && (str = str*"-"*string(n))
        else
            # Handle head-specific flags that overlap with numeric flags
            if k in KSet"string cmdstring Identifier"
                has_flags(head, TRIPLE_STRING_FLAG) && (str = str*"-s")
                has_flags(head, RAW_STRING_FLAG) && (str = str*"-r")
            elseif k in KSet"tuple block macrocall"
                has_flags(head, PARENS_FLAG) && (str = str*"-p")
            elseif k == K"quote"
                has_flags(head, COLON_QUOTE) && (str = str*"-:")
            elseif k == K"toplevel"
                has_flags(head, TOPLEVEL_SEMICOLONS_FLAG) && (str = str*"-;")
            elseif k == K"function"
                has_flags(head, SHORT_FORM_FUNCTION_FLAG) && (str = str*"-=")
            elseif k == K"struct"
                has_flags(head, MUTABLE_FLAG) && (str = str*"-mut")
            elseif k == K"module"
                has_flags(head, BARE_MODULE_FLAG) && (str = str*"-bare")
            end
            if k in KSet"tuple call dotcall macrocall vect curly braces <: >:" &&
                    has_flags(head, TRAILING_COMMA_FLAG)
                str *= "-,"
            end
        end
        is_suffixed(head) && (str = str*"-suf")
    end
    str
end


#-------------------------------------------------------------------------------
# ParseStream Post-processing

function validate_tokens(stream::ParseStream)
    txtbuf = unsafe_textbuf(stream)
    charbuf = IOBuffer()

    # Process terminal nodes in the output
    fbyte = stream.output[1].byte_span+1  # Start after sentinel
    for i = 2:length(stream.output)
        node = stream.output[i]
        if !is_terminal(node) || kind(node) == K"TOMBSTONE"
            continue
        end

        k = kind(node)
        nbyte = fbyte + node.byte_span
        tokrange = fbyte:nbyte-1
        error_kind = K"None"

        if k in KSet"Integer BinInt OctInt HexInt"
            # The following shouldn't be able to error...
            # parse_int_literal
            # parse_uint_literal
        elseif k == K"Float" || k == K"Float32"
            underflow0 = false
            if k == K"Float"
                x, code = parse_float_literal(Float64, txtbuf, fbyte, nbyte)
                # jl_strtod_c can return "underflow" even for valid cases such
                # as `5e-324` where the source is an exact representation of
                # `x`. So only warn when underflowing to zero.
                underflow0 = code === :underflow && x == 0
            else
                x, code = parse_float_literal(Float32, txtbuf, fbyte, nbyte)
                underflow0 = code === :underflow && x == 0
            end
            if code === :ok
                # pass
            elseif code === :overflow
                emit_diagnostic(stream, tokrange,
                                error="overflow in floating point literal")
                error_kind = K"ErrorNumericOverflow"
            elseif underflow0
                emit_diagnostic(stream, tokrange,
                                warning="underflow to zero in floating point literal")
            end
        elseif k == K"Char"
            @assert fbyte < nbyte # Already handled in the parser
            truncate(charbuf, 0)
            had_error = unescape_julia_string(charbuf, txtbuf, fbyte,
                                              nbyte, stream.diagnostics)
            if had_error
                error_kind = K"ErrorInvalidEscapeSequence"
            else
                seek(charbuf,0)
                read(charbuf, Char)
                if !eof(charbuf)
                    error_kind = K"ErrorOverLongCharacter"
                    emit_diagnostic(stream, tokrange,
                                    error="character literal contains multiple characters")
                end
            end
        elseif k == K"String" && !has_flags(node, RAW_STRING_FLAG)
            had_error = unescape_julia_string(devnull, txtbuf, fbyte,
                                              nbyte, stream.diagnostics)
            if had_error
                error_kind = K"ErrorInvalidEscapeSequence"
            end
        elseif is_error(k) && k != K"error"
            # Emit messages for non-generic token errors
            tokstr = String(txtbuf[tokrange])
            msg = if k in KSet"ErrorInvisibleChar ErrorUnknownCharacter ErrorIdentifierStart"
                "$(_token_error_descriptions[k]) $(repr(tokstr[1]))"
            elseif k in KSet"ErrorInvalidUTF8 ErrorBidiFormatting"
                "$(_token_error_descriptions[k]) $(repr(tokstr))"
            else
                _token_error_descriptions[k]
            end
            emit_diagnostic(stream, tokrange, error=msg)
        end

        if error_kind != K"None"
            # Update the node with new error kind
            stream.output[i] = RawGreenNode(SyntaxHead(error_kind, EMPTY_FLAGS),
                                          node.byte_span, node.orig_kind)
        end

        fbyte = nbyte
    end
    sort!(stream.diagnostics, by=first_byte)
end

"""
    bump_split(stream, token_spec1, [token_spec2 ...])

Bump the next token, splitting it into several pieces

Tokens are defined by a number of `token_spec` of shape `(nbyte, kind, flags)`.
If all `nbyte` are positive, the sum must equal the token length. If one
`nbyte` is negative, that token is given `tok_len + nbyte` bytes and the sum of
all `nbyte` must equal zero.

This is a hack which helps resolves the occasional lexing ambiguity. For
example
* Whether .+ should be a single token or the composite (. +) which is used for
  standalone operators.
* Whether ... is splatting (most of the time) or three . tokens in import paths

TODO: Are these the only cases?  Can we replace this general utility with a
simpler one which only splits preceding dots?
"""
function bump_split(stream::ParseStream, split_spec::Vararg{Any, N}) where {N}
    tok = stream.lookahead[stream.lookahead_index]
    stream.lookahead_index += 1
    start_b = _next_byte(stream)
    toklen = tok.next_byte - start_b
    prev_b = start_b
    for (i, (nbyte, k, f)) in enumerate(split_spec)
        h = SyntaxHead(k, f)
        actual_nbyte = nbyte < 0 ? (toklen + nbyte) : nbyte
        orig_k = k == K"." ? K"." : kind(tok)
        node = RawGreenNode(h, actual_nbyte, orig_k)
        push!(stream.output, node)
        prev_b += actual_nbyte
        stream.next_byte += actual_nbyte
    end
    @assert tok.next_byte == prev_b
    stream.peek_count = 0
    return position(stream)
end

function peek_dotted_op_token(ps, allow_whitespace=false)
    # Peek the next token, but if it is a dot, peek the next one as well
    t = peek_token(ps)
    isdotted = kind(t) == K"."
    if isdotted
        t2 = peek_token(ps, 2)
        if !is_operator(t2) || (!allow_whitespace && preceding_whitespace(t2))
            isdotted = false
        else
            t = t2
        end
    end
    return (isdotted, t)
end

function bump_dotted(ps, isdot, flags=EMPTY_FLAGS; emit_dot_node=false, remap_kind=K"None")
    if isdot
        if emit_dot_node
            dotmark = position(ps)
            bump(ps, TRIVIA_FLAG) # TODO: NOTATION_FLAG
        else
            bump(ps, TRIVIA_FLAG) # TODO: NOTATION_FLAG
        end
    end
    pos = bump(ps, flags, remap_kind=remap_kind)
    isdot && emit_dot_node && (pos = emit(ps, dotmark, K"."))
    return pos
end
