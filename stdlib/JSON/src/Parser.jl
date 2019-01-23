module Parser  # JSON

using Mmap
using ..Common

include("pushvector.jl")

"""
Like `isspace`, but work on bytes and includes only the four whitespace
characters defined by the JSON standard: space, tab, line feed, and carriage
return.
"""
isjsonspace(b::UInt8) = b == SPACE || b == TAB || b == NEWLINE || b == RETURN

"""
Like `isdigit`, but for bytes.
"""
isjsondigit(b::UInt8) = DIGIT_ZERO ≤ b ≤ DIGIT_NINE

abstract type ParserState end

mutable struct MemoryParserState <: ParserState
    utf8::String
    s::Int
end

# it is convenient to access MemoryParserState like a Vector{UInt8} to avoid copies
Base.@propagate_inbounds Base.getindex(state::MemoryParserState, i::Int) = codeunit(state.utf8, i)
Base.length(state::MemoryParserState) = sizeof(state.utf8)
Base.unsafe_convert(::Type{Ptr{UInt8}}, state::MemoryParserState) = Base.unsafe_convert(Ptr{UInt8}, state.utf8)

mutable struct StreamingParserState{T <: IO} <: ParserState
    io::T
    cur::UInt8
    used::Bool
    utf8array::PushVector{UInt8, Vector{UInt8}}
end
StreamingParserState(io::IO) = StreamingParserState(io, 0x00, true, PushVector{UInt8}())

struct ParserContext{DictType, IntType} end

"""
Return the byte at the current position of the `ParserState`. If there is no
byte (that is, the `ParserState` is done), then an error is thrown that the
input ended unexpectedly.
"""
@inline function byteat(ps::MemoryParserState)
    @inbounds if hasmore(ps)
        return ps[ps.s]
    else
        _error(E_UNEXPECTED_EOF, ps)
    end
end

@inline function byteat(ps::StreamingParserState)
    if ps.used
        ps.used = false
        if eof(ps.io)
            _error(E_UNEXPECTED_EOF, ps)
        else
            ps.cur = read(ps.io, UInt8)
        end
    end
    ps.cur
end

"""
Like `byteat`, but with no special bounds check and error message. Useful when
a current byte is known to exist.
"""
@inline current(ps::MemoryParserState) = ps[ps.s]
@inline current(ps::StreamingParserState) = byteat(ps)

"""
Require the current byte of the `ParserState` to be the given byte, and then
skip past that byte. Otherwise, an error is thrown.
"""
@inline function skip!(ps::ParserState, c::UInt8)
    if byteat(ps) == c
        incr!(ps)
    else
        _error_expected_char(c, ps)
    end
end
@noinline _error_expected_char(c, ps) = _error("Expected '$(Char(c))' here", ps)

function skip!(ps::ParserState, cs::UInt8...)
    for c in cs
        skip!(ps, c)
    end
end

"""
Move the `ParserState` to the next byte.
"""
@inline incr!(ps::MemoryParserState) = (ps.s += 1)
@inline incr!(ps::StreamingParserState) = (ps.used = true)

"""
Move the `ParserState` to the next byte, and return the value at the byte before
the advancement. If the `ParserState` is already done, then throw an error.
"""
@inline advance!(ps::ParserState) = (b = byteat(ps); incr!(ps); b)

"""
Return `true` if there is a current byte, and `false` if all bytes have been
exausted.
"""
@inline hasmore(ps::MemoryParserState) = ps.s ≤ length(ps)
@inline hasmore(ps::StreamingParserState) = true  # no more now ≠ no more ever

"""
Remove as many whitespace bytes as possible from the `ParserState` starting from
the current byte.
"""
@inline function chomp_space!(ps::ParserState)
    @inbounds while hasmore(ps) && isjsonspace(current(ps))
        incr!(ps)
    end
end


# Used for line counts
function _count_before(haystack::AbstractString, needle::Char, _end::Int)
    count = 0
    for (i,c) in enumerate(haystack)
        i >= _end && return count
        count += c == needle
    end
    return count
end


# Throws an error message with an indicator to the source
@noinline function _error(message::AbstractString, ps::MemoryParserState)
    orig = ps.utf8
    lines = _count_before(orig, '\n', ps.s)
    # Replace all special multi-line/multi-space characters with a space.
    strnl = replace(orig, r"[\b\f\n\r\t\s]" => " ")
    li = (ps.s > 20) ? ps.s - 9 : 1 # Left index
    ri = min(lastindex(orig), ps.s + 20)       # Right index
    error(message *
      "\nLine: " * string(lines) *
      "\nAround: ..." * strnl[li:ri] * "..." *
      "\n           " * (" " ^ (ps.s - li)) * "^\n"
    )
end

@noinline function _error(message::AbstractString, ps::StreamingParserState)
    error("$message\n ...when parsing byte with value '$(current(ps))'")
end

# PARSING

"""
Given a `ParserState`, after possibly any amount of whitespace, return the next
parseable value.
"""
function parse_value(pc::ParserContext, ps::ParserState)
    chomp_space!(ps)

    @inbounds byte = byteat(ps)
    if byte == STRING_DELIM
        parse_string(ps)
    elseif isjsondigit(byte) || byte == MINUS_SIGN
        parse_number(pc, ps)
    elseif byte == OBJECT_BEGIN
        parse_object(pc, ps)
    elseif byte == ARRAY_BEGIN
        parse_array(pc, ps)
    else
        parse_jsconstant(ps::ParserState)
    end
end

function parse_jsconstant(ps::ParserState)
    c = advance!(ps)
    if c == LATIN_T      # true
        skip!(ps, LATIN_R, LATIN_U, LATIN_E)
        true
    elseif c == LATIN_F  # false
        skip!(ps, LATIN_A, LATIN_L, LATIN_S, LATIN_E)
        false
    elseif c == LATIN_N  # null
        skip!(ps, LATIN_U, LATIN_L, LATIN_L)
        nothing
    else
        _error(E_UNEXPECTED_CHAR, ps)
    end
end

function parse_array(pc::ParserContext, ps::ParserState)
    result = Any[]
    @inbounds incr!(ps)  # Skip over opening '['
    chomp_space!(ps)
    if byteat(ps) ≠ ARRAY_END  # special case for empty array
        @inbounds while true
            push!(result, parse_value(pc, ps))
            chomp_space!(ps)
            byteat(ps) == ARRAY_END && break
            skip!(ps, DELIMITER)
        end
    end

    @inbounds incr!(ps)
    result
end


function parse_object(pc::ParserContext{DictType, <:Real}, ps::ParserState) where DictType
    obj = DictType()
    keyT = keytype(typeof(obj))

    incr!(ps)  # Skip over opening '{'
    chomp_space!(ps)
    if byteat(ps) ≠ OBJECT_END  # special case for empty object
        @inbounds while true
            # Read key
            chomp_space!(ps)
            byteat(ps) == STRING_DELIM || _error(E_BAD_KEY, ps)
            key = parse_string(ps)
            chomp_space!(ps)
            skip!(ps, SEPARATOR)
            # Read value
            value = parse_value(pc, ps)
            chomp_space!(ps)
            obj[keyT === Symbol ? Symbol(key) : convert(keyT, key)] = value
            byteat(ps) == OBJECT_END && break
            skip!(ps, DELIMITER)
        end
    end

    incr!(ps)
    obj
end


utf16_is_surrogate(c::UInt16) = (c & 0xf800) == 0xd800
utf16_get_supplementary(lead::UInt16, trail::UInt16) = Char(UInt32(lead-0xd7f7)<<10 + trail)

function read_four_hex_digits!(ps::ParserState)
    local n::UInt16 = 0

    for _ in 1:4
        b = advance!(ps)
        n = n << 4 + if isjsondigit(b)
            b - DIGIT_ZERO
        elseif LATIN_A ≤ b ≤ LATIN_F
            b - (LATIN_A - UInt8(10))
        elseif LATIN_UPPER_A ≤ b ≤ LATIN_UPPER_F
            b - (LATIN_UPPER_A - UInt8(10))
        else
            _error(E_BAD_ESCAPE, ps)
        end
    end

    n
end

function read_unicode_escape!(ps)
    u1 = read_four_hex_digits!(ps)
    if utf16_is_surrogate(u1)
        skip!(ps, BACKSLASH)
        skip!(ps, LATIN_U)
        u2 = read_four_hex_digits!(ps)
        utf16_get_supplementary(u1, u2)
    else
        Char(u1)
    end
end

function parse_string(ps::ParserState)
    b = IOBuffer()
    incr!(ps)  # skip opening quote
    while true
        c = advance!(ps)

        if c == BACKSLASH
            c = advance!(ps)
            if c == LATIN_U  # Unicode escape
                write(b, read_unicode_escape!(ps))
            else
                c = get(ESCAPES, c, 0x00)
                c == 0x00 && _error(E_BAD_ESCAPE, ps)
                write(b, c)
            end
            continue
        elseif c < SPACE
            _error(E_BAD_CONTROL, ps)
        elseif c == STRING_DELIM
            return String(take!(b))
        end

        write(b, c)
    end
end

"""
Return `true` if the given bytes vector, starting at `from` and ending at `to`,
has a leading zero.
"""
function hasleadingzero(bytes, from::Int, to::Int)
    c = bytes[from]
    from + 1 < to && c == UInt8('-') &&
            bytes[from + 1] == DIGIT_ZERO && isjsondigit(bytes[from + 2]) ||
    from < to && to > from + 1 && c == DIGIT_ZERO &&
            isjsondigit(bytes[from + 1])
end

"""
Parse a float from the given bytes vector, starting at `from` and ending at the
byte before `to`. Bytes enclosed should all be ASCII characters.
"""
function float_from_bytes(bytes, from::Int, to::Int)
    # The ccall is not ideal (Base.tryparse would be better), but it actually
    # makes an 2× difference to performance
    hasvalue, val = ccall(:jl_try_substrtod, Tuple{Bool, Float64},
            (Ptr{UInt8}, Csize_t, Csize_t), bytes, from - 1, to - from + 1)
    hasvalue ? val : nothing
end

"""
Parse an integer from the given bytes vector, starting at `from` and ending at
the byte before `to`. Bytes enclosed should all be ASCII characters.
"""
function int_from_bytes(pc::ParserContext{<:Any,IntType},
                        ps::ParserState,
                        bytes,
                        from::Int,
                        to::Int) where IntType <: Real
    @inbounds isnegative = bytes[from] == MINUS_SIGN ? (from += 1; true) : false
    num = IntType(0)
    @inbounds for i in from:to
        c = bytes[i]
        dig = c - DIGIT_ZERO
        if dig < 0x10
            num = IntType(10) * num + IntType(dig)
        else
            _error(E_BAD_NUMBER, ps)
        end
    end
    ifelse(isnegative, -num, num)
end

function number_from_bytes(pc::ParserContext,
                           ps::ParserState,
                           isint::Bool,
                           bytes,
                           from::Int,
                           to::Int)
    @inbounds if hasleadingzero(bytes, from, to)
        _error(E_LEADING_ZERO, ps)
    end

    if isint
        @inbounds if to == from && bytes[from] == MINUS_SIGN
            _error(E_BAD_NUMBER, ps)
        end
        int_from_bytes(pc, ps, bytes, from, to)
    else
        res = float_from_bytes(bytes, from, to)
        res === nothing ? _error(E_BAD_NUMBER, ps) : res
    end
end


function parse_number(pc::ParserContext, ps::ParserState)
    # Determine the end of the floating point by skipping past ASCII values
    # 0-9, +, -, e, E, and .
    number = ps.utf8array
    isint = true

    @inbounds while hasmore(ps)
        c = current(ps)

        if isjsondigit(c) || c == MINUS_SIGN
            push!(number, UInt8(c))
        elseif c in (PLUS_SIGN, LATIN_E, LATIN_UPPER_E, DECIMAL_POINT)
            push!(number, UInt8(c))
            isint = false
        else
            break
        end

        incr!(ps)
    end

    v = number_from_bytes(pc, ps, isint, number, 1, length(number))
    resize!(number, 0)
    return v
end

unparameterize_type(x) = x # Fallback for nontypes -- functions etc
function unparameterize_type(T::Type)
    candidate = typeintersect(T, AbstractDict{String, Any})
    candidate <: Union{} ? T : candidate
end

# Workaround for slow dynamic dispatch for creating objects
const DEFAULT_PARSERCONTEXT = ParserContext{Dict{String, Any}, Int64}()
function _get_parsercontext(dicttype, inttype)
    if dicttype == Dict{String, Any} && inttype == Int64
        DEFAULT_PARSERCONTEXT
    else
        ParserContext{unparameterize_type(dicttype), inttype}.instance
    end
end

function parse(str::AbstractString;
               dicttype=Dict{String,Any},
               inttype::Type{<:Real}=Int64)
    pc = _get_parsercontext(dicttype, inttype)
    ps = MemoryParserState(str, 1)
    v = parse_value(pc, ps)
    chomp_space!(ps)
    if hasmore(ps)
        _error(E_EXPECTED_EOF, ps)
    end
    v
end

function parse(io::IO;
               dicttype=Dict{String,Any},
               inttype::Type{<:Real}=Int64)
    pc = _get_parsercontext(dicttype, inttype)
    ps = StreamingParserState(io)
    parse_value(pc, ps)
end

function parsefile(filename::AbstractString;
                   dicttype=Dict{String, Any},
                   inttype::Type{<:Real}=Int64,
                   use_mmap=true)
    sz = filesize(filename)
    open(filename) do io
        s = use_mmap ? String(Mmap.mmap(io, Vector{UInt8}, sz)) : read(io, String)
        parse(s; dicttype=dicttype, inttype=inttype)
    end
end

# Efficient implementations of some of the above for in-memory parsing
include("specialized.jl")

end  # module Parser
