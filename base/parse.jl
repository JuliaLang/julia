# This file is a part of Julia. License is MIT: https://julialang.org/license

## string to integer functions ##

"""
    parse(type, str; base)

Parse a string as a number. For `Integer` types, a base can be specified
(the default is 10). For floating-point types, the string is parsed as a decimal
floating-point number.  `Complex` types are parsed from decimal strings
of the form `"R±Iim"` as a `Complex(R,I)` of the requested type; `"i"` or `"j"` can also be
used instead of `"im"`, and `"R"` or `"Iim"` are also permitted.
If the string does not contain a valid number, an error is raised.

!!! compat "Julia 1.1"
    `parse(Bool, str)` requires at least Julia 1.1.

# Examples
```jldoctest
julia> parse(Int, "1234")
1234

julia> parse(Int, "1234", base = 5)
194

julia> parse(Int, "afc", base = 16)
2812

julia> parse(Float64, "1.2e-3")
0.0012

julia> parse(Complex{Float64}, "3.2e-1 + 4.5im")
0.32 + 4.5im
```
"""
parse(T::Type, str; base = Int)
parse(::Type{Union{}}, slurp...; kwargs...) = error("cannot parse a value as Union{}")

function parse(::Type{T}, c::AbstractChar; base::Integer = 10) where T<:Integer
    a::Int = (base <= 36 ? 10 : 36)
    2 <= base <= 62 || throw(ArgumentError("invalid base: base must be 2 ≤ base ≤ 62, got $base"))
    d = '0' <= c <= '9' ? c-'0'    :
        'A' <= c <= 'Z' ? c-'A'+10 :
        'a' <= c <= 'z' ? c-'a'+a  : throw(ArgumentError("invalid digit: $(repr(c))"))
    d < base || throw(ArgumentError("invalid base $base digit $(repr(c))"))
    convert(T, d)
end

# Base's integer grammar, on the UTF-8 bytes of a string:
#     [whitespace] [sign] [whitespace] [0x | 0o | 0b] digits [whitespace]
# A sign is only accepted for signed types, and a radix prefix only when no base is given.

isspace_ascii(b::UInt8) = (b == UInt8(' ')) | (UInt8('\t') <= b <= UInt8('\r'))

# Whitespace beyond ASCII is rare; decoding it stays out of the inlined loops below.
# The index after the character at `i` of `s` if it is whitespace, else `i`.
@noinline function skipspacechar(s::UTF8String, i::Int)
    c, next = iterate(s, i)::Tuple{Char, Int}
    return isspace(c) ? next : i
end
# The index before the character ending at `j` of `s` if it is whitespace starting at or
# after `i`, else `j`.
@noinline function skipspacechar_back(s::UTF8String, i::Int, j::Int)
    k = thisind(s, j)
    return k >= i && isspace(s[k]) ? k - 1 : j
end

# The first index in `i:j` of `s` that does not start a whitespace character, or `j + 1`.
# `i:j` must be in bounds, as for the functions below.
@inline function skipspace(s::UTF8String, i::Int, j::Int)
    while i <= j
        b = @inbounds codeunit(s, i)
        if isspace_ascii(b)
            i += 1
        elseif b >= 0x80 && (next = skipspacechar(s, i)) > i
            i = next
        else
            break
        end
    end
    return i
end

# The last index in `i:j` of `s` before any trailing whitespace, or `i - 1`.
@inline function skipspace_back(s::UTF8String, i::Int, j::Int)
    while j >= i
        b = @inbounds codeunit(s, j)
        if isspace_ascii(b)
            j -= 1
        elseif b >= 0x80 && (prev = skipspacechar_back(s, i, j)) < j
            j = prev
        else
            break
        end
    end
    return j
end

# Parse the grammar before the digits in the bytes `i:j` of `s`. Returns the sign, the base
# (from the prefix, or 10, if `base` is 0) and the index of the first digit; `(0, 0, 0)` if
# only whitespace and a sign remain, and a zero index if a prefix has nothing after it.
@inline function parseint_preamble(signed::Bool, base::Int, s::UTF8String, i::Int, j::Int)
    i = skipspace(s, i, j)
    i > j && return 0, 0, 0
    sgn = 1
    if signed
        c = @inbounds codeunit(s, i)
        if c == UInt8('-') || c == UInt8('+')
            c == UInt8('-') && (sgn = -1)
            i = skipspace(s, i + 1, j)
            i > j && return 0, 0, 0
        end
    end
    if base == 0
        base = 10
        if i < j && @inbounds(codeunit(s, i)) == UInt8('0')
            c = @inbounds codeunit(s, i + 1)
            base = c == UInt8('b') ? 2 : c == UInt8('o') ? 8 : c == UInt8('x') ? 16 : 10
            base == 10 || (i = i + 2 <= j ? i + 2 : 0)
        end
    end
    return sgn, base, i
end

# Parse the bytes `i:j` of `s`, which must be in bounds, with the grammar above. Returns the
# value, or `nothing` (or throws, if `raise`) when the bytes are not a valid `T`.
@inline function parseint_utf8(::Type{T}, s::UTF8String, i::Int, j::Int, base::Int, raise::Bool) where {T<:Integer}
    sgn, b, d = parseint_preamble(T <: Signed, base, s, i, j)
    if sgn != 0 && 2 <= b <= 62 && d != 0
        n = Parsers.parseint(T, codeunits(s), d, skipspace_back(s, d, j), b, sgn < 0)
        n isa Parsers.ParseFailure || return n
    end
    raise && throw_parseint_error(T, s, i, j, sgn, b, d)
    return nothing
end

# Throw the error for the bytes `i:j` of `s` that `parseint_utf8` rejected, given the
# preamble's results. In the digits, it is the first problem from the left: a non-digit,
# overflow, or anything but whitespace after whitespace.
@noinline function throw_parseint_error(::Type{T}, s::UTF8String, i::Int, j::Int,
                                        sgn::Int, base::Int, d::Int) where {T}
    sgn == 0 && throw(ArgumentError("input string is empty or only contains whitespace"))
    2 <= base <= 62 ||
        throw(ArgumentError(LazyString("invalid base: base must be 2 ≤ base ≤ 62, got ", base)))
    str = repr(SubString(s, i, thisind(s, j)))
    d == 0 && throw(ArgumentError("premature end of integer: $str"))
    k = skipspace_back(s, d, j)
    p = d
    while p <= k && Parsers.digitvalue(codeunit(s, p), base) < base
        p += 1
    end
    if p > d && Parsers.parseint(T, codeunits(s), d, p - 1, base, sgn < 0) === Parsers.OVERFLOW
        throw(OverflowError("overflow parsing $str"))
    end
    c = s[p]
    p > d && isspace(c) && throw(ArgumentError("extra characters after whitespace in $str"))
    throw(ArgumentError("invalid base $base digit $(repr(c)) in $str"))
end

function tryparse_internal(::Type{T}, s::AbstractString, startpos::Int, endpos::Int, base::Integer, raise::Bool) where T<:Integer
    if s isa UTF8String
        # the last byte of the span, which is empty if `startpos` is not positive
        j = 1 <= startpos <= endpos ? nextind(s, endpos) - 1 : startpos - 1
        return parseint_utf8(T, s, startpos, j, Int(base), raise)
    end
    # other string types are parsed from a UTF-8 copy
    str = 1 <= startpos <= endpos ? String(SubString(s, startpos, endpos)) : ""
    return parseint_utf8(T, str, 1, ncodeunits(str), Int(base), raise)
end

function tryparse_internal(::Type{Bool}, sbuff::AbstractString,
        startpos::Int, endpos::Int, base::Integer, raise::Bool)
    if isempty(sbuff)
        raise && throw(ArgumentError("input string is empty"))
        return nothing
    end

    if isnumeric(sbuff[1])
        intres = tryparse_internal(UInt8, sbuff, startpos, endpos, base, false)
        (intres == 1) && return true
        (intres == 0) && return false
        raise && throw(ArgumentError("invalid Bool representation: $(repr(sbuff))"))
    end

    orig_start = startpos
    orig_end   = endpos

    # Ignore leading and trailing whitespace
    while startpos <= endpos && isspace(sbuff[startpos])
        startpos = nextind(sbuff, startpos)
    end
    while endpos >= startpos && isspace(sbuff[endpos])
        endpos = prevind(sbuff, endpos)
    end

    len = endpos - startpos + 1
    if sbuff isa Union{String, SubString{String}}
        p = pointer(sbuff) + startpos - 1
        truestr = "true"
        falsestr = "false"
        GC.@preserve sbuff truestr falsestr begin
            (len == 4) && (0 == memcmp(p, unsafe_convert(Ptr{UInt8}, truestr), 4)) && (return true)
            (len == 5) && (0 == memcmp(p, unsafe_convert(Ptr{UInt8}, falsestr), 5)) && (return false)
        end
    else
        (len == 4) && (SubString(sbuff, startpos:startpos+3) == "true") && (return true)
        (len == 5) && (SubString(sbuff, startpos:startpos+4) == "false") && (return false)
    end

    if raise
        substr = SubString(sbuff, orig_start, orig_end) # show input string in the error to avoid confusion
        if all(isspace, substr)
            throw(ArgumentError("input string only contains whitespace"))
        else
            throw(ArgumentError("invalid Bool representation: $(repr(substr))"))
        end
    end
    return nothing
end

@inline function check_valid_base(base)
    if 2 <= base <= 62
        return base
    end
    throw(ArgumentError("invalid base: base must be 2 ≤ base ≤ 62, got $base"))
end

"""
    tryparse(type, str; base)

Like [`parse`](@ref), but returns either a value of the requested type,
or [`nothing`](@ref) if the string does not contain a valid number.
"""
tryparse(::Type{T}, s::AbstractString; base::Union{Nothing,Integer} = nothing) where {T<:Integer} =
    parseint_string(T, s, base, false)

function parse(::Type{T}, s::AbstractString; base::Union{Nothing,Integer} = nothing) where {T<:Integer}
    v = parseint_string(T, s, base, true)
    v === nothing && error("should not happen")
    convert(T, v)
end

# Parse all of `s`. Base's fixed-width types parse UTF-8 strings directly; other integer
# types, such as `BigInt`, can have their own `tryparse_internal` methods.
@inline function parseint_string(::Type{T}, s::AbstractString, base, raise::Bool) where {T<:Integer}
    b = base === nothing ? 0 : Int(check_valid_base(base))  # 0 takes the base from a prefix
    T <: BitInteger && s isa UTF8String && return parseint_utf8(T, s, 1, ncodeunits(s), b, raise)
    return tryparse_internal(T, s, firstindex(s), lastindex(s), b, raise)
end
tryparse(::Type{Union{}}, slurp...; kwargs...) = error("cannot parse a value as Union{}")

## string to float functions ##

function tryparse(::Type{Float64}, s::DenseUTF8String)
    hasvalue, val = ccall(:jl_try_substrtod, Tuple{Bool, Float64},
                          (Ptr{UInt8},Csize_t,Csize_t), s, 0, sizeof(s) % UInt)
    hasvalue ? val : nothing
end
function tryparse_internal(::Type{Float64}, s::DenseUTF8String, startpos::Int, endpos::Int)
    hasvalue, val = ccall(:jl_try_substrtod, Tuple{Bool, Float64},
                          (Ptr{UInt8},Csize_t,Csize_t), s, startpos-1, endpos-startpos+1)
    hasvalue ? val : nothing
end
function tryparse(::Type{Float32}, s::DenseUTF8String)
    hasvalue, val = ccall(:jl_try_substrtof, Tuple{Bool, Float32},
                          (Ptr{UInt8},Csize_t,Csize_t), s, 0, sizeof(s) % UInt)
    hasvalue ? val : nothing
end
function tryparse_internal(::Type{Float32}, s::DenseUTF8String, startpos::Int, endpos::Int)
    hasvalue, val = ccall(:jl_try_substrtof, Tuple{Bool, Float32},
                          (Ptr{UInt8},Csize_t,Csize_t), s, startpos-1, endpos-startpos+1)
    hasvalue ? val : nothing
end

tryparse(::Type{T}, s::AbstractString) where {T<:Union{Float32,Float64}} = tryparse(T, String(s)::String)
tryparse(::Type{Float16}, s::AbstractString) =
    convert(Union{Float16, Nothing}, tryparse(Float32, s))
tryparse_internal(::Type{Float16}, s::AbstractString, startpos::Int, endpos::Int) =
    convert(Union{Float16, Nothing}, tryparse_internal(Float32, s, startpos, endpos))

## string to complex functions ##

function tryparse_internal(::Type{Complex{T}}, s::DenseUTF8String, i::Int, e::Int, raise::Bool) where {T<:Real}
    # skip initial whitespace
    while i ≤ e && isspace(s[i])
        i = nextind(s, i)
    end
    if i > e
        raise && throw(ArgumentError("input string is empty or only contains whitespace"))
        return nothing
    end

    # find index of ± separating real/imaginary parts (if any)
    i₊ = something(findnext(in(('+','-')), s, i), 0)
    if i₊ == i # leading ± sign
        i₊ = something(findnext(in(('+','-')), s, i₊+1), 0)
    end
    if i₊ != 0 && s[prevind(s, i₊)] in ('e','E') # exponent sign
        i₊ = something(findnext(in(('+','-')), s, i₊+1), 0)
    end

    # find trailing im/i/j
    iᵢ = something(findprev(in(('m','i','j')), s, e), 0)
    if iᵢ > 0 && s[iᵢ] == 'm' # im
        iᵢ = prevind(s, iᵢ)
        if s[iᵢ] != 'i'
            raise && throw(ArgumentError("expected trailing \"im\", found only \"m\""))
            return nothing
        end
    end

    if i₊ == 0 # purely real or imaginary value
        if iᵢ > i && !(iᵢ == i+1 && s[i] in ('+','-')) # purely imaginary (not "±inf")
            x = tryparse_internal(T, s, i, prevind(s, iᵢ), raise)
            x === nothing && return nothing
            return Complex{T}(zero(x),x)
        else # purely real
            x = tryparse_internal(T, s, i, e, raise)
            x === nothing && return nothing
            return Complex{T}(x)
        end
    end

    if iᵢ < i₊
        raise && throw(ArgumentError("missing imaginary unit"))
        return nothing # no imaginary part
    end

    # parse real part
    re = tryparse_internal(T, s, i, prevind(s, i₊), raise)
    re === nothing && return nothing

    # parse imaginary part
    im = tryparse_internal(T, s, i₊+1, prevind(s, iᵢ), raise)
    im === nothing && return nothing

    return Complex{T}(re, s[i₊]=='-' ? -im : im)
end

# the ±1 indexing above for ascii chars is specific to String, so convert:
tryparse_internal(T::Type{Complex{S}}, s::AbstractString, i::Int, e::Int, raise::Bool) where S<:Real =
    tryparse_internal(T, String(s), i, e, raise)

# fallback methods for tryparse_internal
tryparse_internal(::Type{T}, s::AbstractString, startpos::Int, endpos::Int) where T<:Real =
    startpos == firstindex(s) && endpos == lastindex(s) ? tryparse(T, s) : tryparse(T, SubString(s, startpos, endpos))
function tryparse_internal(::Type{T}, s::AbstractString, startpos::Int, endpos::Int, raise::Bool) where T<:Real
    result = tryparse_internal(T, s, startpos, endpos)
    if raise && result === nothing
        _parse_failure(T, s, startpos, endpos)
    end
    return result
end
function tryparse_internal(::Type{T}, s::AbstractString, raise::Bool; kwargs...) where T<:Real
    result = tryparse(T, s; kwargs...)
    if raise && result === nothing
        _parse_failure(T, s)
    end
    return result
end
@noinline _parse_failure(T, s::AbstractString, startpos = firstindex(s), endpos = lastindex(s)) =
    throw(ArgumentError(LazyString("cannot parse ", repr(s[startpos:endpos]), " as ", T)))

tryparse_internal(::Type{T}, s::AbstractString, startpos::Int, endpos::Int, raise::Bool) where T<:Integer =
    tryparse_internal(T, s, startpos, endpos, 10, raise)

parse(::Type{T}, s::AbstractString; kwargs...) where T<:Real =
    convert(T, tryparse_internal(T, s, true; kwargs...))
parse(::Type{T}, s::AbstractString) where T<:Complex =
    convert(T, tryparse_internal(T, s, firstindex(s), lastindex(s), true))

tryparse(T::Type{Complex{S}}, s::AbstractString) where S<:Real =
    tryparse_internal(T, s, firstindex(s), lastindex(s), false)
