# This file is a part of Julia. License is MIT: https://julialang.org/license

import Base.Checked: add_with_overflow, mul_with_overflow

## string to integer functions ##

"""
    parse(type, str, [base])

Parse a string as a number. For `Integer` types, a base can be specified
(the default is 10). For floating-point types, the string is parsed as a decimal
floating-point number.  `Complex` types are parsed from decimal strings
of the form `"R±Iim"` as a `Complex(R,I)` of the requested type; `i` or `j` can also be
used instead of `im`.  If the string does not contain a valid number, an error is raised.

```jldoctest
julia> parse(Int, "1234")
1234

julia> parse(Int, "1234", 5)
194

julia> parse(Int, "afc", 16)
2812

julia> parse(Float64, "1.2e-3")
0.0012
```
"""
parse(T::Type, str, base=Int)

function parse(::Type{T}, c::Char, base::Integer=36) where T<:Integer
    a::Int = (base <= 36 ? 10 : 36)
    2 <= base <= 62 || throw(ArgumentError("invalid base: base must be 2 ≤ base ≤ 62, got $base"))
    d = '0' <= c <= '9' ? c-'0'    :
        'A' <= c <= 'Z' ? c-'A'+10 :
        'a' <= c <= 'z' ? c-'a'+a  : throw(ArgumentError("invalid digit: $(repr(c))"))
    d < base || throw(ArgumentError("invalid base $base digit $(repr(c))"))
    convert(T, d)
end

function parseint_next(s::AbstractString, startpos::Int, endpos::Int)
    (0 < startpos <= endpos) || (return Char(0), 0, 0)
    j = startpos
    c, startpos = next(s,startpos)
    c, startpos, j
end

function parseint_preamble(signed::Bool, base::Int, s::AbstractString, startpos::Int, endpos::Int)
    c, i, j = parseint_next(s, startpos, endpos)

    while isspace(c)
        c, i, j = parseint_next(s,i,endpos)
    end
    (j == 0) && (return 0, 0, 0)

    sgn = 1
    if signed
        if c == '-' || c == '+'
            (c == '-') && (sgn = -1)
            c, i, j = parseint_next(s,i,endpos)
        end
    end

    while isspace(c)
        c, i, j = parseint_next(s,i,endpos)
    end
    (j == 0) && (return 0, 0, 0)

    if base == 0
        if c == '0' && !done(s,i)
            c, i = next(s,i)
            base = c=='b' ? 2 : c=='o' ? 8 : c=='x' ? 16 : 10
            if base != 10
                c, i, j = parseint_next(s,i,endpos)
            end
        else
            base = 10
        end
    end
    return sgn, base, j
end

function tryparse_internal(::Type{T}, s::AbstractString, startpos::Int, endpos::Int, base_::Integer, raise::Bool) where T<:Integer
    _n = Nullable{T}()
    sgn, base, i = parseint_preamble(T<:Signed, Int(base_), s, startpos, endpos)
    if sgn == 0 && base == 0 && i == 0
        raise && throw(ArgumentError("input string is empty or only contains whitespace"))
        return _n
    end
    if !(2 <= base <= 62)
        raise && throw(ArgumentError("invalid base: base must be 2 ≤ base ≤ 62, got $base"))
        return _n
    end
    if i == 0
        raise && throw(ArgumentError("premature end of integer: $(repr(SubString(s,startpos,endpos)))"))
        return _n
    end
    c, i = parseint_next(s,i,endpos)
    if i == 0
        raise && throw(ArgumentError("premature end of integer: $(repr(SubString(s,startpos,endpos)))"))
        return _n
    end

    base = convert(T,base)
    m::T = div(typemax(T)-base+1,base)
    n::T = 0
    a::Int = base <= 36 ? 10 : 36
    while n <= m
        d::T = '0' <= c <= '9' ? c-'0'    :
               'A' <= c <= 'Z' ? c-'A'+10 :
               'a' <= c <= 'z' ? c-'a'+a  : base
        if d >= base
            raise && throw(ArgumentError("invalid base $base digit $(repr(c)) in $(repr(SubString(s,startpos,endpos)))"))
            return _n
        end
        n *= base
        n += d
        if i > endpos
            n *= sgn
            return Nullable{T}(n)
        end
        c, i = next(s,i)
        isspace(c) && break
    end
    (T <: Signed) && (n *= sgn)
    while !isspace(c)
        d::T = '0' <= c <= '9' ? c-'0'    :
        'A' <= c <= 'Z' ? c-'A'+10 :
            'a' <= c <= 'z' ? c-'a'+a  : base
        if d >= base
            raise && throw(ArgumentError("invalid base $base digit $(repr(c)) in $(repr(SubString(s,startpos,endpos)))"))
            return _n
        end
        (T <: Signed) && (d *= sgn)

        n, ov_mul = mul_with_overflow(n, base)
        n, ov_add = add_with_overflow(n, d)
        if ov_mul | ov_add
            raise && throw(OverflowError("overflow parsing $(repr(SubString(s,startpos,endpos)))"))
            return _n
        end
        (i > endpos) && return Nullable{T}(n)
        c, i = next(s,i)
    end
    while i <= endpos
        c, i = next(s,i)
        if !isspace(c)
            raise && throw(ArgumentError("extra characters after whitespace in $(repr(SubString(s,startpos,endpos)))"))
            return _n
        end
    end
    return Nullable{T}(n)
end

function tryparse_internal(::Type{Bool}, sbuff::Union{String,SubString},
        startpos::Int, endpos::Int, base::Integer, raise::Bool)
    if isempty(sbuff)
        raise && throw(ArgumentError("input string is empty"))
        return Nullable{Bool}()
    end

    orig_start = startpos
    orig_end   = endpos

    # Ignore leading and trailing whitespace
    while isspace(sbuff[startpos]) && startpos <= endpos
        startpos = nextind(sbuff, startpos)
    end
    while isspace(sbuff[endpos]) && endpos >= startpos
        endpos = prevind(sbuff, endpos)
    end

    len = endpos - startpos + 1
    p   = pointer(sbuff) + startpos - 1
    @gc_preserve sbuff begin
        (len == 4) && (0 == ccall(:memcmp, Int32, (Ptr{UInt8}, Ptr{UInt8}, UInt),
                                  p, "true", 4)) && (return Nullable(true))
        (len == 5) && (0 == ccall(:memcmp, Int32, (Ptr{UInt8}, Ptr{UInt8}, UInt),
                                  p, "false", 5)) && (return Nullable(false))
    end

    if raise
        substr = SubString(sbuff, orig_start, orig_end) # show input string in the error to avoid confusion
        if all(isspace, substr)
            throw(ArgumentError("input string only contains whitespace"))
        else
            throw(ArgumentError("invalid Bool representation: $(repr(substr))"))
        end
    end
    return Nullable{Bool}()
end

@inline function check_valid_base(base)
    if 2 <= base <= 62
        return base
    end
    throw(ArgumentError("invalid base: base must be 2 ≤ base ≤ 62, got $base"))
end

"""
    tryparse(type, str, [base])

Like [`parse`](@ref), but returns a [`Nullable`](@ref) of the requested type. The result
will be null if the string does not contain a valid number.
"""
tryparse(::Type{T}, s::AbstractString, base::Integer) where {T<:Integer} =
    tryparse_internal(T, s, start(s), endof(s), check_valid_base(base), false)
tryparse(::Type{T}, s::AbstractString) where {T<:Integer} =
    tryparse_internal(T, s, start(s), endof(s), 0, false)

function parse(::Type{T}, s::AbstractString, base::Integer) where T<:Integer
    get(tryparse_internal(T, s, start(s), endof(s), check_valid_base(base), true))
end

function parse(::Type{T}, s::AbstractString) where T<:Integer
    get(tryparse_internal(T, s, start(s), endof(s), 0, true)) # Zero means, "figure it out"
end

## string to float functions ##

tryparse(::Type{Float64}, s::String) = ccall(:jl_try_substrtod, Nullable{Float64}, (Ptr{UInt8},Csize_t,Csize_t), s, 0, sizeof(s))
tryparse(::Type{Float64}, s::SubString{String}) = ccall(:jl_try_substrtod, Nullable{Float64}, (Ptr{UInt8},Csize_t,Csize_t), s.string, s.offset, s.endof)

tryparse(::Type{Float32}, s::String) = ccall(:jl_try_substrtof, Nullable{Float32}, (Ptr{UInt8},Csize_t,Csize_t), s, 0, sizeof(s))
tryparse(::Type{Float32}, s::SubString{String}) = ccall(:jl_try_substrtof, Nullable{Float32}, (Ptr{UInt8},Csize_t,Csize_t), s.string, s.offset, s.endof)

tryparse(::Type{T}, s::AbstractString) where {T<:Union{Float32,Float64}} = tryparse(T, String(s))

tryparse(::Type{Float16}, s::AbstractString) = convert(Nullable{Float16}, tryparse(Float32, s))


## string to complex functions ##

function tryparse(::Type{Complex{T}}, s::Union{String,SubString{String}}) where {T<:Real}
    # skip initial whitespace
    i = start(s)
    e = endof(s)
    while i ≤ e && isspace(s[i])
        i = nextind(s, i)
    end
    i > e && return Nullable{Complex{T}}()

    # find index of ± separating real/imaginary parts (if any)
    i₊ = search(s, ('+','-'), i)
    if i₊ == i # leading ± sign
        i₊ = search(s, ('+','-'), i₊+1)
    end
    if i₊ != 0 && s[i₊-1] in ('e','E') # exponent sign
        i₊ = search(s, ('+','-'), i₊+1)
    end
    if i₊ == 0 # purely real value
        return Nullable{Complex{T}}(tryparse(T, s))
    end

    # find trailing im/i/j
    iᵢ = rsearch(s, ('m','i','j'), e)
    iᵢ < i₊ && return Nullable{Complex{T}}()
    if s[iᵢ] == 'm' # im
        iᵢ -= 1
        s[iᵢ] == 'i' || return Nullable{Complex{T}}()
    end

    # parse real part
    re = tryparse(T, SubString(s, i, i₊-1))
    isnull(re) && return Nullable{Complex{T}}()

    # parse imaginary part
    im = tryparse(T, SubString(s, i₊+1, iᵢ-1))
    isnull(im) && return Nullable{Complex{T}}()

    return Nullable{Complex{T}}(Complex{T}(get(re), s[i₊]=='-' ? -get(im) : get(im)))
end

# the ±1 indexing above for ascii chars is specific to String, so convert:
tryparse(T::Type{<:Complex}, s::AbstractString) = tryparse(T, String(s))

function parse(::Type{T}, s::AbstractString) where T<:Union{AbstractFloat,Complex}
    result = tryparse(T, s)
    if isnull(result)
        throw(ArgumentError("cannot parse $(repr(s)) as $T"))
    end
    return unsafe_get(result)
end
