# This file is a part of Julia. License is MIT: https://julialang.org/license
#module Parse

#using Base.Checked: add_with_overflow, mul_with_overflow
#import Base: parse, tryparse

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
parse(T::Type, str; base::Int)


"""
    tryparse(type, str; base)

Like [`parse`](@ref), but returns either a value of the requested type,
or [`nothing`](@ref) if the string does not contain a valid number.

```jldoctest
julia> typeof(tryparse(Int, "12three4"))
Nothing

julia> tryparse(Int, "1234")
1234
```
"""
tryparse(T::Type, str; base::Int)


##########
# errors #
##########

@noinline throw_parse_failure(T, s::AbstractString) =
    throw(ArgumentError("cannot parse $(repr(s)) as $T"))
@noinline throw_only_whitespace() =
    throw(ArgumentError("input string is empty or only contains whitespace"))
@noinline throw_characters_after_whitespace(s) =
    throw(ArgumentError("extra characters after whitespace in $(repr(s))"))
@noinline throw_invalid_base_digit(base, c, s) =
    throw(ArgumentError("invalid base $base digit $(repr(c)) in $(repr(s))"))
@noinline throw_overflow_parsing(s) =
    throw(OverflowError("overflow parsing $(repr(s))"))
@noinline throw_extra_characters_whitespace(s) =
    throw(ArgumentError("extra characters after whitespace in $(repr(s))"))
@noinline throw_premature_end_of_integer(s) =
    throw(ArgumentError("premature end of integer: $(repr(s))"))
@noinline throw_invalid_base(base) = throw(ArgumentError("invalid base: base must be 2 ≤ base ≤ 62, got $base"))


###############################
# string to integer functions #
###############################

@inline function check_valid_base(base)
    if !(base === nothing || 2 <= base <= 62)
        throw_invalid_base(base)
    end
end

function parse(::Type{T}, s::Union{AbstractChar, AbstractString}; base::Union{Nothing,Integer} = nothing)::T where {T<:Integer}
    check_valid_base(base)
    _parse(T, s, (base===nothing ? 0 : Int(base)), true)
end

function tryparse(::Type{T}, s::Union{AbstractChar, AbstractString}; base::Union{Nothing,Integer} = nothing)::Union{Nothing, T} where {T<:Integer}
    check_valid_base(base)
    _parse(T, s, (base===nothing ? 0 : Int(base)), false)
end

function _parse(::Type{T}, s, base, raise) where {T <: Integer}
    result = T === Bool ? tryparse_internal_bool(s, base, raise) :
                          tryparse_internal_int(T, s, base, raise)
    if raise && result === nothing
        throw_parse_failure(T, s)
    end
    return result
end

function parseint_preamble(s, signed::Bool, base::Int)
    it = iterate(s)
    it === nothing && return '0', 0, 0, nothing
    c, state = it

    while isspace(c)
        it = iterate(s, state)
        it === nothing && return c, 0, 0, nothing
        c, state = it
    end

    sgn = 1
    if signed
        if c == '-' || c == '+'
            (c == '-') && (sgn = -1)
            it = iterate(s, state)
            it === nothing && return c, sgn, base, nothing
            c, state = it
        end
    end

    while isspace(c)
        it = iterate(s, state)
        it === nothing && return c, sgn, base, nothing
        c, state = it
    end

    if base == 0
        if c == '0'
            prevc = c
            prevstate = state
            it = iterate(s, state)
            it === nothing && return c, sgn, 10, nothing
            c, state = it
            base =
                c=='b' ? 2  :
                c=='o' ? 8  :
                c=='x' ? 16 :
                         10
            if base != 10
                it = iterate(s, state)
                it === nothing && return c, sgn, base, nothing
                c, state = it
            else
                state = prevstate
                c = prevc
            end
        else
            base = 10
        end
    end
    return c, sgn, base, state
end

@inline function __convert_digit(_c::UInt32, base)
    _0 = UInt32('0')
    _9 = UInt32('9')
    _A = UInt32('A')
    _a = UInt32('a')
    _Z = UInt32('Z')
    _z = UInt32('z')
    a::UInt32 = base <= 36 ? 10 : 36
    d = _0 <= _c <= _9 ? _c-_0             :
        _A <= _c <= _Z ? _c-_A+ UInt32(10) :
        _a <= _c <= _z ? _c-_a+a           : UInt32(base)
end

function tryparse_internal_int(::Type{T}, s, base::Int, raise::Bool) where T<:Integer
    c, sgn, base, state = parseint_preamble(s, T<:Signed, base)
    if base == 0 && state === nothing
        raise && throw_only_whitespace()
        return nothing
    end
    if state === nothing && base != 10
        raise && throw_premature_end_of_integer(s)
        return nothing
    end

    base = convert(T, base)
    # Special case the common cases of base being 2, 8, 10 or 16 to avoid expensive runtime div
    m::T = base == 10 ? div(typemax(T) - T(9),  T(10)) :
           base == 16 ? div(typemax(T) - T(15), T(16)) :
           base == 8  ? div(typemax(T) - T(7),  T(8)) :
           base == 2  ? div(typemax(T) - T(1),  T(2))  :
                        div(typemax(T) - base + 1, base)
    n::T = 0
    while n <= m
        # Fast path from `UInt32(::Char)`; non-ascii will be >= 0x80
        _c = reinterpret(UInt32, c) >> 24
        d::T = __convert_digit(_c, base)
        if d >= base
            raise && throw_invalid_base_digit(base, c, s)
            return nothing
        end
        n *= base
        n += d
        if state === nothing || ((it = iterate(s, state)) === nothing)
            n *= sgn
            return n
        end
        c, state = it
        isspace(c) && break
    end
    (T <: Signed) && (n *= sgn)
    # TODO: This could probably be DRYd with some of the code above
    while !isspace(c)
        # Fast path from `UInt32(::Char)`; non-ascii will be >= 0x80
        _c = reinterpret(UInt32, c) >> 24
        d::T = __convert_digit(_c, base)
        if d >= base
            raise && throw_invalid_base_digit(base, c, s)
            return nothing
        end
        (T <: Signed) && (d *= sgn)

        n, ov_mul = mul_with_overflow(n, base)
        n, ov_add = add_with_overflow(n, d)
        if ov_mul | ov_add
            raise && throw_overflow_parsing(s)
            return nothing
        end
        it = iterate(s, state)
        it === nothing && return n
        c, state = it
    end
    while true
        it = iterate(s, state)
        it === nothing && return n
        c, state = it
        if !isspace(c)
            raise && throw_extra_characters_whitespace(s)
            return nothing
        end
    end
end

function tryparse_internal_bool(sbuff::Union{String,SubString{String}}, base::Integer, raise::Bool)
    intres = tryparse(Int8, sbuff; base=(base==0 ? nothing : base))
    (intres == 1) && return true
    (intres == 0) && return false

    startpos = firstindex(sbuff)
    endpos = lastindex(sbuff)

    # Ignore leading and trailing whitespace
    while startpos <= endpos && isspace(sbuff[startpos])
        startpos = nextind(sbuff, startpos)
    end
    while endpos >= startpos && isspace(sbuff[endpos])
        endpos = prevind(sbuff, endpos)
    end

    ssub = SubString(sbuff, startpos:endpos)
    ssub == "true" && return true
    ssub == "false" && return false

    if raise
        if all(isspace, sbuff)
            throw_only_whitespace()
        else
            raise && throw_parse_failure(Bool, sbuff)
        end
    end
    return nothing
end


##############################
# string to float functions  #
##############################

parse(::Type{T}, s::AbstractString)    where {T<:Union{Float64, Float32, Float16}} =
    _parse_float(T, s, true)
tryparse(::Type{T}, s::AbstractString) where {T<:Union{Float64, Float32, Float16}} =
    _parse_float(T, s, false)

function _parse_float(::Type{T}, s::AbstractString, raise) where T
    result = tryparse_internal_float(T, s)
    if raise && result === nothing
        throw_parse_failure(T, s)
    end
    return result
end

# Introduce a "DenseString" trait to allow people to hook into this, e.g. StringViews.jl
function tryparse_internal_float(T::Union{Type{Float64}, Type{Float32}}, s::Union{String, SubString{String}})
    hasvalue, val = if T === Float64
        ccall(:jl_try_substrtod, Tuple{Bool, Float64},
            (Ptr{UInt8},Csize_t,Csize_t), s, 0, sizeof(s))
    else
        ccall(:jl_try_substrtof, Tuple{Bool, Float32},
            (Ptr{UInt8},Csize_t,Csize_t), s, 0, sizeof(s))
    end
    hasvalue ? val : nothing
end
tryparse_internal_float(::Type{Float16}, s::Union{String, SubString{String}}) =
    Float16(tryparse_internal_float(Float32, s))


###############################
# string to complex functions #
###############################

parse(::Type{Complex{T}}, s::AbstractString) where {T<:Real} =
    _parse_complex(Complex{T}, s, true)
tryparse(::Type{Complex{T}}, s::AbstractString) where {T<:Real} =
    _parse_complex(Complex{T}, s, false)

function _parse_complex(::Type{Complex{T}}, s::AbstractString, raise::Bool) where T <: Real
    result = tryparse_internal_complex(Complex{T}, s, raise)
    if raise && result === nothing
        throw_parse_failure(Complex{T}, s)
    end
    return result
end

function tryparse_internal_complex(::Type{Complex{T}}, s::AbstractString, raise::Bool) where {T<:Real}
    # skip initial whitespace
    i, e = firstindex(s), lastindex(s)
    while i ≤ e && isspace(s[i])
        i = nextind(s, i)
    end
    if i > e
        raise && throw_only_whitespace()
        return nothing
    end

    # find index of ± separating real/imaginary parts (if any)
    i₊ = something(findnext(in(('+','-')), s, i), 0)
    if i₊ == i # leading ± sign
        i₊ = something(findnext(in(('+','-')), s, i₊+1), 0)
    end
    if i₊ != 0 && s[i₊-1] in ('e','E') # exponent sign
        i₊ = something(findnext(in(('+','-')), s, i₊+1), 0)
    end

    # find trailing im/i/j
    iᵢ = something(findprev(in(('m','i','j')), s, e), 0)
    if iᵢ > 0 && s[iᵢ] == 'm' # im
        iᵢ -= 1
        if s[iᵢ] != 'i'
            raise && throw(ArgumentError("expected trailing \"im\", found only \"m\""))
            return nothing
        end
    end

    if i₊ == 0 # purely real or imaginary value
        if iᵢ > i && !(iᵢ == i+1 && s[i] in ('+','-')) # purely imaginary (not "±inf")
            x = tryparse(T, SubString(s, i, iᵢ-1))
            x === nothing && (raise ? throw_parse_failure(Complex{T}, s) : return nothing)
            return Complex{T}(zero(x),x)
        else # purely real
            x = tryparse(T, SubString(s, i, e))
            x === nothing && (raise ? throw_parse_failure(Complex{T}, s) : return nothing)
            return Complex{T}(x)
        end
    end

    if iᵢ < i₊
        raise && throw(ArgumentError("missing imaginary unit"))
        return nothing # no imaginary part
    end

    # parse real part
    re = tryparse(T, SubString(s, i, i₊-1))
    re === nothing && (raise ? throw_parse_failure(Complex{T}, s) : return nothing)

    # parse imaginary part

    im = tryparse(T, SubString(s, i₊+1, iᵢ-1))
    im === nothing && (raise ? throw_parse_failure(Complex{T}, s) : return nothing)

    return Complex{T}(re, s[i₊]=='-' ? -im : im)
end

# end
