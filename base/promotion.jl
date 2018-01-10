# This file is a part of Julia. License is MIT: https://julialang.org/license

## type join (closest common ancestor, or least upper bound) ##

"""
    typejoin(T, S)


Return the closest common ancestor of `T` and `S`, i.e. the narrowest type from which
they both inherit.
"""
typejoin() = (@_pure_meta; Bottom)
typejoin(@nospecialize(t)) = (@_pure_meta; t)
typejoin(@nospecialize(t), ts...) = (@_pure_meta; typejoin(t, typejoin(ts...)))
typejoin(@nospecialize(a), @nospecialize(b)) = join_types(a, b, typejoin, Union{})

function join_types(@nospecialize(a), @nospecialize(b), f::Function, joinparams::Type)
    @_pure_meta
    if a <: b
        return b
    elseif b <: a
        return a
    elseif isa(a,UnionAll)
        return UnionAll(a.var, join_types(a.body, b, f, joinparams))
    elseif isa(b,UnionAll)
        return UnionAll(b.var, join_types(a, b.body, f, joinparams))
    elseif isa(a,TypeVar)
        return f(a.ub, b)
    elseif isa(b,TypeVar)
        return f(a, b.ub)
    elseif isa(a,Union)
        return f(f(a.a,a.b), b)
    elseif isa(b,Union)
        return f(a, f(b.a,b.b))
    elseif a <: Tuple
        if !(b <: Tuple)
            return Any
        end
        ap, bp = a.parameters, b.parameters
        lar = length(ap)::Int; lbr = length(bp)::Int
        if lar == 0
            return Tuple{Vararg{tailjoin(bp,1,f)}}
        end
        if lbr == 0
            return Tuple{Vararg{tailjoin(ap,1,f)}}
        end
        laf, afixed = full_va_len(ap)
        lbf, bfixed = full_va_len(bp)
        if laf < lbf
            if isvarargtype(ap[lar]) && !afixed
                c = Vector{Any}(uninitialized, laf)
                c[laf] = Vararg{f(unwrapva(ap[lar]), tailjoin(bp,laf,f))}
                n = laf-1
            else
                c = Vector{Any}(uninitialized, laf+1)
                c[laf+1] = Vararg{tailjoin(bp,laf+1,f)}
                n = laf
            end
        elseif lbf < laf
            if isvarargtype(bp[lbr]) && !bfixed
                c = Vector{Any}(uninitialized, lbf)
                c[lbf] = Vararg{f(unwrapva(bp[lbr]), tailjoin(ap,lbf,f))}
                n = lbf-1
            else
                c = Vector{Any}(uninitialized, lbf+1)
                c[lbf+1] = Vararg{tailjoin(ap,lbf+1,f)}
                n = lbf
            end
        else
            c = Vector{Any}(uninitialized, laf)
            n = laf
        end
        for i = 1:n
            ai = ap[min(i,lar)]; bi = bp[min(i,lbr)]
            ci = f(unwrapva(ai),unwrapva(bi))
            c[i] = i == length(c) && (isvarargtype(ai) || isvarargtype(bi)) ? Vararg{ci} : ci
        end
        return Tuple{c...}
    elseif b <: Tuple
        return Any
    end
    while b !== Any
        if a <: b.name.wrapper
            while a.name !== b.name
                a = supertype(a)
            end
            aprimary = unwrap_unionall(a.name.wrapper)
            # join on parameters
            n = length(a.parameters)
            if n == 0
                return aprimary
            end
            p = Vector{Any}(uninitialized, n)
            for i = 1:n
                ai, bi = a.parameters[i], b.parameters[i]
                if ai === bi || (isa(ai,Type) && isa(bi,Type) && typeseq(ai,bi))
                    p[i] = ai
                elseif aprimary <: joinparams && isa(ai,Type) && isa(bi,Type)
                    p[i] = f(ai, bi)
                else
                    p[i] = aprimary.parameters[i]
                end
            end
            return rewrap_unionall(a.name.wrapper{p...}, a.name.wrapper)
        end
        b = supertype(b)
    end
    return Any
end

# Returns length, isfixed
function full_va_len(p)
    isempty(p) && return 0, true
    last = p[end]
    if isvarargtype(last)
        N = unwrap_unionall(last).parameters[2]
        if isa(N, Integer)
            return (length(p) + N - 1)::Int, true
        end
        return length(p)::Int, false
    end
    return length(p)::Int, true
end

# reduce join_types over A[i:end]
function tailjoin(A, i, f)
    if i > length(A)
        return unwrapva(A[end])
    end
    t = Bottom
    for j = i:length(A)
        t = f(t, unwrapva(A[j]))
    end
    return t
end

## promotion mechanism ##

abstract type PromotionStyle end
struct ExactPromotion <: PromotionStyle end
struct DefaultPromotion <: PromotionStyle end

"""
    promote_type(type1, type2)
    promote_type(::DefaultPromotion, type1, type2)

Promotion refers to converting values of mixed types to a single common type.
`promote_type(type1, type2)`, equivalent to `promote_type(DefaultPromotion(), type1, type2)`
represents the default promotion behavior in Julia when
operators (usually mathematical) are given arguments of differing types.

`promote_type` generally tries to return a type which can at least approximate
most values of either input type without excessively widening. Contrary to
`ExactPromotion` some loss is tolerated. For example, `promote_type(Int64, Float64)`
returns [`Float64`](@ref) even though strictly, not all [`Int64`](@ref) values can be
represented exactly as `Float64` values.

```jldoctest
julia> promote_type(Int64, Float64)
Float64

julia> promote_type(Int32, Int64)
Int64

julia> promote_type(Float32, BigInt)
BigFloat

julia> promote_type(Int16, Float16)
Float16

julia> promote_type(Int64, Float16)
Float16

julia> promote_type(Int8, UInt16)
UInt16
```
"""
function promote_type(::DefaultPromotion, ::Type{T}, ::Type{S}) where {T,S}
    @_inline_meta
    # Try promote_rule in both orders. Typically only one is defined,
    # and there is a fallback returning Bottom below, so the common case is
    #   promote_type(DefaultPromotion(), T, S) =>
    #   promote_result(DefaultPromotion(), T, S, result, Bottom) =>
    #   promote_type(ExactPromotion(), T, S) =>
    #   promote_result(ExactPromotion(), T, S, result, Bottom) =>
    #   typejoin(result, Bottom) => result
    promote_result(DefaultPromotion(), T, S,
                   promote_rule(DefaultPromotion(),T,S),
                   promote_rule(DefaultPromotion(),S,T))
end

"""
    promote_type(::ExactPromotion, type1, type2)

`ExactPromotion` represents the promotion mechanism used by [`collect`](@ref)
[`map`](@ref) and [`broadcast`](@ref) when producing an array with elements of
differing types. It is guaranteed to return a type which can exactly represent
all values of either input type. Contrary to [`promote_type`](@ref),
no loss is tolerated. For example, `promote_type(ExactPromotion(), Int64, Float64)`
returns `Union{Float64, Int64}` since not all [`Int64`](@ref) values can be
represented exactly as `Float64` values.

```jldoctest
julia> promote_type(ExactPromotion(), Int64, Float64)
Union{Float64, Int64}

julia> promote_type(ExactPromotion(), Int32, Int64)
Int64

# FIXME: should this be Union{Float32, BigInt}?
julia> promote_type(ExactPromotion(), Float32, BigInt)
BigFloat

julia> promote_type(ExactPromotion(), Int16, Float16)
Float16

# FIXME: should this be Float16?
julia> promote_type(ExactPromotion(), Int64, Float16)
Union{Float16, Int64}

julia> promote_type(ExactPromotion(), Int8, UInt16)
UInt16
```
"""
function promote_type(::ExactPromotion, ::Type{T}, ::Type{S}) where {T,S}
    @_inline_meta
    # Try promote_rule in both orders. Typically only one is defined,
    # and there is a fallback returning Bottom below, so the common case is
    #   promote_type(ExactPromotion(), T, S) =>
    #   promote_result(ExactPromotion(), T, S, result, Bottom) =>
    #   typejoin(result, Bottom) => result
    promote_result(ExactPromotion(), T, S,
                   promote_rule(ExactPromotion(),T,S),
                   promote_rule(ExactPromotion(),S,T))
end

promote_type(::DefaultPromotion, ::Type{Bottom}, ::Type{Bottom}) = Bottom
promote_type(::DefaultPromotion, ::Type{T}, ::Type{T}) where {T} = T
promote_type(::DefaultPromotion, ::Type{T}, ::Type{Bottom}) where {T} = T
promote_type(::DefaultPromotion, ::Type{Bottom}, ::Type{T}) where {T} = T

promote_type(::ExactPromotion, ::Type{Bottom}, ::Type{Bottom}) = Bottom
promote_type(::ExactPromotion, ::Type{T}, ::Type{T}) where {T} = T
promote_type(::ExactPromotion, ::Type{T}, ::Type{Bottom}) where {T} = T
promote_type(::ExactPromotion, ::Type{Bottom}, ::Type{T}) where {T} = T

promote_type(::PromotionStyle) = Bottom
promote_type(::PromotionStyle, T::Type) = T
promote_type(p::PromotionStyle, T::Type, S::Type, U::Type, V::Type...) =
    (@_inline_meta; promote_type(p, T, promote_type(p, S, U, V...)))
# Default fallback
promote_type(T::Type) = T
function promote_type(T::Type, S::Type, U::Type...)
    @_inline_meta
    promote_type(DefaultPromotion(), T, promote_type(DefaultPromotion(), S, U...))
end

"""
    promote_rule(type1, type2)
    promote_rule(style::PromotionStyle, type1, type2)

Specifies what type should be used by [`promote`](@ref) when given values of types `type1` and
`type2`. This function should not be called directly, but should have definitions added to
it for new types as appropriate.

Defining a two-argument method is equivalent to defining a
`promote_rule(style::DefaultPromotion, type1, type2)` method. `style` can also be of
type `ExactPromotion` to define a promotion rule which is guaranteed to return a type
able to represent all values of `type1` and `type2` exactly (see [`promote_type`](@ref)).
"""
function promote_rule end

# Fallback so that rules defined without DefaultPromotion() are used
promote_rule(::DefaultPromotion, ::Type{T}, ::Type{S}) where {T,S} = promote_rule(T, S)
# TODO: change ::Type{T} to ::Type{<:Any}?
promote_rule(::DefaultPromotion, ::Type{Any}, ::Type{T}) where {T} = Any
promote_rule(::Type{<:Any}, ::Type{<:Any}) = Bottom
promote_rule(::Type{Any}, ::Type{T}) where {T} = Any

promote_rule(::ExactPromotion, ::Type{<:Any}, ::Type{<:Any}) = Bottom
promote_rule(::ExactPromotion, ::Type{Any}, ::Type) = Any

promote_result(::DefaultPromotion,::Type{<:Any},::Type{<:Any},::Type{T},::Type{S}) where {T,S} =
    (@_inline_meta; promote_type(DefaultPromotion(), T, S))
# If no promote_rule is defined, both directions give Bottom. In that
# case use ExactPromotion on the original types instead.
promote_result(::DefaultPromotion,::Type{T},::Type{S},::Type{Bottom},::Type{Bottom}) where {T,S} =
    (@_inline_meta; promote_type(ExactPromotion(),T,S))

promote_result(::ExactPromotion,::Type{<:Any},::Type{<:Any},::Type{T},::Type{S}) where {T,S} =
    (promote_type(ExactPromotion(),T,S))
# If no promote_rule is defined, both directions give Bottom. In that
# case use typejoin on the original types instead.
promote_result(::ExactPromotion,::Type{T},::Type{S},::Type{Bottom},::Type{Bottom}) where {T,S} =
    (join_types(T, S, (T,S)->promote_type(ExactPromotion(),T,S), Union{Tuple, NamedTuple, AbstractArray}))

not_sametype(x::T, y::T) where {T} = sametype_error(x)

not_sametype(x, y) = nothing

function sametype_error(input)
    @_noinline_meta
    error("promotion of types ",
          join(map(x->string(typeof(x)), input), ", ", " and "),
          " failed to change any arguments")
end

"""
    promote(xs...)
    promote(style::PromotionStyle, xs...)

Convert all arguments to a common type, and return them all (as a tuple).
If no arguments can be converted, an error is raised.

`style` can be either `DefaultPromotion` (the default when omitted), or
`ExactPromotion` to guarantee that the returned type can hold all values of
`type1` and `type2` (see [`promote_type`](@ref)).

# Examples
```jldoctest
julia> promote(Int8(1), Float16(4.5), Float32(4.1))
(1.0f0, 4.5f0, 4.1f0)
```
"""
function promote end

function _promote(p::PromotionStyle, x::T, y::S) where {T,S}
    @_inline_meta
    R = promote_type(p, T, S)
    return (convert(R, x), convert(R, y))
end

promote_typeof(args...) = promote_typeof(DefaultPromotion(), args...)
promote_typeof(::PromotionStyle, x) = typeof(x)
promote_typeof(p::PromotionStyle, x, xs...) =
    (@_inline_meta; promote_type(p, typeof(x), promote_typeof(p, xs...)))
function _promote(p::PromotionStyle, x, y, z)
    @_inline_meta
    R = promote_typeof(p, x, y, z)
    return (convert(R, x), convert(R, y), convert(R, z))
end
function _promote(p::PromotionStyle, x, y, zs...)
    @_inline_meta
    R = promote_typeof(p, x, y, zs...)
    return (convert(R, x), convert(R, y), convert(Tuple{Vararg{R}}, zs)...)
end
# TODO: promote(::P, x::T, ys::T...) where {P<:PromotionStyle, T} here to catch all circularities?

promote(args...) = promote(DefaultPromotion(), args...)

promote(::PromotionStyle) = ()
promote(::PromotionStyle, x) = (x,)

function promote(p::PromotionStyle, x, y)
    @_inline_meta
    px, py = _promote(p, x, y)
    not_sametype((x,y), (px,py))
    px, py
end
function promote(p::PromotionStyle, x, y, z)
    @_inline_meta
    px, py, pz = _promote(p, x, y, z)
    not_sametype((x,y,z), (px,py,pz))
    px, py, pz
end
function promote(p::PromotionStyle, x, y, z, a...)
    p = _promote(p, x, y, z, a...)
    not_sametype((x, y, z, a...), p)
    p
end

promote(p::PromotionStyle, x::T, y::T, zs::T...) where {T} = (x, y, zs...)

## promotions in arithmetic, etc. ##

# Because of the promoting fallback definitions for Number, we need
# a special case for undefined promote_rule on numeric types.
# Otherwise, typejoin(T,S) is called (returning Number) so no conversion
# happens, and +(promote(x,y)...) is called again, causing a stack
# overflow.
# FIXME: find a way to re-enable this. The issue is that
# this function should only be called when DefaultPromotion falls back to ExactPromotion,
# just before the latter falls back to typejoin(). But it should not be called when
# ExactPromotion is used directly, as it triggers errors in places where typejoin() would be fine.
#= function promote_result(::ExactPromotion,
                        ::Type{T},::Type{S},
                        ::Type{Bottom},::Type{Bottom}) where {T<:Number,S<:Number}
    @_inline_meta
    promote_to_supertype(T, S, typejoin(T,S))
end
 =#
# promote numeric types T and S to typejoin(T,S) if T<:S or S<:T
# for example this makes promote_type(Integer,Real) == Real without
# promoting arbitrary pairs of numeric types to Number.
promote_to_supertype(::Type{T}, ::Type{T}, ::Type{T}) where {T<:Number}           = (@_inline_meta; T)
promote_to_supertype(::Type{T}, ::Type{S}, ::Type{T}) where {T<:Number,S<:Number} = (@_inline_meta; T)
promote_to_supertype(::Type{T}, ::Type{S}, ::Type{S}) where {T<:Number,S<:Number} = (@_inline_meta; S)
promote_to_supertype(::Type{T}, ::Type{S}, ::Type) where {T<:Number,S<:Number} =
    error("no promotion exists for ", T, " and ", S)

+(x::Number, y::Number) = +(promote(x,y)...)
*(x::Number, y::Number) = *(promote(x,y)...)
-(x::Number, y::Number) = -(promote(x,y)...)
/(x::Number, y::Number) = /(promote(x,y)...)

"""
    ^(x, y)

Exponentiation operator. If `x` is a matrix, computes matrix exponentiation.

If `y` is an `Int` literal (e.g. `2` in `x^2` or `-3` in `x^-3`), the Julia code
`x^y` is transformed by the compiler to `Base.literal_pow(^, x, Val(y))`, to
enable compile-time specialization on the value of the exponent.
(As a default fallback we have `Base.literal_pow(^, x, Val(y)) = ^(x,y)`,
where usually `^ == Base.^` unless `^` has been defined in the calling
namespace.)

```jldoctest
julia> 3^5
243

julia> A = [1 2; 3 4]
2×2 Array{Int64,2}:
 1  2
 3  4

julia> A^3
2×2 Array{Int64,2}:
 37   54
 81  118
```
"""
^(x::Number, y::Number) = ^(promote(x,y)...)

fma(x::Number, y::Number, z::Number) = fma(promote(x,y,z)...)
muladd(x::Number, y::Number, z::Number) = muladd(promote(x,y,z)...)

==(x::Number, y::Number) = (==)(promote(x,y)...)
<( x::Real, y::Real)     = (< )(promote(x,y)...)
<=(x::Real, y::Real)     = (<=)(promote(x,y)...)

div(x::Real, y::Real) = div(promote(x,y)...)
fld(x::Real, y::Real) = fld(promote(x,y)...)
cld(x::Real, y::Real) = cld(promote(x,y)...)
rem(x::Real, y::Real) = rem(promote(x,y)...)
mod(x::Real, y::Real) = mod(promote(x,y)...)

mod1(x::Real, y::Real) = mod1(promote(x,y)...)
fld1(x::Real, y::Real) = fld1(promote(x,y)...)

max(x::Real, y::Real) = max(promote(x,y)...)
min(x::Real, y::Real) = min(promote(x,y)...)
minmax(x::Real, y::Real) = minmax(promote(x, y)...)

# "Promotion" that takes a function into account and tries to preserve
# non-concrete types. These are meant to be used mainly by elementwise
# operations, so it is advised against overriding them
_default_type(T::Type) = (@_inline_meta; T)

if isdefined(Core, :Inference)
    const _return_type = Core.Inference.return_type
else
    _return_type(@nospecialize(f), @nospecialize(t)) = Any
end

promote_op(::Any...) = (@_inline_meta; Any)
function promote_op(f, ::Type{S}) where S
    @_inline_meta
    T = _return_type(f, Tuple{_default_type(S)})
    _isleaftype(S) && return _isleaftype(T) ? T : Any
    return typejoin(S, T)
end
function promote_op(f, ::Type{R}, ::Type{S}) where {R,S}
    @_inline_meta
    T = _return_type(f, Tuple{_default_type(R), _default_type(S)})
    _isleaftype(R) && _isleaftype(S) && return _isleaftype(T) ? T : Any
    return typejoin(R, S, T)
end

## catch-alls to prevent infinite recursion when definitions are missing ##

no_op_err(name, T) = error(name," not defined for ",T)
(+)(x::T, y::T) where {T<:Number} = no_op_err("+", T)
(*)(x::T, y::T) where {T<:Number} = no_op_err("*", T)
(-)(x::T, y::T) where {T<:Number} = no_op_err("-", T)
(/)(x::T, y::T) where {T<:Number} = no_op_err("/", T)
(^)(x::T, y::T) where {T<:Number} = no_op_err("^", T)

fma(x::T, y::T, z::T) where {T<:Number} = no_op_err("fma", T)
fma(x::Integer, y::Integer, z::Integer) = x*y+z
muladd(x::T, y::T, z::T) where {T<:Number} = x*y+z

(&)(x::T, y::T) where {T<:Integer} = no_op_err("&", T)
(|)(x::T, y::T) where {T<:Integer} = no_op_err("|", T)
xor(x::T, y::T) where {T<:Integer} = no_op_err("xor", T)

(==)(x::T, y::T) where {T<:Number} = x === y
(< )(x::T, y::T) where {T<:Real} = no_op_err("<" , T)
(<=)(x::T, y::T) where {T<:Real} = no_op_err("<=", T)

rem(x::T, y::T) where {T<:Real} = no_op_err("rem", T)
mod(x::T, y::T) where {T<:Real} = no_op_err("mod", T)

min(x::Real) = x
max(x::Real) = x
minmax(x::Real) = (x, x)

max(x::T, y::T) where {T<:Real} = select_value(y < x, x, y)
min(x::T, y::T) where {T<:Real} = select_value(y < x, y, x)
minmax(x::T, y::T) where {T<:Real} = y < x ? (y, x) : (x, y)

flipsign(x::T, y::T) where {T<:Signed} = no_op_err("flipsign", T)
