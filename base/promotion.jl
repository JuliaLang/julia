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
function typejoin(@nospecialize(a), @nospecialize(b))
    @_pure_meta
    if a <: b
        return b
    elseif b <: a
        return a
    elseif isa(a,UnionAll)
        return UnionAll(a.var, typejoin(a.body, b))
    elseif isa(b,UnionAll)
        return UnionAll(b.var, typejoin(a, b.body))
    elseif isa(a,TypeVar)
        return typejoin(a.ub, b)
    elseif isa(b,TypeVar)
        return typejoin(a, b.ub)
    elseif isa(a,Union)
        a′ = typejoin(a.a, a.b)
        return a′ === a ? typejoin(a, b) : typejoin(a′, b)
    elseif isa(b,Union)
        b′ = typejoin(b.a, b.b)
        return b′ === b ? typejoin(a, b) : typejoin(a, b′)
    elseif a <: Tuple
        if !(b <: Tuple)
            return Any
        end
        ap, bp = a.parameters, b.parameters
        lar = length(ap)::Int; lbr = length(bp)::Int
        if lar == 0
            return Tuple{Vararg{tailjoin(bp, 1)}}
        end
        if lbr == 0
            return Tuple{Vararg{tailjoin(ap, 1)}}
        end
        laf, afixed = full_va_len(ap)
        lbf, bfixed = full_va_len(bp)
        if laf < lbf
            if isvarargtype(ap[lar]) && !afixed
                c = Vector{Any}(undef, laf)
                c[laf] = Vararg{typejoin(unwrapva(ap[lar]), tailjoin(bp, laf))}
                n = laf-1
            else
                c = Vector{Any}(undef, laf+1)
                c[laf+1] = Vararg{tailjoin(bp, laf+1)}
                n = laf
            end
        elseif lbf < laf
            if isvarargtype(bp[lbr]) && !bfixed
                c = Vector{Any}(undef, lbf)
                c[lbf] = Vararg{typejoin(unwrapva(bp[lbr]), tailjoin(ap, lbf))}
                n = lbf-1
            else
                c = Vector{Any}(undef, lbf+1)
                c[lbf+1] = Vararg{tailjoin(ap, lbf+1)}
                n = lbf
            end
        else
            c = Vector{Any}(undef, laf)
            n = laf
        end
        for i = 1:n
            ai = ap[min(i,lar)]; bi = bp[min(i,lbr)]
            ci = typejoin(unwrapva(ai), unwrapva(bi))
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
            if a.name === Type.body.name
                ap = a.parameters[1]
                bp = b.parameters[1]
                if ((isa(ap,TypeVar) && ap.lb === Bottom && ap.ub === Any) ||
                    (isa(bp,TypeVar) && bp.lb === Bottom && bp.ub === Any))
                    # handle special Type{T} supertype
                    return Type
                end
            end
            aprimary = a.name.wrapper
            # join on parameters
            n = length(a.parameters)
            if n == 0
                return aprimary
            end
            vars = []
            for i = 1:n
                ai, bi = a.parameters[i], b.parameters[i]
                if ai === bi || (isa(ai,Type) && isa(bi,Type) && ai <: bi && bi <: ai)
                    aprimary = aprimary{ai}
                else
                    pushfirst!(vars, aprimary.var)
                    aprimary = aprimary.body
                end
            end
            for v in vars
                aprimary = UnionAll(v, aprimary)
            end
            return aprimary
        end
        b = supertype(b)
    end
    return Any
end

"""
    promote_typejoin(T, S)

Compute a type that contains both `T` and `S`, which could be
either a parent of both types, or a `Union` if appropriate.
Falls back to [`typejoin`](@ref).
"""
promote_typejoin(@nospecialize(a), @nospecialize(b)) = typejoin(a, b)
promote_typejoin(::Type{Nothing}, ::Type{T}) where {T} =
    isconcretetype(T) || T === Union{} ? Union{T, Nothing} : Any
promote_typejoin(::Type{T}, ::Type{Nothing}) where {T} =
    isconcretetype(T) || T === Union{} ? Union{T, Nothing} : Any
promote_typejoin(::Type{Missing}, ::Type{T}) where {T} =
    isconcretetype(T) || T === Union{} ? Union{T, Missing} : Any
promote_typejoin(::Type{T}, ::Type{Missing}) where {T} =
    isconcretetype(T) || T === Union{} ? Union{T, Missing} : Any
promote_typejoin(::Type{Nothing}, ::Type{Missing}) = Union{Nothing, Missing}
promote_typejoin(::Type{Missing}, ::Type{Nothing}) = Union{Nothing, Missing}
promote_typejoin(::Type{Nothing}, ::Type{Nothing}) = Nothing
promote_typejoin(::Type{Missing}, ::Type{Missing}) = Missing

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

# reduce typejoin over A[i:end]
function tailjoin(A, i)
    if i > length(A)
        return unwrapva(A[end])
    end
    t = Bottom
    for j = i:length(A)
        t = typejoin(t, unwrapva(A[j]))
    end
    return t
end

## promotion mechanism ##

"""
    promote_type(type1, type2)

Promotion refers to converting values of mixed types to a single common type.
`promote_type` represents the default promotion behavior in Julia when
operators (usually mathematical) are given arguments of differing types.
`promote_type` generally tries to return a type which can at least approximate
most values of either input type without excessively widening.  Some loss is
tolerated; for example, `promote_type(Int64, Float64)` returns
[`Float64`](@ref) even though strictly, not all [`Int64`](@ref) values can be
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
function promote_type end

promote_type()  = Bottom
promote_type(T) = T
promote_type(T, S, U, V...) = (@_inline_meta; promote_type(T, promote_type(S, U, V...)))

promote_type(::Type{Bottom}, ::Type{Bottom}) = Bottom
promote_type(::Type{T}, ::Type{T}) where {T} = T
promote_type(::Type{T}, ::Type{Bottom}) where {T} = T
promote_type(::Type{Bottom}, ::Type{T}) where {T} = T

function promote_type(::Type{T}, ::Type{S}) where {T,S}
    @_inline_meta
    # Try promote_rule in both orders. Typically only one is defined,
    # and there is a fallback returning Bottom below, so the common case is
    #   promote_type(T, S) =>
    #   promote_result(T, S, result, Bottom) =>
    #   typejoin(result, Bottom) => result
    promote_result(T, S, promote_rule(T,S), promote_rule(S,T))
end

"""
    promote_rule(type1, type2)

Specifies what type should be used by [`promote`](@ref) when given values of types `type1` and
`type2`. This function should not be called directly, but should have definitions added to
it for new types as appropriate.
"""
function promote_rule end

promote_rule(::Type{<:Any}, ::Type{<:Any}) = Bottom
# To fix ambiguities
promote_rule(::Type{Any}, ::Type{<:Any}) = Any
promote_rule(::Type{<:Any}, ::Type{Any}) = Any
promote_rule(::Type{Any}, ::Type{Any}) = Any

promote_result(::Type{<:Any},::Type{<:Any},::Type{T},::Type{S}) where {T,S} = (@_inline_meta; promote_type(T,S))
# If no promote_rule is defined, both directions give Bottom. In that
# case use typejoin on the original types instead.
promote_result(::Type{T},::Type{S},::Type{Bottom},::Type{Bottom}) where {T,S} = (@_inline_meta; typejoin(T, S))

"""
    promote(xs...)

Convert all arguments to a common type, and return them all (as a tuple).
If no arguments can be converted, an error is raised.

# Examples
```jldoctest
julia> promote(Int8(1), Float16(4.5), Float32(4.1))
(1.0f0, 4.5f0, 4.1f0)
```
"""
function promote end

function _promote(x::T, y::S) where {T,S}
    @_inline_meta
    R = promote_type(T, S)
    return (convert(R, x), convert(R, y))
end
promote_typeof(x) = typeof(x)
promote_typeof(x, xs...) = (@_inline_meta; promote_type(typeof(x), promote_typeof(xs...)))
function _promote(x, y, z)
    @_inline_meta
    R = promote_typeof(x, y, z)
    return (convert(R, x), convert(R, y), convert(R, z))
end
function _promote(x, y, zs...)
    @_inline_meta
    R = promote_typeof(x, y, zs...)
    return (convert(R, x), convert(R, y), convert(Tuple{Vararg{R}}, zs)...)
end
# TODO: promote(x::T, ys::T...) where {T} here to catch all circularities?

## promotions in arithmetic, etc. ##

# Because of the promoting fallback definitions for Number, we need
# a special case for undefined promote_rule on numeric types.
# Otherwise, typejoin(T,S) is called (returning Number) so no conversion
# happens, and +(promote(x,y)...) is called again, causing a stack
# overflow.
function promote_result(::Type{T},::Type{S},::Type{Bottom},::Type{Bottom}) where {T<:Number,S<:Number}
    @_inline_meta
    promote_to_supertype(T, S, typejoin(T,S))
end

# promote numeric types T and S to typejoin(T,S) if T<:S or S<:T
# for example this makes promote_type(Integer,Real) == Real without
# promoting arbitrary pairs of numeric types to Number.
promote_to_supertype(::Type{T}, ::Type{T}, ::Type{T}) where {T<:Number}           = (@_inline_meta; T)
promote_to_supertype(::Type{T}, ::Type{S}, ::Type{T}) where {T<:Number,S<:Number} = (@_inline_meta; T)
promote_to_supertype(::Type{T}, ::Type{S}, ::Type{S}) where {T<:Number,S<:Number} = (@_inline_meta; S)
promote_to_supertype(::Type{T}, ::Type{S}, ::Type) where {T<:Number,S<:Number} =
    error("no promotion exists for ", T, " and ", S)

promote() = ()
promote(x) = (x,)

function promote(x, y)
    @_inline_meta
    px, py = _promote(x, y)
    not_sametype((x,y), (px,py))
    px, py
end
function promote(x, y, z)
    @_inline_meta
    px, py, pz = _promote(x, y, z)
    not_sametype((x,y,z), (px,py,pz))
    px, py, pz
end
function promote(x, y, z, a...)
    p = _promote(x, y, z, a...)
    not_sametype((x, y, z, a...), p)
    p
end

promote(x::T, y::T, zs::T...) where {T} = (x, y, zs...)

not_sametype(x::T, y::T) where {T} = sametype_error(x)

not_sametype(x, y) = nothing

function sametype_error(input)
    @_noinline_meta
    error("promotion of types ",
          join(map(x->string(typeof(x)), input), ", ", " and "),
          " failed to change any arguments")
end

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

if isdefined(Core, :Compiler)
    const _return_type = Core.Compiler.return_type
else
    _return_type(@nospecialize(f), @nospecialize(t)) = Any
end

"""
    promote_op(f, argtypes...)

Guess what an appropriate container eltype would be for storing results of
`f(::argtypes...)`. The guess is in part based on type inference, so can change any time.

!!! warning
    In pathological cases, the type returned by `promote_op(f, argtypes...)` may not even
    be a supertype of the return value of `f(::argtypes...)`. Therefore, `promote_op`
    should _not_ be used e.g. in the preallocation of an output array.

!!! warning
    Due to its fragility, use of `promote_op` should be avoided. It is preferable to base
    the container eltype on the type of the actual elements. Only in the absence of any
    elements (for an empty result container), it may be unavoidable to call `promote_op`.
"""
promote_op(::Any...) = (@_inline_meta; Any)
function promote_op(f, ::Type{S}) where S
    @_inline_meta
    TT = Tuple{_default_type(S)}
    T = _return_type(f, TT)
    isdispatchtuple(Tuple{S}) && return isdispatchtuple(Tuple{T}) ? T : Any
    return typejoin(S, T)
end
function promote_op(f, ::Type{R}, ::Type{S}) where {R,S}
    @_inline_meta
    TT = Tuple{_default_type(R), _default_type(S)}
    T = _return_type(f, TT)
    isdispatchtuple(Tuple{R}) && isdispatchtuple(Tuple{S}) && return isdispatchtuple(Tuple{T}) ? T : Any
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
