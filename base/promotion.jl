# This file is a part of Julia. License is MIT: https://julialang.org/license

## type join (closest common ancestor, or least upper bound) ##

"""
    typejoin(T, S)

Return the closest common ancestor of `T` and `S`, i.e. the narrowest type from which
they both inherit.
"""
typejoin() = Bottom
typejoin(@nospecialize(t)) = t
typejoin(@nospecialize(t), ts...) = (@_total_meta; typejoin(t, typejoin(ts...)))
function typejoin(@nospecialize(a), @nospecialize(b))
    @_total_meta
    if isa(a, TypeVar)
        return typejoin(a.ub, b)
    elseif isa(b, TypeVar)
        return typejoin(a, b.ub)
    elseif a <: b
        return b
    elseif b <: a
        return a
    elseif isa(a, UnionAll)
        return UnionAll(a.var, typejoin(a.body, b))
    elseif isa(b, UnionAll)
        return UnionAll(b.var, typejoin(a, b.body))
    elseif isa(a, Union)
        return typejoin(typejoin(a.a, a.b), b)
    elseif isa(b, Union)
        return typejoin(a, typejoin(b.a, b.b))
    end
    # a and b are DataTypes
    # We have to hide Constant info from inference, see #44390
    a, b = inferencebarrier(a)::DataType, inferencebarrier(b)::DataType
    if a <: Tuple
        if !(b <: Tuple)
            return Any
        end
        ap, bp = a.parameters, b.parameters
        lar = length(ap)
        lbr = length(bp)
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
                a = supertype(a)::DataType
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
                    # pushfirst!(vars, aprimary.var)
                    _growbeg!(vars, 1)
                    arrayset(false, vars, aprimary.var, 1)
                    aprimary = aprimary.body
                end
            end
            for v in vars
                aprimary = UnionAll(v, aprimary)
            end
            return aprimary
        end
        b = supertype(b)::DataType
    end
    return Any
end

# return an upper-bound on type `a` with type `b` removed
# such that `return <: a` && `Union{return, b} == Union{a, b}`
# WARNING: this is wrong for some objects for which subtyping is broken
#          (Core.Compiler.isnotbrokensubtype), use only simple types for `b`
function typesplit(@nospecialize(a), @nospecialize(b))
    @_foldable_meta
    if a <: b
        return Bottom
    end
    if isa(a, Union)
        return Union{typesplit(a.a, b),
                     typesplit(a.b, b)}
    end
    return a
end


"""
    promote_typejoin(T, S)

Compute a type that contains both `T` and `S`, which could be
either a parent of both types, or a `Union` if appropriate.
Falls back to [`typejoin`](@ref).

See instead [`promote`](@ref), [`promote_type`](@ref).

# Examples
```jldoctest
julia> Base.promote_typejoin(Int, Float64)
Real

julia> Base.promote_type(Int, Float64)
Float64
```
"""
function promote_typejoin(@nospecialize(a), @nospecialize(b))
    c = typejoin(_promote_typesubtract(a), _promote_typesubtract(b))
    return Union{a, b, c}::Type
end
_promote_typesubtract(@nospecialize(a)) = typesplit(a, Union{Nothing, Missing})

function promote_typejoin_union(::Type{T}) where T
    if T === Union{}
        return Union{}
    elseif T isa UnionAll
        return Any # TODO: compute more precise bounds
    elseif T isa Union
        return promote_typejoin(promote_typejoin_union(T.a), promote_typejoin_union(T.b))
    elseif T isa DataType
        T <: Tuple && return typejoin_union_tuple(T)
        return T
    else
        error("unreachable") # not a type??
    end
end

function typejoin_union_tuple(T::DataType)
    @_foldable_meta
    u = Base.unwrap_unionall(T)
    p = (u::DataType).parameters
    lr = length(p)::Int
    if lr == 0
        return Tuple{}
    end
    c = Vector{Any}(undef, lr)
    for i = 1:lr
        pi = p[i]
        U = Core.Compiler.unwrapva(pi)
        if U === Union{}
            ci = Union{}
        elseif U isa Union
            ci = typejoin(U.a, U.b)
        elseif U isa UnionAll
            return Any # TODO: compute more precise bounds
        else
            ci = promote_typejoin_union(U)
        end
        if i == lr && Core.Compiler.isvarargtype(pi)
            c[i] = isdefined(pi, :N) ? Vararg{ci, pi.N} : Vararg{ci}
        else
            c[i] = ci
        end
    end
    return Base.rewrap_unionall(Tuple{c...}, T)
end

# Returns length, isfixed
function full_va_len(p::Core.SimpleVector)
    isempty(p) && return 0, true
    last = p[end]
    if isvarargtype(last)
        if isdefined(last, :N)
            N = last.N
            isa(N, Int) && return length(p) + N - 1, true
        end
        return length(p), false
    end
    return length(p), true
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
    promote_type(type1, type2, ...)

Promotion refers to converting values of mixed types to a single common type.
`promote_type` represents the default promotion behavior in Julia when
operators (usually mathematical) are given arguments of differing types.
`promote_type` generally tries to return a type which can at least approximate
most values of either input type without excessively widening.  Some loss is
tolerated; for example, `promote_type(Int64, Float64)` returns
[`Float64`](@ref) even though strictly, not all [`Int64`](@ref) values can be
represented exactly as `Float64` values.

See also: [`promote`](@ref), [`promote_typejoin`](@ref), [`promote_rule`](@ref).

# Examples
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

!!! warning "Don't overload this directly"
    To overload promotion for your own types you should overload [`promote_rule`](@ref).
    `promote_type` calls `promote_rule` internally to determine the type.
    Overloading `promote_type` directly can cause ambiguity errors.
"""
function promote_type end

promote_type()  = Bottom
promote_type(T) = T
promote_type(T, S, U, V...) = (@inline; promote_type(T, promote_type(S, U, V...)))

promote_type(::Type{Bottom}, ::Type{Bottom}) = Bottom
promote_type(::Type{T}, ::Type{T}) where {T} = T
promote_type(::Type{T}, ::Type{Bottom}) where {T} = T
promote_type(::Type{Bottom}, ::Type{T}) where {T} = T

function promote_type(::Type{T}, ::Type{S}) where {T,S}
    @inline
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

promote_rule(::Type, ::Type) = Bottom

promote_result(::Type,::Type,::Type{T},::Type{S}) where {T,S} = (@inline; promote_type(T,S))
# If no promote_rule is defined, both directions give Bottom. In that
# case use typejoin on the original types instead.
promote_result(::Type{T},::Type{S},::Type{Bottom},::Type{Bottom}) where {T,S} = (@inline; typejoin(T, S))

"""
    promote(xs...)

Convert all arguments to a common type, and return them all (as a tuple).
If no arguments can be converted, an error is raised.

See also: [`promote_type`], [`promote_rule`].

# Examples
```jldoctest
julia> promote(Int8(1), Float16(4.5), Float32(4.1))
(1.0f0, 4.5f0, 4.1f0)
```
"""
function promote end

function _promote(x::T, y::S) where {T,S}
    @inline
    R = promote_type(T, S)
    return (convert(R, x), convert(R, y))
end
promote_typeof(x) = typeof(x)
promote_typeof(x, xs...) = (@inline; promote_type(typeof(x), promote_typeof(xs...)))
function _promote(x, y, z)
    @inline
    R = promote_typeof(x, y, z)
    return (convert(R, x), convert(R, y), convert(R, z))
end
function _promote(x, y, zs...)
    @inline
    R = promote_typeof(x, y, zs...)
    return (convert(R, x), convert(R, y), convert(Tuple{Vararg{R}}, zs)...)
end
# TODO: promote(x::T, ys::T...) where {T} here to catch all circularities?

## promotions in arithmetic, etc. ##

promote() = ()
promote(x) = (x,)

function promote(x, y)
    @inline
    px, py = _promote(x, y)
    not_sametype((x,y), (px,py))
    px, py
end
function promote(x, y, z)
    @inline
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
    @noinline
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
namespace.) If `y` is a negative integer literal, then `Base.literal_pow`
transforms the operation to `inv(x)^-y` by default, where `-y` is positive.

# Examples
```jldoctest
julia> 3^5
243

julia> A = [1 2; 3 4]
2×2 Matrix{Int64}:
 1  2
 3  4

julia> A^3
2×2 Matrix{Int64}:
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

rem(x::Real, y::Real) = rem(promote(x,y)...)
mod(x::Real, y::Real) = mod(promote(x,y)...)

mod1(x::Real, y::Real) = mod1(promote(x,y)...)
fld1(x::Real, y::Real) = fld1(promote(x,y)...)

max(x::Real, y::Real) = max(promote(x,y)...)
min(x::Real, y::Real) = min(promote(x,y)...)
minmax(x::Real, y::Real) = minmax(promote(x, y)...)

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
    Due to its fragility, use of `promote_op` should be avoided. It is preferable to base
    the container eltype on the type of the actual elements. Only in the absence of any
    elements (for an empty result container), it may be unavoidable to call `promote_op`.
"""
promote_op(f, S::Type...) = _return_type(f, Tuple{S...})

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

max(x::T, y::T) where {T<:Real} = ifelse(y < x, x, y)
min(x::T, y::T) where {T<:Real} = ifelse(y < x, y, x)
minmax(x::T, y::T) where {T<:Real} = y < x ? (y, x) : (x, y)

flipsign(x::T, y::T) where {T<:Signed} = no_op_err("flipsign", T)
