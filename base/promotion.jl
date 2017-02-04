# This file is a part of Julia. License is MIT: http://julialang.org/license

## type join (closest common ancestor, or least upper bound) ##

typejoin() = (@_pure_meta; Bottom)
typejoin(t::ANY) = (@_pure_meta; t)
typejoin(t::ANY, ts...) = (@_pure_meta; typejoin(t, typejoin(ts...)))
function typejoin(a::ANY, b::ANY)
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
        return typejoin(typejoin(a.a,a.b), b)
    elseif isa(b,Union)
        return typejoin(a, typejoin(b.a,b.b))
    elseif a <: Tuple
        if !(b <: Tuple)
            return Any
        end
        ap, bp = a.parameters, b.parameters
        lar = length(ap)::Int; lbr = length(bp)::Int
        laf, afixed = full_va_len(ap)
        lbf, bfixed = full_va_len(bp)
        if lar==0 || lbr==0
            return Tuple
        end
        if laf < lbf
            if isvarargtype(ap[lar]) && !afixed
                c = Vector{Any}(laf)
                c[laf] = Vararg{typejoin(unwrapva(ap[lar]), tailjoin(bp,laf))}
                n = laf-1
            else
                c = Vector{Any}(laf+1)
                c[laf+1] = Vararg{tailjoin(bp,laf+1)}
                n = laf
            end
        elseif lbf < laf
            if isvarargtype(bp[lbr]) && !bfixed
                c = Vector{Any}(lbf)
                c[lbf] = Vararg{typejoin(unwrapva(bp[lbr]), tailjoin(ap,lbf))}
                n = lbf-1
            else
                c = Vector{Any}(lbf+1)
                c[lbf+1] = Vararg{tailjoin(ap,lbf+1)}
                n = lbf
            end
        else
            c = Vector{Any}(laf)
            n = laf
        end
        for i = 1:n
            ai = ap[min(i,lar)]; bi = bp[min(i,lbr)]
            ci = typejoin(unwrapva(ai),unwrapva(bi))
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
            p = Vector{Any}(n)
            for i = 1:n
                ai, bi = a.parameters[i], b.parameters[i]
                if ai === bi || (isa(ai,Type) && isa(bi,Type) && typeseq(ai,bi))
                    p[i] = ai
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

promote_type()  = (@_pure_meta; Bottom)
promote_type(T) = (@_pure_meta; T)
promote_type(T, S, U, V...) = (@_pure_meta; promote_type(T, promote_type(S, U, V...)))

promote_type(::Type{Bottom}, ::Type{Bottom}) = (@_pure_meta; Bottom)
promote_type{T}(::Type{T}, ::Type{T}) = (@_pure_meta; T)
promote_type{T}(::Type{T}, ::Type{Bottom}) = (@_pure_meta; T)
promote_type{T}(::Type{Bottom}, ::Type{T}) = (@_pure_meta; T)

"""
    promote_type(type1, type2)

Determine a type big enough to hold values of each argument type without loss, whenever
possible. In some cases, where no type exists to which both types can be promoted
losslessly, some loss is tolerated; for example, `promote_type(Int64, Float64)` returns
`Float64` even though strictly, not all `Int64` values can be represented exactly as
`Float64` values.

```jldoctest
julia> promote_type(Int64, Float64)
Float64

julia> promote_type(Int32, Int64)
Int64

julia> promote_type(Float32, BigInt)
BigFloat
```
"""
function promote_type{T,S}(::Type{T}, ::Type{S})
    @_pure_meta
    # Try promote_rule in both orders. Typically only one is defined,
    # and there is a fallback returning Bottom below, so the common case is
    #   promote_type(T, S) =>
    #   promote_result(T, S, result, Bottom) =>
    #   typejoin(result, Bottom) => result
    promote_result(T, S, promote_rule(T,S), promote_rule(S,T))
end

promote_rule(T, S) = (@_pure_meta; Bottom)

promote_result(t,s,T,S) = (@_pure_meta; promote_type(T,S))
# If no promote_rule is defined, both directions give Bottom. In that
# case use typejoin on the original types instead.
promote_result{T,S}(::Type{T},::Type{S},::Type{Bottom},::Type{Bottom}) = (@_pure_meta; typejoin(T, S))

promote() = ()
promote(x) = (x,)
function promote{T,S}(x::T, y::S)
    (convert(promote_type(T,S),x), convert(promote_type(T,S),y))
end
promote_typeof(x) = (@_pure_meta; typeof(x))
promote_typeof(x, xs...) = (@_pure_meta; promote_type(typeof(x), promote_typeof(xs...)))
function promote(x, y, z)
    (convert(promote_typeof(x,y,z), x),
     convert(promote_typeof(x,y,z), y),
     convert(promote_typeof(x,y,z), z))
end
function promote(x, y, zs...)
    (convert(promote_typeof(x,y,zs...), x),
     convert(promote_typeof(x,y,zs...), y),
     convert(Tuple{Vararg{promote_typeof(x,y,zs...)}}, zs)...)
end
# TODO: promote{T}(x::T, ys::T...) here to catch all circularities?

## promotions in arithmetic, etc. ##

# Because of the promoting fallback definitions for Number, we need
# a special case for undefined promote_rule on numeric types.
# Otherwise, typejoin(T,S) is called (returning Number) so no conversion
# happens, and +(promote(x,y)...) is called again, causing a stack
# overflow.
function promote_result{T<:Number,S<:Number}(::Type{T},::Type{S},::Type{Bottom},::Type{Bottom})
    @_pure_meta
    promote_to_supertype(T, S, typejoin(T,S))
end

# promote numeric types T and S to typejoin(T,S) if T<:S or S<:T
# for example this makes promote_type(Integer,Real) == Real without
# promoting arbitrary pairs of numeric types to Number.
promote_to_supertype{T<:Number          }(::Type{T}, ::Type{T}, ::Type{T}) = (@_pure_meta; T)
promote_to_supertype{T<:Number,S<:Number}(::Type{T}, ::Type{S}, ::Type{T}) = (@_pure_meta; T)
promote_to_supertype{T<:Number,S<:Number}(::Type{T}, ::Type{S}, ::Type{S}) = (@_pure_meta; S)
promote_to_supertype{T<:Number,S<:Number}(::Type{T}, ::Type{S}, ::Type) =
    error("no promotion exists for ", T, " and ", S)

# promotion with a check for circularity. Can be used to catch what
# would otherwise become StackOverflowErrors.
function promote_noncircular(x, y)
    @_inline_meta
    px, py = promote(x, y)
    not_all_sametype((x,px), (y,py))
    px, py
end
function promote_noncircular(x, y, z)
    @_inline_meta
    px, py, pz = promote(x, y, z)
    not_all_sametype((x,px), (y,py), (z,pz))
    px, py, pz
end
function promote_noncircular(x, y, z, a...)
    p = promote(x, y, z, a...)
    not_all_sametype(map(identity, (x, y, z, a...), p))
    p
end
not_all_sametype(x, y) = nothing
not_all_sametype(x, y, z) = nothing
not_all_sametype{S,T}(x::Tuple{S,S}, y::Tuple{T,T}) = sametype_error(x[1], y[1])
not_all_sametype{R,S,T}(x::Tuple{R,R}, y::Tuple{S,S}, z::Tuple{T,T}) = sametype_error(x[1], y[1], z[1])
function not_all_sametype{R,S,T}(::Tuple{R,R}, y::Tuple{S,S}, z::Tuple{T,T}, args...)
    @_inline_meta
    not_all_sametype(y, z, args...)
end
not_all_sametype() = error("promotion failed to change any input types")
function sametype_error(input...)
    @_noinline_meta
    error("circular method definition: promotion of types ",
          join(map(x->string(typeof(x)), input), ", ", " and "),
          " failed to change any input types")
end

+(x::Number, y::Number) = +(promote(x,y)...)
*(x::Number, y::Number) = *(promote(x,y)...)
-(x::Number, y::Number) = -(promote(x,y)...)
/(x::Number, y::Number) = /(promote(x,y)...)

"""
    ^(x, y)

Exponentiation operator. If `x` is a matrix, computes matrix exponentiation.

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

(&)(x::Integer, y::Integer) = (&)(promote(x,y)...)
(|)(x::Integer, y::Integer) = (|)(promote(x,y)...)
xor(x::Integer, y::Integer) = xor(promote(x,y)...)

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
_default_type(T::Type) = (@_pure_meta; T)

if isdefined(Core, :Inference)
    _return_type(f::ANY, t::ANY) = Core.Inference.return_type(f, t)
else
    _return_type(f::ANY, t::ANY) = Any
end

promote_op(::Any...) = (@_pure_meta; Any)
function promote_op{S}(f, ::Type{S})
    @_inline_meta
    T = _return_type(f, Tuple{_default_type(S)})
    isleaftype(S) && return isleaftype(T) ? T : Any
    return typejoin(S, T)
end
function promote_op{R,S}(f, ::Type{R}, ::Type{S})
    @_inline_meta
    T = _return_type(f, Tuple{_default_type(R), _default_type(S)})
    isleaftype(R) && isleaftype(S) && return isleaftype(T) ? T : Any
    return typejoin(R, S, T)
end

## catch-alls to prevent infinite recursion when definitions are missing ##

no_op_err(name, T) = error(name," not defined for ",T)
+{T<:Number}(x::T, y::T) = no_op_err("+", T)
*{T<:Number}(x::T, y::T) = no_op_err("*", T)
-{T<:Number}(x::T, y::T) = no_op_err("-", T)
/{T<:Number}(x::T, y::T) = no_op_err("/", T)
^{T<:Number}(x::T, y::T) = no_op_err("^", T)

fma{T<:Number}(x::T, y::T, z::T) = no_op_err("fma", T)
fma(x::Integer, y::Integer, z::Integer) = x*y+z
muladd{T<:Number}(x::T, y::T, z::T) = x*y+z

(&){T<:Integer}(x::T, y::T) = no_op_err("&", T)
(|){T<:Integer}(x::T, y::T) = no_op_err("|", T)
xor{T<:Integer}(x::T, y::T) = no_op_err("xor", T)

=={T<:Number}(x::T, y::T) = x === y
 <{T<:Real}(x::T, y::T) = no_op_err("<" , T)
<={T<:Real}(x::T, y::T) = no_op_err("<=", T)

rem{T<:Real}(x::T, y::T) = no_op_err("rem", T)
mod{T<:Real}(x::T, y::T) = no_op_err("mod", T)

min(x::Real) = x
max(x::Real) = x
minmax(x::Real) = (x, x)

max{T<:Real}(x::T, y::T) = ifelse(y < x, x, y)
min{T<:Real}(x::T, y::T) = ifelse(y < x, y, x)
minmax{T<:Real}(x::T, y::T) = y < x ? (y, x) : (x, y)
