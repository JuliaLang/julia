## type join (closest common ancestor, or least upper bound) ##

typejoin() = Bottom
typejoin(t::ANY) = t
typejoin(t::ANY, ts...) = typejoin(t, typejoin(ts...))
function typejoin(a::ANY, b::ANY)
    if isa(a,TypeConstructor); a = a.body; end
    if isa(b,TypeConstructor); b = b.body; end
    if a <: b
        return b
    elseif b <: a
        return a
    end
    if isa(a,TypeVar)
        return typejoin(a.ub, b)
    end
    if isa(b,TypeVar)
        return typejoin(a, b.ub)
    end
    if isa(a,UnionType) || isa(b,UnionType)
        u = Union(a, b)
        if !isa(u,UnionType)
            return u
        end
        return reduce(typejoin, Bottom, u.types)
    end
    if isa(a,Tuple)
        if !isa(b,Tuple)
            return Any
        end
        la = length(a)::Int; lb = length(b)::Int
        if la==0 || lb==0
            return Tuple
        end
        if la < lb
            if isvarargtype(a[la])
                c = cell(la)
                c[la] = Vararg{typejoin(a[la].parameters[1], tailjoin(b,la))}
                n = la-1
            else
                c = cell(la+1)
                c[la+1] = Vararg{tailjoin(b,la+1)}
                n = la
            end
        elseif lb < la
            if isvarargtype(b[lb])
                c = cell(lb)
                c[lb] = Vararg{typejoin(b[lb].parameters[1], tailjoin(a,lb))}
                n = lb-1
            else
                c = cell(lb+1)
                c[lb+1] = Vararg{tailjoin(a,lb+1)}
                n = lb
            end
        else
            c = cell(la)
            n = la
        end
        for i=1:n
            ai = a[i]; bi = b[i]
            va = false
            if isvarargtype(ai); va=true; ai = ai.parameters[1]; end
            if isvarargtype(bi); va=true; bi = bi.parameters[1]; end
            t = typejoin(ai,bi)
            c[i] = va ? Vararg{t} : t
        end
        return tuple(c...)
    elseif isa(b,Tuple)
        return Any
    end
    while !is(b,Any)
        if a <: b
            return b
        end
        if a <: b.name.primary
            return b.name.primary
        end
        b = super(b)
    end
    return Any
end

# reduce typejoin over tup[i:end]
function tailjoin(tup, i)
    t = Bottom
    for j = i:length(tup)
        tj = tup[j]
        t = typejoin(t, isvarargtype(tj)?tj.parameters[1]:tj)
    end
    return t
end

## promotion mechanism ##

promote_type()  = Bottom
promote_type(T) = T
promote_type(T, S, U, V...) = promote_type(T, promote_type(S, U, V...))

promote_type(::Type{Bottom}, ::Type{Bottom}) = Bottom
promote_type{T}(::Type{T}, ::Type{T}) = T
promote_type{T}(::Type{T}, ::Type{Bottom}) = T
promote_type{T}(::Type{Bottom}, ::Type{T}) = T

# Try promote_rule in both orders. Typically only one is defined,
# and there is a fallback returning Bottom below, so the common case is
#   promote_type(T, S) =>
#   promote_result(T, S, result, Bottom) =>
#   typejoin(result, Bottom) => result
promote_type{T,S}(::Type{T}, ::Type{S}) =
    promote_result(T, S, promote_rule(T,S), promote_rule(S,T))

promote_rule(T, S) = Bottom

promote_result(t,s,T,S) = promote_type(T,S)
# If no promote_rule is defined, both directions give Bottom. In that
# case use typejoin on the original types instead.
promote_result{T,S}(::Type{T},::Type{S},::Type{Bottom},::Type{Bottom}) = typejoin(T, S)

promote() = ()
promote(x) = (x,)
function promote{T,S}(x::T, y::S)
    (convert(promote_type(T,S),x), convert(promote_type(T,S),y))
end
promote_typeof(x) = typeof(x)
promote_typeof(x, xs...) = promote_type(typeof(x), promote_typeof(xs...))
function promote(x, y, z)
    (convert(promote_typeof(x,y,z), x),
     convert(promote_typeof(x,y,z), y),
     convert(promote_typeof(x,y,z), z))
end
function promote(x, y, zs...)
    tuple(convert(promote_typeof(x,y,zs...), x),
          convert(promote_typeof(x,y,zs...), y),
          convert((promote_typeof(x,y,zs...)...), zs)...)
end
# TODO: promote{T}(x::T, ys::T...) here to catch all circularities?

## promotions in arithmetic, etc. ##

# Because of the promoting fallback definitions for Number, we need
# a special case for undefined promote_rule on numeric types.
# Otherwise, typejoin(T,S) is called (returning Number) so no conversion
# happens, and +(promote(x,y)...) is called again, causing a stack
# overflow.
promote_result{T<:Number,S<:Number}(::Type{T},::Type{S},::Type{Bottom},::Type{Bottom}) =
    promote_to_super(T, S, typejoin(T,S))

# promote numeric types T and S to typejoin(T,S) if T<:S or S<:T
# for example this makes promote_type(Integer,Real) == Real without
# promoting arbitrary pairs of numeric types to Number.
promote_to_super{T<:Number          }(::Type{T}, ::Type{T}, ::Type{T}) = T
promote_to_super{T<:Number,S<:Number}(::Type{T}, ::Type{S}, ::Type{T}) = T
promote_to_super{T<:Number,S<:Number}(::Type{T}, ::Type{S}, ::Type{S}) = S
promote_to_super{T<:Number,S<:Number}(::Type{T}, ::Type{S}, ::Type) =
    error("no promotion exists for ", T, " and ", S)

+(x::Number, y::Number) = +(promote(x,y)...)
*(x::Number, y::Number) = *(promote(x,y)...)
-(x::Number, y::Number) = -(promote(x,y)...)
/(x::Number, y::Number) = /(promote(x,y)...)
^(x::Number, y::Number) = ^(promote(x,y)...)

(&)(x::Integer, y::Integer) = (&)(promote(x,y)...)
(|)(x::Integer, y::Integer) = (|)(promote(x,y)...)
($)(x::Integer, y::Integer) = ($)(promote(x,y)...)

==(x::Number, y::Number) = (==)(promote(x,y)...)
< (x::Real, y::Real)     = (< )(promote(x,y)...)
<=(x::Real, y::Real)     = (<=)(promote(x,y)...)

div(x::Real, y::Real) = div(promote(x,y)...)
fld(x::Real, y::Real) = fld(promote(x,y)...)
cld(x::Real, y::Real) = cld(promote(x,y)...)
rem(x::Real, y::Real) = rem(promote(x,y)...)
mod(x::Real, y::Real) = mod(promote(x,y)...)

mod1(x::Real, y::Real) = mod1(promote(x,y)...)
rem1(x::Real, y::Real) = rem1(promote(x,y)...)
fld1(x::Real, y::Real) = fld1(promote(x,y)...)

max(x::Real, y::Real) = max(promote(x,y)...)
min(x::Real, y::Real) = min(promote(x,y)...)
minmax(x::Real, y::Real) = minmax(promote(x, y)...)

## catch-alls to prevent infinite recursion when definitions are missing ##

no_op_err(name, T) = error(name," not defined for ",T)
+{T<:Number}(x::T, y::T) = no_op_err("+", T)
*{T<:Number}(x::T, y::T) = no_op_err("*", T)
-{T<:Number}(x::T, y::T) = no_op_err("-", T)
/{T<:Number}(x::T, y::T) = no_op_err("/", T)
^{T<:Number}(x::T, y::T) = no_op_err("^", T)

(&){T<:Integer}(x::T, y::T) = no_op_err("&", T)
(|){T<:Integer}(x::T, y::T) = no_op_err("|", T)
($){T<:Integer}(x::T, y::T) = no_op_err("\$", T)

=={T<:Number}(x::T, y::T) = x === y
<{T<:Real}(x::T, y::T) = no_op_err("<", T)

max{T<:Real}(x::T, y::T) = ifelse(y < x, x, y)
min{T<:Real}(x::T, y::T) = ifelse(y < x, y, x)
minmax{T<:Real}(x::T, y::T) = y < x ? (y, x) : (x, y)
