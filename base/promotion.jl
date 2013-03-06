## type join (closest common ancestor, or least upper bound) ##

typejoin() = None
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
        return reduce(typejoin, None, u.types)
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
    t = None
    for j = i:length(tup)
        tj = tup[j]
        t = typejoin(t, isvarargtype(tj)?tj.parameters[1]:tj)
    end
    return t
end

## promotion mechanism ##

promote_type()  = None
promote_type(T) = T
promote_type(T, S   ) = typejoin(T, S)
promote_type(T, S...) = promote_type(T, promote_type(S...))

promote_type(::Type{None}, ::Type{None}) = None
promote_type{T}(::Type{T}, ::Type{T}) = T
promote_type{T}(::Type{T}, ::Type{None}) = T
promote_type{T}(::Type{None}, ::Type{T}) = T

function promote_type{T,S}(::Type{T}, ::Type{S})
    if applicable(promote_rule, T, S)
        return promote_rule(T,S)
    elseif applicable(promote_rule, S, T)
        return promote_rule(S,T)
    else
        return typejoin(T,S)
    end
end

promote() = ()
promote(x) = (x,)
function promote{T,S}(x::T, y::S)
    (convert(promote_type(T,S),x), convert(promote_type(T,S),y))
end
function promote{T,S,U}(x::T, y::S, z::U)
    R = promote_type(promote_type(T,S), U)
    convert((R...), (x, y, z))
end
function promote{T,S}(x::T, y::S, zs...)
    R = promote_type(T,S)
    for z in zs
        R = promote_type(R,typeof(z))
    end
    convert((R...), tuple(x,y,zs...))
end
# TODO: promote{T}(x::T, ys::T...) here to catch all circularities?

## promotions in arithmetic, etc. ##

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
rem(x::Real, y::Real) = rem(promote(x,y)...)
mod(x::Real, y::Real) = mod(promote(x,y)...)

mod1(x::Real, y::Real) = mod1(promote(x,y)...)
cmp(x::Real, y::Real) = cmp(promote(x,y)...)

max(x::Real, y::Real) = max(promote(x,y)...)
min(x::Real, y::Real) = min(promote(x,y)...)

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

=={T<:Number}(x::T, y::T) = no_op_err("==", T)
<{T<:Real}(x::T, y::T) = no_op_err("<", T)

max{T<:Real}(x::T, y::T) = y < x ? x : y
min{T<:Real}(x::T, y::T) = x < y ? x : y
