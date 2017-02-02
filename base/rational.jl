# This file is a part of Julia. License is MIT: http://julialang.org/license

immutable Rational{T<:Integer} <: Real
    num::T
    den::T

    function Rational{T}(num::Integer, den::Integer) where T<:Integer
        num == den == zero(T) && throw(ArgumentError("invalid rational: zero($T)//zero($T)"))
        g = den < 0 ? -gcd(den, num) : gcd(den, num)
        new(div(num, g), div(den, g))
    end
end
Rational(n::T, d::T) where T<:Integer = Rational{T}(n,d)
Rational(n::Integer, d::Integer) = Rational(promote(n,d)...)
Rational(n::Integer) = Rational(n,one(n))

function divgcd(x::Integer,y::Integer)
    g = gcd(x,y)
    div(x,g), div(y,g)
end

"""
    //(num, den)

Divide two integers or rational numbers, giving a `Rational` result.

```jldoctest
julia> 3 // 5
3//5

julia> (3 // 5) // (2 // 1)
3//10
```
"""
//(n::Integer,  d::Integer ) = Rational(n,d)

function //(x::Rational, y::Integer )
    xn,yn = divgcd(x.num,y)
    xn//checked_mul(x.den,yn)
end
function //(x::Integer,  y::Rational)
    xn,yn = divgcd(x,y.num)
    checked_mul(xn,y.den)//yn
end
function //(x::Rational, y::Rational)
    xn,yn = divgcd(x.num,y.num)
    xd,yd = divgcd(x.den,y.den)
    checked_mul(xn,yd)//checked_mul(xd,yn)
end

//(x::Complex,  y::Real) = complex(real(x)//y,imag(x)//y)
//(x::Number, y::Complex) = x*y'//abs2(y)


//(X::AbstractArray, y::Number) = X .// y

function show(io::IO, x::Rational)
    show(io, numerator(x))
    print(io, "//")
    show(io, denominator(x))
end

function read{T<:Integer}(s::IO, ::Type{Rational{T}})
    r = read(s,T)
    i = read(s,T)
    r//i
end
function write(s::IO, z::Rational)
    write(s,numerator(z),denominator(z))
end

convert{T<:Integer}(::Type{Rational{T}}, x::Rational) = Rational{T}(convert(T,x.num),convert(T,x.den))
convert{T<:Integer}(::Type{Rational{T}}, x::Integer) = Rational{T}(convert(T,x), convert(T,1))

convert(::Type{Rational}, x::Rational) = x
convert(::Type{Rational}, x::Integer) = convert(Rational{typeof(x)},x)

convert(::Type{Bool}, x::Rational) = x==0 ? false : x==1 ? true : throw(InexactError()) # to resolve ambiguity
convert(::Type{Integer}, x::Rational) = (isinteger(x) ? convert(Integer, x.num) : throw(InexactError()))
convert{T<:Integer}(::Type{T}, x::Rational) = (isinteger(x) ? convert(T, x.num) : throw(InexactError()))

convert(::Type{AbstractFloat}, x::Rational) = float(x.num)/float(x.den)
function convert{T<:AbstractFloat,S}(::Type{T}, x::Rational{S})
    P = promote_type(T,S)
    convert(T, convert(P,x.num)/convert(P,x.den))
end

function convert{T<:Integer}(::Type{Rational{T}}, x::AbstractFloat)
    r = rationalize(T, x, tol=0)
    x == convert(typeof(x), r) || throw(InexactError())
    r
end
convert(::Type{Rational}, x::Float64) = convert(Rational{Int64}, x)
convert(::Type{Rational}, x::Float32) = convert(Rational{Int}, x)

big{T<:Integer}(z::Complex{Rational{T}}) = Complex{Rational{BigInt}}(z)

promote_rule{T<:Integer,S<:Integer}(::Type{Rational{T}}, ::Type{S}) = Rational{promote_type(T,S)}
promote_rule{T<:Integer,S<:Integer}(::Type{Rational{T}}, ::Type{Rational{S}}) = Rational{promote_type(T,S)}
promote_rule{T<:Integer,S<:AbstractFloat}(::Type{Rational{T}}, ::Type{S}) = promote_type(T,S)

widen{T}(::Type{Rational{T}}) = Rational{widen(T)}

"""
    rationalize([T<:Integer=Int,] x; tol::Real=eps(x))

Approximate floating point number `x` as a `Rational` number with components
of the given integer type. The result will differ from `x` by no more than `tol`.
If `T` is not provided, it defaults to `Int`.

```jldoctest
julia> rationalize(5.6)
28//5

julia> a = rationalize(BigInt, 10.3)
103//10

julia> typeof(numerator(a))
BigInt
```
"""
function rationalize{T<:Integer}(::Type{T}, x::AbstractFloat, tol::Real)
    if tol < 0
        throw(ArgumentError("negative tolerance $tol"))
    end
    isnan(x) && return zero(T)//zero(T)
    isinf(x) && return (x < 0 ? -one(T) : one(T))//zero(T)

    p,  q  = (x < 0 ? -one(T) : one(T)), zero(T)
    pp, qq = zero(T), one(T)

    x = abs(x)
    a = trunc(x)
    r = x-a
    y = one(x)

    tolx = oftype(x, tol)
    nt, t, tt = tolx, zero(tolx), tolx
    ia = np = nq = zero(T)

    # compute the successive convergents of the continued fraction
    #  np // nq = (p*a + pp) // (q*a + qq)
    while r > nt
        try
            ia = convert(T,a)

            np = checked_add(checked_mul(ia,p),pp)
            nq = checked_add(checked_mul(ia,q),qq)
            p, pp = np, p
            q, qq = nq, q
        catch e
            isa(e,InexactError) || isa(e,OverflowError) || rethrow(e)
            return p // q
        end

        # naive approach of using
        #   x = 1/r; a = trunc(x); r = x - a
        # is inexact, so we store x as x/y
        x, y = y, r
        a, r = divrem(x,y)

        # maintain
        # x0 = (p + (-1)^i * r) / q
        t, tt = nt, t
        nt = a*t+tt
    end

    # find optimal semiconvergent
    # smallest a such that x-a*y < a*t+tt
    a = cld(x-tt,y+t)
    try
        ia = convert(T,a)
        np = checked_add(checked_mul(ia,p),pp)
        nq = checked_add(checked_mul(ia,q),qq)
        return np // nq
    catch e
        isa(e,InexactError) || isa(e,OverflowError) || rethrow(e)
        return p // q
    end
end
rationalize{T<:Integer}(::Type{T}, x::AbstractFloat; tol::Real=eps(x)) = rationalize(T, x, tol)::Rational{T}
rationalize(x::AbstractFloat; kvs...) = rationalize(Int, x; kvs...)

"""
    numerator(x)

Numerator of the rational representation of `x`.

```jldoctest
julia> numerator(2//3)
2

julia> numerator(4)
4
```
"""
numerator(x::Integer) = x
numerator(x::Rational) = x.num

"""
    denominator(x)

Denominator of the rational representation of `x`.

```jldoctest
julia> denominator(2//3)
3

julia> denominator(4)
1
```
"""
denominator(x::Integer) = one(x)
denominator(x::Rational) = x.den

sign(x::Rational) = oftype(x, sign(x.num))
signbit(x::Rational) = signbit(x.num)
copysign(x::Rational, y::Real) = copysign(x.num,y) // x.den
copysign(x::Rational, y::Rational) = copysign(x.num,y.num) // x.den

typemin{T<:Integer}(::Type{Rational{T}}) = -one(T)//zero(T)
typemax{T<:Integer}(::Type{Rational{T}}) = one(T)//zero(T)

isinteger(x::Rational) = x.den == 1

-(x::Rational) = (-x.num) // x.den
function -{T<:Signed}(x::Rational{T})
    x.num == typemin(T) && throw(OverflowError())
    (-x.num) // x.den
end
function -{T<:Unsigned}(x::Rational{T})
    x.num != zero(T) && throw(OverflowError())
    x
end

for (op,chop) in ((:+,:checked_add), (:-,:checked_sub),
                  (:rem,:rem), (:mod,:mod))
    @eval begin
        function ($op)(x::Rational, y::Rational)
            xd, yd = divgcd(x.den, y.den)
            Rational(($chop)(checked_mul(x.num,yd), checked_mul(y.num,xd)), checked_mul(x.den,yd))
        end
    end
end

function *(x::Rational, y::Rational)
    xn,yd = divgcd(x.num,y.den)
    xd,yn = divgcd(x.den,y.num)
    checked_mul(xn,yn) // checked_mul(xd,yd)
end
/(x::Rational, y::Rational) = x//y
/{T<:Union{Integer,Rational}}(x::Rational, y::Complex{T}) = x//y

fma(x::Rational, y::Rational, z::Rational) = x*y+z

==(x::Rational, y::Rational) = (x.den == y.den) & (x.num == y.num)
<( x::Rational, y::Rational) = x.den == y.den ? x.num < y.num :
                               widemul(x.num,y.den) < widemul(x.den,y.num)
<=(x::Rational, y::Rational) = x.den == y.den ? x.num <= y.num :
                               widemul(x.num,y.den) <= widemul(x.den,y.num)


==(x::Rational, y::Integer ) = (x.den == 1) & (x.num == y)
==(x::Integer , y::Rational) = y == x
<( x::Rational, y::Integer ) = x.num < widemul(x.den,y)
<( x::Integer , y::Rational) = widemul(x,y.den) < y.num
<=(x::Rational, y::Integer ) = x.num <= widemul(x.den,y)
<=(x::Integer , y::Rational) = widemul(x,y.den) <= y.num

function ==(x::AbstractFloat, q::Rational)
    if isfinite(x)
        (count_ones(q.den) == 1) & (x*q.den == q.num)
    else
        x == q.num/q.den
    end
end

==(q::Rational, x::AbstractFloat) = x == q

for rel in (:<,:<=,:cmp)
    for (Tx,Ty) in ((Rational,AbstractFloat), (AbstractFloat,Rational))
        @eval function ($rel)(x::$Tx, y::$Ty)
            if isnan(x) || isnan(y)
                $(rel == :cmp ? :(throw(DomainError())) : :(return false))
            end

            xn, xp, xd = decompose(x)
            yn, yp, yd = decompose(y)

            if xd < 0
                xn = -xn
                xd = -xd
            end
            if yd < 0
                yn = -yn
                yd = -yd
            end

            xc, yc = widemul(xn,yd), widemul(yn,xd)
            xs, ys = sign(xc), sign(yc)

            if xs != ys
                return ($rel)(xs,ys)
            elseif xs == 0
                # both are zero or ±Inf
                return ($rel)(xn,yn)
            end

            xb, yb = ndigits0z(xc,2) + xp, ndigits0z(yc,2) + yp

            if xb == yb
                xc, yc = promote(xc,yc)
                if xp > yp
                    xc = (xc<<(xp-yp))
                else
                    yc = (yc<<(yp-xp))
                end
                return ($rel)(xc,yc)
            else
                return xc > 0 ? ($rel)(xb,yb) : ($rel)(yb,xb)
            end
        end
    end
end

# needed to avoid ambiguity between ==(x::Real, z::Complex) and ==(x::Rational, y::Number)
==(z::Complex , x::Rational) = isreal(z) & (real(z) == x)
==(x::Rational, z::Complex ) = isreal(z) & (real(z) == x)

for op in (:div, :fld, :cld)
    @eval begin
        function ($op)(x::Rational, y::Integer )
            xn,yn = divgcd(x.num,y)
            ($op)(xn, checked_mul(x.den,yn))
        end
        function ($op)(x::Integer,  y::Rational)
            xn,yn = divgcd(x,y.num)
            ($op)(checked_mul(xn,y.den), yn)
        end
        function ($op)(x::Rational, y::Rational)
            xn,yn = divgcd(x.num,y.num)
            xd,yd = divgcd(x.den,y.den)
            ($op)(checked_mul(xn,yd), checked_mul(xd,yn))
        end
    end
end

trunc{T}(::Type{T}, x::Rational) = convert(T,div(x.num,x.den))
floor{T}(::Type{T}, x::Rational) = convert(T,fld(x.num,x.den))
ceil{ T}(::Type{T}, x::Rational) = convert(T,cld(x.num,x.den))


function round{T, Tr}(::Type{T}, x::Rational{Tr}, ::RoundingMode{:Nearest})
    if denominator(x) == zero(Tr) && T <: Integer
        throw(DivideError())
    elseif denominator(x) == zero(Tr)
        return convert(T, copysign(one(Tr)//zero(Tr), numerator(x)))
    end
    q,r = divrem(numerator(x), denominator(x))
    s = q
    if abs(r) >= abs((denominator(x)-copysign(Tr(4), numerator(x))+one(Tr)+iseven(q))>>1 + copysign(Tr(2), numerator(x)))
        s += copysign(one(Tr),numerator(x))
    end
    convert(T, s)
end

round{T}(::Type{T}, x::Rational) = round(T, x, RoundNearest)

function round{T, Tr}(::Type{T}, x::Rational{Tr}, ::RoundingMode{:NearestTiesAway})
    if denominator(x) == zero(Tr) && T <: Integer
        throw(DivideError())
    elseif denominator(x) == zero(Tr)
        return convert(T, copysign(one(Tr)//zero(Tr), numerator(x)))
    end
    q,r = divrem(numerator(x), denominator(x))
    s = q
    if abs(r) >= abs((denominator(x)-copysign(Tr(4), numerator(x))+one(Tr))>>1 + copysign(Tr(2), numerator(x)))
        s += copysign(one(Tr),numerator(x))
    end
    convert(T, s)
end

function round{T, Tr}(::Type{T}, x::Rational{Tr}, ::RoundingMode{:NearestTiesUp})
    if denominator(x) == zero(Tr) && T <: Integer
        throw(DivideError())
    elseif denominator(x) == zero(Tr)
        return convert(T, copysign(one(Tr)//zero(Tr), numerator(x)))
    end
    q,r = divrem(numerator(x), denominator(x))
    s = q
    if abs(r) >= abs((denominator(x)-copysign(Tr(4), numerator(x))+one(Tr)+(numerator(x)<0))>>1 + copysign(Tr(2), numerator(x)))
        s += copysign(one(Tr),numerator(x))
    end
    convert(T, s)
end

function round{T}(::Type{T}, x::Rational{Bool})
    if denominator(x) == false && issubtype(T, Union{Integer, Bool})
        throw(DivideError())
    end
    convert(T, x)
end

round{T}(::Type{T}, x::Rational{Bool}, ::RoundingMode{:Nearest}) = round(T, x)
round{T}(::Type{T}, x::Rational{Bool}, ::RoundingMode{:NearestTiesAway}) = round(T, x)
round{T}(::Type{T}, x::Rational{Bool}, ::RoundingMode{:NearestTiesUp}) = round(T, x)
round{T}(::Type{T}, x::Rational{Bool}, ::RoundingMode) = round(T, x)

trunc{T}(x::Rational{T}) = Rational(trunc(T,x))
floor{T}(x::Rational{T}) = Rational(floor(T,x))
ceil{ T}(x::Rational{T}) = Rational(ceil(T,x))
round{T}(x::Rational{T}) = Rational(round(T,x))

function ^(x::Rational, n::Integer)
    n >= 0 ? power_by_squaring(x,n) : power_by_squaring(inv(x),-n)
end

^(x::Number, y::Rational) = x^(y.num/y.den)
^{T<:AbstractFloat}(x::T, y::Rational) = x^convert(T,y)
^{T<:AbstractFloat}(x::Complex{T}, y::Rational) = x^convert(T,y)

^{T<:Rational}(z::Complex{T}, n::Bool) = n ? z : one(z) # to resolve ambiguity
function ^{T<:Rational}(z::Complex{T}, n::Integer)
    n >= 0 ? power_by_squaring(z,n) : power_by_squaring(inv(z),-n)
end

iszero(x::Rational) = iszero(numerator(x))

function lerpi(j::Integer, d::Integer, a::Rational, b::Rational)
    ((d-j)*a)/d + (j*b)/d
end
