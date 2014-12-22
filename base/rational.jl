immutable Rational{T<:Integer} <: Real
    num::T
    den::T

    function Rational(num::T, den::T)
        num == den == 0 && error("invalid rational: 0//0")
        g = den < 0 ? -gcd(den, num) : gcd(den, num)
        new(div(num, g), div(den, g))
    end
end
Rational{T<:Integer}(n::T, d::T) = Rational{T}(n,d)
Rational(n::Integer, d::Integer) = Rational(promote(n,d)...)
Rational(n::Integer) = Rational(n,one(n))

function divgcd(x::Integer,y::Integer)
    g = gcd(x,y)
    div(x,g), div(y,g)
end

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

//(x::Complex,  y::Real    ) = complex(real(x)//y,imag(x)//y)
function //(x::Number, y::Complex)
    xr = complex(Rational(real(x)),Rational(imag(x)))
    yr = complex(Rational(real(y)),Rational(imag(y)))
    xr // yr
end
function //{Ra<:Rational,Rb<:Rational}(x::Complex{Ra}, y::Complex{Rb})
    xy = x*y'
    yy = real(y*y')
    complex(real(xy)//yy, imag(xy)//yy)
end

//(X::AbstractArray, y::Number) = X .// y
.//(X::AbstractArray, y::Number) = reshape([ x // y for x in X ], size(X))
.//(y::Number, X::AbstractArray) = reshape([ y // x for x in X ], size(X))

function show(io::IO, x::Rational)
    show(io, num(x))
    print(io, "//")
    show(io, den(x))
end

convert{T<:Integer}(::Type{Rational{T}}, x::Rational) = Rational{T}(convert(T,x.num),convert(T,x.den))
convert{T<:Integer}(::Type{Rational{T}}, x::Integer) = Rational{T}(convert(T,x), convert(T,1))

convert(::Type{Rational}, x::Rational) = x
convert(::Type{Rational}, x::Integer) = convert(Rational{typeof(x)},x)

convert(::Type{Bool}, x::Rational) = (x!=0) # to resolve ambiguity
convert{T<:Integer}(::Type{T}, x::Rational) = (isinteger(x) ? convert(T, x.num) : throw(InexactError()))

convert(::Type{FloatingPoint}, x::Rational) = float(x.num)/float(x.den)
function convert{T<:FloatingPoint,S}(::Type{T}, x::Rational{S})
    P = promote_type(T,S)
    convert(T, convert(P,x.num)/convert(P,x.den))
end

function convert{T<:Integer}(::Type{Rational{T}}, x::FloatingPoint)
    r = rationalize(T, x, tol=0)
    x == convert(typeof(x), r) || throw(InexactError())
    r
end
convert(::Type{Rational}, x::Float64) = convert(Rational{Int64}, x)
convert(::Type{Rational}, x::Float32) = convert(Rational{Int}, x)

promote_rule{T<:Integer,S<:Integer}(::Type{Rational{T}}, ::Type{S}) = Rational{promote_type(T,S)}
promote_rule{T<:Integer,S<:Integer}(::Type{Rational{T}}, ::Type{Rational{S}}) = Rational{promote_type(T,S)}
promote_rule{T<:Integer,S<:FloatingPoint}(::Type{Rational{T}}, ::Type{S}) = promote_type(T,S)

widen{T}(::Type{Rational{T}}) = Rational{widen(T)}

function rationalize{T<:Integer}(::Type{T}, x::FloatingPoint; tol::Real=eps(x))
    tol < 0 && throw(ArgumentError("negative tolerance"))
    isnan(x) && return zero(T)//zero(T)
    isinf(x) && return (x < 0 ? -one(T) : one(T))//zero(T)

    p,  q  = (x < 0 ? -one(T) : one(T)), zero(T)
    pp, qq = zero(T), one(T)

    x = abs(x)
    a = trunc(x)
    r = x-a
    y = one(x)

    nt, t, tt = tol, zero(tol), zero(tol)

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

        t, tt = nt, t
        x, y = y, r

        a, r = divrem(x,y)
        nt = a*t + tt
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
rationalize(x::FloatingPoint; kvs...) = rationalize(Int, x; kvs...)

num(x::Integer) = x
den(x::Integer) = one(x)
num(x::Rational) = x.num
den(x::Rational) = x.den

sign(x::Rational) = sign(x.num)
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
/(x::Rational, z::Complex ) = inv(z/x)

==(x::Rational, y::Rational) = (x.den == y.den) & (x.num == y.num)
==(x::Rational, y::Integer ) = (x.den == 1) & (x.num == y)
==(x::Integer , y::Rational) = y == x

# needed to avoid ambiguity between ==(x::Real, z::Complex) and ==(x::Rational, y::Number)
==(z::Complex , x::Rational) = isreal(z) & (real(z) == x)
==(x::Rational, z::Complex ) = isreal(z) & (real(z) == x)

==(x::FloatingPoint, q::Rational) = count_ones(q.den) <= 1 & (x == q.num/q.den) & (x*q.den == q.num)
==(q::Rational, x::FloatingPoint) = count_ones(q.den) <= 1 & (x == q.num/q.den) & (x*q.den == q.num)

# TODO: fix inequalities to be in line with equality check
< (x::Rational, y::Rational) = x.den == y.den ? x.num < y.num :
                               widemul(x.num,y.den) < widemul(x.den,y.num)
< (x::Rational, y::Integer ) = x.num < widemul(x.den,y)
< (x::Rational, y::Real    ) = x.num < x.den*y
< (x::Integer , y::Rational) = widemul(x,y.den) < y.num
< (x::Real    , y::Rational) = x*y.den < y.num

<=(x::Rational, y::Rational) = x.den == y.den ? x.num <= y.num :
                               widemul(x.num,y.den) <= widemul(x.den,y.num)
<=(x::Rational, y::Integer ) = x.num <= widemul(x.den,y)
<=(x::Rational, y::Real    ) = x.num <= x.den*y
<=(x::Integer , y::Rational) = widemul(x,y.den) <= y.num
<=(x::Real    , y::Rational) = x*y.den <= y.num

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
ceil {T}(::Type{T}, x::Rational) = convert(T,cld(x.num,x.den))

function round{T}(::Type{T}, x::Rational, ::RoundingMode{:Nearest})
    q,r = divrem(x.num,x.den)
    s = abs(r) < (x.den+one(x.den)+iseven(q))>>1 ? q : q+copysign(one(q),x.num)
    convert(T,s)
end
round{T}(::Type{T}, x::Rational) = round(T,x,RoundNearest)
function round{T}(::Type{T}, x::Rational, ::RoundingMode{:NearestTiesAway})
    q,r = divrem(x.num,x.den)
    s = abs(r) < (x.den+one(x.den))>>1 ? q : q+copysign(one(q),x.num)
    convert(T,s)
end
function round{T}(::Type{T}, x::Rational, ::RoundingMode{:NearestTiesUp})
    q,r = divrem(x.num,x.den)
    s = abs(r) < (x.den+one(x.den)+(x.num<0))>>1 ? q : q+copysign(one(q),x.num)
    convert(T,s)
end

trunc{T}(x::Rational{T}) = Rational(trunc(T,x))
floor{T}(x::Rational{T}) = Rational(floor(T,x))
ceil {T}(x::Rational{T}) = Rational(ceil(T,x))
round{T}(x::Rational{T}) = Rational(round(T,x))

## rational to int coercion ##

for f in (:int8, :int16, :int32, :int64, :int128,
          :uint8, :uint16, :uint32, :uint64, :uint128,
          :signed, :integer, :unsigned, :int, :uint)
    @eval ($f)(x::Rational) = ($f)(round(Integer,x))
end

function ^(x::Rational, n::Integer)
    n >= 0 ? power_by_squaring(x,n) : power_by_squaring(inv(x),-n)
end

^(x::Number, y::Rational) = x^(y.num/y.den)
^{T<:FloatingPoint}(x::T, y::Rational) = x^(convert(T,y.num)/y.den)
^{T<:FloatingPoint}(x::Complex{T}, y::Rational) = x^(convert(T,y.num)/y.den)

^{T<:Rational}(z::Complex{T}, n::Bool) = n ? z : one(z) # to resolve ambiguity
function ^{T<:Rational}(z::Complex{T}, n::Integer)
    n >= 0 ? power_by_squaring(z,n) : power_by_squaring(inv(z),-n)
end
