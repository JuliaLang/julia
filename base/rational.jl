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

//(n::Integer,  d::Integer ) = Rational(n,d)
//(x::Rational, y::Integer ) = x.num//(x.den*y)
//(x::Integer,  y::Rational) = (x*y.den)//y.num
//(x::Rational, y::Rational) = (x.num*y.den)//(x.den*y.num)
//(x::Complex,  y::Real    ) = complex(real(x)//y,imag(x)//y)
//(x::Real,     y::Complex ) = x*y'//real(y*y')

function //(x::Complex, y::Complex)
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
    y = widen(x)
    a::T = d::T = 1
    b::T = c::T = 0
    ok(r,s) = abs(oftype(x,r)/oftype(x,s) - x) <= tol
    local n
    const max_n = 1000
    for n = 0:max_n
        local f::T, p::T, q::T
        try
            f = itrunc(T,y)
            abs(y - f) < 1 || throw(InexactError()) # XXX: remove when #3040 is fixed
        catch e
            isa(e,InexactError) || rethrow(e)
            n == 0 && return (x < 0 ? -one(T) : one(T))//zero(T)
            break
        end
        y -= f
        try
            p = checked_add(checked_mul(f,a), c)
            q = checked_add(checked_mul(f,b), d)
        catch e
            isa(e,OverflowError) || rethrow(e)
            break
        end
        if y == 0 || ok(p, q)
            if n > 0 && f > 1
                # check semi-convergents
                u::T, v::T = iceil(f/2), f
                p, q = u*a+c, u*b+d
                ok(p, q) && return p//q
                while u + 1 < v
                    m = div(u+v, 2)
                    p, q = m*a+c, m*b+d
                    if ok(p, q)
                        v = m
                    else
                        u = m
                    end
                end
                p, q = v*a+c, v*b+d
            end
            return p//q
        end
        a, b, c, d = p, q, a, b
        y = inv(y)
    end
    @assert n < max_n
    return a//b
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
for op in (:+, :-, :rem, :mod)
    @eval begin
        function ($op)(x::Rational, y::Rational)
            g = gcd(x.den, y.den)
            Rational(($op)(x.num * div(y.den, g), y.num * div(x.den, g)), x.den * div(y.den, g))
        end
    end
end
*(x::Rational, y::Rational) = (x.num*y.num) // (x.den*y.den)
/(x::Rational, y::Rational) = (x.num*y.den) // (x.den*y.num)
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

div(x::Rational, y::Rational) = div(x.num*y.den, x.den*y.num)
div(x::Rational, y::Real    ) = div(x.num, x.den*y)
div(x::Real    , y::Rational) = div(x*y.den, y.num)

fld(x::Rational, y::Rational) = fld(x.num*y.den, x.den*y.num)
fld(x::Rational, y::Real    ) = fld(x.num, x.den*y)
fld(x::Real    , y::Rational) = fld(x*y.den, y.num)

itrunc(x::Rational) = div(x.num,x.den)
ifloor(x::Rational) = fld(x.num,x.den)
iceil (x::Rational) = -fld(-x.num,x.den)
iround(x::Rational) = div(x.num*2 + copysign(x.den,x.num), x.den*2)

trunc(x::Rational) = Rational(itrunc(x))
floor(x::Rational) = Rational(ifloor(x))
ceil (x::Rational) = Rational(iceil(x))
round(x::Rational) = Rational(iround(x))

## rational to int coercion ##

for f in (:int8, :int16, :int32, :int64, :int128,
          :uint8, :uint16, :uint32, :uint64, :uint128,
          :signed, :integer, :unsigned, :int, :uint)
    @eval ($f)(x::Rational) = ($f)(iround(x))
end

^(x::Rational, y::Integer) = y < 0 ?
    Rational(x.den^-y, x.num^-y) : Rational(x.num^y, x.den^y)

^(x::Number, y::Rational) = x^(y.num/y.den)
^{T<:FloatingPoint}(x::T, y::Rational) = x^(convert(T,y.num)/y.den)
^{T<:FloatingPoint}(x::Complex{T}, y::Rational) = x^(convert(T,y.num)/y.den)

^{T<:Rational}(z::Complex{T}, n::Bool) = n ? z : one(z) # to resolve ambiguity
^{T<:Rational}(z::Complex{T}, n::Integer) =
    n>=0 ? power_by_squaring(z,n) : power_by_squaring(inv(z),-n)
