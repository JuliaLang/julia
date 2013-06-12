immutable Rational{T<:Integer} <: Real
    num::T
    den::T

    function Rational(num::T, den::T)
        if num == 0 && den == 0
            error("invalid rational: 0//0")
        end
        g = gcd(den, num)
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

function show(io::IO, x::Rational)
    if isinf(x)
        print(io, x.num > 0 ? "Inf" : "-Inf")
    else
        show(io, num(x)); print(io, "//"); show(io, den(x))
    end
end

convert{T<:Integer}(::Type{Rational{T}}, x::Rational) = Rational(convert(T,x.num),convert(T,x.den))
convert{T<:Integer}(::Type{Rational{T}}, x::Integer) = Rational(convert(T,x), convert(T,1))

convert(::Type{Rational}, x::Rational) = x
convert(::Type{Rational}, x::Integer) = convert(Rational{typeof(x)},x)

convert(::Type{Bool}, x::Rational) = (x!=0) # to resolve ambiguity
convert{T<:Integer}(::Type{T}, x::Rational) = (isinteger(x) ? convert(T, x.num) : throw(InexactError()))
convert{T<:FloatingPoint}(::Type{T}, x::Rational) = convert(T,x.num)/convert(T,x.den)

function convert{T<:Integer}(::Type{Rational{T}}, x::FloatingPoint)
    x == 0 && return zero(T)//one(T)
    if !isfinite(x)
        x < 0 && return -one(T)//zero(T)
        x > 0 && return +one(T)//zero(T)
        error(InexactError())
    end
    # TODO: handle values that can't be converted exactly
    p = get_precision(x)-1
    n = convert(T, significand(x)*2.0^p)
    p -= exponent(x)
    z = trailing_zeros(n)
    p - z > 0 ? (n>>z)//(one(T)<<(p-z)) :
        p > 0 ? (n>>p)//one(T) :
                (n<<-p)//one(T)
end
convert(::Type{Rational}, x::Union(Float64,Float32)) = convert(Rational{Int}, x)

promote_rule{T<:Integer}(::Type{Rational{T}}, ::Type{T}) = Rational{T}
promote_rule{T<:Integer,S<:Integer}(::Type{Rational{T}}, ::Type{S}) = Rational{promote_type(T,S)}
promote_rule{T<:Integer,S<:Integer}(::Type{Rational{T}}, ::Type{Rational{S}}) = Rational{promote_type(T,S)}

promote_rule{T<:SmallSigned,S<:Union(Float32,Float64)}(::Type{Rational{T}}, ::Type{S}) = Rational{Int}
promote_rule{T<:SmallUnsigned,S<:Union(Float32,Float64)}(::Type{Rational{T}}, ::Type{S}) = Rational{Uint}
promote_rule{T<:Union(Int64,Int128),S<:Union(Float32,Float64)}(::Type{Rational{T}}, ::Type{S}) = Rational{T}
promote_rule{T<:Union(Uint64,Uint128),S<:Union(Float32,Float64)}(::Type{Rational{T}}, ::Type{S}) = Rational{T}

function rationalize{T<:Integer}(::Type{T}, x::FloatingPoint; tol::Real=eps(x))
    if isnan(x);       return zero(T)//zero(T); end
    if x < typemin(T); return -one(T)//zero(T); end
    if typemax(T) < x; return  one(T)//zero(T); end
    tm = x < 0 ? typemin(T) : typemax(T)
    z = x*tm
    if z <= 0.5 return zero(T)//one(T) end
    if z <= 1.0 return one(T)//tm end
    y = x
    a = d = 1
    b = c = 0
    while true
        f = itrunc(y); y -= f
        p, q = f*a+c, f*b+d
        typemin(T) <= p <= typemax(T) &&
        typemin(T) <= q <= typemax(T) || break
        a, b, c, d = p, q, a, b
        if y == 0 || abs(a/b-x) <= tol
            break
        end
        y = inv(y)
    end
    return convert(T,a)//convert(T,b)
end
rationalize(x::Union(Float64,Float32); tol::Real=eps(x)) = rationalize(Int, x, tol=tol)

num(x::Integer) = x
den(x::Integer) = one(x)
num(x::Rational) = x.num
den(x::Rational) = x.den

sign(x::Rational) = sign(x.num)
signbit(x::Rational) = signbit(x.num)
copysign(x::Rational, y::Real) = copysign(x.num,y) // x.den
copysign(x::Rational, y::Rational) = copysign(x.num,y.num) // x.den

isnan(x::Rational) = false
isinf(x::Rational) = x.den == 0
isfinite(x::Rational) = x.den != 0

typemin{T<:Integer}(::Type{Rational{T}}) = -one(T)//zero(T)
typemax{T<:Integer}(::Type{Rational{T}}) = one(T)//zero(T)

isinteger(x::Rational) = x.den == 1
isfloat64(x::Rational) = ispow2(x.den) & (abs(x.num) <= x.den*maxintfloat(Float64))

hash(x::Rational) = isinteger(x) ? hash(x.num) :
                    isfloat64(x) ? hash(float64(x)) :
                    bitmix(hash(x.num), hash(x.den))

-(x::Rational) = (-x.num) // x.den
+(x::Rational, y::Rational) = (x.num*y.den + x.den*y.num) // (x.den*y.den)
-(x::Rational, y::Rational) = (x.num*y.den - x.den*y.num) // (x.den*y.den)
*(x::Rational, y::Rational) = (x.num*y.num) // (x.den*y.den)
/(x::Rational, y::Rational) = (x.num*y.den) // (x.den*y.num)
/(x::Rational, z::Complex ) = inv(z/x)

==(x::Rational, y::Rational) = (x.den == y.den) & (x.num == y.num)
==(x::Rational, y::Integer ) = (x.den == 1) & (x.num == y)
==(x::Integer , y::Rational) = y == x

# needed to avoid ambiguity between ==(x::Real, z::Complex) and ==(x::Rational, y::Number)
==(z::Complex , x::Rational) = isreal(z) & (real(z) == x)
==(x::Rational, z::Complex ) = isreal(z) & (real(z) == x)

==(x::FloatingPoint, q::Rational) = ispow2(q.den) & (x == q.num/q.den) & (x*q.den == q.num)
==(q::Rational, x::FloatingPoint) = ispow2(q.den) & (x == q.num/q.den) & (x*q.den == q.num)

# TODO: fix inequalities to be in line with equality check
< (x::Rational, y::Rational) = x.den == y.den ? x.num < y.num : x.num*y.den < x.den*y.num
< (x::Rational, y::Real    ) = x.num < x.den*y
< (x::Real    , y::Rational) = x*y.den < y.num

<=(x::Rational, y::Rational) = x.den == y.den ? x.num <= y.num : x.num*y.den <= x.den*y.num
<=(x::Rational, y::Real    ) = x.num <= x.den*y
<=(x::Real    , y::Rational) = x*y.den <= y.num

rem(x::Rational, y::Rational) = Rational(rem(x.num*y.den, x.den*y.num),
                                         x.den*y.den)

mod(x::Rational, y::Rational) = Rational(mod(x.num*y.den, x.den*y.num),
                                         x.den*y.den)

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
