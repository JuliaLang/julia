module GMP

export BigInt

import Base: *, +, -, /, <, <<, >>, >>>, <=, ==, >, >=, ^, (~), (&), (|), ($),
             binomial, cmp, convert, div, factorial, fld, gcd, gcdx, lcm, mod,
             ndigits, promote_rule, rem, show, isqrt, string, isprime, powermod,
             widemul, sum, trailing_zeros, trailing_ones, count_ones

type BigInt <: Integer
    alloc::Cint
    size::Cint
    d::Ptr{Void}
    function BigInt()
        b = new(zero(Cint), zero(Cint), C_NULL)
        ccall((:__gmpz_init,:libgmp), Void, (Ptr{BigInt},), &b)
        finalizer(b, BigInt_clear)
        return b
    end
end
BigInt_clear(mpz::BigInt) = ccall((:__gmpz_clear, :libgmp), Void, (Ptr{BigInt},), &mpz)

BigInt(x::BigInt) = x
function BigInt(x::String)
    z = BigInt()
    err = ccall((:__gmpz_set_str, :libgmp), Int32, (Ptr{BigInt}, Ptr{Uint8}, Int32), &z, bytestring(x), 0)
    if err != 0; error("Invalid input"); end
    return z
end

function BigInt(x::Clong)
    z = BigInt()
    ccall((:__gmpz_set_si, :libgmp), Void, (Ptr{BigInt}, Clong), &z, x)
    return z
end
function BigInt(x::Culong)
    z = BigInt()
    ccall((:__gmpz_set_ui, :libgmp), Void,(Ptr{BigInt}, Culong), &z, x)
    return z
end

BigInt(x::Bool) = BigInt(uint(x))
BigInt(x::Integer) =
    typemin(Clong) <= x <= typemax(Clong) ? BigInt(convert(Clong,x)) : BigInt(string(x))
BigInt(x::Unsigned) =
    x <= typemax(Culong) ? BigInt(convert(Culong,x)) : BigInt(string(x))

convert(::Type{BigInt}, x::Integer) = BigInt(x)

convert(::Type{Int64}, n::BigInt) = int64(convert(Clong, n))
convert(::Type{Int32}, n::BigInt) = int32(convert(Clong, n))
convert(::Type{Int16}, n::BigInt) = int16(convert(Clong, n))
convert(::Type{Int8}, n::BigInt) = int8(convert(Clong, n))

function convert(::Type{Clong}, n::BigInt)
    fits = ccall((:__gmpz_fits_slong_p, :libgmp), Int32, (Ptr{BigInt},), &n) != 0
    if fits
        convert(Int, ccall((:__gmpz_get_si, :libgmp), Clong, (Ptr{BigInt},), &n))
    else
        throw(InexactError())
    end
end

convert(::Type{Uint64}, x::BigInt) = uint64(convert(Culong, x))
convert(::Type{Uint32}, x::BigInt) = uint32(convert(Culong, x))
convert(::Type{Uint16}, x::BigInt) = uint16(convert(Culong, x))
convert(::Type{Uint8}, x::BigInt) = uint8(convert(Culong, x))

function convert(::Type{Culong}, n::BigInt)
    fits = ccall((:__gmpz_fits_ulong_p, :libgmp), Int32, (Ptr{BigInt},), &n) != 0
    if fits
        convert(Uint, ccall((:__gmpz_get_ui, :libgmp), Culong, (Ptr{BigInt},), &n))
    else
        throw(InexactError())
    end
end

if sizeof(Int32) == sizeof(Clong)
    function convert(::Type{Uint128}, x::BigInt)
        uint128(uint(x>>>96))<<96 +
        uint128(uint((x>>>64) & typemax(Uint32)))<<64 +
        uint128(uint((x>>>32) & typemax(Uint32)))<<32 +
        uint128(uint(x & typemax(Uint32)))
    end
end
if sizeof(Int64) == sizeof(Clong)
    function convert(::Type{Uint128}, x::BigInt)
        uint128(uint(ax>>>64))<<64 +
        uint128(uint(ax & typemax(Uint64)))
    end
end
convert(::Type{Int128}, x::BigInt) = copysign(int128(uint128(abs(x))),x)

promote_rule{T<:Integer}(::Type{BigInt}, ::Type{T}) = BigInt

# Binary ops
for (fJ, fC) in ((:+, :add), (:-,:sub), (:*, :mul),
                 (:fld, :fdiv_q), (:div, :tdiv_q), (:mod, :fdiv_r), (:rem, :tdiv_r),
                 (:gcd, :gcd), (:lcm, :lcm),
                 (:&, :and), (:|, :ior), (:$, :xor))
    @eval begin
        function ($fJ)(x::BigInt, y::BigInt)
            z = BigInt()
            ccall(($(string(:__gmpz_,fC)), :libgmp), Void, (Ptr{BigInt}, Ptr{BigInt}, Ptr{BigInt}), &z, &x, &y)
            return z
        end
    end
end

# More efficient commutative operations
for (fJ, fC) in ((:+, :add), (:*, :mul), (:&, :and), (:|, :ior), (:$, :xor))
    @eval begin
        function ($fJ)(a::BigInt, b::BigInt, c::BigInt)
            z = BigInt()
            ccall(($(string(:__gmpz_,fC)), :libgmp), Void, (Ptr{BigInt}, Ptr{BigInt}, Ptr{BigInt}), &z, &a, &b)
            ccall(($(string(:__gmpz_,fC)), :libgmp), Void, (Ptr{BigInt}, Ptr{BigInt}, Ptr{BigInt}), &z, &z, &c)
            return z
        end
        function ($fJ)(a::BigInt, b::BigInt, c::BigInt, d::BigInt)
            z = BigInt()
            ccall(($(string(:__gmpz_,fC)), :libgmp), Void, (Ptr{BigInt}, Ptr{BigInt}, Ptr{BigInt}), &z, &a, &b)
            ccall(($(string(:__gmpz_,fC)), :libgmp), Void, (Ptr{BigInt}, Ptr{BigInt}, Ptr{BigInt}), &z, &z, &c)
            ccall(($(string(:__gmpz_,fC)), :libgmp), Void, (Ptr{BigInt}, Ptr{BigInt}, Ptr{BigInt}), &z, &z, &d)
            return z
        end
        function ($fJ)(a::BigInt, b::BigInt, c::BigInt, d::BigInt, e::BigInt)
            z = BigInt()
            ccall(($(string(:__gmpz_,fC)), :libgmp), Void, (Ptr{BigInt}, Ptr{BigInt}, Ptr{BigInt}), &z, &a, &b)
            ccall(($(string(:__gmpz_,fC)), :libgmp), Void, (Ptr{BigInt}, Ptr{BigInt}, Ptr{BigInt}), &z, &z, &c)
            ccall(($(string(:__gmpz_,fC)), :libgmp), Void, (Ptr{BigInt}, Ptr{BigInt}, Ptr{BigInt}), &z, &z, &d)
            ccall(($(string(:__gmpz_,fC)), :libgmp), Void, (Ptr{BigInt}, Ptr{BigInt}, Ptr{BigInt}), &z, &z, &e)
            return z
        end
    end
end

# Basic arithmetic without promotion
function +(x::BigInt, c::Culong)
    z = BigInt()
    ccall((:__gmpz_add_ui, :libgmp), Void, (Ptr{BigInt}, Ptr{BigInt}, Culong), &z, &x, c)
    return z
end
+(c::Culong, x::BigInt) = x + c
+(c::Unsigned, x::BigInt) = x + convert(Culong, c)
+(x::BigInt, c::Unsigned) = x + convert(Culong, c)
+(x::BigInt, c::Signed) = c < 0 ? -(x, convert(Culong, -c)) : x + convert(Culong, c)
+(c::Signed, x::BigInt) = c < 0 ? -(x, convert(Culong, -c)) : x + convert(Culong, c)

function -(x::BigInt, c::Culong)
    z = BigInt()
    ccall((:__gmpz_sub_ui, :libgmp), Void, (Ptr{BigInt}, Ptr{BigInt}, Culong), &z, &x, c)
    return z
end
function -(c::Culong, x::BigInt)
    z = BigInt()
    ccall((:__gmpz_ui_sub, :libgmp), Void, (Ptr{BigInt}, Culong, Ptr{BigInt}), &z, c, &x)
    return z
end
-(x::BigInt, c::Unsigned) = -(x, convert(Culong, c))
-(c::Unsigned, x::BigInt) = -(convert(Culong, c), x)
-(x::BigInt, c::Signed) = c < 0 ? +(x, convert(Culong, -c)) : -(x, convert(Culong, c))
-(c::Signed, x::BigInt) = c < 0 ? -(x + convert(Culong, -c)) : -(convert(Culong, c), x)

function *(x::BigInt, c::Culong)
    z = BigInt()
    ccall((:__gmpz_mul_ui, :libgmp), Void, (Ptr{BigInt}, Ptr{BigInt}, Culong), &z, &x, c)
    return z
end
*(c::Culong, x::BigInt) = x * c
*(c::Unsigned, x::BigInt) = x * convert(Culong, c)
*(x::BigInt, c::Unsigned) = x * convert(Culong, c)
function *(x::BigInt, c::Clong)
    z = BigInt()
    ccall((:__gmpz_mul_si, :libgmp), Void, (Ptr{BigInt}, Ptr{BigInt}, Clong), &z, &x, c)
    return z
end
*(c::Clong, x::BigInt) = x * c
*(x::BigInt, c::Signed) = x * convert(Clong, c)
*(c::Signed, x::BigInt) = x * convert(Clong, c)

# unary ops
for (fJ, fC) in ((:-, :neg), (:~, :com))
    @eval begin
        function ($fJ)(x::BigInt)
            z = BigInt()
            ccall(($(string(:__gmpz_,fC)), :libgmp), Void, (Ptr{BigInt}, Ptr{BigInt}), &z, &x)
            return z
        end
    end
end

function <<(x::BigInt, c::Uint)
    z = BigInt()
    ccall((:__gmpz_mul_2exp, :libgmp), Void, (Ptr{BigInt}, Ptr{BigInt}, Culong), &z, &x, c)
    return z
end

function >>>(x::BigInt, c::Uint)
    z = BigInt()
    ccall((:__gmpz_fdiv_q_2exp, :libgmp), Void, (Ptr{BigInt}, Ptr{BigInt}, Culong), &z, &x, c)
    return z
end

<<(x::BigInt, c::Int32) = c < 0 ? throw(DomainError()) : x << uint(c)
>>>(x::BigInt, c::Int32) = c < 0 ? throw(DomainError()) : x >>> uint(c)
>>(x::BigInt, c::Int32) = x >>> c

trailing_zeros(x::BigInt) = int(ccall((:__gmpz_scan1, :libgmp), Culong, (Ptr{BigInt}, Culong), &x, 0))
trailing_ones(x::BigInt) = int(ccall((:__gmpz_scan0, :libgmp), Culong, (Ptr{BigInt}, Culong), &x, 0))

count_ones(x::BigInt) = int(ccall((:__gmpz_popcount, :libgmp), Culong, (Ptr{BigInt},), &x))

function divrem(x::BigInt, y::BigInt)
    z1 = BigInt()
    z2 = BigInt()
    ccall((:__gmpz_tdiv_qr, :libgmp), Void, (Ptr{BigInt}, Ptr{BigInt}, Ptr{BigInt}, Ptr{BigInt}), &(z1.mpz), &(z2.mpz), &x, &y)
    z1, z2
end

function cmp(x::BigInt, y::BigInt)
    ccall((:__gmpz_cmp, :libgmp), Int32, (Ptr{BigInt}, Ptr{BigInt}), &x, &y)
end

function isqrt(x::BigInt)
    z = BigInt()
    ccall((:__gmpz_sqrt, :libgmp), Void, (Ptr{BigInt}, Ptr{BigInt}), &z, &x)
    return z
end

function ^(x::BigInt, y::Uint)
    z = BigInt()
    ccall((:__gmpz_pow_ui, :libgmp), Void, (Ptr{BigInt}, Ptr{BigInt}, Culong), &z, &x, y)
    return z
end

function bigint_pow(x::BigInt, y::Integer)
    if y<0; throw(DomainError()); end
    if x== 1; return x; end
    if x==-1; return isodd(y) ? x : -x; end
    if y>typemax(Uint); throw(DomainError()); end
    return x^uint(y)
end

^(x::BigInt , y::BigInt ) = bigint_pow(x, y)
^(x::BigInt , y::Bool   ) = y ? x : one(x)
^(x::BigInt , y::Integer) = bigint_pow(x, y)
^(x::Integer, y::BigInt ) = bigint_pow(BigInt(x), y)

function powermod(x::BigInt, p::BigInt, m::BigInt)
    r = BigInt()
    ccall((:__gmpz_powm, :libgmp), Void,
          (Ptr{BigInt}, Ptr{BigInt}, Ptr{BigInt}, Ptr{BigInt}),
          &r, &x, &p, &m)
    return r
end
powermod(x::BigInt, p::Integer, m::BigInt) = powermod(x, BigInt(p), m)
powermod(x::BigInt, p::Integer, m::Integer) = powermod(x, BigInt(p), BigInt(m))

function gcdx(a::BigInt, b::BigInt)
    g = BigInt()
    s = BigInt()
    t = BigInt()
    ccall((:__gmpz_gcdext, :libgmp), Void,
        (Ptr{BigInt}, Ptr{BigInt}, Ptr{BigInt}, Ptr{BigInt}, Ptr{BigInt}),
        &g, &s, &t, &a, &b)
    BigInt(g), BigInt(s), BigInt(t)
end

function sum(arr::AbstractArray{BigInt})
    n = BigInt(0)
    for i in arr
        ccall((:__gmpz_add, :libgmp), Void,
            (Ptr{BigInt}, Ptr{BigInt}, Ptr{BigInt}),
            &n, &n, &i)
    end
    return n
end

function factorial(bn::BigInt)
    if bn<0
        return BigInt(0)
    else
        n = uint(bn)
    end
    z = BigInt()
    ccall((:__gmpz_fac_ui, :libgmp), Void,
        (Ptr{BigInt}, Culong), &z, n)
    return z
end

function binomial(n::BigInt, k::Uint)
    z = BigInt()
    ccall((:__gmpz_bin_ui, :libgmp), Void,
        (Ptr{BigInt}, Ptr{BigInt}, Culong), &z, &n, k)
    return z
end
binomial(n::BigInt, k::Integer) = k<0 ? throw(DomainError()) : binomial(n, uint(k))

==(x::BigInt, y::BigInt) = cmp(x,y) == 0
<=(x::BigInt, y::BigInt) = cmp(x,y) <= 0
>=(x::BigInt, y::BigInt) = cmp(x,y) >= 0
<(x::BigInt, y::BigInt) = cmp(x,y) < 0
>(x::BigInt, y::BigInt) = cmp(x,y) > 0

function string(x::BigInt)
    lng = ndigits(x) + 2
    z = Array(Uint8, lng)
    lng = ccall((:__gmp_snprintf,:libgmp), Int32, (Ptr{Uint8}, Culong, Ptr{Uint8}, Ptr{BigInt}...), z, lng, "%Zd", &x)
    return bytestring(convert(Ptr{Uint8}, z[1:lng]))
end

function show(io::IO, x::BigInt)
    print(io, string(x))
end

ndigits(x::BigInt) = ccall((:__gmpz_sizeinbase,:libgmp), Culong, (Ptr{BigInt}, Int32), &x, 10)
isprime(x::BigInt, reps=25) = ccall((:__gmpz_probab_prime_p,:libgmp), Cint, (Ptr{BigInt}, Cint), &x, reps) > 0

widemul(x::BigInt, y::BigInt) = x*y
widemul(x::Union(Int128,Uint128), y::Union(Int128,Uint128)) = BigInt(x)*BigInt(y)

end # module
