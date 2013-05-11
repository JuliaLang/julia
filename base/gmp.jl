module GMP

export BigInt

import Base: *, +, -, /, <, <<, >>, <=, ==, >, >=, ^, (~), (&), (|), ($),
             binomial, cmp, convert, div, factorial, fld, gcd, gcdx, isprime,
             isqrt, lcm, mod, ndigits, powermod, promote_rule, rand, rand!,
             rem, show, string, sum, widemul

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
    function BigInt(x_alloc::Cint, x_size::Cint, x_d::Ptr{Void})
        b = BigInt()
        b.alloc = x_alloc
        b.size = x_size
        b.d = x_d
        return b
    end
end
BigInt_clear(mpz::BigInt) = ccall((:__gmpz_clear, :libgmp), Void,
  (Ptr{BigInt},), &mpz)

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

convert{T<:Integer}(::Type{BigInt}, x::T) = BigInt(x)

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

if sizeof(Int64) == sizeof(Clong)
    function convert(::Type{Int128}, x::BigInt)
        ax = abs(x)
        top = ax>>64
        bot = ax - (top<<64)
        n = int128(convert(Uint,top))<<64 + int128(convert(Uint,bot))
        return x<0 ? -n : n
    end
    convert(::Type{Uint128}, x::BigInt) = uint128(convert(Int128,x))
end

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
<<(x::BigInt, c::Int32)   = c<0 ? throw(DomainError()) : x<<uint(c)
<<(x::BigInt, c::Integer) = c<0 ? throw(DomainError()) : x<<uint(c)

function >>(x::BigInt, c::Uint)
    z = BigInt()
    ccall((:__gmpz_fdiv_q_2exp, :libgmp), Void, (Ptr{BigInt}, Ptr{BigInt}, Culong), &z, &x, c)
    return z
end
>>(x::BigInt, c::Int32)   = c<0 ? throw(DomainError()) : x>>uint(c)
>>(x::BigInt, c::Integer) = c<0 ? throw(DomainError()) : x>>uint(c)

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

############################
# Random number generation #
############################
#Corresponds to __gmp_randstate_struct
type BigIntRand <: AbstractRNG
    seed_alloc::Cint
    seed_size::Cint
    seed_d::Ptr{Void}
    alg::Cint
    alg_data::Ptr{Void}
    function BigIntRand()
        randstate=new(zero(Cint), zero(Cint), C_NULL, zero(Cint), C_NULL)
        #The default algorithm in GMP is currently MersenneTwister
        ccall((:__gmp_randinit_default, :libgmp), Void, (Ptr{BigIntRand},),
          &randstate)
        finalizer(randstate, BigIntRand_clear)
        randstate
    end

    #Initialize with seed
    function BigIntRand(seed::Uint64)
        randstate=BigIntRand()
        ccall((:__gmp_randseed_ui, :libgmp), Void, (Ptr{BigIntRand}, Culong),
          &randstate, seed)
    end
    function BigIntRand(seed::BigInt)
        randstate=BigIntRand()
        ccall((:__gmp_randseed, :libgmp), Void, (Ptr{BigIntRand}, Ptr{BigInt}),
          &randstate, &seed)
    end
end

BigIntRand_clear(x::BigIntRand) = ccall((:__gmp_randclear, :libgmp), Void,
    (Ptr{BigIntRand},), &x)

function rand{T<:Integer}(randstate::BigIntRand, x::Range1{T})
    rop=BigInt()
    n=BigInt(x.len)
    ccall((:__gmpz_urandomm, :libgmp), Void,
      (Ptr{BigInt}, Ptr{BigIntRand}, Ptr{BigInt}), &rop, &randstate, &n)
    rop + x.start
end

function rand!{T<:Integer}(r::Range1{T}, A::Array{T})
    RandState=BigIntRand()
    for i=1:length(A) 
        A[i] = rand(RandState, r)
    end
    return A
end

end # module
