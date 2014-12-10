module GMP

export BigInt

import Base: *, +, -, /, <, <<, >>, >>>, <=, ==, >, >=, ^, (~), (&), (|), ($),
             binomial, cmp, convert, div, divrem, factorial, fld, gcd, gcdx, lcm, mod,
             ndigits, promote_rule, rem, show, isqrt, string, isprime, powermod,
             widemul, sum, trailing_zeros, trailing_ones, count_ones, base, parseint,
             serialize, deserialize, bin, oct, dec, hex, isequal, invmod,
             prevpow2, nextpow2, ndigits0z, widen

if Clong == Int32
    typealias ClongMax Union(Int8, Int16, Int32)
    typealias CulongMax Union(UInt8, UInt16, UInt32)
else
    typealias ClongMax Union(Int8, Int16, Int32, Int64)
    typealias CulongMax Union(UInt8, UInt16, UInt32, UInt64)
end
typealias CdoubleMax Union(Float16, Float32, Float64)

gmp_version() = VersionNumber(bytestring(unsafe_load(cglobal((:__gmp_version, :libgmp), Ptr{Cchar}))))
gmp_bits_per_limb() = Int(unsafe_load(cglobal((:__gmp_bits_per_limb, :libgmp), Cint)))

const GMP_VERSION = gmp_version()
const GMP_BITS_PER_LIMB = gmp_bits_per_limb()

# GMP's mp_limb_t is by default a typedef of `unsigned long`, but can also be configured to be either
# `unsigned int` or `unsigned long long int`. The correct unsigned type is here named Limb, and must
# be used whenever mp_limb_t is in the signature of ccall'ed GMP functions.
if GMP_BITS_PER_LIMB == 32
    typealias Limb UInt32
elseif GMP_BITS_PER_LIMB == 64
    typealias Limb UInt64
else
    error("GMP: cannot determine the type mp_limb_t (__gmp_bits_per_limb == $GMP_BITS_PER_LIMB)")
end


type BigInt <: Integer
    alloc::Cint
    size::Cint
    d::Ptr{Limb}
    function BigInt()
        b = new(zero(Cint), zero(Cint), C_NULL)
        ccall((:__gmpz_init,:libgmp), Void, (Ptr{BigInt},), &b)
        finalizer(b, _gmp_clear_func)
        return b
    end
end

_gmp_clear_func = C_NULL
_mpfr_clear_func = C_NULL

function __init__()
    if gmp_version().major != GMP_VERSION.major || gmp_bits_per_limb() != GMP_BITS_PER_LIMB
        error(string("The dynamically loaded GMP library (version $(gmp_version()) with __gmp_bits_per_limb == $(gmp_bits_per_limb()))\n",
                     "does not correspond to the compile time version (version $GMP_VERSION with __gmp_bits_per_limb == $GMP_BITS_PER_LIMB).\n",
                     "Please rebuild Julia."))
    end

    global _gmp_clear_func = cglobal((:__gmpz_clear, :libgmp))
    global _mpfr_clear_func = cglobal((:mpfr_clear, :libmpfr))
    ccall((:__gmp_set_memory_functions, :libgmp), Void,
          (Ptr{Void},Ptr{Void},Ptr{Void}),
          cglobal(:jl_gc_counted_malloc),
          cglobal(:jl_gc_counted_realloc_with_old_size),
          cglobal(:jl_gc_counted_free))
end

widen(::Type{Int128})  = BigInt
widen(::Type{UInt128}) = BigInt
widen(::Type{BigInt})  = BigInt

BigInt(x::BigInt) = x
BigInt(s::AbstractString) = parseint(BigInt,s)

function Base.parseint_nocheck(::Type{BigInt}, s::AbstractString, base::Int)
    s = bytestring(s)
    sgn, base, i = Base.parseint_preamble(true,s,base)
    z = BigInt()
    err = ccall((:__gmpz_set_str, :libgmp),
               Int32, (Ptr{BigInt}, Ptr{UInt8}, Int32),
               &z, convert(Ptr{UInt8},SubString(s,i)), base)
    err == 0 || error("invalid big integer: $(repr(s))")
    return sgn < 0 ? -z : z
end

function BigInt(x::Union(Clong,Int32))
    z = BigInt()
    ccall((:__gmpz_set_si, :libgmp), Void, (Ptr{BigInt}, Clong), &z, x)
    return z
end
function BigInt(x::Union(Culong,UInt32))
    z = BigInt()
    ccall((:__gmpz_set_ui, :libgmp), Void, (Ptr{BigInt}, Culong), &z, x)
    return z
end

BigInt(x::Bool) = BigInt(uint(x))

function BigInt(x::Float64)
    !isinteger(x) && throw(InexactError())
    z = BigInt()
    ccall((:__gmpz_set_d, :libgmp), Void, (Ptr{BigInt}, Cdouble), &z, x)
    return z
end

BigInt(x::Union(Float16,Float32)) = BigInt(float64(x))

function BigInt(x::Integer)
    if x < 0
        if typemin(Clong) <= x
            return BigInt(convert(Clong,x))
        end
        b = BigInt(0)
        shift = 0
        while x < -1
            b += BigInt(~uint32(x&0xffffffff))<<shift
            x >>= 32
            shift += 32
        end
        return -b-1
    else
        if x <= typemax(Culong)
            return BigInt(convert(Culong,x))
        end
        b = BigInt(0)
        shift = 0
        while x > 0
            b += BigInt(uint32(x&0xffffffff))<<shift
            x >>>= 32
            shift += 32
        end
        return b
    end
end

convert(::Type{BigInt}, x::Integer) = BigInt(x)
convert(::Type{BigInt}, x::Float16) = BigInt(x)
convert(::Type{BigInt}, x::FloatingPoint) = BigInt(x)

function convert(::Type{Int64}, x::BigInt)
    lo = int64(convert(Culong, x & typemax(UInt32)))
    hi = int64(convert(Clong, x >> 32))
    hi << 32 | lo
end
convert(::Type{Int32}, n::BigInt) = int32(convert(Clong, n))
convert(::Type{Int16}, n::BigInt) = int16(convert(Clong, n))
convert(::Type{Int8}, n::BigInt) = int8(convert(Clong, n))

function convert(::Type{Clong}, n::BigInt)
    fits = ccall((:__gmpz_fits_slong_p, :libgmp), Int32, (Ptr{BigInt},), &n) != 0
    if fits
        ccall((:__gmpz_get_si, :libgmp), Clong, (Ptr{BigInt},), &n)
    else
        throw(InexactError())
    end
end

function convert(::Type{UInt64}, x::BigInt)
    lo = uint64(convert(Culong, x & typemax(UInt32)))
    hi = uint64(convert(Culong, x >> 32))
    hi << 32 | lo
end
convert(::Type{UInt32}, x::BigInt) = uint32(convert(Culong, x))
convert(::Type{UInt16}, x::BigInt) = uint16(convert(Culong, x))
convert(::Type{UInt8}, x::BigInt) = uint8(convert(Culong, x))

function convert(::Type{Culong}, n::BigInt)
    fits = ccall((:__gmpz_fits_ulong_p, :libgmp), Int32, (Ptr{BigInt},), &n) != 0
    if fits
        ccall((:__gmpz_get_ui, :libgmp), Culong, (Ptr{BigInt},), &n)
    else
        throw(InexactError())
    end
end

if sizeof(Int32) == sizeof(Clong)
    function convert(::Type{UInt128}, x::BigInt)
        uint128(uint(x>>>96))<<96 +
        uint128(uint((x>>>64) & typemax(UInt32)))<<64 +
        uint128(uint((x>>>32) & typemax(UInt32)))<<32 +
        uint128(uint(x & typemax(UInt32)))
    end
end
if sizeof(Int64) == sizeof(Clong)
    function convert(::Type{UInt128}, x::BigInt)
        uint128(uint(x>>>64))<<64 +
        uint128(uint(x & typemax(UInt64)))
    end
end
convert(::Type{Int128}, x::BigInt) = copysign(int128(uint128(abs(x))),x)

function convert(::Type{Float64}, n::BigInt)
    # TODO: this should round to nearest but instead rounds to zero
    ccall((:__gmpz_get_d, :libgmp), Float64, (Ptr{BigInt},), &n)
end
convert(::Type{Float32}, n::BigInt) = float32(float64(n))
convert(::Type{Float16}, n::BigInt) = float16(float64(n))

promote_rule{T<:Integer}(::Type{BigInt}, ::Type{T}) = BigInt

# serialization

function serialize(s, n::BigInt)
    Base.serialize_type(s, BigInt)
    serialize(s, base(62,n))
end

deserialize(s, ::Type{BigInt}) = Base.parseint_nocheck(BigInt, deserialize(s), 62)

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

function invmod(x::BigInt, y::BigInt)
    z = BigInt()
    y = abs(y)
    if y == 1
        return big(0)
    end
    if (y==0 || ccall((:__gmpz_invert, :libgmp), Cint, (Ptr{BigInt}, Ptr{BigInt}, Ptr{BigInt}), &z, &x, &y) == 0)
        error("no inverse exists")
    end
    return z
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
function +(x::BigInt, c::CulongMax)
    z = BigInt()
    ccall((:__gmpz_add_ui, :libgmp), Void, (Ptr{BigInt}, Ptr{BigInt}, Culong), &z, &x, c)
    return z
end
+(c::CulongMax, x::BigInt) = x + c

function -(x::BigInt, c::CulongMax)
    z = BigInt()
    ccall((:__gmpz_sub_ui, :libgmp), Void, (Ptr{BigInt}, Ptr{BigInt}, Culong), &z, &x, c)
    return z
end
function -(c::CulongMax, x::BigInt)
    z = BigInt()
    ccall((:__gmpz_ui_sub, :libgmp), Void, (Ptr{BigInt}, Culong, Ptr{BigInt}), &z, c, &x)
    return z
end
+(x::BigInt, c::ClongMax) = c < 0 ? -(x, convert(Culong, -c)) : x + convert(Culong, c)
+(c::ClongMax, x::BigInt) = c < 0 ? -(x, convert(Culong, -c)) : x + convert(Culong, c)
-(x::BigInt, c::ClongMax) = c < 0 ? +(x, convert(Culong, -c)) : -(x, convert(Culong, c))
-(c::ClongMax, x::BigInt) = c < 0 ? -(x + convert(Culong, -c)) : -(convert(Culong, c), x)

function *(x::BigInt, c::CulongMax)
    z = BigInt()
    ccall((:__gmpz_mul_ui, :libgmp), Void, (Ptr{BigInt}, Ptr{BigInt}, Culong), &z, &x, c)
    return z
end
*(c::CulongMax, x::BigInt) = x * c
function *(x::BigInt, c::ClongMax)
    z = BigInt()
    ccall((:__gmpz_mul_si, :libgmp), Void, (Ptr{BigInt}, Ptr{BigInt}, Clong), &z, &x, c)
    return z
end
*(c::ClongMax, x::BigInt) = x * c

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

function <<(x::BigInt, c::Int32)
    c < 0 && throw(DomainError())
    c == 0 && return x
    z = BigInt()
    ccall((:__gmpz_mul_2exp, :libgmp), Void, (Ptr{BigInt}, Ptr{BigInt}, Culong), &z, &x, c)
    return z
end

function >>(x::BigInt, c::Int32)
    c < 0 && throw(DomainError())
    c == 0 && return x
    z = BigInt()
    ccall((:__gmpz_fdiv_q_2exp, :libgmp), Void, (Ptr{BigInt}, Ptr{BigInt}, Culong), &z, &x, c)
    return z
end

>>>(x::BigInt, c::Int32) = x >> c

trailing_zeros(x::BigInt) = int(ccall((:__gmpz_scan1, :libgmp), Culong, (Ptr{BigInt}, Culong), &x, 0))
trailing_ones(x::BigInt) = int(ccall((:__gmpz_scan0, :libgmp), Culong, (Ptr{BigInt}, Culong), &x, 0))

count_ones(x::BigInt) = int(ccall((:__gmpz_popcount, :libgmp), Culong, (Ptr{BigInt},), &x))

function divrem(x::BigInt, y::BigInt)
    z1 = BigInt()
    z2 = BigInt()
    ccall((:__gmpz_tdiv_qr, :libgmp), Void, (Ptr{BigInt}, Ptr{BigInt}, Ptr{BigInt}, Ptr{BigInt}), &z1, &z2, &x, &y)
    z1, z2
end

function cmp(x::BigInt, y::BigInt)
    ccall((:__gmpz_cmp, :libgmp), Int32, (Ptr{BigInt}, Ptr{BigInt}), &x, &y)
end
function cmp(x::BigInt, y::ClongMax)
    ccall((:__gmpz_cmp_si, :libgmp), Int32, (Ptr{BigInt}, Clong), &x, y)
end
function cmp(x::BigInt, y::CulongMax)
    ccall((:__gmpz_cmp_ui, :libgmp), Int32, (Ptr{BigInt}, Culong), &x, y)
end
cmp(x::BigInt, y::Integer) = cmp(x,big(y))
cmp(x::Integer, y::BigInt) = -cmp(y,x)

function cmp(x::BigInt, y::CdoubleMax)
    isnan(y) && throw(DomainError())
    ccall((:__gmpz_cmp_d, :libgmp), Int32, (Ptr{BigInt}, Cdouble), &x, y)
end
cmp(x::CdoubleMax, y::BigInt) = -cmp(y,x)

function isqrt(x::BigInt)
    z = BigInt()
    ccall((:__gmpz_sqrt, :libgmp), Void, (Ptr{BigInt}, Ptr{BigInt}), &z, &x)
    return z
end

function ^(x::BigInt, y::UInt)
    z = BigInt()
    ccall((:__gmpz_pow_ui, :libgmp), Void, (Ptr{BigInt}, Ptr{BigInt}, Culong), &z, &x, y)
    return z
end

function bigint_pow(x::BigInt, y::Integer)
    if y<0; throw(DomainError()); end
    if x== 1; return x; end
    if x==-1; return isodd(y) ? x : -x; end
    if y>typemax(UInt); throw(DomainError()); end
    return x^uint(y)
end

^(x::BigInt , y::BigInt ) = bigint_pow(x, y)
^(x::BigInt , y::Bool   ) = y ? x : one(x)
^(x::BigInt , y::Integer) = bigint_pow(x, y)
^(x::Integer, y::BigInt ) = bigint_pow(BigInt(x), y)

function powermod(x::BigInt, p::BigInt, m::BigInt)
    p < 0 && throw(DomainError())
    r = BigInt()
    ccall((:__gmpz_powm, :libgmp), Void,
          (Ptr{BigInt}, Ptr{BigInt}, Ptr{BigInt}, Ptr{BigInt}),
          &r, &x, &p, &m)
    return m < 0 && r > 0 ? r + m : r # choose sign conistent with mod(x^p, m)
end
powermod(x::BigInt, p::Integer, m::BigInt) = powermod(x, BigInt(p), m)
powermod(x::BigInt, p::Integer, m::Integer) = powermod(x, BigInt(p), BigInt(m))

function gcdx(a::BigInt, b::BigInt)
    if b == 0 # shortcut this to ensure consistent results with gcdx(a,b)
        return a < 0 ? (-a,-one(BigInt),zero(BigInt)) : (a,one(BigInt),zero(BigInt))
    end
    g = BigInt()
    s = BigInt()
    t = BigInt()
    ccall((:__gmpz_gcdext, :libgmp), Void,
        (Ptr{BigInt}, Ptr{BigInt}, Ptr{BigInt}, Ptr{BigInt}, Ptr{BigInt}),
        &g, &s, &t, &a, &b)
    if t == 0
        # work around a difference in some versions of GMP
        if a == b
            return g, t, s
        elseif abs(a)==abs(b)
            return g, t, -s
        end
    end
    g, s, t
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

function factorial(x::BigInt)
    x.size < 0 && return BigInt(0)
    z = BigInt()
    ccall((:__gmpz_fac_ui, :libgmp), Void, (Ptr{BigInt}, Culong), &z, x)
    return z
end

function binomial(n::BigInt, k::UInt)
    z = BigInt()
    ccall((:__gmpz_bin_ui, :libgmp), Void, (Ptr{BigInt}, Ptr{BigInt}, Culong), &z, &n, k)
    return z
end
binomial(n::BigInt, k::Integer) = k < 0 ? throw(DomainError()) : binomial(n, uint(k))

==(x::BigInt, y::BigInt) = cmp(x,y) == 0
==(x::BigInt, i::Integer) = cmp(x,i) == 0
==(i::Integer, x::BigInt) = cmp(x,i) == 0
==(x::BigInt, f::CdoubleMax) = isnan(f) ? false : cmp(x,f) == 0
==(f::CdoubleMax, x::BigInt) = isnan(f) ? false : cmp(x,f) == 0

<=(x::BigInt, y::BigInt) = cmp(x,y) <= 0
<=(x::BigInt, i::Integer) = cmp(x,i) <= 0
<=(i::Integer, x::BigInt) = cmp(x,i) >= 0
<=(x::BigInt, f::CdoubleMax) = isnan(f) ? false : cmp(x,f) <= 0
<=(f::CdoubleMax, x::BigInt) = isnan(f) ? false : cmp(x,f) >= 0

<(x::BigInt, y::BigInt) = cmp(x,y) < 0
<(x::BigInt, i::Integer) = cmp(x,i) < 0
<(i::Integer, x::BigInt) = cmp(x,i) > 0
<(x::BigInt, f::CdoubleMax) = isnan(f) ? false : cmp(x,f) < 0
<(f::CdoubleMax, x::BigInt) = isnan(f) ? false : cmp(x,f) > 0

string(x::BigInt) = dec(x)
show(io::IO, x::BigInt) = print(io, string(x))

bin(n::BigInt) = base( 2, n)
oct(n::BigInt) = base( 8, n)
dec(n::BigInt) = base(10, n)
hex(n::BigInt) = base(16, n)

function base(b::Integer, n::BigInt)
    2 <= b <= 62 || error("invalid base: $b")
    p = ccall((:__gmpz_get_str,:libgmp), Ptr{UInt8}, (Ptr{UInt8}, Cint, Ptr{BigInt}), C_NULL, b, &n)
    len = int(ccall(:strlen, Csize_t, (Ptr{UInt8},), p))
    ASCIIString(pointer_to_array(p,len,true))
end

function ndigits0z(x::BigInt, b::Integer=10)
    b < 2 && throw(DomainError())
    if ispow2(b)
        int(ccall((:__gmpz_sizeinbase,:libgmp), Culong, (Ptr{BigInt}, Int32), &x, b))
    else
        # non-base 2 mpz_sizeinbase might return an answer 1 too big
        # use property that log(b, x) < ndigits(x, b) <= log(b, x) + 1
        n = int(ccall((:__gmpz_sizeinbase,:libgmp), Culong, (Ptr{BigInt}, Int32), &x, 2))
        lb = log2(b) # assumed accurate to <1ulp (true for openlibm)
        q,r = divrem(n,lb)
        iq = int(q)
        maxerr = q*eps(lb) # maximum error in remainder
        if r-1.0 < maxerr
            abs(x) >= big(b)^iq ? iq+1 : iq
        elseif lb-r < maxerr
            abs(x) >= big(b)^(iq+1) ? iq+2 : iq+1
        else
            iq+1
        end
    end
end
ndigits(x::BigInt, b::Integer=10) = x.size == 0 ? 1 : ndigits0z(x,b)

isprime(x::BigInt, reps=25) = ccall((:__gmpz_probab_prime_p,:libgmp), Cint, (Ptr{BigInt}, Cint), &x, reps) > 0

widemul(x::Int128, y::UInt128)  = BigInt(x)*BigInt(y)
widemul(x::UInt128, y::Int128)  = BigInt(x)*BigInt(y)

prevpow2(x::BigInt) = x.size < 0 ? -prevpow2(-x) : (x <= 2 ? x : one(BigInt) << (ndigits(x, 2)-1))
nextpow2(x::BigInt) = x.size < 0 ? -nextpow2(-x) : (x <= 2 ? x : one(BigInt) << ndigits(x-1, 2))

Base.checked_add(a::BigInt, b::BigInt) = a + b
Base.checked_sub(a::BigInt, b::BigInt) = a - b
Base.checked_mul(a::BigInt, b::BigInt) = a * b

end # module
