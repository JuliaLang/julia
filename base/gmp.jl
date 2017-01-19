# This file is a part of Julia. License is MIT: http://julialang.org/license

module GMP

export BigInt

import Base: *, +, -, /, <, <<, >>, >>>, <=, ==, >, >=, ^, (~), (&), (|), xor,
             binomial, cmp, convert, div, divrem, factorial, fld, gcd, gcdx, lcm, mod,
             ndigits, promote_rule, rem, show, isqrt, string, powermod,
             sum, trailing_zeros, trailing_ones, count_ones, base, tryparse_internal,
             bin, oct, dec, hex, isequal, invmod, prevpow2, nextpow2, ndigits0z, widen, signed, unsafe_trunc, trunc,
             iszero

if Clong == Int32
    typealias ClongMax Union{Int8, Int16, Int32}
    typealias CulongMax Union{UInt8, UInt16, UInt32}
else
    typealias ClongMax Union{Int8, Int16, Int32, Int64}
    typealias CulongMax Union{UInt8, UInt16, UInt32, UInt64}
end
typealias CdoubleMax Union{Float16, Float32, Float64}

gmp_version() = VersionNumber(unsafe_string(unsafe_load(cglobal((:__gmp_version, :libgmp), Ptr{Cchar}))))
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
        finalizer(b, cglobal((:__gmpz_clear, :libgmp)))
        return b
    end
end

function __init__()
    try
        if gmp_version().major != GMP_VERSION.major || gmp_bits_per_limb() != GMP_BITS_PER_LIMB
            error(string("The dynamically loaded GMP library (version $(gmp_version()) with __gmp_bits_per_limb == $(gmp_bits_per_limb()))\n",
                         "does not correspond to the compile time version (version $GMP_VERSION with __gmp_bits_per_limb == $GMP_BITS_PER_LIMB).\n",
                         "Please rebuild Julia."))
        end

        ccall((:__gmp_set_memory_functions, :libgmp), Void,
              (Ptr{Void},Ptr{Void},Ptr{Void}),
              cglobal(:jl_gc_counted_malloc),
              cglobal(:jl_gc_counted_realloc_with_old_size),
              cglobal(:jl_gc_counted_free))
    catch ex
        Base.showerror_nostdio(ex,
            "WARNING: Error during initialization of module GMP")
    end
end

widen(::Type{Int128})  = BigInt
widen(::Type{UInt128}) = BigInt
widen(::Type{BigInt})  = BigInt

signed(x::BigInt) = x

convert(::Type{BigInt}, x::BigInt) = x

function tryparse_internal(::Type{BigInt}, s::AbstractString, startpos::Int, endpos::Int, base_::Integer, raise::Bool)
    _n = Nullable{BigInt}()

    # don't make a copy in the common case where we are parsing a whole String
    bstr = startpos == start(s) && endpos == endof(s) ? String(s) : String(SubString(s,startpos,endpos))

    sgn, base, i = Base.parseint_preamble(true,Int(base_),bstr,start(bstr),endof(bstr))
    if !(2 <= base <= 62)
        raise && throw(ArgumentError("invalid base: base must be 2 ≤ base ≤ 62, got $base"))
        return _n
    end
    if i == 0
        raise && throw(ArgumentError("premature end of integer: $(repr(bstr))"))
        return _n
    end
    z = BigInt()
    if Base.containsnul(bstr)
        err = -1 # embedded NUL char (not handled correctly by GMP)
    else
        err = ccall((:__gmpz_set_str, :libgmp),
                    Int32, (Ptr{BigInt}, Ptr{UInt8}, Int32),
                    &z, pointer(bstr)+(i-start(bstr)), base)
    end
    if err != 0
        raise && throw(ArgumentError("invalid BigInt: $(repr(bstr))"))
        return _n
    end
    Nullable(sgn < 0 ? -z : z)
end

function convert(::Type{BigInt}, x::Union{Clong,Int32})
    z = BigInt()
    ccall((:__gmpz_set_si, :libgmp), Void, (Ptr{BigInt}, Clong), &z, x)
    return z
end
function convert(::Type{BigInt}, x::Union{Culong,UInt32})
    z = BigInt()
    ccall((:__gmpz_set_ui, :libgmp), Void, (Ptr{BigInt}, Culong), &z, x)
    return z
end

convert(::Type{BigInt}, x::Bool) = BigInt(UInt(x))


function unsafe_trunc(::Type{BigInt}, x::Union{Float32,Float64})
    z = BigInt()
    ccall((:__gmpz_set_d, :libgmp), Void, (Ptr{BigInt}, Cdouble), &z, x)
    return z
end

function convert(::Type{BigInt}, x::Union{Float32,Float64})
    isinteger(x) || throw(InexactError())
    unsafe_trunc(BigInt,x)
end

function trunc(::Type{BigInt}, x::Union{Float32,Float64})
    isfinite(x) || throw(InexactError())
    unsafe_trunc(BigInt,x)
end

convert(::Type{BigInt}, x::Float16) = BigInt(Float64(x))
convert(::Type{BigInt}, x::Float32) = BigInt(Float64(x))

function convert(::Type{BigInt}, x::Integer)
    if x < 0
        if typemin(Clong) <= x
            return BigInt(convert(Clong,x))
        end
        b = BigInt(0)
        shift = 0
        while x < -1
            b += BigInt(~UInt32(x&0xffffffff))<<shift
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
            b += BigInt(UInt32(x&0xffffffff))<<shift
            x >>>= 32
            shift += 32
        end
        return b
    end
end


rem(x::BigInt, ::Type{Bool}) = ((x&1)!=0)
function rem{T<:Union{Unsigned,Signed}}(x::BigInt, ::Type{T})
    u = zero(T)
    for l = 1:min(abs(x.size), cld(sizeof(T),sizeof(Limb)))
        u += (unsafe_load(x.d,l)%T) << ((sizeof(Limb)<<3)*(l-1))
    end
    x.size < 0 ? -u : u
end

function convert{T<:Unsigned}(::Type{T}, x::BigInt)
    if sizeof(T) < sizeof(Limb)
        convert(T, convert(Limb,x))
    else
        0 <= x.size <= cld(sizeof(T),sizeof(Limb)) || throw(InexactError())
        x % T
    end
end

function convert{T<:Signed}(::Type{T}, x::BigInt)
    n = abs(x.size)
    if sizeof(T) < sizeof(Limb)
        SLimb = typeof(Signed(one(Limb)))
        convert(T, convert(SLimb, x))
    else
        0 <= n <= cld(sizeof(T),sizeof(Limb)) || throw(InexactError())
        y = x % T
        (x.size > 0) ⊻ (y > 0) && throw(InexactError()) # catch overflow
        y
    end
end


function (::Type{Float64})(n::BigInt, ::RoundingMode{:ToZero})
    ccall((:__gmpz_get_d, :libgmp), Float64, (Ptr{BigInt},), &n)
end

function (::Type{T}){T<:Union{Float16,Float32}}(n::BigInt, ::RoundingMode{:ToZero})
    T(Float64(n,RoundToZero),RoundToZero)
end

function (::Type{T}){T<:CdoubleMax}(n::BigInt, ::RoundingMode{:Down})
    x = T(n,RoundToZero)
    x > n ? prevfloat(x) : x
end
function (::Type{T}){T<:CdoubleMax}(n::BigInt, ::RoundingMode{:Up})
    x = T(n,RoundToZero)
    x < n ? nextfloat(x) : x
end

function (::Type{T}){T<:CdoubleMax}(n::BigInt, ::RoundingMode{:Nearest})
    x = T(n,RoundToZero)
    if maxintfloat(T) <= abs(x) < T(Inf)
        r = n-BigInt(x)
        h = eps(x)/2
        if iseven(reinterpret(Unsigned,x)) # check if last bit is odd/even
            if r < -h
                return prevfloat(x)
            elseif r > h
                return nextfloat(x)
            end
        else
            if r <= -h
                return prevfloat(x)
            elseif r >= h
                return nextfloat(x)
            end
        end
    end
    x
end

convert(::Type{Float64}, n::BigInt) = Float64(n,RoundNearest)
convert(::Type{Float32}, n::BigInt) = Float32(n,RoundNearest)
convert(::Type{Float16}, n::BigInt) = Float16(n,RoundNearest)

promote_rule{T<:Integer}(::Type{BigInt}, ::Type{T}) = BigInt

# Binary ops
for (fJ, fC) in ((:+, :add), (:-,:sub), (:*, :mul),
                 (:fld, :fdiv_q), (:div, :tdiv_q), (:mod, :fdiv_r), (:rem, :tdiv_r),
                 (:gcd, :gcd), (:lcm, :lcm),
                 (:&, :and), (:|, :ior), (:xor, :xor))
    @eval begin
        function ($fJ)(x::BigInt, y::BigInt)
            z = BigInt()
            ccall(($(string(:__gmpz_,fC)), :libgmp), Void, (Ptr{BigInt}, Ptr{BigInt}, Ptr{BigInt}), &z, &x, &y)
            return z
        end
    end
end

/(x::BigInt, y::BigInt) = float(x)/float(y)

function invmod(x::BigInt, y::BigInt)
    z = zero(BigInt)
    ya = abs(y)
    if ya == 1
        return z
    end
    if (y==0 || ccall((:__gmpz_invert, :libgmp), Cint, (Ptr{BigInt}, Ptr{BigInt}, Ptr{BigInt}), &z, &x, &ya) == 0)
        throw(DomainError())
    end
    # GMP always returns a positive inverse; we instead want to
    # normalize such that div(z, y) == 0, i.e. we want a negative z
    # when y is negative.
    if y < 0
        z = z + y
    end
    # The postcondition is: mod(z * x, y) == mod(big(1), m) && div(z, y) == 0
    return z
end

# More efficient commutative operations
for (fJ, fC) in ((:+, :add), (:*, :mul), (:&, :and), (:|, :ior), (:xor, :xor))
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
+(x::BigInt, c::ClongMax) = c < 0 ? -(x, -(c % Culong)) : x + convert(Culong, c)
+(c::ClongMax, x::BigInt) = c < 0 ? -(x, -(c % Culong)) : x + convert(Culong, c)
-(x::BigInt, c::ClongMax) = c < 0 ? +(x, -(c % Culong)) : -(x, convert(Culong, c))
-(c::ClongMax, x::BigInt) = c < 0 ? -(x + -(c % Culong)) : -(convert(Culong, c), x)

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

/(x::BigInt, y::Union{ClongMax,CulongMax}) = float(x)/y
/(x::Union{ClongMax,CulongMax}, y::BigInt) = x/float(y)

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

function <<(x::BigInt, c::UInt)
    c == 0 && return x
    z = BigInt()
    ccall((:__gmpz_mul_2exp, :libgmp), Void, (Ptr{BigInt}, Ptr{BigInt}, Culong), &z, &x, c)
    return z
end

function >>(x::BigInt, c::UInt)
    c == 0 && return x
    z = BigInt()
    ccall((:__gmpz_fdiv_q_2exp, :libgmp), Void, (Ptr{BigInt}, Ptr{BigInt}, Culong), &z, &x, c)
    return z
end

>>>(x::BigInt, c::UInt) = x >> c


trailing_zeros(x::BigInt) = Int(ccall((:__gmpz_scan1, :libgmp), Culong, (Ptr{BigInt}, Culong), &x, 0))
trailing_ones(x::BigInt) = Int(ccall((:__gmpz_scan0, :libgmp), Culong, (Ptr{BigInt}, Culong), &x, 0))

count_ones(x::BigInt) = Int(ccall((:__gmpz_popcount, :libgmp), Culong, (Ptr{BigInt},), &x))

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

function ^(x::BigInt, y::Culong)
    z = BigInt()
    ccall((:__gmpz_pow_ui, :libgmp), Void, (Ptr{BigInt}, Ptr{BigInt}, Culong), &z, &x, y)
    return z
end

function bigint_pow(x::BigInt, y::Integer)
    if y<0; throw(DomainError()); end
    if x== 1; return x; end
    if x==-1; return isodd(y) ? x : -x; end
    if y>typemax(Culong)
       x==0 && return x

       #At this point, x is not 1, 0 or -1 and it is not possible to use
       #gmpz_pow_ui to compute the answer. Note that the magnitude of the
       #answer is:
       #- at least 2^(2^32-1) ≈ 10^(1.3e9) (if Culong === UInt32).
       #- at least 2^(2^64-1) ≈ 10^(5.5e18) (if Culong === UInt64).
       #
       #Assume that the answer will definitely overflow.

       throw(OverflowError())
    end
    return x^convert(Culong, y)
end

^(x::BigInt , y::BigInt ) = bigint_pow(x, y)
^(x::BigInt , y::Bool   ) = y ? x : one(x)
^(x::BigInt , y::Integer) = bigint_pow(x, y)
^(x::Integer, y::BigInt ) = bigint_pow(BigInt(x), y)
^(x::Bool   , y::BigInt ) = Base.power_by_squaring(x, y)

function powermod(x::BigInt, p::BigInt, m::BigInt)
    r = BigInt()
    ccall((:__gmpz_powm, :libgmp), Void,
          (Ptr{BigInt}, Ptr{BigInt}, Ptr{BigInt}, Ptr{BigInt}),
          &r, &x, &p, &m)
    return m < 0 && r > 0 ? r + m : r # choose sign conistent with mod(x^p, m)
end

powermod(x::Integer, p::Integer, m::BigInt) = powermod(big(x), big(p), m)

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
binomial(n::BigInt, k::Integer) = k < 0 ? BigInt(0) : binomial(n, UInt(k))

==(x::BigInt, y::BigInt) = cmp(x,y) == 0
==(x::BigInt, i::Integer) = cmp(x,i) == 0
==(i::Integer, x::BigInt) = cmp(x,i) == 0
==(x::BigInt, f::CdoubleMax) = isnan(f) ? false : cmp(x,f) == 0
==(f::CdoubleMax, x::BigInt) = isnan(f) ? false : cmp(x,f) == 0
iszero(x::BigInt) = x == Clong(0)

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

bin(n::BigInt, pad::Int) = base( 2, n, pad)
oct(n::BigInt, pad::Int) = base( 8, n, pad)
dec(n::BigInt, pad::Int) = base(10, n, pad)
hex(n::BigInt, pad::Int) = base(16, n, pad)

function base(b::Integer, n::BigInt)
    2 <= b <= 62 || throw(ArgumentError("base must be 2 ≤ base ≤ 62, got $b"))
    nd = ndigits(n, b)
    str = Base._string_n(n < 0 ? nd+1 : nd)
    ccall((:__gmpz_get_str,:libgmp), Ptr{UInt8}, (Ptr{UInt8}, Cint, Ptr{BigInt}), str, b, &n)
    return str
end

function base(b::Integer, n::BigInt, pad::Integer)
    s = base(b, n)
    buf = IOBuffer()
    if n < 0
        s = s[2:end]
        write(buf, '-')
    end
    for i in 1:pad-sizeof(s) # `s` is known to be ASCII, and `length` is slower
        write(buf, '0')
    end
    write(buf, s)
    String(buf)
end

function ndigits0z(x::BigInt, b::Integer=10)
    b < 2 && throw(DomainError())
    if ispow2(b) && 2 <= b <= 62 # GMP assumes b is in this range
        Int(ccall((:__gmpz_sizeinbase,:libgmp), Csize_t, (Ptr{BigInt}, Cint), &x, b))
    else
        # non-base 2 mpz_sizeinbase might return an answer 1 too big
        # use property that log(b, x) < ndigits(x, b) <= log(b, x) + 1
        n = Int(ccall((:__gmpz_sizeinbase,:libgmp), Csize_t, (Ptr{BigInt}, Cint), &x, 2))
        lb = log2(b) # assumed accurate to <1ulp (true for openlibm)
        q,r = divrem(n,lb)
        iq = Int(q)
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

prevpow2(x::BigInt) = x.size < 0 ? -prevpow2(-x) : (x <= 2 ? x : one(BigInt) << (ndigits(x, 2)-1))
nextpow2(x::BigInt) = x.size < 0 ? -nextpow2(-x) : (x <= 2 ? x : one(BigInt) << ndigits(x-1, 2))

Base.checked_abs(x::BigInt) = abs(x)
Base.checked_neg(x::BigInt) = -x
Base.checked_add(a::BigInt, b::BigInt) = a + b
Base.checked_sub(a::BigInt, b::BigInt) = a - b
Base.checked_mul(a::BigInt, b::BigInt) = a * b
Base.checked_div(a::BigInt, b::BigInt) = div(a, b)
Base.checked_rem(a::BigInt, b::BigInt) = rem(a, b)
Base.checked_fld(a::BigInt, b::BigInt) = fld(a, b)
Base.checked_mod(a::BigInt, b::BigInt) = mod(a, b)
Base.checked_cld(a::BigInt, b::BigInt) = cld(a, b)
Base.add_with_overflow(a::BigInt, b::BigInt) = a + b, false
Base.sub_with_overflow(a::BigInt, b::BigInt) = a - b, false
Base.mul_with_overflow(a::BigInt, b::BigInt) = a * b, false

function Base.deepcopy_internal(x::BigInt, stackdict::ObjectIdDict)
    if haskey(stackdict, x)
        return stackdict[x]
    end
    y = BigInt()
    ccall((:__gmpz_set,:libgmp), Void, (Ptr{BigInt}, Ptr{BigInt}), &y, &x)
    stackdict[x] = y
    return y
end

end # module
