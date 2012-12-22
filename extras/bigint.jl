import Base.convert, Base.promote_rule, Base.+, Base.-, Base.*, Base.<<
import Base.^, Base.div, Base.rem, Base.cmp, Base.sqrt
import Base.gcd, Base.gcdx, Base.factorial, Base.binomial
import Base.==, Base.<=, Base.>=, Base.<, Base.>, Base.string, Base.show

_jl_libgmp_wrapper = dlopen("libgmp_wrapper")

type BigInt <: Integer
    mpz::Ptr{Void}

    function BigInt(x::String)
        z = _jl_bigint_init()
        ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpz_set_string), Void, (Ptr{Void}, Ptr{Uint8}),z,bytestring(x))
        b = new(z)
        finalizer(b, _jl_bigint_clear)
        b
    end

    function BigInt(x::Int)
        z = _jl_bigint_init()
        ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpz_set_si), Void, (Ptr{Void}, Int),z,x)
        b = new(z)
        finalizer(b, _jl_bigint_clear)
        b
    end
    BigInt{T<:Signed}(x::T) = BigInt(int(x))
    BigInt(x::Int128) = BigInt(string(x))

    function BigInt(x::Uint)
        z = _jl_bigint_init()
        ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpz_set_ui), Void,
            (Ptr{Void}, Uint), z, x)
        b = new(z)
        finalizer(b, _jl_bigint_clear)
        b
    end
    BigInt{T<:Unsigned}(x::T) = BigInt(uint(x))
    BigInt(x::Uint128) = BigInt(string(x))

    function BigInt(z::Ptr{Void})
        b = new(z)
        finalizer(b, _jl_bigint_clear)
        b
    end
end

convert(::Type{BigInt}, x::Int8) = BigInt(int(x))
convert(::Type{BigInt}, x::Int16) = BigInt(int(x))
convert(::Type{BigInt}, x::Int) = BigInt(x)

convert(::Type{BigInt}, x::Uint8) = BigInt(uint(x))
convert(::Type{BigInt}, x::Uint16) = BigInt(uint(x))
convert(::Type{BigInt}, x::Uint) = BigInt(x)

if WORD_SIZE == 64
    convert(::Type{BigInt}, x::Int32) = BigInt(int(x))
    convert(::Type{BigInt}, x::Uint32) = BigInt(uint(x))
else
    BigInt(l::Int64) = BigInt(string(l))
    BigInt(l::Uint64) = BigInt(string(l))
    convert(::Type{BigInt}, x::Int64) = BigInt(string(x))
    convert(::Type{BigInt}, x::Uint64) = BigInt(string(x))
end

convert(::Type{Int}, n::BigInt) =
    ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpz_get_si), Int, (Ptr{Void},), n.mpz)

convert(::Type{Uint}, n::BigInt) =
    ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpz_get_ui), Uint, (Ptr{Void},), n.mpz)

promote_rule(::Type{BigInt}, ::Type{Int8}) = BigInt
promote_rule(::Type{BigInt}, ::Type{Int16}) = BigInt
promote_rule(::Type{BigInt}, ::Type{Int32}) = BigInt
promote_rule(::Type{BigInt}, ::Type{Int64}) = BigInt
promote_rule(::Type{BigInt}, ::Type{Int128}) = BigInt

promote_rule(::Type{BigInt}, ::Type{Uint8}) = BigInt
promote_rule(::Type{BigInt}, ::Type{Uint16}) = BigInt
promote_rule(::Type{BigInt}, ::Type{Uint32}) = BigInt
promote_rule(::Type{BigInt}, ::Type{Uint64}) = BigInt
promote_rule(::Type{BigInt}, ::Type{Uint128}) = BigInt

function +(x::BigInt, y::BigInt)
    z= _jl_bigint_init()
    ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpz_add), Void, (Ptr{Void}, Ptr{Void}, Ptr{Void}),z,x.mpz,y.mpz)
    BigInt(z)
end

function -(x::BigInt)
    z= _jl_bigint_init()
    ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpz_neg), Void, (Ptr{Void}, Ptr{Void}),z,x.mpz)
    BigInt(z)
end

function -(x::BigInt, y::BigInt)
    z= _jl_bigint_init()
    ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpz_sub), Void, (Ptr{Void}, Ptr{Void}, Ptr{Void}),z,x.mpz,y.mpz)
    BigInt(z)
end

function *(x::BigInt, y::BigInt)
    z= _jl_bigint_init()
    ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpz_mul), Void, (Ptr{Void}, Ptr{Void}, Ptr{Void}),z,x.mpz,y.mpz)
    BigInt(z)
end

function <<(x::BigInt, c::Uint)
    z= _jl_bigint_init()
    ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpz_lshift), Void, (Ptr{Void}, Ptr{Void}, Uint), z, x.mpz, c)
    BigInt(z)
end
<<(x::BigInt, c::Int32)   = c<0 ? throw(DomainError()) : x<<uint(c)
<<(x::BigInt, c::Integer) = c<0 ? throw(DomainError()) : x<<uint(c)

function div(x::BigInt, y::BigInt)
    z= _jl_bigint_init()
    ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpz_div), Void, (Ptr{Void}, Ptr{Void}, Ptr{Void}),z,x.mpz,y.mpz)
    BigInt(z)
end

function divmod(x::BigInt, y::BigInt)
    z1= _jl_bigint_init()
    z2= _jl_bigint_init()
    ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpz_divmod), Void, (Ptr{Void}, Ptr{Void}, Ptr{Void}, Ptr{Void}), z1, z2, x.mpz, y.mpz)
    BigInt(z1),BigInt(z2)
end

function rem(x::BigInt, y::BigInt)
    z= _jl_bigint_init()
    ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpz_rem), Void, (Ptr{Void}, Ptr{Void}, Ptr{Void}),z,x.mpz,y.mpz)
    BigInt(z)
end

function cmp(x::BigInt, y::BigInt)
    ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpz_cmp), Int32, (Ptr{Void}, Ptr{Void}),x.mpz, y.mpz)
end

function sqrt(x::BigInt)
    z = _jl_bigint_init()
    ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpz_sqrt), Void, (Ptr{Void}, Ptr{Void}),z,x.mpz)
    BigInt(z)
end

function ^(x::BigInt, y::Uint)
    z = _jl_bigint_init()
    ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpz_pow_ui), Void, (Ptr{Void}, Ptr{Void}, Uint), z, x.mpz, y)
    BigInt(z)
end
^(x::BigInt, y::Integer) = y<0 ? throw(DomainError()) : ^(x, uint(y))

function gcd(x::BigInt, y::BigInt)
    z = _jl_bigint_init()
    ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpz_gcd), Void,
        (Ptr{Void}, Ptr{Void}, Ptr{Void}), z, x.mpz, y.mpz)
    BigInt(z)
end

function gcdx(a::BigInt, b::BigInt)
    g = _jl_bigint_init()
    s = _jl_bigint_init()
    t = _jl_bigint_init()
    ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpz_gcdext), Void,
        (Ptr{Void}, Ptr{Void}, Ptr{Void}, Ptr{Void}, Ptr{Void}),
        g, s, t, a.mpz, b.mpz)
    BigInt(g), BigInt(s), BigInt(t)
end

function factorial(bn::BigInt)
    if bn<0
        return BigInt(0)
    else
        n = uint(bn)
    end
    z = _jl_bigint_init()
    ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpz_fac_ui), Void,
        (Ptr{Void}, Uint), z, n)
    BigInt(z)
end

function binomial(n::BigInt, k::Uint)
    z = _jl_bigint_init()
    ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpz_bin_ui), Void,
        (Ptr{Void}, Ptr{Void}, Uint), z, n.mpz, k)
    BigInt(z)
end
binomial(n::BigInt, k::Integer) = k<0 ? throw(DomainError()) : binomial(n, uint(k))

==(x::BigInt, y::BigInt) = cmp(x,y) == 0
<=(x::BigInt, y::BigInt) = cmp(x,y) <= 0
>=(x::BigInt, y::BigInt) = cmp(x,y) >= 0
<(x::BigInt, y::BigInt) = cmp(x,y) < 0
>(x::BigInt, y::BigInt) = cmp(x,y) > 0

function string(x::BigInt)
    s=ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpz_printf), Ptr{Uint8}, (Ptr{Void},),x.mpz)
    ret = bytestring(s) #This copies s.
    ccall(dlsym(_jl_libgmp_wrapper,:_jl_gmp_free), Void, (Ptr{Void},), s)
    ret
end

function show(io, x::BigInt)
    print(io, string(x))
end

function _jl_bigint_clear(x::BigInt)
    ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpz_clear), Void, (Ptr{Void},),x.mpz)
end

function _jl_bigint_init()
    return ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpz_init), Ptr{Void}, ())
end
