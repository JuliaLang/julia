import Base.convert, Base.promote_rule, Base.+, Base.-, Base.*, Base.<<
import Base.^, Base.div, Base.rem, Base.cmp, Base.sqrt
import Base.gcd, Base.gcdx, Base.factorial, Base.binomial
import Base.==, Base.<=, Base.>=, Base.<, Base.>, Base.string, Base.show

type BigInt <: Integer
    mpz::Ptr{Void}

    function BigInt(x::String)
        z = BigInt_init()
        ccall((:jl_mpz_set_string, :libgmp_wrapper), Void, (Ptr{Void}, Ptr{Uint8}),z,bytestring(x))
        b = new(z)
        finalizer(b, BigInt_clear)
        b
    end

    function BigInt(x::Int)
        z = BigInt_init()
        ccall((:jl_mpz_set_si, :libgmp_wrapper), Void, (Ptr{Void}, Int),z,x)
        b = new(z)
        finalizer(b, BigInt_clear)
        b
    end
    BigInt{T<:Signed}(x::T) = BigInt(int(x))
    BigInt(x::Int128) = BigInt(string(x))

    function BigInt(x::Uint)
        z = BigInt_init()
        ccall((:jl_mpz_set_ui, :libgmp_wrapper), Void,
            (Ptr{Void}, Uint), z, x)
        b = new(z)
        finalizer(b, BigInt_clear)
        b
    end
    BigInt{T<:Unsigned}(x::T) = BigInt(uint(x))
    BigInt(x::Uint128) = BigInt(string(x))

    function BigInt(z::Ptr{Void})
        b = new(z)
        finalizer(b, BigInt_clear)
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
    ccall((:jl_mpz_get_si, :libgmp_wrapper), Int, (Ptr{Void},), n.mpz)

convert(::Type{Uint}, n::BigInt) =
    ccall((:jl_mpz_get_ui, :libgmp_wrapper), Uint, (Ptr{Void},), n.mpz)

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
    z= BigInt_init()
    ccall((:jl_mpz_add, :libgmp_wrapper), Void, (Ptr{Void}, Ptr{Void}, Ptr{Void}),z,x.mpz,y.mpz)
    BigInt(z)
end

function -(x::BigInt)
    z= BigInt_init()
    ccall((:jl_mpz_neg, :libgmp_wrapper), Void, (Ptr{Void}, Ptr{Void}),z,x.mpz)
    BigInt(z)
end

function -(x::BigInt, y::BigInt)
    z= BigInt_init()
    ccall((:jl_mpz_sub, :libgmp_wrapper), Void, (Ptr{Void}, Ptr{Void}, Ptr{Void}),z,x.mpz,y.mpz)
    BigInt(z)
end

function *(x::BigInt, y::BigInt)
    z= BigInt_init()
    ccall((:jl_mpz_mul, :libgmp_wrapper), Void, (Ptr{Void}, Ptr{Void}, Ptr{Void}),z,x.mpz,y.mpz)
    BigInt(z)
end

function <<(x::BigInt, c::Uint)
    z= BigInt_init()
    ccall((:jl_mpz_lshift, :libgmp_wrapper), Void, (Ptr{Void}, Ptr{Void}, Uint), z, x.mpz, c)
    BigInt(z)
end
<<(x::BigInt, c::Int32)   = c<0 ? throw(DomainError()) : x<<uint(c)
<<(x::BigInt, c::Integer) = c<0 ? throw(DomainError()) : x<<uint(c)

function div(x::BigInt, y::BigInt)
    z= BigInt_init()
    ccall((:jl_mpz_div, :libgmp_wrapper), Void, (Ptr{Void}, Ptr{Void}, Ptr{Void}),z,x.mpz,y.mpz)
    BigInt(z)
end

function divmod(x::BigInt, y::BigInt)
    z1= BigInt_init()
    z2= BigInt_init()
    ccall((:jl_mpz_divmod, :libgmp_wrapper), Void, (Ptr{Void}, Ptr{Void}, Ptr{Void}, Ptr{Void}), z1, z2, x.mpz, y.mpz)
    BigInt(z1),BigInt(z2)
end

function rem(x::BigInt, y::BigInt)
    z= BigInt_init()
    ccall((:jl_mpz_rem, :libgmp_wrapper), Void, (Ptr{Void}, Ptr{Void}, Ptr{Void}),z,x.mpz,y.mpz)
    BigInt(z)
end

function cmp(x::BigInt, y::BigInt)
    ccall((:jl_mpz_cmp, :libgmp_wrapper), Int32, (Ptr{Void}, Ptr{Void}),x.mpz, y.mpz)
end

function sqrt(x::BigInt)
    z = BigInt_init()
    ccall((:jl_mpz_sqrt, :libgmp_wrapper), Void, (Ptr{Void}, Ptr{Void}),z,x.mpz)
    BigInt(z)
end

function ^(x::BigInt, y::Uint)
    z = BigInt_init()
    ccall((:jl_mpz_pow_ui, :libgmp_wrapper), Void, (Ptr{Void}, Ptr{Void}, Uint), z, x.mpz, y)
    BigInt(z)
end
^(x::BigInt, y::Integer) = y<0 ? throw(DomainError()) : ^(x, uint(y))

function gcd(x::BigInt, y::BigInt)
    z = BigInt_init()
    ccall((:jl_mpz_gcd, :libgmp_wrapper), Void,
        (Ptr{Void}, Ptr{Void}, Ptr{Void}), z, x.mpz, y.mpz)
    BigInt(z)
end

function gcdx(a::BigInt, b::BigInt)
    g = BigInt_init()
    s = BigInt_init()
    t = BigInt_init()
    ccall((:jl_mpz_gcdext, :libgmp_wrapper), Void,
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
    z = BigInt_init()
    ccall((:jl_mpz_fac_ui, :libgmp_wrapper), Void,
        (Ptr{Void}, Uint), z, n)
    BigInt(z)
end

function binomial(n::BigInt, k::Uint)
    z = BigInt_init()
    ccall((:jl_mpz_bin_ui, :libgmp_wrapper), Void,
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
    s=ccall((:jl_mpz_printf, :libgmp_wrapper), Ptr{Uint8}, (Ptr{Void},),x.mpz)
    ret = bytestring(s) #This copies s.
    ccall((:jl_gmp_free, :libgmp_wrapper), Void, (Ptr{Void},), s)
    ret
end

function show(io, x::BigInt)
    print(io, string(x))
end

function BigInt_clear(x::BigInt)
    ccall((:jl_mpz_clear, :libgmp_wrapper), Void, (Ptr{Void},),x.mpz)
end

function BigInt_init()
    return ccall((:jl_mpz_init, :libgmp_wrapper), Ptr{Void}, ())
end
