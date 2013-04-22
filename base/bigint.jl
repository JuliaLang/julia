BigInt_clear(mpz::Vector{Int32}) = ccall((:__gmpz_clear, :libgmp), Void, (Ptr{Void},), mpz)

immutable BigInt <: Integer
    mpz::Vector{Int32}
    function BigInt() 
        z = Array(Int32, 4)
        ccall((:__gmpz_init,:libgmp), Void, (Ptr{Void},), z)
        b = new(z)
        finalizer(b.mpz, BigInt_clear)
        return b
    end
end

function BigInt(x::BigInt)
    z = BigInt()
    ccall((:__gmpz_set, :libgmp), Void, (Ptr{Void}, Ptr{Void}), z.mpz, x.mpz)
    return z
end

function BigInt(x::String)
    z = BigInt()
    err = ccall((:__gmpz_set_str, :libgmp), Int32, (Ptr{Void}, Ptr{Uint8}, Int32), z.mpz, bytestring(x), 0)
    if err != 0; error("Invalid input"); end
    return z
end

function BigInt(x::Int)
    z = BigInt()
    ccall((:__gmpz_set_si, :libgmp), Void, (Ptr{Void}, Int), z.mpz, x)
    return z
end

function BigInt(x::Uint)
    z = BigInt()
    ccall((:__gmpz_set_ui, :libgmp), Void,
        (Ptr{Void}, Uint), z.mpz, x)
    return z
end

BigInt(x::Bool) = BigInt(uint(x))
BigInt(x::Signed) = BigInt(int(x))
BigInt(x::Unsigned) = BigInt(uint(x))
#BigInt(x::Int128) = BigInt(string(x))
#BigInt(x::Uint128) = BigInt(string(x))
if WORD_SIZE == 32
    BigInt(x::Int64) = BigInt(string(x))
    BigInt(x::Uint64) = BigInt(string(x))
end

convert{T<:Integer}(::Type{BigInt}, x::T) = BigInt(x)

convert(::Type{Int}, n::BigInt) =
    ccall((:__gmpz_get_si, :libgmp), Int, (Ptr{Void},), n.mpz)

convert(::Type{Uint}, n::BigInt) =
    ccall((:__gmpz_get_ui, :libgmp), Uint, (Ptr{Void},), n.mpz)

promote_rule{T<:Integer}(::Type{BigInt}, ::Type{T}) = BigInt

# Binary ops
for (fJ, fC) in ((:+, :add), (:-,:sub), (:*, :mul),
                 (:fld, :fdiv_q), (:div, :tdiv_q), (:mod, :fdiv_r), (:rem, :tdiv_r),
                 (:gcd, :gcd), (:lcm, :lcm),
                 (:&, :and), (:|, :ior), (:$, :xor))
    @eval begin
        function ($fJ)(x::BigInt, y::BigInt)
            z = BigInt()
            ccall(($(string(:__gmpz_,fC)), :libgmp), Void, (Ptr{Void}, Ptr{Void}, Ptr{Void}), z.mpz, x.mpz, y.mpz)
            return z
        end
    end
end

# unary ops
for (fJ, fC) in ((:-, :neg), (:~, :com))
    @eval begin
        function ($fJ)(x::BigInt)
            z = BigInt()
            ccall(($(string(:__gmpz_,fC)), :libgmp), Void, (Ptr{Void}, Ptr{Void}), z.mpz, x.mpz)
            return z
        end
    end
end

function <<(x::BigInt, c::Uint)
    z = BigInt()
    ccall((:__gmpz_mul_2exp, :libgmp), Void, (Ptr{Void}, Ptr{Void}, Uint), z.mpz, x.mpz, c)
    return z
end
<<(x::BigInt, c::Int32)   = c<0 ? throw(DomainError()) : x<<uint(c)
<<(x::BigInt, c::Integer) = c<0 ? throw(DomainError()) : x<<uint(c)

function >>(x::BigInt, c::Uint)
    z = BigInt()
    ccall((:__gmpz_fdiv_q_2exp, :libgmp), Void, (Ptr{Void}, Ptr{Void}, Uint), z.mpz, x.mpz, c)
    return z
end
>>(x::BigInt, c::Int32)   = c<0 ? throw(DomainError()) : x>>uint(c)
>>(x::BigInt, c::Integer) = c<0 ? throw(DomainError()) : x>>uint(c)

function divrem(x::BigInt, y::BigInt)
    z1 = BigInt()
    z2 = BigInt()
    ccall((:__gmpz_tdiv_qr, :libgmp), Void, (Ptr{Void}, Ptr{Void}, Ptr{Void}, Ptr{Void}), z1.mpz, z2.mpz, x.mpz, y.mpz)
    z1, z2
end

function cmp(x::BigInt, y::BigInt)
    ccall((:__gmpz_cmp, :libgmp), Int32, (Ptr{Void}, Ptr{Void}), x.mpz, y.mpz)
end

function sqrt(x::BigInt)
    z = BigInt()
    ccall((:__gmpz_sqrt, :libgmp), Void, (Ptr{Void}, Ptr{Void}), z.mpz, x.mpz)
    return z
end

function ^(x::BigInt, y::Uint)
    z = BigInt()
    ccall((:__gmpz_pow_ui, :libgmp), Void, (Ptr{Void}, Ptr{Void}, Uint), z.mpz, x.mpz, y)
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

function gcdx(a::BigInt, b::BigInt)
    g = BigInt()
    s = BigInt()
    t = BigInt()
    ccall((:__gmpz_gcdext, :libgmp), Void,
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
    z = BigInt()
    ccall((:__gmpz_fac_ui, :libgmp), Void,
        (Ptr{Void}, Uint), z.mpz, n)
    return z
end

function binomial(n::BigInt, k::Uint)
    z = BigInt()
    ccall((:__gmpz_bin_ui, :libgmp), Void,
        (Ptr{Void}, Ptr{Void}, Uint), z.mpz, n.mpz, k)
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
    lng = ccall((:__gmp_snprintf,:libgmp), Int32, (Ptr{Uint8}, Uint, Ptr{Uint8}, Ptr{Void}...), z, lng, "%Zd", x.mpz)
    return bytestring(convert(Ptr{Uint8}, z[1:lng]))
end

function show(io::IO, x::BigInt)
    print(io, string(x))
end

ndigits(x::BigInt) = ccall((:__gmpz_sizeinbase,:libgmp), Uint, (Ptr{Void}, Int32), x.mpz, 10)
