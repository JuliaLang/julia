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

function BigInt(x::Int)
    z = BigInt()
    ccall((:__gmpz_set_si, :libgmp), Void, (Ptr{BigInt}, Clong), &z, x)
    return z
end

function BigInt(x::Uint)
    z = BigInt()
    ccall((:__gmpz_set_ui, :libgmp), Void,(Ptr{BigInt}, Culong), &z, x)
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
    convert(Int, ccall((:__gmpz_get_si, :libgmp), Clong, (Ptr{BigInt},), &n))

convert(::Type{Uint}, n::BigInt) =
    convert(Uint, ccall((:__gmpz_get_ui, :libgmp), Culong, (Ptr{BigInt},), &n))

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
    ccall((:__gmpz_mul_si, :libgmp), Void, (Ptr{BigInt}, Ptr{BigInt}, Culong), &z, &x, c)
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

function sqrt(x::BigInt)
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

function gcdx(a::BigInt, b::BigInt)
    g = BigInt()
    s = BigInt()
    t = BigInt()
    ccall((:__gmpz_gcdext, :libgmp), Void,
        (Ptr{BigInt}, Ptr{BigInt}, Ptr{BigInt}, Ptr{BigInt}, Ptr{BigInt}),
        &g, &s, &t, &a, &b)
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
