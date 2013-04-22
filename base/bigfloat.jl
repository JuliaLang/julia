BigFloat_clear(mpf::Vector{Int32}) = ccall((:__gmpf_clear, :libgmp), Void, (Ptr{Void},), mpf)

immutable BigFloat <: FloatingPoint
    mpf::Vector{Int32}
    function BigFloat() 
        z = Array(Int32, 6)
        ccall((:__gmpf_init,:libgmp), Void, (Ptr{Void},), z)
        b = new(z)
        finalizer(b.mpf, BigFloat_clear)
        return b
    end
end

function BigFloat(x::BigFloat)
    z = BigFloat()
    ccall((:__gmpf_set, :libgmp), Void, (Ptr{Void}, Ptr{Void}), z.mpf, x.mpf)
    return z
end

function BigFloat(x::String)
    z = BigFloat()
    err = ccall((:__gmpf_set_str, :libgmp), Int32, (Ptr{Void}, Ptr{Uint8}, Int32), z.mpf, bytestring(x), 0)
    if err != 0; error("Invalid input"); end
    return z
end

function BigFloat(x::Float64)
    z = BigFloat()
    ccall((:__gmpf_set_d, :libgmp), Void, (Ptr{Void}, Float64), z.mpf, x)
    return z
end

function BigFloat(x::Uint)
    z = BigFloat()
    ccall((:__gmpf_set_ui, :libgmp), Void, (Ptr{Void}, Uint), z.mpf, x)
    return z
end

function BigFloat(x::Int)
    z = BigFloat()
    ccall((:__gmpf_set_si, :libgmp), Void, (Ptr{Void}, Int), z.mpf, x)
    return z
end

function BigFloat(x::BigInt)
    z = BigFloat()
    ccall((:__gmpf_set_z, :libgmp), Void, (Ptr{Void}, Ptr{Void}), z.mpf, x.mpz)
    return z
end

BigFloat(x::Bool) = BigFloat(uint(x))
BigFloat(x::Signed) = BigFloat(int(x))
BigFloat(x::Unsigned) = BigFloat(uint(x))
#BigFloat(x::Int128) = BigFloat(BigInt(x))
#BigFloat(x::Uint128) = BigFloat(BigInt(x))
if WORD_SIZE == 32
    BigFloat(x::Int64) = BigFloat(string(x))
    BigFloat(x::Uint64) = BigFloat(BigInt(x))
end
BigFloat(x::Float32) = BigFloat(float64(x))
BigFloat(x::Rational) = BigFloat(num(x)) / BigFloat(den(x))

convert(::Type{BigFloat}, x::Rational) = BigFloat(x) # to resolve ambiguity
convert(::Type{BigFloat}, x::Real) = BigFloat(x)

convert(::Type{Float64}, x::BigFloat) = ccall((:__gmpf_get_d,:libgmp), Float64, (Ptr{Void},), x.mpf)
convert(::Type{FloatingPoint}, x::BigInt) = BigFloat(x)

promote_rule{T<:Union(Integer,FloatingPoint)}(::Type{BigFloat}, ::Type{T}) = BigFloat
promote_rule{T<:FloatingPoint}(::Type{BigInt},::Type{T}) = BigFloat


# mpf doesn't have inf or nan
isnan(x::BigFloat) = false
isinf(x::BigFloat) = false

# Binary ops
for (fJ, fC) in ((:+,:add), (:-,:sub), (:*,:mul), (:/,:div))
    @eval begin 
        function ($fJ)(x::BigFloat, y::BigFloat)
            z = BigFloat()
            ccall(($(string(:__gmpf_,fC)),:libgmp), Void, (Ptr{Void}, Ptr{Void}, Ptr{Void}), z.mpf, x.mpf, y.mpf)
            return z
        end
    end
end

function -(x::BigFloat)
    z = BigFloat()
    ccall((:__gmpf_neg, :libgmp), Void, (Ptr{Void}, Ptr{Void}), z.mpf, x.mpf)
    return z
end

function cmp(x::BigFloat, y::BigFloat)
    ccall((:__gmpf_cmp, :libgmp), Int32, (Ptr{Void}, Ptr{Void}), x.mpf, y.mpf)
end

for f in (:sqrt, :ceil, :floor, :trunc)
    @eval begin
        function ($f)(x::BigFloat)
            z = BigFloat()
            ccall(($(string(:__gmpf_,f)), :libgmp), Void, (Ptr{Void}, Ptr{Void}), z.mpf, x.mpf)
            return z
        end
    end
end

function ^(x::BigFloat, y::Uint)
    z = BigFloat()
    ccall((:__gmpf_pow_ui, :libgmp), Void, (Ptr{Void}, Ptr{Void}, Uint), z.mpf, x.mpf, y)
    return z
end

^(x::Float32, y::BigInt) = BigFloat(x)^y
^(x::Float64, y::BigInt) = BigFloat(x)^y

==(x::BigFloat, y::BigFloat) = cmp(x,y) == 0
<=(x::BigFloat, y::BigFloat) = cmp(x,y) <= 0
>=(x::BigFloat, y::BigFloat) = cmp(x,y) >= 0
<(x::BigFloat, y::BigFloat) = cmp(x,y) < 0
>(x::BigFloat, y::BigFloat) = cmp(x,y) > 0

complex(x::BigFloat, y::BigFloat) = ComplexPair(x, y)

function string(x::BigFloat)
    lng = 128
    for i = 1:2
        z = Array(Uint8, lng)
        lng = ccall((:__gmp_snprintf,:libgmp), Int32, (Ptr{Uint8}, Uint, Ptr{Uint8}, Ptr{Void}...), z, lng, "%.Fe", x.mpf)
        if lng < 128 || i == 2; return bytestring(convert(Ptr{Uint8}, z[1:lng])); end
    end
end

show(io::IO, b::BigFloat) = print(io, string(b))
showcompact(io::IO, b::BigFloat) = print(io, string(b))
