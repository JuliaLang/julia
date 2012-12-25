import Base.convert, Base.promote_rule, Base.+, Base.-, Base.*, Base./
import Base.isnan, Base.isinf, Base.^, Base.cmp, Base.sqrt
import Base.==, Base.<=, Base.>=, Base.<, Base.>, Base.string, Base.show
import Base.showcompact

require("bigint")

type BigFloat <: FloatingPoint
    mpf::Ptr{Void}

    function BigFloat(x::String)
        z = BigFloat_init()
        ccall((:jl_mpf_set_string, :libgmp_wrapper), Void, (Ptr{Void}, Ptr{Uint8}), z, bytestring(x))
        b = new(z)
        finalizer(b, BigFloat_clear)
        b
    end

    function BigFloat(x::Float64)
        z = BigFloat_init()
        ccall((:jl_mpf_set_d, :libgmp_wrapper), Void, (Ptr{Void}, Float64), z, x)
        b = new(z)
        finalizer(b, BigFloat_clear)
        b
    end

    function BigFloat(x::Uint)
        z = BigFloat_init()
        ccall((:jl_mpf_set_ui, :libgmp_wrapper), Void, (Ptr{Void}, Uint), z, x)
        b = new(z)
        finalizer(b, BigFloat_clear)
        b
    end

    function BigFloat(x::Int)
        z = BigFloat_init()
        ccall((:jl_mpf_set_si, :libgmp_wrapper), Void, (Ptr{Void}, Int), z, x)
        b = new(z)
        finalizer(b, BigFloat_clear)
        b
    end

    function BigFloat(x::BigInt)
        z = BigFloat_init()
        ccall((:jl_mpf_set_z, :libgmp_wrapper), Void, (Ptr{Void}, Ptr{Void}), z, x.mpz)
        b = new(z)
        finalizer(b, BigFloat_clear)
        b
    end

    function BigFloat(z::Ptr{Void})
        b = new(z)
        finalizer(b, BigFloat_clear)
        b
    end
end

convert(::Type{BigFloat}, x::Int8)   = BigFloat(int(x))
convert(::Type{BigFloat}, x::Int16)  = BigFloat(int(x))
convert(::Type{BigFloat}, x::Int)  = BigFloat(x)
if WORD_SIZE == 64
    convert(::Type{BigFloat}, x::Int32) = BigFloat(int(x))
    convert(::Type{BigFloat}, x::Uint32) = BigFloat(int(x))
else
    convert(::Type{BigFloat}, x::Int64) = BigFloat(string(x))
    convert(::Type{BigFloat}, x::Uint64) = BigFloat(string(x))
end
convert(::Type{BigFloat}, x::Uint8)  = BigFloat(int(x))
convert(::Type{BigFloat}, x::Uint16) = BigFloat(int(x))
convert(::Type{BigFloat}, x::Float64) = BigFloat(x)
convert(::Type{BigFloat}, x::Float32) = BigFloat(float64(x))

promote_rule(::Type{BigFloat}, ::Type{Float32}) = BigFloat
promote_rule(::Type{BigFloat}, ::Type{Float64}) = BigFloat
promote_rule(::Type{BigFloat}, ::Type{Int8}) = BigFloat
promote_rule(::Type{BigFloat}, ::Type{Int16}) = BigFloat
promote_rule(::Type{BigFloat}, ::Type{Int32}) = BigFloat
promote_rule(::Type{BigFloat}, ::Type{Int64}) = BigFloat
promote_rule(::Type{BigFloat}, ::Type{Uint8}) = BigFloat
promote_rule(::Type{BigFloat}, ::Type{Uint16}) = BigFloat
promote_rule(::Type{BigFloat}, ::Type{Uint32}) = BigFloat
promote_rule(::Type{BigFloat}, ::Type{Uint64}) = BigFloat

# mpf doesn't have inf or nan
isnan(x::BigFloat) = false
isinf(x::BigFloat) = false

function +(x::BigFloat, y::BigFloat)
    z= BigFloat_init()
    ccall((:jl_mpf_add, :libgmp_wrapper), Void, (Ptr{Void}, Ptr{Void}, Ptr{Void}), z, x.mpf, y.mpf)
    BigFloat(z)
end

function -(x::BigFloat)
    z= BigFloat_init()
    ccall((:jl_mpf_neg, :libgmp_wrapper), Void, (Ptr{Void}, Ptr{Void}), z, x.mpf)
    BigFloat(z)
end

function -(x::BigFloat, y::BigFloat)
    z= BigFloat_init()
    ccall((:jl_mpf_sub, :libgmp_wrapper), Void, (Ptr{Void}, Ptr{Void}, Ptr{Void}), z, x.mpf, y.mpf)
    BigFloat(z)
end

function *(x::BigFloat, y::BigFloat)
    z= BigFloat_init()
    ccall((:jl_mpf_mul, :libgmp_wrapper), Void, (Ptr{Void}, Ptr{Void}, Ptr{Void}), z, x.mpf, y.mpf)
    BigFloat(z)
end

function /(x::BigFloat, y::BigFloat)
    z= BigFloat_init()
    ccall((:jl_mpf_div, :libgmp_wrapper), Void, (Ptr{Void}, Ptr{Void}, Ptr{Void}), z, x.mpf, y.mpf)
    BigFloat(z)
end

function cmp(x::BigFloat, y::BigFloat)
    ccall((:jl_mpf_cmp, :libgmp_wrapper), Int32, (Ptr{Void}, Ptr{Void}), x.mpf, y.mpf)
end

function sqrt(x::BigFloat)
    z = BigFloat_init()
    ccall((:jl_mpf_sqrt, :libgmp_wrapper), Void, (Ptr{Void}, Ptr{Void}), z, x.mpf)
    BigFloat(z)
end

function ^(x::BigFloat, y::Uint)
    z = BigFloat_init()
    ccall((:jl_mpf_pow_ui, :libgmp_wrapper), Void, (Ptr{Void}, Ptr{Void}, Uint), z, x.mpf, y)
    BigFloat(z)
end

==(x::BigFloat, y::BigFloat) = cmp(x,y) == 0
<=(x::BigFloat, y::BigFloat) = cmp(x,y) <= 0
>=(x::BigFloat, y::BigFloat) = cmp(x,y) >= 0
<(x::BigFloat, y::BigFloat) = cmp(x,y) < 0
>(x::BigFloat, y::BigFloat) = cmp(x,y) > 0

function string(x::BigFloat)
    s=ccall((:jl_mpf_printf, :libgmp_wrapper), Ptr{Uint8}, (Ptr{Void},), x.mpf)
    ret = bytestring(s) #This copies s.
    ccall((:jl_gmp_free, :libgmp_wrapper), Void, (Ptr{Void},), s)
    ret
end

show(io, b::BigFloat) = print(io, string(b))
showcompact(io, b::BigFloat) = print(io, string(b))

function BigFloat_clear(x::BigFloat)
    ccall((:jl_mpf_clear, :libgmp_wrapper), Void, (Ptr{Void},), x.mpf)
end

function BigFloat_init()
    return ccall((:jl_mpf_init, :libgmp_wrapper), Ptr{Void}, ())
end
