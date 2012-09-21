_jl_libgmp_wrapper = dlopen("libgmp_wrapper")

require("bigint.jl")

type BigFloat <: FloatingPoint
    mpf::Ptr{Void}

    function BigFloat(x::String)
        z = _jl_BigFloat_init()
        ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpf_set_string), Void, (Ptr{Void}, Ptr{Uint8}), z, bytestring(x))
        b = new(z)
        finalizer(b, _jl_BigFloat_clear)
        b
    end

    function BigFloat(x::Float64)
        z = _jl_BigFloat_init()
        ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpf_set_d), Void, (Ptr{Void}, Float64), z, x)
        b = new(z)
        finalizer(b, _jl_BigFloat_clear)
        b
    end

    function BigFloat(x::Uint)
        z = _jl_BigFloat_init()
        ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpf_set_ui), Void, (Ptr{Void}, Uint), z, x)
        b = new(z)
        finalizer(b, _jl_BigFloat_clear)
        b
    end

    function BigFloat(x::Int)
        z = _jl_BigFloat_init()
        ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpf_set_si), Void, (Ptr{Void}, Int), z, x)
        b = new(z)
        finalizer(b, _jl_BigFloat_clear)
        b
    end

    function BigFloat(x::BigInt)
        z = _jl_BigFloat_init()
        ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpf_set_z), Void, (Ptr{Void}, Ptr{Void}), z, x.mpz)
        b = new(z)
        finalizer(b, _jl_BigFloat_clear)
        b
    end

    function BigFloat(z::Ptr{Void})
        b = new(z)
        finalizer(b, _jl_BigFloat_clear)
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

function +(x::BigFloat, y::BigFloat)
    z= _jl_BigFloat_init()
    ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpf_add), Void, (Ptr{Void}, Ptr{Void}, Ptr{Void}), z, x.mpf, y.mpf)
    BigFloat(z)
end

function -(x::BigFloat)
    z= _jl_BigFloat_init()
    ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpf_neg), Void, (Ptr{Void}, Ptr{Void}), z, x.mpf)
    BigFloat(z)
end

function -(x::BigFloat, y::BigFloat)
    z= _jl_BigFloat_init()
    ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpf_sub), Void, (Ptr{Void}, Ptr{Void}, Ptr{Void}), z, x.mpf, y.mpf)
    BigFloat(z)
end

function *(x::BigFloat, y::BigFloat)
    z= _jl_BigFloat_init()
    ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpf_mul), Void, (Ptr{Void}, Ptr{Void}, Ptr{Void}), z, x.mpf, y.mpf)
    BigFloat(z)
end

function /(x::BigFloat, y::BigFloat)
    z= _jl_BigFloat_init()
    ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpf_div), Void, (Ptr{Void}, Ptr{Void}, Ptr{Void}), z, x.mpf, y.mpf)
    BigFloat(z)
end

function cmp(x::BigFloat, y::BigFloat)
    ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpf_cmp), Int32, (Ptr{Void}, Ptr{Void}), x.mpf, y.mpf)
end

function sqrt(x::BigFloat)
    z = _jl_BigFloat_init()
    ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpf_sqrt), Void, (Ptr{Void}, Ptr{Void}), z, x.mpf)
    BigFloat(z)
end

function pow(x::BigFloat, y::Uint)
    z = _jl_BigFloat_init()
    ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpf_pow_ui), Void, (Ptr{Void}, Ptr{Void}, Uint), z, x.mpf, y)
    BigFloat(z)
end

==(x::BigFloat, y::BigFloat) = cmp(x,y) == 0
<=(x::BigFloat, y::BigFloat) = cmp(x,y) <= 0
>=(x::BigFloat, y::BigFloat) = cmp(x,y) >= 0
<(x::BigFloat, y::BigFloat) = cmp(x,y) < 0
>(x::BigFloat, y::BigFloat) = cmp(x,y) > 0

function string(x::BigFloat)
    s=ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpf_printf), Ptr{Uint8}, (Ptr{Void},), x.mpf)
    ret = bytestring(s) #This copies s.
    c_free(s)
    ret
end

show(io, b::BigFloat) = print(io, string(b))

function _jl_BigFloat_clear(x::BigFloat)
    ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpf_clear), Void, (Ptr{Void},), x.mpf)
end

function _jl_BigFloat_init()
    return ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpf_init), Ptr{Void}, ())
end
