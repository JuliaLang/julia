module MPC

export
    # Types
    MPCComplex,
    # Functions
    get_bigcomplex_precision,
    set_bigcomplex_precision,
    with_bigcomplex_precision
    
import
    Base: (*), +, -, /, <, <<, >>, <=, ==, >, >=, ^, (~), (&), (|), ($), cmp,
        complex, convert, div, imag, integer_valued, isfinite, isinf, isnan, 
        promote_rule, real, show, showcompact, sqrt, string, get_precision

const ROUNDING_MODE = [0]
const DEFAULT_PRECISION = [53, 53]

# Basic type and initialization definitions

type MPCComplex <: Number
    # Real MPFR part
    reprec::Clong
    resign::Cint
    reexp::Clong
    red::Ptr{Void}
    # Imaginary MPFR part
    imprec::Clong
    imsign::Cint
    imexp::Clong
    imd::Ptr{Void}
    function MPCComplex()
        N, P = get_bigcomplex_precision()
        z = new(convert(Clong, 0), convert(Cint, 0), convert(Clong, 0),
            C_NULL, convert(Clong, 0), convert(Cint, 0), convert(Clong, 0), C_NULL)
        ccall((:mpc_init3,:libmpc), Void, (Ptr{MPCComplex}, Clong, Clong), &z, N, P)
        finalizer(z, MPC_clear)
        return z
    end
end

MPC_clear(mpc::MPCComplex) = ccall((:mpc_clear, :libmpc), Void, (Ptr{MPCComplex},), &mpc)

MPCComplex(x::MPCComplex) = x

# Real constructors
for (fJ, fC) in ((:si,:Int), (:ui,:Uint), (:d,:Float64))
    @eval begin
        function MPCComplex(x::($fC))
            z = MPCComplex()
            ccall(($(string(:mpc_set_,fJ)), :libmpc), Int32, (Ptr{MPCComplex}, ($fC), Int32), &z, x, ROUNDING_MODE[end])
            return z
        end
    end
end

function MPCComplex(x::BigInt)
    z = MPCComplex()
    ccall((:mpc_set_z, :libmpc), Int32, (Ptr{MPCComplex}, Ptr{BigInt}, Int32), &z, &x, ROUNDING_MODE[end])
    return z
end

function MPCComplex(x::BigFloat)
    z = MPCComplex()
    ccall((:mpc_set_f, :libmpc), Int32, (Ptr{MPCComplex}, Ptr{Void}, Int32), &z, x.mpf, ROUNDING_MODE[end])
    return z
end

function MPCComplex(x::MPFRFloat)
    z = MPCComplex()
    ccall((:mpc_set_fr, :libmpc), Int32, (Ptr{MPCComplex}, Ptr{MPFRFloat}, Int32), &z, &x, ROUNDING_MODE[end])
    return z
end

function MPCComplex(x::String, base::Int)
    z = MPCComplex()
    err = ccall((:mpc_set_str, :libmpc), Int32, (Ptr{MPCComplex}, Ptr{Uint8}, Int32, Int32), &z, x, base, ROUNDING_MODE[end])
    if err != 0; error("Invalid input"); end
    return z
end
MPCComplex(x::String) = MPCComplex(x, 10)

MPCComplex(x::Bool) = MPCComplex(uint(x))
MPCComplex(x::Signed) = MPCComplex(int(x))
MPCComplex(x::Unsigned) = MPCComplex(uint(x))
if WORD_SIZE == 32
    MPCComplex(x::Int64) = MPCComplex(BigInt(x))
    MPCComplex(x::Uint64) = MPCComplex(BigInt(x))
end
MPCComplex(x::Float32) = MPCComplex(float64(x))
MPCComplex(x::Rational) = MPCComplex(num(x)) / MPCComplex(den(x))

convert(::Type{MPCComplex}, x::Rational) = MPCComplex(x) # to resolve ambiguity
convert(::Type{MPCComplex}, x::Real) = MPCComplex(x)
convert(::Type{MPCComplex}, x::Complex) = MPCComplex(x)
convert(::Type{MPCComplex}, x::ImaginaryUnit) = MPCComplex(x)

function convert(::Type{Complex{Float64}}, x::MPCComplex)
    return complex(
        ccall((:mpfr_get_d,:libmpfr), Float64, (Ptr{Void},), &(realref(x))),
        ccall((:mpfr_get_d,:libmpfr), Float64, (Ptr{Void},), &(imagref(x))))
end
function convert(::Type{Complex{Float32}}, x::MPCComplex)
    return complex(
        ccall((:mpfr_get_flt,:libmpfr), Float32, (Ptr{Void},), &(realref(x))),
        ccall((:mpfr_get_flt,:libmpfr), Float32, (Ptr{Void},), &(imagref(x))))
end
function convert(::Type{Complex{Int64}}, x::MPCComplex)
    if realint(x) && imagint(x)
        return complex(
            ccall((:mpfr_get_si,:libmpfr), Int64, (Ptr{Void}, Int32), &(realref(x)), ROUNDING_MODE[end]),
            ccall((:mpfr_get_si,:libmpfr), Int64, (Ptr{Void}, Int32), &(imagref(x)), ROUNDING_MODE[end]))
    else
        throw(InexactError())
    end
end
function convert(::Type{Complex{Int32}}, x::MPCComplex)
    if realint(x) && imagint(x)
        return complex(
            int32(ccall((:mpfr_get_si,:libmpfr), Int64, (Ptr{Void}, Int32), &(realref(x)), ROUNDING_MODE[end])),
            int32(ccall((:mpfr_get_si,:libmpfr), Int64, (Ptr{Void}, Int32), &(imagref(x)), ROUNDING_MODE[end])))
    else
        throw(InexactError())
    end
end
promote_rule{T<:Real}(::Type{MPCComplex}, ::Type{T}) = MPCComplex
promote_rule{T<:Real}(::Type{MPCComplex}, ::Type{Complex{T}}) = MPCComplex
promote_rule{T<:Number}(::Type{MPCComplex}, ::Type{T}) = MPCComplex
promote_rule(::Type{MPCComplex}, ::Type{ImaginaryUnit}) = MPCComplex

# Complex constructors
for (fJ, fC) in ((:si,:Int), (:ui,:Uint), (:d,:Float64))
    @eval begin
        function MPCComplex(x::($fC), y::($fC))
            z = MPCComplex()
            ccall(($(string(:mpc_set_,fJ,'_',fJ)), :libmpc), Int32, (Ptr{MPCComplex}, ($fC), ($fC), Int32), &z, x, y, ROUNDING_MODE[end])
            return z
        end
    end
end

function MPCComplex(x::BigInt, y::BigInt)
    z = MPCComplex()
    ccall((:mpc_set_z_z, :libmpc), Int32, (Ptr{MPCComplex}, Ptr{BigInt}, Ptr{BigInt}, Int32), &z, &x, &y, ROUNDING_MODE[end])
    return z
end

function MPCComplex(x::BigFloat, y::BigFloat)
    z = MPCComplex()
    ccall((:mpc_set_f_f, :libmpc), Int32, (Ptr{MPCComplex}, Ptr{Void}, Ptr{Void}, Int32), &z, x.mpf, y.mpf, ROUNDING_MODE[end])
    return z
end

function MPCComplex(x::MPFRFloat, y::MPFRFloat)
    z = MPCComplex()
    ccall((:mpc_set_fr_fr, :libmpc), Int32, (Ptr{MPCComplex}, Ptr{MPFRFloat}, Ptr{MPFRFloat}, Int32), &z, &x, &y, ROUNDING_MODE[end])
    return z
end

MPCComplex(x::Complex) = MPCComplex(real(x), imag(x))
MPCComplex(x::ImaginaryUnit) = MPCComplex(1im)
MPCComplex(x::Bool, y::Bool) = MPCComplex(uint(x), uint(y))
MPCComplex(x::Signed, y::Signed) = MPCComplex(int(x), int(y))
MPCComplex(x::Unsigned, y::Unsigned) = MPCComplex(uint(x), uint(y))
if WORD_SIZE == 32
    MPCComplex(x::Int64, y::Int64) = MPCComplex(BigInt(x), BigInt(y))
    MPCComplex(x::Uint64, y::Uint64) = MPCComplex(BigInt(x), BigInt(y))
end
MPCComplex(x::Float32, y::Float32) = MPCComplex(float64(x), float64(y))
MPCComplex(x::Rational, y::Rational) = MPCComplex(MPFRFloat(num(x))/MPFRFloat(den(x)),
                                        MPFRFloat(num(y))/MPFRFloat(den(y)))

# Basic operations

for (fJ, fC) in ((:+,:add), (:-,:sub), (:*,:mul), (:/,:div), (:^, :pow))
    @eval begin 
        function ($fJ)(x::MPCComplex, y::MPCComplex)
            z = MPCComplex()
            ccall(($(string(:mpc_,fC)),:libmpc), Int32, (Ptr{MPCComplex}, Ptr{MPCComplex}, Ptr{MPCComplex}, Int32), &z, &x, &y, ROUNDING_MODE[end])
            return z
        end
    end
end

function -(x::MPCComplex)
    z = MPCComplex()
    ccall((:mpc_neg, :libmpc), Int32, (Ptr{MPCComplex}, Ptr{MPCComplex}, Int32), &z, &x, ROUNDING_MODE[end])
    return z
end

function cmp(x::MPCComplex, y::MPCComplex)
    ccall((:mpc_cmp, :libmpc), Int32, (Ptr{MPCComplex}, Ptr{MPCComplex}), &x, &y)
end

function sqrt(x::MPCComplex)
    z = MPCComplex()
    ccall((:mpc_sqrt, :libmpc), Int32, (Ptr{MPCComplex}, Ptr{MPCComplex}, Int32), &z, &x, ROUNDING_MODE[end])
    if isnan(z)
        throw(DomainError())
    end
    return z
end

function ^(x::MPCComplex, y::Uint)
    z = MPCComplex()
    ccall((:mpc_pow_ui, :libmpc), Int32, (Ptr{MPCComplex}, Ptr{MPCComplex}, Uint, Int32), &z, &x, y, ROUNDING_MODE[end])
    return z
end

function ^(x::MPCComplex, y::Int)
    z = MPCComplex()
    ccall((:mpc_pow_si, :libmpc), Int32, (Ptr{MPCComplex}, Ptr{MPCComplex}, Int, Int32), &z, &x, y, ROUNDING_MODE[end])
    return z
end

function ^(x::MPCComplex, y::BigInt)
    z = MPCComplex()
    ccall((:mpc_pow_z, :libmpc), Int32, (Ptr{MPCComplex}, Ptr{MPCComplex}, Ptr{BigInt}, Int32), &z, &x, &y, ROUNDING_MODE[end])
    return z
end

# Utility functions
==(x::MPCComplex, y::MPCComplex) = ccall((:mpc_cmp, :libmpc), Int32, (Ptr{MPCComplex}, Ptr{MPCComplex}), &x, &y) == 0

function get_precision(x::MPCComplex)
    a = [0]
    b = [0]
    ccall((:mpc_get_prec2, :libmpc), Int, (Ptr{Int}, Ptr{Int}, Ptr{MPCComplex}), a, b, &x)
    return (a[1],b[1])
end

get_bigcomplex_precision() = (DEFAULT_PRECISION[1],DEFAULT_PRECISION[end])
function set_bigcomplex_precision(x::Int, y::Int)
    if x < 2
        throw(DomainError())
    end
    DEFAULT_PRECISION[1], DEFAULT_PRECISION[end] = x, y
end
set_bigcomplex_precision(x::(Int,Int)) = set_bigcomplex_precision(x...)

isfinite(x::MPCComplex) = isfinite(real(x)) && isfinite(imag(x))
isinf(x::MPCComplex) = !isfinite(x)
integer_valued(x::MPCComplex) = imag(x) == 0 && integer_valued(real(x))

function with_bigcomplex_precision(f::Function, realprec::Integer, imagprec::Integer)
    old_precision = get_bigcomplex_precision()
    set_bigcomplex_precision(realprec, imagprec)
    ret = f()
    set_bigcomplex_precision(old_precision)
    return ret
end
with_bigcomplex_precision(f::Function, prec::Integer) = with_bigcomplex_precision(f, prec, prec)

function imag(x::MPCComplex)
    z = with_bigfloat_precision(get_bigcomplex_precision()[end]) do
        MPFRFloat()
    end
    ccall((:mpc_imag, :libmpc), Int32, (Ptr{MPFRFloat}, Ptr{MPCComplex}, Int32), &z, &x, ROUNDING_MODE[end])
    return z
end

function real(x::MPCComplex)
    z = with_bigfloat_precision(get_bigcomplex_precision()[1]) do
        MPFRFloat()
    end
    ccall((:mpc_real, :libmpc), Int32, (Ptr{MPFRFloat}, Ptr{MPCComplex}, Int32), &z, &x, ROUNDING_MODE[end])
    return z
end

string(x::MPCComplex) = "$(string(real(x))) + $(string(imag(x)))im"

show(io::IO, b::MPCComplex) = print(io, string(b) * " with $(get_precision(b)) bits of precision")
showcompact(io::IO, b::MPCComplex) = print(io, string(b))

function copy(x::MPCComplex)
    z = MPCComplex()
    ccall((:mpc_set, :libmpc), Int32, (Ptr{MPCComplex}, Ptr{MPCComplex}, Int32), &z, &x, ROUNDING_MODE)
    return z
end

# Internal functions
# Unsafe for general use
realref(x::MPCComplex) = MPFRFloat(x.reprec, x.resign, x.reexp, x.red)
imagref(x::MPCComplex) = MPFRFloat(x.imprec, x.imsign, x.imexp, x.imd)
realint(x::MPCComplex) = ccall((:mpfr_integer_p, :libmpfr), Int32, (Ptr{Void},), &(realref(x))) != 0
imagint(x::MPCComplex) = ccall((:mpfr_integer_p, :libmpfr), Int32, (Ptr{Void},), &(imagref(x))) != 0


end #module
