module MPC

export
    # Types
    MPCComplex,
    # Functions
    prec2,
    get_complex_precision,
    set_complex_precision,
    with_complex_precision
    
import
    Base: (*), +, -, /, <, <<, >>, <=, ==, >, >=, ^, (~), (&), (|), ($), cmp,
        complex, convert, div, imag, integer_valued, isfinite, isinf, isnan, 
        promote_rule, real, show, showcompact, sqrt, string, prec

const ROUNDING_MODE = 0
const DEFAULT_PRECISION = [53, 53]

# Basic type and initialization definitions

immutable MPCComplex{N,P} <: Number
    mpc::Vector{Int32}
    function MPCComplex()
        if N < 2 || P < 2
            error("Invalid precision")
        end
        z = Array(Int32, 16)
        ccall((:mpc_init3,:libmpc), Void, (Ptr{Void}, Int32, Int32), z, N, P)
        b = new(z)
        finalizer(b.mpc, MPC_clear)
        return b
    end
end

MPC_clear(mpc::Vector{Int32}) = ccall((:mpc_clear, :libmpc), Void, (Ptr{Void},), mpc)

function MPCComplex{N,P}(x::MPCComplex{N,P})
    z = MPCComplex{N,P}()
    ccall((:mpc_set, :libmpc), Int32, (Ptr{Void}, Ptr{Void}, Int32), z.mpc, x.mpc, ROUNDING_MODE)
    return z
end

# Real constructors
for (fJ, fC) in ((:si,:Int), (:ui,:Uint), (:d,:Float64))
    @eval begin
        function MPCComplex(x::($fC))
            z = MPCComplex{DEFAULT_PRECISION[1], DEFAULT_PRECISION[end]}()
            ccall(($(string(:mpc_set_,fJ)), :libmpc), Int32, (Ptr{Void}, ($fC), Int32), z.mpc, x, ROUNDING_MODE)
            return z
        end
    end
end

function MPCComplex(x::BigInt)
    z = MPCComplex{DEFAULT_PRECISION[1],DEFAULT_PRECISION[end]}()
    ccall((:mpc_set_z, :libmpc), Int32, (Ptr{Void}, Ptr{Void}, Int32), z.mpc, &(x.mpz), ROUNDING_MODE)
    return z
end

function MPCComplex(x::BigFloat)
    z = MPCComplex{DEFAULT_PRECISION[1],DEFAULT_PRECISION[end]}()
    ccall((:mpc_set_f, :libmpc), Int32, (Ptr{Void}, Ptr{Void}, Int32), z.mpc, x.mpf, ROUNDING_MODE)
    return z
end

function MPCComplex(x::MPFRFloat)
    z = MPCComplex{DEFAULT_PRECISION[1],DEFAULT_PRECISION[end]}()
    ccall((:mpc_set_fr, :libmpc), Int32, (Ptr{Void}, Ptr{Void}, Int32), z.mpc, x.mpfr, ROUNDING_MODE)
    return z
end

function MPCComplex(x::String, base::Int)
    z = MPCComplex{DEFAULT_PRECISION[1],DEFAULT_PRECISION[end]}()
    err = ccall((:mpc_set_str, :libmpc), Int32, (Ptr{Void}, Ptr{Uint8}, Int32, Int32), z.mpc, x, base, ROUNDING_MODE)
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

# TODO: fix the precision support here
convert{N,P}(::Type{MPCComplex{N,P}}, x::Rational) = MPCComplex(x) # to resolve ambiguity
convert{N,P}(::Type{MPCComplex{N,P}}, x::Real) = MPCComplex(x)
convert(::Type{MPCComplex}, x::Real) = MPCComplex(x)
convert{N,P}(::Type{MPCComplex{N,P}}, x::Complex) = MPCComplex(x)
convert(::Type{MPCComplex}, x::Complex) = MPCComplex(x)
convert{N,P}(::Type{MPCComplex{N,P}}, x::ImaginaryUnit) = MPCComplex(x)
convert(::Type{MPCComplex}, x::ImaginaryUnit) = MPCComplex(x)

convert(::Type{Float64}, x::MPCComplex) = ccall((:mpc_get_d,:libmpc), Float64, (Ptr{Void},), x.mpc)
convert(::Type{Float32}, x::MPCComplex) = ccall((:mpc_get_flt,:libmpc), Float32, (Ptr{Void},), x.mpc)
#convert(::Type{FloatingPoint}, x::BigInt) = MPCComplex(x)

promote_rule{T<:Real,N,P}(::Type{MPCComplex{N,P}}, ::Type{T}) = MPCComplex{N,P}
promote_rule{T<:Real}(::Type{MPCComplex}, ::Type{T}) = MPCComplex
promote_rule{T<:Real,N,P}(::Type{MPCComplex{N,P}}, ::Type{Complex{T}}) = MPCComplex{N,P}
promote_rule{T<:Real}(::Type{MPCComplex}, ::Type{Complex{T}}) = MPCComplex
promote_rule{T<:Number,N,P}(::Type{MPCComplex{N,P}}, ::Type{T}) = MPCComplex{N,P}
promote_rule{T<:Number}(::Type{MPCComplex}, ::Type{T}) = MPCComplex
promote_rule{N,P}(::Type{MPCComplex{N,P}}, ::Type{ImaginaryUnit}) = MPCComplex{N,P}
promote_rule(::Type{MPCComplex}, ::Type{ImaginaryUnit}) = MPCComplex

# TODO: Decide if overwriting the default BigFloat rule is good
#promote_rule{T<:FloatingPoint}(::Type{BigInt},::Type{T}) = MPCComplex
#promote_rule{T<:FloatingPoint,N,P}(::Type{BigFloat},::Type{T}) = MPCComplex{DEFAULT_PRECISION[1],DEFAULT_PRECISION[end]}

# Complex constructors
for (fJ, fC) in ((:si,:Int), (:ui,:Uint), (:d,:Float64))
    @eval begin
        function MPCComplex(x::($fC), y::($fC))
            z = MPCComplex{DEFAULT_PRECISION[1], DEFAULT_PRECISION[end]}()
            ccall(($(string(:mpc_set_,fJ,'_',fJ)), :libmpc), Int32, (Ptr{Void}, ($fC), ($fC), Int32), z.mpc, x, y, ROUNDING_MODE)
            return z
        end
    end
end

function MPCComplex(x::BigInt, y::BigInt)
    z = MPCComplex{DEFAULT_PRECISION[1],DEFAULT_PRECISION[end]}()
    ccall((:mpc_set_z_z, :libmpc), Int32, (Ptr{Void}, Ptr{Void}, Ptr{Void}, Int32), z.mpc, &(x.mpz), &(y.mpz), ROUNDING_MODE)
    return z
end

function MPCComplex(x::BigFloat, y::BigFloat)
    z = MPCComplex{DEFAULT_PRECISION[1],DEFAULT_PRECISION[end]}()
    ccall((:mpc_set_f_f, :libmpc), Int32, (Ptr{Void}, Ptr{Void}, Ptr{Void}, Int32), z.mpc, x.mpf, y.mpf, ROUNDING_MODE)
    return z
end

function MPCComplex(x::MPFRFloat, y::MPFRFloat)
    z = MPCComplex{DEFAULT_PRECISION[1],DEFAULT_PRECISION[end]}()
    ccall((:mpc_set_fr_fr, :libmpc), Int32, (Ptr{Void}, Ptr{Void}, Ptr{Void}, Int32), z.mpc, x.mpfr, y.mpfr, ROUNDING_MODE)
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
            z = MPCComplex{DEFAULT_PRECISION[1],DEFAULT_PRECISION[end]}()
            ccall(($(string(:mpc_,fC)),:libmpc), Int32, (Ptr{Void}, Ptr{Void}, Ptr{Void}, Int32), z.mpc, x.mpc, y.mpc, ROUNDING_MODE)
            return z
        end
    end
end

function -(x::MPCComplex)
    z = MPCComplex{DEFAULT_PRECISION[1],DEFAULT_PRECISION[end]}()
    ccall((:mpc_neg, :libmpc), Int32, (Ptr{Void}, Ptr{Void}, Int32), z.mpc, x.mpc, ROUNDING_MODE)
    return z
end

function cmp(x::MPCComplex, y::MPCComplex)
    ccall((:mpc_cmp, :libmpc), Int32, (Ptr{Void}, Ptr{Void}), x.mpc, y.mpc)
end

function sqrt(x::MPCComplex)
    z = MPCComplex{DEFAULT_PRECISION[1],DEFAULT_PRECISION[end]}()
    ccall((:mpc_sqrt, :libmpc), Int32, (Ptr{Void}, Ptr{Void}, Int32), z.mpc, x.mpc, ROUNDING_MODE)
    if isnan(z)
        throw(DomainError())
    end
    return z
end

function ^(x::MPCComplex, y::Uint)
    z = MPCComplex{DEFAULT_PRECISION[1],DEFAULT_PRECISION[end]}()
    ccall((:mpc_pow_ui, :libmpc), Int32, (Ptr{Void}, Ptr{Void}, Uint, Int32), z.mpc, x.mpc, y, ROUNDING_MODE)
    return z
end

function ^(x::MPCComplex, y::Int)
    z = MPCComplex{DEFAULT_PRECISION[1],DEFAULT_PRECISION[end]}()
    ccall((:mpc_pow_si, :libmpc), Int32, (Ptr{Void}, Ptr{Void}, Int, Int32), z.mpc, x.mpc, y, ROUNDING_MODE)
    return z
end

function ^(x::MPCComplex, y::BigInt)
    z = MPCComplex{DEFAULT_PRECISION[1],DEFAULT_PRECISION[end]}()
    ccall((:mpc_pow_z, :libmpc), Int32, (Ptr{Void}, Ptr{Void}, Ptr{Void}, Int32), z.mpc, x.mpc, &(y.mpz), ROUNDING_MODE)
    return z
end

# Utility functions
==(x::MPCComplex, y::MPCComplex) = ccall((:mpc_cmp, :libmpc), Int32, (Ptr{Void}, Ptr{Void}), x.mpc, y.mpc) == 0


# TODO: decide if prec and prec2 should be just one method or not.
function prec(x::MPCComplex)
    return ccall((:mpc_get_prec, :libmpc), Int, (Ptr{Void},), x.mpc)
end

function prec2(x::MPCComplex)
    a = [0]
    b = [0]
    ccall((:mpc_get_prec2, :libmpc), Int, (Ptr{Int}, Ptr{Int}, Ptr{Void}), a, b, x.mpc)
    return (a[1],b[1])
end

get_complex_precision() = (DEFAULT_PRECISION[1],DEFAULT_PRECISION[end])
function set_complex_precision(x::Int, y::Int)
    if x < 2
        throw(DomainError())
    end
    DEFAULT_PRECISION[1], DEFAULT_PRECISION[end] = x, y
end
set_complex_precision(x::(Int,Int)) = set_complex_precision(x...)

iscomplex(::MPCComplex) = true
isfinite(x::MPCComplex) = isfinite(real(x)) && isfinite(imag(x))
isinf(x::MPCComplex) = !isfinite(x)
integer_valued(x::MPCComplex) = imag(x) == 0 && integer_valued(real(x))

function with_complex_precision(f::Function, realprec::Integer, imagprec::Integer)
    old_precision = get_complex_precision()
    set_complex_precision(realprec, imagprec)
    ret = f()
    set_complex_precision(old_precision)
    return ret
end

function imag{N,P}(x::MPCComplex{N,P})
    z = MPFRFloat{N}()
    ccall((:mpc_imag, :libmpc), Int32, (Ptr{Void}, Ptr{Void}, Int32), z.mpfr, x.mpc, ROUNDING_MODE)
    return z
end

function real{N,P}(x::MPCComplex{N,P})
    z = MPFRFloat{N}()
    ccall((:mpc_real, :libmpc), Int32, (Ptr{Void}, Ptr{Void}, Int32), z.mpfr, x.mpc, ROUNDING_MODE)
    return z
end

string(x::MPCComplex) = "$(string(real(x))) + $(string(imag(x)))im"

show(io::IO, b::MPCComplex) = print(io, string(b) * " with $(prec2(b)) bits of precision")
showcompact(io::IO, b::MPCComplex) = print(io, string(b))

end #module
