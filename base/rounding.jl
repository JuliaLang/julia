module Rounding
include("fenv_constants.jl")

export
    RoundingMode, RoundNearest, RoundToZero, RoundUp, RoundDown, RoundFromZero,
    get_rounding, set_rounding, with_rounding

## rounding modes ##
immutable RoundingMode
    code::Int
    RoundingMode(c::Integer) = new(c)
end
const RoundNearest = RoundingMode(0)
const RoundToZero = RoundingMode(1)
const RoundUp = RoundingMode(2)
const RoundDown = RoundingMode(3)
const RoundFromZero = RoundingMode(4)

function to_fenv(r::RoundingMode)
    if r === RoundNearest
        JL_FE_TONEAREST
    elseif r === RoundToZero
        JL_FE_TOWARDZERO
    elseif r === RoundUp
        JL_FE_UPWARD
    elseif r === RoundDown
        JL_FE_DOWNWARD
    elseif r === RoundFromZero
        error("unsupported rounding mode")
    else
        error("invalid rounding mode")
    end
end

function from_fenv(r::Integer)
    if r == JL_FE_TONEAREST
        return RoundNearest
    elseif r == JL_FE_DOWNWARD
        return RoundDown
    elseif r == JL_FE_UPWARD
        return RoundUp
    elseif r == JL_FE_TOWARDZERO
        return RoundToZero
    else
        error("invalid rounding mode code")
    end
end

set_rounding{T<:Union(Float32,Float64)}(::Type{T},r::RoundingMode) = ccall(:fesetround, Cint, (Cint,), to_fenv(r))
get_rounding{T<:Union(Float32,Float64)}(::Type{T}) = from_fenv(ccall(:fegetround, Cint, ()))

function with_rounding{T}(f::Function, ::Type{T}, rounding::RoundingMode)
    old_rounding = get_rounding(T)
    set_rounding(T,rounding)
    try
        return f()
    finally
        set_rounding(T,old_rounding)
    end
end


## floating point exceptions ##
import Base: show, in, convert

export FloatExceptions, FEInexact, FEUnderflow, FEOverflow, FEDivByZero, FEInvalid, FloatExceptionSet, 
FEAll, clear_floatexcept, get_floatexcept, is_floatexcept


abstract FloatExceptions

immutable FEInexact <: FloatExceptions end
immutable FEUnderflow <: FloatExceptions end
immutable FEOverflow <: FloatExceptions end
immutable FEDivByZero <: FloatExceptions end
immutable FEInvalid <: FloatExceptions end


# IEEE 754 requires the ability to check/set/clear multiple exceptions
immutable FloatExceptionSet
    flags::Cint
    FloatExceptionSet(e::Integer) = new(convert(Cint,e))
end    

convert(::Type{FloatExceptionSet},::Type{FEInexact}) = FloatExceptionSet(JL_FE_INEXACT)
convert(::Type{FloatExceptionSet},::Type{FEUnderflow}) = FloatExceptionSet(JL_FE_UNDERFLOW)
convert(::Type{FloatExceptionSet},::Type{FEOverflow}) = FloatExceptionSet(JL_FE_OVERFLOW)
convert(::Type{FloatExceptionSet},::Type{FEDivByZero}) = FloatExceptionSet(JL_FE_DIVBYZERO)
convert(::Type{FloatExceptionSet},::Type{FEInvalid}) = FloatExceptionSet(JL_FE_INVALID)

const FEAll = FloatExceptionSet(JL_FE_INEXACT | JL_FE_UNDERFLOW | JL_FE_OVERFLOW | JL_FE_DIVBYZERO | JL_FE_INVALID)

in(fs1::FloatExceptionSet,fs2::FloatExceptionSet) = fs1.flags & fs2.flags != zero(Cint)
in{E<:FloatExceptions}(::Type{E},fs::FloatExceptionSet) = in(convert(FloatExceptionSet,E), fs)

show(io::IO,fe::FloatExceptionSet) = showcompact(io, filter(x->in(x,fe),subtypes(FloatExceptions)))



# IEEE754 2008 5.7.4 requires the following functions:
# lowerFlags, raiseFlags, testFlags, testSavedFlags (handled by "in"), restoreFlags, saveAllFlags

# lowerFlags
function clear_floatexcept(f::FloatExceptionSet) 
    if ccall(:feclearexcept, Cint, (Cint,), f.flags) != zero(Cint)
        error("Could not clear floating point exception flag")
    end
end
clear_floatexcept{E<:FloatExceptions}(::Type{E}) = clear_floatexcept(convert(FloatExceptionSet,E))
clear_floatexcept() = clear_floatexcept(FEAll)

function get_floatexcept(f::FloatExceptionSet)
    FloatExceptionSet(ccall(:fetestexcept, Cint, (Cint,), f.flags))
end
# saveAllFlags
get_floatexcept() = get_floatexcept(FEAll)

# testFlags 
is_floatexcept(f::FloatExceptionSet) = in(f,get_floatexcept(f))
is_floatexcept{E<:FloatExceptions}(::Type{E}) = is_floatexcept(convert(FloatExceptionSet,E))
is_floatexcept() = is_floatexcept(FEAll)

# TODO: raiseFlags, restoreFlags



end #module
