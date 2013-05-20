module Rounding
include("fenv_constants.jl")

export
    RoundingMode, RoundToNearest, RoundToZero, RoundUp, RoundDown,
    get_rounding, set_rounding, with_rounding

## rounding modes ##
abstract RoundingMode
type RoundToNearest <: RoundingMode end
type RoundToZero <: RoundingMode end
type RoundUp <: RoundingMode end
type RoundDown <: RoundingMode end

set_rounding(::Type{RoundToNearest}) = ccall(:fesetround, Cint, (Cint, ), JL_FE_TONEAREST)
set_rounding(::Type{RoundToZero}) = ccall(:fesetround, Cint, (Cint, ), JL_FE_TOWARDZERO)
set_rounding(::Type{RoundUp}) = ccall(:fesetround, Cint, (Cint, ), JL_FE_UPWARD)
set_rounding(::Type{RoundDown}) = ccall(:fesetround, Cint, (Cint, ), JL_FE_DOWNWARD)

function get_rounding()
    r = ccall(:fegetround, Cint, ())
    if r == JL_FE_TONEAREST
        return RoundToNearest
    elseif r == JL_FE_DOWNWARD
        return RoundDown
    elseif r == JL_FE_UPWARD
        return RoundUp
    elseif r == JL_FE_TOWARDZERO
        return RoundToZero
    else
        error()
    end
end

function with_rounding{T<:RoundingMode}(f::Function, rounding::Type{T})
    old_rounding = get_rounding()
    set_rounding(rounding)
    try
        return f()
    finally
        set_rounding(old_rounding)
    end
end

end #module