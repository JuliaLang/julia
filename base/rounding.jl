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

end #module
