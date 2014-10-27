module Rounding
include("fenv_constants.jl")

export
    RoundingMode, RoundNearest, RoundToZero, RoundUp, RoundDown, RoundFromZero,
    get_rounding, set_rounding, with_rounding

## rounding modes ##
immutable RoundingMode{T} end

const RoundNearest = RoundingMode{:TiesToEven}()
# const RoundNearestTiesAway = RoundingMode{:TiesToAway}() # currently unsupported
const RoundToZero = RoundingMode{:TowardZero}()
const RoundUp = RoundingMode{:TowardPositive}()
const RoundDown = RoundingMode{:TowardNegative}()
const RoundFromZero = RoundingMode{:AwayFromZero}() # mpfr only

to_fenv(::RoundingMode{:TiesToEven}) = JL_FE_TONEAREST
to_fenv(::RoundingMode{:TowardZero}) = JL_FE_TOWARDZERO
to_fenv(::RoundingMode{:TowardPositive}) = JL_FE_UPWARD
to_fenv(::RoundingMode{:TowardNegative}) = JL_FE_DOWNWARD

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
