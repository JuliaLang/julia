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

set_rounding_raw{T<:Union(Float32,Float64)}(::Type{T},i::Integer) = ccall(:fesetround, Cint, (Cint,), i)
get_rounding_raw{T<:Union(Float32,Float64)}(::Type{T}) = ccall(:fegetround, Cint, ())

set_rounding{T<:Union(Float32,Float64)}(::Type{T},r::RoundingMode) = set_rounding_raw(T,to_fenv(r))
get_rounding{T<:Union(Float32,Float64)}(::Type{T}) = from_fenv(get_rounding_raw(T))

function with_rounding{T}(f::Function, ::Type{T}, rounding::RoundingMode)
    old_rounding_raw = get_rounding_raw(T)
    set_rounding(T,rounding)
    try
        return f()
    finally
        set_rounding_raw(T,old_rounding_raw)
    end
end


# Should be equivalent to:
#   with_rounding(Float64,r) do
#       convert(T,x)
#   end
# but explicit checks are currently quicker (~20x).
# Assumes conversion is performed by rounding to nearest value.

# To avoid ambiguous dispatch with methods in mpfr.jl:
call{T<:FloatingPoint}(::Type{T},x::Real,r::RoundingMode) = _convert_rounding(T,x,r)

_convert_rounding{T<:FloatingPoint}(::Type{T},x::Real,r::RoundingMode{:TiesToEven}) = convert(T,x)
function _convert_rounding{T<:FloatingPoint}(::Type{T},x::Real,r::RoundingMode{:TowardNegative})
    y = convert(T,x)
    y > x ? prevfloat(y) : y
end
function _convert_rounding{T<:FloatingPoint}(::Type{T},x::Real,r::RoundingMode{:TowardPositive})
    y = convert(T,x)
    y < x ? nextfloat(y) : y
end
function _convert_rounding{T<:FloatingPoint}(::Type{T},x::Real,r::RoundingMode{:TowardZero})
    y = convert(T,x)
    if x > 0.0
        y > x ? prevfloat(y) : y
    else
        y < x ? nextfloat(y) : y
    end
end

end #module
