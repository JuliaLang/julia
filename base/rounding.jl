# This file is a part of Julia. License is MIT: http://julialang.org/license

module Rounding
include(String(vcat(length(Core.ARGS)>=2?Core.ARGS[2].data:"".data, "fenv_constants.jl".data))) # include($BUILDROOT/base/fenv_constants.jl)

export
    RoundingMode, RoundNearest, RoundToZero, RoundUp, RoundDown, RoundFromZero,
    RoundNearestTiesAway, RoundNearestTiesUp,
    rounding, setrounding,
    get_zero_subnormals, set_zero_subnormals

## rounding modes ##
immutable RoundingMode{T} end

const RoundNearest = RoundingMode{:Nearest}()
const RoundToZero = RoundingMode{:ToZero}()
const RoundUp = RoundingMode{:Up}()
const RoundDown = RoundingMode{:Down}()
const RoundFromZero = RoundingMode{:FromZero}() # mpfr only
# C-style round behaviour
const RoundNearestTiesAway = RoundingMode{:NearestTiesAway}()
# Java-style round behaviour
const RoundNearestTiesUp = RoundingMode{:NearestTiesUp}()

to_fenv(::RoundingMode{:Nearest}) = JL_FE_TONEAREST
to_fenv(::RoundingMode{:ToZero}) = JL_FE_TOWARDZERO
to_fenv(::RoundingMode{:Up}) = JL_FE_UPWARD
to_fenv(::RoundingMode{:Down}) = JL_FE_DOWNWARD

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
        throw(ArgumentError("invalid rounding mode code: $r"))
    end
end

setrounding_raw{T<:Union{Float32,Float64}}(::Type{T},i::Integer) = ccall(:fesetround, Int32, (Int32,), i)
rounding_raw{T<:Union{Float32,Float64}}(::Type{T}) = ccall(:fegetround, Int32, ())

setrounding{T<:Union{Float32,Float64}}(::Type{T},r::RoundingMode) = setrounding_raw(T,to_fenv(r))
rounding{T<:Union{Float32,Float64}}(::Type{T}) = from_fenv(rounding_raw(T))


function setrounding{T}(f::Function, ::Type{T}, rounding::RoundingMode)
    old_rounding_raw = rounding_raw(T)
    setrounding(T,rounding)
    try
        return f()
    finally
        setrounding_raw(T,old_rounding_raw)
    end
end


# Should be equivalent to:
#   setrounding(Float64,r) do
#       convert(T,x)
#   end
# but explicit checks are currently quicker (~20x).
# Assumes conversion is performed by rounding to nearest value.

# To avoid ambiguous dispatch with methods in mpfr.jl:
call{T<:AbstractFloat}(::Type{T},x::Real,r::RoundingMode) = _convert_rounding(T,x,r)

_convert_rounding{T<:AbstractFloat}(::Type{T},x::Real,r::RoundingMode{:Nearest}) = convert(T,x)
function _convert_rounding{T<:AbstractFloat}(::Type{T},x::Real,r::RoundingMode{:Down})
    y = convert(T,x)
    y > x ? prevfloat(y) : y
end
function _convert_rounding{T<:AbstractFloat}(::Type{T},x::Real,r::RoundingMode{:Up})
    y = convert(T,x)
    y < x ? nextfloat(y) : y
end
function _convert_rounding{T<:AbstractFloat}(::Type{T},x::Real,r::RoundingMode{:ToZero})
    y = convert(T,x)
    if x > 0.0
        y > x ? prevfloat(y) : y
    else
        y < x ? nextfloat(y) : y
    end
end

set_zero_subnormals(yes::Bool) = ccall(:jl_set_zero_subnormals,Int32,(Int8,),yes)==0
get_zero_subnormals() = ccall(:jl_get_zero_subnormals,Int32,())!=0

end #module
