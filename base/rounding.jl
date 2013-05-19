module Rounding

export
    RoundingMode, RoundToNearest, RoundToZero, RoundUp, RoundDown,
    get_rounding, set_rounding, with_rounding

## rounding modes ##
abstract RoundingMode
type RoundToNearest <: RoundingMode end
type RoundToZero <: RoundingMode end
type RoundUp <: RoundingMode end
type RoundDown <: RoundingMode end

@unix_only set_rounding(::Type{RoundToNearest}) = ccall(:fesetround, Cint, (Cint, ), 0)
@unix_only set_rounding(::Type{RoundToZero}) = ccall(:fesetround, Cint, (Cint, ), 3072)
@unix_only set_rounding(::Type{RoundUp}) = ccall(:fesetround, Cint, (Cint, ), 2048)
@unix_only set_rounding(::Type{RoundDown}) = ccall(:fesetround, Cint, (Cint, ), 1024)

@unix_only function get_rounding()
    r = ccall(:fegetround, Cint, ())
    if r == 0
        return RoundToNearest
    elseif r == 1024
        return RoundDown
    elseif r == 2048
        return RoundUp
    elseif r == 3072
        return RoundToZero
    else
        error()
    end
end

@unix_only function with_rounding{T<:RoundingMode}(f::Function, rounding::Type{T})
    old_rounding = get_rounding()
    set_rounding(rounding)
    try
        return f()
    finally
        set_rounding(old_rounding)
    end
end

end #module