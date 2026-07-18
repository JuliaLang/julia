# This file is a part of Julia. License is MIT: https://julialang.org/license

module ULPError
    export ulp_error, ulp_error_maximum
    function ulp_error(accurate::AbstractFloat, approximate::AbstractFloat)
        # the ULP error is usually not required to great accuracy, so `Float32` should be precise enough
        zero_return = 0f0
        inf_return = Inf32
        # handle floating-point edge cases
        if !(isfinite(accurate) && isfinite(approximate))
            accur_is_nan = isnan(accurate)
            approx_is_nan = isnan(approximate)
            if accur_is_nan || approx_is_nan
                return if accur_is_nan === approx_is_nan
                    zero_return
                else
                    inf_return
                end
            end
            if isinf(approximate)
                return if isinf(accurate) && (signbit(accurate) == signbit(approximate))
                    zero_return
                else
                    inf_return
                end
            end
        end
        acc = if accurate isa Union{Float16, Float32}
            # widen for better accuracy when doing so does not impact performance too much
            widen(accurate)
        else
            accurate
        end
        abs(Float32((approximate - acc) / eps(approximate))::Float32)
    end
    function ulp_error(accurate, approximate, x::AbstractFloat)
        acc = accurate(x)
        app = approximate(x)
        ulp_error(acc, app)
    end
    function ulp_error(func::Func, x::AbstractFloat) where {Func}
        ulp_error(func ∘ BigFloat, func, x)
    end
    function ulp_error_maximum(func::Func, iterator) where {Func}
        maximum(Base.Fix1(ulp_error, func), iterator)
    end
end
