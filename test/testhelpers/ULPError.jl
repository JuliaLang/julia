module ULPError
    export ulp_error, ulp_error_maximum
    @noinline function throw_invalid()
        throw(ArgumentError("invalid"))
    end
    function ulp_error(accurate::AbstractFloat, approximate::AbstractFloat)
        if isnan(accurate)
            @noinline throw_invalid()
        end
        if isnan(approximate)
            @noinline throw_invalid()
        end
        # the ULP error is usually not required to great accuracy, so `Float32` should be precise enough
        zero_return = Float32(0)
        inf_return = Float32(Inf)
        if isinf(accurate) || iszero(accurate)  # handle floating-point edge cases
            if isinf(accurate)
                if isinf(approximate) && (signbit(accurate) == signbit(approximate))
                    return zero_return
                end
                return inf_return
            end
            # `iszero(accurate)`
            if iszero(approximate)
                return zero_return
            end
            return inf_return
        end
        # assuming `precision(BigFloat)` is great enough
        acc = if accurate isa BigFloat
            accurate
        else
            BigFloat(accurate)::BigFloat
        end
        err = abs(Float32((approximate - acc) / eps(approximate))::Float32)
        if isnan(err)
            @noinline throw_invalid()  # unexpected
        end
        err
    end
    function ulp_error(accurate::Acc, approximate::App, x::AbstractFloat) where {Acc, App}
        acc = accurate(x)
        app = approximate(x)
        ulp_error(acc, app)
    end
    function ulp_error(func::Func, x::AbstractFloat) where {Func}
        ulp_error(func ∘ BigFloat, func, x)
    end
    function ulp_error_maximum(func::Func, iterator) where {Func}
        function f(x::AbstractFloat)
            ulp_error(func, x)
        end
        maximum(f, iterator)
    end
end
