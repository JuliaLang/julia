
# 0.7 deprecations

begin
    approx_full(x::AbstractArray) = x
    approx_full(x::Number) = x
    approx_full(x) = full(x)

    function test_approx_eq(va, vb, Eps, astr, bstr)
        va = approx_full(va)
        vb = approx_full(vb)
        la, lb = length(linearindices(va)), length(linearindices(vb))
        if la != lb
            error("lengths of ", astr, " and ", bstr, " do not match: ",
                "\n  ", astr, " (length $la) = ", va,
                "\n  ", bstr, " (length $lb) = ", vb)
        end
        diff = real(zero(eltype(va)))
        for (xa, xb) = zip(va, vb)
            if isfinite(xa) && isfinite(xb)
                diff = max(diff, abs(xa-xb))
            elseif !isequal(xa,xb)
                error("mismatch of non-finite elements: ",
                    "\n  ", astr, " = ", va,
                    "\n  ", bstr, " = ", vb)
            end
        end

        if !isnan(Eps) && !(diff <= Eps)
            sdiff = string("|", astr, " - ", bstr, "| <= ", Eps)
            error("assertion failed: ", sdiff,
                "\n  ", astr, " = ", va,
                "\n  ", bstr, " = ", vb,
                "\n  difference = ", diff, " > ", Eps)
        end
    end

    array_eps(a::AbstractArray{Complex{T}}) where {T} = eps(float(maximum(x->(isfinite(x) ? abs(x) : T(NaN)), a)))
    array_eps(a) = eps(float(maximum(x->(isfinite(x) ? abs(x) : oftype(x,NaN)), a)))

    test_approx_eq(va, vb, astr, bstr) =
        test_approx_eq(va, vb, 1E4*length(linearindices(va))*max(array_eps(va), array_eps(vb)), astr, bstr)

    """
        @test_approx_eq_eps(a, b, tol)

    Test two floating point numbers `a` and `b` for equality taking into account
    a margin of tolerance given by `tol`.
    """
    macro test_approx_eq_eps(a, b, c)
        Base.depwarn(string("@test_approx_eq_eps is deprecated, use `@test ", a, " ≈ ", b, " atol=", c, "` instead"),
                    Symbol("@test_approx_eq_eps"))
        :(test_approx_eq($(esc(a)), $(esc(b)), $(esc(c)), $(string(a)), $(string(b))))
    end
    export @test_approx_eq_eps

    """
        @test_approx_eq(a, b)

    Deprecated. Test two floating point numbers `a` and `b` for equality taking into
    account small numerical errors.
    """
    macro test_approx_eq(a, b)
        Base.depwarn(string("@test_approx_eq is deprecated, use `@test ", a, " ≈ ", b, "` instead"),
                    Symbol("@test_approx_eq"))
        :(test_approx_eq($(esc(a)), $(esc(b)), $(string(a)), $(string(b))))
    end
    export @test_approx_eq
end

