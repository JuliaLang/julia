module Integrate
    # simple integration routines mostly for tabulated data
    export trapz, simps

    function trapz(y)
        trapz(y, one(eltype(y)))
    end

    # integration for uniform points (in x)
    function trapz(y, dx::Number)
        r = zero(zero(eltype(y)) * zero(dx))
        r += (y[1] + y[end])/2
        r += sum(y[2:end-1])
        r * dx
    end

    # integration for non-uniform points (in x)
    function trapz(y,x)
        n = length(y)
        if n != length(x)
            error("Input x,y must be of same length")
        end
        r = zero(zero(eltype(x))*zero(eltype(y)))
        for i in 2:n
            r += (x[i] - x[i-1]) * (y[i] + y[i-1])
        end
        r/2.0
    end

    function simps(y)
        simps(y, one(eltype(y)))
    end

    function simps(y, dx::Number)
        n = length(y)
        if iseven(n)
            error("Simpson rule requires ODD length input (EVEN number of intervals)")
        end
        r = zero(zero(eltype(y))*zero(dx))
        for i in 2:2:n-1
            r += y[i]
        end
        r *= 2.0
        for i in 3:2:n-2
            r += y[i]
        end
        r *= 2.0
        for i in (1,n)
            r += y[i]
        end
        r * dx / 3.0
    end

    function simps(y,x)
        n = length(y)
        if n != length(x)
            error("Input x,y must be of same length")
        end
        if iseven(n)
            error("Simpson rule requires ODD length input (EVEN number of intervals)")
        end

        # this is a quick generalization of the simpson's rule for
        # arbitrary separations in x
        r = zero(zero(eltype(x))*zero(eltype(y)))
        for i in 3:2:n
            d1 = x[i-1] - x[i-2]
            d2 = x[i] - x[i-1]
            h = x[i] - x[i-2]
            r += h * (
                    y[i-2] * (2 - d2 / d1) +
                    y[i-1] * h * h / (d1 * d2) +
                    y[i] * (2 - d1 / d2)
                )
        end
        r / 6.0
    end

end
