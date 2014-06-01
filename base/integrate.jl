module Integrate
    export trapz, simps
    # simple integration routines, mostly for tabulated vector data
    # There are notable interface differences with quadgk()
    #  * these do does not return an error term
    #  * these only return scalar values
    
    # trapezoidal function integration, probably not the most accurate 
    # method for functions, but this method is included to aid testing 
    # and provide comparisons with the vector versions
    function trapz(f::Function, a, b, n=1)
        h = (b-a) / n
        r = f(a) + f(b)
        for i in 1:n-1
            r += 2*f(i*h + a)
        end
        r * h / 2
    end

    # vector integration for uniform points (in x)
    function trapz{T<:Number}(y::Vector{T}, dx=one(T))
        r = zero(zero(T)*zero(T))
        for i in 2:length(y)
            r += y[i] + y[i-1]
        end
        r * dx /2.0
    end

    # vector integration for non-uniform points (in x)
    function trapz{T<:Number}(y::Vector{T}, x::Vector{T})
        n = length(y)
        if n != length(x)
            error("Vectors must be of same length")
        end
        r = zero(zero(T)*zero(T))
        for i in 2:n
            r += (x[i] - x[i-1]) * (y[i] + y[i-1])
        end
        r/2.0
    end

    # some simpson rules for more simple integration
    function simps(f::Function, a, b, n=2)
        h = (b-a) / n
        r = f(a) + f(b)
        for i in 2:2:n-1
            r += 2*f(i*h + a)
        end
        for i in 1:2:n
            r += 4*f(i*h + a)
        end
        r * h / 3
    end

    function simps{T<:Number}(y::Vector{T}, dx=one(T))
        n = length(y) 
        if iseven(n) 
            error("Simpson rule requires ODD length input (EVEN number of intervals)")
        end
        r = zero(zero(T)*zero(T))
        for i in 3:2:n
            r += (y[i-2] + 4*y[i-1] + y[i])
        end
        r * dx / 3.0
    end

    function simps{T<:Number}(y::Vector{T}, x::Vector{T})
        n = length(y)
        if n != length(x)
            error("Vectors must be of same length")
        end
        if iseven(n) 
            error("Simpson rule requires ODD length input (EVEN number of intervals)")
        end

        # this is a quick generalization of the simpson's rule for 
        # arbitrary separations in x
        r = zero(zero(T)*zero(T))
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
