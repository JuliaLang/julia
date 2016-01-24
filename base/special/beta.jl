import Base.Math.lgamma_r
import Base.Math.lbeta
import Base.Math.beta
using Base.Test
#=
Port of Chephes' Beta function implementation as found in
scipy libray https://github.com/scipy/scipy/blob/0cff7a5fe6226668729fc2551105692ce114c2b3/scipy/special/cephes/beta.c

* Cephes Math Library Release 2.0:  April, 1987
* Copyright 1984, 1987 by Stephen L. Moshier
* Direct inquiries to 30 Frost Street, Cambridge, MA 02140
=#

const MAXGAM = 171.624376956302725
const ASYMP_FACTOR = 1e6

fastabs(x::Real) = abs(x)
fastabs(x::Complex) = abs(real(x)) + abs(imag(x))

beta(a::Number, b::Number) = beta(promote(float(a), float(b))...)

function beta{T<:Number}(a::T, b::T)
    real(a) <= 0.0 && isinteger(a) && return beta_negint(a, b)
    real(b) <= 0.0 && isinteger(b) && return beta_negint(b, a)
    #
    if fastabs(a) < fastabs(b)
        a, b = b, a
    end
    #
    if fastabs(a) > ASYMP_FACTOR * fastabs(b) && a > ASYMP_FACTOR
        # Avoid loss of precision in lgamma(a + b) - lgamma(a)
        y, s = lbeta_asymp(a, b)
        return s * exp(y)
    end

    y = a + b
    if fastabs(y) > MAXGAM || fastabs(a) > MAXGAM || fastabs(b) > MAXGAM
    	y, s = lgamma_r(y)
    	yb, sb = lgamma_r(b)
        y = yb - y
    	ya, sa = lgamma_r(a)
        y = ya + y
    	# if (y > MAXLOG) {
    	#     goto overflow;
    	# }
    	return s*sa*sb * exp(y)
    end

    y = gamma(y)
    a = gamma(a)
    b = gamma(b)
    y == 0.0 && return convert(T, Inf)

    if fastabs(fastabs(a) - fastabs(y)) > fastabs(fastabs(b) - fastabs(y))
        y = b / y
        y *= a
    else
        y = a / y
        y *= b
    end

    return y
end

lbeta(a::Number, b::Number) = lbeta(promote(float(a), float(b))...)

function lbeta{T<:Number}(a::T, b::T)
    real(a) <= 0.0 && isinteger(a) && return lbeta_negint(a, b)
    real(b) <= 0.0 && isinteger(b) && return lbeta_negint(b, a)

    if fastabs(a) < fastabs(b)
        a, b = b, a
    end

    if fastabs(a) > ASYMP_FACTOR * fastabs(b) && a > ASYMP_FACTOR
        # Avoid loss of precision in lgamma(a + b) - lgamma(a)
        y, s = lbeta_asymp(a, b)
        return y
    end

    y = a + b
    if fastabs(y) > MAXGAM || fastabs(a) > MAXGAM || fastabs(b) > MAXGAM
    	y, s = lgamma_r(y)
    	yb, sb = lgamma_r(b)
        y = yb - y
    	ya, sa = lgamma_r(a)
        y = ya + y
    	return y
    end

    y = gamma(y)
    a = gamma(a)
    b = gamma(b)
    y == 0.0 && return inf(T)

    if fastabs(fastabs(a) - fastabs(y)) > fastabs(fastabs(b) - fastabs(y))
        y = b / y
        y *= a
    else
        y = a / y
        y *= b
    end

    return real(y) < 0 ? log(-y) : log(y)
end

# assuming isinteger(x) and x < 0.
function beta_negint{T}(x::T, w::T)
    if isinteger(w) && 1 - x - w > 0
        s = ifelse(Int(w) % 2 == 0, T(1), T(-1))
        return s * beta(1 - x - w, w)
    else
        return convert(T, Inf)
    end
end

# assuming isinteger(x) and x < 0.
function lbeta_negint{T}(x::T, w::T)
    if isinteger(w) && 1 - x - w > 0
        s = ifelse(Int(w) % 2 == 0 , T(1), T(-1))
        return s
        return s * lbeta(1 - x - w, w)
    else
        return convert(T, Inf)
    end
end

 # Asymptotic expansion for  ln(|B(a, b)|) for a > ASYMP_FACTOR*max(|b|, 1).
function lbeta_asymp{T}(a::T, b::T)
    r, s = lgamma_r(b)
    r -= b * log(a)
    r += b*(1-b)/(2*a);
    r += b*(1-b)*(1-2*b)/(12*a*a)
    r += - b*b*(1-b)*(1-b)/(12*a*a*a)
    return r, T(s)
end

@vectorize_2arg Number beta
@vectorize_2arg Number lbeta
