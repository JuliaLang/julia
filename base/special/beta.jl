"""
    lbeta(α::Float64, β::Float64, x::Float64)
    lbeta(α::Float64, β::Float64, x₁::Float64, x₂::Float64)

Logarithm of the incomplete Beta function,
    log B(α, β, x)
    log B(α, β, x₁, x₂) = log(B(α, β, x₂) - B(α, β, x₁))
"""
function lbeta end

function lbeta(α::Float64, β::Float64, x::Float64)
    @assert α > 0 && β > 0 && 0 ≤ x ≤ 1
    if x == 0.0
        return 0.0
    elseif x == 1.0
        return lbeta(α, β)
    elseif x ≤ (α + 1) / (α + β + 2)
        return α * log(x) + β * log(1 - x) - log(α) + lbeta_cf(α, β, x)
    else
        return log_sub(lbeta(α, β), lbeta(β, α, 1 - x))
    end
end

function lbeta(α::Float64, β::Float64, x₁::Float64, x₂::Float64)
    @assert α > 0 && β > 0 && 0 ≤ x₁ ≤ x₂ ≤ 1
    if x₁ == 0
        return lbeta(α, β, x₂)
    elseif x₂ == 1
        return lbeta(β, α, 1 - x₁)
    elseif α ≤ 1 || β ≤ 1
        return log(beta_reg(α, β, x₁, x₂)) + lbeta(α, β)
    end

    mode = (α - 1) / (α + β - 2)

    if x₂ ≤ mode
        return log_sub(lbeta(α, β, x₂), lbeta(α, β, x₁))
    elseif mode ≤ x₁
        return log_sub(lbeta(β, α, 1 - x₁), lbeta(β, α, 1 - x₂))
    else
        return log_sub(lbeta(α, β), log_add(lbeta(α, β, x₁), lbeta(β, α, 1 - x₂)))
    end
end


"""
    betar(α::Float64, β::Float64, x::Float64)
    betar(α::Float64, β::Float64, x₁::Float64, x₂::Float64)

Regularized incomplete Beta function,
    Br(α, β, x) = B(α, β, x) / B(α, β)
    Br(α, β, x₂, x₁) = (B(α, β, x₂) - B(α, β, x₁)) / B(α, β) = Br(α, β, x₂) - Br(α, β, x₁)
"""
function betar end

betar(α::Float64, β::Float64, x::Float64) = exp(lbeta(α, β, x) - lbeta(α, β))

function betar(α::Float64, β::Float64, x₁::Float64, x₂::Float64)
    @assert α > 0 && β > 0 && 0 ≤ x₁ ≤ x₂ ≤ 1
    if x₁ == 0
        return betar(α, β, x₂)
    elseif x₂ == 1
        return betar(β, α, 1 - x₁)
    elseif α ≤ 1 || β ≤ 1
        return betar(α, β, x₂) - betar(α, β, x₁)
    end

    mode = (α - 1) / (α + β - 2)

    if x₂ ≤ mode
        return betar(α, β, x₂) - betar(α, β, x₁)
    elseif x₁ ≥ mode
        return betar(β, α, 1 - x₁) - betar(β, α, 1 - x₂)
    else
        return 1 - betar(α, β, x₁) - betar(β, α, 1 - x₂)
    end
end


"""
    log1exp(x::Float64)

Stable computation of log(1 - exp(-x)), for x ≥ 0.
"""
function log1exp(x::Float64)
    @assert x ≥ 0
    if x > log(2); log(-expm1(-x)) else log1p(-exp(-x)) end
end


"""
    log_sub(logx::Float64, logy::Float64)

Stable computation of log(|x-y|) from log(x) and log(y)
"""
function log_sub(logx::Float64, logy::Float64)
    logmin, logmax = minmax(logx, logy)
    if logmin > -Inf
        return logmax + log1exp(logmax - logmin)
    else
        return logmax
    end
end

"""
    log_add(logx::Float64, logy::Float64)

Stable computation of log(x + y) from log(x) and log(y)
"""
function log_add(logx::Float64, logy::Float64)
    logmin, logmax = minmax(logx, logy)
    if -Inf < logmin && logmax < Inf
        return logmax + log1p(exp(logmin - logmax))
    else
        return logmax
    end
end


"""
    lbeta_cf(α::Float64, β::Float64, x::Float64)

Computes the logarithm of the incomplete Beta function, by a continued fraction expansion.
See Eq. 6.4.5 of Numerical Recipes 3rd Edition.
Assumes that x ≤ (α + 1) / (α + β + 2)

This is an internal function, you are not supposed to call it.
"""
function lbeta_cf(α::Float64, β::Float64, x::Float64)
    @assert α > 0 && β > 0 && 0 ≤ x ≤ 1
    @assert x ≤ (α + 1) / (α + β + 2)

    # Evaluates the continued fraction for the incomplete Beta function by the modified Lentz's method

    const MAXIT::Int = 10^3
    const ϵ::Float64 = 1e-10
    const fpmin::Float64 = 1e-30

    # some constant factors
    const qab::Float64 = α + β
    const qap::Float64 = α + 1.0
    const qam::Float64 = α - 1.0

    # first iteration of Lentz's method
    C::Float64 = 1.0
    D::Float64 = 1.0 - qab * x / qap
    if abs(D) < fpmin; D = fpmin end
    D = 1.0 / D
    @assert D > 0

    # function estimate
    log_cf::Float64 = log(D)

    for m = 1:MAXIT
        m2::Int = 2m

        # one step (the even one) of the recurrence. The index here is 2m
        aa::Float64 = m * (β - m) * x / ((qam + m2) * (α + m2))

        D = 1.0 + aa * D
        if abs(D) < fpmin; D = fpmin end
        D = 1.0 / D

        C = 1.0 + aa / C
        if abs(C) < fpmin; C = fpmin end

        log_cf += log(D) + log(C)

        # next step (the odd one) of the recurrence. The index here is 2m + 1
        aa = -(α + m) * (qab + m) * x / ((α + m2) * (qap + m2))

        D = 1.0 + aa * D
        if abs(D) < fpmin; D = fpmin end
        D = 1.0 / D

        C = 1.0 + aa / C
        if abs(C) < fpmin; C = fpmin end

        log_del::Float64 = log(D) + log(C)
        log_cf += log_del

        if abs(log_del) < ϵ; return log_cf end
    end

    error("beta continued fraction did not converge after $MAXIT iterations")
end

# tests
@assert lbeta(1.0, 2.0, 0.2, 0.9) ≈ -1.15518264015650398955737181354
@assert lbeta(3.0, 2.0, 0.01, 0.99) ≈ -2.48550282746660144854598013619
@assert lbeta(3.0, 2.0, 0.001, 0.01) ≈ -14.9226584212253042301433768824
@assert lbeta(3.0, 2.0, 0.99, 0.999) ≈ -9.92703257898043949664632355807
@assert lbeta(3.0, 2.0, 0.2, 0.9) ≈ -2.56774492809700481550963695770
@assert lbeta(3.0, 2.0, 0.1, 0.2) ≈ -6.23566150762002408487996529558
@assert lbeta(3.0, 2.0, 0.9, 0.95) ≈ -5.74770170859103654995762029784
