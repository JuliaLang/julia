##### Skewness and Kurtosis

# Skewness
# This is Type 1 definition according to Joanes and Gill (1998)
"""
    skewness(x; [weights::AbstractArray], [mean::Real])

Compute the standardized skewness of collection `x`, optionally
specifying a pre-computed `mean`.
If `x` is an `AbstractArray`, a `weights` array of the same length as `x`
can be specified to compute the weighted skewness.

!!! compat "Julia 1.3"
    This function requires at least Julia 1.3.
"""
skewness(A; mean::Union{Real, Nothing}=nothing) = _skewness(A, nothing, mean)

skewness(A::AbstractArray;
         weights::Union{AbstractArray,Nothing}=nothing,
         mean::Union{Real, Nothing}=nothing) =
    _skewness(A, weights, mean)

function _skewness(x, w::Nothing, m::Real)
    y = iterate(x)
    if y === nothing
        T = eltype(x)
        # Return the NaN of the type that we would get, had this collection
        # contained any elements (this is consistent with var)
        z0 = zero(T) - zero(m)
        return (z0^3 + z0^3)/sqrt((z0^2+z0^2)^3)
    end

    v, s = y
    z = v - m
    cm2 = z * z   # empirical 2nd centered moment (variance)
    cm3 = cm2 * z # empirical 3rd centered moment
    n = 1
    y = iterate(x, s)
    while y !== nothing
        v, s = y
        n += 1

        z = v - m
        z2 = z * z
        cm2 += z2
        cm3 += z2 * z
        y = iterate(x, s)
    end
    cm3 /= n
    cm2 /= n
    return cm3 / sqrt(cm2^3)
end

function _skewness(x::AbstractArray{T}, w::AbstractArray{W}, m::Real) where {T, W}
    length(x) == length(w) ||
        throw(ArgumentError("data and weight vectors must be the same size," *
                            "got $(length(v)) and $(length(w))"))
    z0 = zero(T) - zero(m)
    cm2 = z0 * zero(W) + z0 * zero(W) # empirical 2nd centered moment (variance)
    cm3 = cm2                         # empirical 3rd centered moment

    @inbounds @simd for i in eachindex(x, w)
        z = x[i] - m
        z2w = z * z * w[i]
        cm2 += z2w
        cm3 += z2w * z
    end
    sw = sum(w)
    cm3 /= sw
    cm2 /= sw
    return cm3 / sqrt(cm2^3)
end

_skewness(A::AbstractArray, w::Union{AbstractArray, Nothing}, m::Nothing) =
    _skewness(A, w, mean(A, weights=w))

# (excessive) Kurtosis
# This is Type 1 definition according to Joanes and Gill (1998)
"""
    kurtosis(x; [weights::AbstractArray], [mean::Real])

Compute the excess kurtosis of collection `x`, optionally
specifying a pre-computed `mean`.
If `x` is an `AbstractArray`, a `weights` array of the same length as `x`
can be specified to compute the weighted kurtosis.

!!! compat "Julia 1.3"
    This function requires at least Julia 1.3.
"""
kurtosis(A; mean::Union{Real, Nothing}=nothing) = _kurtosis(A, nothing, mean)

kurtosis(A::AbstractArray;
         weights::Union{AbstractArray,Nothing}=nothing,
         mean::Union{Real, Nothing}=nothing) =
    _kurtosis(A, weights, mean)

function _kurtosis(x, w::Nothing, m::Real)
    y = iterate(x)
    if y === nothing
        T = eltype(x)
        # Return the NaN of the type that we would get, had this collection
        # contained any elements (this is consistent with var)
        z0 = zero(T) - zero(m)
        return (z0^3 + z0^3)/sqrt((z0^2+z0^2)^3)
    end

    v, s = y
    z = v - m
    cm2 = z * z     # empirical 2nd centered moment (variance)
    cm4 = cm2 * cm2 # empirical 4th centered moment

    n = 1
    y = iterate(x, s)
    while y !== nothing
        v, s = y
        n += 1

        z = v - m
        z2 = z * z
        cm2 += z2
        cm4 += z2 * z2
        y = iterate(x, s)
    end
    cm4 /= n
    cm2 /= n
    return (cm4 / (cm2 * cm2)) - 3
end

function _kurtosis(x::AbstractArray{T}, w::AbstractWeights{W}, m::Real) where {T, W}
    length(x) == length(w) ||
        throw(ArgumentError("data and weight vectors must be the same size," *
                            "got $(length(v)) and $(length(w))"))
    z0 = zero(T) - zero(m)
    cm2 = z0 * zero(W) + z0 * zero(W) # empirical 2nd centered moment (variance)
    cm4 = cm2                         # empirical 4rd centered moment

    @inbounds @simd for i in eachindex(x, w)
        z = x[i] - m
        z2 = z * z
        z2w = z2 * w[i]
        cm2 += z2w
        cm4 += z2w * z2
    end
    sw = sum(w)
    cm4 /= sw
    cm2 /= sw
    return (cm4 / (cm2 * cm2)) - 3
end

_kurtosis(A::AbstractArray, w::Union{AbstractWeights, Nothing}, m::Nothing) =
    _kurtosis(A, w, mean(A, weights=w))
