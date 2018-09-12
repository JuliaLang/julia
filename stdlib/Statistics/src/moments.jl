##### Weighted var & std

## var

## var along dim

function varm!(R::AbstractArray, A::AbstractArray, M::AbstractArray, w::AbstractWeights;
               corrected::Bool=true)
    rmul!(centralize_sumabs2!(R, A, M, values(w)),
          varcorrection(w, corrected))
end

function var!(R::AbstractArray, A::AbstractArray, w::AbstractWeights, dims::Int;
              mean=nothing, corrected::Bool=true)
    if mean == 0
        varm!(R, A, w, Base.reducedim_initarray(A, dims, 0, eltype(R)), dims;
                   corrected=corrected)
    elseif mean == nothing
        varm!(R, A, w, Statistics.mean(A, w, dims=dims), dims; corrected=corrected)
    else
        # check size of mean
        for i = 1:ndims(A)
            dA = size(A,i)
            dM = size(mean,i)
            if i == dims
                dM == 1 || throw(DimensionMismatch("Incorrect size of mean."))
            else
                dM == dA || throw(DimensionMismatch("Incorrect size of mean."))
            end
        end
        varm!(R, A, w, mean, dims; corrected=corrected)
    end
end

function var(A::AbstractArray, w::AbstractWeights, dim::Int; mean=nothing,
                  corrected::Bool=true)
    corrected = depcheck(:var, corrected)
    var!(similar(A, Float64, Base.reduced_indices(axes(A), dim)), A, w, dim;
         mean=mean, corrected=corrected)
end

##### Fused statistics
"""
    mean_and_var(x; [weights::AbstractWeights,] [dims,] corrected=false) -> (mean, var)

Return the mean and variance of a real-valued array `x`, optionally over dimensions
`dims`, as a tuple. Observations in `x` can be weighted using `weights`.
Finally, bias correction is be applied to the variance calculation if `corrected=true`.
See [`var`](@ref) documentation for more details.
"""
function mean_and_var(A::AbstractArray;
                      weights::Union{AbstractWeights, Nothing}=nothing,
                      dims=:,
                      corrected::Bool=true)
    m = mean(A, weights=weights, dims=dims)
    v = var(A, mean=m, weights=weights, dims=dims, corrected=corrected)
    m, v
end

"""
    mean_and_std(x; [weights::AbstractWeights], [dims,] corrected=true) -> (mean, std)

Return the mean and standard deviation of a real-valued array `x`,
optionally over dimensions `dims`, as a tuple. Observations in `x`
can be weighted using `weights`. Finally, bias correction is be applied to the
standard deviation calculation if `corrected=true`.
See [`std`](@ref) documentation for more details.
"""
function mean_and_std(A::AbstractArray;
                      weights::Union{AbstractWeights, Nothing}=nothing,
                      dims=:,
                      corrected::Bool=true)
    m = mean(A, weights=weights, dims=dims)
    s = std(A, mean=m, weights=weights, dims=dims, corrected=corrected)
    m, s
end



##### General central moment
function _moment2(A::AbstractArray, m::Real; corrected=false)
    s = 0.0
    for i in eachindex(A)
        @inbounds z = A[i] - m
        s += z * z
    end
    varcorrection(length(A), corrected) * s
end

function _moment2(A::AbstractArray, w::AbstractArray, m::Real; corrected=false)
    s = 0.0
    for i in eachindex(A, w)
        @inbounds z = A[i] - m
        @inbounds s += (z * z) * w[i]
    end

    varcorrection(w, corrected) * s
end

function _moment3(A::AbstractArray, m::Real)
    s = 0.0
    for i in eachindex(A)
        @inbounds z = A[i] - m
        s += z * z * z
    end
    s / length(A)
end

function _moment3(A::AbstractArray, w::AbstractArray, m::Real)
    s = 0.0
    for i in eachindex(A, w)
        @inbounds z = A[i] - m
        @inbounds s += (z * z * z) * w[i]
    end
    s / sum(w)
end

function _moment4(A::AbstractArray, m::Real)
    s = 0.0
    for i in eachindex(A)
        @inbounds z = A[i] - m
        s += abs2(z * z)
    end
    s / length(A)
end

function _moment4(A::AbstractArray, w::AbstractArray, m::Real)
    s = 0.0
    for i in eachindex(A, w)
        @inbounds z = A[i] - m
        @inbounds s += abs2(z * z) * w[i]
    end
    s / sum(w)
end

function _momentk(A::AbstractArray, k::Int, m::Real)
    s = 0.0
    for i = eachindex(A)
        @inbounds z = A[i] - m
        s += (z ^ k)
    end
    s / length(A)
end

function _momentk(A::AbstractArray, k::Int, w::AbstractArray, m::Real)
    s = 0.0
    for i in eachindex(A, w)
        @inbounds z = A[i] - m
        @inbounds s += (z ^ k) * w[i]
    end
    s / sum(w)
end


"""
    moment(A, k; [weights::AbstractArray], [mean::Real])

Return the `k`th order central moment of a real-valued array `A`, optionally
specifying `weights` and `mean`.

!!! compat "Julia 1.2"
    This function requires at least Julia 1.2.
"""
moment(A::AbstractArray, k::Int;
       weights::Union{AbstractWeights,Nothing}=nothing,
       mean::Union{Real, Nothing}=nothing) =
    _moment(A, k, weights, mean)

function _moment(A::AbstractArray, k::Int, w::Nothing, m::Real)
    k == 2 ? _moment2(A, m) :
    k == 3 ? _moment3(A, m) :
    k == 4 ? _moment4(A, m) :
    _momentk(A, k, m)
end
_moment(A::AbstractArray, k::Int, w::Nothing, m::Nothing) =
    _moment(A, k, nothing, mean(A))

function _moment(A::AbstractArray, k::Int, w::AbstractArray, m::Real)
    k == 2 ? _moment2(A, w, m) :
    k == 3 ? _moment3(A, w, m) :
    k == 4 ? _moment4(A, w, m) :
    _momentk(A, k, w, m)
end
_moment(A::AbstractArray, k::Int, w::AbstractArray, m::Nothing) =
    _moment(A, k, w, mean(A, weights=w))


##### Skewness and Kurtosis

# Skewness
# This is Type 1 definition according to Joanes and Gill (1998)
"""
    skewness(A; [weights::AbstractArray], [mean::Real])

Compute the standardized skewness of a real-valued array `A`, optionally
specifying `weights` and `mean`.

!!! compat "Julia 1.2"
    This function requires at least Julia 1.2.
"""
skewness(A::AbstractArray;
         weights::Union{AbstractWeights,Nothing}=nothing,
         mean::Union{Real, Nothing}=nothing) =
    _skewness(A, weights, mean)

function _skewness(A::AbstractArray, w::Nothing, m::Real)
    n = length(A)
    cm2 = 0.0   # empirical 2nd centered moment (variance)
    cm3 = 0.0   # empirical 3rd centered moment
    for i in eachindex(A)
        @inbounds z = A[i] - m
        z2 = z * z

        cm2 += z2
        cm3 += z2 * z
    end
    cm3 /= n
    cm2 /= n
    return cm3 / sqrt(cm2 * cm2 * cm2)  # this is much faster than cm2^1.5
end

function _skewness(A::AbstractArray, w::AbstractArray, m::Real)
    n = length(A)
    length(w) == n || throw(DimensionMismatch("Inconsistent array lengths."))
    cm2 = 0.0   # empirical 2nd centered moment (variance)
    cm3 = 0.0   # empirical 3rd centered moment

    @inbounds for i in eachindex(A, w)
        x_i = A[i]
        w_i = w[i]
        z = x_i - m
        z2w = z * z * w_i
        cm2 += z2w
        cm3 += z2w * z
    end
    sw = sum(w)
    cm3 /= sw
    cm2 /= sw
    return cm3 / sqrt(cm2 * cm2 * cm2)  # this is much faster than cm2^1.5
end

_skewness(A::AbstractArray, w::Union{AbstractArray, Nothing}, m::Nothing) =
    _skewness(A, w, mean(A, weights=w))

# (excessive) Kurtosis
# This is Type 1 definition according to Joanes and Gill (1998)
"""
    kurtosis(A; [weights::AbstractArray], [mean::Real])

Compute the excess kurtosis of a real-valued array `A`, optionally
specifying `weights` and `mean`.

!!! compat "Julia 1.2"
    This function requires at least Julia 1.2.
"""
kurtosis(A::AbstractArray;
         weights::Union{AbstractWeights,Nothing}=nothing,
         mean::Union{Real, Nothing}=nothing) =
    _kurtosis(A, weights, mean)

function _kurtosis(A::AbstractArray, w::Nothing, m::Real)
    n = length(A)
    cm2 = 0.0  # empirical 2nd centered moment (variance)
    cm4 = 0.0  # empirical 4th centered moment
    for i in eachindex(A)
        @inbounds z = A[i] - m
        z2 = z * z
        cm2 += z2
        cm4 += z2 * z2
    end
    cm4 /= n
    cm2 /= n
    return (cm4 / (cm2 * cm2)) - 3.0
end

function _kurtosis(A::AbstractArray, w::AbstractWeights, m::Real)
    length(w) == length(A) || throw(DimensionMismatch("Inconsistent array lengths."))
    cm2 = 0.0  # empirical 2nd centered moment (variance)
    cm4 = 0.0  # empirical 4th centered moment

    @inbounds for i in eachindex(A, w)
        x_i = A[i]
        w_i = w[i]
        z = x_i - m
        z2 = z * z
        z2w = z2 * w_i
        cm2 += z2w
        cm4 += z2w * z2
    end
    sw = sum(w)
    cm4 /= sw
    cm2 /= sw
    return (cm4 / (cm2 * cm2)) - 3.0
end

_kurtosis(A::AbstractArray, w::Union{AbstractWeights, Nothing}, m::Nothing) =
    _kurtosis(A, w, mean(A, weights=w))
