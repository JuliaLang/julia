###### Weights array #####

"""
    AbstractWeights <: AbstractVector

The abstract supertype of all vectors of statistical weights.

Object of this type behave like other `AbstractVector`s, but
they store the sum of their values internally for efficiency.
Concrete `AbstractWeights` type indicates what correction
has to be applied when computing statistics which depend on the
meaning of weights.

!!! compat "Julia 1.3"
    This type requires at least Julia 1.3.
"""
abstract type AbstractWeights{S<:Real, T<:Real, V<:AbstractVector{T}} <: AbstractVector{T} end

"""
    @weights name

Generate a new generic weight type with specified `name`, which subtypes `AbstractWeights`
and stores the `values` (`V<:AbstractVector{<:Real}`) and `sum` (`S<:Real`).
"""
macro weights(name)
    return quote
        mutable struct $name{S<:Real, T<:Real, V<:AbstractVector{T}} <: AbstractWeights{S, T, V}
            values::V
            sum::S
        end
        $(esc(name))(vs) = $(esc(name))(vs, sum(vs))
    end
end

Base.eltype(wv::AbstractWeights) = eltype(wv.values)
Base.length(wv::AbstractWeights) = length(wv.values)
Base.values(wv::AbstractWeights) = wv.values
Base.sum(wv::AbstractWeights) = wv.sum
Base.isempty(wv::AbstractWeights) = isempty(wv.values)

Base.getindex(wv::AbstractWeights, i::Int) = getindex(wv.values, i)
Base.size(wv::AbstractWeights) = size(wv.values)

Base.@propagate_inbounds function Base.setindex!(wv::AbstractWeights, v::Real, i::Int)
    s = v - wv[i]
    wv.values[i] = v
    wv.sum += s
    v
end

@weights Weights

@doc """
    Weights(vs, wsum=sum(vs))

Construct a `Weights` vector with weight values `vs`.
A precomputed sum may be provided as `wsum`.

The `Weights` type describes a generic weights vector which does not support
all operations possible for [`FrequencyWeights`](@ref), [`AnalyticWeights`](@ref)
and [`ProbabilityWeights`](@ref).

!!! compat "Julia 1.3"
    This type requires at least Julia 1.3.
""" Weights

"""
    weights(vs)

Construct a `Weights` vector from array `vs`.
See the documentation for [`Weights`](@ref) for more details.
"""
weights(vs::AbstractVector{<:Real}) = Weights(vs)
weights(vs::AbstractArray{<:Real}) = Weights(vec(vs))

@weights AnalyticWeights

@doc """
    AnalyticWeights(vs, wsum=sum(vs))

Construct an `AnalyticWeights` vector with weight values `vs`.
A precomputed sum may be provided as `wsum`.

Analytic weights describe a non-random relative importance (usually between 0 and 1)
for each observation. These weights may also be referred to as reliability weights,
precision weights or inverse variance weights. These are typically used when the observations
being weighted are aggregate values (e.g., averages) with differing variances.

!!! compat "Julia 1.3"
    This type requires at least Julia 1.3.
""" AnalyticWeights

"""
    aweights(vs)

Construct an `AnalyticWeights` vector from array `vs`.
See the documentation for [`AnalyticWeights`](@ref) for more details.

!!! compat "Julia 1.3"
    This function requires at least Julia 1.3.
"""
aweights(vs::AbstractVector{<:Real}) = AnalyticWeights(vs)
aweights(vs::AbstractArray{<:Real}) = AnalyticWeights(vec(vs))

@weights FrequencyWeights

@doc """
    FrequencyWeights(vs, wsum=sum(vs))

Construct a `FrequencyWeights` vector with weight values `vs`.
A precomputed sum may be provided as `wsum`.

Frequency weights describe the number of times (or frequency) each observation
was observed. These weights may also be referred to as case weights or repeat weights.

!!! compat "Julia 1.3"
    This type requires at least Julia 1.3.
""" FrequencyWeights

"""
    fweights(vs)

Construct a `FrequencyWeights` vector from a given array.
See the documentation for [`FrequencyWeights`](@ref) for more details.

!!! compat "Julia 1.3"
    This function requires at least Julia 1.3.
"""
fweights(vs::AbstractVector{<:Real}) = FrequencyWeights(vs)
fweights(vs::AbstractArray{<:Real}) = FrequencyWeights(vec(vs))

@weights ProbabilityWeights

@doc """
    ProbabilityWeights(vs, wsum=sum(vs))

Construct a `ProbabilityWeights` vector with weight values `vs`.
A precomputed sum may be provided as `wsum`.

Probability weights represent the inverse of the sampling probability for each observation,
providing a correction mechanism for under- or over-sampling certain population groups.
These weights may also be referred to as sampling weights.

!!! compat "Julia 1.3"
    This type requires at least Julia 1.3.
""" ProbabilityWeights

"""
    pweights(vs)

Construct a `ProbabilityWeights` vector from a given array.
See the documentation for [`ProbabilityWeights`](@ref) for more details.

!!! compat "Julia 1.3"
    This function requires at least Julia 1.3.
"""
pweights(vs::AbstractVector{<:Real}) = ProbabilityWeights(vs)
pweights(vs::AbstractArray{<:Real}) = ProbabilityWeights(vec(vs))

##### Equality tests #####

for w in (AnalyticWeights, FrequencyWeights, ProbabilityWeights, Weights)
    @eval begin
        Base.isequal(x::$w, y::$w) = isequal(x.sum, y.sum) && isequal(x.values, y.values)
        Base.:(==)(x::$w, y::$w)   = (x.sum == y.sum) && (x.values == y.values)
    end
end

Base.isequal(x::AbstractWeights, y::AbstractWeights) = false
Base.:(==)(x::AbstractWeights, y::AbstractWeights)   = false