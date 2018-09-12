using LinearAlgebra: BlasReal

###### Weights array #####

"""
    AbstractWeights <: AbstractVector

The abstract supertype of all vectors of statistical weights.

Object of this type behave like other `AbstractVector`s, but
they store the sum of their values internally for efficiency.
Concrete `AbstractWeights` type indicates what correction
has to be applied when computing statistics which depend on the
meaning of weights.

!!! compat "Julia 1.2"
    This type requires at least Julia 1.2.
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

"""
    varcorrection(n::Integer, corrected=false)

Compute a bias correction factor for calculating `var`, `std` and `cov` with
`n` observations. Returns ``\\frac{1}{n - 1}`` when `corrected=true`
(i.e. [Bessel's correction](https://en.wikipedia.org/wiki/Bessel's_correction)),
otherwise returns ``\\frac{1}{n}`` (i.e. no correction).
"""
@inline varcorrection(n::Integer, corrected::Bool=false) = 1 / (n - Int(corrected))

@weights Weights

@doc """
    Weights(vs, wsum=sum(vs))

Construct a `Weights` vector with weight values `vs`.
A precomputed sum may be provided as `wsum`.

The `Weights` type describes a generic weights vector which does not support
all operations possible for [`FrequencyWeights`](@ref), [`AnalyticWeights`](@ref)
and [`ProbabilityWeights`](@ref).

!!! compat "Julia 1.2"
    This type requires at least Julia 1.2.
""" Weights

"""
    weights(vs)

Construct a `Weights` vector from array `vs`.
See the documentation for [`Weights`](@ref) for more details.
"""
weights(vs::AbstractVector{<:Real}) = Weights(vs)
weights(vs::AbstractArray{<:Real}) = Weights(vec(vs))

"""
    varcorrection(w::Weights, corrected=false)

Returns ``\\frac{1}{\\sum w}`` when `corrected=false` and throws an `ArgumentError`
if `corrected=true`.
"""
@inline function varcorrection(w::Weights, corrected::Bool=false)
    corrected && throw(ArgumentError("Weights type does not support bias correction: " *
                                     "use FrequencyWeights, AnalyticWeights or ProbabilityWeights if applicable."))
    1 / w.sum
end

@weights AnalyticWeights

@doc """
    AnalyticWeights(vs, wsum=sum(vs))

Construct an `AnalyticWeights` vector with weight values `vs`.
A precomputed sum may be provided as `wsum`.

Analytic weights describe a non-random relative importance (usually between 0 and 1)
for each observation. These weights may also be referred to as reliability weights,
precision weights or inverse variance weights. These are typically used when the observations
being weighted are aggregate values (e.g., averages) with differing variances.

!!! compat "Julia 1.2"
    This type requires at least Julia 1.2.
""" AnalyticWeights

"""
    aweights(vs)

Construct an `AnalyticWeights` vector from array `vs`.
See the documentation for [`AnalyticWeights`](@ref) for more details.

!!! compat "Julia 1.2"
    This function requires at least Julia 1.2.
"""
aweights(vs::AbstractVector{<:Real}) = AnalyticWeights(vs)
aweights(vs::AbstractArray{<:Real}) = AnalyticWeights(vec(vs))

"""
    varcorrection(w::AnalyticWeights, corrected=false)

* `corrected=true`: ``\\frac{1}{\\sum w - \\sum {w^2} / \\sum w}``
* `corrected=false`: ``\\frac{1}{\\sum w}``
"""
@inline function varcorrection(w::AnalyticWeights, corrected::Bool=false)
    s = w.sum

    if corrected
        sum_sn = sum(x -> (x / s) ^ 2, w)
        1 / (s * (1 - sum_sn))
    else
        1 / s
    end
end

@weights FrequencyWeights

@doc """
    FrequencyWeights(vs, wsum=sum(vs))

Construct a `FrequencyWeights` vector with weight values `vs`.
A precomputed sum may be provided as `wsum`.

Frequency weights describe the number of times (or frequency) each observation
was observed. These weights may also be referred to as case weights or repeat weights.

!!! compat "Julia 1.2"
    This type requires at least Julia 1.2.
""" FrequencyWeights

"""
    fweights(vs)

Construct a `FrequencyWeights` vector from a given array.
See the documentation for [`FrequencyWeights`](@ref) for more details.

!!! compat "Julia 1.2"
    This function requires at least Julia 1.2.
"""
fweights(vs::AbstractVector{<:Real}) = FrequencyWeights(vs)
fweights(vs::AbstractArray{<:Real}) = FrequencyWeights(vec(vs))

"""
    varcorrection(w::FrequencyWeights, corrected=false)

* `corrected=true`: ``\\frac{1}{\\sum{w} - 1}``
* `corrected=false`: ``\\frac{1}{\\sum w}``
"""
@inline function varcorrection(w::FrequencyWeights, corrected::Bool=false)
    s = w.sum

    if corrected
        1 / (s - 1)
    else
        1 / s
    end
end

@weights ProbabilityWeights

@doc """
    ProbabilityWeights(vs, wsum=sum(vs))

Construct a `ProbabilityWeights` vector with weight values `vs`.
A precomputed sum may be provided as `wsum`.

Probability weights represent the inverse of the sampling probability for each observation,
providing a correction mechanism for under- or over-sampling certain population groups.
These weights may also be referred to as sampling weights.

!!! compat "Julia 1.2"
    This type requires at least Julia 1.2.
""" ProbabilityWeights

"""
    pweights(vs)

Construct a `ProbabilityWeights` vector from a given array.
See the documentation for [`ProbabilityWeights`](@ref) for more details.

!!! compat "Julia 1.2"
    This function requires at least Julia 1.2.
"""
pweights(vs::AbstractVector{<:Real}) = ProbabilityWeights(vs)
pweights(vs::AbstractArray{<:Real}) = ProbabilityWeights(vec(vs))

"""
    varcorrection(w::ProbabilityWeights, corrected=false)

* `corrected=true`: ``\\frac{n}{(n - 1) \\sum w}`` where ``n`` equals `count(!iszero, w)`
* `corrected=false`: ``\\frac{1}{\\sum w}``
"""
@inline function varcorrection(w::ProbabilityWeights, corrected::Bool=false)
    s = w.sum

    if corrected
        n = count(!iszero, w)
        n / (s * (n - 1))
    else
        1 / s
    end
end

##### Equality tests #####

for w in (AnalyticWeights, FrequencyWeights, ProbabilityWeights, Weights)
    @eval begin
        Base.isequal(x::$w, y::$w) = isequal(x.sum, y.sum) && isequal(x.values, y.values)
        Base.:(==)(x::$w, y::$w)   = (x.sum == y.sum) && (x.values == y.values)
    end
end

Base.isequal(x::AbstractWeights, y::AbstractWeights) = false
Base.:(==)(x::AbstractWeights, y::AbstractWeights)   = false

##### Weighted sum #####

## weighted sum over vectors

"""
    wsum(v::AbstractArray, w::AbstractArray, [dim])

Compute the weighted sum of an array `v` with weights `w`, optionally over the dimension `dim`.
"""
function wsum(A::AbstractArray, w::AbstractArray)
    sw = size(w)
    sA = size(A)
    if sw != sA
        throw(DimensionMismatch("weights must have the same dimension as data (got $sw and $sA)."))
    end
    _wsum(A, w)
end

_wsum(A::AbstractVector, w::AbstractVector) = dot(A, w)
_wsum(A::AbstractArray, w::AbstractArray) = dot(vec(A), vec(w))

## wsum along dimension
#
#  Brief explanation of the algorithm:
#  ------------------------------------
#
#  1. _wsum! provides the core implementation, which assumes that
#     the dimensions of all input arguments are consistent, and no
#     dimension checking is performed therein.
#
#     wsum and wsum! perform argument checking and call _wsum!
#     internally.
#
#  2. _wsum! adopt a Cartesian based implementation for general
#     sub types of AbstractArray. Particularly, a faster routine
#     that keeps a local accumulator will be used when dim = 1.
#
#     The internal function that implements this is _wsum_general!
#
#  3. _wsum! is specialized for following cases:
#     (a) A is a vector: we invoke the vector version wsum above.
#         The internal function that implements this is _wsum1!
#
#     (b) A is a dense matrix with eltype <: BlasReal: we call gemv!
#         The internal function that implements this is _wsum2_blas!
#
#     (c) A is a contiguous array with eltype <: BlasReal:
#         dim == 1: treat A like a matrix of size (d1, d2 x ... x dN)
#         dim == N: treat A like a matrix of size (d1 x ... x d(N-1), dN)
#         otherwise: decompose A into multiple pages, and apply _wsum2!
#         for each
#
#     (d) A is a general dense array with eltype <: BlasReal:
#         dim <= 2: delegate to (a) and (b)
#         otherwise, decompose A into multiple pages
#

function _wsum1!(R::AbstractArray, A::AbstractVector, w::AbstractVector, init::Bool)
    r = wsum(A, w)
    if init
        R[1] = r
    else
        R[1] += r
    end
    return R
end

function _wsum2_blas!(R::StridedVector{T}, A::StridedMatrix{T}, w::StridedVector{T},
                      dim::Int, init::Bool) where T<:BlasReal
    beta = ifelse(init, zero(T), one(T))
    trans = dim == 1 ? 'T' : 'N'
    BLAS.gemv!(trans, one(T), A, w, beta, R)
    return R
end

function _wsumN!(R::StridedArray{T}, A::StridedArray{T,N}, w::StridedVector{T},
                 dim::Int, init::Bool) where {T<:BlasReal,N}
    if dim == 1
        m = size(A, 1)
        n = div(length(A), m)
        _wsum2_blas!(view(R,:), reshape(A, (m, n)), w, 1, init)
    elseif dim == N
        n = size(A, N)
        m = div(length(A), n)
        _wsum2_blas!(view(R,:), reshape(A, (m, n)), w, 2, init)
    else # 1 < dim < N
        m = 1
        for i = 1:dim-1; m *= size(A, i); end
        n = size(A, dim)
        k = 1
        for i = dim+1:N; k *= size(A, i); end
        Av = reshape(A, (m, n, k))
        Rv = reshape(R, (m, k))
        for i = 1:k
            _wsum2_blas!(view(Rv,:,i), view(Av,:,:,i), w, 2, init)
        end
    end
    return R
end

function _wsumN!(R::StridedArray{T}, A::DenseArray{T,N}, w::StridedVector{T},
                 dim::Int, init::Bool) where {T<:BlasReal,N}
    @assert N >= 3
    if dim <= 2
        m = size(A, 1)
        n = size(A, 2)
        npages = 1
        for i = 3:N
            npages *= size(A, i)
        end
        rlen = ifelse(dim == 1, n, m)
        Rv = reshape(R, (rlen, npages))
        for i = 1:npages
            _wsum2_blas!(view(Rv,:,i), view(A,:,:,i), w, dim, init)
        end
    else
        _wsum_general!(R, identity, A, w, dim, init)
    end
    return R
end

# General weighted sum across dimensions
function _wsum_general!(R::AbstractArray{S}, A::AbstractArray{T, N}, w::AbstractVector{W},
                        dim::Int, init::Bool) where {S, T, N, W}
    # following the implementation of _mapreducedim! at base/reducedim.jl
    lsiz = Base.check_reducedims(R,A)
    isempty(R) || fill!(R, zero(S))
    isempty(A) && return R

    indsAt, indsRt = Base.safe_tail(axes(A)), Base.safe_tail(axes(R)) # handle d=1 manually
    keep, Idefault = Broadcast.shapeindexer(indsRt)
    if Base.reducedim1(R, A)
        i1 = first(Base.axes1(R))
        for IA in CartesianIndices(indsAt)
            IR = Broadcast.newindex(IA, keep, Idefault)
            r = R[i1,IR]
            @simd for i in axes(A, 1)
                r += A[i,IA] * w[dim > 1 ? IA[dim-1] : i]
            end
            R[i1,IR] = r
        end
    else
        for IA in CartesianIndices(indsAt)
            IR = Broadcast.newindex(IA, keep, Idefault)
            @simd for i in axes(A, 1)
                R[i,IR] += A[i,IA] * w[dim > 1 ? IA[dim-1] : i]
            end
        end
    end
    return R
end

# N = 1
_wsum!(R::StridedArray{T}, A::DenseArray{T,1}, w::StridedVector{T},
       dim::Int, init::Bool) where {T<:BlasReal} =
    _wsum1!(R, A, w, init)

# N = 2
_wsum!(R::StridedArray{T}, A::DenseArray{T,2}, w::StridedVector{T},
       dim::Int, init::Bool) where {T<:BlasReal} =
    _wsum2_blas!(view(R,:), A, w, init)

# N >= 3
_wsum!(R::StridedArray{T}, A::DenseArray{T}, w::StridedVector{T},
       dim::Int, init::Bool) where {T<:BlasReal} =
    _wsumN!(R, A, w, init)

_wsum!(R::AbstractArray, A::AbstractArray, w::AbstractVector,
       dim::Int, init::Bool) =
    _wsum_general!(R, A, w, dim, init)

## wsum! and wsum

function wsum!(R::AbstractArray, A::AbstractArray{T,N}, w::AbstractVector;
               init::Bool=true) where {T,N}
    Base.check_reducedims(R,A)
    reddims = size(R) .!= size(A)
    dim = something(findfirst(reddims), ndims(R)+1)
    if findnext(reddims, dim+1) !== nothing
        throw(ArgumentError("reducing over more than one dimension is not supported with weights"))
    end
    lw = length(w)
    ldim = size(A, dim)
    if lw != ldim
        throw(DimensionMismatch("weights must have the same length as the dimension " *
                                "over which reduction is performed (got $lw and $ldim)."))
    end
    _wsum!(R, A, w, dim, init)
end