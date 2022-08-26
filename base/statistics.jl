# This file is a part of Julia. License is MIT: https://julialang.org/license

##### mean #####

"""
    mean(itr)

Compute the mean of all elements in a collection.

!!! note
    If `itr` contains `NaN` or [`missing`](@ref) values, the result is also
    `NaN` or `missing` (`missing` takes precedence if array contains both).
    Use the [`skipmissing`](@ref) function to omit `missing` entries and compute the
    mean of non-missing values.

!!! compat "Julia 1.9"
    This method requires at least Julia 1.9.
    Support for older releases is provided by the Statistics package.

# Examples
```jldoctest
julia> mean(1:20)
10.5

julia> mean([1, missing, 3])
missing

julia> mean(skipmissing([1, missing, 3]))
2.0
```
"""
mean(itr) = mean(identity, itr)

"""
    mean(f, itr)

Apply the function `f` to each element of collection `itr` and take the mean.

!!! compat "Julia 1.9"
    This method requires at least Julia 1.9.
    Support for older releases is provided by the Statistics package.

# Examples
```jldoctest
julia> mean(√, [1, 2, 3])
1.3820881233139908

julia> mean([√1, √2, √3])
1.3820881233139908
```
"""
function mean(f, itr)
    y = iterate(itr)
    if y === nothing
        return Base.mapreduce_empty_iter(f, +, itr,
                                         Base.IteratorEltype(itr)) / 0
    end
    count = 1
    value, state = y
    f_value = f(value)/1
    total = Base.reduce_first(+, f_value)
    y = iterate(itr, state)
    while y !== nothing
        value, state = y
        total += _mean_promote(total, f(value))
        count += 1
        y = iterate(itr, state)
    end
    return total/count
end

"""
    mean(f, A::AbstractArray; dims)

Apply the function `f` to each element of array `A` and take the mean over dimensions `dims`.

!!! compat "Julia 1.9"
    This method requires at least Julia 1.9.
    Support for older releases is provided by the Statistics package.

```jldoctest
julia> mean(√, [1, 2, 3])
1.3820881233139908

julia> mean([√1, √2, √3])
1.3820881233139908

julia> mean(√, [1 2 3; 4 5 6], dims=2)
2×1 Matrix{Float64}:
 1.3820881233139908
 2.2285192400943226
```
"""
mean(f, A::AbstractArray; dims=:) = _mean(f, A, dims)

"""
    mean!(r, v)

Compute the mean of `v` over the singleton dimensions of `r`, and write results to `r`.

!!! compat "Julia 1.9"
    This method requires at least Julia 1.9.
    Support for older releases is provided by the Statistics package.

# Examples
```jldoctest
julia> v = [1 2; 3 4]
2×2 Matrix{Int64}:
 1  2
 3  4

julia> mean!([1., 1.], v)
2-element Vector{Float64}:
 1.5
 3.5

julia> mean!([1. 1.], v)
1×2 Matrix{Float64}:
 2.0  3.0
```
"""
function mean!(R::AbstractArray, A::AbstractArray)
    sum!(R, A; init=true)
    x = max(1, length(R)) // length(A)
    R .= R .* x
    return R
end

"""
    mean(A::AbstractArray; dims)

Compute the mean of an array over the given dimensions.

!!! compat "Julia 1.9"
    This method requires at least Julia 1.9.
    Support for older releases is provided by the Statistics package.

# Examples
```jldoctest
julia> A = [1 2; 3 4]
2×2 Matrix{Int64}:
 1  2
 3  4

julia> mean(A, dims=1)
1×2 Matrix{Float64}:
 2.0  3.0

julia> mean(A, dims=2)
2×1 Matrix{Float64}:
 1.5
 3.5
```
"""
mean(A::AbstractArray; dims=:) = _mean(identity, A, dims)

_mean_promote(x::T, y::S) where {T,S} = convert(promote_type(T, S), y)

# ::Dims is there to force specializing on Colon (as it is a Function)
function _mean(f, A::AbstractArray, dims::Dims=:) where Dims
    isempty(A) && return sum(f, A, dims=dims)/0
    if dims === (:)
        n = length(A)
    else
        n = mapreduce(i -> size(A, i), *, unique(dims); init=1)
    end
    x1 = f(first(A)) / 1
    result = sum(x -> _mean_promote(x1, f(x)), A, dims=dims)
    if dims === (:)
        return result / n
    else
        return result ./= n
    end
end

function mean(r::AbstractRange{<:Real})
    isempty(r) && return oftype((first(r) + last(r)) / 2, NaN)
    (first(r) + last(r)) / 2
end


##### variances #####

# faster computation of real(conj(x)*y)
realXcY(x::Real, y::Real) = x*y
realXcY(x::Complex, y::Complex) = real(x)*real(y) + imag(x)*imag(y)

var(iterable; corrected::Bool=true, mean=nothing) = _var(iterable, corrected, mean)

function _var(iterable, corrected::Bool, mean)
    y = iterate(iterable)
    if y === nothing
        T = eltype(iterable)
        return oftype((abs2(zero(T)) + abs2(zero(T)))/2, NaN)
    end
    count = 1
    value, state = y
    y = iterate(iterable, state)
    if mean === nothing
        # Use Welford algorithm as seen in (among other places)
        # Knuth's TAOCP, Vol 2, page 232, 3rd edition.
        M = value / 1
        S = real(zero(M))
        while y !== nothing
            value, state = y
            y = iterate(iterable, state)
            count += 1
            new_M = M + (value - M) / count
            S = S + realXcY(value - M, value - new_M)
            M = new_M
        end
        return S / (count - Int(corrected))
    elseif isa(mean, Number) # mean provided
        # Cannot use a compensated version, e.g. the one from
        # "Updating Formulae and a Pairwise Algorithm for Computing Sample Variances."
        # by Chan, Golub, and LeVeque, Technical Report STAN-CS-79-773,
        # Department of Computer Science, Stanford University,
        # because user can provide mean value that is different to mean(iterable)
        sum2 = abs2(value - mean::Number)
        while y !== nothing
            value, state = y
            y = iterate(iterable, state)
            count += 1
            sum2 += abs2(value - mean)
        end
        return sum2 / (count - Int(corrected))
    else
        throw(ArgumentError("invalid value of mean, $(mean)::$(typeof(mean))"))
    end
end

centralizedabs2fun(m) = x -> abs2.(x - m)
centralize_sumabs2(A::AbstractArray, m) =
    mapreduce(centralizedabs2fun(m), +, A)
centralize_sumabs2(A::AbstractArray, m, ifirst::Int, ilast::Int) =
    Base.mapreduce_impl(centralizedabs2fun(m), +, A, ifirst, ilast)

function centralize_sumabs2!(R::AbstractArray{S}, A::AbstractArray, means::AbstractArray) where S
    # following the implementation of _mapreducedim! at base/reducedim.jl
    lsiz = Base.check_reducedims(R,A)
    for i in 1:max(ndims(R), ndims(means))
        if axes(means, i) != axes(R, i)
            throw(DimensionMismatch("dimension $i of `mean` should have indices $(axes(R, i)), but got $(axes(means, i))"))
        end
    end
    isempty(R) || fill!(R, zero(S))
    isempty(A) && return R

    if Base.has_fast_linear_indexing(A) && lsiz > 16 && !Base.has_offset_axes(R, means)
        nslices = div(length(A), lsiz)
        ibase = first(LinearIndices(A))-1
        for i = 1:nslices
            @inbounds R[i] = centralize_sumabs2(A, means[i], ibase+1, ibase+lsiz)
            ibase += lsiz
        end
        return R
    end
    indsAt, indsRt = Base.safe_tail(axes(A)), Base.safe_tail(axes(R)) # handle d=1 manually
    keep, Idefault = Broadcast.shapeindexer(indsRt)
    if Base.reducedim1(R, A)
        i1 = first(Base.axes1(R))
        @inbounds for IA in CartesianIndices(indsAt)
            IR = Broadcast.newindex(IA, keep, Idefault)
            r = R[i1,IR]
            m = means[i1,IR]
            @simd for i in axes(A, 1)
                r += abs2(A[i,IA] - m)
            end
            R[i1,IR] = r
        end
    else
        @inbounds for IA in CartesianIndices(indsAt)
            IR = Broadcast.newindex(IA, keep, Idefault)
            @simd for i in axes(A, 1)
                R[i,IR] += abs2(A[i,IA] - means[i,IR])
            end
        end
    end
    return R
end

function varm!(R::AbstractArray{S}, A::AbstractArray, m::AbstractArray; corrected::Bool=true) where S
    if isempty(A)
        fill!(R, convert(S, NaN))
    else
        rn = div(length(A), length(R)) - Int(corrected)
        centralize_sumabs2!(R, A, m)
        R .= R .* (1 // rn)
    end
    return R
end

varm(A::AbstractArray, m::AbstractArray; corrected::Bool=true, dims=:) = _varm(A, m, corrected, dims)

_varm(A::AbstractArray{T}, m, corrected::Bool, region) where {T} =
    varm!(Base.reducedim_init(t -> abs2(t)/2, +, A, region), A, m; corrected=corrected)

varm(A::AbstractArray, m; corrected::Bool=true) = _varm(A, m, corrected, :)

function _varm(A::AbstractArray{T}, m, corrected::Bool, ::Colon) where T
    n = length(A)
    n == 0 && return oftype((abs2(zero(T)) + abs2(zero(T)))/2, NaN)
    return centralize_sumabs2(A, m) / (n - Int(corrected))
end


"""
    var(itr; corrected::Bool=true, mean=nothing[, dims])

Compute the sample variance of collection `itr`.

The algorithm returns an estimator of the generative distribution's variance
under the assumption that each entry of `itr` is a sample drawn from the same
unknown distribution, with the samples uncorrelated.
For arrays, this computation is equivalent to calculating
`sum((itr .- mean(itr)).^2) / (length(itr) - 1)`.
If `corrected` is `true`, then the sum is scaled with `n-1`,
whereas the sum is scaled with `n` if `corrected` is
`false` where `n` is the number of elements in `itr`.

If `itr` is an `AbstractArray`, `dims` can be provided to compute the variance
over dimensions.

A pre-computed `mean` may be provided. When `dims` is specified, `mean` must be
an array with the same shape as `mean(itr, dims=dims)` (additional trailing
singleton dimensions are allowed).

!!! note
    If array contains `NaN` or [`missing`](@ref) values, the result is also
    `NaN` or `missing` (`missing` takes precedence if array contains both).
    Use the [`skipmissing`](@ref) function to omit `missing` entries and compute the
    variance of non-missing values.

!!! compat "Julia 1.9"
    This method requires at least Julia 1.9.
    Support for older releases is provided by the Statistics package.
"""
var(A::AbstractArray; corrected::Bool=true, mean=nothing, dims=:) = _var(A, corrected, mean, dims)

function _var(A::AbstractArray, corrected::Bool, m, dims)
  if m === nothing
      m = mean(A, dims=dims)
  end
  return varm(A, m; corrected=corrected, dims=dims)
end

function _var(A::AbstractArray, corrected::Bool, m, ::Colon)
  if m === nothing
      m = mean(A)
  end
  return real(varm(A, m; corrected=corrected))
end

varm(iterable, m; corrected::Bool=true) = _var(iterable, corrected, m)

## variances over ranges

varm(v::AbstractRange, m::AbstractArray) = range_varm(v, m)
varm(v::AbstractRange, m) = range_varm(v, m)

function range_varm(v::AbstractRange, m)
    f  = first(v) - m
    s  = step(v)
    l  = length(v)
    vv = f^2 * l / (l - 1) + f * s * l + s^2 * l * (2 * l - 1) / 6
    if l == 0 || l == 1
        return typeof(vv)(NaN)
    end
    return vv
end

function var(v::AbstractRange)
    s  = step(v)
    l  = length(v)
    vv = abs2(s) * (l + 1) * l / 12
    if l == 0 || l == 1
        return typeof(vv)(NaN)
    end
    return vv
end


##### standard deviation #####

function sqrt!(A::AbstractArray)
    for i in eachindex(A)
        @inbounds A[i] = sqrt(A[i])
    end
    A
end

"""
    std(itr; corrected::Bool=true, mean=nothing[, dims])

Compute the sample standard deviation of collection `itr`.

The algorithm returns an estimator of the generative distribution's standard
deviation under the assumption that each entry of `itr` is a sample drawn from
the same unknown distribution, with the samples uncorrelated.
For arrays, this computation is equivalent to calculating
`sqrt(sum((itr .- mean(itr)).^2) / (length(itr) - 1))`.
If `corrected` is `true`, then the sum is scaled with `n-1`,
whereas the sum is scaled with `n` if `corrected` is
`false` with `n` the number of elements in `itr`.

If `itr` is an `AbstractArray`, `dims` can be provided to compute the standard deviation
over dimensions, and `means` may contain means for each dimension of `itr`.

A pre-computed `mean` may be provided. When `dims` is specified, `mean` must be
an array with the same shape as `mean(itr, dims=dims)` (additional trailing
singleton dimensions are allowed).

!!! note
    If array contains `NaN` or [`missing`](@ref) values, the result is also
    `NaN` or `missing` (`missing` takes precedence if array contains both).
    Use the [`skipmissing`](@ref) function to omit `missing` entries and compute the
    standard deviation of non-missing values.

!!! compat "Julia 1.9"
    This method requires at least Julia 1.9.
    Support for older releases is provided by the Statistics package.
"""
std(A::AbstractArray; corrected::Bool=true, mean=nothing, dims=:) = _std(A, corrected, mean, dims)

_std(A::AbstractArray, corrected::Bool, mean, dims) =
    sqrt.(var(A; corrected=corrected, mean=mean, dims=dims))

_std(A::AbstractArray, corrected::Bool, mean, ::Colon) =
    sqrt.(var(A; corrected=corrected, mean=mean))

_std(A::AbstractArray{<:AbstractFloat}, corrected::Bool, mean, dims) =
    sqrt!(var(A; corrected=corrected, mean=mean, dims=dims))

_std(A::AbstractArray{<:AbstractFloat}, corrected::Bool, mean, ::Colon) =
    sqrt.(var(A; corrected=corrected, mean=mean))

std(iterable; corrected::Bool=true, mean=nothing) =
    sqrt(var(iterable, corrected=corrected, mean=mean))
