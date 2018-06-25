# This file is a part of Julia. License is MIT: https://julialang.org/license

##### mean #####

"""
    mean(f::Function, v)

Apply the function `f` to each element of `v` and take the mean.

```jldoctest
julia> mean(√, [1, 2, 3])
1.3820881233139908

julia> mean([√1, √2, √3])
1.3820881233139908
```
"""
function mean(f::Callable, iterable)
    y = iterate(iterable)
    if y === nothing
        throw(ArgumentError("mean of empty collection undefined: $(repr(iterable))"))
    end
    count = 1
    value, state = y
    f_value = f(value)
    total = reduce_first(add_sum, f_value)
    y = iterate(iterable, state)
    while y !== nothing
        value, state = y
        total += f(value)
        count += 1
        y = iterate(iterable, state)
    end
    return total/count
end
mean(iterable) = mean(identity, iterable)
mean(f::Callable, A::AbstractArray) = sum(f, A) / _length(A)

"""
    mean!(r, v)

Compute the mean of `v` over the singleton dimensions of `r`, and write results to `r`.

# Examples
```jldoctest
julia> v = [1 2; 3 4]
2×2 Array{Int64,2}:
 1  2
 3  4

julia> mean!([1., 1.], v)
2-element Array{Float64,1}:
 1.5
 3.5

julia> mean!([1. 1.], v)
1×2 Array{Float64,2}:
 2.0  3.0
```
"""
function mean!(R::AbstractArray, A::AbstractArray)
    sum!(R, A; init=true)
    x = max(1, _length(R)) // _length(A)
    R .= R .* x
    return R
end

"""
    mean(v; dims)

Compute the mean of whole array `v`, or optionally along the given dimensions.

!!! note
    Julia does not ignore `NaN` values in the computation. Use the [`missing`](@ref) type
    to represent missing values, and the [`skipmissing`](@ref) function to omit them.
"""
mean(A::AbstractArray; dims=:) = _mean(A, dims)

_mean(A::AbstractArray{T}, region) where {T} = mean!(reducedim_init(t -> t/2, +, A, region), A)
_mean(A::AbstractArray, ::Colon) = sum(A) / _length(A)

##### median & quantiles #####

"""
    middle(x)

Compute the middle of a scalar value, which is equivalent to `x` itself, but of the type of `middle(x, x)` for consistency.
"""
middle(x::Union{Bool,Int8,Int16,Int32,Int64,Int128,UInt8,UInt16,UInt32,UInt64,UInt128}) = Float64(x)
# Specialized functions for real types allow for improved performance
middle(x::AbstractFloat) = x
middle(x::Real) = (x + zero(x)) / 1

"""
    middle(x, y)

Compute the middle of two reals `x` and `y`, which is
equivalent in both value and type to computing their mean (`(x + y) / 2`).
"""
middle(x::Real, y::Real) = x/2 + y/2

"""
    middle(range)

Compute the middle of a range, which consists of computing the mean of its extrema.
Since a range is sorted, the mean is performed with the first and last element.

```jldoctest
julia> middle(1:10)
5.5
```
"""
middle(a::AbstractRange) = middle(a[1], a[end])

"""
    middle(a)

Compute the middle of an array `a`, which consists of finding its
extrema and then computing their mean.

```jldoctest
julia> a = [1,2,3.6,10.9]
4-element Array{Float64,1}:
  1.0
  2.0
  3.6
 10.9

julia> middle(a)
5.95
```
"""
middle(a::AbstractArray) = ((v1, v2) = extrema(a); middle(v1, v2))

"""
    median!(v)

Like [`median`](@ref), but may overwrite the input vector.
"""
function median!(v::AbstractVector)
    isempty(v) && throw(ArgumentError("median of an empty array is undefined, $(repr(v))"))
    if eltype(v)<:AbstractFloat
        @inbounds for x in v
            isnan(x) && return x
        end
    end
    inds = axes(v, 1)
    n = _length(inds)
    mid = div(first(inds)+last(inds),2)
    if isodd(n)
        return middle(partialsort!(v,mid))
    else
        m = partialsort!(v, mid:mid+1)
        return middle(m[1], m[2])
    end
end
median!(v::AbstractArray) = median!(vec(v))

"""
    median(v; dims)

Compute the median of an entire array `v`, or, optionally,
along the given dimensions. For an even number of
elements no exact median element exists, so the result is
equivalent to calculating mean of two median elements.

!!! note
    Julia does not ignore `NaN` values in the computation. Use the [`missing`](@ref) type
    to represent missing values, and the [`skipmissing`](@ref) function to omit them.
"""
median(v::AbstractArray; dims=:) = _median(v, dims)

_median(v::AbstractArray, dims) = mapslices(median!, v, dims)

_median(v::AbstractArray{T}, ::Colon) where {T} = median!(copyto!(Array{T,1}(undef, _length(v)), v))

# for now, use the R/S definition of quantile; may want variants later
# see ?quantile in R -- this is type 7
"""
    quantile!([q, ] v, p; sorted=false)

Compute the quantile(s) of a vector `v` at the probability or probabilities `p`, which
can be given as a single value, a vector, or a tuple. If `p` is a vector, an optional
output array `q` may also be specified. (If not provided, a new output array is created.)
The keyword argument `sorted` indicates whether `v` can be assumed to be sorted; if
`false` (the default), then the elements of `v` may be partially sorted.

The elements of `p` should be on the interval [0,1], and `v` should not have any `NaN`
values.

Quantiles are computed via linear interpolation between the points `((k-1)/(n-1), v[k])`,
for `k = 1:n` where `n = length(v)`. This corresponds to Definition 7 of Hyndman and Fan
(1996), and is the same as the R default.

!!! note
    Julia does not ignore `NaN` values in the computation: `quantile!` will
    throw an `ArgumentError` in the presence of `NaN` values in the data array.
    Use the [`missing`](@ref) type to represent missing values, and the
    [`skipmissing`](@ref) function to omit them.

* Hyndman, R.J and Fan, Y. (1996) "Sample Quantiles in Statistical Packages",
  *The American Statistician*, Vol. 50, No. 4, pp. 361-365
"""
function quantile!(q::AbstractArray, v::AbstractVector, p::AbstractArray;
                   sorted::Bool=false)
    if size(p) != size(q)
        throw(DimensionMismatch("size of p, $(size(p)), must equal size of q, $(size(q))"))
    end
    isempty(q) && return q

    minp, maxp = extrema(p)
    _quantilesort!(v, sorted, minp, maxp)

    for (i, j) in zip(eachindex(p), eachindex(q))
        @inbounds q[j] = _quantile(v,p[i])
    end
    return q
end

quantile!(v::AbstractVector, p::AbstractArray; sorted::Bool=false) =
    quantile!(similar(p,float(eltype(v))), v, p; sorted=sorted)

quantile!(v::AbstractVector, p::Real; sorted::Bool=false) =
    _quantile(_quantilesort!(v, sorted, p, p), p)

function quantile!(v::AbstractVector, p::Tuple{Vararg{Real}}; sorted::Bool=false)
    isempty(p) && return ()
    minp, maxp = extrema(p)
    _quantilesort!(v, sorted, minp, maxp)
    return map(x->_quantile(v, x), p)
end

# Function to perform partial sort of v for quantiles in given range
function _quantilesort!(v::AbstractArray, sorted::Bool, minp::Real, maxp::Real)
    isempty(v) && throw(ArgumentError("empty data vector"))

    if !sorted
        lv = length(v)
        lo = floor(Int,1+minp*(lv-1))
        hi = ceil(Int,1+maxp*(lv-1))

        # only need to perform partial sort
        sort!(v, 1, lv, Sort.PartialQuickSort(lo:hi), Base.Sort.Forward)
    end
    isnan(v[end]) && throw(ArgumentError("quantiles are undefined in presence of NaNs"))
    return v
end

# Core quantile lookup function: assumes `v` sorted
@inline function _quantile(v::AbstractVector, p::Real)
    0 <= p <= 1 || throw(ArgumentError("input probability out of [0,1] range"))

    lv = length(v)
    f0 = (lv - 1)*p # 0-based interpolated index
    t0 = trunc(f0)
    h  = f0 - t0
    i  = trunc(Int,t0) + 1

    T  = promote_type(eltype(v), typeof(v[1]*h))

    if h == 0
        return convert(T, v[i])
    else
        a = v[i]
        b = v[i+1]
        if isfinite(a) && isfinite(b)
            return convert(T, a + h*(b-a))
        else
            return convert(T, (1-h)*a + h*b)
        end
    end
end


"""
    quantile(v, p; sorted=false)

Compute the quantile(s) of a vector `v` at a specified probability or vector or tuple of
probabilities `p`. The keyword argument `sorted` indicates whether `v` can be assumed to
be sorted.

The `p` should be on the interval [0,1], and `v` should not have any `NaN` values.

Quantiles are computed via linear interpolation between the points `((k-1)/(n-1), v[k])`,
for `k = 1:n` where `n = length(v)`. This corresponds to Definition 7 of Hyndman and Fan
(1996), and is the same as the R default.

!!! note
    Julia does not ignore `NaN` values in the computation: `quantile` will
    throw an `ArgumentError` in the presence of `NaN` values in the data array.
    Use the [`missing`](@ref) type to represent missing values, and the
    [`skipmissing`](@ref) function to omit them.

- Hyndman, R.J and Fan, Y. (1996) "Sample Quantiles in Statistical Packages",
  *The American Statistician*, Vol. 50, No. 4, pp. 361-365
"""
quantile(v::AbstractVector, p; sorted::Bool=false) =
    quantile!(sorted ? v : copymutable(v), p; sorted=sorted)
