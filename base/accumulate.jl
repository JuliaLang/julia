# accumulate_pairwise slightly slower then accumulate, but more numerically
# stable in certain situations (e.g. sums).
# it does double the number of operations compared to accumulate,
# though for cheap operations like + this does not have much impact (20%)
function _accumulate_pairwise!(op::Op, c::AbstractVector{T}, v::AbstractVector, s, i1, n)::T where {T,Op}
    @inbounds if n < 128
        s_ = v[i1]
        c[i1] = op(s, s_)
        for i = i1+1:i1+n-1
            s_ = op(s_, v[i])
            c[i] = op(s, s_)
        end
    else
        n2 = n >> 1
        s_ = _accumulate_pairwise!(op, c, v, s, i1, n2)
        s_ = op(s_, _accumulate_pairwise!(op, c, v, op(s, s_), i1+n2, n-n2))
    end
    return s_
end

function accumulate_pairwise!(op::Op, result::AbstractVector, v::AbstractVector) where Op
    li = LinearIndices(v)
    li != LinearIndices(result) && throw(DimensionMismatch("input and output array sizes and indices must match"))
    n = length(li)
    n == 0 && return result
    i1 = first(li)
    @inbounds result[i1] = v1 = reduce_first(op,v[i1])
    n == 1 && return result
    _accumulate_pairwise!(op, result, v, v1, i1+1, n-1)
    return result
end

function accumulate_pairwise(op, v::AbstractVector{T}) where T
    out = similar(v, promote_op(op, T, T))
    return accumulate_pairwise!(op, out, v)
end


"""
    cumsum!(B, A; dims::Integer)

Cumulative sum of `A` along the dimension `dims`, storing the result in `B`. See also [`cumsum`](@ref).
"""
cumsum!(B::AbstractArray{T}, A; dims::Integer) where {T} =
    accumulate!(add_sum, B, A, dims=dims)

function cumsum!(out::AbstractArray, v::AbstractVector; dims::Integer=1)
    # we dispatch on the possibility of numerical stability issues
    _cumsum!(out, v, dims, ArithmeticStyle(eltype(out)))
end

function _cumsum!(out::AbstractArray{T}, v, dim, ::ArithmeticRounds) where {T}
    dim == 1 ? accumulate_pairwise!(add_sum, out, v) : copyto!(out, v)
end
function _cumsum!(out::AbstractArray, v, dim, ::ArithmeticUnknown)
    _cumsum!(out, v, dim, ArithmeticRounds())
end
function _cumsum!(out::AbstractArray{T}, v, dim, ::ArithmeticStyle) where {T}
    dim == 1 ? accumulate!(add_sum, out, v) : copyto!(out, v)
end

"""
    cumsum(A; dims::Integer)

Cumulative sum along the dimension `dims`. See also [`cumsum!`](@ref)
to use a preallocated output array, both for performance and to control the precision of the
output (e.g. to avoid overflow).

# Examples
```jldoctest
julia> a = [1 2 3; 4 5 6]
2×3 Array{Int64,2}:
 1  2  3
 4  5  6

julia> cumsum(a, dims=1)
2×3 Array{Int64,2}:
 1  2  3
 5  7  9

julia> cumsum(a, dims=2)
2×3 Array{Int64,2}:
 1  3   6
 4  9  15
```
"""
function cumsum(A::AbstractArray{T}; dims::Union{Nothing,Integer}=nothing) where T
    if dims === nothing
        depwarn("`cumsum(A::AbstractArray)` is deprecated, use `cumsum(A, dims=1)` instead.", :cumsum)
        dims = 1
    end
    out = similar(A, promote_op(add_sum, T, T))
    cumsum!(out, A, dims=dims)
end

"""
    cumsum(x::AbstractVector)

Cumulative sum a vector. See also [`cumsum!`](@ref)
to use a preallocated output array, both for performance and to control the precision of the
output (e.g. to avoid overflow).

# Examples
```jldoctest
julia> cumsum([1, 1, 1])
3-element Array{Int64,1}:
 1
 2
 3

julia> cumsum([fill(1, 2) for i in 1:3])
3-element Array{Array{Int64,1},1}:
 [1, 1]
 [2, 2]
 [3, 3]
```
"""
cumsum(x::AbstractVector) = cumsum(x, dims=1)


"""
    cumprod!(B, A; dims::Integer)

Cumulative product of `A` along the dimension `dims`, storing the result in `B`.
See also [`cumprod`](@ref).
"""
cumprod!(B::AbstractArray{T}, A; dims::Integer) where {T} =
    accumulate!(mul_prod, B, A, dims=dims)

"""
    cumprod!(y::AbstractVector, x::AbstractVector)

Cumulative product of a vector `x`, storing the result in `y`.
See also [`cumprod`](@ref).
"""
cumprod!(y::AbstractVector, x::AbstractVector) = cumprod!(y, x, dims=1)

"""
    cumprod(A; dims::Integer)

Cumulative product along the dimension `dim`. See also
[`cumprod!`](@ref) to use a preallocated output array, both for performance and
to control the precision of the output (e.g. to avoid overflow).

# Examples
```jldoctest
julia> a = [1 2 3; 4 5 6]
2×3 Array{Int64,2}:
 1  2  3
 4  5  6

julia> cumprod(a, dims=1)
2×3 Array{Int64,2}:
 1   2   3
 4  10  18

julia> cumprod(a, dims=2)
2×3 Array{Int64,2}:
 1   2    6
 4  20  120
```
"""
function cumprod(A::AbstractArray; dims::Union{Nothing,Integer}=nothing)
    if dims === nothing
        depwarn("`cumprod(A::AbstractArray)` is deprecated, use `cumprod(A, dims=1)` instead.", :cumprod)
        dims = 1
    end
    return accumulate(mul_prod, A, dims=dims)
end

"""
    cumprod(x::AbstractVector)

Cumulative product of a vector. See also
[`cumprod!`](@ref) to use a preallocated output array, both for performance and
to control the precision of the output (e.g. to avoid overflow).

# Examples
```jldoctest
julia> cumprod(fill(1//2, 3))
3-element Array{Rational{Int64},1}:
 1//2
 1//4
 1//8

julia> cumprod([fill(1//3, 2, 2) for i in 1:3])
3-element Array{Array{Rational{Int64},2},1}:
 [1//3 1//3; 1//3 1//3]
 [2//9 2//9; 2//9 2//9]
 [4//27 4//27; 4//27 4//27]
```
"""
cumprod(x::AbstractVector) = cumprod(x, dims=1)


"""
    accumulate(op, A; dims::Integer)

Cumulative operation `op` along the dimension `dims`. See also
[`accumulate!`](@ref) to use a preallocated output array, both for performance and
to control the precision of the output (e.g. to avoid overflow). For common operations
there are specialized variants of `accumulate`, see:
[`cumsum`](@ref), [`cumprod`](@ref)

# Examples
```jldoctest
julia> accumulate(+, fill(1, 3, 3), dims=1)
3×3 Array{Int64,2}:
 1  1  1
 2  2  2
 3  3  3

julia> accumulate(+, fill(1, 3, 3), dims=2)
3×3 Array{Int64,2}:
 1  2  3
 1  2  3
 1  2  3
```
"""
function accumulate(op, A; dims::Integer)
    out = similar(A, promote_op(op, eltype(A), eltype(A)))
    accumulate!(op, out, A, dims=dims)
end

"""
    accumulate(op, x::AbstractVector)

Cumulative operation `op` on a vector. See also
[`accumulate!`](@ref) to use a preallocated output array, both for performance and
to control the precision of the output (e.g. to avoid overflow). For common operations
there are specialized variants of `accumulate`, see:
[`cumsum`](@ref), [`cumprod`](@ref)

# Examples
```jldoctest
julia> accumulate(+, [1,2,3])
3-element Array{Int64,1}:
 1
 3
 6

julia> accumulate(*, [1,2,3])
3-element Array{Int64,1}:
 1
 2
 6
```
"""
accumulate(op, x::AbstractVector) = accumulate(op, x, dims=1)

"""
    accumulate!(op, B, A; dims::Integer)

Cumulative operation `op` on `A` along the dimension `dims`, storing the result in `B`.
See also [`accumulate`](@ref).

# Examples
```jldoctest
julia> A = [1 2; 3 4];

julia> B = [0 0; 0 0];

julia> accumulate!(-, B, A, dims=1);

julia> B
2×2 Array{Int64,2}:
  1   2
 -2  -2

julia> accumulate!(-, B, A, dims=2);

julia> B
2×2 Array{Int64,2}:
 1  -1
 3  -1
```
"""
function accumulate!(op, B, A; dims::Integer)
    dim = dims
    dim > 0 || throw(ArgumentError("dim must be a positive integer"))
    inds_t = axes(A)
    axes(B) == inds_t || throw(DimensionMismatch("shape of B must match A"))
    dim > ndims(A) && return copyto!(B, A)
    isempty(inds_t[dim]) && return B
    if dim == 1
        # We can accumulate to a temporary variable, which allows
        # register usage and will be slightly faster
        ind1 = inds_t[1]
        @inbounds for I in CartesianIndices(tail(inds_t))
            tmp = reduce_first(op, A[first(ind1), I])
            B[first(ind1), I] = tmp
            for i_1 = first(ind1)+1:last(ind1)
                tmp = op(tmp, A[i_1, I])
                B[i_1, I] = tmp
            end
        end
    else
        R1 = CartesianIndices(axes(A)[1:dim-1])   # not type-stable
        R2 = CartesianIndices(axes(A)[dim+1:end])
        _accumulate!(op, B, A, R1, inds_t[dim], R2) # use function barrier
    end
    return B
end

"""
    accumulate!(op, y, x::AbstractVector)

Cumulative operation `op` on a vector `x`, storing the result in `y`.
See also [`accumulate`](@ref).

# Examples
``jldoctest
julia> x = [1, 0, 2, 0, 3];

julia> y = [0, 0, 0, 0, 0];

julia> accumulate!(+, y, x);

julia> y
5-element Array{Int64,1}:
 1
 1
 3
 3
 6
```
"""
function accumulate!(op::Op, y, x::AbstractVector) where Op
    isempty(x) && return y
    v1 = first(x)
    _accumulate1!(op, y, v1, x, 1)
end

@noinline function _accumulate!(op, B, A, R1, ind, R2)
    # Copy the initial element in each 1d vector along dimension `dim`
    ii = first(ind)
    @inbounds for J in R2, I in R1
        B[I, ii, J] = reduce_first(op, A[I, ii, J])
    end
    # Accumulate
    @inbounds for J in R2, i in first(ind)+1:last(ind), I in R1
        B[I, i, J] = op(B[I, i-1, J], A[I, i, J])
    end
    B
end

"""
    accumulate(op, v0, x::AbstractVector)

Like `accumulate`, but using a starting element `v0`. The first entry of the result will be
`op(v0, first(A))`.

# Examples
```jldoctest
julia> accumulate(+, 100, [1,2,3])
3-element Array{Int64,1}:
 101
 103
 106

julia> accumulate(min, 0, [1,2,-1])
3-element Array{Int64,1}:
  0
  0
 -1
```
"""
function accumulate(op, v0, x::AbstractVector)
    T = promote_op(op, typeof(v0), eltype(x))
    out = similar(x, T)
    accumulate!(op, out, v0, x)
end

function accumulate!(op, y, v0, x::AbstractVector)
    isempty(x) && return y
    v1 = op(v0, first(x))
    _accumulate1!(op, y, v1, x, 1)
end

function _accumulate1!(op, B, v1, A::AbstractVector, dim::Integer)
    dim > 0 || throw(ArgumentError("dim must be a positive integer"))
    inds = LinearIndices(A)
    inds == LinearIndices(B) || throw(DimensionMismatch("LinearIndices of A and B don't match"))
    dim > 1 && return copyto!(B, A)
    i1 = inds[1]
    cur_val = reduce_first(op, v1)
    B[i1] = cur_val
    @inbounds for i in inds[2:end]
        cur_val = op(cur_val, A[i])
        B[i] = cur_val
    end
    return B
end
