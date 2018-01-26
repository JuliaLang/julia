# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    Accumulate(op[, v0], iter)

An iterator which cumulatively applies the binary operation `op` to the iterator `iter`, in a similar manner
to [`foldl`](@ref) but returning intermediate values.

If an initial `v0` value is provided, then the first value of the iterator will be

    op(v0, first(iter))

Otherwise it will be

    Base.reduce_first(op, first(iter))

See also [`Base.reduce_first`](@ref) and [`accumulate`](@ref).
"""
struct Accumulate{O,V,I}
    op::O
    v0::V
    iter::I
end
Accumulate(op, iter) = Accumulate(op, uninitialized, iter) # use `uninitialized` as a sentinel

# the following is largely based on Generator objects
Base.IteratorEltype(acc::Accumulate) = EltypeUnknown()
Base.IteratorSize(acc::Accumulate) = Base.IteratorSize(acc.iter)

length(acc::Accumulate) = length(acc.iter)
size(acc::Accumulate)   = size(acc.iter)
axes(acc::Accumulate)   = axes(acc.iter)
ndims(acc::Accumulate)  = ndims(acc.iter)

start(acc::Accumulate) = (@_inline_meta; (start(acc.iter), acc.v0))
done(acc::Accumulate, accstate) = (@_inline_meta; done(acc.iter, accstate[1]))
function next(acc::Accumulate, accstate)
    @_inline_meta
    itrstate, accval = accstate
    val, itrstate = next(acc.iter, itrstate)
    if accval === uninitialized
        accval = reduce_first(acc.op, val)
        return accval, (itrstate, accval)
    else
        accval = acc.op(accval, val)
        return accval, (itrstate, accval)
    end
end

accumulate(op, itr) = collect(Accumulate(op, itr))
accumulate(op, v0, itr) = collect(Accumulate(op, v0, itr))


# TODO
# below was copied directly from old code in multidimensional.jl

# see discussion in #18364 ... we try not to widen type of the resulting array
# from cumsum or cumprod, but in some cases (+, Bool) we may not have a choice.
rcum_promote_type(op, ::Type{T}, ::Type{S}) where {T,S<:Number} = promote_op(op, T, S)
rcum_promote_type(op, ::Type{T}) where {T<:Number} = rcum_promote_type(op, T,T)
rcum_promote_type(op, ::Type{T}) where {T} = T

# handle sums of Vector{Bool} and similar.   it would be nice to handle
# any AbstractArray here, but it's not clear how that would be possible
rcum_promote_type(op, ::Type{Array{T,N}}) where {T,N} = Array{rcum_promote_type(op,T), N}

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
    li = linearindices(v)
    li != linearindices(result) && throw(DimensionMismatch("input and output array sizes and indices must match"))
    n = length(li)
    n == 0 && return result
    i1 = first(li)
    @inbounds result[i1] = v1 = v[i1]
    n == 1 && return result
    _accumulate_pairwise!(op, result, v, v1, i1+1, n-1)
    return result
end

function accumulate_pairwise(op, v::AbstractVector{T}) where T
    out = similar(v, rcum_promote_type(op, T))
    return accumulate_pairwise!(op, out, v)
end

function cumsum!(out, v::AbstractVector, dim::Integer)
    # we dispatch on the possibility of numerical stability issues
    _cumsum!(out, v, dim, ArithmeticStyle(eltype(out)))
end

function _cumsum!(out, v, dim, ::ArithmeticRounds)
    dim == 1 ? accumulate_pairwise!(+, out, v) : copyto!(out, v)
end
function _cumsum!(out, v, dim, ::ArithmeticUnknown)
    _cumsum!(out, v, dim, ArithmeticRounds())
end
function _cumsum!(out, v, dim, ::ArithmeticStyle)
    dim == 1 ? accumulate!(+, out, v) : copyto!(out, v)
end

"""
    cumsum(A, dim::Integer)

Cumulative sum along the dimension `dim`. See also [`cumsum!`](@ref)
to use a preallocated output array, both for performance and to control the precision of the
output (e.g. to avoid overflow).

# Examples
```jldoctest
julia> a = [1 2 3; 4 5 6]
2×3 Array{Int64,2}:
 1  2  3
 4  5  6

julia> cumsum(a,1)
2×3 Array{Int64,2}:
 1  2  3
 5  7  9

julia> cumsum(a,2)
2×3 Array{Int64,2}:
 1  3   6
 4  9  15
```
"""
function cumsum(A::AbstractArray{T}, dim::Integer) where T
    out = similar(A, rcum_promote_type(+, T))
    cumsum!(out, A, dim)
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
cumsum(x::AbstractVector) = cumsum(x, 1)

"""
    cumsum!(B, A, dim::Integer)

Cumulative sum of `A` along the dimension `dim`, storing the result in `B`. See also [`cumsum`](@ref).
"""
cumsum!(B, A, dim::Integer) = accumulate!(+, B, A, dim)

"""
    cumsum!(y::AbstractVector, x::AbstractVector)

Cumulative sum of a vector `x`, storing the result in `y`. See also [`cumsum`](@ref).
"""
cumsum!(y::AbstractVector, x::AbstractVector) = cumsum!(y, x, 1)

"""
    cumprod(A, dim::Integer)

Cumulative product along the dimension `dim`. See also
[`cumprod!`](@ref) to use a preallocated output array, both for performance and
to control the precision of the output (e.g. to avoid overflow).

# Examples
```jldoctest
julia> a = [1 2 3; 4 5 6]
2×3 Array{Int64,2}:
 1  2  3
 4  5  6

julia> cumprod(a,1)
2×3 Array{Int64,2}:
 1   2   3
 4  10  18

julia> cumprod(a,2)
2×3 Array{Int64,2}:
 1   2    6
 4  20  120
```
"""
cumprod(A::AbstractArray, dim::Integer) = accumulate(*, A, dim)

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
 Rational{Int64}[1//3 1//3; 1//3 1//3]
 Rational{Int64}[2//9 2//9; 2//9 2//9]
 Rational{Int64}[4//27 4//27; 4//27 4//27]
```
"""
cumprod(x::AbstractVector) = cumprod(x, 1)

"""
    cumprod!(B, A, dim::Integer)

Cumulative product of `A` along the dimension `dim`, storing the result in `B`.
See also [`cumprod`](@ref).
"""
cumprod!(B, A, dim::Integer) = accumulate!(*, B, A, dim)

"""
    cumprod!(y::AbstractVector, x::AbstractVector)

Cumulative product of a vector `x`, storing the result in `y`.
See also [`cumprod`](@ref).
"""
cumprod!(y::AbstractVector, x::AbstractVector) = cumprod!(y, x, 1)

"""
    accumulate(op, A, dim::Integer)

Cumulative operation `op` along the dimension `dim`. See also
[`accumulate!`](@ref) to use a preallocated output array, both for performance and
to control the precision of the output (e.g. to avoid overflow). For common operations
there are specialized variants of `accumulate`, see:
[`cumsum`](@ref), [`cumprod`](@ref)

# Examples
```jldoctest
julia> accumulate(+, fill(1, 3, 3), 1)
3×3 Array{Int64,2}:
 1  1  1
 2  2  2
 3  3  3

julia> accumulate(+, fill(1, 3, 3), 2)
3×3 Array{Int64,2}:
 1  2  3
 1  2  3
 1  2  3
```
"""
function accumulate(op, A, dim::Integer)
    out = similar(A, rcum_promote_type(op, eltype(A)))
    accumulate!(op, out, A, dim)
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
accumulate(op, x::AbstractVector) = accumulate(op, x, 1)

"""
    accumulate!(op, B, A, dim::Integer)

Cumulative operation `op` on `A` along the dimension `dim`, storing the result in `B`.
See also [`accumulate`](@ref).

# Examples
```jldoctest
julia> A = [1 2; 3 4];

julia> B = [0 0; 0 0];

julia> accumulate!(-, B, A, 1);

julia> B
2×2 Array{Int64,2}:
  1   2
 -2  -2

julia> accumulate!(-, B, A, 2);

julia> B
2×2 Array{Int64,2}:
 1  -1
 3  -1
```
"""
function accumulate!(op, B, A, dim::Integer)
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
            tmp = convert(eltype(B), A[first(ind1), I])
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
        B[I, ii, J] = A[I, ii, J]
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
    T = rcum_promote_type(op, typeof(v0), eltype(x))
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
    inds = linearindices(A)
    inds == linearindices(B) || throw(DimensionMismatch("linearindices of A and B don't match"))
    dim > 1 && return copyto!(B, A)
    i1 = inds[1]
    cur_val = v1
    B[i1] = cur_val
    @inbounds for i in inds[2:end]
        cur_val = op(cur_val, A[i])
        B[i] = cur_val
    end
    return B
end
