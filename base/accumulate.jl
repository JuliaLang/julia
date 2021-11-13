# This file is a part of Julia. License is MIT: https://julialang.org/license

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

Cumulative sum along the dimension `dims`. See also [`cumsum!`](@ref) to use a
preallocated output array, both for performance and to control the precision of
the output (e.g. to avoid overflow).

# Examples
```jldoctest
julia> a = [1 2 3; 4 5 6]
2×3 Matrix{Int64}:
 1  2  3
 4  5  6

julia> cumsum(a, dims=1)
2×3 Matrix{Int64}:
 1  2  3
 5  7  9

julia> cumsum(a, dims=2)
2×3 Matrix{Int64}:
 1  3   6
 4  9  15
```

!!! note
    The return array's `eltype` is `Int` for signed integers of less than system
    word size  and `UInt` for unsigned integers of less than system word size.
    To preserve `eltype` of arrays with small signed or unsigned integer
    `accumulate(+, A)` should be used.

    ```jldoctest
    julia> cumsum(Int8[100, 28])
    2-element Vector{Int64}:
     100
     128

    julia> accumulate(+,Int8[100, 28])
    2-element Vector{Int8}:
      100
     -128
    ```

    In the former case, the integers are widened to system word size and
    therefore the result is `Int64[100, 128]`. In the latter case, no such
    widening happens and integer overflow results in `Int8[100, -128]`.
"""
function cumsum(A::AbstractArray{T}; dims::Integer) where T
    out = similar(A, promote_op(add_sum, T, T))
    cumsum!(out, A, dims=dims)
end

"""
    cumsum(itr)

Cumulative sum of an iterator.

See also [`accumulate`](@ref) to apply functions other than `+`.

!!! compat "Julia 1.5"
    `cumsum` on a non-array iterator requires at least Julia 1.5.

# Examples
```jldoctest
julia> cumsum(1:3)
3-element Vector{Int64}:
 1
 3
 6

julia> cumsum((true, false, true, false, true))
(1, 1, 2, 2, 3)

julia> cumsum(fill(1, 2) for i in 1:3)
3-element Vector{Vector{Int64}}:
 [1, 1]
 [2, 2]
 [3, 3]
```
"""
cumsum(x::AbstractVector) = cumsum(x, dims=1)
cumsum(itr) = accumulate(add_sum, itr)


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
julia> a = Int8[1 2 3; 4 5 6];

julia> cumprod(a, dims=1)
2×3 Matrix{Int64}:
 1   2   3
 4  10  18

julia> cumprod(a, dims=2)
2×3 Matrix{Int64}:
 1   2    6
 4  20  120
```
"""
function cumprod(A::AbstractArray; dims::Integer)
    return accumulate(mul_prod, A, dims=dims)
end

"""
    cumprod(itr)

Cumulative product of an iterator.

See also [`cumprod!`](@ref), [`accumulate`](@ref), [`cumsum`](@ref).

!!! compat "Julia 1.5"
    `cumprod` on a non-array iterator requires at least Julia 1.5.

# Examples
```jldoctest
julia> cumprod(fill(1//2, 3))
3-element Vector{Rational{Int64}}:
 1//2
 1//4
 1//8

julia> cumprod((1, 2, 1, 3, 1))
(1, 2, 2, 6, 6)

julia> cumprod("julia")
5-element Vector{String}:
 "j"
 "ju"
 "jul"
 "juli"
 "julia"
```
"""
cumprod(x::AbstractVector) = cumprod(x, dims=1)
cumprod(itr) = accumulate(mul_prod, itr)


"""
    accumulate(op, A; dims::Integer, [init])

Cumulative operation `op` along the dimension `dims` of `A` (providing `dims` is optional
for vectors). An initial value `init` may optionally be provided by a keyword argument. See
also [`accumulate!`](@ref) to use a preallocated output array, both for performance and
to control the precision of the output (e.g. to avoid overflow).

For common operations there are specialized variants of `accumulate`,
see [`cumsum`](@ref), [`cumprod`](@ref). For a lazy version, see
[`Iterators.accumulate`](@ref).

!!! compat "Julia 1.5"
    `accumulate` on a non-array iterator requires at least Julia 1.5.

# Examples
```jldoctest
julia> accumulate(+, [1,2,3])
3-element Vector{Int64}:
 1
 3
 6

julia> accumulate(min, (1, -2, 3, -4, 5), init=0)
(0, -2, -2, -4, -4)

julia> accumulate(/, (2, 4, Inf), init=100)
(50.0, 12.5, 0.0)

julia> accumulate(=>, i^2 for i in 1:3)
3-element Vector{Any}:
          1
        1 => 4
 (1 => 4) => 9

julia> accumulate(+, fill(1, 3, 4))
3×4 Matrix{Int64}:
 1  4  7  10
 2  5  8  11
 3  6  9  12

julia> accumulate(+, fill(1, 2, 5), dims=2, init=100.0)
2×5 Matrix{Float64}:
 101.0  102.0  103.0  104.0  105.0
 101.0  102.0  103.0  104.0  105.0
```
"""
function accumulate(op, A; dims::Union{Nothing,Integer}=nothing, kw...)
    if dims === nothing && !(A isa AbstractVector)
        # This branch takes care of the cases not handled by `_accumulate!`.
        return collect(Iterators.accumulate(op, A; kw...))
    end
    nt = values(kw)
    if isempty(kw)
        out = similar(A, promote_op(op, eltype(A), eltype(A)))
    elseif keys(nt) === (:init,)
        out = similar(A, promote_op(op, typeof(nt.init), eltype(A)))
    else
        throw(ArgumentError("acccumulate does not support the keyword arguments $(setdiff(keys(nt), (:init,)))"))
    end
    accumulate!(op, out, A; dims=dims, kw...)
end

function accumulate(op, xs::Tuple; init = _InitialValue())
    rf = BottomRF(op)
    ys, = afoldl(((), init), xs...) do (ys, acc), x
        acc = rf(acc, x)
        (ys..., acc), acc
    end
    return ys
end

"""
    accumulate!(op, B, A; [dims], [init])

Cumulative operation `op` on `A` along the dimension `dims`, storing the result in `B`.
Providing `dims` is optional for vectors.  If the keyword argument `init` is given, its
value is used to instantiate the accumulation.

See also [`accumulate`](@ref), [`cumsum!`](@ref), [`cumprod!`](@ref).

# Examples
```jldoctest
julia> x = [1, 0, 2, 0, 3];

julia> y = rand(5);

julia> accumulate!(+, y, x);

julia> y
5-element Vector{Float64}:
 1.0
 1.0
 3.0
 3.0
 6.0

julia> A = [1 2 3; 4 5 6];

julia> B = similar(A);

julia> accumulate!(-, B, A, dims=1)
2×3 Matrix{Int64}:
  1   2   3
 -3  -3  -3

julia> accumulate!(*, B, A, dims=2, init=10)
2×3 Matrix{Int64}:
 10   20    60
 40  200  1200
```
"""
function accumulate!(op, B, A; dims::Union{Integer, Nothing} = nothing, kw...)
    nt = values(kw)
    if isempty(kw)
        _accumulate!(op, B, A, dims, nothing)
    elseif keys(kw) === (:init,)
        _accumulate!(op, B, A, dims, Some(nt.init))
    else
        throw(ArgumentError("acccumulate! does not support the keyword arguments $(setdiff(keys(nt), (:init,)))"))
    end
end

function _accumulate!(op, B, A, dims::Nothing, init::Union{Nothing, Some})
    throw(ArgumentError("Keyword argument dims must be provided for multidimensional arrays"))
end

function _accumulate!(op, B, A::AbstractVector, dims::Nothing, init::Nothing)
    isempty(A) && return B
    v1 = reduce_first(op, first(A))
    _accumulate1!(op, B, v1, A, 1)
end

function _accumulate!(op, B, A::AbstractVector, dims::Nothing, init::Some)
    isempty(A) && return B
    v1 = op(something(init), first(A))
    _accumulate1!(op, B, v1, A, 1)
end

function _accumulate!(op, B, A, dims::Integer, init::Union{Nothing, Some})
    dims > 0 || throw(ArgumentError("dims must be a positive integer"))
    inds_t = axes(A)
    axes(B) == inds_t || throw(DimensionMismatch("shape of B must match A"))
    dims > ndims(A) && return copyto!(B, A)
    isempty(inds_t[dims]) && return B
    if dims == 1
        # We can accumulate to a temporary variable, which allows
        # register usage and will be slightly faster
        ind1 = inds_t[1]
        @inbounds for I in CartesianIndices(tail(inds_t))
            if init === nothing
                tmp = reduce_first(op, A[first(ind1), I])
            else
                tmp = op(something(init), A[first(ind1), I])
            end
            B[first(ind1), I] = tmp
            for i_1 = first(ind1)+1:last(ind1)
                tmp = op(tmp, A[i_1, I])
                B[i_1, I] = tmp
            end
        end
    else
        R1 = CartesianIndices(axes(A)[1:dims-1])   # not type-stable
        R2 = CartesianIndices(axes(A)[dims+1:end])
        _accumulaten!(op, B, A, R1, inds_t[dims], R2, init) # use function barrier
    end
    return B
end

@noinline function _accumulaten!(op, B, A, R1, ind, R2, init::Nothing)
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

@noinline function _accumulaten!(op, B, A, R1, ind, R2, init::Some)
    # Copy the initial element in each 1d vector along dimension `dim`
    ii = first(ind)
    @inbounds for J in R2, I in R1
        B[I, ii, J] = op(something(init), A[I, ii, J])
    end
    # Accumulate
    @inbounds for J in R2, i in first(ind)+1:last(ind), I in R1
        B[I, i, J] = op(B[I, i-1, J], A[I, i, J])
    end
    B
end

function _accumulate1!(op, B, v1, A::AbstractVector, dim::Integer)
    dim > 0 || throw(ArgumentError("dim must be a positive integer"))
    inds = LinearIndices(A)
    inds == LinearIndices(B) || throw(DimensionMismatch("LinearIndices of A and B don't match"))
    dim > 1 && return copyto!(B, A)
    (i1, state) = iterate(inds)::NTuple{2,Any} # We checked earlier that A isn't empty
    cur_val = v1
    B[i1] = cur_val
    next = iterate(inds, state)
    @inbounds while next !== nothing
        (i, state) = next
        cur_val = op(cur_val, A[i])
        B[i] = cur_val
        next = iterate(inds, state)
    end
    return B
end
