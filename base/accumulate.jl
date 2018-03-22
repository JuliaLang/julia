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
Accumulate(op, iter) = Accumulate(op, undef, iter) # use `undef` as a sentinel

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

    accval2 = reduce_first(acc.op, accval, val)
    return accval2, (itrstate, accval2)
end


"""
    accumulate(op[, v0], itr; dims::Integer)

Cumulative apply binary operation `op` on an iterable `itr`, along the dimension
`dims`. If an initial `v0` value is provided, then the first value of the iterator will be

    op(v0, first(iter))

Otherwise it will use

    Base.reduce_first(op, first(iter))

The keyword argument `dims` is optional for 1-dimensional iterators.

See also:
 - [`accumulate!`](@ref) to use a preallocated output array,
 - [`cumsum`](@ref) and [`cumprod`](@ref) for specialized versions.

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

julia> accumulate(+, fill(1, 3, 3); dims=1)
3×3 Array{Int64,2}:
 1  1  1
 2  2  2
 3  3  3

julia> accumulate(+, fill(1, 3, 3); dims=2)
3×3 Array{Int64,2}:
 1  2  3
 1  2  3
 1  2  3
```
"""
@inline accumulate(op, itr; dims::Union{Nothing,Integer}=nothing) = accumulate(op, undef, itr; dims=dims)
@inline accumulate(op, v0, itr; dims::Union{Nothing,Integer}=nothing) = _accumulate(op, v0, itr, IteratorSize(itr), dims)

@inline function _accumulate(op, v0, itr, ::Union{SizeUnknown,HasLength,IsInfinite,HasShape{1}}, ::Nothing)
    collect(Accumulate(op, v0, itr))
end


"""
    accumulate!(op, dest[, v0], itr; dims::Integer)

Cumulative operation `op` on an iterator `itr`, storing the result in `dest`, along the
dimension `dims` (optional for 1-dimensional iterators).

See also:
 - [`accumulate`](@ref) for the non-inplace version
 - [`cumsum!`](@ref) and [`cumprod!`](@ref)

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

julia> A = [1 2; 3 4];

julia> B = [0 0; 0 0];

julia> accumulate!(B, -, A; dims=1);

julia> B
2×2 Array{Int64,2}:
  1   2
 -2  -2

julia> accumulate!(-, B, A; dims=2);

julia> B
2×2 Array{Int64,2}:
 1  -1
 3  -1
```
"""
@inline accumulate!(op, dest, itr; dims::Union{Nothing,Integer}=nothing) = accumulate!(op, dest, undef, itr; dims=dims)
@inline accumulate!(op, dest, v0, itr; dims::Union{Nothing,Integer}=nothing) = _accumulate!(op, dest, undef, itr, IteratorSize(itr), dims)

function _accumulate!(op::F, dest, v0, x, ::Union{SizeUnknown,HasLength,IsInfinite,HasShape{1}}, ::Nothing) where F
    src = Accumulate(op, v0, x)

    # this is essentially `copyto!`, but unrolled to deal with potential type instability in first state
    # hopefully won't be necessary with better Union splitting
    destiter = eachindex(dest)

    sstate0 = start(src)
    dstate = start(destiter)
    sdone = done(src, sstate0)
    ddone = done(destiter, dstate)

    sdone && ddone && return dest
    (sdone || ddone) && throw(DimensionMismatch("input and output array sizes and indices must match"))

    i, dstate = next(destiter, dstate)
    s, sstate = next(src, sstate0)
    while true
        @inbounds dest[i] = s

        sdone = done(src, sstate)
        ddone = done(destiter, dstate)

        sdone && ddone && return dest
        (sdone || ddone) && throw(DimensionMismatch("input and output array sizes and indices must match"))

        i, dstate = next(destiter, dstate)
        s, sstate = next(src, sstate)
    end
end

function _accumulate(op::F, v0, X, ::HasShape{N}, dim::Integer) where {F,N}
    dim > 0 || throw(ArgumentError("dim must be a positive integer"))
    if isempty(X)
        # fallback on collect machinery
        return collect(Accumulate(op, v0, X))
    end
    inds = CartesianIndices(axes(X))
    first_in_dim = first(axes(X, dim))
    dim_delta = CartesianIndex(ntuple(d -> d==dim ? 1 : 0, Val{N}()))
    st = start(inds)
    i, st = next(inds, st)
    v = reduce_first(op, v0, X[i])
    dest = similar(X, typeof(v))
    dest[i] = v
    return _accumulate!(op, dest, v0, X, dim, inds, st, first_in_dim, dim_delta, true)
end

function _accumulate!(op::F, dest, v0, X, ::HasShape{N}, dim::Integer) where {F,N}
    dim > 0 || throw(ArgumentError("dim must be a positive integer"))
    axes(dest) == axes(X) || throw(DimensionMismatch("shape of source and destination must match"))
    inds = CartesianIndices(axes(X))
    first_in_dim = first(axes(X, dim))
    dim_delta = CartesianIndex(ntuple(d -> d==dim ? 1 : 0, Val{N}()))
    st = start(inds)
    return _accumulate!(op, dest, v0, X, dim, inds, st, first_in_dim, dim_delta, false)
end

function _accumulate!(op::F, dest::AbstractArray{T}, v0, X, dim, inds, st, first_in_dim, dim_delta, widen) where {F,T}
    while !done(inds, st)
        i, st = next(inds, st)
        if dim > length(i) || i[dim] == first_in_dim
            v = reduce_first(op, v0, @inbounds X[i])
        else
            v = op(@inbounds(dest[i - dim_delta]), @inbounds(X[i]))
        end
        if !widen || v isa T
            @inbounds dest[i] = v
        else
            R = promote_typejoin(T, typeof(v))
            dest´ = similar(dest, R)
            copyto!(dest´, 1, dest, 1, linearindices(dest)[i]-1)
            @inbounds dest´[i] = v
            return _accumulate!(op, dest´, v0, X, dim, inds, st, first_in_dim, dim_delta, widen)
        end
    end
    return dest
end



"""
    accumulate_pairwise(op[, v0], itr)

Similar to [`accumulate`](@ref), but performs accumulation using a divide-and-conquer
approach, which can be more numerically accurate for certain operations, such as
[`+`](@ref).

It involves roughly double the number of `op` calls, but for cheap operations like `+`
this does not have much impact (approximately 20%).
"""
@inline accumulate_pairwise(op, itr) = accumulate_pairwise(op, undef, itr)
@inline accumulate_pairwise(op, v0, itr) =  accumulate_pairwise(op, v0, itr, IteratorSize(itr))
function accumulate_pairwise(op::F, v0, itr, ::Union{HasLength,HasShape{1}}) where F
    i = start(itr)
    if done(itr,i)
        return collect(Accumulate(op, v0, itr))
    end
    v1,i = next(itr,i)
    y = reduce_first(op,v0,v1)

    Y = _similar_for(1:1, typeof(y), itr, IteratorSize(itr))
    L = linearindices(Y)
    n = length(L)
    j = first(L)

    while true
        Y[j] = y
        if done(itr,i)
            return Y
        end
        y,j,i,wider = _accumulate_pairwise!(op,Y,itr,y,j+1,i,last(L)-j,true)
        if !wider
            return Y
        end
        R = promote_typejoin(eltype(Y), typeof(y))
        newY = similar(Y, R)
        copyto!(newY,1,Y,1,j)
        Y = newY
    end
end

@inline accumulate_pairwise!(op, dest, itr) = accumulate_pairwise!(op, dest, undef, itr)
function accumulate_pairwise!(op::F, Y, v0, itr) where F
    L = linearindices(Y)
    L == linearindices(itr) || throw(DimensionMismatch("indices of source and destination must match"))

    n = length(L)

    i = start(itr)
    v1,i = next(itr,i)

    j = first(L)
    Y[j] = y = reduce_first(op,v0,v1)
    _accumulate_pairwise!(op,Y,itr,y,j+1,i,n-1,false)
    return Y
end

function _accumulate_pairwise!(op::F,Y,X,y0,j,i,m,widen) where F
    if m < 128 # m >= 1
        @inbounds begin
            x,i = next(X,i)
            y = op(y0, x)
            if widen && !isa(y, eltype(Y))
                return y, j, i, true
            end
            Y[j] = y

            # to keep type stability, could also unroll loop?
            w = reduce_first(op, x)
            for k = 1:m-1
                j += 1
                x,i = next(X,i)
                w = op(w,  x)
                y = op(y0, w)
                if widen && !isa(y, eltype(Y))
                    return y, j, i, true
                end
                Y[j] = y
            end
            return w, j+1, i, false
        end
    else
        m1 = m >> 1
        v1, j, i, wider = _accumulate_pairwise!(op,Y,X,y0,j,i,m1,widen)
        wider && return v1, j, i, wider
        v2, j, i, wider = _accumulate_pairwise!(op,Y,X,op(y0,v1),j,i,m-m1,widen)
        wider && return v1, j, i, wider
        return op(v1, v2), j, i, false
    end
end



"""
    cumsum(A; dims::Integer)

Cumulative sum `A` along the dimension `dims` (`dims` is option for 1-dimensional
objects). This has the same promotion behaviour as [`sum`](@ref), namely that lower-width
integer types are promoted to `Int`/`UInt`.

See also:
 - [`cumsum!`](@ref) to use a preallocated output array, both for performance and
to control the precision of the output (e.g. to avoid overflow).
 - [`accumulate`](@ref) for accumulation using other operators.

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

julia> a = [1 2 3; 4 5 6]
2×3 Array{Int64,2}:
 1  2  3
 4  5  6

julia> cumsum(a; dims=1)
2×3 Array{Int64,2}:
 1  2  3
 5  7  9

julia> cumsum(a; dims=2)
2×3 Array{Int64,2}:
 1  3   6
 4  9  15
```
"""
function cumsum(A; dims::Union{Nothing,Integer}=nothing)
    if A isa AbstractArray && ndims(A) > 1 && dims === nothing
        depwarn("`cumsum(A::AbstractArray)` is deprecated, use `cumsum(A; dims=1)` instead.", :cumsum)
        dims = 1
    end
    accumulate(add_sum, A; dims=dims)
end

function cumsum(v::AbstractVector{T}) where T
    # we dispatch on the possibility of numerical accuracy issues
    _cumsum(v, ArithmeticStyle(T))
end
_cumsum(v::AbstractVector, ::ArithmeticRounds)  = accumulate_pairwise(add_sum, v)
_cumsum(v::AbstractVector, ::ArithmeticUnknown) = accumulate_pairwise(add_sum, v)
_cumsum(v::AbstractVector, ::ArithmeticStyle)   = accumulate(add_sum, v)


"""
    Base.ConvertOp{T}(op)(x,y)

An operator which converts `x` and `y` to type `T` before performing the `op`.

The main purpose is for use in [`cumsum!`](@ref) and [`cumprod!`](@ref), where `T` is determined by the output array.
"""
struct ConvertOp{T,O} <: Function
    op::O
end
ConvertOp{T}(op::O) where {T,O} = ConvertOp{T,O}(op)
(c::ConvertOp{T})(x,y) where {T} = c.op(convert(T,x),convert(T,y))
reduce_first(c::ConvertOp{T},x) where {T} = reduce_first(c.op, convert(T,x))




"""
    cumsum!(B, A; dims::Integer)

Cumulative sum of `A` along the dimension `dims`, storing the result in `B`. `dims` is
optional for 1-dimensional objects.

Elements of `A` are first converted to the element type of `B` before addition, so as to
improve accuracy and reduce occurences of arithemtic overflow.

See also [`cumsum`](@ref).
"""
cumsum!(dest::AbstractArray{T}, A; dims::Union{Nothing,Integer}=nothing) where {T} =
    accumulate!(ConvertOp{T}(+), dest, A; dims=dims)

function cumsum!(out::AbstractVector, v::AbstractVector{T}) where T
    # we dispatch on the possibility of numerical accuracy issues
    cumsum!(out, v, ArithmeticStyle(T))
end
cumsum!(out::AbstractVector{T}, v::AbstractVector, ::ArithmeticRounds) where {T} =
    accumulate_pairwise!(ConvertOp{T}(+), out, v)
cumsum!(out::AbstractVector{T}, v::AbstractVector, ::ArithmeticUnknown) where {T} =
    accumulate_pairwise!(ConvertOp{T}(+), out, v)
cumsum!(out::AbstractVector{T}, v::AbstractVector, ::ArithmeticStyle) where {T} =
    accumulate!(ConvertOp{T}(+), out, v)


"""
    cumprod(A; dims::Integer)

Cumulative product along the dimension `dims` (`dims` is optional for 1-dimensional objects).

See also:
 - [`cumprod!`](@ref) to use a preallocated output array, both for performance and
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

julia> a = [1 2 3; 4 5 6]
2×3 Array{Int64,2}:
 1  2  3
 4  5  6

julia> cumprod(a; dims=1)
2×3 Array{Int64,2}:
 1   2   3
 4  10  18

julia> cumprod(a; dims=2)
2×3 Array{Int64,2}:
 1   2    6
 4  20  120
```
"""
function cumprod(A; dims::Union{Nothing,Integer}=nothing)
    if A isa AbstractArray && ndims(A) > 1 && dims === nothing
        depwarn("`cumprod(A::AbstractArray)` is deprecated, use `cumprod(A; dims=1)` instead.", :cumprod)
        dims = 1
    end
    accumulate(mul_prod, A; dims=dims)
end

"""
    cumprod!(B, A; dims::Integer)

Cumulative product of `A` along the dimension `dims`, storing the result in `B`. `dims` is
optional for 1-dimensional objects.

Elements of `A` are first converted to the element type of `B` before multiplication, so
as to improve accuracy and reduce occurences of arithemtic overflow.

See also [`cumprod`](@ref).
"""
cumprod!(dest::AbstractArray{T}, A; dims::Union{Nothing,Integer}=nothing) where {T} =
    accumulate!(ConvertOp{T}(*), dest, A; dims=dims)
