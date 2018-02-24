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

    accval2 = reduce_first(acc.op, accval, val)
    return accval2, (itrstate, accval2)
end


"""
    accumulate(op[, v0], itr)

Cumulative apply binary operation `op` on an iterable `itr`. If an initial `v0` value is
provided, then the first value of the iterator will be

    op(v0, first(iter))

Otherwise it will use

    Base.reduce_first(op, first(iter))

See also: [`accumulate!`](@ref) to use a preallocated output array, and [`cumsum`](@ref),
[`cumprod`](@ref) for specialized versions.

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
```
"""
accumulate(op, itr) = accumulate(op, uninitialized, itr)
accumulate(op, v0, itr) = accumulate(op, v0, itr, IteratorSize(itr))
accumulate(op, v0, itr, ::Union{SizeUnknown,HasLength,IsInfinite,HasShape{1}}) =
    collect(Accumulate(op, v0, itr))


"""
    accumulate!(op, dest,[, v0] itr)

Cumulative operation `op` on an iterator `itr`, storing the result in `dest`.
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
accumulate!(op, dest, x) = accumulate!(op, dest, uninitialized, x)
function accumulate!(op, dest, v0, x)
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


"""
    accumulate(op[, v0], A, dim::Integer)

Cumulative operation `op` along the dimension `dim`. See also
[`accumulate!`](@ref) to use a preallocated output array. For common operations
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
accumulate(op, X, dim::Int) = accumulate(op, uninitialized, X, dim)

# split into (head, slice, tail) dimension iterators
@inline function split_dimensions(X,dim::Integer)
    inds = axes(X)
    if dim > length(inds)
        (CartesianIndices(inds), CartesianIndices(), CartesianIndices())
    else
        (CartesianIndices(inds[1:dim-1]), inds[dim], CartesianIndices(inds[dim+1:end]))
    end
end

function accumulate(op, v0, X, dim::Integer)
    dim > 0 || throw(ArgumentError("dim must be a positive integer"))
    if length(X) == 0
        # fallback on collect machinery
        return collect(Accumulate(op, v0, X))
    end
    indH, indD, indT = split_dimensions(X,dim)

    # TODO: this could be much nicer with the new iteration protocol
    # unroll loops to get first element
    sH = start(indH)
    @assert !done(indH, sH)
    iH,sH = next(indH, sH)

    sD = start(indD)
    @assert !done(indD, sD)
    iD,sD = next(indD, sD)
    pD = iD

    sT = start(indT)
    @assert !done(indT, sT)
    iT,sT = next(indT, sT)

    # first element
    accv = reduce_first(op, v0, X[iH, iD, iT])
    dest = similar(X, typeof(accv))
    i = first(linearindices(dest))
    dest[i] = accv

    return _accumulate!(op, dest, i, v0, X, indH, indD, indT, sH, sD, sT, iH, iD, iT, pD)
end


"""
    accumulate!(op, dest, A, dim::Integer)

Cumulative operation `op` on `A` along the dimension `dim`, storing the result in `B`.
See also [`accumulate`](@ref).

# Examples
```jldoctest
julia> A = [1 2; 3 4];

julia> B = [0 0; 0 0];

julia> accumulate!(B, -, A, 1);

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
accumulate!(op, dest, X, dim::Integer) = accumulate!(op, dest, uninitialized, X, dim::Integer)

function accumulate!(op, dest, v0, X, dim::Integer)
    dim > 0 || throw(ArgumentError("dim must be a positive integer"))
    axes(dest) == axes(X) || throw(DimensionMismatch("shape of source and destination must match"))
    indH, indD, indT = split_dimensions(X,dim)

    sH = start(indH)
    @assert !done(indH, sH)
    iH,sH = next(indH, sH)

    sD = start(indD)
    @assert !done(indD, sD)
    iD,sD = next(indD, sD)
    pD = iD

    sT = start(indT)
    @assert !done(indT, sT)
    iT,sT = next(indT, sT)

    return _accumulate!(op, dest, first(linearindices(dest))-1, v0, X, indH, indD, indT, sH, sD, sT, iH, iD, iT, pD, false)
end

function _accumulate!(op, dest::AbstractArray{T}, i, v0, X, indH, indD, indT, sH, sD, sT, iH, iD, iT, pD, widen=true) where {T}
    while true
        if done(indH,sH)
            if done(indD, sD)
                if done(indT,sT)
                    return dest
                else
                    iT, sT = next(indT, sT)
                end
                iD, sD = next(indD, start(indD))
                pD = iD
            else
                pD = iD
                iD, sD = next(indD, sD)
            end
            iH, sH = next(indH, start(indH))
        else
            iH, sH = next(indH, sH)
        end

        if iD == pD
            accv = reduce_first(op, v0, X[iH, iD, iT])
        else
            paccv = isempty(indH) ? accv : @inbounds dest[iH,pD,iT]
            accv = op(paccv, X[iH,iD,iT])
        end

        S = typeof(accv)
        if !widen || S === T || S <: T
            @inbounds dest[i+=1] = accv
        else
            R = promote_typejoin(T, S)
            new = similar(dest, R)
            copyto!(new,1,dest,1,i)
            @inbounds new[i+=1] = accv
            return _accumulate!(op, dest, i, v0, X, indH, indD, indT, sH, sD, sT, iH, iD, iT, pD)
        end
    end
end

"""
    accumulate_pairwise(op[, v0], itr)

Similar to [`accumulate`](@ref), but performs accumulation using a divide-and-conquer
approach, which can be more numerically accurate for certain operations, such as
[`+`](@ref).

It involves roughly double the number of `op` calls, but for cheap operations like `+`
this does not have much impact (approximately 20%).
"""
accumulate_pairwise(op, itr) = accumulate_pairwise(op, uninitialized, itr)
accumulate_pairwise(op, v0, itr) =  accumulate_pairwise(op, v0, itr, IteratorSize(itr))
function accumulate_pairwise(op, v0, itr, ::Union{HasLength,HasShape{1}})
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
        y,j,i,wider = _accum!(op,Y,itr,y,j+1,i,last(L)-j,true)
        if !wider
            return Y
        end
        R = promote_typejoin(eltype(Y), typeof(y))
        newY = similar(Y, R)
        copyto!(newY,1,Y,1,j)
        Y = newY
    end
end

accumulate_pairwise!(op, dest, itr) = accumulate_pairwise!(op, dest, uninitialized, itr)
function accumulate_pairwise!(op, Y, v0, itr)
    L = linearindices(Y)
    L == linearindices(itr) || throw(DimensionMismatch("indices of source and destination must match"))

    n = length(L)

    i = start(itr)
    v1,i = next(itr,i)

    j = first(L)
    Y[j] = y = reduce_first(op,v0,v1)
    _accum!(op,Y,itr,y,j+1,i,n-1,false)
    return Y
end

function _accumulate_pairwise!(op,Y,X,y0,j,i,m,widen)
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
        v1, j, i, wider = _accum!(op,Y,X,y0,j,i,m1,widen)
        wider && return v1, j, i, wider
        v2, j, i, wider = _accum!(op,Y,X,op(y0,v1),j,i,m-m1,widen)
        wider && return v1, j, i, wider
        return op(v1, v2), j, i, false
    end
end

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
cumsum(A, dim::Integer) = accumulate(add_sum, A, dim)

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
function cumsum(v::AbstractVector{T}) where T
    # we dispatch on the possibility of numerical accuracy issues
    cumsum(v, ArithmeticStyle(T))
end
cumsum(v::AbstractVector, ::ArithmeticRounds)  = accumulate_pairwise(add_sum, v)
cumsum(v::AbstractVector, ::ArithmeticUnknown) = accumulate_pairwise(add_sum, v)
cumsum(v::AbstractVector, ::ArithmeticStyle)   = accumulate(add_sum, v)

"""
    cumsum!(B, A, dim::Integer)

Cumulative sum of `A` along the dimension `dim`, storing the result in `B`. See also [`cumsum`](@ref).
"""
cumsum!(dest::AbstractArray{T}, A, dim::Integer) where {T} = accumulate!(ConvertOp{T}(+), dest, A, dim)

"""
    cumsum!(y::AbstractVector, x::AbstractVector)

Cumulative sum of a vector `x`, storing the result in `y`. See also [`cumsum`](@ref).
"""
cumsum!(dest::AbstractArray{T}, itr) where {T} = accumulate!(ConvertOp{T}(+), dest, src)

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
cumprod(A, dim::Integer) = accumulate(mul_prod, A, dim)

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
cumprod(x) = accumulate(mul_prod, x)
cumprod(x::AbstractVector) = accumulate(mul_prod, x)

"""
    cumprod!(B, A, dim::Integer)

Cumulative product of `A` along the dimension `dim`, storing the result in `B`.
See also [`cumprod`](@ref).
"""
cumprod!(dest::AbstractArray{T}, A, dim::Integer) where {T} = accumulate!(ConvertOp{T}(*), dest, A, dim)

"""
    cumprod!(y::AbstractVector, x::AbstractVector)

Cumulative product of a vector `x`, storing the result in `y`.
See also [`cumprod`](@ref).
"""
cumprod!(dest::AbstractArray{T}, itr) where {T} = accumulate!(ConvertOp{T}(*), dest, itr)
