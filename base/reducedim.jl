# This file is a part of Julia. License is MIT: https://julialang.org/license

## Functions to compute the reduced shape

# for reductions that expand 0 dims to 1
reduced_indices(a::AbstractArray, region) = reduced_indices(axes(a), region)

# for reductions that keep 0 dims as 0
reduced_indices0(a::AbstractArray, region) = reduced_indices0(axes(a), region)

function reduced_indices(inds::Indices{N}, d::Int, rd::AbstractUnitRange) where N
    d < 1 && throw(ArgumentError("dimension must be ≥ 1, got $d"))
    if d == 1
        return (oftype(inds[1], rd), tail(inds)...)
    elseif 1 < d <= N
        return tuple(inds[1:d-1]..., oftype(inds[d], rd), inds[d+1:N]...)::typeof(inds)
    else
        return inds
    end
end
reduced_indices(inds::Indices, d::Int) = reduced_indices(inds, d, OneTo(1))

function reduced_indices0(inds::Indices{N}, d::Int) where N
    d < 1 && throw(ArgumentError("dimension must be ≥ 1, got $d"))
    if d <= N
        ind = inds[d]
        return reduced_indices(inds, d, (isempty(ind) ? ind : OneTo(1)))
    else
        return inds
    end
end

function reduced_indices(inds::Indices{N}, region) where N
    rinds = [inds...]
    for i in region
        isa(i, Integer) || throw(ArgumentError("reduced dimension(s) must be integers"))
        d = Int(i)
        if d < 1
            throw(ArgumentError("region dimension(s) must be ≥ 1, got $d"))
        elseif d <= N
            rinds[d] = oftype(rinds[d], OneTo(1))
        end
    end
    tuple(rinds...)::typeof(inds)
end

function reduced_indices0(inds::Indices{N}, region) where N
    rinds = [inds...]
    for i in region
        isa(i, Integer) || throw(ArgumentError("reduced dimension(s) must be integers"))
        d = Int(i)
        if d < 1
            throw(ArgumentError("region dimension(s) must be ≥ 1, got $d"))
        elseif d <= N
            rind = rinds[d]
            rinds[d] = oftype(rind, (isempty(rind) ? rind : OneTo(1)))
        end
    end
    tuple(rinds...)::typeof(inds)
end

###### Generic reduction functions #####

## initialization
# initarray! is only called by sum!, prod!, etc.
for (Op, initfun) in ((:(typeof(add_sum)), :zero), (:(typeof(mul_prod)), :one))
    @eval initarray!(a::AbstractArray{T}, ::$(Op), init::Bool, src::AbstractArray) where {T} = (init && fill!(a, $(initfun)(T)); a)
end

for Op in (:(typeof(max)), :(typeof(min)))
    @eval initarray!(a::AbstractArray{T}, ::$(Op), init::Bool, src::AbstractArray) where {T} = (init && copyfirst!(a, src); a)
end

for (Op, initval) in ((:(typeof(&)), true), (:(typeof(|)), false))
    @eval initarray!(a::AbstractArray, ::$(Op), init::Bool, src::AbstractArray) = (init && fill!(a, $initval); a)
end

# reducedim_initarray is called by
reducedim_initarray(A::AbstractArray, region, v0, ::Type{R}) where {R} = fill!(similar(A,R,reduced_indices(A,region)), v0)
reducedim_initarray(A::AbstractArray, region, v0::T) where {T} = reducedim_initarray(A, region, v0, T)

function reducedim_initarray0(A::AbstractArray{T}, region, f, ops) where T
    ri = reduced_indices0(A, region)
    if isempty(A)
        if prod(map(length, reduced_indices(A, region))) != 0
            reducedim_initarray0_empty(A, region, f, ops) # ops over empty slice of A
        else
            R = f == identity ? T : Core.Compiler.return_type(f, (T,))
            similar(A, R, ri)
        end
    else
        R = f == identity ? T : typeof(f(first(A)))
        si = similar(A, R, ri)
        mapfirst!(f, si, A)
    end
end

reducedim_initarray0_empty(A::AbstractArray, region, f, ops) = mapslices(x->ops(f.(x)), A, region)
reducedim_initarray0_empty(A::AbstractArray, region,::typeof(identity), ops) = mapslices(ops, A, region)

# TODO: better way to handle reducedim initialization
#
# The current scheme is basically following Steven G. Johnson's original implementation
#
promote_union(T::Union) = promote_type(promote_union(T.a), promote_union(T.b))
promote_union(T) = T

function reducedim_init(f, op::Union{typeof(+),typeof(add_sum)}, A::AbstractArray, region)
    _reducedim_init(f, op, zero, sum, A, region)
end
function reducedim_init(f, op::Union{typeof(*),typeof(mul_prod)}, A::AbstractArray, region)
    _reducedim_init(f, op, one, prod, A, region)
end
function _reducedim_init(f, op, fv, fop, A, region)
    T = promote_union(eltype(A))
    if T !== Any && applicable(zero, T)
        x = f(zero(T))
        z = op(fv(x), fv(x))
        Tr = typeof(z) == typeof(x) && !isbits(T) ? T : typeof(z)
    else
        z = fv(fop(f, A))
        Tr = typeof(z)
    end
    return reducedim_initarray(A, region, z, Tr)
end

reducedim_init(f, op::typeof(max), A::AbstractArray{T}, region) where {T} = reducedim_initarray0(A, region, f, maximum)
reducedim_init(f, op::typeof(min), A::AbstractArray{T}, region) where {T} = reducedim_initarray0(A, region, f, minimum)
reducedim_init(f::Union{typeof(abs),typeof(abs2)}, op::typeof(max), A::AbstractArray{T}, region) where {T} =
    reducedim_initarray(A, region, zero(f(zero(T))))

reducedim_init(f, op::typeof(&), A::AbstractArray, region) = reducedim_initarray(A, region, true)
reducedim_init(f, op::typeof(|), A::AbstractArray, region) = reducedim_initarray(A, region, false)

# specialize to make initialization more efficient for common cases

let
    BitIntFloat = Union{BitInteger, Math.IEEEFloat}
    T = Union{
        [AbstractArray{t} for t in uniontypes(BitIntFloat)]...,
        [AbstractArray{Complex{t}} for t in uniontypes(BitIntFloat)]...}

    global reducedim_init(f, op::Union{typeof(+),typeof(add_sum)}, A::T, region) =
        reducedim_initarray(A, region, mapreduce_first(f, op, zero(eltype(A))))
    global reducedim_init(f, op::Union{typeof(*),typeof(mul_prod)}, A::T, region) =
        reducedim_initarray(A, region, mapreduce_first(f, op, one(eltype(A))))
end

## generic (map)reduction

has_fast_linear_indexing(a::AbstractArray) = false
has_fast_linear_indexing(a::Array) = true

function check_reducedims(R, A)
    # Check whether R has compatible dimensions w.r.t. A for reduction
    #
    # It returns an integer value (useful for choosing implementation)
    # - If it reduces only along leading dimensions, e.g. sum(A, 1) or sum(A, (1, 2)),
    #   it returns the length of the leading slice. For the two examples above,
    #   it will be size(A, 1) or size(A, 1) * size(A, 2).
    # - Otherwise, e.g. sum(A, 2) or sum(A, (1, 3)), it returns 0.
    #
    ndims(R) <= ndims(A) || throw(DimensionMismatch("cannot reduce $(ndims(A))-dimensional array to $(ndims(R)) dimensions"))
    lsiz = 1
    had_nonreduc = false
    for i = 1:ndims(A)
        Ri, Ai = axes(R, i), axes(A, i)
        sRi, sAi = length(Ri), length(Ai)
        if sRi == 1
            if sAi > 1
                if had_nonreduc
                    lsiz = 0  # to reduce along i, but some previous dimensions were non-reducing
                else
                    lsiz *= sAi  # if lsiz was set to zero, it will stay to be zero
                end
            end
        else
            Ri == Ai || throw(DimensionMismatch("reduction on array with indices $(axes(A)) with output with indices $(axes(R))"))
            had_nonreduc = true
        end
    end
    return lsiz
end

"""
Extract first entry of slices of array A into existing array R.
"""
copyfirst!(R::AbstractArray, A::AbstractArray) = mapfirst!(identity, R, A)

function mapfirst!(f, R::AbstractArray, A::AbstractArray)
    lsiz = check_reducedims(R, A)
    iA = axes(A)
    iR = axes(R)
    t = []
    for i in 1:length(iR)
        iAi = iA[i]
        push!(t, iAi == iR[i] ? iAi : first(iAi))
    end
    map!(f, R, view(A, t...))
end

function _mapreducedim!(f, op, R::AbstractArray, A::AbstractArray)
    lsiz = check_reducedims(R,A)
    isempty(A) && return R

    if has_fast_linear_indexing(A) && lsiz > 16
        # use mapreduce_impl, which is probably better tuned to achieve higher performance
        nslices = div(_length(A), lsiz)
        ibase = first(linearindices(A))-1
        for i = 1:nslices
            @inbounds R[i] = op(R[i], mapreduce_impl(f, op, A, ibase+1, ibase+lsiz))
            ibase += lsiz
        end
        return R
    end
    indsAt, indsRt = safe_tail(axes(A)), safe_tail(axes(R)) # handle d=1 manually
    keep, Idefault = Broadcast.shapeindexer(indsAt, indsRt)
    if reducedim1(R, A)
        # keep the accumulator as a local variable when reducing along the first dimension
        i1 = first(indices1(R))
        @inbounds for IA in CartesianIndices(indsAt)
            IR = Broadcast.newindex(IA, keep, Idefault)
            r = R[i1,IR]
            @simd for i in axes(A, 1)
                r = op(r, f(A[i, IA]))
            end
            R[i1,IR] = r
        end
    else
        @inbounds for IA in CartesianIndices(indsAt)
            IR = Broadcast.newindex(IA, keep, Idefault)
            @simd for i in axes(A, 1)
                R[i,IR] = op(R[i,IR], f(A[i,IA]))
            end
        end
    end
    return R
end

mapreducedim!(f, op, R::AbstractArray, A::AbstractArray) =
    (_mapreducedim!(f, op, R, A); R)

reducedim!(op, R::AbstractArray{RT}, A::AbstractArray) where {RT} =
    mapreducedim!(identity, op, R, A)

"""
    mapreducedim(f, op, A, region[, v0])

Evaluates to the same as `reducedim(op, map(f, A), region, f(v0))`, but is generally
faster because the intermediate array is avoided.

# Examples
```jldoctest
julia> a = reshape(Vector(1:16), (4,4))
4×4 Array{Int64,2}:
 1  5   9  13
 2  6  10  14
 3  7  11  15
 4  8  12  16

julia> mapreducedim(isodd, *, a, 1)
1×4 Array{Bool,2}:
 false  false  false  false

julia> mapreducedim(isodd, |, a, 1, true)
1×4 Array{Bool,2}:
 true  true  true  true
```
"""
mapreducedim(f, op, A::AbstractArray, region, v0) =
    mapreducedim!(f, op, reducedim_initarray(A, region, v0), A)
mapreducedim(f, op, A::AbstractArray, region) =
    mapreducedim!(f, op, reducedim_init(f, op, A, region), A)

"""
    reducedim(f, A, region[, v0])

Reduce 2-argument function `f` along dimensions of `A`. `region` is a vector specifying the
dimensions to reduce, and `v0` is the initial value to use in the reductions. For `+`, `*`,
`max` and `min` the `v0` argument is optional.

The associativity of the reduction is implementation-dependent; if you need a particular
associativity, e.g. left-to-right, you should write your own loop. See documentation for
[`reduce`](@ref).

# Examples
```jldoctest
julia> a = reshape(Vector(1:16), (4,4))
4×4 Array{Int64,2}:
 1  5   9  13
 2  6  10  14
 3  7  11  15
 4  8  12  16

julia> reducedim(max, a, 2)
4×1 Array{Int64,2}:
 13
 14
 15
 16

julia> reducedim(max, a, 1)
1×4 Array{Int64,2}:
 4  8  12  16
```
"""
reducedim(op, A::AbstractArray, region, v0) = mapreducedim(identity, op, A, region, v0)
reducedim(op, A::AbstractArray, region) = mapreducedim(identity, op, A, region)

##### Specific reduction functions #####
"""
    sum(A, dims)

Sum elements of an array over the given dimensions.

# Examples
```jldoctest
julia> A = [1 2; 3 4]
2×2 Array{Int64,2}:
 1  2
 3  4

julia> sum(A, 1)
1×2 Array{Int64,2}:
 4  6

julia> sum(A, 2)
2×1 Array{Int64,2}:
 3
 7
```
"""
sum(A, dims)

"""
    sum!(r, A)

Sum elements of `A` over the singleton dimensions of `r`, and write results to `r`.

# Examples
```jldoctest
julia> A = [1 2; 3 4]
2×2 Array{Int64,2}:
 1  2
 3  4

julia> sum!([1; 1], A)
2-element Array{Int64,1}:
 3
 7

julia> sum!([1 1], A)
1×2 Array{Int64,2}:
 4  6
```
"""
sum!(r, A)

"""
    prod(A, dims)

Multiply elements of an array over the given dimensions.

# Examples
```jldoctest
julia> A = [1 2; 3 4]
2×2 Array{Int64,2}:
 1  2
 3  4

julia> prod(A, 1)
1×2 Array{Int64,2}:
 3  8

julia> prod(A, 2)
2×1 Array{Int64,2}:
  2
 12
```
"""
prod(A, dims)

"""
    prod!(r, A)

Multiply elements of `A` over the singleton dimensions of `r`, and write results to `r`.

# Examples
```jldoctest
julia> A = [1 2; 3 4]
2×2 Array{Int64,2}:
 1  2
 3  4

julia> prod!([1; 1], A)
2-element Array{Int64,1}:
  2
 12

julia> prod!([1 1], A)
1×2 Array{Int64,2}:
 3  8
```
"""
prod!(r, A)

"""
    maximum(A, dims)

Compute the maximum value of an array over the given dimensions. See also the
[`max(a,b)`](@ref) function to take the maximum of two or more arguments,
which can be applied elementwise to arrays via `max.(a,b)`.

```jldoctest
julia> A = [1 2; 3 4]
2×2 Array{Int64,2}:
 1  2
 3  4

julia> maximum(A, 1)
1×2 Array{Int64,2}:
 3  4

julia> maximum(A, 2)
2×1 Array{Int64,2}:
 2
 4
```
"""
maximum(A, dims)

"""
    maximum!(r, A)

Compute the maximum value of `A` over the singleton dimensions of `r`, and write results to `r`.

# Examples
```jldoctest
julia> A = [1 2; 3 4]
2×2 Array{Int64,2}:
 1  2
 3  4

julia> maximum!([1; 1], A)
2-element Array{Int64,1}:
 2
 4

julia> maximum!([1 1], A)
1×2 Array{Int64,2}:
 3  4
```
"""
maximum!(r, A)

"""
    minimum(A, dims)

Compute the minimum value of an array over the given dimensions. See also the
[`min(a,b)`](@ref) function to take the minimum of two or more arguments,
which can be applied elementwise to arrays via `min.(a,b)`.

# Examples
```jldoctest
julia> A = [1 2; 3 4]
2×2 Array{Int64,2}:
 1  2
 3  4

julia> minimum(A, 1)
1×2 Array{Int64,2}:
 1  2

julia> minimum(A, 2)
2×1 Array{Int64,2}:
 1
 3
```
"""
minimum(A, dims)

"""
    minimum!(r, A)

Compute the minimum value of `A` over the singleton dimensions of `r`, and write results to `r`.

# Examples
```jldoctest
julia> A = [1 2; 3 4]
2×2 Array{Int64,2}:
 1  2
 3  4

julia> minimum!([1; 1], A)
2-element Array{Int64,1}:
 1
 3

julia> minimum!([1 1], A)
1×2 Array{Int64,2}:
 1  2
```
"""
minimum!(r, A)

"""
    all(A, dims)

Test whether all values along the given dimensions of an array are `true`.

# Examples
```jldoctest
julia> A = [true false; true true]
2×2 Array{Bool,2}:
 true  false
 true   true

julia> all(A, 1)
1×2 Array{Bool,2}:
 true  false

julia> all(A, 2)
2×1 Array{Bool,2}:
 false
  true
```
"""
all(A::AbstractArray, dims)

"""
    all!(r, A)

Test whether all values in `A` along the singleton dimensions of `r` are `true`, and write results to `r`.

# Examples
```jldoctest
julia> A = [true false; true false]
2×2 Array{Bool,2}:
 true  false
 true  false

julia> all!([1; 1], A)
2-element Array{Int64,1}:
 0
 0

julia> all!([1 1], A)
1×2 Array{Int64,2}:
 1  0
```
"""
all!(r, A)

"""
    any(A, dims)

Test whether any values along the given dimensions of an array are `true`.

# Examples
```jldoctest
julia> A = [true false; true false]
2×2 Array{Bool,2}:
 true  false
 true  false

julia> any(A, 1)
1×2 Array{Bool,2}:
 true  false

julia> any(A, 2)
2×1 Array{Bool,2}:
 true
 true
```
"""
any(::AbstractArray,dims)

"""
    any!(r, A)

Test whether any values in `A` along the singleton dimensions of `r` are `true`, and write
results to `r`.

# Examples
```jldoctest
julia> A = [true false; true false]
2×2 Array{Bool,2}:
 true  false
 true  false

julia> any!([1; 1], A)
2-element Array{Int64,1}:
 1
 1

julia> any!([1 1], A)
1×2 Array{Int64,2}:
 1  0
```
"""
any!(r, A)

for (fname, op) in [(:sum, :add_sum), (:prod, :mul_prod),
                    (:maximum, :max), (:minimum, :min),
                    (:all, :&), (:any, :|)]
    fname! = Symbol(fname, '!')
    @eval begin
        $(fname!)(f::Function, r::AbstractArray, A::AbstractArray; init::Bool=true) =
            mapreducedim!(f, $(op), initarray!(r, $(op), init, A), A)
        $(fname!)(r::AbstractArray, A::AbstractArray; init::Bool=true) = $(fname!)(identity, r, A; init=init)

        $(fname)(f::Function, A::AbstractArray, region) =
            mapreducedim(f, $(op), A, region)
        $(fname)(A::AbstractArray, region) = $(fname)(identity, A, region)
    end
end

##### findmin & findmax #####
# The initial values of Rval are not used if the correponding indices in Rind are 0.
#
function findminmax!(f, Rval, Rind, A::AbstractArray{T,N}) where {T,N}
    (isempty(Rval) || isempty(A)) && return Rval, Rind
    lsiz = check_reducedims(Rval, A)
    for i = 1:N
        axes(Rval, i) == axes(Rind, i) || throw(DimensionMismatch("Find-reduction: outputs must have the same indices"))
    end
    # If we're reducing along dimension 1, for efficiency we can make use of a temporary.
    # Otherwise, keep the result in Rval/Rind so that we traverse A in storage order.
    indsAt, indsRt = safe_tail(axes(A)), safe_tail(axes(Rval))
    keep, Idefault = Broadcast.shapeindexer(indsAt, indsRt)
    ks = keys(A)
    k, kss = next(ks, start(ks))
    zi = zero(eltype(ks))
    if reducedim1(Rval, A)
        i1 = first(indices1(Rval))
        @inbounds for IA in CartesianIndices(indsAt)
            IR = Broadcast.newindex(IA, keep, Idefault)
            tmpRv = Rval[i1,IR]
            tmpRi = Rind[i1,IR]
            for i in axes(A,1)
                tmpAv = A[i,IA]
                if tmpRi == zi || (tmpRv == tmpRv && (tmpAv != tmpAv || f(tmpAv, tmpRv)))
                    tmpRv = tmpAv
                    tmpRi = k
                end
                k, kss = next(ks, kss)
            end
            Rval[i1,IR] = tmpRv
            Rind[i1,IR] = tmpRi
        end
    else
        @inbounds for IA in CartesianIndices(indsAt)
            IR = Broadcast.newindex(IA, keep, Idefault)
            for i in axes(A, 1)
                tmpAv = A[i,IA]
                tmpRv = Rval[i,IR]
                tmpRi = Rind[i,IR]
                if tmpRi == zi || (tmpRv == tmpRv && (tmpAv != tmpAv || f(tmpAv, tmpRv)))
                    Rval[i,IR] = tmpAv
                    Rind[i,IR] = k
                end
                k, kss = next(ks, kss)
            end
        end
    end
    Rval, Rind
end

"""
    findmin!(rval, rind, A, [init=true]) -> (minval, index)

Find the minimum of `A` and the corresponding linear index along singleton
dimensions of `rval` and `rind`, and store the results in `rval` and `rind`.
`NaN` is treated as less than all other values.
"""
function findmin!(rval::AbstractArray, rind::AbstractArray, A::AbstractArray;
                  init::Bool=true)
    findminmax!(isless, init && !isempty(A) ? fill!(rval, first(A)) : rval, fill!(rind,zero(eltype(keys(A)))), A)
end

"""
    findmin(A, region) -> (minval, index)

For an array input, returns the value and index of the minimum over the given region.
`NaN` is treated as less than all other values.

# Examples
```jldoctest
julia> A = [1.0 2; 3 4]
2×2 Array{Float64,2}:
 1.0  2.0
 3.0  4.0

julia> findmin(A, 1)
([1.0 2.0], CartesianIndex{2}[CartesianIndex(1, 1) CartesianIndex(1, 2)])

julia> findmin(A, 2)
([1.0; 3.0], CartesianIndex{2}[CartesianIndex(1, 1); CartesianIndex(2, 1)])
```
"""
function findmin(A::AbstractArray{T}, region) where T
    ri = reduced_indices0(A, region)
    if isempty(A)
        if prod(map(length, reduced_indices(A, region))) != 0
            throw(ArgumentError("collection slices must be non-empty"))
        end
        (similar(A, ri), similar(dims->zeros(eltype(keys(A)), dims), ri))
    else
        findminmax!(isless, fill!(similar(A, ri), first(A)),
                    similar(dims->zeros(eltype(keys(A)), dims), ri), A)
    end
end

isgreater(a, b) = isless(b,a)

"""
    findmax!(rval, rind, A, [init=true]) -> (maxval, index)

Find the maximum of `A` and the corresponding linear index along singleton
dimensions of `rval` and `rind`, and store the results in `rval` and `rind`.
`NaN` is treated as greater than all other values.
"""
function findmax!(rval::AbstractArray, rind::AbstractArray, A::AbstractArray;
                  init::Bool=true)
    findminmax!(isgreater, init && !isempty(A) ? fill!(rval, first(A)) : rval, fill!(rind,zero(eltype(keys(A)))), A)
end

"""
    findmax(A, region) -> (maxval, index)

For an array input, returns the value and index of the maximum over the given region.
`NaN` is treated as greater than all other values.

# Examples
```jldoctest
julia> A = [1.0 2; 3 4]
2×2 Array{Float64,2}:
 1.0  2.0
 3.0  4.0

julia> findmax(A,1)
([3.0 4.0], CartesianIndex{2}[CartesianIndex(2, 1) CartesianIndex(2, 2)])

julia> findmax(A,2)
([2.0; 4.0], CartesianIndex{2}[CartesianIndex(1, 2); CartesianIndex(2, 2)])
```
"""
function findmax(A::AbstractArray{T}, region) where T
    ri = reduced_indices0(A, region)
    if isempty(A)
        if prod(map(length, reduced_indices(A, region))) != 0
            throw(ArgumentError("collection slices must be non-empty"))
        end
        similar(A, ri), similar(dims->zeros(eltype(keys(A)), dims), ri)
    else
        findminmax!(isgreater, fill!(similar(A, ri), first(A)),
                    similar(dims->zeros(eltype(keys(A)), dims), ri), A)
    end
end

reducedim1(R, A) = length(indices1(R)) == 1
