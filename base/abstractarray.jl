# This file is a part of Julia. License is MIT: http://julialang.org/license

## Type aliases for convenience ##

typealias AbstractVector{T} AbstractArray{T,1}
typealias AbstractMatrix{T} AbstractArray{T,2}
typealias AbstractVecOrMat{T} Union{AbstractVector{T}, AbstractMatrix{T}}
typealias RangeIndex Union{Int, Range{Int}, UnitRange{Int}, Colon}

## Basic functions ##

vect() = Array{Any,1}(0)
vect{T}(X::T...) = T[ X[i] for i=1:length(X) ]

function vect(X...)
    T = promote_typeof(X...)
    #T[ X[i] for i=1:length(X) ]
    # TODO: this is currently much faster. should figure out why. not clear.
    copy!(Array{T,1}(length(X)), X)
end

size{T,N}(t::AbstractArray{T,N}, d) = d <= N ? size(t)[d] : 1
size{N}(x, d1::Integer, d2::Integer, dx::Vararg{Integer, N}) = (size(x, d1), size(x, d2), ntuple(k->size(x, dx[k]), Val{N})...)
eltype{T}(::Type{AbstractArray{T}}) = T
eltype{T,N}(::Type{AbstractArray{T,N}}) = T
elsize{T}(::AbstractArray{T}) = sizeof(T)
ndims{T,N}(::AbstractArray{T,N}) = N
ndims{T,N}(::Type{AbstractArray{T,N}}) = N
ndims{T<:AbstractArray}(::Type{T}) = ndims(supertype(T))
length(t::AbstractArray) = prod(size(t))::Int
endof(a::AbstractArray) = length(a)
first(a::AbstractArray) = a[first(eachindex(a))]

function first(itr)
    state = start(itr)
    done(itr, state) && throw(ArgumentError("collection must be non-empty"))
    next(itr, state)[1]
end
last(a) = a[end]

function stride(a::AbstractArray, i::Integer)
    if i > ndims(a)
        return length(a)
    end
    s = 1
    for n=1:(i-1)
        s *= size(a, n)
    end
    return s
end

strides(a::AbstractArray) = ntuple(i->stride(a,i), ndims(a))::Dims

function isassigned(a::AbstractArray, i::Int...)
    # TODO
    try
        a[i...]
        true
    catch
        false
    end
end

# used to compute "end" for last index
function trailingsize(A, n)
    s = 1
    for i=n:ndims(A)
        s *= size(A,i)
    end
    return s
end

## Traits for array types ##

abstract LinearIndexing
immutable LinearFast <: LinearIndexing end
immutable LinearSlow <: LinearIndexing end

linearindexing(A::AbstractArray) = linearindexing(typeof(A))
linearindexing{T<:AbstractArray}(::Type{T}) = LinearSlow()
linearindexing{T<:Array}(::Type{T}) = LinearFast()
linearindexing{T<:Range}(::Type{T}) = LinearFast()

linearindexing(A::AbstractArray, B::AbstractArray) = linearindexing(linearindexing(A), linearindexing(B))
linearindexing(A::AbstractArray, B::AbstractArray...) = linearindexing(linearindexing(A), linearindexing(B...))
linearindexing(::LinearFast, ::LinearFast) = LinearFast()
linearindexing(::LinearIndexing, ::LinearIndexing) = LinearSlow()

## Bounds checking ##
@generated function trailingsize{T,N,n}(A::AbstractArray{T,N}, ::Type{Val{n}})
    n > N && return 1
    ex = :(size(A, $n))
    for m = n+1:N
        ex = :($ex * size(A, $m))
    end
    Expr(:block, Expr(:meta, :inline), ex)
end

# check along a single dimension
checkbounds(::Type{Bool}, sz::Integer, i) = throw(ArgumentError("unable to check bounds for indices of type $(typeof(i))"))
checkbounds(::Type{Bool}, sz::Integer, i::Real) = 1 <= i <= sz
checkbounds(::Type{Bool}, sz::Integer, ::Colon) = true
function checkbounds(::Type{Bool}, sz::Integer, r::Range)
    @_propagate_inbounds_meta
    isempty(r) | (checkbounds(Bool, sz, first(r)) & checkbounds(Bool, sz, last(r)))
end
checkbounds{N}(::Type{Bool}, sz::Integer, I::AbstractArray{Bool,N}) = N == 1 && length(I) == sz
function checkbounds(::Type{Bool}, sz::Integer, I::AbstractArray)
    @_inline_meta
    b = true
    for i in I
        b &= checkbounds(Bool, sz, i)
    end
    b
end

# check all dimensions
function checkbounds{N,T}(::Type{Bool}, sz::NTuple{N,Integer}, I1::T, I...)
    @_inline_meta
    checkbounds(Bool, sz[1], I1) & checkbounds(Bool, tail(sz), I...)
end
checkbounds{T<:Integer}(::Type{Bool}, sz::Tuple{T}, I1) = (@_inline_meta; checkbounds(Bool, sz[1], I1))
checkbounds{N}(::Type{Bool}, sz::NTuple{N,Integer}, I1) = (@_inline_meta; checkbounds(Bool, prod(sz), I1))
checkbounds{N}(::Type{Bool}, sz::NTuple{N,Integer}) = (@_inline_meta; checkbounds(Bool, sz, 1))  # for a[]

checkbounds(::Type{Bool}, sz::Tuple{}, i) = (@_inline_meta; checkbounds(Bool, 1, i))
function checkbounds(::Type{Bool}, sz::Tuple{}, i, I...)
    @_inline_meta
    checkbounds(Bool, 1, i) & checkbounds(Bool, (), I...)
end
# Prevent allocation of a GC frame by hiding the BoundsError in a noinline function
throw_boundserror(A, I) = (@_noinline_meta; throw(BoundsError(A, I)))

# Don't define index types on checkbounds to make extending easier
checkbounds(A::AbstractArray, I...) = (@_inline_meta; _internal_checkbounds(A, I...))
# The internal function is named _internal_checkbounds since there had been a
# _checkbounds previously that meant something different.
_internal_checkbounds(A::AbstractArray) = _internal_checkbounds(A,1)
_internal_checkbounds(A::AbstractArray, I::AbstractArray{Bool}) = size(A) == size(I) || throw_boundserror(A, I)
_internal_checkbounds(A::AbstractArray, I::AbstractVector{Bool}) = length(A) == length(I) || throw_boundserror(A, I)
function _internal_checkbounds(A::AbstractArray, I1, I...)
    # having I1 seems important for good codegen
    @_inline_meta
    checkbounds(Bool, size(A), I1, I...) || throw_boundserror(A, (I1, I...))
end

# See also specializations in multidimensional

## Constructors ##

# default arguments to similar()
similar{T}(a::AbstractArray{T})                          = similar(a, T)
similar(   a::AbstractArray, T::Type)                    = similar(a, T, size(a))
similar{T}(a::AbstractArray{T}, dims::DimsInteger)       = similar(a, T, dims)
similar{T}(a::AbstractArray{T}, dims::Integer...)        = similar(a, T, dims)
similar(   a::AbstractArray, T::Type, dims::Integer...)  = similar(a, T, dims)
# similar creates an Array by default
similar(   a::AbstractArray, T::Type, dims::DimsInteger) = similar(a, T, convert(Dims, dims))
similar{N}(a::AbstractArray, T::Type, dims::Dims{N})     = Array{T,N}(dims)

## from general iterable to any array

function copy!(dest::AbstractArray, src)
    i = 1
    for x in src
        dest[i] = x
        i += 1
    end
    return dest
end

# if src is not an AbstractArray, moving to the offset might be O(n)
function copy!(dest::AbstractArray, doffs::Integer, src)
    doffs < 1 && throw(BoundsError(dest, doffs))
    st = start(src)
    i, dmax = doffs, length(dest)
    while !done(src, st)
        i > dmax && throw(BoundsError(dest, i))
        val, st = next(src, st)
        @inbounds dest[i] = val
        i += 1
    end
    return dest
end

# copy from an some iterable object into an AbstractArray
function copy!(dest::AbstractArray, doffs::Integer, src, soffs::Integer)
    if (doffs < 1) | (soffs < 1)
        doffs < 1 && throw(BoundsError(dest, doffs))
        throw(ArgumentError(string("source start offset (",soffs,") is < 1")))
    end
    st = start(src)
    for j = 1:(soffs-1)
        if done(src, st)
            throw(ArgumentError(string("source has fewer elements than required, ",
                                       "expected at least ",soffs,", got ",j-1)))
        end
        _, st = next(src, st)
    end
    dn = done(src, st)
    if dn
        throw(ArgumentError(string("source has fewer elements than required, ",
                                      "expected at least ",soffs,", got ",soffs-1)))
    end
    i, dmax = doffs, length(dest)
    while !dn
        i > dmax && throw(BoundsError(dest, i))
        val, st = next(src, st)
        @inbounds dest[i] = val
        i += 1
        dn = done(src, st)
    end
    return dest
end

# this method must be separate from the above since src might not have a length
function copy!(dest::AbstractArray, doffs::Integer, src, soffs::Integer, n::Integer)
    n < 0 && throw(BoundsError(dest, n))
    n == 0 && return dest
    dmax = doffs + n - 1
    if (dmax > length(dest)) | (doffs < 1) | (soffs < 1)
        doffs < 1 && throw(BoundsError(dest, doffs))
        soffs < 1 && throw(ArgumentError(string("source start offset (",soffs,") is < 1")))
        throw(BoundsError(dest, dmax))
    end
    st = start(src)
    for j = 1:(soffs-1)
        if done(src, st)
            throw(ArgumentError(string("source has fewer elements than required, ",
                                       "expected at least ",soffs,", got ",j-1)))
        end
        _, st = next(src, st)
    end
    i = doffs
    while i <= dmax && !done(src, st)
        val, st = next(src, st)
        @inbounds dest[i] = val
        i += 1
    end
    i <= dmax && throw(BoundsError(dest, i))
    return dest
end

## copy between abstract arrays - generally more efficient
## since a single index variable can be used.

copy!(dest::AbstractArray, src::AbstractArray) =
    copy!(linearindexing(dest), dest, linearindexing(src), src)

function copy!(::LinearIndexing, dest::AbstractArray, ::LinearIndexing, src::AbstractArray)
    n = length(src)
    n > length(dest) && throw(BoundsError(dest, n))
    @inbounds for i = 1:n
        dest[i] = src[i]
    end
    return dest
end

function copy!(::LinearIndexing, dest::AbstractArray, ::LinearSlow, src::AbstractArray)
    n = length(src)
    n > length(dest) && throw(BoundsError(dest, n))
    i = 0
    @inbounds for a in src
        dest[i+=1] = a
    end
    return dest
end

function copy!(dest::AbstractArray, doffs::Integer, src::AbstractArray)
    copy!(dest, doffs, src, 1, length(src))
end

function copy!(dest::AbstractArray, doffs::Integer, src::AbstractArray, soffs::Integer)
    soffs > length(src) && throw(BoundsError(src, soffs))
    copy!(dest, doffs, src, soffs, length(src)-soffs+1)
end

function copy!(dest::AbstractArray, doffs::Integer,
               src::AbstractArray, soffs::Integer,
               n::Integer)
    n == 0 && return dest
    n < 0  && throw(BoundsError(src, n))
    soffs+n-1 > length(src)  && throw(BoundsError(src, soffs+n-1))
    doffs+n-1 > length(dest) && throw(BoundsError(dest, doffs+n-1))
    doffs < 1 && throw(BoundsError(dest, doffs))
    soffs < 1 && throw(BoundsError(src, soffs))
    @inbounds for i = 0:(n-1) #Fixme iter
        dest[doffs+i] = src[soffs+i]
    end
    return dest
end

copy(a::AbstractArray) = copymutable(a)

function copy!{R,S}(B::AbstractVecOrMat{R}, ir_dest::Range{Int}, jr_dest::Range{Int},
                    A::AbstractVecOrMat{S}, ir_src::Range{Int}, jr_src::Range{Int})
    if length(ir_dest) != length(ir_src)
        throw(ArgumentError(string("source and destination must have same size (got ",
                                   length(ir_src)," and ",length(ir_dest),")")))
    end
    if length(jr_dest) != length(jr_src)
        throw(ArgumentError(string("source and destination must have same size (got ",
                                   length(jr_src)," and ",length(jr_dest),")")))
    end
    @boundscheck checkbounds(B, ir_dest, jr_dest)
    @boundscheck checkbounds(A, ir_src, jr_src)
    jdest = first(jr_dest)
    for jsrc in jr_src
        idest = first(ir_dest)
        for isrc in ir_src
            B[idest,jdest] = A[isrc,jsrc]
            idest += step(ir_dest)
        end
        jdest += step(jr_dest)
    end
    return B
end

function copy_transpose!{R,S}(B::AbstractVecOrMat{R}, ir_dest::Range{Int}, jr_dest::Range{Int},
                              A::AbstractVecOrMat{S}, ir_src::Range{Int}, jr_src::Range{Int})
    if length(ir_dest) != length(jr_src)
        throw(ArgumentError(string("source and destination must have same size (got ",
                                   length(jr_src)," and ",length(ir_dest),")")))
    end
    if length(jr_dest) != length(ir_src)
        throw(ArgumentError(string("source and destination must have same size (got ",
                                   length(ir_src)," and ",length(jr_dest),")")))
    end
    @boundscheck checkbounds(B, ir_dest, jr_dest)
    @boundscheck checkbounds(A, ir_src, jr_src)
    idest = first(ir_dest)
    for jsrc in jr_src
        jdest = first(jr_dest)
        for isrc in ir_src
            B[idest,jdest] = A[isrc,jsrc]
            jdest += step(jr_dest)
        end
        idest += step(ir_dest)
    end
    return B
end

copymutable(a::AbstractArray) = copy!(similar(a), a)
copymutable(itr) = collect(itr)
"""
    copymutable(a)

Make a mutable copy of an array or iterable `a`.  For `a::Array`,
this is equivalent to `copy(a)`, but for other array types it may
differ depending on the type of `similar(a)`.  For generic iterables
this is equivalent to `collect(a)`.
"""
copymutable

zero{T}(x::AbstractArray{T}) = fill!(similar(x), zero(T))

## iteration support for arrays by iterating over `eachindex` in the array ##
# Allows fast iteration by default for both LinearFast and LinearSlow arrays

# While the definitions for LinearFast are all simple enough to inline on their
# own, LinearSlow's CartesianRange is more complicated and requires explicit
# inlining.
start(A::AbstractArray) = (@_inline_meta(); itr = eachindex(A); (itr, start(itr)))
next(A::AbstractArray,i) = (@_inline_meta(); (idx, s) = next(i[1], i[2]); (A[idx], (i[1], s)))
done(A::AbstractArray,i) = done(i[1], i[2])

# eachindex iterates over all indices. LinearSlow definitions are later.
eachindex(A::AbstractArray) = (@_inline_meta(); eachindex(linearindexing(A), A))

function eachindex(A::AbstractArray, B::AbstractArray)
    @_inline_meta
    eachindex(linearindexing(A,B), A, B)
end
function eachindex(A::AbstractArray, B::AbstractArray...)
    @_inline_meta
    eachindex(linearindexing(A,B...), A, B...)
end
eachindex(::LinearFast, A::AbstractArray) = 1:length(A)
function eachindex(::LinearFast, A::AbstractArray, B::AbstractArray...)
    @_inline_meta
    1:_maxlength(A, B...)
end
_maxlength(A) = length(A)
function _maxlength(A, B, C...)
    @_inline_meta
    max(length(A), _maxlength(B, C...))
end

isempty(a::AbstractArray) = (length(a) == 0)

## Conversions ##

convert{T,N  }(::Type{AbstractArray{T,N}}, A::AbstractArray{T,N}) = A
convert{T,S,N}(::Type{AbstractArray{T,N}}, A::AbstractArray{S,N}) = copy!(similar(A,T), A)
convert{T,S,N}(::Type{AbstractArray{T  }}, A::AbstractArray{S,N}) = convert(AbstractArray{T,N}, A)

convert{T,N}(::Type{Array}, A::AbstractArray{T,N}) = convert(Array{T,N}, A)

full(x::AbstractArray) = x

map(::Type{Integer},  a::Array) = map!(Integer, similar(a,typeof(Integer(one(eltype(a))))), a)
map(::Type{Signed},   a::Array) = map!(Signed, similar(a,typeof(Signed(one(eltype(a))))), a)
map(::Type{Unsigned}, a::Array) = map!(Unsigned, similar(a,typeof(Unsigned(one(eltype(a))))), a)

## range conversions ##

map{T<:Real}(::Type{T}, r::StepRange) = T(r.start):T(r.step):T(last(r))
map{T<:Real}(::Type{T}, r::UnitRange) = T(r.start):T(last(r))
map{T<:AbstractFloat}(::Type{T}, r::FloatRange) = FloatRange(T(r.start), T(r.step), r.len, T(r.divisor))
function map{T<:AbstractFloat}(::Type{T}, r::LinSpace)
    new_len = T(r.len)
    new_len == r.len || error("$r: too long for $T")
    LinSpace(T(r.start), T(r.stop), new_len, T(r.divisor))
end

## unsafe/pointer conversions ##

# note: the following type definitions don't mean any AbstractArray is convertible to
# a data Ref. they just map the array element type to the pointer type for
# convenience in cases that work.
pointer{T}(x::AbstractArray{T}) = unsafe_convert(Ptr{T}, x)
pointer{T}(x::AbstractArray{T}, i::Integer) = (@_inline_meta; unsafe_convert(Ptr{T},x) + (i-1)*elsize(x))


## Approach:
# We only define one fallback method on getindex for all argument types.
# That dispatches to an (inlined) internal _getindex function, where the goal is
# to transform the indices such that we can call the only getindex method that
# we require the type A{T,N} <: AbstractArray{T,N} to define; either:
#       getindex(::A, ::Int) # if linearindexing(A) == LinearFast() OR
#       getindex{T,N}(::A{T,N}, ::Vararg{Int, N}) # if LinearSlow()
# If the subtype hasn't defined the required method, it falls back to the
# _getindex function again where an error is thrown to prevent stack overflows.

function getindex(A::AbstractArray, I...)
    @_propagate_inbounds_meta
    _getindex(linearindexing(A), A, I...)
end
function unsafe_getindex(A::AbstractArray, I...)
    @_inline_meta
    @inbounds r = getindex(A, I...)
    r
end
## Internal definitions
_getindex(::LinearIndexing, A::AbstractArray, I...) = error("indexing $(typeof(A)) with types $(typeof(I)) is not supported")

## LinearFast Scalar indexing: canonical method is one Int
_getindex(::LinearFast, A::AbstractArray, ::Int) = error("indexing not defined for ", typeof(A))
_getindex(::LinearFast, A::AbstractArray, i::Real) = (@_propagate_inbounds_meta; getindex(A, to_index(i)))
function _getindex{T,N}(::LinearFast, A::AbstractArray{T,N}, I::Vararg{Real,N})
    # We must check bounds for sub2ind; so we can then use @inbounds
    @_inline_meta
    J = to_indexes(I...)
    @boundscheck checkbounds(A, J...)
    @inbounds r = getindex(A, sub2ind(size(A), J...))
    r
end
function _getindex(::LinearFast, A::AbstractArray, I::Real...) # TODO: DEPRECATE FOR #14770
    @_inline_meta
    J = to_indexes(I...)
    @boundscheck checkbounds(A, J...)
    @inbounds r = getindex(A, sub2ind(size(A), J...))
    r
end


## LinearSlow Scalar indexing: Canonical method is full dimensionality of Ints
_getindex{T,N}(::LinearSlow, A::AbstractArray{T,N}, ::Vararg{Int, N}) = error("indexing not defined for ", typeof(A))
_getindex{T,N}(::LinearSlow, A::AbstractArray{T,N}, I::Vararg{Real, N}) = (@_propagate_inbounds_meta; getindex(A, to_indexes(I...)...))
function _getindex(::LinearSlow, A::AbstractArray, i::Real)
    # ind2sub requires all dimensions to be > 0; may as well just check bounds
    @_inline_meta
    @boundscheck checkbounds(A, i)
    @inbounds r = getindex(A, ind2sub(size(A), to_index(i))...)
    r
end
@generated function _getindex{T,AN}(::LinearSlow, A::AbstractArray{T,AN}, I::Real...) # TODO: DEPRECATE FOR #14770
    N = length(I)
    if N > AN
        # Drop trailing ones
        Isplat = Expr[:(I[$d]) for d = 1:AN]
        Osplat = Expr[:(to_index(I[$d]) == 1) for d = AN+1:N]
        quote
            @_propagate_inbounds_meta
            @boundscheck (&)($(Osplat...)) || throw_boundserror(A, I)
            getindex(A, $(Isplat...))
        end
    else
        # Expand the last index into the appropriate number of indices
        Isplat = Expr[:(I[$d]) for d = 1:N-1]
        sz = Expr(:tuple)
        sz.args = Expr[:(size(A, $d)) for d=max(N,1):AN]
        szcheck = Expr[:(size(A, $d) > 0) for d=max(N,1):AN]
        last_idx = N > 0 ? :(to_index(I[$N])) : 1
        quote
            # ind2sub requires all dimensions to be > 0:
            @_propagate_inbounds_meta
            @boundscheck (&)($(szcheck...)) || throw_boundserror(A, I)
            getindex(A, $(Isplat...), ind2sub($sz, $last_idx)...)
        end
    end
end

## Setindex! is defined similarly. We first dispatch to an internal _setindex!
# function that allows dispatch on array storage
function setindex!(A::AbstractArray, v, I...)
    @_propagate_inbounds_meta
    _setindex!(linearindexing(A), A, v, I...)
end
function unsafe_setindex!(A::AbstractArray, v, I...)
    @_inline_meta
    @inbounds r = setindex!(A, v, I...)
    r
end
## Internal defitions
_setindex!(::LinearIndexing, A::AbstractArray, v, I...) = error("indexing $(typeof(A)) with types $(typeof(I)) is not supported")

## LinearFast Scalar indexing
_setindex!(::LinearFast, A::AbstractArray, v, ::Int) = error("indexed assignment not defined for ", typeof(A))
_setindex!(::LinearFast, A::AbstractArray, v, i::Real) = (@_propagate_inbounds_meta; setindex!(A, v, to_index(i)))
function _setindex!{T,N}(::LinearFast, A::AbstractArray{T,N}, v, I::Vararg{Real,N})
    # We must check bounds for sub2ind; so we can then use @inbounds
    @_inline_meta
    J = to_indexes(I...)
    @boundscheck checkbounds(A, J...)
    @inbounds r = setindex!(A, v, sub2ind(size(A), J...))
    r
end
function _setindex!(::LinearFast, A::AbstractArray, v, I::Real...) # TODO: DEPRECATE FOR #14770
    @_inline_meta
    J = to_indexes(I...)
    @boundscheck checkbounds(A, J...)
    @inbounds r = setindex!(A, v, sub2ind(size(A), J...))
    r
end

# LinearSlow Scalar indexing
_setindex!{T,N}(::LinearSlow, A::AbstractArray{T,N}, v, ::Vararg{Int, N}) = error("indexed assignment not defined for ", typeof(A))
_setindex!{T,N}(::LinearSlow, A::AbstractArray{T,N}, v, I::Vararg{Real, N}) = (@_propagate_inbounds_meta; setindex!(A, v, to_indexes(I...)...))
function _setindex!(::LinearSlow, A::AbstractArray, v, i::Real)
    # ind2sub requires all dimensions to be > 0; may as well just check bounds
    @_inline_meta
    @boundscheck checkbounds(A, i)
    @inbounds r = setindex!(A, v, ind2sub(size(A), to_index(i))...)
    r
end
@generated function _setindex!{T,AN}(::LinearSlow, A::AbstractArray{T,AN}, v, I::Real...) # TODO: DEPRECATE FOR #14770
    N = length(I)
    if N > AN
        # Drop trailing ones
        Isplat = Expr[:(I[$d]) for d = 1:AN]
        Osplat = Expr[:(to_index(I[$d]) == 1) for d = AN+1:N]
        quote
            # We only check the trailing ones, so just propagate @inbounds state
            @_propagate_inbounds_meta
            @boundscheck (&)($(Osplat...)) || throw_boundserror(A, I)
            setindex!(A, v, $(Isplat...))
        end
    else
        # Expand the last index into the appropriate number of indices
        Isplat = Expr[:(I[$d]) for d = 1:N-1]
        sz = Expr(:tuple)
        sz.args = Expr[:(size(A, $d)) for d=max(N,1):AN]
        szcheck = Expr[:(size(A, $d) > 0) for d=max(N,1):AN]
        last_idx = N > 0 ? :(to_index(I[$N])) : 1
        quote
            # ind2sub requires all dimensions to be > 0:
            @_propagate_inbounds_meta
            @boundscheck (&)($(szcheck...)) || throw_boundserror(A, I)
            setindex!(A, v, $(Isplat...), ind2sub($sz, $last_idx)...)
        end
    end
end

## get (getindex with a default value) ##

typealias RangeVecIntList{A<:AbstractVector{Int}} Union{Tuple{Vararg{Union{Range, AbstractVector{Int}}}}, AbstractVector{UnitRange{Int}}, AbstractVector{Range{Int}}, AbstractVector{A}}

get(A::AbstractArray, i::Integer, default) = checkbounds(Bool, length(A), i) ? A[i] : default
get(A::AbstractArray, I::Tuple{}, default) = similar(A, typeof(default), 0)
get(A::AbstractArray, I::Dims, default) = checkbounds(Bool, size(A), I...) ? A[I...] : default

function get!{T}(X::AbstractArray{T}, A::AbstractArray, I::Union{Range, AbstractVector{Int}}, default::T)
    ind = findin(I, 1:length(A))
    X[ind] = A[I[ind]]
    X[1:first(ind)-1] = default
    X[last(ind)+1:length(X)] = default
    X
end

get(A::AbstractArray, I::Range, default) = get!(similar(A, typeof(default), length(I)), A, I, default)

function get!{T}(X::AbstractArray{T}, A::AbstractArray, I::RangeVecIntList, default::T)
    fill!(X, default)
    dst, src = indcopy(size(A), I)
    X[dst...] = A[src...]
    X
end

get(A::AbstractArray, I::RangeVecIntList, default) = get!(similar(A, typeof(default), map(length, I)...), A, I, default)

## structured matrix methods ##
replace_in_print_matrix(A::AbstractMatrix,i::Integer,j::Integer,s::AbstractString) = s
replace_in_print_matrix(A::AbstractVector,i::Integer,j::Integer,s::AbstractString) = s

## Concatenation ##

promote_eltype() = Bottom
promote_eltype(v1, vs...) = promote_type(eltype(v1), promote_eltype(vs...))

#TODO: ERROR CHECK
cat(catdim::Integer) = Array{Any,1}(0)

vcat() = Array{Any,1}(0)
hcat() = Array{Any,1}(0)
typed_vcat{T}(::Type{T}) = Array{T,1}(0)
typed_hcat{T}(::Type{T}) = Array{T,1}(0)

## cat: special cases
vcat{T}(X::T...)         = T[ X[i] for i=1:length(X) ]
vcat{T<:Number}(X::T...) = T[ X[i] for i=1:length(X) ]
hcat{T}(X::T...)         = T[ X[j] for i=1, j=1:length(X) ]
hcat{T<:Number}(X::T...) = T[ X[j] for i=1, j=1:length(X) ]

vcat(X::Number...) = hvcat_fill(Array{promote_typeof(X...)}(length(X)), X)
hcat(X::Number...) = hvcat_fill(Array{promote_typeof(X...)}(1,length(X)), X)
typed_vcat{T}(::Type{T}, X::Number...) = hvcat_fill(Array{T,1}(length(X)), X)
typed_hcat{T}(::Type{T}, X::Number...) = hvcat_fill(Array{T,2}(1,length(X)), X)

vcat(V::AbstractVector...) = typed_vcat(promote_eltype(V...), V...)
vcat{T}(V::AbstractVector{T}...) = typed_vcat(T, V...)

function typed_vcat{T}(::Type{T}, V::AbstractVector...)
    n::Int = 0
    for Vk in V
        n += length(Vk)
    end
    a = similar(full(V[1]), T, n)
    pos = 1
    for k=1:length(V)
        Vk = V[k]
        p1 = pos+length(Vk)-1
        a[pos:p1] = Vk
        pos = p1+1
    end
    a
end

hcat(A::AbstractVecOrMat...) = typed_hcat(promote_eltype(A...), A...)
hcat{T}(A::AbstractVecOrMat{T}...) = typed_hcat(T, A...)

function typed_hcat{T}(::Type{T}, A::AbstractVecOrMat...)
    nargs = length(A)
    nrows = size(A[1], 1)
    ncols = 0
    dense = true
    for j = 1:nargs
        Aj = A[j]
        if size(Aj, 1) != nrows
            throw(ArgumentError("number of rows of each array must match (got $(map(x->size(x,1), A)))"))
        end
        dense &= isa(Aj,Array)
        nd = ndims(Aj)
        ncols += (nd==2 ? size(Aj,2) : 1)
    end
    B = similar(full(A[1]), T, nrows, ncols)
    pos = 1
    if dense
        for k=1:nargs
            Ak = A[k]
            n = length(Ak)
            copy!(B, pos, Ak, 1, n)
            pos += n
        end
    else
        for k=1:nargs
            Ak = A[k]
            p1 = pos+(isa(Ak,AbstractMatrix) ? size(Ak, 2) : 1)-1
            B[:, pos:p1] = Ak
            pos = p1+1
        end
    end
    return B
end

vcat(A::AbstractMatrix...) = typed_vcat(promote_eltype(A...), A...)
vcat{T}(A::AbstractMatrix{T}...) = typed_vcat(T, A...)

function typed_vcat{T}(::Type{T}, A::AbstractMatrix...)
    nargs = length(A)
    nrows = sum(a->size(a, 1), A)::Int
    ncols = size(A[1], 2)
    for j = 2:nargs
        if size(A[j], 2) != ncols
            throw(ArgumentError("number of columns of each array must match (got $(map(x->size(x,2), A)))"))
        end
    end
    B = similar(full(A[1]), T, nrows, ncols)
    pos = 1
    for k=1:nargs
        Ak = A[k]
        p1 = pos+size(Ak,1)-1
        B[pos:p1, :] = Ak
        pos = p1+1
    end
    return B
end

## cat: general case

function cat(catdims, X...)
    T = promote_type(map(x->isa(x,AbstractArray) ? eltype(x) : typeof(x), X)...)
    cat_t(catdims, T, X...)
end

function cat_t(catdims, typeC::Type, X...)
    catdims = collect(catdims)
    nargs = length(X)
    ndimsX = Int[isa(a,AbstractArray) ? ndims(a) : 0 for a in X]
    ndimsC = max(maximum(ndimsX), maximum(catdims))
    catsizes = zeros(Int,(nargs,length(catdims)))
    dims2cat = zeros(Int,ndimsC)
    for k = 1:length(catdims)
        dims2cat[catdims[k]]=k
    end

    dimsC = Int[d <= ndimsX[1] ? size(X[1],d) : 1 for d=1:ndimsC]
    for k = 1:length(catdims)
        catsizes[1,k] = dimsC[catdims[k]]
    end
    for i = 2:nargs
        for d = 1:ndimsC
            currentdim = (d <= ndimsX[i] ? size(X[i],d) : 1)
            if dims2cat[d] != 0
                dimsC[d] += currentdim
                catsizes[i,dims2cat[d]] = currentdim
            elseif dimsC[d] != currentdim
                throw(DimensionMismatch(string("mismatch in dimension ",d,
                                               " (expected ",dimsC[d],
                                               " got ",currentdim,")")))
            end
        end
    end

    C = similar(isa(X[1],AbstractArray) ? full(X[1]) : [X[1]], typeC, tuple(dimsC...))
    if length(catdims)>1
        fill!(C,0)
    end

    offsets = zeros(Int,length(catdims))
    for i=1:nargs
        cat_one = [ dims2cat[d] == 0 ? (1:dimsC[d]) : (offsets[dims2cat[d]]+(1:catsizes[i,dims2cat[d]]))
                   for d=1:ndimsC ]
        C[cat_one...] = X[i]
        for k = 1:length(catdims)
            offsets[k] += catsizes[i,k]
        end
    end
    return C
end

vcat(X...) = cat(1, X...)
hcat(X...) = cat(2, X...)

typed_vcat(T::Type, X...) = cat_t(1, T, X...)
typed_hcat(T::Type, X...) = cat_t(2, T, X...)

cat{T}(catdims, A::AbstractArray{T}...) = cat_t(catdims, T, A...)

cat(catdims, A::AbstractArray...) = cat_t(catdims, promote_eltype(A...), A...)

# The specializations for 1 and 2 inputs are important
# especially when running with --inline=no, see #11158
vcat(A::AbstractArray) = cat(1, A)
vcat(A::AbstractArray, B::AbstractArray) = cat(1, A, B)
vcat(A::AbstractArray...) = cat(1, A...)
hcat(A::AbstractArray) = cat(2, A)
hcat(A::AbstractArray, B::AbstractArray) = cat(2, A, B)
hcat(A::AbstractArray...) = cat(2, A...)

typed_vcat(T::Type, A::AbstractArray) = cat_t(1, T, A)
typed_vcat(T::Type, A::AbstractArray, B::AbstractArray) = cat_t(1, T, A, B)
typed_vcat(T::Type, A::AbstractArray...) = cat_t(1, T, A...)
typed_hcat(T::Type, A::AbstractArray) = cat_t(2, T, A)
typed_hcat(T::Type, A::AbstractArray, B::AbstractArray) = cat_t(2, T, A, B)
typed_hcat(T::Type, A::AbstractArray...) = cat_t(2, T, A...)

# 2d horizontal and vertical concatenation

function hvcat(nbc::Integer, as...)
    # nbc = # of block columns
    n = length(as)
    mod(n,nbc) != 0 &&
        throw(ArgumentError("number of arrays $n is not a multiple of the requested number of block columns $nbc"))
    nbr = div(n,nbc)
    hvcat(ntuple(i->nbc, nbr), as...)
end

hvcat(rows::Tuple{Vararg{Int}}, xs::AbstractMatrix...) = typed_hvcat(promote_eltype(xs...), rows, xs...)
hvcat{T}(rows::Tuple{Vararg{Int}}, xs::AbstractMatrix{T}...) = typed_hvcat(T, rows, xs...)

function typed_hvcat{T}(::Type{T}, rows::Tuple{Vararg{Int}}, as::AbstractMatrix...)
    nbr = length(rows)  # number of block rows

    nc = 0
    for i=1:rows[1]
        nc += size(as[i],2)
    end

    nr = 0
    a = 1
    for i = 1:nbr
        nr += size(as[a],1)
        a += rows[i]
    end

    out = similar(full(as[1]), T, nr, nc)

    a = 1
    r = 1
    for i = 1:nbr
        c = 1
        szi = size(as[a],1)
        for j = 1:rows[i]
            Aj = as[a+j-1]
            szj = size(Aj,2)
            if size(Aj,1) != szi
                throw(ArgumentError("mismatched height in block row $(i) (expected $szi, got $(size(Aj,1)))"))
            end
            if c-1+szj > nc
                throw(ArgumentError("block row $(i) has mismatched number of columns (expected $nc, got $(c-1+szj))"))
            end
            out[r:r-1+szi, c:c-1+szj] = Aj
            c += szj
        end
        if c != nc+1
            throw(ArgumentError("block row $(i) has mismatched number of columns (expected $nc, got $(c-1))"))
        end
        r += szi
        a += rows[i]
    end
    out
end

hvcat(rows::Tuple{Vararg{Int}}) = []
typed_hvcat{T}(::Type{T}, rows::Tuple{Vararg{Int}}) = Array{T,1}(0)

function hvcat{T<:Number}(rows::Tuple{Vararg{Int}}, xs::T...)
    nr = length(rows)
    nc = rows[1]

    a = Array{T,2}(nr, nc)
    if length(a) != length(xs)
        throw(ArgumentError("argument count does not match specified shape (expected $(length(a)), got $(length(xs)))"))
    end
    k = 1
    @inbounds for i=1:nr
        if nc != rows[i]
            throw(ArgumentError("row $(i) has mismatched number of columns (expected $nc, got $(rows[i]))"))
        end
        for j=1:nc
            a[i,j] = xs[k]
            k += 1
        end
    end
    a
end

function hvcat_fill(a::Array, xs::Tuple)
    k = 1
    nr, nc = size(a,1), size(a,2)
    for i=1:nr
        @inbounds for j=1:nc
            a[i,j] = xs[k]
            k += 1
        end
    end
    a
end

hvcat(rows::Tuple{Vararg{Int}}, xs::Number...) = typed_hvcat(promote_typeof(xs...), rows, xs...)

function typed_hvcat{T}(::Type{T}, rows::Tuple{Vararg{Int}}, xs::Number...)
    nr = length(rows)
    nc = rows[1]
    for i = 2:nr
        if nc != rows[i]
            throw(ArgumentError("row $(i) has mismatched number of columns (expected $nc, got $(rows[i]))"))
        end
    end
    len = length(xs)
    if nr*nc != len
        throw(ArgumentError("argument count $(len) does not match specified shape $((nr,nc))"))
    end
    hvcat_fill(Array{T,2}(nr, nc), xs)
end

# fallback definition of hvcat in terms of hcat and vcat
function hvcat(rows::Tuple{Vararg{Int}}, as...)
    nbr = length(rows)  # number of block rows
    rs = Array{Any,1}(nbr)
    a = 1
    for i = 1:nbr
        rs[i] = hcat(as[a:a-1+rows[i]]...)
        a += rows[i]
    end
    vcat(rs...)
end

function typed_hvcat{T}(::Type{T}, rows::Tuple{Vararg{Int}}, as...)
    nbr = length(rows)  # number of block rows
    rs = Array{Any,1}(nbr)
    a = 1
    for i = 1:nbr
        rs[i] = hcat(as[a:a-1+rows[i]]...)
        a += rows[i]
    end
    T[rs...;]
end

## Reductions and scans ##

function isequal(A::AbstractArray, B::AbstractArray)
    if A === B return true end
    if size(A) != size(B)
        return false
    end
    if isa(A,Range) != isa(B,Range)
        return false
    end
    for (a, b) in zip(A, B)
        if !isequal(a, b)
            return false
        end
    end
    return true
end

function lexcmp(A::AbstractArray, B::AbstractArray)
    for (a, b) in zip(A, B)
        res = lexcmp(a, b)
        res == 0 || return res
    end
    return cmp(length(A), length(B))
end

function (==)(A::AbstractArray, B::AbstractArray)
    if size(A) != size(B)
        return false
    end
    if isa(A,Range) != isa(B,Range)
        return false
    end
    for (a, b) in zip(A, B)
        if !(a == b)
            return false
        end
    end
    return true
end

sub2ind(dims::Tuple{Vararg{Integer}}) = 1
sub2ind(dims::Tuple{Vararg{Integer}}, I::Integer...) = _sub2ind(dims,I)
@generated function _sub2ind{N,M}(dims::NTuple{N,Integer}, I::NTuple{M,Integer})
    meta = Expr(:meta,:inline)
    ex = :(I[$M] - 1)
    for i = M-1:-1:1
        if i > N
            ex = :(I[$i] - 1 + $ex)
        else
            ex = :(I[$i] - 1 + dims[$i]*$ex)
        end
    end
    Expr(:block, meta,:($ex + 1))
end

@generated function ind2sub{N}(dims::NTuple{N,Integer}, ind::Integer)
    meta = Expr(:meta,:inline)
    N==0 && return :($meta; ind==1 ? () : throw(BoundsError()))
    exprs = Expr[:(ind = ind-1)]
    for i = 1:N-1
        push!(exprs,:(ind2 = div(ind,dims[$i])))
        push!(exprs,Expr(:(=),Symbol(:s,i),:(ind-dims[$i]*ind2+1)))
        push!(exprs,:(ind=ind2))
    end
    push!(exprs,Expr(:(=),Symbol(:s,N),:(ind+1)))
    Expr(:block,meta,exprs...,Expr(:tuple,[Symbol(:s,i) for i=1:N]...))
end

ind2sub(a::AbstractArray, ind::Integer) = ind2sub(size(a), ind)
sub2ind(a::AbstractArray, I::Integer...) = sub2ind(size(a), I...)

function sub2ind{T<:Integer}(dims::Tuple{Vararg{Integer}}, I::AbstractVector{T}...)
    N = length(dims)
    M = length(I[1])
    indices = Array{T,1}(length(I[1]))
    copy!(indices,I[1])

    s = dims[1]
    for j=2:length(I)
        Ij = I[j]
        for (i, k) in zip(eachindex(indices), eachindex(Ij))
            indices[i] += s*(Ij[k]-1)
        end
        s *= (j <= N ? dims[j] : 1)
    end
    return indices
end

function ind2sub{N,T<:Integer}(dims::NTuple{N,Integer}, ind::AbstractVector{T})
    M = length(ind)
    t = NTuple{N,Vector{T}}(ntuple(n->Array{T,1}(M), N))
    copy!(t[1],ind)
    for j = 1:N-1
        d = dims[j]
        tj = t[j]
        tj2 = t[j+1]
        for i = 1:M
            ind2 = div(tj[i]-1, d)
            tj[i] -= d*ind2
            tj2[i] = ind2+1
        end
    end
    return t
end

function ind2sub!{T<:Integer}(sub::Array{T}, dims::Tuple{Vararg{T}}, ind::T)
    ndims = length(dims)
    for i=1:ndims-1
        ind2 = div(ind-1,dims[i])+1
        sub[i] = ind - dims[i]*(ind2-1)
        ind = ind2
    end
    sub[ndims] = ind
    return sub
end

## iteration utilities ##

"""
    foreach(f, c...) -> Void

Call function `f` on each element of iterable `c`.
For multiple iterable arguments, `f` is called elementwise.
`foreach` should be used instead of `map` when the results of `f` are not
needed, for example in `foreach(println, array)`.
"""
foreach(f) = (f(); nothing)
foreach(f, itr) = (for x in itr; f(x); end; nothing)
foreach(f, itrs...) = (for z in zip(itrs...); f(z...); end; nothing)

## map over arrays ##

## transform any set of dimensions
## dims specifies which dimensions will be transformed. for example
## dims==1:2 will call f on all slices A[:,:,...]
mapslices(f, A::AbstractArray, dims) = mapslices(f, A, [dims...])
function mapslices(f, A::AbstractArray, dims::AbstractVector)
    if isempty(dims)
        return map(f,A)
    end

    dimsA = [size(A)...]
    ndimsA = ndims(A)
    alldims = [1:ndimsA;]

    otherdims = setdiff(alldims, dims)

    idx = Array{Any,1}(ndimsA)
    fill!(idx, 1)
    Asliceshape = tuple(dimsA[dims]...)
    itershape   = tuple(dimsA[otherdims]...)
    for d in dims
        idx[d] = 1:size(A,d)
    end

    r1 = f(reshape(A[idx...], Asliceshape))

    # determine result size and allocate
    Rsize = copy(dimsA)
    # TODO: maybe support removing dimensions
    if !isa(r1, AbstractArray) || ndims(r1) == 0
        r1 = [r1]
    end
    Rsize[dims] = [size(r1)...; ones(Int,max(0,length(dims)-ndims(r1)))]
    R = similar(r1, tuple(Rsize...))

    ridx = Array{Any,1}(ndims(R))
    fill!(ridx, 1)
    for d in dims
        ridx[d] = 1:size(R,d)
    end

    R[ridx...] = r1

    first = true
    nidx = length(otherdims)
    for I in CartesianRange(itershape)
        if first
            first = false
        else
            for i in 1:nidx
                idx[otherdims[i]] = ridx[otherdims[i]] = I.I[i]
            end
            R[ridx...] = f(reshape(A[idx...], Asliceshape))
        end
    end

    return R
end

# These are needed because map(eltype, As) is not inferrable
promote_eltype_op(::Any) = (@_pure_meta; Bottom)
promote_eltype_op{T}(op, ::AbstractArray{T}) = (@_pure_meta; promote_op(op, T))
promote_eltype_op{T}(op, ::T               ) = (@_pure_meta; promote_op(op, T))
promote_eltype_op{R,S}(op, ::AbstractArray{R}, ::AbstractArray{S}) = (@_pure_meta; promote_op(op, R, S))
promote_eltype_op{R,S}(op, ::AbstractArray{R}, ::S) = (@_pure_meta; promote_op(op, R, S))
promote_eltype_op{R,S}(op, ::R, ::AbstractArray{S}) = (@_pure_meta; promote_op(op, R, S))
promote_eltype_op(op, A, B, C, D...) = (@_pure_meta; promote_op(op, eltype(A), promote_eltype_op(op, B, C, D...)))

## 1 argument
map!{F}(f::F, A::AbstractArray) = map!(f, A, A)
function map!{F}(f::F, dest::AbstractArray, A::AbstractArray)
    for (i,j) in zip(eachindex(dest),eachindex(A))
        dest[i] = f(A[j])
    end
    return dest
end

# map on collections
map(f, A::Union{AbstractArray,AbstractSet,Associative}) = collect_similar(A, Generator(f,A))

# default to returning an Array for `map` on general iterators
map(f, A) = collect(Generator(f,A))

## 2 argument
function map!{F}(f::F, dest::AbstractArray, A::AbstractArray, B::AbstractArray)
    for (i, j, k) in zip(eachindex(dest), eachindex(A), eachindex(B))
        dest[i] = f(A[j], B[k])
    end
    return dest
end

## N argument

ith_all(i, ::Tuple{}) = ()
ith_all(i, as) = (as[1][i], ith_all(i, tail(as))...)

function map_n!{F}(f::F, dest::AbstractArray, As)
    n = length(As[1])
    for i = 1:n #Fixme iter, one might make a @generated function here
        dest[i] = f(ith_all(i, As)...)
    end
    return dest
end

map!{F}(f::F, dest::AbstractArray, As::AbstractArray...) = map_n!(f, dest, As)

map(f, iters...) = collect(Generator(f, iters...))

# multi-item push!, unshift! (built on top of type-specific 1-item version)
# (note: must not cause a dispatch loop when 1-item case is not defined)
push!(A, a, b) = push!(push!(A, a), b)
push!(A, a, b, c...) = push!(push!(A, a, b), c...)
unshift!(A, a, b) = unshift!(unshift!(A, b), a)
unshift!(A, a, b, c...) = unshift!(unshift!(A, c...), a, b)

## hashing collections ##

const hashaa_seed = UInt === UInt64 ? 0x7f53e68ceb575e76 : 0xeb575e76
const hashrle_seed = UInt == UInt64 ? 0x2aab8909bfea414c : 0xbfea414c
function hash(a::AbstractArray, h::UInt)
    h += hashaa_seed
    h += hash(size(a))

    state = start(a)
    done(a, state) && return h
    x2, state = next(a, state)
    done(a, state) && return hash(x2, h)

    x1 = x2
    while !done(a, state)
        x1 = x2
        x2, state = next(a, state)
        if isequal(x2, x1)
            # For repeated elements, use run length encoding
            # This allows efficient hashing of sparse arrays
            runlength = 2
            while !done(a, state)
                x2, state = next(a, state)
                isequal(x1, x2) || break
                runlength += 1
            end
            h += hashrle_seed
            h = hash(runlength, h)
        end
        h = hash(x1, h)
    end
    !isequal(x2, x1) && (h = hash(x2, h))
    return h
end
