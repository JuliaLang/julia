# This file is a part of Julia. License is MIT: http://julialang.org/license

## Type aliases for convenience ##

typealias AbstractVector{T} AbstractArray{T,1}
typealias AbstractMatrix{T} AbstractArray{T,2}
typealias AbstractVecOrMat{T} Union{AbstractVector{T}, AbstractMatrix{T}}
typealias RangeIndex Union{Int, Range{Int}, UnitRange{Int}, Colon}

## Basic functions ##

vect() = Array(Any, 0)
vect{T}(X::T...) = T[ X[i] for i=1:length(X) ]

const _oldstyle_array_vcat_ = true

if _oldstyle_array_vcat_
    function oldstyle_vcat_warning(n::Int)
        if n == 1
            before = "[a]"
            after  = "collect(a)"
        elseif n == 2
            before = "[a,b]"
            after  = "[a;b]"
        else
            before = "[a,b,...]"
            after  = "[a;b;...]"
        end
        depwarn("$before concatenation is deprecated; use $after instead", :vect)
    end
    function vect(A::AbstractArray...)
        oldstyle_vcat_warning(length(A))
        vcat(A...)
    end
    function vect(X...)
        for a in X
            if typeof(a) <: AbstractArray
                oldstyle_vcat_warning(length(X))
                break
            end
        end
        vcat(X...)
    end
else
    function vect(X...)
        T = promote_typeof(X...)
        #T[ X[i] for i=1:length(X) ]
        # TODO: this is currently much faster. should figure out why. not clear.
        copy!(Array(T,length(X)), X)
    end
end

size{T,n}(t::AbstractArray{T,n}, d) = d <= n ? size(t)[d] : 1
size(x, d1::Integer, d2::Integer, dx::Integer...) = tuple(size(x, d1), size(x, d2, dx...)...)
eltype{T}(::Type{AbstractArray{T}}) = T
eltype{T,n}(::Type{AbstractArray{T,n}}) = T
iseltype(x,T) = eltype(x) <: T
elsize{T}(::AbstractArray{T}) = sizeof(T)
ndims{T,n}(::AbstractArray{T,n}) = n
ndims{T,n}(::Type{AbstractArray{T,n}}) = n
ndims{T<:AbstractArray}(::Type{T}) = ndims(super(T))
length(t::AbstractArray) = prod(size(t))::Int
endof(a::AbstractArray) = length(a)
first(a::AbstractArray) = a[1]
first(a) = (s = start(a); done(a, s) ? throw(ArgumentError("collection must be non-empty")) : next(a, s)[1])
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

# The real @inline macro is not available this early in the bootstrap, so this
# internal macro splices the meta Expr directly into the function body.
macro _inline_meta()
    Expr(:meta, :inline)
end
macro _noinline_meta()
    Expr(:meta, :noinline)
end

## Bounds checking ##
@generated function trailingsize{T,N,n}(A::AbstractArray{T,N}, ::Type{Val{n}})
    n > N && return 1
    ex = :(size(A, $n))
    for m = n+1:N
        ex = :($ex * size(A, $m))
    end
    Expr(:block, Expr(:meta, :inline), ex)
end

_checkbounds(sz, i::Integer) = 1 <= i <= sz
_checkbounds(sz, i::Real) = 1 <= to_index(i) <= sz
_checkbounds(sz, I::AbstractVector{Bool}) = length(I) == sz
_checkbounds(sz, r::Range{Int}) = (@_inline_meta; isempty(r) || (minimum(r) >= 1 && maximum(r) <= sz))
_checkbounds{T<:Real}(sz, r::Range{T}) = (@_inline_meta; _checkbounds(sz, to_index(r)))
_checkbounds(sz, ::Colon) = true
function _checkbounds{T <: Real}(sz, I::AbstractArray{T})
    @_inline_meta
    b = true
    for i in I
        b &= _checkbounds(sz, i)
    end
    b
end
# Prevent allocation of a GC frame by hiding the BoundsError in a noinline function
throw_boundserror(A, I) = (@_noinline_meta; throw(BoundsError(A, I)))

checkbounds(A::AbstractArray, I::AbstractArray{Bool}) = size(A) == size(I) || throw_boundserror(A, I)
checkbounds(A::AbstractArray, I::AbstractVector{Bool}) = length(A) == length(I) || throw_boundserror(A, I)
checkbounds(A::AbstractArray, I::Union{Real,AbstractArray,Colon}) = (@_inline_meta; _checkbounds(length(A), I) || throw_boundserror(A, I))
function checkbounds(A::AbstractMatrix, I::Union{Real,AbstractArray,Colon}, J::Union{Real,AbstractArray,Colon})
    @_inline_meta
    (_checkbounds(size(A,1), I) && _checkbounds(size(A,2), J)) || throw_boundserror(A, (I, J))
end
function checkbounds(A::AbstractArray, I::Union{Real,AbstractArray,Colon}, J::Union{Real,AbstractArray,Colon})
    @_inline_meta
    (_checkbounds(size(A,1), I) && _checkbounds(trailingsize(A,Val{2}), J)) || throw_boundserror(A, (I, J))
end
@generated function checkbounds(A::AbstractArray, I::Union{Real,AbstractArray,Colon}...)
    meta = Expr(:meta, :inline)
    N = length(I)
    Isplat = [:(I[$d]) for d=1:N]
    error = :(throw_boundserror(A, tuple($(Isplat...))))
    args = Expr[:(_checkbounds(size(A,$dim), I[$dim]) || $error) for dim in 1:N-1]
    push!(args, :(_checkbounds(trailingsize(A,Val{$N}), I[$N]) || $error))
    Expr(:block, meta, args...)
end

## Bounds-checking without errors ##
in_bounds(l::Int, i::Integer) = 1 <= i <= l
function in_bounds(sz::Dims, I::Int...)
    n = length(I)
    for dim = 1:(n-1)
        1 <= I[dim] <= sz[dim] || return false
    end
    s = sz[n]
    for i = n+1:length(sz)
        s *= sz[i]
    end
    1 <= I[n] <= s
end

## Constructors ##

# default arguments to similar()
similar{T}(a::AbstractArray{T})               = similar(a, T, size(a))
similar(   a::AbstractArray, T)               = similar(a, T, size(a))
similar{T}(a::AbstractArray{T}, dims::Dims)   = similar(a, T, dims)
similar{T}(a::AbstractArray{T}, dims::Int...) = similar(a, T, dims)
similar(   a::AbstractArray, T, dims::Int...) = similar(a, T, dims)
# similar creates an Array by default
similar(   a::AbstractArray, T, dims::Dims)   = Array(T, dims)

function reshape(a::AbstractArray, dims::Dims)
    if prod(dims) != length(a)
        throw(ArgumentError("dimensions must be consistent with array size"))
    end
    copy!(similar(a, dims), a)
end
reshape(a::AbstractArray, dims::Int...) = reshape(a, dims)

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
    doffs < 1 && throw(BoundsError())
    st = start(src)
    i, dmax = doffs, length(dest)
    @inbounds while !done(src, st)
        i > dmax && throw(BoundsError())
        val, st = next(src, st)
        dest[i] = val
        i += 1
    end
    return dest
end

function copy!(dest::AbstractArray, doffs::Integer, src, soffs::Integer)
    if (doffs < 1) | (soffs < 1)
        throw(BoundsError())
    end
    st = start(src)
    for j = 1:(soffs-1)
        done(src, st) && throw(BoundsError())
        _, st = next(src, st)
    end
    dn = done(src, st)
    dn && throw(BoundsError())
    i, dmax = doffs, length(dest)
    @inbounds while !dn
        i > dmax && throw(BoundsError())
        val, st = next(src, st)
        dest[i] = val
        i += 1
        dn = done(src, st)
    end
    return dest
end

# this method must be separate from the above since src might not have a length
function copy!(dest::AbstractArray, doffs::Integer, src, soffs::Integer, n::Integer)
    n < 0 && throw(BoundsError())
    n == 0 && return dest
    dmax = doffs + n - 1
    if (dmax > length(dest)) | (doffs < 1) | (soffs < 1)
        throw(BoundsError())
    end
    st = start(src)
    for j = 1:(soffs-1)
        done(src, st) && throw(BoundsError())
        _, st = next(src, st)
    end
    i = doffs
    @inbounds while i <= dmax && !done(src, st)
        val, st = next(src, st)
        dest[i] = val
        i += 1
    end
    i <= dmax && throw(BoundsError())
    return dest
end

## copy between abstract arrays - generally more efficient
## since a single index variable can be used.

function copy!(dest::AbstractArray, src::AbstractArray)
    n = length(src)
    if n > length(dest)
        throw(BoundsError())
    end
    @inbounds for i = 1:n
        dest[i] = src[i]
    end
    return dest
end

function copy!(dest::AbstractArray, doffs::Integer, src::AbstractArray)
    copy!(dest, doffs, src, 1, length(src))
end
function copy!(dest::AbstractArray, doffs::Integer, src::AbstractArray, soffs::Integer)
    soffs > length(src) && throw(BoundsError())
    copy!(dest, doffs, src, soffs, length(src)-soffs+1)
end
function copy!(dest::AbstractArray, doffs::Integer, src::AbstractArray, soffs::Integer, n::Integer)
    n < 0 && throw(BoundsError())
    n == 0 && return dest
    if soffs+n-1 > length(src) || doffs+n-1 > length(dest) || doffs < 1 || soffs < 1
        throw(BoundsError())
    end
    @inbounds for i = 0:(n-1)
        dest[doffs+i] = src[soffs+i]
    end
    return dest
end

copy(a::AbstractArray) = copy!(similar(a), a)

function copy!{R,S}(B::AbstractVecOrMat{R}, ir_dest::Range{Int}, jr_dest::Range{Int}, A::AbstractVecOrMat{S}, ir_src::Range{Int}, jr_src::Range{Int})
    if length(ir_dest) != length(ir_src) || length(jr_dest) != length(jr_src)
        throw(ArgumentError("source and destination must have same size"))
    end
    checkbounds(B, ir_dest, jr_dest)
    checkbounds(A, ir_src, jr_src)
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

function copy_transpose!{R,S}(B::AbstractVecOrMat{R}, ir_dest::Range{Int}, jr_dest::Range{Int}, A::AbstractVecOrMat{S}, ir_src::Range{Int}, jr_src::Range{Int})
    if length(ir_dest) != length(jr_src) || length(jr_dest) != length(ir_src)
        throw(ArgumentError("source and destination must have same size"))
    end
    checkbounds(B, ir_dest, jr_dest)
    checkbounds(A, ir_src, jr_src)
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
eachindex(::LinearFast, A::AbstractArray) = 1:length(A)

function eachindex(A::AbstractArray, B::AbstractArray)
    @_inline_meta
    eachindex(linearindexing(A,B), A, B)
end
function eachindex(A::AbstractArray, B::AbstractArray...)
    @_inline_meta
    eachindex(linearindexing(A,B...), A, B...)
end
eachindex(::LinearFast, A::AbstractArray, B::AbstractArray) = 1:max(length(A),length(B))
eachindex(::LinearFast, A::AbstractArray, B::AbstractArray...) = 1:max(length(A), map(length, B)...)

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
map{T<:FloatingPoint}(::Type{T}, r::FloatRange) = FloatRange(T(r.start), T(r.step), r.len, T(r.divisor))

## unsafe/pointer conversions ##

# note: the following type definitions don't mean any AbstractArray is convertible to
# a data Ref. they just map the array element type to the pointer type for
# convenience in cases that work.
pointer{T}(x::AbstractArray{T}) = unsafe_convert(Ptr{T}, x)
pointer{T}(x::AbstractArray{T}, i::Integer) = unsafe_convert(Ptr{T},x) + (i-1)*elsize(x)


## Approach:
# We only define one fallback method on getindex for all argument types.
# That dispatches to an (inlined) internal _getindex function, where the goal is
# to transform the indices such that we can call the only getindex method that
# we require AbstractArray subtypes must define, either:
#       getindex(::T, ::Int) # if linearindexing(T) == LinearFast()
#       getindex(::T, ::Int, ::Int, #=...ndims(A) indices...=#) if LinearSlow()
# Unfortunately, it is currently impossible to express the latter method for
# arbitrary dimensionalities. We could get around that with ::CartesianIndex{N},
# but that isn't as obvious and would require that the function be inlined to
# avoid allocations.  If the subtype hasn't defined those methods, it goes back
# to the _getindex function where an error is thrown to prevent stack overflows.
#
# We use the same scheme for unsafe_getindex, with the exception that we can
# fallback to the safe version if the subtype hasn't defined the required
# unsafe method.

function getindex(A::AbstractArray, I...)
    @_inline_meta
    _getindex(linearindexing(A), A, I...)
end
function unsafe_getindex(A::AbstractArray, I...)
    @_inline_meta
    _unsafe_getindex(linearindexing(A), A, I...)
end
## Internal defitions
# 0-dimensional indexing is defined to prevent ambiguities. LinearFast is easy:
_getindex(::LinearFast, A::AbstractArray) = (@_inline_meta; getindex(A, 1))
# But LinearSlow must take into account the dimensionality of the array:
_getindex{T}(::LinearSlow, A::AbstractArray{T,0}) = error("indexing not defined for ", typeof(A))
_getindex(::LinearSlow, A::AbstractVector) = (@_inline_meta; getindex(A, 1))
_getindex(l::LinearSlow, A::AbstractArray) = (@_inline_meta; _getindex(l, A, 1))
_unsafe_getindex(::LinearFast, A::AbstractArray) = (@_inline_meta; unsafe_getindex(A, 1))
_unsafe_getindex{T}(::LinearSlow, A::AbstractArray{T,0}) = error("indexing not defined for ", typeof(A))
_unsafe_getindex(::LinearSlow, A::AbstractVector) = (@_inline_meta; unsafe_getindex(A, 1))
_unsafe_getindex(l::LinearSlow, A::AbstractArray) = (@_inline_meta; _unsafe_getindex(l, A, 1))

_getindex(::LinearIndexing, A::AbstractArray, I...) = error("indexing $(typeof(A)) with types $(typeof(I)) is not supported")
_unsafe_getindex(::LinearIndexing, A::AbstractArray, I...) = error("indexing $(typeof(A)) with types $(typeof(I)) is not supported")

## LinearFast Scalar indexing
_getindex(::LinearFast, A::AbstractArray, I::Int) = error("indexing not defined for ", typeof(A))
function _getindex(::LinearFast, A::AbstractArray, I::Real...)
    @_inline_meta
    # We must check bounds for sub2ind; so we can then call unsafe_getindex
    checkbounds(A, I...)
    unsafe_getindex(A, sub2ind(size(A), to_indexes(I...)...))
end
_unsafe_getindex(::LinearFast, A::AbstractArray, I::Int) = (@_inline_meta; getindex(A, I))
function _unsafe_getindex(::LinearFast, A::AbstractArray, I::Real...)
    @_inline_meta
    unsafe_getindex(A, sub2ind(size(A), to_indexes(I...)...))
end

# LinearSlow Scalar indexing
@generated function _getindex{T,AN}(::LinearSlow, A::AbstractArray{T,AN}, I::Real...)
    N = length(I)
    if N == AN
        :(error("indexing not defined for ", typeof(A)))
    elseif N > AN
        # Drop trailing ones
        Isplat = Expr[:(to_index(I[$d])) for d = 1:AN]
        Osplat = Expr[:(to_index(I[$d]) == 1) for d = AN+1:N]
        quote
            $(Expr(:meta, :inline))
            (&)($(Osplat...)) || throw_boundserror(A, I)
            getindex(A, $(Isplat...))
        end
    else
        # Expand the last index into the appropriate number of indices
        Isplat = Expr[:(to_index(I[$d])) for d = 1:N-1]
        i = 0
        for d=N:AN
            push!(Isplat, :(s[$(i+=1)]))
        end
        sz = Expr(:tuple)
        sz.args = Expr[:(size(A, $d)) for d=N:AN]
        quote
            $(Expr(:meta, :inline))
            # ind2sub requires all dimensions to be nonzero, so checkbounds first
            checkbounds(A, I...)
            s = ind2sub($sz, to_index(I[$N]))
            unsafe_getindex(A, $(Isplat...))
        end
    end
end
@generated function _unsafe_getindex{T,AN}(::LinearSlow, A::AbstractArray{T,AN}, I::Real...)
    N = length(I)
    if N == AN
        Isplat = Expr[:(to_index(I[$d])) for d = 1:N]
        :($(Expr(:meta, :inline)); getindex(A, $(Isplat...)))
    elseif N > AN
        # Drop trailing dimensions (unchecked)
        Isplat = Expr[:(to_index(I[$d])) for d = 1:AN]
        quote
            $(Expr(:meta, :inline))
            unsafe_getindex(A, $(Isplat...))
        end
    else
        # Expand the last index into the appropriate number of indices
        Isplat = Expr[:(to_index(I[$d])) for d = 1:N-1]
        for d=N:AN
            push!(Isplat, :(s[$(d-N+1)]))
        end
        sz = Expr(:tuple)
        sz.args = Expr[:(size(A, $d)) for d=N:AN]
        quote
            $(Expr(:meta, :inline))
            s = ind2sub($sz, to_index(I[$N]))
            unsafe_getindex(A, $(Isplat...))
        end
    end
end

## Setindex! is defined similarly. We first dispatch to an internal _setindex!
# function that allows dispatch on array storage
function setindex!(A::AbstractArray, v, I...)
    @_inline_meta
    _setindex!(linearindexing(A), A, v, I...)
end
function unsafe_setindex!(A::AbstractArray, v, I...)
    @_inline_meta
    _unsafe_setindex!(linearindexing(A), A, v, I...)
end
## Internal defitions
_setindex!(::LinearFast, A::AbstractArray, v) = (@_inline_meta; setindex!(A, v, 1))
_setindex!{T}(::LinearSlow, A::AbstractArray{T,0}, v) = error("indexing not defined for ", typeof(A))
_setindex!(::LinearSlow, A::AbstractVector, v) = (@_inline_meta; setindex!(A, v, 1))
_setindex!(l::LinearSlow, A::AbstractArray, v) = (@_inline_meta; _setindex!(l, A, v, 1))
_unsafe_setindex!(::LinearFast, A::AbstractArray, v) = (@_inline_meta; unsafe_setindex!(A, v, 1))
_unsafe_setindex!{T}(::LinearSlow, A::AbstractArray{T,0}, v) = error("indexing not defined for ", typeof(A))
_unsafe_setindex!(::LinearSlow, A::AbstractVector, v) = (@_inline_meta; unsafe_setindex!(A, v, 1))
_unsafe_setindex!(l::LinearSlow, A::AbstractArray, v) = (@_inline_meta; _unsafe_setindex!(l, A, v, 1))
_setindex!(::LinearIndexing, A::AbstractArray, v, I...) = error("indexing $(typeof(A)) with types $(typeof(I)) is not supported")
_unsafe_setindex!(::LinearIndexing, A::AbstractArray, v, I...) = error("indexing $(typeof(A)) with types $(typeof(I)) is not supported")

## LinearFast Scalar indexing
_setindex!(::LinearFast, A::AbstractArray, v, I::Int) = error("indexed assignment not defined for ", typeof(A))
function _setindex!(::LinearFast, A::AbstractArray, v, I::Real...)
    @_inline_meta
    # We must check bounds for sub2ind; so we can then call unsafe_setindex!
    checkbounds(A, I...)
    unsafe_setindex!(A, v, sub2ind(size(A), to_indexes(I...)...))
end
_unsafe_setindex!(::LinearFast, A::AbstractArray, v, I::Int) = (@_inline_meta; setindex!(A, v, I))
function _unsafe_setindex!(::LinearFast, A::AbstractArray, v, I::Real...)
    @_inline_meta
    unsafe_setindex!(A, v, sub2ind(size(A), to_indexes(I...)...))
end

# LinearSlow Scalar indexing
@generated function _setindex!{T,AN}(::LinearSlow, A::AbstractArray{T,AN}, v, I::Real...)
    N = length(I)
    if N == AN
        :(error("indexed assignment not defined for ", typeof(A)))
    elseif N > AN
        # Drop trailing ones
        Isplat = Expr[:(to_index(I[$d])) for d = 1:AN]
        Osplat = Expr[:(to_index(I[$d]) == 1) for d = AN+1:N]
        quote
            $(Expr(:meta, :inline))
            (&)($(Osplat...)) || throw_boundserror(A, I)
            setindex!(A, v, $(Isplat...))
        end
    else
        # Expand the last index into the appropriate number of indices
        Isplat = Expr[:(to_index(I[$d])) for d = 1:N-1]
        i = 0
        for d=N:AN
            push!(Isplat, :(s[$(i+=1)]))
        end
        sz = Expr(:tuple)
        sz.args = Expr[:(size(A, $d)) for d=N:AN]
        quote
            $(Expr(:meta, :inline))
            checkbounds(A, I...)
            s = ind2sub($sz, to_index(I[$N]))
            unsafe_setindex!(A, v, $(Isplat...))
        end
    end
end
@generated function _unsafe_setindex!{T,AN}(::LinearSlow, A::AbstractArray{T,AN}, v, I::Real...)
    N = length(I)
    if N == AN
        Isplat = Expr[:(to_index(I[$d])) for d = 1:N]
        :(setindex!(A, v, $(Isplat...)))
    elseif N > AN
        # Drop trailing dimensions (unchecked)
        Isplat = Expr[:(to_index(I[$d])) for d = 1:AN]
        quote
            $(Expr(:meta, :inline))
            unsafe_setindex!(A, v, $(Isplat...))
        end
    else
        # Expand the last index into the appropriate number of indices
        Isplat = Expr[:(to_index(I[$d])) for d = 1:N-1]
        for d=N:AN
            push!(Isplat, :(s[$(d-N+1)]))
        end
        sz = Expr(:tuple)
        sz.args = Expr[:(size(A, $d)) for d=N:AN]
        quote
            $(Expr(:meta, :inline))
            s = ind2sub($sz, to_index(I[$N]))
            unsafe_setindex!(A, v, $(Isplat...))
        end
    end
end

## get (getindex with a default value) ##

typealias RangeVecIntList{A<:AbstractVector{Int}} Union{Tuple{Vararg{Union{Range, AbstractVector{Int}}}}, AbstractVector{UnitRange{Int}}, AbstractVector{Range{Int}}, AbstractVector{A}}

get(A::AbstractArray, i::Integer, default) = in_bounds(length(A), i) ? A[i] : default
get(A::AbstractArray, I::Tuple{}, default) = similar(A, typeof(default), 0)
get(A::AbstractArray, I::Dims, default) = in_bounds(size(A), I...) ? A[I...] : default

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


## Concatenation ##

promote_eltype() = Bottom
promote_eltype(v1, vs...) = promote_type(eltype(v1), promote_eltype(vs...))

#TODO: ERROR CHECK
cat(catdim::Integer) = Array(Any, 0)

vcat() = Array(Any, 0)
hcat() = Array(Any, 0)

## cat: special cases
hcat{T}(X::T...)         = T[ X[j] for i=1, j=1:length(X) ]
hcat{T<:Number}(X::T...) = T[ X[j] for i=1, j=1:length(X) ]
vcat{T}(X::T...)         = T[ X[i] for i=1:length(X) ]
vcat{T<:Number}(X::T...) = T[ X[i] for i=1:length(X) ]

function vcat(X::Number...)
    T = promote_typeof(X...)
    hvcat_fill(Array(T,length(X)), X)
end

function hcat(X::Number...)
    T = promote_typeof(X...)
    hvcat_fill(Array(T,1,length(X)), X)
end

function vcat{T}(V::AbstractVector{T}...)
    n = 0
    for Vk in V
        n += length(Vk)
    end
    a = similar(full(V[1]), n)
    pos = 1
    for k=1:length(V)
        Vk = V[k]
        p1 = pos+length(Vk)-1
        a[pos:p1] = Vk
        pos = p1+1
    end
    a
end

function hcat{T}(A::AbstractVecOrMat{T}...)
    nargs = length(A)
    nrows = size(A[1], 1)
    ncols = 0
    dense = true
    for j = 1:nargs
        Aj = A[j]
        if size(Aj, 1) != nrows
            throw(ArgumentError("number of rows must match"))
        end
        dense &= isa(Aj,Array)
        nd = ndims(Aj)
        ncols += (nd==2 ? size(Aj,2) : 1)
    end
    B = similar(full(A[1]), nrows, ncols)
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

function vcat{T}(A::AbstractMatrix{T}...)
    nargs = length(A)
    nrows = sum(a->size(a, 1), A)::Int
    ncols = size(A[1], 2)
    for j = 2:nargs
        if size(A[j], 2) != ncols
            throw(ArgumentError("number of columns must match"))
        end
    end
    B = similar(full(A[1]), nrows, ncols)
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
            if dims2cat[d]==0
                dimsC[d] == currentdim || throw(DimensionMismatch("mismatch in dimension $(d)"))
            else
                dimsC[d] += currentdim
                catsizes[i,dims2cat[d]] = currentdim
            end
        end
    end

    C = similar(isa(X[1],AbstractArray) ? full(X[1]) : [X[1]], typeC, tuple(dimsC...))
    if length(catdims)>1
        fill!(C,0)
    end

    offsets = zeros(Int,length(catdims))
    for i=1:nargs
        cat_one = [ dims2cat[d]==0 ? (1:dimsC[d]) : (offsets[dims2cat[d]]+(1:catsizes[i,dims2cat[d]])) for d=1:ndimsC]
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
    if mod(n,nbc) != 0
        throw(ArgumentError("all rows must have the same number of block columns"))
    end
    nbr = div(n,nbc)
    hvcat(ntuple(i->nbc, nbr), as...)
end

function hvcat{T}(rows::Tuple{Vararg{Int}}, as::AbstractMatrix{T}...)
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
                throw(ArgumentError("mismatched height in block row $(i)"))
            end
            if c-1+szj > nc
                throw(ArgumentError("block row $(i) has mismatched number of columns"))
            end
            out[r:r-1+szi, c:c-1+szj] = Aj
            c += szj
        end
        if c != nc+1
            throw(ArgumentError("block row $(i) has mismatched number of columns"))
        end
        r += szi
        a += rows[i]
    end
    out
end

hvcat(rows::Tuple{Vararg{Int}}) = []

function hvcat{T<:Number}(rows::Tuple{Vararg{Int}}, xs::T...)
    nr = length(rows)
    nc = rows[1]

    a = Array(T, nr, nc)
    if length(a) != length(xs)
        throw(ArgumentError("argument count does not match specified shape"))
    end
    k = 1
    @inbounds for i=1:nr
        if nc != rows[i]
            throw(ArgumentError("row $(i) has mismatched number of columns"))
        end
        for j=1:nc
            a[i,j] = xs[k]
            k += 1
        end
    end
    a
end

function hvcat_fill(a, xs)
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

function typed_hvcat(T::Type, rows::Tuple{Vararg{Int}}, xs::Number...)
    nr = length(rows)
    nc = rows[1]
    for i = 2:nr
        if nc != rows[i]
            throw(ArgumentError("row $(i) has mismatched number of columns"))
        end
    end
    len = length(xs)
    if nr*nc != len
        throw(ArgumentError("argument count $(len) does not match specified shape $((nr,nc))"))
    end
    hvcat_fill(Array(T, nr, nc), xs)
end

function hvcat(rows::Tuple{Vararg{Int}}, xs::Number...)
    T = promote_typeof(xs...)
    typed_hvcat(T, rows, xs...)
end

# fallback definition of hvcat in terms of hcat and vcat
function hvcat(rows::Tuple{Vararg{Int}}, as...)
    nbr = length(rows)  # number of block rows
    rs = cell(nbr)
    a = 1
    for i = 1:nbr
        rs[i] = hcat(as[a:a-1+rows[i]]...)
        a += rows[i]
    end
    vcat(rs...)
end

function typed_hvcat(T::Type, rows::Tuple{Vararg{Int}}, as...)
    nbr = length(rows)  # number of block rows
    rs = cell(nbr)
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
    for i in eachindex(A)
        if !isequal(A[i], B[i])
            return false
        end
    end
    return true
end

function lexcmp(A::AbstractArray, B::AbstractArray)
    nA, nB = length(A), length(B)
    for i = 1:min(nA, nB)
        res = lexcmp(A[i], B[i])
        res == 0 || return res
    end
    return cmp(nA, nB)
end

function (==)(A::AbstractArray, B::AbstractArray)
    if size(A) != size(B)
        return false
    end
    if isa(A,Range) != isa(B,Range)
        return false
    end
    for i in eachindex(A)
        if !(A[i]==B[i])
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
        push!(exprs,Expr(:(=),symbol(:s,i),:(ind-dims[$i]*ind2+1)))
        push!(exprs,:(ind=ind2))
    end
    push!(exprs,Expr(:(=),symbol(:s,N),:(ind+1)))
    Expr(:block,meta,exprs...,Expr(:tuple,[symbol(:s,i) for i=1:N]...))
end

# TODO in v0.5: either deprecate line 1 or add line 2
ind2sub(a::AbstractArray, ind::Integer) = ind2sub(size(a), ind)
# sub2ind(a::AbstractArray, I::Integer...) = sub2ind(size(a), I...)

function sub2ind{T<:Integer}(dims::Tuple{Vararg{Integer}}, I::AbstractVector{T}...)
    N = length(dims)
    M = length(I[1])
    indices = Array{T}(length(I[1]))
    copy!(indices,I[1])

    s = dims[1]
    for j=2:length(I)
        Ij = I[j]
        for i=1:M
            indices[i] += s*(Ij[i]-1)
        end
        s*= (j <= N ? dims[j] : 1)
    end
    return indices
end

function ind2sub{N,T<:Integer}(dims::NTuple{N,Integer}, ind::AbstractVector{T})
    M = length(ind)
    t = NTuple{N,Vector{T}}(ntuple(n->Array{T}(M),N))
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

# slow, but useful
function cartesianmap(body, t::Tuple{Vararg{Int}}, it...)
    idx = length(t)-length(it)
    if idx == 1
        for i = 1:t[1]
            body(i, it...)
        end
    elseif idx == 2
        for j = 1:t[2]
            for i = 1:t[1]
                body(i, j, it...)
            end
        end
    elseif idx > 1
        for j = 1:t[idx]
            for i = 1:t[idx-1]
                cartesianmap(body, t, i, j, it...)
            end
        end
    else
        throw(ArgumentError("cartesianmap length(t) - length(it) ≤ 0"))
    end
end

cartesianmap(body, t::Tuple{}) = (body(); nothing)

function cartesianmap(body, t::Tuple{Int,})
    for i = 1:t[1]
        body(i)
    end
end

function cartesianmap(body, t::Tuple{Int,Int})
    for j = 1:t[2]
        for i = 1:t[1]
            body(i,j)
        end
    end
end

function cartesianmap(body, t::Tuple{Int,Int,Int})
    for k = 1:t[3]
        for j = 1:t[2]
            for i = 1:t[1]
                body(i,j,k)
            end
        end
    end
end

##
# generic map on any iterator
function map(f, iters...)
    result = []
    len = length(iters)
    states = [start(iters[idx]) for idx in 1:len]
    nxtvals = cell(len)
    cont = true
    for idx in 1:len
        done(iters[idx], states[idx]) && (cont = false; break)
    end
    while cont
        for idx in 1:len
            nxtvals[idx],states[idx] = next(iters[idx], states[idx])
        end
        push!(result, f(nxtvals...))
        for idx in 1:len
            done(iters[idx], states[idx]) && (cont = false; break)
        end
    end
    result
end

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

    idx = cell(ndimsA)
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

    ridx = cell(ndims(R))
    fill!(ridx, 1)
    for d in dims
        ridx[d] = 1:size(R,d)
    end

    R[ridx...] = r1

    first = true
    cartesianmap(itershape) do idxs...
        if first
            first = false
        else
            ia = [idxs...]
            idx[otherdims] = ia
            ridx[otherdims] = ia
            R[ridx...] = f(reshape(A[idx...], Asliceshape))
        end
    end

    return R
end


# using promote_type
function promote_to!{T,F}(f::F, offs, dest::AbstractArray{T}, A::AbstractArray)
    # map to dest array, checking the type of each result. if a result does not
    # match, do a type promotion and re-dispatch.
    @inbounds for i = offs:length(A)
        el = f(A[i])
        S = typeof(el)
        if S === T || S <: T
            dest[i] = el::T
        else
            R = promote_type(T, S)
            if R !== T
                new = similar(dest, R)
                copy!(new,1, dest,1, i-1)
                new[i] = el
                return promote_to!(f, i+1, new, A)
            end
            dest[i] = el
        end
    end
    return dest
end

function map_promote(f, A::AbstractArray)
    if isempty(A); return similar(A, Bottom); end
    first = f(A[1])
    dest = similar(A, typeof(first))
    dest[1] = first
    return promote_to!(f, 2, dest, A)
end

## 1 argument
map!{F}(f::F, A::AbstractArray) = map!(f, A, A)
function map!{F}(f::F, dest::AbstractArray, A::AbstractArray)
    for i = 1:length(A)
        dest[i] = f(A[i])
    end
    return dest
end

function map_to!{T,F}(f::F, offs, dest::AbstractArray{T}, A::AbstractArray)
    # map to dest array, checking the type of each result. if a result does not
    # match, widen the result type and re-dispatch.
    @inbounds for i = offs:length(A)
        el = f(A[i])
        S = typeof(el)
        if S === T || S <: T
            dest[i] = el::T
        else
            R = typejoin(T, S)
            new = similar(dest, R)
            copy!(new,1, dest,1, i-1)
            new[i] = el
            return map_to!(f, i+1, new, A)
        end
    end
    return dest
end

function map(f, A::AbstractArray)
    if isempty(A)
        return isa(f,Type) ? similar(A,f) : similar(A)
    end
    first = f(A[1])
    dest = similar(A, typeof(first))
    dest[1] = first
    return map_to!(f, 2, dest, A)
end

## 2 argument
function map!{F}(f::F, dest::AbstractArray, A::AbstractArray, B::AbstractArray)
    for i = 1:length(A)
        dest[i] = f(A[i], B[i])
    end
    return dest
end

function map_to!{T,F}(f::F, offs, dest::AbstractArray{T}, A::AbstractArray, B::AbstractArray)
    @inbounds for i = offs:length(A)
        el = f(A[i], B[i])
        S = typeof(el)
        if (S !== T) && !(S <: T)
            R = typejoin(T, S)
            new = similar(dest, R)
            copy!(new,1, dest,1, i-1)
            new[i] = el
            return map_to!(f, i+1, new, A, B)
        end
        dest[i] = el::T
    end
    return dest
end

function map(f, A::AbstractArray, B::AbstractArray)
    shp = promote_shape(size(A),size(B))
    if prod(shp) == 0
        return similar(A, promote_type(eltype(A),eltype(B)), shp)
    end
    first = f(A[1], B[1])
    dest = similar(A, typeof(first), shp)
    dest[1] = first
    return map_to!(f, 2, dest, A, B)
end

## N argument

ith_all(i, ::Tuple{}) = ()
ith_all(i, as) = (as[1][i], ith_all(i, tail(as))...)

function map_n!{F}(f::F, dest::AbstractArray, As)
    n = length(As[1])
    for i = 1:n
        dest[i] = f(ith_all(i, As)...)
    end
    return dest
end

map!{F}(f::F, dest::AbstractArray, As::AbstractArray...) = map_n!(f, dest, As)

function map_to_n!{T,F}(f::F, offs, dest::AbstractArray{T}, As)
    @inbounds for i = offs:length(As[1])
        el = f(ith_all(i, As)...)
        S = typeof(el)
        if (S !== T) && !(S <: T)
            R = typejoin(T, S)
            new = similar(dest, R)
            copy!(new,1, dest,1, i-1)
            new[i] = el
            return map_to_n!(f, i+1, new, As)
        end
        dest[i] = el::T
    end
    return dest
end

function map(f, As::AbstractArray...)
    shape = mapreduce(size, promote_shape, As)
    if prod(shape) == 0
        return similar(As[1], promote_eltype(As...), shape)
    end
    first = f(map(a->a[1], As)...)
    dest = similar(As[1], typeof(first), shape)
    dest[1] = first
    return map_to_n!(f, 2, dest, As)
end

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
