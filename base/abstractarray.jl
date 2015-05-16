# This file is a part of Julia. License is MIT: http://julialang.org/license

## Type aliases for convenience ##

typealias AbstractVector{T} AbstractArray{T,1}
typealias AbstractMatrix{T} AbstractArray{T,2}
typealias AbstractVecOrMat{T} Union(AbstractVector{T}, AbstractMatrix{T})

## Basic functions ##

vect() = Array(Any, 0)
vect{T}(X::T...) = T[ X[i] for i=1:length(X) ]

const _oldstyle_array_vcat_ = true

if _oldstyle_array_vcat_
    function oldstyle_vcat_warning(n::Int)
        if n == 1
            before = "a"
            after  = "a;"
        elseif n == 2
            before = "a,b"
            after  = "a;b"
        else
            before = "a,b,..."
            after  = "a;b;..."
        end
        depwarn("[$before] concatenation is deprecated; use [$after] instead", :vect)
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
isinteger(x::AbstractArray) = all(isinteger,x)
isinteger{T<:Integer,n}(x::AbstractArray{T,n}) = true
isreal(x::AbstractArray) = all(isreal,x)
isreal{T<:Real,n}(x::AbstractArray{T,n}) = true
ndims{T,n}(::AbstractArray{T,n}) = n
ndims{T,n}(::Type{AbstractArray{T,n}}) = n
ndims{T<:AbstractArray}(::Type{T}) = ndims(super(T))
length(t::AbstractArray) = prod(size(t))::Int
endof(a::AbstractArray) = length(a)
first(a::AbstractArray) = a[1]
first(a) = (s = start(a); done(a, s) ? throw(ArgumentError("collection must be non-empty")) : next(a, s)[1])
last(a) = a[end]
ctranspose(a::AbstractArray) = error("ctranspose not implemented for $(typeof(a)). Consider adding parentheses, e.g. A*(B*C') instead of A*B*C' to avoid explicit calculation of the transposed matrix.")
transpose(a::AbstractArray) = error("transpose not implemented for $(typeof(a)). Consider adding parentheses, e.g. A*(B*C.') instead of A*B*C' to avoid explicit calculation of the transposed matrix.")

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

strides(a::AbstractArray) = ntuple(ndims(a), i->stride(a,i))::Dims

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

*(::LinearFast, ::LinearFast) = LinearFast()
*(::LinearSlow, ::LinearFast) = LinearSlow()
*(::LinearFast, ::LinearSlow) = LinearSlow()
*(::LinearSlow, ::LinearSlow) = LinearSlow()

## Bounds checking ##
checkbounds(sz::Int, ::Colon) = nothing
checkbounds(sz::Int, i::Int) = 1 <= i <= sz || throw(BoundsError())
checkbounds(sz::Int, i::Real) = checkbounds(sz, to_index(i))
checkbounds(sz::Int, I::AbstractVector{Bool}) = length(I) == sz || throw(BoundsError())
checkbounds(sz::Int, r::Range{Int}) = isempty(r) || (minimum(r) >= 1 && maximum(r) <= sz) || throw(BoundsError())
checkbounds{T<:Real}(sz::Int, r::Range{T}) = checkbounds(sz, to_index(r))

function checkbounds{T <: Real}(sz::Int, I::AbstractArray{T})
    for i in I
        checkbounds(sz, i)
    end
end

checkbounds(A::AbstractArray, I::AbstractArray{Bool}) = size(A) == size(I) || throw(BoundsError())

checkbounds(A::AbstractArray, I) = checkbounds(length(A), I)

function checkbounds(A::AbstractMatrix, I::Union(Real,Colon,AbstractArray), J::Union(Real,Colon,AbstractArray))
    checkbounds(size(A,1), I)
    checkbounds(size(A,2), J)
end

function checkbounds(A::AbstractArray, I::Union(Real,Colon,AbstractArray), J::Union(Real,Colon,AbstractArray))
    checkbounds(size(A,1), I)
    checkbounds(trailingsize(A,2), J)
end

function checkbounds(A::AbstractArray, I::Union(Real,Colon,AbstractArray)...)
    n = length(I)
    if n > 0
        for dim = 1:(n-1)
            checkbounds(size(A,dim), I[dim])
        end
        checkbounds(trailingsize(A,n), I[n])
    end
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
similar   (a::AbstractArray, T)               = similar(a, T, size(a))
similar{T}(a::AbstractArray{T}, dims::Dims)   = similar(a, T, dims)
similar{T}(a::AbstractArray{T}, dims::Int...) = similar(a, T, dims)
similar   (a::AbstractArray, T, dims::Int...) = similar(a, T, dims)

function reshape(a::AbstractArray, dims::Dims)
    if prod(dims) != length(a)
        throw(ArgumentError("dimensions must be consistent with array size"))
    end
    copy!(similar(a, dims), a)
end
reshape(a::AbstractArray, dims::Int...) = reshape(a, dims)

vec(a::AbstractArray) = reshape(a,length(a))
vec(a::AbstractVector) = a

_sub(::Tuple{}, ::Tuple{}) = ()
_sub(t::Tuple, ::Tuple{}) = t
_sub(t::Tuple, s::Tuple) = _sub(tail(t), tail(s))

function squeeze(A::AbstractArray, dims::Dims)
    for i in 1:length(dims)
        1 <= dims[i] <= ndims(A) || throw(ArgumentError("squeezed dims must be in range 1:ndims(A)"))
        size(A, dims[i]) == 1 || throw(ArgumentError("squeezed dims must all be size 1"))
        for j = 1:i-1
            dims[j] == dims[i] && throw(ArgumentError("squeezed dims must be unique"))
        end
    end
    d = ()
    for i = 1:ndims(A)
        if !in(i, dims)
            d = tuple(d..., size(A, i))
        end
    end
    reshape(A, d::typeof(_sub(size(A), dims)))
end

squeeze(A::AbstractArray, dim::Integer) = squeeze(A, (Int(dim),))

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
# inlining. The real @inline macro is not available this early in the bootstrap,
# so this internal macro splices the meta Expr directly into the function body.
macro _inline_meta()
    Expr(:meta, :inline)
end
start(A::AbstractArray) = (@_inline_meta(); itr = eachindex(A); (itr, start(itr)))
next(A::AbstractArray,i) = (@_inline_meta(); (idx, s) = next(i[1], i[2]); (A[idx], (i[1], s)))
done(A::AbstractArray,i) = done(i[1], i[2])

# eachindex iterates over all indices. LinearSlow definitions are later.
eachindex(A::AbstractArray) = (@_inline_meta(); eachindex(linearindexing(A), A))
eachindex(::LinearFast, A::AbstractArray) = 1:length(A)

function eachindex(A::AbstractArray, B::AbstractArray)
    @_inline_meta
    eachindex(linearindexing(A)*linearindexing(B), A, B)
end
eachindex(::LinearFast, A::AbstractArray, B::AbstractArray) = 1:max(length(A),length(B))

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

## Unary operators ##

conj{T<:Real}(x::AbstractArray{T}) = x
conj!{T<:Real}(x::AbstractArray{T}) = x

real{T<:Real}(x::AbstractArray{T}) = x
imag{T<:Real}(x::AbstractArray{T}) = zero(x)

+{T<:Number}(x::AbstractArray{T}) = x
*{T<:Number}(x::AbstractArray{T,2}) = x

## Binary arithmetic operators ##

*(A::Number, B::AbstractArray) = A .* B
*(A::AbstractArray, B::Number) = A .* B

/(A::AbstractArray, B::Number) = A ./ B

\(A::Number, B::AbstractArray) = B ./ A

## Indexing: getindex ##

getindex(t::AbstractArray, i::Real) = error("indexing not defined for ", typeof(t))

# linear indexing with a single multi-dimensional index
function getindex(A::AbstractArray, I::AbstractArray)
    x = similar(A, size(I))
    for i in eachindex(I)
        x[i] = A[I[i]]
    end
    return x
end

# index A[:,:,...,i,:,:,...] where "i" is in dimension "d"
# TODO: more optimized special cases
slicedim(A::AbstractArray, d::Integer, i) =
    A[[ n==d ? i : (1:size(A,n)) for n in 1:ndims(A) ]...]

function flipdim(A::AbstractVector, d::Integer)
    d > 0 || throw(ArgumentError("dimension to flip must be positive"))
    d == 1 || return copy(A)
    reverse(A)
end

function flipdim(A::AbstractArray, d::Integer)
    nd = ndims(A)
    sd = d > nd ? 1 : size(A, d)
    if sd == 1 || isempty(A)
        return copy(A)
    end
    B = similar(A)
    nnd = 0
    for i = 1:nd
        nnd += Int(size(A,i)==1 || i==d)
    end
    if nnd==nd
        # flip along the only non-singleton dimension
        for i = 1:sd
            B[i] = A[sd+1-i]
        end
        return B
    end
    alli = [ 1:size(B,n) for n in 1:nd ]
    for i = 1:sd
        B[[ n==d ? sd+1-i : alli[n] for n in 1:nd ]...] = slicedim(A, d, i)
    end
    return B
end

circshift(a::AbstractArray, shiftamt::Real) = circshift(a, [Integer(shiftamt)])
function circshift{T,N}(a::AbstractArray{T,N}, shiftamts)
    I = ()
    for i=1:N
        s = size(a,i)
        d = i<=length(shiftamts) ? shiftamts[i] : 0
        I = tuple(I..., d==0 ? [1:s;] : mod([-d:s-1-d;], s).+1)
    end
    a[(I::NTuple{N,Vector{Int}})...]
end

## Indexing: setindex! ##

# 1-d indexing is assumed defined on subtypes
setindex!(t::AbstractArray, x, i::Real) =
    error("setindex! not defined for ",typeof(t))
setindex!(t::AbstractArray, x) = throw(MethodError(setindex!, (t, x)))

## Indexing: handle more indices than dimensions if "extra" indices are 1

# Don't require vector/matrix subclasses to implement more than 1/2 indices,
# respectively, by handling the extra dimensions in AbstractMatrix.

function getindex(A::AbstractVector, i1,i2,i3...)
    if i2*prod(i3) != 1
        throw(BoundsError())
    end
    A[i1]
end
function getindex(A::AbstractMatrix, i1,i2,i3,i4...)
    if i3*prod(i4) != 1
        throw(BoundsError())
    end
    A[i1,i2]
end

function setindex!(A::AbstractVector, x, i1,i2,i3...)
    if i2*prod(i3) != 1
        throw(BoundsError())
    end
    A[i1] = x
end
function setindex!(A::AbstractMatrix, x, i1,i2,i3,i4...)
    if i3*prod(i4) != 1
        throw(BoundsError())
    end
    A[i1,i2] = x
end

## get (getindex with a default value) ##

typealias RangeVecIntList{A<:AbstractVector{Int}} Union(Tuple{Vararg{Union(Range, AbstractVector{Int})}}, AbstractVector{UnitRange{Int}}, AbstractVector{Range{Int}}, AbstractVector{A})

get(A::AbstractArray, i::Integer, default) = in_bounds(length(A), i) ? A[i] : default
get(A::AbstractArray, I::Tuple{}, default) = similar(A, typeof(default), 0)
get(A::AbstractArray, I::Dims, default) = in_bounds(size(A), I...) ? A[I...] : default

function get!{T}(X::AbstractArray{T}, A::AbstractArray, I::Union(Range, AbstractVector{Int}), default::T)
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
    hvcat(ntuple(nbr, i->nbc), as...)
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

# Uses K-B-N summation
function cumsum_kbn{T<:FloatingPoint}(v::AbstractVector{T})
    n = length(v)
    r = similar(v, n)
    if n == 0; return r; end

    s = r[1] = v[1]
    c = zero(T)
    for i=2:n
        vi = v[i]
        t = s + vi
        if abs(s) >= abs(vi)
            c += ((s-t) + vi)
        else
            c += ((vi-t) + s)
        end
        s = t
        r[i] = s+c
    end
    return r
end

# Uses K-B-N summation
function cumsum_kbn{T<:FloatingPoint}(A::AbstractArray{T}, axis::Integer=1)
    dimsA = size(A)
    ndimsA = ndims(A)
    axis_size = dimsA[axis]
    axis_stride = 1
    for i = 1:(axis-1)
        axis_stride *= size(A,i)
    end

    if axis_size <= 1
        return A
    end

    B = similar(A)
    C = similar(A)

    for i = 1:length(A)
        if div(i-1, axis_stride) % axis_size == 0
            B[i] = A[i]
            C[i] = zero(T)
        else
            s = B[i-axis_stride]
            Ai = A[i]
            B[i] = t = s + Ai
            if abs(s) >= abs(Ai)
                C[i] = C[i-axis_stride] + ((s-t) + Ai)
            else
                C[i] = C[i-axis_stride] + ((Ai-t) + s)
            end
        end
    end

    return B + C
end

## ipermutedims in terms of permutedims ##

function ipermutedims(A::AbstractArray,perm)
    iperm = Array(Int,length(perm))
    for i = 1:length(perm)
        iperm[perm[i]] = i
    end
    return permutedims(A,iperm)
end

## Other array functions ##

function repmat(a::AbstractVecOrMat, m::Int, n::Int=1)
    o, p = size(a,1), size(a,2)
    b = similar(a, o*m, p*n)
    for j=1:n
        d = (j-1)*p+1
        R = d:d+p-1
        for i=1:m
            c = (i-1)*o+1
            b[c:c+o-1, R] = a
        end
    end
    return b
end

function repmat(a::AbstractVector, m::Int)
    o = length(a)
    b = similar(a, o*m)
    for i=1:m
        c = (i-1)*o+1
        b[c:c+o-1] = a
    end
    return b
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
    t = NTuple{N,Vector{T}}(ntuple(N,n->Array{T}(M)))
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

# Generalized repmat
function repeat{T}(A::Array{T};
                   inner::Array{Int} = ones(Int, ndims(A)),
                   outer::Array{Int} = ones(Int, ndims(A)))
    ndims_in = ndims(A)
    length_inner = length(inner)
    length_outer = length(outer)
    ndims_out = max(ndims_in, length_inner, length_outer)

    if length_inner < ndims_in || length_outer < ndims_in
        throw(ArgumentError("inner/outer repetitions must be set for all input dimensions"))
    end

    inner = vcat(inner, ones(Int,ndims_out-length_inner))
    outer = vcat(outer, ones(Int,ndims_out-length_outer))

    size_in = size(A)
    size_out = ntuple(i->inner[i]*size(A,i)*outer[i],ndims_out)::Dims
    inner_size_out = ntuple(i->inner[i]*size(A,i),ndims_out)::Dims

    indices_in = Array(Int, ndims_in)
    indices_out = Array(Int, ndims_out)

    length_out = prod(size_out)
    R = Array(T, size_out)

    for index_out in 1:length_out
        ind2sub!(indices_out, size_out, index_out)
        for t in 1:ndims_in
            # "Project" outer repetitions into inner repetitions
            indices_in[t] = mod1(indices_out[t], inner_size_out[t])
            # Find inner repetitions using flooring division
            if inner[t] != 1
                indices_in[t] = fld1(indices_in[t], inner[t])
            end
        end
        index_in = sub2ind(size_in, indices_in...)
        R[index_out] = A[index_in]
    end

    return R
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
mapslices(f::Function, A::AbstractArray, dims) = mapslices(f, A, [dims...])
function mapslices(f::Function, A::AbstractArray, dims::AbstractVector)
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
function map!{F}(f::F, dest::AbstractArray, As::AbstractArray...)
    n = length(As[1])
    i = 1
    ith = a->a[i]
    for i = 1:n
        dest[i] = f(map(ith, As)...)
    end
    return dest
end

function map_to!{T,F}(f::F, offs, dest::AbstractArray{T}, As::AbstractArray...)
    local i
    ith = a->a[i]
    @inbounds for i = offs:length(As[1])
        el = f(map(ith, As)...)
        S = typeof(el)
        if (S !== T) && !(S <: T)
            R = typejoin(T, S)
            new = similar(dest, R)
            copy!(new,1, dest,1, i-1)
            new[i] = el
            return map_to!(f, i+1, new, As...)
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
    return map_to!(f, 2, dest, As...)
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
