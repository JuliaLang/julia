## array.jl: Dense arrays

typealias Vector{T} Array{T,1}
typealias Matrix{T} Array{T,2}
typealias VecOrMat{T} Union(Vector{T}, Matrix{T})

typealias DenseVector{T} DenseArray{T,1}
typealias DenseMatrix{T} DenseArray{T,2}
typealias DenseVecOrMat{T} Union(DenseVector{T}, DenseMatrix{T})

typealias StridedArray{T,N,A<:DenseArray,I<:(RangeIndex...)} Union(DenseArray{T,N}, SubArray{T,N,A,I})
typealias StridedVector{T,A<:DenseArray,I<:(RangeIndex...)}  Union(DenseArray{T,1}, SubArray{T,1,A,I})
typealias StridedMatrix{T,A<:DenseArray,I<:(RangeIndex...)}  Union(DenseArray{T,2}, SubArray{T,2,A,I})
typealias StridedVecOrMat{T} Union(StridedVector{T}, StridedMatrix{T})

call{T}(::Type{Vector{T}}, m::Integer) = Array{T}(m)
call{T}(::Type{Matrix{T}}, m::Integer, n::Integer) = Array{T}(m, n)

## Basic functions ##

# convert Arrays to pointer arrays for ccall
function call{P<:Ptr,T<:Ptr}(::Type{Ref{P}}, a::Array{T}) # Ref{P<:Ptr}(a::Array{T<:Ptr})
    return RefArray(a) # effectively a no-op
end
function call{P<:Ptr,T}(::Type{Ref{P}}, a::Array{T}) # Ref{P<:Ptr}(a::Array)
    if (!isbits(T) && T <: eltype(P))
        # this Array already has the right memory layout for the requested Ref
        return RefArray(a,1,false) # root something, so that this function is type-stable
    else
        ptrs = Array(P, length(a)+1)
        roots = Array(Any, length(a))
        for i = 1:length(a)
            root = cconvert(P, a[i])
            ptrs[i] = unsafe_convert(P, root)::P
            roots[i] = root
        end
        ptrs[length(a)+1] = C_NULL
        return RefArray(ptrs,1,roots)
    end
end
cconvert{P<:Ptr,T<:Ptr}(::Union(Type{Ptr{P}},Type{Ref{P}}), a::Array{T}) = a
cconvert{P<:Ptr}(::Union(Type{Ptr{P}},Type{Ref{P}}), a::Array) = Ref{P}(a)

size(a::Array) = arraysize(a)
size(a::Array, d) = arraysize(a, d)
size(a::Matrix) = (arraysize(a,1), arraysize(a,2))
length(a::Array) = arraylen(a)
elsize{T}(a::Array{T}) = isbits(T) ? sizeof(T) : sizeof(Ptr)
sizeof(a::Array) = elsize(a) * length(a)

strides{T}(a::Array{T,1}) = (1,)
strides{T}(a::Array{T,2}) = (1, size(a,1))
strides{T}(a::Array{T,3}) = (1, size(a,1), size(a,1)*size(a,2))

isassigned(a::Array, i::Int...) = isdefined(a, i...)

## copy ##

function unsafe_copy!{T}(dest::Ptr{T}, src::Ptr{T}, n)
    ccall(:memmove, Ptr{Void}, (Ptr{Void}, Ptr{Void}, UInt),
          dest, src, n*sizeof(T))
    return dest
end

function unsafe_copy!{T}(dest::Array{T}, doffs, src::Array{T}, soffs, n)
    if isbits(T)
        unsafe_copy!(pointer(dest, doffs), pointer(src, soffs), n)
    else
        for i=0:n-1
            @inbounds arrayset(dest, src[i+soffs], i+doffs)
        end
    end
    return dest
end

function copy!{T}(dest::Array{T}, doffs::Integer, src::Array{T}, soffs::Integer, n::Integer)
    n == 0 && return dest
    if n < 0 || soffs < 1 || doffs < 1 || soffs+n-1 > length(src) || doffs+n-1 > length(dest)
        throw(BoundsError())
    end
    unsafe_copy!(dest, doffs, src, soffs, n)
end

copy!{T}(dest::Array{T}, src::Array{T}) = copy!(dest, 1, src, 1, length(src))

function copy(a::Array)
    b = similar(a)
    ccall(:memcpy, Ptr{Void}, (Ptr{Void}, Ptr{Void}, UInt), b, a, sizeof(a))
    return b
end

function reinterpret{T,S}(::Type{T}, a::Array{S,1})
    nel = Int(div(length(a)*sizeof(S),sizeof(T)))
    # TODO: maybe check that remainder is zero?
    return reinterpret(T, a, (nel,))
end

function reinterpret{T,S}(::Type{T}, a::Array{S})
    if sizeof(S) != sizeof(T)
        throw(ArgumentError("result shape not specified"))
    end
    reinterpret(T, a, size(a))
end

function reinterpret{T,S,N}(::Type{T}, a::Array{S}, dims::NTuple{N,Int})
    if !isbits(T)
        throw(ArgumentError("cannot reinterpret Array{$(S)} to ::Type{Array{$(T)}}, type $(T) is not a bitstype"))
    end
    if !isbits(S)
        throw(ArgumentError("cannot reinterpret Array{$(S)} to ::Type{Array{$(T)}}, type $(S) is not a bitstype"))
    end
    nel = div(length(a)*sizeof(S),sizeof(T))
    if prod(dims) != nel
        throw(DimensionMismatch("new dimensions $(dims) must be consistent with array size $(nel)"))
    end
    ccall(:jl_reshape_array, Array{T,N}, (Any, Any, Any), Array{T,N}, a, dims)
end

# reshaping to same # of dimensions
function reshape{T,N}(a::Array{T,N}, dims::NTuple{N,Int})
    if prod(dims) != length(a)
        throw(DimensionMismatch("new dimensions $(dims) must be consistent with array size $(length(a))"))
    end
    if dims == size(a)
        return a
    end
    ccall(:jl_reshape_array, Array{T,N}, (Any, Any, Any), Array{T,N}, a, dims)
end

# reshaping to different # of dimensions
function reshape{T,N}(a::Array{T}, dims::NTuple{N,Int})
    if prod(dims) != length(a)
        throw(DimensionMismatch("new dimensions $(dims) must be consistent with array size $(length(a))"))
    end
    ccall(:jl_reshape_array, Array{T,N}, (Any, Any, Any), Array{T,N}, a, dims)
end

## Constructors ##

similar(a::Array, T, dims::Dims)      = Array(T, dims)
similar{T}(a::Array{T,1})             = Array(T, size(a,1))
similar{T}(a::Array{T,2})             = Array(T, size(a,1), size(a,2))
similar{T}(a::Array{T,1}, dims::Dims) = Array(T, dims)
similar{T}(a::Array{T,1}, m::Int)     = Array(T, m)
similar{T}(a::Array{T,1}, S)          = Array(S, size(a,1))
similar{T}(a::Array{T,2}, dims::Dims) = Array(T, dims)
similar{T}(a::Array{T,2}, m::Int)     = Array(T, m)
similar{T}(a::Array{T,2}, S)          = Array(S, size(a,1), size(a,2))

# T[x...] constructs Array{T,1}
function getindex(T::NonTupleType, vals...)
    a = Array(T,length(vals))
    @inbounds for i = 1:length(vals)
        a[i] = vals[i]
    end
    return a
end

function getindex(::Type{Any}, vals::ANY...)
    a = Array(Any,length(vals))
    @inbounds for i = 1:length(vals)
        a[i] = vals[i]
    end
    return a
end

function getindex(T::(Type...), vals::Tuple...)
    a = Array(T,length(vals))
    @inbounds for i = 1:length(vals)
        a[i] = vals[i]
    end
    return a
end

if _oldstyle_array_vcat_
# T[a:b] and T[a:s:b] also construct typed ranges
function getindex{T<:Union(Char,Number)}(::Type{T}, r::Range)
    depwarn("T[a:b] concatenation is deprecated; use T[a:b;] instead", :getindex)
    copy!(Array(T,length(r)), r)
end

function getindex{T<:Union(Char,Number)}(::Type{T}, r1::Range, rs::Range...)
    depwarn("T[a:b,...] concatenation is deprecated; use T[a:b;...] instead", :getindex)
    a = Array(T,length(r1)+sum(length,rs))
    o = 1
    copy!(a, o, r1)
    o += length(r1)
    for r in rs
        copy!(a, o, r)
        o += length(r)
    end
    return a
end
end

function fill!(a::Union(Array{UInt8}, Array{Int8}), x::Integer)
    ccall(:memset, Ptr{Void}, (Ptr{Void}, Cint, Csize_t), a, x, length(a))
    return a
end
function fill!{T<:Union(Integer,FloatingPoint)}(a::Array{T}, x)
    # note: checking bit pattern
    xT = convert(T,x)
    if isbits(T) && ((sizeof(T)==1 && reinterpret(UInt8, xT) == 0) ||
                     (sizeof(T)==2 && reinterpret(UInt16, xT) == 0) ||
                     (sizeof(T)==4 && reinterpret(UInt32, xT) == 0) ||
                     (sizeof(T)==8 && reinterpret(UInt64, xT) == 0))
        ccall(:memset, Ptr{Void}, (Ptr{Void}, Cint, Csize_t),
              a, 0, length(a)*sizeof(T))
    else
        for i = 1:length(a)
            @inbounds a[i] = xT
        end
    end
    return a
end

fill(v, dims::Dims)       = fill!(Array(typeof(v), dims), v)
fill(v, dims::Integer...) = fill!(Array(typeof(v), dims...), v)

cell(dims::Integer...)   = Array(Any, dims...)
cell(dims::(Integer...)) = Array(Any, convert((Int...), dims))

for (fname, felt) in ((:zeros,:zero), (:ones,:one))
    @eval begin
        ($fname)(T::Type, dims...)       = fill!(Array(T, dims...), ($felt)(T))
        ($fname)(dims...)                = fill!(Array(Float64, dims...), ($felt)(Float64))
        ($fname){T}(A::AbstractArray{T}) = fill!(similar(A), ($felt)(T))
    end
end

function eye(T::Type, m::Integer, n::Integer)
    a = zeros(T,m,n)
    for i = 1:min(m,n)
        a[i,i] = one(T)
    end
    return a
end
eye(m::Integer, n::Integer) = eye(Float64, m, n)
eye(T::Type, n::Integer) = eye(T, n, n)
eye(n::Integer) = eye(Float64, n)
eye{T}(x::AbstractMatrix{T}) = eye(T, size(x, 1), size(x, 2))

function one{T}(x::AbstractMatrix{T})
    m,n = size(x)
    m==n || throw(DimensionMismatch("multiplicative identity defined only for square matrices"))
    eye(T, m)
end

linspace(start::Integer, stop::Integer, n::Integer) =
    linspace(float(start), float(stop), n)
function linspace(start::Real, stop::Real, n::Integer)
    (start, stop) = promote(start, stop)
    T = typeof(start)
    a = Array(T, Int(n))
    if n == 1
        a[1] = start
        return a
    end
    n -= 1
    S = promote_type(T, Float64)
    for i = 0:n
        a[i+1] = start*(convert(S, (n-i))/n) + stop*(convert(S, i)/n)
    end
    a
end
linspace(start::Real, stop::Real) = linspace(start, stop, 100)

function linspace{T<:FloatingPoint}(start::T, stop::T, n::Int)
    n == 1 && return [start]
    n -= 1
    a0, b = rat(start)
    a = convert(T,a0)
    if a/convert(T,b) == start
        c0, d = rat(stop)
        c = convert(T,c0)
        if c/convert(T,d) == stop
            e = lcm(b,d)
            a *= div(e,b)
            c *= div(e,d)
            ne = convert(T,n*e)
            if a*n/ne == start && c*n/ne == stop
                return [ (a*(n-k) + c*k)/ne for k=0:n ]
            end
        end
    end
    return [ start*((n-k)/n) + stop*(k/n) for k=0:n ]
end
linspace(start::FloatingPoint, stop::FloatingPoint, n::Integer) =
    linspace(promote(start, stop)..., Int(n))

logspace(start::Real, stop::Real, n::Integer) = 10.^linspace(start, stop, n)
logspace(start::Real, stop::Real) = logspace(start, stop, 50)

## Conversions ##

convert{T,n}(::Type{Array{T}}, x::Array{T,n}) = x
convert{T,n}(::Type{Array{T,n}}, x::Array{T,n}) = x
convert{T,n,S}(::Type{Array{T}}, x::Array{S,n}) = convert(Array{T,n}, x)
convert{T,n,S}(::Type{Array{T,n}}, x::Array{S,n}) = copy!(similar(x,T), x)

promote_rule{T,n,S}(::Type{Array{T,n}}, ::Type{Array{S,n}}) = Array{promote_type(T,S),n}

function collect(T::Type, itr)
    if applicable(length, itr)
        # when length() isn't defined this branch might pollute the
        # type of the other.
        a = Array(T,length(itr)::Integer)
        i = 0
        for x in itr
            a[i+=1] = x
        end
    else
        a = Array(T,0)
        for x in itr
            push!(a,x)
        end
    end
    return a
end

collect(itr) = collect(eltype(itr), itr)

## Iteration ##
start(A::Array) = 1
next(a::Array,i) = (a[i],i+1)
done(a::Array,i) = (i > length(a))

## Indexing: getindex ##

getindex(a::Array) = arrayref(a,1)

getindex(A::Array, i0::Real) = arrayref(A,to_index(i0))
getindex(A::Array, i0::Real, i1::Real) = arrayref(A,to_index(i0),to_index(i1))
getindex(A::Array, i0::Real, i1::Real, i2::Real) =
    arrayref(A,to_index(i0),to_index(i1),to_index(i2))
getindex(A::Array, i0::Real, i1::Real, i2::Real, i3::Real) =
    arrayref(A,to_index(i0),to_index(i1),to_index(i2),to_index(i3))
getindex(A::Array, i0::Real, i1::Real, i2::Real, i3::Real,  i4::Real) =
    arrayref(A,to_index(i0),to_index(i1),to_index(i2),to_index(i3),to_index(i4))
getindex(A::Array, i0::Real, i1::Real, i2::Real, i3::Real,  i4::Real, i5::Real) =
    arrayref(A,to_index(i0),to_index(i1),to_index(i2),to_index(i3),to_index(i4),to_index(i5))

getindex(A::Array, i0::Real, i1::Real, i2::Real, i3::Real,  i4::Real, i5::Real, I::Real...) =
    arrayref(A,to_index(i0),to_index(i1),to_index(i2),to_index(i3),to_index(i4),to_index(i5),to_index(I)...)

# Fast copy using copy! for UnitRange
function getindex(A::Array, I::UnitRange{Int})
    lI = length(I)
    X = similar(A, lI)
    if lI > 0
        copy!(X, 1, A, first(I), lI)
    end
    return X
end

function getindex{T<:Real}(A::Array, I::AbstractVector{T})
    return [ A[i] for i in to_index(I) ]
end
function getindex{T<:Real}(A::Range, I::AbstractVector{T})
    return [ A[i] for i in to_index(I) ]
end
function getindex(A::Range, I::AbstractVector{Bool})
    checkbounds(A, I)
    return [ A[i] for i in to_index(I) ]
end


# logical indexing
# (when the indexing is provided as an Array{Bool} or a BitArray we can be
# sure about the behaviour and use unsafe_getindex; in the general case
# we can't and must use getindex, otherwise silent corruption can happen)

stagedfunction getindex_bool_1d(A::Array, I::AbstractArray{Bool})
    idxop = I <: Union(Array{Bool}, BitArray) ? :unsafe_getindex : :getindex
    quote
        checkbounds(A, I)
        n = sum(I)
        out = similar(A, n)
        c = 1
        for i = 1:length(I)
            if $idxop(I, i)
                @inbounds out[c] = A[i]
                c += 1
            end
        end
        out
    end
end

getindex(A::Vector, I::AbstractVector{Bool}) = getindex_bool_1d(A, I)
getindex(A::Vector, I::AbstractArray{Bool}) = getindex_bool_1d(A, I)
getindex(A::Array, I::AbstractVector{Bool}) = getindex_bool_1d(A, I)
getindex(A::Array, I::AbstractArray{Bool}) = getindex_bool_1d(A, I)


## Indexing: setindex! ##
setindex!{T}(A::Array{T}, x) = arrayset(A, convert(T,x), 1)

setindex!{T}(A::Array{T}, x, i0::Real) = arrayset(A, convert(T,x), to_index(i0))
setindex!{T}(A::Array{T}, x, i0::Real, i1::Real) =
    arrayset(A, convert(T,x), to_index(i0), to_index(i1))
setindex!{T}(A::Array{T}, x, i0::Real, i1::Real, i2::Real) =
    arrayset(A, convert(T,x), to_index(i0), to_index(i1), to_index(i2))
setindex!{T}(A::Array{T}, x, i0::Real, i1::Real, i2::Real, i3::Real) =
    arrayset(A, convert(T,x), to_index(i0), to_index(i1), to_index(i2), to_index(i3))
setindex!{T}(A::Array{T}, x, i0::Real, i1::Real, i2::Real, i3::Real, i4::Real) =
    arrayset(A, convert(T,x), to_index(i0), to_index(i1), to_index(i2), to_index(i3), to_index(i4))
setindex!{T}(A::Array{T}, x, i0::Real, i1::Real, i2::Real, i3::Real, i4::Real, i5::Real) =
    arrayset(A, convert(T,x), to_index(i0), to_index(i1), to_index(i2), to_index(i3), to_index(i4), to_index(i5))
setindex!{T}(A::Array{T}, x, i0::Real, i1::Real, i2::Real, i3::Real, i4::Real, i5::Real, I::Real...) =
    arrayset(A, convert(T,x), to_index(i0), to_index(i1), to_index(i2), to_index(i3), to_index(i4), to_index(i5), to_index(I)...)

function setindex!{T<:Real}(A::Array, x, I::AbstractVector{T})
    for i in I
        A[i] = x
    end
    return A
end

function setindex!{T}(A::Array{T}, X::Array{T}, I::UnitRange{Int})
    if length(X) != length(I)
        throw_setindex_mismatch(X, (I,))
    end
    copy!(A, first(I), X, 1, length(I))
    return A
end

function setindex!{T<:Real}(A::Array, X::AbstractArray, I::AbstractVector{T})
    if length(X) != length(I)
        throw_setindex_mismatch(X, (I,))
    end
    count = 1
    if is(X,A)
        X = copy(X)
        is(I,A) && (I = X)
    elseif is(I,A)
        I = copy(I)
    end
    for i in I
        A[i] = X[count]
        count += 1
    end
    return A
end


# logical indexing
# (when the indexing is provided as an Array{Bool} or a BitArray we can be
# sure about the behaviour and use unsafe_getindex; in the general case
# we can't and must use getindex, otherwise silent corruption can happen)

stagedfunction assign_bool_scalar_1d!(A::Array, x, I::AbstractArray{Bool})
    idxop = I <: Union(Array{Bool}, BitArray) ? :unsafe_getindex : :getindex
    quote
        checkbounds(A, I)
        for i = 1:length(I)
            if $idxop(I, i)
                @inbounds A[i] = x
            end
        end
        A
    end
end

stagedfunction assign_bool_vector_1d!(A::Array, X::AbstractArray, I::AbstractArray{Bool})
    idxop = I <: Union(Array{Bool}, BitArray) ? :unsafe_getindex : :getindex
    quote
        checkbounds(A, I)
        c = 1
        for i = 1:length(I)
            if $idxop(I, i)
                x = X[c]
                @inbounds A[i] = x
                c += 1
            end
        end
        if length(X) != c-1
            throw(DimensionMismatch("assigned $(length(X)) elements to length $(c-1) destination"))
        end
        A
    end
end

setindex!(A::Array, X::AbstractArray, I::AbstractVector{Bool}) = assign_bool_vector_1d!(A, X, I)
setindex!(A::Array, X::AbstractArray, I::AbstractArray{Bool}) = assign_bool_vector_1d!(A, X, I)
setindex!(A::Array, x, I::AbstractVector{Bool}) = assign_bool_scalar_1d!(A, x, I)
setindex!(A::Array, x, I::AbstractArray{Bool}) = assign_bool_scalar_1d!(A, x, I)

# efficiently grow an array

function _growat!(a::Vector, i::Integer, delta::Integer)
    n = length(a)
    if i < div(n,2)
        _growat_beg!(a, i, delta)
    else
        _growat_end!(a, i, delta)
    end
    return a
end

function _growat_beg!(a::Vector, i::Integer, delta::Integer)
    ccall(:jl_array_grow_beg, Void, (Any, UInt), a, delta)
    if i > 1
        ccall(:memmove, Ptr{Void}, (Ptr{Void}, Ptr{Void}, Csize_t),
              pointer(a, 1), pointer(a, 1+delta), (i-1)*elsize(a))
    end
    return a
end

function _growat_end!(a::Vector, i::Integer, delta::Integer)
    ccall(:jl_array_grow_end, Void, (Any, UInt), a, delta)
    n = length(a)
    if n >= i+delta
        ccall(:memmove, Ptr{Void}, (Ptr{Void}, Ptr{Void}, Csize_t),
              pointer(a, i+delta), pointer(a, i), (n-i-delta+1)*elsize(a))
    end
    return a
end

# efficiently delete part of an array

function _deleteat!(a::Vector, i::Integer, delta::Integer)
    n = length(a)
    last = i+delta-1
    if i-1 < n-last
        _deleteat_beg!(a, i, delta)
    else
        _deleteat_end!(a, i, delta)
    end
    return a
end

function _deleteat_beg!(a::Vector, i::Integer, delta::Integer)
    if i > 1
        ccall(:memmove, Ptr{Void}, (Ptr{Void}, Ptr{Void}, Csize_t),
              pointer(a, 1+delta), pointer(a, 1), (i-1)*elsize(a))
    end
    ccall(:jl_array_del_beg, Void, (Any, UInt), a, delta)
    return a
end

function _deleteat_end!(a::Vector, i::Integer, delta::Integer)
    n = length(a)
    if n >= i+delta
        ccall(:memmove, Ptr{Void}, (Ptr{Void}, Ptr{Void}, Csize_t),
              pointer(a, i), pointer(a, i+delta), (n-i-delta+1)*elsize(a))
    end
    ccall(:jl_array_del_end, Void, (Any, UInt), a, delta)
    return a
end

## Dequeue functionality ##

function push!{T}(a::Array{T,1}, item)
    # convert first so we don't grow the array if the assignment won't work
    item = convert(T, item)
    ccall(:jl_array_grow_end, Void, (Any, UInt), a, 1)
    a[end] = item
    return a
end

function push!(a::Array{Any,1}, item::ANY)
    ccall(:jl_array_grow_end, Void, (Any, UInt), a, 1)
    arrayset(a, item, length(a))
    return a
end

function append!{T}(a::Array{T,1}, items::AbstractVector)
    n = length(items)
    ccall(:jl_array_grow_end, Void, (Any, UInt), a, n)
    copy!(a, length(a)-n+1, items, 1, n)
    return a
end

function prepend!{T}(a::Array{T,1}, items::AbstractVector)
    n = length(items)
    ccall(:jl_array_grow_beg, Void, (Any, UInt), a, n)
    if a === items
        copy!(a, 1, items, n+1, n)
    else
        copy!(a, 1, items, 1, n)
    end
    return a
end

function resize!(a::Vector, nl::Integer)
    l = length(a)
    if nl > l
        ccall(:jl_array_grow_end, Void, (Any, UInt), a, nl-l)
    else
        if nl < 0
            throw(ArgumentError("new length must be ≥ 0"))
        end
        ccall(:jl_array_del_end, Void, (Any, UInt), a, l-nl)
    end
    return a
end

function sizehint!(a::Vector, sz::Integer)
    ccall(:jl_array_sizehint, Void, (Any, UInt), a, sz)
    a
end

function pop!(a::Vector)
    if isempty(a)
        throw(ArgumentError("array must be non-empty"))
    end
    item = a[end]
    ccall(:jl_array_del_end, Void, (Any, UInt), a, 1)
    return item
end

function unshift!{T}(a::Array{T,1}, item)
    item = convert(T, item)
    ccall(:jl_array_grow_beg, Void, (Any, UInt), a, 1)
    a[1] = item
    return a
end

function shift!(a::Vector)
    if isempty(a)
        throw(ArgumentError("array must be non-empty"))
    end
    item = a[1]
    ccall(:jl_array_del_beg, Void, (Any, UInt), a, 1)
    return item
end

function insert!{T}(a::Array{T,1}, i::Integer, item)
    if !(1 <= i <= length(a)+1)
        throw(BoundsError())
    end
    if i == length(a)+1
        return push!(a, item)
    end
    item = convert(T, item)
    _growat!(a, i, 1)
    a[i] = item
    return a
end

function deleteat!(a::Vector, i::Integer)
    if !(1 <= i <= length(a))
        throw(BoundsError())
    end
    return _deleteat!(a, i, 1)
end

function deleteat!{T<:Integer}(a::Vector, r::UnitRange{T})
    n = length(a)
    isempty(r) && return a
    f = first(r)
    l = last(r)
    if !(1 <= f && l <= n)
        throw(BoundsError())
    end
    return _deleteat!(a, f, length(r))
end

function deleteat!(a::Vector, inds)
    n = length(a)
    s = start(inds)
    done(inds, s) && return a
    (p, s) = next(inds, s)
    q = p+1
    while !done(inds, s)
        (i,s) = next(inds, s)
        if !(q <= i <= n)
            if i < q
                throw(ArgumentError("indices must be unique and sorted"))
            else
                throw(BoundsError())
            end
        end
        while q < i
            @inbounds a[p] = a[q]
            p += 1; q += 1
        end
        q = i+1
    end
    while q <= n
        @inbounds a[p] = a[q]
        p += 1; q += 1
    end
    ccall(:jl_array_del_end, Void, (Any, UInt), a, n-p+1)
    return a
end

const _default_splice = []

function splice!(a::Vector, i::Integer, ins=_default_splice)
    v = a[i]
    m = length(ins)
    if m == 0
        _deleteat!(a, i, 1)
    elseif m == 1
        a[i] = ins[1]
    else
        _growat!(a, i, m-1)
        k = 1
        for x in ins
            a[i+k-1] = x
            k += 1
        end
    end
    return v
end

function splice!{T<:Integer}(a::Vector, r::UnitRange{T}, ins=_default_splice)
    v = a[r]
    m = length(ins)
    if m == 0
        deleteat!(a, r)
        return v
    end

    n = length(a)
    f = first(r)
    l = last(r)
    d = length(r)

    if m < d
        delta = d - m
        if f-1 < n-l
            _deleteat_beg!(a, f, delta)
        else
            _deleteat_end!(a, l-delta+1, delta)
        end
    elseif m > d
        delta = m - d
        if f-1 < n-l
            _growat_beg!(a, f, delta)
        else
            _growat_end!(a, l+1, delta)
        end
    end

    k = 1
    for x in ins
        a[f+k-1] = x
        k += 1
    end
    return v
end

function empty!(a::Vector)
    ccall(:jl_array_del_end, Void, (Any, UInt), a, length(a))
    return a
end

## Unary operators ##

function conj!{T<:Number}(A::AbstractArray{T})
    for i=1:length(A)
        A[i] = conj(A[i])
    end
    return A
end

for f in (:-, :~, :conj, :sign)
    @eval begin
        function ($f)(A::StridedArray)
            F = similar(A)
            for i=1:length(A)
                F[i] = ($f)(A[i])
            end
            return F
        end
    end
end

(-)(A::StridedArray{Bool}) = reshape([ -A[i] for i=1:length(A) ], size(A))

real(A::StridedArray) = reshape([ real(x) for x in A ], size(A))
imag(A::StridedArray) = reshape([ imag(x) for x in A ], size(A))
real{T<:Real}(x::StridedArray{T}) = x
imag{T<:Real}(x::StridedArray{T}) = zero(x)

function !(A::StridedArray{Bool})
    F = similar(A)
    for i=1:length(A)
        F[i] = !A[i]
    end
    return F
end

## Binary arithmetic operators ##

promote_array_type{Scalar, Arry}(::Type{Scalar}, ::Type{Arry}) = promote_type(Scalar, Arry)
promote_array_type{S<:Real, A<:FloatingPoint}(::Type{S}, ::Type{A}) = A
promote_array_type{S<:Union(Complex, Real), AT<:FloatingPoint}(::Type{S}, ::Type{Complex{AT}}) = Complex{AT}
promote_array_type{S<:Integer, A<:Integer}(::Type{S}, ::Type{A}) = A
promote_array_type{S<:Integer}(::Type{S}, ::Type{Bool}) = S

# Handle operations that return different types
./(x::Number, Y::AbstractArray) =
    reshape([ x ./ y for y in Y ], size(Y))
./(X::AbstractArray, y::Number) =
    reshape([ x ./ y for x in X ], size(X))
.\(x::Number, Y::AbstractArray) =
    reshape([ x .\ y for y in Y ], size(Y))
.\(X::AbstractArray, y::Number) =
    reshape([ x .\ y for x in X ], size(X))
.^(x::Number, Y::AbstractArray) =
    reshape([ x ^ y for y in Y ], size(Y))
.^(X::AbstractArray, y::Number      ) =
    reshape([ x ^ y for x in X ], size(X))

for f in (:+, :-, :div, :mod, :&, :|, :$)
    @eval begin
        function ($f){S,T}(A::Range{S}, B::Range{T})
            F = similar(A, promote_type(S,T), promote_shape(size(A),size(B)))
            i = 1
            for (a,b) in zip(A,B)
                @inbounds F[i] = ($f)(a, b)
                i += 1
            end
            return F
        end
        function ($f){S,T}(A::AbstractArray{S}, B::Range{T})
            F = similar(A, promote_type(S,T), promote_shape(size(A),size(B)))
            i = 1
            for b in B
                @inbounds F[i] = ($f)(A[i], b)
                i += 1
            end
            return F
        end
        function ($f){S,T}(A::Range{S}, B::AbstractArray{T})
            F = similar(B, promote_type(S,T), promote_shape(size(A),size(B)))
            i = 1
            for a in A
                @inbounds F[i] = ($f)(a, B[i])
                i += 1
            end
            return F
        end
        function ($f){S,T}(A::AbstractArray{S}, B::AbstractArray{T})
            F = similar(A, promote_type(S,T), promote_shape(size(A),size(B)))
            for i=1:length(A)
                @inbounds F[i] = ($f)(A[i], B[i])
            end
            return F
        end
    end
end
for f in (:.+, :.-, :.*, :.%, :.<<, :.>>, :div, :mod, :rem, :&, :|, :$)
    @eval begin
        function ($f){T}(A::Number, B::AbstractArray{T})
            F = similar(B, promote_array_type(typeof(A),T))
            for i=1:length(B)
                @inbounds F[i] = ($f)(A, B[i])
            end
            return F
        end
        function ($f){T}(A::AbstractArray{T}, B::Number)
            F = similar(A, promote_array_type(typeof(B),T))
            for i=1:length(A)
                @inbounds F[i] = ($f)(A[i], B)
            end
            return F
        end
    end
end

# familiar aliases for broadcasting operations of array ± scalar (#7226):
(+)(A::AbstractArray{Bool},x::Bool) = A .+ x
(+)(x::Bool,A::AbstractArray{Bool}) = x .+ A
(-)(A::AbstractArray{Bool},x::Bool) = A .- x
(-)(x::Bool,A::AbstractArray{Bool}) = x .- A
(+)(A::AbstractArray,x::Number) = A .+ x
(+)(x::Number,A::AbstractArray) = x .+ A
(-)(A::AbstractArray,x::Number) = A .- x
(-)(x::Number,A::AbstractArray) = x .- A

# functions that should give an Int result for Bool arrays
for f in (:.+, :.-)
    @eval begin
        function ($f)(A::Bool, B::StridedArray{Bool})
            F = similar(B, Int, size(B))
            for i=1:length(B)
                @inbounds F[i] = ($f)(A, B[i])
            end
            return F
        end
        function ($f)(A::StridedArray{Bool}, B::Bool)
            F = similar(A, Int, size(A))
            for i=1:length(A)
                @inbounds F[i] = ($f)(A[i], B)
            end
            return F
        end
    end
end
for f in (:+, :-)
    @eval begin
        function ($f)(A::StridedArray{Bool}, B::StridedArray{Bool})
            F = similar(A, Int, promote_shape(size(A), size(B)))
            for i=1:length(A)
                @inbounds F[i] = ($f)(A[i], B[i])
            end
            return F
        end
    end
end

## promotion to complex ##

function complex{S<:Real,T<:Real}(A::Array{S}, B::Array{T})
    if size(A) != size(B); throw(DimensionMismatch()); end
    F = similar(A, typeof(complex(zero(S),zero(T))))
    for i=1:length(A)
        @inbounds F[i] = complex(A[i], B[i])
    end
    return F
end

function complex{T<:Real}(A::Real, B::Array{T})
    F = similar(B, typeof(complex(A,zero(T))))
    for i=1:length(B)
        @inbounds F[i] = complex(A, B[i])
    end
    return F
end

function complex{T<:Real}(A::Array{T}, B::Real)
    F = similar(A, typeof(complex(zero(T),B)))
    for i=1:length(A)
        @inbounds F[i] = complex(A[i], B)
    end
    return F
end

# use memcmp for lexcmp on byte arrays
function lexcmp(a::Array{UInt8,1}, b::Array{UInt8,1})
    c = ccall(:memcmp, Int32, (Ptr{UInt8}, Ptr{UInt8}, UInt),
              a, b, min(length(a),length(b)))
    c < 0 ? -1 : c > 0 ? +1 : cmp(length(a),length(b))
end

## data movement ##

function slicedim(A::Array, d::Integer, i::Integer)
    if d < 1
        throw(ArgumentError("dimension must be ≥ 1"))
    end
    d_in = size(A)
    leading = d_in[1:(d-1)]
    d_out = tuple(leading..., 1, d_in[(d+1):end]...)

    M = prod(leading)
    N = length(A)
    stride = M * d_in[d]

    B = similar(A, d_out)
    index_offset = 1 + (i-1)*M

    l = 1

    if M==1
        for j=0:stride:(N-stride)
            B[l] = A[j + index_offset]
            l += 1
        end
    else
        for j=0:stride:(N-stride)
            offs = j + index_offset
            for k=0:(M-1)
                B[l] = A[offs + k]
                l += 1
            end
        end
    end
    return B
end

function flipdim{T}(A::Array{T}, d::Integer)
    if d < 1
        throw(ArgumentError("dimension d must be ≥ 1"))
    end
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

    d_in = size(A)
    leading = d_in[1:(d-1)]
    M = prod(leading)
    N = length(A)
    stride = M * sd

    if M==1
        for j = 0:stride:(N-stride)
            for i = 1:sd
                ri = sd+1-i
                B[j + ri] = A[j + i]
            end
        end
    else
        if isbits(T) && M>200
            for i = 1:sd
                ri = sd+1-i
                for j=0:stride:(N-stride)
                    offs = j + 1 + (i-1)*M
                    boffs = j + 1 + (ri-1)*M
                    copy!(B, boffs, A, offs, M)
                end
            end
        else
            for i = 1:sd
                ri = sd+1-i
                for j=0:stride:(N-stride)
                    offs = j + 1 + (i-1)*M
                    boffs = j + 1 + (ri-1)*M
                    for k=0:(M-1)
                        B[boffs + k] = A[offs + k]
                    end
                end
            end
        end
    end
    return B
end

function rotl90(A::StridedMatrix)
    m,n = size(A)
    B = similar(A,(n,m))
    for i=1:m, j=1:n
        B[n-j+1,i] = A[i,j]
    end
    return B
end
function rotr90(A::StridedMatrix)
    m,n = size(A)
    B = similar(A,(n,m))
    for i=1:m, j=1:n
        B[j,m-i+1] = A[i,j]
    end
    return B
end
function rot180(A::StridedMatrix)
    m,n = size(A)
    B = similar(A)
    for i=1:m, j=1:n
        B[m-i+1,n-j+1] = A[i,j]
    end
    return B
end
function rotl90(A::AbstractMatrix, k::Integer)
    k = mod(k, 4)
    k == 1 ? rotl90(A) :
    k == 2 ? rot180(A) :
    k == 3 ? rotr90(A) : copy(A)
end
rotr90(A::AbstractMatrix, k::Integer) = rotl90(A,-k)
rot180(A::AbstractMatrix, k::Integer) = mod(k, 2) == 1 ? rot180(A) : copy(A)

# note: probably should be StridedVector or AbstractVector
function reverse(A::AbstractVector, s=1, n=length(A))
    B = similar(A)
    for i = 1:s-1
        B[i] = A[i]
    end
    for i = s:n
        B[i] = A[n+s-i]
    end
    for i = n+1:length(A)
        B[i] = A[i]
    end
    B
end
reverseind(a::AbstractVector, i::Integer) = length(a) + 1 - i

reverse(v::StridedVector) = (n=length(v); [ v[n-i+1] for i=1:n ])
reverse(v::StridedVector, s, n=length(v)) = reverse!(copy(v), s, n)
function reverse!(v::StridedVector, s=1, n=length(v))
    r = n
    for i=s:div(s+n-1,2)
        v[i], v[r] = v[r], v[i]
        r -= 1
    end
    v
end

function vcat{T}(arrays::Vector{T}...)
    n = 0
    for a in arrays
        n += length(a)
    end
    arr = Array(T, n)
    ptr = pointer(arr)
    offset = 0
    if isbits(T)
        elsz = sizeof(T)
    else
        elsz = div(WORD_SIZE,8)
    end
    for a in arrays
        nba = length(a)*elsz
        ccall(:memcpy, Ptr{Void}, (Ptr{Void}, Ptr{Void}, UInt),
              ptr+offset, a, nba)
        offset += nba
    end
    return arr
end

function hcat{T}(V::Vector{T}...)
    height = length(V[1])
    for j = 2:length(V)
        if length(V[j]) != height
            throw(DimensionMismatch("vectors must have same lengths"))
        end
    end
    [ V[j][i]::T for i=1:length(V[1]), j=1:length(V) ]
end

## find ##

# returns the index of the next non-zero element, or 0 if all zeros
function findnext(A, start::Integer)
    for i = start:length(A)
        if A[i] != 0
            return i
        end
    end
    return 0
end
findfirst(A) = findnext(A, 1)

# returns the index of the next matching element
function findnext(A, v, start::Integer)
    for i = start:length(A)
        if A[i] == v
            return i
        end
    end
    return 0
end
findfirst(A, v) = findnext(A, v, 1)

# returns the index of the next element for which the function returns true
function findnext(testf::Function, A, start::Integer)
    for i = start:length(A)
        if testf(A[i])
            return i
        end
    end
    return 0
end
findfirst(testf::Function, A) = findnext(testf, A, 1)

# returns the index of the previous non-zero element, or 0 if all zeros
function findprev(A, start)
    for i = start:-1:1
        A[i] != 0 && return i
    end
    0
end
findlast(A) = findprev(A, length(A))

# returns the index of the matching element, or 0 if no matching
function findprev(A, v, start)
    for i = start:-1:1
        A[i] == v && return i
    end
    0
end
findlast(A, v) = findprev(A, v, length(A))

# returns the index of the previous element for which the function returns true, or zero if it never does
function findprev(testf::Function, A, start)
    for i = start:-1:1
        testf(A[i]) && return i
    end
    0
end
findlast(testf::Function, A) = findprev(testf, A, length(A))

function find(testf::Function, A::AbstractArray)
    # use a dynamic-length array to store the indexes, then copy to a non-padded
    # array for the return
    tmpI = Array(Int, 0)
    for i = 1:length(A)
        if testf(A[i])
            push!(tmpI, i)
        end
    end
    I = Array(Int, length(tmpI))
    copy!(I, tmpI)
    I
end

function find(A::StridedArray)
    nnzA = countnz(A)
    I = similar(A, Int, nnzA)
    count = 1
    for i=1:length(A)
        if A[i] != 0
            I[count] = i
            count += 1
        end
    end
    return I
end

find(x::Number) = x == 0 ? Array(Int,0) : [1]
find(testf::Function, x::Number) = !testf(x) ? Array(Int,0) : [1]

findn(A::AbstractVector) = find(A)

function findn(A::StridedMatrix)
    nnzA = countnz(A)
    I = similar(A, Int, nnzA)
    J = similar(A, Int, nnzA)
    count = 1
    for j=1:size(A,2), i=1:size(A,1)
        if A[i,j] != 0
            I[count] = i
            J[count] = j
            count += 1
        end
    end
    return (I, J)
end

function findnz{T}(A::AbstractMatrix{T})
    nnzA = countnz(A)
    I = zeros(Int, nnzA)
    J = zeros(Int, nnzA)
    NZs = Array(T, nnzA)
    count = 1
    if nnzA > 0
        for j=1:size(A,2), i=1:size(A,1)
            Aij = A[i,j]
            if Aij != 0
                I[count] = i
                J[count] = j
                NZs[count] = Aij
                count += 1
            end
        end
    end
    return (I, J, NZs)
end

function findmax(a)
    if isempty(a)
        throw(ArgumentError("collection must be non-empty"))
    end
    m = a[1]
    mi = 1
    for i=2:length(a)
        ai = a[i]
        if ai > m || m!=m
            m = ai
            mi = i
        end
    end
    return (m, mi)
end

function findmin(a)
    if isempty(a)
        throw(ArgumentError("collection must be non-empty"))
    end
    m = a[1]
    mi = 1
    for i=2:length(a)
        ai = a[i]
        if ai < m || m!=m
            m = ai
            mi = i
        end
    end
    return (m, mi)
end

indmax(a) = findmax(a)[2]
indmin(a) = findmin(a)[2]

# similar to Matlab's ismember
# returns a vector containing the highest index in b for each value in a that is a member of b
function indexin(a::AbstractArray, b::AbstractArray)
    bdict = Dict(zip(b, 1:length(b)))
    [get(bdict, i, 0) for i in a]
end

# findin (the index of intersection)
function findin(a, b::UnitRange)
    ind = Array(Int, 0)
    f = first(b)
    l = last(b)
    for i = 1:length(a)
        if f <= a[i] <= l
            push!(ind, i)
        end
    end
    ind
end

function findin(a, b)
    ind = Array(Int, 0)
    bset = Set(b)
    @inbounds for i = 1:length(a)
        a[i] in bset && push!(ind, i)
    end
    ind
end

# Copying subregions
function indcopy(sz::Dims, I::Vector)
    n = length(I)
    s = sz[n]
    for i = n+1:length(sz)
        s *= sz[i]
    end
    dst = eltype(I)[findin(I[i], i < n ? (1:sz[i]) : (1:s)) for i = 1:n]
    src = eltype(I)[I[i][findin(I[i], i < n ? (1:sz[i]) : (1:s))] for i = 1:n]
    dst, src
end

function indcopy(sz::Dims, I::(RangeIndex...))
    n = length(I)
    s = sz[n]
    for i = n+1:length(sz)
        s *= sz[i]
    end
    dst::typeof(I) = ntuple(n, i-> findin(I[i], i < n ? (1:sz[i]) : (1:s)))::typeof(I)
    src::typeof(I) = ntuple(n, i-> I[i][findin(I[i], i < n ? (1:sz[i]) : (1:s))])::typeof(I)
    dst, src
end


## Filter ##

# given a function returning a boolean and an array, return matching elements
filter(f, As::AbstractArray) = As[map(f, As)::AbstractArray{Bool}]

function filter!(f, a::Vector)
    insrt = 1
    for curr = 1:length(a)
        if f(a[curr])
            a[insrt] = a[curr]
            insrt += 1
        end
    end
    deleteat!(a, insrt:length(a))
    return a
end

function filter(f, a::Vector)
    r = Array(eltype(a), 0)
    for i = 1:length(a)
        if f(a[i])
            push!(r, a[i])
        end
    end
    return r
end

## Transpose ##
const transposebaselength=64
function transpose!(B::StridedMatrix,A::StridedMatrix)
    m, n = size(A)
    size(B,1) == n && size(B,2) == m || throw(DimensionMismatch("transpose"))

    if m*n<=4*transposebaselength
        @inbounds begin
            for j = 1:n
                for i = 1:m
                    B[j,i] = transpose(A[i,j])
                end
            end
        end
    else
        transposeblock!(B,A,m,n,0,0)
    end
    return B
end
function transpose!(B::StridedVector, A::StridedMatrix)
    length(B) == length(A) && size(A,1) == 1 || throw(DimensionMismatch("transpose"))
    copy!(B, A)
end
function transpose!(B::StridedMatrix, A::StridedVector)
    length(B) == length(A) && size(B,1) == 1 || throw(DimensionMismatch("transpose"))
    copy!(B, A)
end
function transposeblock!(B::StridedMatrix,A::StridedMatrix,m::Int,n::Int,offseti::Int,offsetj::Int)
    if m*n<=transposebaselength
        @inbounds begin
            for j = offsetj+(1:n)
                for i = offseti+(1:m)
                    B[j,i] = transpose(A[i,j])
                end
            end
        end
    elseif m>n
        newm=m>>1
        transposeblock!(B,A,newm,n,offseti,offsetj)
        transposeblock!(B,A,m-newm,n,offseti+newm,offsetj)
    else
        newn=n>>1
        transposeblock!(B,A,m,newn,offseti,offsetj)
        transposeblock!(B,A,m,n-newn,offseti,offsetj+newn)
    end
    return B
end
function ctranspose!(B::StridedMatrix,A::StridedMatrix)
    m, n = size(A)
    size(B,1) == n && size(B,2) == m || throw(DimensionMismatch("transpose"))

    if m*n<=4*transposebaselength
        @inbounds begin
            for j = 1:n
                for i = 1:m
                    B[j,i] = ctranspose(A[i,j])
                end
            end
        end
    else
        ctransposeblock!(B,A,m,n,0,0)
    end
    return B
end
function ctranspose!(B::StridedVector, A::StridedMatrix)
    length(B) == length(A) && size(A,1) == 1 || throw(DimensionMismatch("transpose"))
    ccopy!(B, A)
end
function ctranspose!(B::StridedMatrix, A::StridedVector)
    length(B) == length(A) && size(B,1) == 1 || throw(DimensionMismatch("transpose"))
    ccopy!(B, A)
end
function ctransposeblock!(B::StridedMatrix,A::StridedMatrix,m::Int,n::Int,offseti::Int,offsetj::Int)
    if m*n<=transposebaselength
        @inbounds begin
            for j = offsetj+(1:n)
                for i = offseti+(1:m)
                    B[j,i] = ctranspose(A[i,j])
                end
            end
        end
    elseif m>n
        newm=m>>1
        ctransposeblock!(B,A,newm,n,offseti,offsetj)
        ctransposeblock!(B,A,m-newm,n,offseti+newm,offsetj)
    else
        newn=n>>1
        ctransposeblock!(B,A,m,newn,offseti,offsetj)
        ctransposeblock!(B,A,m,n-newn,offseti,offsetj+newn)
    end
    return B
end
function ccopy!(B, A)
    for i = 1:length(A)
        B[i] = ctranspose(A[i])
    end
end

function transpose(A::StridedMatrix)
    B = similar(A, size(A, 2), size(A, 1))
    transpose!(B, A)
end
function ctranspose(A::StridedMatrix)
    B = similar(A, size(A, 2), size(A, 1))
    ctranspose!(B, A)
end
ctranspose{T<:Real}(A::StridedVecOrMat{T}) = transpose(A)

transpose(x::StridedVector) = [ transpose(x[j]) for i=1, j=1:size(x,1) ]
ctranspose{T}(x::StridedVector{T}) = T[ ctranspose(x[j]) for i=1, j=1:size(x,1) ]

# set-like operators for vectors
# These are moderately efficient, preserve order, and remove dupes.

function intersect(v1, vs...)
    ret = Array(eltype(v1),0)
    for v_elem in v1
        inall = true
        for i = 1:length(vs)
            if !in(v_elem, vs[i])
                inall=false; break
            end
        end
        if inall
            push!(ret, v_elem)
        end
    end
    ret
end

function union(vs...)
    ret = Array(promote_eltype(vs...),0)
    seen = Set()
    for v in vs
        for v_elem in v
            if !in(v_elem, seen)
                push!(ret, v_elem)
                push!(seen, v_elem)
            end
        end
    end
    ret
end
# setdiff only accepts two args
function setdiff(a, b)
    args_type = promote_type(eltype(a), eltype(b))
    bset = Set(b)
    ret = Array(args_type,0)
    seen = Set{eltype(a)}()
    for a_elem in a
        if !in(a_elem, seen) && !in(a_elem, bset)
            push!(ret, a_elem)
            push!(seen, a_elem)
        end
    end
    ret
end
# symdiff is associative, so a relatively clean
# way to implement this is by using setdiff and union, and
# recursing. Has the advantage of keeping order, too, but
# not as fast as other methods that make a single pass and
# store counts with a Dict.
symdiff(a) = a
symdiff(a, b) = union(setdiff(a,b), setdiff(b,a))
symdiff(a, b, rest...) = symdiff(a, symdiff(b, rest...))

_cumsum_type{T<:Number}(v::AbstractArray{T}) = typeof(+zero(T))
_cumsum_type(v) = typeof(v[1]+v[1])

for (f, f!, fp, op) = ((:cumsum, :cumsum!, :cumsum_pairwise!, :+),
                       (:cumprod, :cumprod!, :cumprod_pairwise!, :*) )
    # in-place cumsum of c = s+v[range(i1,n)], using pairwise summation
    @eval function ($fp){T}(v::AbstractVector, c::AbstractVector{T}, s, i1, n)
        local s_::T # for sum(v[range(i1,n)]), i.e. sum without s
        if n < 128
            @inbounds s_ = v[i1]
            @inbounds c[i1] = ($op)(s, s_)
            for i = i1+1:i1+n-1
                @inbounds s_ = $(op)(s_, v[i])
                @inbounds c[i] = $(op)(s, s_)
            end
        else
            n2 = n >> 1
            s_ = ($fp)(v, c, s, i1, n2)
            s_ = $(op)(s_, ($fp)(v, c, s + s_, i1+n2, n-n2))
        end
        return s_
    end

    @eval function ($f!)(result::AbstractVector, v::AbstractVector)
        n = length(v)
        if n == 0; return result; end
        ($fp)(v, result, $(op==:+ ? :(zero(v[1])) : :(one(v[1]))), 1, n)
        return result
    end

    @eval function ($f)(v::AbstractVector)
        c = $(op===:+ ? (:(similar(v,_cumsum_type(v)))) : (:(similar(v))))
        return ($f!)(c, v)
    end
end

for (f, op) = ((:cummin, :min), (:cummax, :max))
    @eval function ($f)(v::AbstractVector)
        n = length(v)
        cur_val = v[1]
        res = similar(v, n)
        res[1] = cur_val
        for i in 2:n
            cur_val = ($op)(v[i], cur_val)
            res[i] = cur_val
        end
        return res
    end

    @eval function ($f)(A::StridedArray, axis::Integer)
        dimsA = size(A)
        ndimsA = ndims(A)
        axis_size = dimsA[axis]
        axis_stride = 1
        for i = 1:(axis-1)
            axis_stride *= size(A,i)
        end

        if axis_size < 1
            return A
        end

        B = similar(A)

        for i = 1:length(A)
            if div(i-1, axis_stride) % axis_size == 0
               B[i] = A[i]
            else
               B[i] = ($op)(A[i], B[i-axis_stride])
            end
        end

        return B
    end

    @eval ($f)(A::AbstractArray) = ($f)(A, 1)
end
