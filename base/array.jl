## array.jl: Dense arrays

typealias Vector{T} Array{T,1}
typealias Matrix{T} Array{T,2}
typealias VecOrMat{T} Union(Vector{T}, Matrix{T})

typealias StridedArray{T,N,A<:Array}  Union(Array{T,N}, SubArray{T,N,A})
typealias StridedVector{T,A<:Array}   Union(Vector{T} , SubArray{T,1,A})
typealias StridedMatrix{T,A<:Array}   Union(Matrix{T} , SubArray{T,2,A})
typealias StridedVecOrMat{T} Union(StridedVector{T}, StridedMatrix{T})

## Basic functions ##

size(a::Array) = arraysize(a)
size(a::Array, d) = arraysize(a, d)
size(a::Matrix) = (arraysize(a,1), arraysize(a,2))
length(a::Array) = arraylen(a)
sizeof(a::Array) = sizeof(eltype(a)) * length(a)

function stride(a::Array, i::Integer)
    s = 1
    if i > ndims(a)
        return length(a)
    end
    for n=1:(i-1)
        s *= size(a, n)
    end
    return s
end
strides{T}(a::Array{T,1}) = (1,)
strides{T}(a::Array{T,2}) = (1, size(a,1))
strides{T}(a::Array{T,3}) = (1, size(a,1), size(a,1)*size(a,2))

isassigned(a::Array, i::Int...) = isdefined(a, i...)

## copy ##

function copy!{T}(dest::Array{T}, dsto, src::Array{T}, so, N)
    if so+N-1 > length(src) || dsto+N-1 > length(dest) || dsto < 1 || so < 1
        throw(BoundsError())
    end
    copy_unsafe!(dest, dsto, src, so, N)
end

function copy_unsafe!{T}(dest::Array{T}, dsto, src::Array{T}, so, N)
    if isbits(T)
        ccall(:memcpy, Ptr{Void}, (Ptr{Void}, Ptr{Void}, Uint),
              pointer(dest, dsto), pointer(src, so), N*sizeof(T))
    else
        for i=0:N-1
            arrayset(dest, src[i+so], i+dsto)
        end
    end
    return dest
end

copy!{T}(dest::Array{T}, src::Array{T}) = copy!(dest, 1, src, 1, length(src))

function copy!{R,S}(B::Matrix{R}, ir_dest::Range1{Int}, jr_dest::Range1{Int}, A::StridedMatrix{S}, ir_src::Range1{Int}, jr_src::Range1{Int})
    if length(ir_dest) != length(ir_src) || length(jr_dest) != length(jr_src)
        error("copy!: size mismatch")
    end
    check_bounds(B, ir_dest, jr_dest)
    check_bounds(A, ir_src, jr_src)
    jdest = first(jr_dest)
    Askip = size(A, 1)
    Bskip = size(B, 1)
    if stride(A, 1) == 1 && R == S
        for jsrc in jr_src
            copy!(B, (jdest-1)*Bskip+first(ir_dest), A, (jsrc-1)*Askip+first(ir_src), length(ir_src))
            jdest += 1
        end
    else
        for jsrc in jr_src
            aoffset = (jsrc-1)*Askip
            boffset = (jdest-1)*Bskip
            idest = first(ir_dest)
            for isrc in ir_src
                B[boffset+idest] = A[aoffset+isrc]
                idest += 1
            end
            jdest += 1
        end
    end
end
function copy_transpose!{R,S}(B::Matrix{R}, ir_dest::Range1{Int}, jr_dest::Range1{Int}, A::StridedMatrix{S}, ir_src::Range1{Int}, jr_src::Range1{Int})
    if length(ir_dest) != length(jr_src) || length(jr_dest) != length(ir_src)
        error("copy_transpose!: size mismatch")
    end
    check_bounds(B, ir_dest, jr_dest)
    check_bounds(A, ir_src, jr_src)
    idest = first(ir_dest)
    Askip = size(A, 1)
    for jsrc in jr_src
        offset = (jsrc-1)*Askip
        jdest = first(jr_dest)
        for isrc in ir_src
            B[idest,jdest] = A[offset+isrc]
            jdest += 1
        end
        idest += 1
    end
end

function reinterpret{T,S}(::Type{T}, a::Array{S,1})
    nel = int(div(length(a)*sizeof(S),sizeof(T)))
    return reinterpret(T, a, (nel,))
end

function reinterpret{T,S}(::Type{T}, a::Array{S})
    if sizeof(S) != sizeof(T)
        error("reinterpret: result shape not specified")
    end
    reinterpret(T, a, size(a))
end

function reinterpret{T,S,N}(::Type{T}, a::Array{S}, dims::NTuple{N,Int})
    if !isbits(T)
        error("cannot reinterpret to type ", T)
    end
    if !isbits(S)
        error("cannot reinterpret Array of type ", S)
    end
    nel = div(length(a)*sizeof(S),sizeof(T))
    if prod(dims) != nel
        error("reinterpret: invalid dimensions")
    end
    ccall(:jl_reshape_array, Array{T,N}, (Any, Any, Any), Array{T,N}, a, dims)
end
reinterpret(t::Type,x) = reinterpret(t,[x])[1]

function reshape{T,N}(a::Array{T}, dims::NTuple{N,Int})
    if prod(dims) != length(a)
        error("reshape: invalid dimensions")
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
function getindex{T}(::Type{T}, vals...)
    a = Array(T,length(vals))
    for i = 1:length(vals)
        a[i] = vals[i]
    end
    return a
end

# T[a:b] and T[a:s:b] also contruct typed ranges
function getindex{T<:Number}(::Type{T}, r::Ranges)
    a = Array(T,length(r))
    i = 1
    for x in r
        a[i] = x
        i += 1
    end
    return a
end

function fill!{T<:Union(Int8,Uint8)}(a::Array{T}, x::Integer)
    ccall(:memset, Void, (Ptr{T}, Int32, Int), a, x, length(a))
    return a
end
function fill!{T<:Union(Integer,FloatingPoint)}(a::Array{T}, x)
    if isbits(T) && convert(T,x) == 0
        ccall(:memset, Ptr{T}, (Ptr{T}, Int32, Int32), a,0,length(a)*sizeof(T))
    else
        for i = 1:length(a)
            a[i] = x
        end
    end
    return a
end

fill(v, dims::Dims)       = fill!(Array(typeof(v), dims), v)
fill(v, dims::Integer...) = fill!(Array(typeof(v), dims...), v)

zeros{T}(::Type{T}, args...) = fill!(Array(T, args...), zero(T))
zeros(args...)               = fill!(Array(Float64, args...), float64(0))

ones{T}(::Type{T}, args...) = fill!(Array(T, args...), one(T))
ones(args...)               = fill!(Array(Float64, args...), float64(1))

function eye(T::Type, m::Int, n::Int)
    a = zeros(T,m,n)
    for i = 1:min(m,n)
        a[i,i] = one(T)
    end
    return a
end
eye(m::Int, n::Int) = eye(Float64, m, n)
eye(T::Type, n::Int) = eye(T, n, n)
eye(n::Int) = eye(Float64, n)
eye{T}(x::AbstractMatrix{T}) = eye(T, size(x, 1), size(x, 2))
function one{T}(x::AbstractMatrix{T})
    m,n = size(x)
    if m != n; error("Multiplicative identity only defined for square matrices!"); end;
    eye(T, m)
end


function linspace(start::Real, stop::Real, n::Integer)
    (start, stop) = promote(start, stop)
    T = typeof(start)
    a = Array(T, int(n))
    if n == 1
        a[1] = start
        return a
    end
    step = (stop-start)/(n-1)
    if isa(start,Integer)
        for i=1:n
            a[i] = iround(T,start+(i-1)*step)
        end
    else
        for i=1:n
            a[i] = start+(i-1)*step
        end
    end
    a
end
linspace(start::Real, stop::Real) = linspace(start, stop, 100)

logspace(start::Real, stop::Real, n::Integer) = 10.^linspace(start, stop, n)
logspace(start::Real, stop::Real) = logspace(start, stop, 50)

## Conversions ##

convert{T,n}(::Type{Array{T}}, x::Array{T,n}) = x
convert{T,n}(::Type{Array{T,n}}, x::Array{T,n}) = x
convert{T,n,S}(::Type{Array{T}}, x::Array{S,n}) = convert(Array{T,n}, x)
convert{T,n,S}(::Type{Array{T,n}}, x::Array{S,n}) = copy!(similar(x,T), x)

collect(itr) = [x for x in itr]

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

getindex(A::Array, i0::Real, i1::Real, i2::Real, i3::Real,  i4::Real, i5::Real, I::Int...) =
    arrayref(A,to_index(i0),to_index(i1),to_index(i2),to_index(i3),to_index(i4),to_index(i5),I...)

# Fast copy using copy! for Range1
function getindex(A::Array, I::Range1{Int})
    lI = length(I)
    X = similar(A, lI)
    if lI > 0
        copy!(X, 1, A, first(I), lI)
    end
    return X
end

# note: this is also useful for Ranges
function getindex{T<:Real}(A::AbstractArray, I::AbstractVector{T})
    return [ A[i] for i in indices(I) ]
end

# 2d indexing
function getindex(A::Array, I::Range1{Int}, j::Real)
    j = to_index(j)
    check_bounds(A, I, j)
    X = similar(A,length(I))
    copy_unsafe!(X, 1, A, (j-1)*size(A,1) + first(I), length(I))
    return X
end
function getindex(A::Array, I::Range1{Int}, J::Range1{Int})
    check_bounds(A, I, J)
    X = similar(A, index_shape(I, J))
    if length(I) == size(A,1)
        copy_unsafe!(X, 1, A, (first(J)-1)*size(A,1) + 1, size(A,1)*length(J))
    else
        storeoffset = 1
        for j = J
            copy_unsafe!(X, storeoffset, A, (j-1)*size(A,1) + first(I), length(I))
            storeoffset += length(I)
        end
    end
    return X
end
function getindex(A::Array, I::Range1{Int}, J::AbstractVector{Int})
    check_bounds(A, I, J)
    X = similar(A, index_shape(I, J))
    storeoffset = 1
    for j = J
        copy_unsafe!(X, storeoffset, A, (j-1)*size(A,1) + first(I), length(I))
        storeoffset += length(I)
    end
    return X
end

getindex{T<:Real}(A::Array, I::AbstractVector{T}, j::Real) = [ A[i,j] for i=indices(I) ]
getindex{T<:Real}(A::Array, I::Real, J::AbstractVector{T}) = [ A[i,j] for i=I,j=indices(J) ]

# This next is a 2d specialization of the algorithm used for general
# multidimensional indexing
function getindex{T<:Real}(A::Array, I::AbstractVector{T}, J::AbstractVector{T})
    check_bounds(A, I, J)
    I = indices(I); J = indices(J)
    X = similar(A, index_shape(I, J))
    storeind = 1
    for j = J
        offset = (j-1)*size(A,1)
        for i = I
            X[storeind] = A[i+offset]
            storeind += 1
        end
    end
    return X
end
# Multidimensional indexing
let getindex_cache = nothing
global getindex
function getindex(A::Array, I::Union(Real,AbstractArray)...)
    check_bounds(A, I...)
    I = indices(I)
    X = similar(A, index_shape(I...))

    if is(getindex_cache,nothing)
        getindex_cache = Dict()
    end
    gen_array_index_map(getindex_cache, refind -> quote
            X[storeind] = A[$refind]
            storeind += 1
        end, I, (:A, :X, :storeind), A, X, 1)
    return X
end
end


# logical indexing

function getindex_bool_1d(A::Array, I::AbstractArray{Bool})
    check_bounds(A, I)
    n = sum(I)
    out = similar(A, n)
    c = 1
    for i = 1:length(I)
        if I[i]
            out[c] = A[i]
            c += 1
        end
    end
    out
end

getindex(A::Vector, I::AbstractVector{Bool}) = getindex_bool_1d(A, I)
getindex(A::Vector, I::AbstractArray{Bool}) = getindex_bool_1d(A, I)
getindex(A::Array, I::AbstractVector{Bool}) = getindex_bool_1d(A, I)
getindex(A::Array, I::AbstractArray{Bool}) = getindex_bool_1d(A, I)

# @Jeff: more efficient is to check the bool vector, and then do
# indexing without checking. Turn off checking for the second stage?
getindex(A::Matrix, I::Real, J::AbstractVector{Bool}) = A[I,find(J)]
getindex(A::Matrix, I::AbstractVector{Bool}, J::Real) = A[find(I),J]
getindex(A::Matrix, I::AbstractVector{Bool}, J::AbstractVector{Bool}) = A[find(I),find(J)]
getindex{T<:Real}(A::Matrix, I::AbstractVector{T}, J::AbstractVector{Bool}) = A[I,find(J)]
getindex{T<:Real}(A::Matrix, I::AbstractVector{Bool}, J::AbstractVector{T}) = A[find(I),J]

## Indexing: setindex! ##
setindex!{T}(A::Array{T,0}, x) = arrayset(A, convert(T,x), 1)

setindex!(A::Array{Any}, x::ANY, i::Real) = arrayset(A, x, to_index(i))

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
setindex!{T}(A::Array{T}, x, i0::Real, i1::Real, i2::Real, i3::Real, i4::Real, i5::Real, I::Int...) =
    arrayset(A, convert(T,x), to_index(i0), to_index(i1), to_index(i2), to_index(i3), to_index(i4), to_index(i5), I...)

function setindex!{T<:Real}(A::Array, x, I::AbstractVector{T})
    for i in I
        A[i] = x
    end
    return A
end

function setindex!{T}(A::Array{T}, X::Array{T}, I::Range1{Int})
    if length(X) != length(I); error("argument dimensions must match"); end
    copy!(A, first(I), X, 1, length(I))
    return A
end

function setindex!{T<:Real}(A::Array, X::AbstractArray, I::AbstractVector{T})
    if length(X) != length(I); error("argument dimensions must match"); end
    count = 1
    for i in I
        A[i] = X[count]
        count += 1
    end
    return A
end

function setindex!{T<:Real}(A::Array, x, i::Real, J::AbstractVector{T})
    i = to_index(i)
    check_bounds(A, i, J)
    m = size(A, 1)
    if !isa(x,AbstractArray)
        for j in J
            A[(j-1)*m + i] = x
        end
    else
        X = x
        if length(X) != length(J); error("argument dimensions must match"); end
        count = 1
        for j in J
            A[(j-1)*m + i] = X[count]
            count += 1
        end
    end
    return A
end

function setindex!{T<:Real}(A::Array, x, I::AbstractVector{T}, j::Real)
    j = to_index(j)
    check_bounds(A, I, j)
    m = size(A, 1)
    offset = (j-1)*m

    if !isa(x,AbstractArray)
        for i in I
            A[offset + i] = x
        end
    else
        X = x
        if length(X) != length(I); error("argument dimensions must match"); end
        count = 1
        for i in I
            A[offset + i] = X[count]
            count += 1
        end
    end
    return A
end

function setindex!{T}(A::Array{T}, X::Array{T}, I::Range1{Int}, j::Real)
    j = to_index(j)
    check_bounds(A, I, j)
    if length(X) != length(I); error("argument dimensions must match"); end
    copy_unsafe!(A, first(I) + (j-1)*size(A,1), X, 1, length(I))
    return A
end

function setindex!{T}(A::Array{T}, X::Array{T}, I::Range1{Int}, J::Range1{Int})
    check_bounds(A, I, J)
    nel = length(I)*length(J)
    if length(X) != nel ||
        (ndims(X) > 1 && (size(X,1)!=length(I) || size(X,2)!=length(J)))
        error("argument dimensions must match")
    end
    if length(I) == size(A,1)
        copy_unsafe!(A, first(I) + (first(J)-1)*size(A,1), X, 1, size(A,1)*length(J))
    else
        refoffset = 1
        for j = J
            copy_unsafe!(A, first(I) + (j-1)*size(A,1), X, refoffset, length(I))
            refoffset += length(I)
        end
    end
    return A
end

function setindex!{T}(A::Array{T}, X::Array{T}, I::Range1{Int}, J::AbstractVector{Int})
    check_bounds(A, I, J)
    nel = length(I)*length(J)
    if length(X) != nel ||
        (ndims(X) > 1 && (size(X,1)!=length(I) || size(X,2)!=length(J)))
        error("argument dimensions must match")
    end
    refoffset = 1
    for j = J
        copy_unsafe!(A, first(I) + (j-1)*size(A,1), X, refoffset, length(I))
        refoffset += length(I)
    end
    return A
end

function setindex!{T<:Real}(A::Array, x, I::AbstractVector{T}, J::AbstractVector{T})
    check_bounds(A, I, J)
    m = size(A, 1)
    if !isa(x,AbstractArray)
        for j in J
            offset = (j-1)*m
            for i in I
                A[offset + i] = x
            end
        end
    else
        X = x
        nel = length(I)*length(J)
        if length(X) != nel ||
            (ndims(X) > 1 && (size(X,1)!=length(I) || size(X,2)!=length(J)))
            error("argument dimensions must match")
        end
        count = 1
        for j in J
            offset = (j-1)*m
            for i in I
                A[offset + i] = X[count]
                count += 1
            end
        end
    end
    return A
end

let assign_cache = nothing, assign_scalar_cache = nothing
global setindex!
function setindex!(A::Array, x, I::Union(Real,AbstractArray)...)
    check_bounds(A, I...)
    I = indices(I)
    if !isa(x,AbstractArray)
        if is(assign_scalar_cache,nothing)
            assign_scalar_cache = Dict()
        end
        gen_array_index_map(assign_scalar_cache, storeind -> quote
                              A[$storeind] = x
                            end,
                            I,
                            (:A, :x),
                            A, x)
    else
        if is(assign_cache,nothing)
            assign_cache = Dict()
        end
        X = x
        nel = 1
        for idx in I
            nel *= length(idx)
        end
        if length(X) != nel
            error("argument dimensions must match")
        end
        if ndims(X) > 1
            for i = 1:length(I)
                if size(X,i) != length(I[i])
                    error("argument dimensions must match")
                end
            end
        end
        if is(assign_cache,nothing)
            assign_cache = Dict()
        end
        gen_array_index_map(assign_cache, storeind -> quote
                              A[$storeind] = X[refind]
                              refind += 1
                            end,
                            I,
                            (:A, :X, :refind),
                            A, X, 1)
    end
    return A
end
end

# logical indexing

function assign_bool_scalar_1d(A::Array, x, I::AbstractArray{Bool})
    check_bounds(A, I)
    for i = 1:length(I)
        if I[i]
            A[i] = x
        end
    end
    A
end

function assign_bool_vector_1d(A::Array, X::AbstractArray, I::AbstractArray{Bool})
    check_bounds(A, I)
    c = 1
    for i = 1:length(I)
        if I[i]
            A[i] = X[c]
            c += 1
        end
    end
    A
end

setindex!(A::Array, X::AbstractArray, I::AbstractVector{Bool}) = assign_bool_vector_1d(A, X, I)
setindex!(A::Array, X::AbstractArray, I::AbstractArray{Bool}) = assign_bool_vector_1d(A, X, I)
setindex!(A::Array, x, I::AbstractVector{Bool}) = assign_bool_scalar_1d(A, x, I)
setindex!(A::Array, x, I::AbstractArray{Bool}) = assign_bool_scalar_1d(A, x, I)

setindex!(A::Array, x, I::Real, J::AbstractVector{Bool}) = setindex!(A, x, I,find(J))
setindex!(A::Array, x, I::AbstractVector{Bool}, J::Real) = setindex!(A,x,find(I),J)
setindex!(A::Array, x, I::AbstractVector{Bool}, J::AbstractVector{Bool}) = setindex!(A, x, find(I),find(J))
setindex!{T<:Real}(A::Array, x, I::AbstractVector{T}, J::AbstractVector{Bool}) = setindex!(A, x, I,find(J))
setindex!{T<:Real}(A::Array, x, I::AbstractVector{Bool}, J::AbstractVector{T}) = setindex!(A, x, find(I),J)

# get (getindex with a default value)

get{T}(A::Array, i::Integer, default::T) = in_bounds(length(A), i) ? A[i] : default
get{T}(A::Array, I::(), default::T) = Array(T, 0)
get{T}(A::Array, I::Dims, default::T) = in_bounds(size(A), I...) ? A[I...] : default

function get{T}(X::Array{T}, A::Array, I::Union(Ranges, Vector{Int}), default::T)
    ind = findin(I, 1:length(A))
    X[ind] = A[I[ind]]
    X[1:first(ind)-1] = default
    X[last(ind)+1:length(X)] = default
    X
end
get{T}(A::Array, I::Ranges, default::T) = get(Array(T, length(I)), A, I, default)

RangeVecIntList = Union((Union(Ranges, Vector{Int})...), Vector{Range1{Int}}, Vector{Range{Int}}, Vector{Vector{Int}})

function get{T}(X::Array{T}, A::Array, I::RangeVecIntList, default::T)
    fill!(X, default)
    dst, src = indcopy(size(A), I)
    X[dst...] = A[src...]
    X
end
get{T}(A::Array, I::RangeVecIntList, default::T) = get(Array(T, map(length, I)...), A, I, default)

## Dequeue functionality ##

function push!{T}(a::Array{T,1}, item)
    if is(T,None)
        error("[] cannot grow. Instead, initialize the array with \"T[]\".")
    end
    # convert first so we don't grow the array if the assignment won't work
    item = convert(T, item)
    ccall(:jl_array_grow_end, Void, (Any, Uint), a, 1)
    a[end] = item
    return a
end

function push!(a::Array{Any,1}, item::ANY)
    ccall(:jl_array_grow_end, Void, (Any, Uint), a, 1)
    arrayset(a, item, length(a))
    return a
end

function append!{T}(a::Array{T,1}, items::Array{T,1})
    if is(T,None)
        error("[] cannot grow. Instead, initialize the array with \"T[]\".")
    end
    n = length(items)
    ccall(:jl_array_grow_end, Void, (Any, Uint), a, n)
    a[end-n+1:end] = items
    return a
end

function resize!(a::Vector, nl::Integer)
    l = length(a)
    if nl > l
        ccall(:jl_array_grow_end, Void, (Any, Uint), a, nl-l)
    else
        if nl < 0
            throw(BoundsError())
        end
        ccall(:jl_array_del_end, Void, (Any, Uint), a, l-nl)
    end
    return a
end

function sizehint(a::Vector, sz::Integer)
    ccall(:jl_array_sizehint, Void, (Any, Uint), a, sz)
    a
end

function pop!(a::Vector)
    if isempty(a)
        error("pop!: array is empty")
    end
    item = a[end]
    ccall(:jl_array_del_end, Void, (Any, Uint), a, 1)
    return item
end

function unshift!{T}(a::Array{T,1}, item)
    if is(T,None)
        error("[] cannot grow. Instead, initialize the array with \"T[]\".")
    end
    item = convert(T, item)
    ccall(:jl_array_grow_beg, Void, (Any, Uint), a, 1)
    a[1] = item
    return a
end

function shift!(a::Vector)
    if isempty(a)
        error("shift!: array is empty")
    end
    item = a[1]
    ccall(:jl_array_del_beg, Void, (Any, Uint), a, 1)
    return item
end

function insert!{T}(a::Array{T,1}, i::Integer, item)
    if i < 1
        throw(BoundsError())
    end
    item = convert(T, item)
    n = length(a)
    if i > n
        ccall(:jl_array_grow_end, Void, (Any, Uint), a, i-n)
    elseif i > div(n,2)
        ccall(:jl_array_grow_end, Void, (Any, Uint), a, 1)
        for k=n+1:-1:i+1
            a[k] = a[k-1]
        end
    else
        ccall(:jl_array_grow_beg, Void, (Any, Uint), a, 1)
        for k=1:(i-1)
            a[k] = a[k+1]
        end
    end
    a[i] = item
end

function delete!(a::Vector, i::Integer)
    n = length(a)
    if !(1 <= i <= n)
        throw(BoundsError())
    end
    v = a[i]
    if i < div(n,2)
        for k = i:-1:2
            a[k] = a[k-1]
        end
        ccall(:jl_array_del_beg, Void, (Any, Uint), a, 1)
    else
        for k = i:n-1
            a[k] = a[k+1]
        end
        ccall(:jl_array_del_end, Void, (Any, Uint), a, 1)
    end
    return v
end

function delete!{T<:Integer}(a::Vector, r::Range1{T})
    n = length(a)
    f = first(r)
    l = last(r)
    if !(1 <= f && l <= n)
        throw(BoundsError())
    end
    if l < f
        return T[]
    end
    v = a[r]
    d = l-f+1
    if f-1 < n-l
        for k = l:-1:1+d
            a[k] = a[k-d]
        end
        ccall(:jl_array_del_beg, Void, (Any, Uint), a, d)
    else
        for k = f:n-d
            a[k] = a[k+d]
        end
        ccall(:jl_array_del_end, Void, (Any, Uint), a, d)
    end
    return v
end

function empty!(a::Vector)
    ccall(:jl_array_del_end, Void, (Any, Uint), a, length(a))
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
        function ($f)(A::AbstractArray)
            F = similar(A)
            for i=1:length(A)
                F[i] = ($f)(A[i])
            end
            return F
        end
    end
end

(-)(A::StridedArray{Bool}) = reshape([ -A[i] for i=1:length(A) ], size(A))

for f in (:real, :imag)
    @eval begin
        function ($f){T}(A::AbstractArray{T})
            S = typeof(($f)(zero(T)))
            F = similar(A, S)
            for i=1:length(A)
                F[i] = ($f)(A[i])
            end
            return F
        end
    end
end

function !(A::AbstractArray{Bool})
    F = similar(A)
    for i=1:length(A)
        F[i] = !A[i]
    end
    return F
end

## Binary arithmetic operators ##

promote_array_type{Scalar, Arry}(::Type{Scalar}, ::Type{Arry}) = promote_type(Scalar, Arry)
promote_array_type{S<:Real, A<:Real}(::Type{S}, ::Type{A}) = A
promote_array_type{S<:Complex, A<:Complex}(::Type{S}, ::Type{A}) = A
promote_array_type{S<:Integer, A<:Integer}(::Type{S}, ::Type{A}) = A
promote_array_type{S<:Real, A<:Integer}(::Type{S}, ::Type{A}) = promote_type(S, A)
promote_array_type{S<:Integer}(::Type{S}, ::Type{Bool}) = S

./{T<:Integer,S<:Integer}(x::StridedArray{T}, y::StridedArray{S}) =
    reshape([ x[i] ./ y[i] for i=1:length(x) ], promote_shape(size(x),size(y)))
./{T<:Integer}(x::Integer, y::StridedArray{T}) =
    reshape([ x    ./ y[i] for i=1:length(y) ], size(y))
./{T<:Integer}(x::StridedArray{T}, y::Integer) =
    reshape([ x[i] ./ y    for i=1:length(x) ], size(x))

./{T<:Integer,S<:Integer}(x::StridedArray{ComplexPair{T}}, y::StridedArray{S}) =
    reshape([ x[i] ./ y[i] for i=1:length(x) ], promote_shape(size(x),size(y)))
./{T<:Integer,S<:Integer}(x::StridedArray{T}, y::StridedArray{ComplexPair{S}}) =
    reshape([ x[i] ./ y[i] for i=1:length(x) ], promote_shape(size(x),size(y)))
./{T<:Integer,S<:Integer}(x::StridedArray{ComplexPair{T}}, y::StridedArray{ComplexPair{S}}) =
    reshape([ x[i] ./ y[i] for i=1:length(x) ], promote_shape(size(x),size(y)))
./{T<:Integer}(x::Integer, y::StridedArray{ComplexPair{T}}) =
    reshape([ x    ./ y[i] for i=1:length(y) ], size(y))
./{T<:Integer}(x::StridedArray{ComplexPair{T}}, y::Integer) =
    reshape([ x[i] ./ y    for i=1:length(x) ], size(x))
./{S<:Integer,T<:Integer}(x::ComplexPair{S}, y::StridedArray{T}) =
    reshape([ x    ./ y[i] for i=1:length(y) ], size(y))
./{S<:Integer,T<:Integer}(x::StridedArray{S}, y::ComplexPair{T}) =
    reshape([ x[i] ./ y    for i=1:length(x) ], size(x))

# ^ is difficult, since negative exponents give a different type

.^(x::StridedArray, y::StridedArray) =
    reshape([ x[i] ^ y[i] for i=1:length(x) ], promote_shape(size(x),size(y)))
.^(x::Number,       y::StridedArray) =
    reshape([ x    ^ y[i] for i=1:length(y) ], size(y))
.^(x::StridedArray, y::Number      ) =
    reshape([ x[i] ^ y    for i=1:length(x) ], size(x))

for f in (:+, :-, :.*, :./, :div, :mod, :&, :|, :$)
    @eval begin
        function ($f){S,T}(A::StridedArray{S}, B::StridedArray{T})
            F = Array(promote_type(S,T), promote_shape(size(A),size(B)))
            for i=1:length(A)
                F[i] = ($f)(A[i], B[i])
            end
            return F
        end
        function ($f){T}(A::Number, B::StridedArray{T})
            F = similar(B, promote_array_type(typeof(A),T))
            for i=1:length(B)
                F[i] = ($f)(A, B[i])
            end
            return F
        end
        function ($f){T}(A::StridedArray{T}, B::Number)
            F = similar(A, promote_array_type(typeof(B),T))
            for i=1:length(A)
                F[i] = ($f)(A[i], B)
            end
            return F
        end
        # interaction with Ranges
        function ($f){S,T<:Real}(A::StridedArray{S}, B::Ranges{T})
            F = Array(promote_type(S,T), promote_shape(size(A),size(B)))
            i = 1
            for b in B
                F[i] = ($f)(A[i], b)
                i += 1
            end
            return F
        end
        function ($f){S<:Real,T}(A::Ranges{S}, B::StridedArray{T})
            F = Array(promote_type(S,T), promote_shape(size(A),size(B)))
            i = 1
            for a in A
                F[i] = ($f)(a, B[i])
                i += 1
            end
            return F
        end
    end
end

# functions that should give an Int result for Bool arrays
for f in (:+, :-)
    @eval begin
        function ($f)(x::Bool, y::StridedArray{Bool})
            reshape([ ($f)(x, y[i]) for i=1:length(y) ], size(y))
        end
        function ($f)(x::StridedArray{Bool}, y::Bool)
            reshape([ ($f)(x[i], y) for i=1:length(x) ], size(x))
        end
        function ($f)(x::StridedArray{Bool}, y::StridedArray{Bool})
            shp = promote_shape(size(x),size(y))
            reshape([ ($f)(x[i], y[i]) for i=1:length(x) ], shp)
        end
    end
end

## promotion to complex ##

function complex{S<:Real,T<:Real}(A::Array{S}, B::Array{T})
    F = similar(A, typeof(complex(zero(S),zero(T))))
    for i=1:length(A)
        F[i] = complex(A[i], B[i])
    end
    return F
end

function complex{T<:Real}(A::Real, B::Array{T})
    F = similar(B, typeof(complex(A,zero(T))))
    for i=1:length(B)
        F[i] = complex(A, B[i])
    end
    return F
end

function complex{T<:Real}(A::Array{T}, B::Real)
    F = similar(A, typeof(complex(zero(T),B)))
    for i=1:length(A)
        F[i] = complex(A[i], B)
    end
    return F
end

# use memcmp for cmp on byte arrays
function cmp(a::Array{Uint8,1}, b::Array{Uint8,1})
    c = ccall(:memcmp, Int32, (Ptr{Uint8}, Ptr{Uint8}, Uint),
              a, b, min(length(a),length(b)))
    c < 0 ? -1 : c > 0 ? +1 : cmp(length(a),length(b))
end

## data movement ##

function slicedim(A::Array, d::Integer, i::Integer)
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
    nd = ndims(A)
    sd = d > nd ? 1 : size(A, d)
    if sd == 1
        return copy(A)
    end

    B = similar(A)

    nnd = 0
    for i = 1:nd
        nnd += int(size(A,i)==1 || i==d)
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

function rotl90(A::AbstractMatrix)
    m,n = size(A)
    B = similar(A,(n,m))
    for i=1:m, j=1:n
        B[n-j+1,i] = A[i,j]
    end
    return B
end
function rotr90(A::AbstractMatrix)
    m,n = size(A)
    B = similar(A,(n,m))
    for i=1:m, j=1:n
        B[j,m-i+1] = A[i,j]
    end
    return B
end
function rot180(A::AbstractMatrix)
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

reverse(v::StridedVector) = (n=length(v); [ v[n-i+1] for i=1:n ])
function reverse!(v::AbstractVector)
    n = length(v)
    r = n
    for i=1:div(n,2)
        v[i], v[r] = v[r], v[i]
        r -= 1
    end
    v
end

function vcat{T}(arrays::Array{T,1}...)
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
        ccall(:memcpy, Ptr{Void}, (Ptr{Void}, Ptr{Void}, Uint),
              ptr+offset, a, nba)
        offset += nba
    end
    return arr
end

## find ##

function nnz(a)
    n = 0
    for i = 1:length(a)
        n += bool(a[i]) ? 1 : 0
    end
    return n
end

# returns the index of the next non-zero element, or 0 if all zeros
function findnext(A, start::Integer)
    for i = start:length(A)
        if A[i] != 0
            return i
        end
    end
    return 0
end
findfirst(A) = findnext(A,1)

# returns the index of the next matching element
function findnext(A, v, start::Integer)
    for i = start:length(A)
        if A[i] == v
            return i
        end
    end
    return 0
end
findfirst(A,v) = findnext(A,v,1)

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

function find(A::AbstractArray)
    nnzA = nnz(A)
    I = Array(Int, nnzA)
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
find(testf::Function, x) = find(testf(x))

findn(A::AbstractVector) = find(A)

function findn(A::AbstractMatrix)
    nnzA = nnz(A)
    I = Array(Int, nnzA)
    J = Array(Int, nnzA)
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

let findn_cache = nothing
function findn_one(ivars)
    s = { quote I[$i][count] = $(ivars[i]) end for i = 1:length(ivars)}
    quote
    	Aind = A[$(ivars...)]
    	if Aind != z
    	    $(s...)
    	    count +=1
    	end
    end
end

global findn
function findn{T}(A::AbstractArray{T})
    ndimsA = ndims(A)
    nnzA = nnz(A)
    I = ntuple(ndimsA, x->Array(Int, nnzA))
    if nnzA > 0
        ranges = ntuple(ndims(A), d->(1:size(A,d)))

        if is(findn_cache,nothing)
            findn_cache = Dict()
        end

        gen_cartesian_map(findn_cache, findn_one, ranges,
                          (:A, :I, :count, :z), A,I,1, zero(T))
    end
    return I
end
end

function findn_nzs{T}(A::AbstractMatrix{T})
    nnzA = nnz(A)
    I = zeros(Int, nnzA)
    J = zeros(Int, nnzA)
    NZs = zeros(T, nnzA)
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

function nonzeros{T}(A::AbstractArray{T})
    nnzA = nnz(A)
    V = Array(T, nnzA)
    count = 1
    if nnzA > 0
        for i=1:length(A)
            Ai = A[i]
            if Ai != 0
                V[count] = Ai
                count += 1
            end
        end
    end
    return V
end

nonzeros(x::Number) = x == 0 ? Array(typeof(x),0) : [x]

function findmax(a)
    if isempty(a)
        error("findmax: array is empty")
    end
    m = a[1]
    mi = 1
    for i=2:length(a)
        if a[i] > m
            m = a[i]
            mi = i
        end
    end
    return (m, mi)
end

function findmin(a)
    if isempty(a)
        error("findmin: array is empty")
    end
    m = a[1]
    mi = 1
    for i=2:length(a)
        if a[i] < m
            m = a[i]
            mi = i
        end
    end
    return (m, mi)
end

indmax(a) = findmax(a)[2]
indmin(a) = findmin(a)[2]

# findin (the index of intersection)
function findin(a, b::Range1)
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
    bset = Set(b...)
    for i = 1:length(a)
        if has(bset, a[i])
            push!(ind, i)
        end
    end
    ind
end

# Copying subregions
function indcopy(sz::Dims, I::RangeVecIntList)
    n = length(I)
    dst = Array(AbstractVector{Int}, n)
    src = Array(AbstractVector{Int}, n)
    for dim = 1:(n-1)
        tmp = findin(I[dim], 1:sz[dim])
        dst[dim] = tmp
        src[dim] = I[dim][tmp]
    end
    s = sz[n]
    for i = n+1:length(sz)
        s *= sz[i]
    end
    tmp = findin(I[n], 1:s)
    dst[n] = tmp
    src[n] = I[n][tmp]
    dst, src
end


## Reductions ##

# TODO:
# - find out why inner loop with dimsA[i] instead of size(A,i) is way too slow

let reducedim_cache = nothing
# generate the body of the N-d loop to compute a reduction
function gen_reducedim_func(n, f)
    ivars = { symbol(string("i",i)) for i=1:n }
    # limits and vars for reduction loop
    lo    = { symbol(string("lo",i)) for i=1:n }
    hi    = { symbol(string("hi",i)) for i=1:n }
    rvars = { symbol(string("r",i)) for i=1:n }
    setlims = { quote
        # each dim of reduction is either 1:sizeA or ivar:ivar
        if contains(region,$i)
            $(lo[i]) = 1
            $(hi[i]) = size(A,$i)
        else
            $(lo[i]) = $(hi[i]) = $(ivars[i])
        end
               end for i=1:n }
    rranges = { :( $(lo[i]):$(hi[i]) ) for i=1:n }  # lo:hi for all dims
    body =
    quote
        _tot = v0
        $(setlims...)
        $(make_loop_nest(rvars, rranges,
                         :(_tot = ($f)(_tot, A[$(rvars...)]))))
        R[_ind] = _tot
        _ind += 1
    end
    quote
        local _F_
        function _F_(f, A, region, R, v0)
            _ind = 1
            $(make_loop_nest(ivars, { :(1:size(R,$i)) for i=1:n }, body))
        end
        _F_
    end
end

global reducedim
function reducedim(f::Function, A, region, v0, R)
    ndimsA = ndims(A)

    if is(reducedim_cache,nothing)
        reducedim_cache = Dict()
    end

    key = ndimsA
    fname = :f

    if  (is(f,+)     && (fname=:+;true)) ||
        (is(f,*)     && (fname=:*;true)) ||
        (is(f,max)   && (fname=:max;true)) ||
        (is(f,min)   && (fname=:min;true)) ||
        (is(f,any)   && (fname=:any;true)) ||
        (is(f,all)   && (fname=:all;true))
        key = (fname, ndimsA)
    end

    if !has(reducedim_cache,key)
        fexpr = gen_reducedim_func(ndimsA, fname)
        func = eval(fexpr)
        reducedim_cache[key] = func
    else
        func = reducedim_cache[key]
    end

    func(f, A, region, R, v0)

    return R
end
end

## Filter ##

# given a function returning a boolean and an array, return matching elements
filter(f::Function, As::AbstractArray) = As[map(f, As)::AbstractArray{Bool}]

function filter!(f::Function, a::Vector)
    insrt = 1
    for curr = 1:length(a)
        if f(a[curr])
            a[insrt] = a[curr]
            insrt += 1
        end
    end
    delete!(a, insrt:length(a))
    return a
end

function filter(f::Function, a::Vector)
    r = Array(eltype(a), 0)
    for i = 1:length(a)
        if f(a[i])
            push!(r, a[i])
        end
    end
    return r
end

## Transpose ##

function transpose{T<:Union(Float64,Float32,Complex128,Complex64)}(A::Matrix{T})
    if length(A) > 50000
        return FFTW.transpose(reshape(A, size(A, 2), size(A, 1)))
    else
        return [ A[j,i] for i=1:size(A,2), j=1:size(A,1) ]
    end
end

ctranspose{T<:Real}(A::StridedVecOrMat{T}) = transpose(A)
ctranspose(x::StridedVecOrMat) = transpose(x)

transpose(x::StridedVector) = [ x[j] for i=1, j=1:size(x,1) ]
transpose(x::StridedMatrix) = [ x[j,i] for i=1:size(x,2), j=1:size(x,1) ]

ctranspose{T<:Number}(x::StridedVector{T}) = [ conj(x[j]) for i=1, j=1:size(x,1) ]
ctranspose{T<:Number}(x::StridedMatrix{T}) = [ conj(x[j,i]) for i=1:size(x,2), j=1:size(x,1) ]

# set-like operators for vectors
# These are moderately efficient, preserve order, and remove dupes.

# algorithm: do intersect on sets first, then iterate through the first
# vector and produce only those in the set
function intersect(vs...)
    args_type = promote_type([eltype(v) for v in vs]...)
    ret = Array(args_type,0)
    all_elems = intersect([Set(v...) for v in vs]...)
    for v_elem in vs[1]
        if has(all_elems, v_elem)
            push!(ret, v_elem)
        end
    end
    ret
end
function union(vs...)
    args_type = promote_type([eltype(v) for v in vs]...)
    ret = Array(args_type,0)
    seen = Set()
    for v in vs
        for v_elem in v
            if !has(seen, v_elem)
                push!(ret, v_elem)
                add!(seen, v_elem)
            end
        end
    end
    ret
end
# setdiff only accepts two args
function setdiff(a, b)
    args_type = promote_type(eltype(a), eltype(b))
    bset = Set(b...)
    ret = Array(args_type,0)
    seen = Set()
    for a_elem in a
        if !has(seen, a_elem) && !has(bset, a_elem)
            push!(ret, a_elem)
            add!(seen, a_elem)
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
