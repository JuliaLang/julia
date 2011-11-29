## array.j: Dense arrays

typealias Vector{T} Array{T,1}
typealias Matrix{T} Array{T,2}
typealias VecOrMat{T} Union(Vector{T}, Matrix{T})

typealias StridedVector{T} Union(Vector{T}, SubArray{T,1,Array{T}})
typealias StridedMatrix{T} Union(Matrix{T}, SubArray{T,2,Array{T}})
typealias StridedVecOrMat{T} Union(StridedVector{T}, StridedMatrix{T})

## Basic functions ##

size(a::Array) = arraysize(a)
size(a::Array, d) = arraysize(a, d)
size(a::Matrix) = (arraysize(a,1), arraysize(a,2))
numel(a::Array) = arraylen(a)

## copy ##

copy_to{T}(dest::Ptr{T}, src::Ptr{T}, n::Int) =
    ccall(:memcpy, Ptr{Void}, (Ptr{Void}, Ptr{Void}, Ulong), dest, src, ulong(n*sizeof(T)))

function copy_to{T}(dest::Array{T}, src::Array{T})
    if isa(T, BitsKind)
        copy_to(pointer(dest), pointer(src), numel(src))
    else
        for i=1:numel(src)
            dest[i] = copy(src[i])
        end
    end
    return dest
end

function reinterpret{T,S}(::Type{T}, a::Array{S})
    b = Array(T, div(numel(a)*sizeof(S),sizeof(T)))
    ccall(dlsym(libc, :memcpy),
          Ptr{T}, (Ptr{T}, Ptr{S}, Ulong),
          b, a, ulong(length(b)*sizeof(T)))
    return b
end
reinterpret(t,x) = reinterpret(t,[x])[1]

function reshape{T,N}(a::Array{T}, dims::NTuple{N,Size})
    if prod(dims) != numel(a)
        error("reshape: invalid dimensions")
    end
    ccall(:jl_reshape_array, Any, (Any, Any, Any),
          Array{T,N}, a, dims)::Array{T,N}
end

## Constructors ##

jl_comprehension_zeros{T,n}(oneresult::AbstractArray{T,n}, dims...) = Array(T, dims...)
jl_comprehension_zeros{T}(oneresult::T, dims...) = Array(T, dims...)
jl_comprehension_zeros(oneresult::(), dims...) = Array(None, dims...)

similar(a::Array, T::Type, dims::Dims) = Array(T, dims)
similar{T}(a::Array{T,1}) = Array(T, size(a,1))
similar{T}(a::Array{T,2}) = Array(T, size(a,1), size(a,2))
similar{T}(a::Array{T,1}, S::Type) = Array(S, size(a,1))
similar{T}(a::Array{T,2}, S::Type) = Array(S, size(a,1), size(a,2))

empty(T) = Array(T, 0)

zeros{T}(::Type{T}, dims::Dims) = fill!(Array(T, dims), zero(T))
zeros(T::Type, dims::Size...) = zeros(T, dims)
zeros(dims::Dims) = zeros(Float64, dims)
zeros(dims::Size...) = zeros(dims)

ones{T}(::Type{T}, dims::Dims) = fill!(Array(T, dims), one(T))
ones(T::Type, dims::Size...) = ones(T, dims)
ones(dims::Dims) = ones(Float64, dims)
ones(dims::Size...) = ones(dims)

trues(dims::Dims) = fill!(Array(Bool, dims), true)
trues(dims::Size...) = trues(dims)

falses(dims::Dims) = fill!(Array(Bool, dims), false)
falses(dims::Size...) = falses(dims)

fill(v, dims::Dims) = fill!(Array(typeof(v), dims), v)
fill(v, dims::Size...) = fill(v, dims)

function linspace(start::Real, stop::Real, n::Int)
    (start, stop) = promote(start, stop)
    a = Array(typeof(start), long(n))
    if n == 1
        a[1] = start
        return a
    end
    step = (stop-start)/(n-1)
    for i=1:n
        a[i] = start+(i-1)*step
    end
    a
end

linspace(start::Real, stop::Real) = [ i | i=start:stop ]

## Conversions ##

convert{T,n}(::Type{Array{T,n}}, x::Array{T,n}) = x
convert{T,n,S}(::Type{Array{T,n}}, x::Array{S,n}) = copy_to(similar(x,T), x)

## Transpose ##

function transpose{T<:Union(Float64,Float32,Complex128,Complex64)}(A::Matrix{T})
    if numel(A) > 50000
        return _jl_fftw_transpose(A)
    else
        return [ A[j,i] | i=1:size(A,2), j=1:size(A,1) ]
    end
end

function ctranspose{T<:Union(Float64,Float32)}(A::Matrix{T})
    if numel(A) > 50000
        return _jl_fftw_transpose(A)
    else
        return [ A[j,i] | i=1:size(A,2), j=1:size(A,1) ]
    end
end

## Indexing: ref ##

ref(a::Array, i::Index) = arrayref(a,i)
ref(a::Array, i::Int) = arrayref(a,long(i))
ref{T}(a::Array{T,0}) = arrayref(a,1)
ref{T}(a::Array{T,1}, i::Index) = arrayref(a,i)
ref{T}(a::Array{T,1}, i::Int) = arrayref(a,long(i))
ref(a::Array{Any,1}, i::Index) = arrayref(a,i)
ref(a::Array{Any,1}, i::Int) = arrayref(a,long(i))
ref{T}(a::Array{T,2}, i::Index, j::Index) = arrayref(a, (j-1)*arraysize(a,1)+i)
ref{T}(a::Array{T,2}, i::Int, j::Int) = arrayref(a,long((j-1)*arraysize(a,1)+i))

function slicedim(A::Array, d::Int, i::Int)
    d_in = size(A)
    leading = d_in[1:(d-1)]
    d_out = append(leading, (1,), d_in[(d+1):end])

    M = prod(leading)
    N = numel(A)
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

function flipdim{T}(A::Array{T}, d::Int)
    nd = ndims(A)
    sd = d > nd ? 1 : size(A, d)
    if sd == 1
        return copy(A)
    end

    B = similar(A)

    nnd = 0
    for i = 1:nd
        nnd += count(size(A,i)==1 || i==d)
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
    N = numel(A)
    stride = M * sd

    if M==1
        for j = 0:stride:(N-stride)
            for i = 1:sd
                ri = sd+1-i
                B[j + ri] = A[j + i]
            end
        end
    else
        if isa(T,BitsKind) && M>200
            for i = 1:sd
                ri = sd+1-i
                for j=0:stride:(N-stride)
                    offs = j + 1 + (i-1)*M
                    boffs = j + 1 + (ri-1)*M
                    copy_to(pointer(B, boffs), pointer(A, offs), M)
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

## Indexing: assign ##

assign(A::Array{Any}, x::AbstractArray, i::Index) = arrayset(A,i,x)
assign(A::Array{Any}, x::AbstractArray, i::Int) = arrayset(A,long(i),x)
assign(A::Array{Any}, x::ANY, i::Index) = arrayset(A,i,x)
assign(A::Array{Any}, x::ANY, i::Int) = arrayset(A,long(i),x)
assign{T}(A::Array{T}, x::AbstractArray, i::Index) = arrayset(A,i,convert(T, x))
assign{T}(A::Array{T}, x::AbstractArray, i::Int) = arrayset(A,long(i),convert(T, x))
assign{T}(A::Array{T}, x, i::Index) = arrayset(A,i,convert(T, x))
assign{T}(A::Array{T}, x, i::Int) = arrayset(A,long(i),convert(T, x))
assign{T}(A::Array{T,0}, x) = arrayset(A,1,convert(T, x))

## Dequeue functionality ##

function push{T}(a::Array{T,1}, item)
    if is(T,None)
        error("[] cannot grow. Instead, initialize the array with \"empty(element_type)\".")
    end
    # convert first so we don't grow the array if the assignment won't work
    item = convert(T, item)
    ccall(:jl_array_grow_end, Void, (Any, Ulong), a, ulong(1))
    a[end] = item
    return a
end

function push(a::Array{Any,1}, item::ANY)
    ccall(:jl_array_grow_end, Void, (Any, Ulong), a, ulong(1))
    a[end] = item
    return a
end

function append!{T}(a::Array{T,1}, items::Array{T,1})
    if is(T,None)
        error("[] cannot grow. Instead, initialize the array with \"empty(element_type)\".")
    end
    n = length(items)
    ccall(:jl_array_grow_end, Void, (Any, Ulong), a, ulong(n))
    a[end-n+1:end] = items
    return a
end

function grow{T}(a::Array{T,1}, n::Int)
    ccall(:jl_array_grow_end, Void, (Any, Ulong), a, ulong(n))
    return a
end

function pop{T}(a::Array{T,1})
    if isempty(a)
        error("pop: array is empty")
    end
    item = a[end]
    ccall(:jl_array_del_end, Void, (Any, Ulong), a, ulong(1))
    return item
end

function enq{T}(a::Array{T,1}, item)
    if is(T,None)
        error("[] cannot grow. Instead, initialize the array with \"empty(element_type)\".")
    end
    item = convert(T, item)
    ccall(:jl_array_grow_beg, Void, (Any, Ulong), a, ulong(1))
    a[1] = item
    return a
end

function insert{T}(a::Array{T,1}, i::Int, item)
    if i < 1
        throw(BoundsError())
    end
    item = convert(T, item)
    l = length(a)
    if i > l
        ccall(:jl_array_grow_end, Void, (Any, Ulong), a, ulong(i-l))
    elseif i > div(l,2)
        ccall(:jl_array_grow_end, Void, (Any, Ulong), a, ulong(1))
        for k=l+1:-1:i+1
            a[k] = a[k-1]
        end
    else
        ccall(:jl_array_grow_beg, Void, (Any, Ulong), a, ulong(1))
        for k=1:(i-1)
            a[k] = a[k+1]
        end
    end
    a[i] = item
end

function del{T}(a::Array{T,1}, i::Int)
    l = length(a)
    if !(1 <= i <= l)
        throw(BoundsError())
    end
    if i > div(l,2)
        for k=i:l-1
            a[k] = a[k+1]
        end
        ccall(:jl_array_del_end, Void, (Any, Ulong), a, ulong(1))
    else
        for k=i:-1:2
            a[k] = a[k-1]
        end
        ccall(:jl_array_del_beg, Void, (Any, Ulong), a, ulong(1))
    end
    a
end

function del_all{T}(a::Array{T,1})
    ccall(:jl_array_del_end, Void, (Any, Ulong), a, ulong(length(a)))
    a
end
