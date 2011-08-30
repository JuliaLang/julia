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

mcopy_to{T}(dest::Ptr{T}, src::Ptr{T}, n::Int) =
    ccall(:memcpy, Ptr{Void}, (Ptr{Void}, Ptr{Void}, Ulong), dest, src, ulong(n*sizeof(T)))

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

zeros{T}(::Type{T}, dims::Dims) = fill(Array(T, dims), zero(T))
zeros(T::Type, dims::Size...) = zeros(T, dims)
zeros(dims::Dims) = zeros(Float64, dims)
zeros(dims::Size...) = zeros(dims)

ones{T}(::Type{T}, dims::Dims) = fill(Array(T, dims), one(T))
ones(T::Type, dims::Size...) = ones(T, dims)
ones(dims::Dims) = ones(Float64, dims)
ones(dims::Size...) = ones(dims)

trues(dims::Dims) = fill(Array(Bool, dims), true)
trues(dims::Size...) = trues(dims)

falses(dims::Dims) = fill(Array(Bool, dims), false)
falses(dims::Size...) = falses(dims)

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

int8   {T,n}(x::Array{T,n}) = convert(Array{Int8   ,n}, x)
uint8  {T,n}(x::Array{T,n}) = convert(Array{Uint8  ,n}, x)
int16  {T,n}(x::Array{T,n}) = convert(Array{Int16  ,n}, x)
uint16 {T,n}(x::Array{T,n}) = convert(Array{Uint16 ,n}, x)
int32  {T,n}(x::Array{T,n}) = convert(Array{Int32  ,n}, x)
uint32 {T,n}(x::Array{T,n}) = convert(Array{Uint32 ,n}, x)
int64  {T,n}(x::Array{T,n}) = convert(Array{Int64  ,n}, x)
uint64 {T,n}(x::Array{T,n}) = convert(Array{Uint64 ,n}, x)
bool   {T,n}(x::Array{T,n}) = convert(Array{Bool   ,n}, x)
char   {T,n}(x::Array{T,n}) = convert(Array{Char   ,n}, x)
float32{T,n}(x::Array{T,n}) = convert(Array{typeof(float32(zero(T))),n}, x)
float64{T,n}(x::Array{T,n}) = convert(Array{typeof(float64(zero(T))),n}, x)

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
    ccall(:jl_array_grow_end, Void, (Any, Ulong), a, ulong(1))
    a[end] = item
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
    ccall(:jl_array_grow_beg, Void, (Any, Ulong), a, ulong(1))
    a[1] = item
    return a
end

function insert{T}(a::Array{T,1}, i::Int, item)
    if i < 1
        throw(BoundsError())
    end
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
