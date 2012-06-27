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

## copy ##

function copy_to{T}(dest::Array{T}, dsto, src::Array{T}, so, N)
    if so+N-1 > numel(src) || dsto+N-1 > numel(dest) || dsto < 1 || so < 1
        throw(BoundsError())
    end
    if isa(T, BitsKind)
        ccall(:memcpy, Ptr{Void}, (Ptr{Void}, Ptr{Void}, Uint),
              pointer(dest, dsto), pointer(src, so), N*sizeof(T))
    else
        for i=0:N-1
            dest[i+dsto] = src[i+so]
        end
    end
    return dest
end

copy_to{T}(dest::Array{T}, src::Array{T}) = copy_to(dest, 1, src, 1, numel(src))

function reinterpret{T,S}(::Type{T}, a::Array{S,1})
    nel = int(div(numel(a)*sizeof(S),sizeof(T)))
    ccall(:jl_reshape_array, Array{T,1}, (Any, Any, Any), Array{T,1}, a, (nel,))
end

function reinterpret{T,S}(::Type{T}, a::Array{S})
    if sizeof(S) != sizeof(T)
        error("reinterpret: result shape not specified")
    end
    reinterpret(T, a, size(a))
end

function reinterpret{T,S,N}(::Type{T}, a::Array{S}, dims::NTuple{N,Int})
    nel = div(numel(a)*sizeof(S),sizeof(T))
    if prod(dims) != nel
        error("reinterpret: invalid dimensions")
    end
    ccall(:jl_reshape_array, Array{T,N}, (Any, Any, Any), Array{T,N}, a, dims)
end
reinterpret(t::Type,x) = reinterpret(t,[x])[1]

function reshape{T,N}(a::Array{T}, dims::NTuple{N,Int})
    if prod(dims) != numel(a)
        error("reshape: invalid dimensions")
    end
    ccall(:jl_reshape_array, Array{T,N}, (Any, Any, Any), Array{T,N}, a, dims)
end

## Constructors ##

_jl_comprehension_zeros{T,n}(oneresult::AbstractArray{T,n}, dims...) = Array(T, dims...)
_jl_comprehension_zeros{T}(oneresult::T, dims...) = Array(T, dims...)
_jl_comprehension_zeros(oneresult::(), dims...) = Array(None, dims...)

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
function ref{T}(::Type{T}, vals...)
    a = Array(T,length(vals))
    for i = 1:length(vals)
        a[i] = vals[i]
    end
    return a
end

# T[a:b] and T[a:s:b] also contruct typed ranges
function ref{T<:Number}(::Type{T}, r::Ranges)
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
function fill!{T<:Union(Integer,Float)}(a::Array{T}, x)
    if isa(T,BitsKind) && convert(T,x) == 0
        ccall(:memset, Ptr{T}, (Ptr{T}, Int32, Int32), a,0,length(a)*sizeof(T))
    else
        for i = 1:numel(a)
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

trues(args...)  = fill(true, args...)
falses(args...) = fill(false, args...)

eye(n::Int) = eye(n, n)
function eye(m::Int, n::Int)
    a = zeros(m,n)
    for i = 1:min(m,n)
        a[i,i] = 1
    end
    return a
end
function one{T}(x::StridedMatrix{T})
    m, n = size(x)
    a = zeros(T,size(x))
    for i = 1:min(m,n)
        a[i,i] = 1
    end
    return a
end

function linspace(start::Real, stop::Real, n::Integer)
    (start, stop) = promote(start, stop)
    a = Array(typeof(start), int(n))
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

linspace(start::Real, stop::Real) = linspace(start, stop, 100)

logspace(start::Real, stop::Real, n::Integer) = 10.^linspace(start, stop, n)
logspace(start::Real, stop::Real) = logspace(start, stop, 50)

## Conversions ##

convert{T,n}(::Type{Array{T}}, x::Array{T,n}) = x
convert{T,n}(::Type{Array{T,n}}, x::Array{T,n}) = x
convert{T,n,S}(::Type{Array{T}}, x::Array{S,n}) = convert(Array{T,n}, x)
convert{T,n,S}(::Type{Array{T,n}}, x::Array{S,n}) = copy_to(similar(x,T), x)

## Indexing: ref ##

ref(a::Array, i::Int) = arrayref(a,i)
ref(a::Array, i::Integer) = arrayref(a,int(i))
ref{T}(a::Array{T,0}) = arrayref(a,1)
ref{T}(a::Array{T,1}, i::Int) = arrayref(a,i)
ref{T}(a::Array{T,1}, i::Integer) = arrayref(a,int(i))
ref(a::Array{Any,1}, i::Int) = arrayref(a,i)
ref(a::Array{Any,1}, i::Integer) = arrayref(a,int(i))

ref(A::Array, i0::Integer, i1::Integer) = A[i0 + size(A,1)*(i1-1)]
ref(A::Array, i0::Integer, i1::Integer, i2::Integer) =
    A[i0 + size(A,1)*((i1-1) + size(A,2)*(i2-1))]
ref(A::Array, i0::Integer, i1::Integer, i2::Integer, i3::Integer) =
    A[i0 + size(A,1)*((i1-1) + size(A,2)*((i2-1) + size(A,3)*(i3-1)))]

function ref(A::Array, I::Integer...)
    ndims = length(I)
    index = I[1]
    stride = 1
    for k=2:ndims
        stride = stride * size(A, k-1)
        index += (I[k]-1) * stride
    end
    return A[index]
end

# note: this is also useful for Ranges
ref{T<:Integer}(A::Vector, I::AbstractVector{T}) = [ A[i] for i=I ]
ref{T<:Integer}(A::AbstractVector, I::AbstractVector{T}) = [ A[i] for i=I ]

ref{T<:Integer}(A::Matrix, I::AbstractVector{T}, j::Integer) = [ A[i,j] for i=I ]
ref{T<:Integer}(A::Matrix, I::Integer, J::AbstractVector{T}) = [ A[i,j] for i=I, j=J ]
ref{T<:Integer}(A::Matrix, I::AbstractVector{T}, J::AbstractVector{T}) = [ A[i,j] for i=I, j=J ]

let ref_cache = nothing
global ref
function ref(A::Array, I::Indices...)
    I = indices(I...)
    X = similar(A, ref_shape(I...))

    if is(ref_cache,nothing)
        ref_cache = Dict()
    end
    gen_cartesian_map(ref_cache, ivars -> quote
            X[storeind] = A[$(ivars...)]
            storeind += 1
        end, I, (:A, :X, :storeind), A, X, 1)
    return X
end
end

# logical indexing

function _jl_ref_bool_1d(A::Array, I::AbstractArray{Bool})
    n = sum(I)
    out = similar(A, n)
    c = 1
    for i = 1:numel(I)
        if I[i]
            out[c] = A[i]
            c += 1
        end
    end
    out
end

ref(A::Vector, I::AbstractVector{Bool}) = _jl_ref_bool_1d(A, I)
ref(A::Vector, I::AbstractArray{Bool}) = _jl_ref_bool_1d(A, I)
ref(A::Array, I::AbstractVector{Bool}) = _jl_ref_bool_1d(A, I)
ref(A::Array, I::AbstractArray{Bool}) = _jl_ref_bool_1d(A, I)

ref(A::Matrix, I::Integer, J::AbstractVector{Bool}) = A[I,find(J)]
ref(A::Matrix, I::AbstractVector{Bool}, J::Integer) = A[find(I),J]
ref(A::Matrix, I::AbstractVector{Bool}, J::AbstractVector{Bool}) = A[find(I),find(J)]
ref{T<:Integer}(A::Matrix, I::AbstractVector{T}, J::AbstractVector{Bool}) = [ A[i,j] for i=I, j=find(J) ]
ref{T<:Integer}(A::Matrix, I::AbstractVector{Bool}, J::AbstractVector{T}) = [ A[i,j] for i=find(I), j=J ]

## Indexing: assign ##

assign(A::Array{Any}, x::AbstractArray, i::Integer) = arrayset(A,int(i),x)
assign(A::Array{Any}, x::ANY, i::Integer) = arrayset(A,int(i),x)
assign{T}(A::Array{T}, x::AbstractArray, i::Integer) = arrayset(A,int(i),convert(T, x))
assign{T}(A::Array{T}, x, i::Integer) = arrayset(A,int(i),convert(T, x))
assign{T}(A::Array{T,0}, x) = arrayset(A,1,convert(T, x))

assign(A::Array, x, i0::Integer, i1::Integer) =
    A[i0 + size(A,1)*(i1-1)] = x
assign(A::Array, x::AbstractArray, i0::Integer, i1::Integer) =
    A[i0 + size(A,1)*(i1-1)] = x

assign(A::Array, x, i0::Integer, i1::Integer, i2::Integer) =
    A[i0 + size(A,1)*((i1-1) + size(A,2)*(i2-1))] = x
assign(A::Array, x::AbstractArray, i0::Integer, i1::Integer, i2::Integer) =
    A[i0 + size(A,1)*((i1-1) + size(A,2)*(i2-1))] = x

assign(A::Array, x, i0::Integer, i1::Integer, i2::Integer, i3::Integer) =
    A[i0 + size(A,1)*((i1-1) + size(A,2)*((i2-1) + size(A,3)*(i3-1)))] = x
assign(A::Array, x::AbstractArray, i0::Integer, i1::Integer, i2::Integer, i3::Integer) =
    A[i0 + size(A,1)*((i1-1) + size(A,2)*((i2-1) + size(A,3)*(i3-1)))] = x

assign(A::Array, x, I0::Integer, I::Integer...) = assign_scalarND(A,x,I0,I...)
assign(A::Array, x::AbstractArray, I0::Integer, I::Integer...) =
    assign_scalarND(A,x,I0,I...)

function assign_scalarND(A, x, I0::Integer, I::Integer...)
    index = I0
    stride = 1
    for k=1:length(I)
        stride = stride * size(A, k)
        index += (I[k]-1) * stride
    end
    A[index] = x
    return A
end

function assign{T<:Integer}(A::Array, x, I::AbstractVector{T})
    for i in I
        A[i] = x
    end
    return A
end

function assign{T<:Integer}(A::Array, X::AbstractArray, I::AbstractVector{T})
    if length(X) != length(I); error("argument dimensions must match"); end
    count = 1
    for i in I
        A[i] = X[count]
        count += 1
    end
    return A
end

function assign{T<:Integer}(A::Matrix, x, i::Integer, J::AbstractVector{T})
    m = size(A, 1)
    for j in J
        A[(j-1)*m + i] = x
    end
    return A
end
function assign{T<:Integer}(A::Matrix, X::AbstractArray, i::Integer, J::AbstractVector{T})
    if length(X) != length(J); error("argument dimensions must match"); end
    m = size(A, 1)
    count = 1
    for j in J
        A[(j-1)*m + i] = X[count]
        count += 1
    end
    return A
end

function assign{T<:Integer}(A::Matrix, x, I::AbstractVector{T}, j::Integer)
    m = size(A, 1)
    offset = (j-1)*m
    for i in I
        A[offset + i] = x
    end
    return A
end
function assign{T<:Integer}(A::Matrix, X::AbstractArray, I::AbstractVector{T}, j::Integer)
    if length(X) != length(I); error("argument dimensions must match"); end
    m = size(A, 1)
    offset = (j-1)*m
    count = 1
    for i in I
        A[offset + i] = X[count]
        count += 1
    end
    return A
end

function assign{T<:Integer}(A::Matrix, x, I::AbstractVector{T}, J::AbstractVector{T})
    m = size(A, 1)
    for j in J
        offset = (j-1)*m
        for i in I
            A[offset + i] = x
        end
    end
    return A
end
function assign{T<:Integer}(A::Matrix, X::AbstractArray, I::AbstractVector{T}, J::AbstractVector{T})
    nel = length(I)*length(J)
    if length(X) != nel ||
        (ndims(X) > 1 && (size(X,1)!=length(I) || size(X,2)!=length(J)))
        error("argument dimensions must match")
    end
    m = size(A, 1)
    count = 1
    for j in J
        offset = (j-1)*m
        for i in I
            A[offset + i] = X[count]
            count += 1
        end
    end
    return A
end

let assign_cache = nothing
global assign
function assign(A::Array, x, I0::Indices, I::Indices...)
    I0 = indices(I0)
    I = indices(I...)
    if is(assign_cache,nothing)
        assign_cache = Dict()
    end
    gen_cartesian_map(assign_cache, ivars->:(A[$(ivars...)] = x),
                      tuple(I0, I...),
                      (:A, :x),
                      A, x)
    return A
end
end

let assign_cache = nothing
global assign
function assign(A::Array, X::AbstractArray, I0::Indices, I::Indices...)
    I0 = indices(I0)
    I = indices(I...)
    nel = length(I0)
    for idx in I
        nel *= length(idx)
    end
    if length(X) != nel
        error("argument dimensions must match")
    end
    if ndims(X) > 1
        if size(X,1) != length(I0)
            error("argument dimensions must match")
        end
        for i = 1:length(I)
            if size(X,i+1) != length(I[i])
                error("argument dimensions must match")
            end
        end
    end
    if is(assign_cache,nothing)
        assign_cache = Dict()
    end
    gen_cartesian_map(assign_cache, ivars->:(A[$(ivars...)] = X[refind];
                                             refind += 1),
                      tuple(I0, I...),
                      (:A, :X, :refind),
                      A, X, 1)
    return A
end
end

# logical indexing

function _jl_assign_bool_scalar_1d(A::Array, x, I::AbstractArray{Bool})
    n = sum(I)
    for i = 1:numel(I)
        if I[i]
            A[i] = x
        end
    end
    A
end

function _jl_assign_bool_vector_1d(A::Array, X::AbstractArray, I::AbstractArray{Bool})
    n = sum(I)
    c = 1
    for i = 1:numel(I)
        if I[i]
            A[i] = X[c]
            c += 1
        end
    end
    A
end

assign(A::Array, X::AbstractArray, I::AbstractVector{Bool}) = _jl_assign_bool_vector_1d(A, X, I)
assign(A::Array, X::AbstractArray, I::AbstractArray{Bool}) = _jl_assign_bool_vector_1d(A, X, I)
assign(A::Array, x, I::AbstractVector{Bool}) = _jl_assign_bool_scalar_1d(A, x, I)
assign(A::Array, x, I::AbstractArray{Bool}) = _jl_assign_bool_scalar_1d(A, x, I)

assign(A::Matrix, x::AbstractArray, I::Integer, J::AbstractVector{Bool}) = (A[I,find(J)]=x)
assign(A::Matrix, x, I::Integer, J::AbstractVector{Bool}) = (A[I,find(J)]=x)

assign(A::Matrix, x::AbstractArray, I::AbstractVector{Bool}, J::Integer) = (A[find(I),J]=x)
assign(A::Matrix, x, I::AbstractVector{Bool}, J::Integer) = (A[find(I),J]=x)

assign(A::Matrix, x::AbstractArray, I::AbstractVector{Bool}, J::AbstractVector{Bool}) = (A[find(I),find(J)]=x)
assign(A::Matrix, x, I::AbstractVector{Bool}, J::AbstractVector{Bool}) = (A[find(I),find(J)]=x)

assign{T<:Integer}(A::Matrix, x::AbstractArray, I::AbstractVector{T}, J::AbstractVector{Bool}) = (A[I,find(J)]=x)
assign{T<:Integer}(A::Matrix, x, I::AbstractVector{T}, J::AbstractVector{Bool}) = (A[I,find(J)]=x)

assign{T<:Integer}(A::Matrix, x::AbstractArray, I::AbstractVector{Bool}, J::AbstractVector{T}) = (A[find(I),J]=x)
assign{T<:Integer}(A::Matrix, x, I::AbstractVector{Bool}, J::AbstractVector{T}) = (A[find(I),J]=x)

## Dequeue functionality ##

function push{T}(a::Array{T,1}, item)
    if is(T,None)
        error("[] cannot grow. Instead, initialize the array with \"T[]\".")
    end
    # convert first so we don't grow the array if the assignment won't work
    item = convert(T, item)
    ccall(:jl_array_grow_end, Void, (Any, Uint), a, 1)
    a[end] = item
    return a
end

function push(a::Array{Any,1}, item::ANY)
    ccall(:jl_array_grow_end, Void, (Any, Uint), a, 1)
    arrayset(a, length(a), item)
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

function append{T}(a::Array{T,1}, items::Array{T,1})
    if is(T,None)
        error("[] cannot grow. Instead, initialize the array with \"T[]\".")
    end
    n0 = length(a)
    n1 = length(items)
    r = Array(T, n0 + n1)
    r[1:n0] = a
    r[n0+1:n0+n1] = items
    return r
end

function grow(a::Vector, n::Integer)
    if n < -length(a)
        throw(BoundsError())
    end
    ccall(:jl_array_grow_end, Void, (Any, Uint), a, n)
    return a
end

function pop(a::Vector)
    if isempty(a)
        error("pop: array is empty")
    end
    item = a[end]
    ccall(:jl_array_del_end, Void, (Any, Uint), a, 1)
    return item
end

function enqueue{T}(a::Array{T,1}, item)
    if is(T,None)
        error("[] cannot grow. Instead, initialize the array with \"T[]\".")
    end
    item = convert(T, item)
    ccall(:jl_array_grow_beg, Void, (Any, Uint), a, 1)
    a[1] = item
    return a
end
const unshift = enqueue

function shift(a::Vector)
    if isempty(a)
        error("shift: array is empty")
    end
    item = a[1]
    ccall(:jl_array_del_beg, Void, (Any, Uint), a, 1)
    return item
end

function insert{T}(a::Array{T,1}, i::Integer, item)
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

function del(a::Vector, i::Integer)
    n = length(a)
    if !(1 <= i <= n)
        throw(BoundsError())
    end
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
    return a
end

function del{T<:Integer}(a::Vector, r::Range1{T})
    n = length(a)
    f = first(r)
    l = last(r)
    if !(1 <= f && l <= n)
        throw(BoundsError())
    end
    if l < f
        return a
    end
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
    return a
end

function del_all(a::Vector)
    ccall(:jl_array_del_end, Void, (Any, Uint), a, length(a))
    return a
end

## Unary operators ##

function conj!{T<:Number}(A::StridedArray{T})
    for i=1:numel(A)
        A[i] = conj(A[i])
    end
    return A
end

for f in (:-, :~, :conj, :sign)
    @eval begin
        function ($f)(A::StridedArray)
            F = similar(A)
            for i=1:numel(A)
                F[i] = ($f)(A[i])
            end
            return F
        end
    end
end

for f in (:real, :imag)
    @eval begin
        function ($f){T}(A::StridedArray{T})
            S = typeof(($f)(zero(T)))
            F = similar(A, S)
            for i=1:numel(A)
                F[i] = ($f)(A[i])
            end
            return F
        end
    end
end

function !(A::StridedArray{Bool})
    F = similar(A)
    for i=1:numel(A)
        F[i] = !A[i]
    end
    return F
end

## Binary arithmetic operators ##

# ^ is difficult, since negative exponents give a different type

./(x::Array, y::Array ) = reshape( [ x[i] ./ y[i] for i=1:numel(x) ], size(x) )
./(x::Number,y::Array ) = reshape( [ x    ./ y[i] for i=1:numel(y) ], size(y) )
./(x::Array, y::Number) = reshape( [ x[i] ./ y    for i=1:numel(x) ], size(x) )

.^(x::Array, y::Array ) = reshape( [ x[i] ^ y[i] for i=1:numel(x) ], size(x) )
.^(x::Number,y::Array ) = reshape( [ x    ^ y[i] for i=1:numel(y) ], size(y) )
.^(x::Array, y::Number) = reshape( [ x[i] ^ y    for i=1:numel(x) ], size(x) )

function .^{S<:Integer,T<:Integer}(A::Array{S}, B::Array{T})
    F = Array(Float64, promote_shape(size(A), size(B)))
    for i=1:numel(A)
        F[i] = float64(A[i])^float64(B[i])
    end
    return F
end

function .^{T<:Integer}(A::Integer, B::Array{T})
    F = similar(B, Float64)
    for i=1:numel(B)
        F[i] = float64(A)^float64(B[i])
    end
    return F
end

function _jl_power_array_int_body{T}(F::Array{T}, A, B)
    for i=1:numel(A)
        F[i] = A[i]^convert(T,B)
    end
    return F
end

function .^{T<:Integer}(A::Array{T}, B::Integer)
    F = similar(A, B < 0 ? Float64 : promote_type(T,typeof(B)))
    _jl_power_array_int_body(F, A, B)
end

for f in (:+, :-, :.*, :div, :mod, :&, :|, :$)
    @eval begin
        function ($f){S,T}(A::AbstractArray{S}, B::AbstractArray{T})
            F = Array(promote_type(S,T), promote_shape(size(A),size(B)))
            for i=1:numel(A)
                F[i] = ($f)(A[i], B[i])
            end
            return F
        end
        function ($f){T}(A::Number, B::AbstractArray{T})
            F = similar(B, promote_type(typeof(A),T))
            for i=1:numel(B)
                F[i] = ($f)(A, B[i])
            end
            return F
        end
        function ($f){T}(A::AbstractArray{T}, B::Number)
            F = similar(A, promote_type(T,typeof(B)))
            for i=1:numel(A)
                F[i] = ($f)(A[i], B)
            end
            return F
        end
    end
end

# functions that should give an Int result for Bool arrays
for f in (:+, :-, :div)
    @eval begin
        function ($f)(x::Bool, y::Array{Bool})
            reshape([ ($f)(x, y[i]) for i=1:numel(y) ], size(y))
        end
        function ($f)(x::Array{Bool}, y::Bool)
            reshape([ ($f)(x[i], y) for i=1:numel(x) ], size(x))
        end
        function ($f)(x::Array{Bool}, y::Array{Bool})
            shp = promote_shape(size(x),size(y))
            reshape([ ($f)(x[i], y[i]) for i=1:numel(x) ], shp)
        end
    end
end

## promotion to complex ##

function complex{S<:Real,T<:Real}(A::Array{S}, B::Array{T})
    F = similar(A, typeof(complex(zero(S),zero(T))))
    for i=1:numel(A)
        F[i] = complex(A[i], B[i])
    end
    return F
end

function complex{T<:Real}(A::Real, B::Array{T})
    F = similar(B, typeof(complex(A,zero(T))))
    for i=1:numel(B)
        F[i] = complex(A, B[i])
    end
    return F
end

function complex{T<:Real}(A::Array{T}, B::Real)
    F = similar(A, typeof(complex(zero(T),B)))
    for i=1:numel(A)
        F[i] = complex(A[i], B)
    end
    return F
end

function complex{T<:Real}(A::Array{T})
    z = zero(T)
    F = similar(A, typeof(complex(z,z)))
    for i=1:numel(A)
        F[i] = complex(A[i], z)
    end
    return F
end

## Binary comparison operators ##

for (f,scalarf) in ((:(.==),:(==)), (:.<, :<), (:.!=,:!=), (:.<=,:<=))
    @eval begin
        function ($f)(A::AbstractArray, B::AbstractArray)
            F = Array(Bool, promote_shape(size(A),size(B)))
            for i = 1:numel(B)
                F[i] = ($scalarf)(A[i], B[i])
            end
            return F
        end
        ($f)(A, B::AbstractArray) =
            reshape([ ($scalarf)(A, B[i]) for i=1:length(B)], size(B))
        ($f)(A::AbstractArray, B) =
            reshape([ ($scalarf)(A[i], B) for i=1:length(A)], size(A))
    end
end

## data movement ##

function slicedim(A::Array, d::Integer, i::Integer)
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
                    copy_to(B, boffs, A, offs, M)
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
const rot90 = rotl90

reverse(v::StridedVector) = (n=length(v); [ v[n-i+1] for i=1:n ])
function reverse!(v::StridedVector)
    n = length(v)
    r = n
    for i=1:div(n,2)
        v[i], v[r] = v[r], v[i]
        r -= 1
    end
    v
end

## find ##

function nnz(a::StridedArray)
    n = 0
    for i = 1:numel(a)
        n += bool(a[i]) ? 1 : 0
    end
    return n
end

# returns the index of the first non-zero element, or 0 if all zeros
function findfirst(A::StridedArray)
    for i = 1:length(A)
        if A[i] != 0
            return i
        end
    end
    return 0
end

# returns the index of the first matching element
function findfirst(A::StridedArray, v)
    for i = 1:length(A)
        if A[i] == v
            return i
        end
    end
    return 0
end

# returns the index of the first element for which the function returns true
function findfirst(testf::Function, A::StridedArray)
    for i = 1:length(A)
        if testf(A[i])
            return i
        end
    end
    return 0
end

function find(testf::Function, A::StridedArray)
    # use a dynamic-length array to store the indexes, then copy to a non-padded
    # array for the return
    tmpI = Array(Int, 0)
    for i = 1:length(A)
        if testf(A[i])
            push(tmpI, i)
        end
    end
    I = Array(Int, length(tmpI))
    copy_to(I, tmpI)
    I
end

function find(A::StridedArray)
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

findn(A::StridedVector) = find(A)

function findn(A::StridedMatrix)
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
    s = { quote I[$i][count] = $ivars[i] end for i = 1:length(ivars)}
    quote
    	Aind = A[$(ivars...)]
    	if Aind != z
    	    $(s...)
    	    count +=1
    	end
    end
end

global findn
function findn{T}(A::StridedArray{T})
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

function findn_nzs{T}(A::StridedMatrix{T})
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

function nonzeros{T}(A::StridedArray{T})
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

## Reductions ##

contains(s::Number, n::Number) = (s == n)

areduce{T}(f::Function, A::StridedArray{T}, region::Region, v0) =
    areduce(f,A,region,v0,T)

# TODO:
# - find out why inner loop with dimsA[i] instead of size(A,i) is way too slow

let areduce_cache = nothing
# generate the body of the N-d loop to compute a reduction
function gen_areduce_func(n, f)
    ivars = { gensym() for i=1:n }
    # limits and vars for reduction loop
    lo    = { gensym() for i=1:n }
    hi    = { gensym() for i=1:n }
    rvars = { gensym() for i=1:n }
    setlims = { quote
        # each dim of reduction is either 1:sizeA or ivar:ivar
        if contains(region,$i)
            $lo[i] = 1
            $hi[i] = size(A,$i)
        else
            $lo[i] = $hi[i] = $ivars[i]
        end
               end for i=1:n }
    rranges = { :( ($lo[i]):($hi[i]) ) for i=1:n }  # lo:hi for all dims
    body =
    quote
        _tot = v0
        $(setlims...)
        $make_loop_nest(rvars, rranges,
                        :(_tot = ($f)(_tot, A[$(rvars...)])))
        R[_ind] = _tot
        _ind += 1
    end
    quote
        local _F_
        function _F_(f, A, region, R, v0)
            _ind = 1
            $make_loop_nest(ivars, { :(1:size(R,$i)) for i=1:n }, body)
        end
        _F_
    end
end

global areduce
function areduce(f::Function, A::StridedArray, region::Region, v0, RType::Type)
    dimsA = size(A)
    ndimsA = ndims(A)
    dimsR = ntuple(ndimsA, i->(contains(region, i) ? 1 : dimsA[i]))
    R = similar(A, RType, dimsR)

    if is(areduce_cache,nothing)
        areduce_cache = Dict()
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

    if !has(areduce_cache,key)
        fexpr = gen_areduce_func(ndimsA, fname)
        func = eval(fexpr)
        areduce_cache[key] = func
    else
        func = areduce_cache[key]
    end

    func(f, A, region, R, v0)

    return R
end
end

function sum{T}(A::StridedArray{T})
    if isempty(A)
        return zero(T)
    end
    v = A[1]
    for i=2:numel(A)
        v += A[i]
    end
    v
end

function prod{T}(A::StridedArray{T})
    if isempty(A)
        return one(T)
    end
    v = A[1]
    for i=2:numel(A)
        v *= A[i]
    end
    v
end

function min{T<:Integer}(A::StridedArray{T})
    v = typemax(T)
    for i=1:numel(A)
        x = A[i]
        if x < v
            v = x
        end
    end
    v
end

function max{T<:Integer}(A::StridedArray{T})
    v = typemin(T)
    for i=1:numel(A)
        x = A[i]
        if x > v
            v = x
        end
    end
    v
end

max{T}(A::StridedArray{T}, b::(), region::Region) = areduce(max,A,region,typemin(T),T)
min{T}(A::StridedArray{T}, b::(), region::Region) = areduce(min,A,region,typemax(T),T)
sum{T}(A::StridedArray{T}, region::Region)  = areduce(+,A,region,zero(T))
prod{T}(A::StridedArray{T}, region::Region) = areduce(*,A,region,one(T))

all(A::StridedArray{Bool}, region::Region) = areduce(all,A,region,true)
any(A::StridedArray{Bool}, region::Region) = areduce(any,A,region,false)
sum(A::StridedArray{Bool}, region::Region) = areduce(+,A,region,0,Int)
sum(A::StridedArray{Bool}) = count(A)
prod(A::StridedArray{Bool}) =
    error("use all() instead of prod() for boolean arrays")
prod(A::StridedArray{Bool}, region::Region) =
    error("use all() instead of prod() for boolean arrays")

## map over arrays ##

## along an axis
function amap(f::Function, A::StridedArray, axis::Integer)
    dimsA = size(A)
    ndimsA = ndims(A)
    axis_size = dimsA[axis]

    if axis_size == 0
        return f(A)
    end

    idx = ntuple(ndimsA, j -> j == axis ? 1 : 1:dimsA[j])
    r = f(sub(A, idx))
    R = Array(typeof(r), axis_size)
    R[1] = r

    for i = 2:axis_size
        idx = ntuple(ndimsA, j -> j == axis ? i : 1:dimsA[j])
        R[i] = f(sub(A, idx))
    end

    return R
end


## 1 argument
function map_to(dest::StridedArray, f, A::StridedArray)
    for i=1:numel(A)
        dest[i] = f(A[i])
    end
    return dest
end
function map_to2(first, dest::StridedArray, f, A::StridedArray)
    dest[1] = first
    for i=2:numel(A)
        dest[i] = f(A[i])
    end
    return dest
end

function map(f, A::StridedArray)
    if isempty(A); return A; end
    first = f(A[1])
    dest = similar(A, typeof(first))
    return map_to2(first, dest, f, A)
end

## 2 argument
function map_to(dest::StridedArray, f, A::StridedArray, B::StridedArray)
    for i=1:numel(A)
        dest[i] = f(A[i], B[i])
    end
    return dest
end
function map_to2(first, dest::StridedArray, f,
                 A::StridedArray, B::StridedArray)
    dest[1] = first
    for i=2:numel(A)
        dest[i] = f(A[i], B[i])
    end
    return dest
end

function map(f, A::StridedArray, B::StridedArray)
    shp = promote_shape(size(A),size(B))
    if isempty(A)
        return similar(A, eltype(A), shp)
    end
    first = f(A[1], B[1])
    dest = similar(A, typeof(first), shp)
    return map_to2(first, dest, f, A, B)
end

function map_to(dest::StridedArray, f, A::StridedArray, B::Number)
    for i=1:numel(A)
        dest[i] = f(A[i], B)
    end
    return dest
end
function map_to2(first, dest::StridedArray, f, A::StridedArray, B::Number)
    dest[1] = first
    for i=2:numel(A)
        dest[i] = f(A[i], B)
    end
    return dest
end

function map(f, A::StridedArray, B::Number)
    if isempty(A); return A; end
    first = f(A[1], B)
    dest = similar(A, typeof(first))
    return map_to2(first, dest, f, A, B)
end

function map_to(dest::StridedArray, f, A::Number, B::StridedArray)
    for i=1:numel(B)
        dest[i] = f(A, B[i])
    end
    return dest
end
function map_to2(first, dest::StridedArray, f, A::Number, B::StridedArray)
    dest[1] = first
    for i=2:numel(B)
        dest[i] = f(A, B[i])
    end
    return dest
end

function map(f, A::Number, B::StridedArray)
    if isempty(A); return A; end
    first = f(A, B[1])
    dest = similar(B, typeof(first))
    return map_to2(first, dest, f, A, B)
end

## N argument
function map_to(dest::StridedArray, f, As::StridedArray...)
    n = numel(As[1])
    i = 1
    ith = a->a[i]
    for i=1:n
        dest[i] = f(map(ith, As)...)
    end
    return dest
end
function map_to2(first, dest::StridedArray, f, As::StridedArray...)
    n = numel(As[1])
    i = 1
    ith = a->a[i]
    dest[1] = first
    for i=2:n
        dest[i] = f(map(ith, As)...)
    end
    return dest
end

function map(f, As::StridedArray...)
    shape = mapreduce(promote_shape, size, As)
    if prod(shape) == 0
        return similar(As[1], eltype(As[1]), shape)
    end
    first = f(map(a->a[1], As)...)
    dest = similar(As[1], typeof(first), shape)
    return map_to2(first, dest, f, As...)
end

## Filter ##

# given a function returning a boolean and an array, return matching elements
function filter(f::Function, As::StridedArray)
    boolmap::Array{Bool} = map(f, As)
    As[boolmap]
end

## Transpose ##

function transpose{T<:Union(Float64,Float32,Complex128,Complex64)}(A::Matrix{T})
    if numel(A) > 50000
        return _jl_fftw_transpose(reshape(A, size(A, 2), size(A, 1)))
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

## Permute ##

let permute_cache = nothing, stridenames::Array{Any,1} = {}
global permute
function permute(A::StridedArray, perm)
    dimsA = size(A)
    ndimsA = length(dimsA)
    dimsP = ntuple(ndimsA, i->dimsA[perm[i]])
    P = similar(A, dimsP)
    ranges = ntuple(ndimsA, i->(colon(1,dimsP[i])))
    while length(stridenames) < ndimsA
        push(stridenames, gensym())
    end

    #calculates all the strides
    strides = [ stride(A, perm[dim]) for dim = 1:length(perm) ]

    #Creates offset, because indexing starts at 1
    offset = 0
    for i in strides
        offset+=i
    end
    offset = 1-offset

    function permute_one(ivars)
        len = length(ivars)
        counts = { gensym() for i=1:len}
        toReturn = cell(len+1,2)
        for i = 1:numel(toReturn)
            toReturn[i] = nothing
        end

        tmp = counts[end]
        toReturn[len+1] = quote
            ind = 1
            $tmp = $stridenames[len]
        end

        #inner most loop
        toReturn[1] = quote
            P[ind] = A[+($counts...)+offset]
            ind+=1
            $counts[1]+= $stridenames[1]
        end
        for i = 1:len-1
            tmp = counts[i]
            val = i
            toReturn[(i+1)] = quote
                $tmp = $stridenames[val]
            end
            tmp2 = counts[i+1]
            val = i+1
            toReturn[(i+1)+(len+1)] = quote
                 $tmp2 += $stridenames[val]
            end
        end
        toReturn
    end

    if is(permute_cache,nothing)
	permute_cache = Dict()
    end

    gen_cartesian_map(permute_cache, permute_one, ranges,
                      tuple(:A, :P, :perm, :offset, stridenames[1:ndimsA]...),
                      A, P, perm, offset, strides...)

    return P
end
end # let
