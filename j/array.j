## array.j: Dense arrays

typealias Vector{T} Array{T,1}
typealias Matrix{T} Array{T,2}
typealias VecOrMat{T} Union(Vector{T}, Matrix{T})

typealias StridedArray{T,N}  Union(Array{T,N}, SubArray{T,N,Array{T}})
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

function reshape{T,N}(a::Array{T}, dims::NTuple{N,Long})
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

similar(a::Array, T, dims::Dims)      = Array(T, dims)
similar{T}(a::Array{T,1})             = Array(T, size(a,1))
similar{T}(a::Array{T,2})             = Array(T, size(a,1), size(a,2))
similar{T}(a::Array{T,1}, dims::Dims) = Array(T, dims)
similar{T}(a::Array{T,1}, m::Long)    = Array(T, m)
similar{T}(a::Array{T,1}, S)          = Array(S, size(a,1))
similar{T}(a::Array{T,2}, dims::Dims) = Array(T, dims)
similar{T}(a::Array{T,2}, m::Long)    = Array(T, m)
similar{T}(a::Array{T,2}, S)          = Array(S, size(a,1), size(a,2))

empty(T) = Array(T, 0)

function fill!{T<:Union(Int8,Uint8)}(a::Array{T}, x::Int)
    ccall(:memset, Void, (Ptr{T}, Int32, Long), a, int32(x), long(length(a)))
    return a
end
function fill!{T<:Union(Int,Float)}(a::Array{T}, x)
    if convert(T,x) == zero(T)
        ccall(:bzero, Void, (Ptr{T}, Long), a, long(length(a)*sizeof(T)))
    else
        for i = 1:numel(a)
            a[i] = x
        end
    end
    return a
end

zeros{T}(::Type{T}, dims::Dims) = fill!(Array(T, dims), zero(T))
zeros(T::Type, dims::Long...) = zeros(T, dims)
zeros(dims::Dims) = zeros(Float64, dims)
zeros(dims::Long...) = zeros(dims)

ones{T}(::Type{T}, dims::Dims) = fill!(Array(T, dims), one(T))
ones(T::Type, dims::Long...) = ones(T, dims)
ones(dims::Dims) = ones(Float64, dims)
ones(dims::Long...) = ones(dims)

trues(dims::Dims) = fill!(Array(Bool, dims), true)
trues(dims::Long...) = trues(dims)

falses(dims::Dims) = fill!(Array(Bool, dims), false)
falses(dims::Long...) = falses(dims)

fill(v, dims::Dims) = fill!(Array(typeof(v), dims), v)
fill(v, dims::Long...) = fill(v, dims)

eye(n::Long) = eye(n, n)
function eye(m::Long, n::Long)
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

## Indexing: ref ##

ref(a::Array, i::Long) = arrayref(a,i)
ref(a::Array, i::Int) = arrayref(a,long(i))
ref{T}(a::Array{T,0}) = arrayref(a,1)
ref{T}(a::Array{T,1}, i::Long) = arrayref(a,i)
ref{T}(a::Array{T,1}, i::Int) = arrayref(a,long(i))
ref(a::Array{Any,1}, i::Long) = arrayref(a,i)
ref(a::Array{Any,1}, i::Int) = arrayref(a,long(i))
ref{T}(a::Array{T,2}, i::Long, j::Long) = arrayref(a, (j-1)*arraysize(a,1)+i)
ref{T}(a::Array{T,2}, i::Int, j::Int) = arrayref(a,long((j-1)*arraysize(a,1)+i))

## Indexing: assign ##

assign(A::Array{Any}, x::AbstractArray, i::Long) = arrayset(A,i,x)
assign(A::Array{Any}, x::AbstractArray, i::Int) = arrayset(A,long(i),x)
assign(A::Array{Any}, x::ANY, i::Long) = arrayset(A,i,x)
assign(A::Array{Any}, x::ANY, i::Int) = arrayset(A,long(i),x)
assign{T}(A::Array{T}, x::AbstractArray, i::Long) = arrayset(A,i,convert(T, x))
assign{T}(A::Array{T}, x::AbstractArray, i::Int) = arrayset(A,long(i),convert(T, x))
assign{T}(A::Array{T}, x, i::Long) = arrayset(A,i,convert(T, x))
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

## Transpose ##

function transpose{T<:Union(Float64,Float32,Complex128,Complex64)}(A::Matrix{T})
    if numel(A) > 50000
        return _jl_fftw_transpose(A)
    else
        return [ A[j,i] | i=1:size(A,2), j=1:size(A,1) ]
    end
end
ctranspose{T<:Union(Float64,Float32)}(A::Matrix{T}) = transpose(A)

ctranspose(x::StridedVector) = transpose(x)
ctranspose(x::StridedMatrix) = transpose(x)

transpose(x::StridedVector) = [ x[j] | i=1, j=1:size(x,1) ]
transpose(x::StridedMatrix) = [ x[j,i] | i=1:size(x,2), j=1:size(x,1) ]

ctranspose{T<:Number}(x::StridedVector{T}) = [ conj(x[j]) | i=1, j=1:size(x,1) ]
ctranspose{T<:Number}(x::StridedMatrix{T}) = [ conj(x[j,i]) | i=1:size(x,2), j=1:size(x,1) ]

## Unary operators ##

function conj!{T<:Number}(A::StridedArray{T})
    for i=1:numel(A)
        A[i] = conj(A[i])
    end
    return A
end

macro unary_op(f)
    quote
        function ($f)(A::StridedArray)
            F = similar(A)
            for i=1:numel(A)
                F[i] = ($f)(A[i])
            end
            return F
        end
    end
end

@unary_op (-)
@unary_op (~)
@unary_op (conj)

macro unary_c2r_op(f)
    quote
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

@unary_c2r_op (real)
@unary_c2r_op (imag)

function !(A::StridedArray{Bool})
    F = similar(A)
    for i=1:numel(A)
        F[i] = !A[i]
    end
    return F
end

## Binary arithmetic operators ##

./(x::Array, y::Array ) = reshape( [ x[i] ./ y[i] | i=1:numel(x) ], size(x) )
./(x::Number,y::Array ) = reshape( [ x    ./ y[i] | i=1:numel(y) ], size(y) )
./(x::Array, y::Number) = reshape( [ x[i] ./ y    | i=1:numel(x) ], size(x) )

.^(x::Array, y::Array ) = reshape( [ x[i] ^ y[i] | i=1:numel(x) ], size(x) )
.^(x::Number,y::Array ) = reshape( [ x    ^ y[i] | i=1:numel(y) ], size(y) )
.^(x::Array, y::Number) = reshape( [ x[i] ^ y    | i=1:numel(x) ], size(x) )

function .^{S<:Int,T<:Int}(A::Array{S}, B::Array{T})
    if size(A) != size(B); error("argument dimensions must match"); end
    F = similar(A, Float64)
    for i=1:numel(A)
        F[i] = A[i]^B[i]
    end
    return F
end

function .^{T<:Int}(A::Int, B::Array{T})
    F = similar(B, Float64)
    for i=1:numel(B)
        F[i] = A^B[i]
    end
    return F
end

function _jl_power_array_int_body(F, A, B)
    for i=1:numel(A)
        F[i] = A[i]^B
    end
    return F
end

function .^{T<:Int}(A::Array{T}, B::Int)
    F = similar(A, B < 0 ? Float64 : promote_type(T,typeof(B)))
    _jl_power_array_int_body(F, A, B)
end

macro binary_arithmetic_op(f)
    quote
        function ($f){S,T}(A::Array{S}, B::Array{T})
            if size(A) != size(B); error("argument dimensions must match"); end
            F = similar(A, promote_type(S,T))
            for i=1:numel(A)
                F[i] = ($f)(A[i], B[i])
            end
            return F
        end
        function ($f){T}(A::Number, B::Array{T})
            F = similar(B, promote_type(typeof(A),T))
            for i=1:numel(B)
                F[i] = ($f)(A, B[i])
            end
            return F
        end
        function ($f){T}(A::Array{T}, B::Number)
            F = similar(A, promote_type(T,typeof(B)))
            for i=1:numel(A)
                F[i] = ($f)(A[i], B)
            end
            return F
        end
    end
end

@binary_arithmetic_op (+)
@binary_arithmetic_op (-)
@binary_arithmetic_op (.*)
@binary_arithmetic_op div
@binary_arithmetic_op mod
@binary_arithmetic_op (&)
@binary_arithmetic_op (|)
@binary_arithmetic_op ($)

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

macro binary_comparison_op(f)
    quote
        function ($f)(A::Array, B::Array)
            if size(A) != size(B); error("argument dimensions must match"); end
            F = similar(A, Bool)
            for i = 1:numel(A)
                F[i] = ($f)(A[i], B[i])
            end
            return F
        end
        function ($f)(A::Number, B::Array)
            F = similar(B, Bool)
            for i = 1:numel(B)
                F[i] = ($f)(A, B[i])
            end
            return F
        end
        function ($f)(A::Array, B::Number)
            F = similar(A, Bool)
            for i = 1:numel(A)
                F[i] = ($f)(A[i], B)
            end
            return F
        end
    end
end

@binary_comparison_op (==)
@binary_comparison_op (!=)
@binary_comparison_op (<)
@binary_comparison_op (<=)

## data movement ##

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
function rotl90(A::StridedMatrix, k::Int)
    k = k % 4
    k == 1 ? rotl90(A) :
    k == 2 ? rot180(A) :
    k == 3 ? rotr90(A) : copy(A)
end
rotr90(A::AbstractMatrix, k::Int) = rotl90(A,-k)
rot180(A::AbstractMatrix, k::Int) = k % 2 == 1 ? rot180(A) : copy(A)
const rot90 = rotl90

reverse(v::StridedVector) = (n=length(v); [ v[n-i+1] | i=1:n ])
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

function find{T}(A::StridedArray{T})
    nnzA = nnz(A)
    I = Array(Long, nnzA)
    z = zero(T)
    count = 1
    for i=1:length(A)
        if A[i] != z
            I[count] = i
            count += 1
        end
    end
    return I
end

findn(A::StridedVector) = find(A)

function findn{T}(A::StridedMatrix{T})
    nnzA = nnz(A)
    I = Array(Long, nnzA)
    J = Array(Long, nnzA)
    z = zero(T)
    count = 1
    for j=1:size(A,2), i=1:size(A,1)
        if A[i,j] != z
            I[count] = i
            J[count] = j
            count += 1
        end
    end
    return (I, J)
end

let findn_cache = nothing
function findn_one(ivars)
    s = { quote I[$i][count] = $ivars[i] end | i = 1:length(ivars)}
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
    I = ntuple(ndimsA, x->Array(Long, nnzA))
    ranges = ntuple(ndims(A), d->(1:size(A,d)))

    if is(findn_cache,nothing)
        findn_cache = HashTable()
    end

    gen_cartesian_map(findn_cache, findn_one, ranges,
                      (:A, :I, :count, :z), A,I,1, zero(T))
    return I
end
end

function findn_nzs{T}(A::StridedMatrix{T})
    nnzA = nnz(A)
    I = zeros(Long, nnzA)
    J = zeros(Long, nnzA)
    NZs = zeros(T, nnzA)
    z = zero(T)
    count = 1
    for j=1:size(A,2), i=1:size(A,1)
        if A[i,j] != z
            I[count] = i
            J[count] = j
            NZs[count] = A[i,j]
            count += 1
        end
    end
    return (I, J, NZs)
end

function nonzeros{T}(A::StridedArray{T})
    nnzA = nnz(A)
    V = Array(T, nnzA)
    z = zero(T)
    count = 1
    for i=1:length(A)
        Ai = A[i]
        if Ai != z
            V[count] = Ai
            count += 1
        end
    end
    return V
end

## hist ##

function hist(v::StridedVector, nbins::Int)
    h = zeros(Long, nbins)
    if nbins == 0
        return h
    end
    lo, hi = min(v), max(v)
    if lo == hi
        lo = lo - div(nbins,2)
        hi = hi + div(nbins,2)
    end
    binsz = (hi-lo)/nbins
    for x = v
        if isfinite(x)
            i = iround((x-lo+binsz/2)/binsz)
            h[i > nbins ? nbins : i] += 1
        end
    end
    h
end

hist(x) = hist(x, 10)

function hist(A::StridedMatrix, nbins::Int)
    m, n = size(A)
    h = Array(Long, nbins, n)
    for j=1:n
        i = 1+(j-1)*m
        h[:,j] = hist(sub(A, i:(i+m-1)), nbins)
    end
    h
end
