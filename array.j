typealias Vector{T} Tensor{T,1}
typealias Matrix{T} Tensor{T,2}
typealias Indices{T} Union(Range, RangeFrom, RangeBy, RangeTo, Vector{T})

# Basic functions
size(a::Array) = a.dims

size(t::Tensor, d) = size(t)[d]
ndims(t::Tensor) = length(size(t))
numel(t::Tensor) = prod(size(t))
length(v::Vector) = size(v,1)
nnz(a::Array) = (n = 0; for i=1:numel(a); n += a[i] != 0 ? 1 : 0; end; n)

jl_comprehension_zeros{T,n}(oneresult::Tensor{T,n}, dims...) = Array(T, dims...)
jl_comprehension_zeros{T,n}(oneresult::Tensor{T,n}, dims::Tuple) = Array(T, dims...)

zeros(T::Type, m::Size) = (z=convert(T,0); [ z | i=1:m ])
zeros(T::Type, m::Size, n::Size) = (z=convert(T,0); [ z | i=1:m, j=1:n ])
zeros(m::Size) = [ 0.0 | i=1:m ]
zeros(m::Size, n::Size) = [ 0.0 | i=1:m, j=1:n ]
zeros(T::Type, dims::Tuple) = zeros (T, dims...)
zeros(dims::Tuple) = zeros(dims...)

ones(T::Type, m::Size) = (o=convert(T,1); [ o | i=1:m ])
ones(T::Type, m::Size, n::Size) = (o=convert(T,1); [ o | i=1:m, j=1:n ])
ones(m::Size) = [ 1.0 | i=1:m ]
ones(m::Size, n::Size) = [ 1.0 | i=1:m, j=1:n ]
ones(T::Type, dims::Tuple) = ones (T, dims...)
ones(dims::Tuple) = ones(dims...)

rand(m::Size) = [ rand() | i=1:m ]
rand(m::Size, n::Size) = [ rand() | i=1:m, j=1:n ]
rand(dims::Tuple) = rand(dims...)

randf(m::Size) = [ randf() | i=1:m ]
randf(m::Size, n::Size) = [ randf() | i=1:m, j=1:n ]
randf(dims::Tuple) = randf(dims...)

eye(n::Size) = diagm(ones(n))

colon(start::Size, stop::Size, stride::Size) = [ i | i=start:stride:stop ]

copy(a::Vector) = [ a[i] | i=1:length(a) ]
copy(a::Matrix) = [ a[i,j] | i=1:size(a,1), j=1:size(a,2) ]

reshape{T,n}(a::Array{T,n}, dims...) = (b = zeros(T, dims...);
                                        for i=1:numel(a); b[i] = a[i]; end;
                                        b)
reshape{T,n}(a::Array{T,n}, dims::Tuple) = reshape(a, dims...)

function issymmetric (A::Matrix)
    m = size(A, 1)
    n = size(A, 2)
    if m != n; error("Input matrix must be square"); end
    for i=1:n
        for j=1:n
            if A[i,j] != A[j,i]; return false; end
        end
    end

    return true
end

(+)(x::Scalar, y::Vector) = [ x + y[i] | i=1:length(y) ]
(-)(x::Scalar, y::Vector) = [ x - y[i] | i=1:length(y) ]
(*)(x::Scalar, y::Vector) = [ x * y[i] | i=1:length(y) ]
(/)(x::Scalar, y::Vector) = [ x / y[i] | i=1:length(y) ]

(+)(x::Vector, y::Scalar) = [ x[i] + y | i=1:length(x) ]
(-)(x::Vector, y::Scalar) = [ x[i] - y | i=1:length(x) ]
(*)(x::Vector, y::Scalar) = [ x[i] * y | i=1:length(x) ]
(/)(x::Vector, y::Scalar) = [ x[i] / y | i=1:length(x) ]

(+)(x::Vector, y::Vector) = [ x[i] + y[i] | i=1:length(x) ]
(-)(x::Vector, y::Vector) = [ x[i] - y[i] | i=1:length(x) ]
(.*)(x::Vector, y::Vector) = [ x[i] * y[i] | i=1:length(x) ]
(./)(x::Vector, y::Vector) = [ x[i] / y[i] | i=1:length(x) ]

(+)(x::Scalar, y::Matrix) = [ x + y[i,j] | i=1:size(y,1), j=1:size(y,2) ]
(-)(x::Scalar, y::Matrix) = [ x - y[i,j] | i=1:size(y,1), j=1:size(y,2) ]
(*)(x::Scalar, y::Matrix) = [ x * y[i,j] | i=1:size(y,1), j=1:size(y,2) ]
(/)(x::Scalar, y::Matrix) = [ x / y[i,j] | i=1:size(y,1), j=1:size(y,2) ]

(+)(x::Matrix, y::Scalar) = [ x[i,j] + y | i=1:size(x,1), j=1:size(x,2) ]
(-)(x::Matrix, y::Scalar) = [ x[i,j] - y | i=1:size(x,1), j=1:size(x,2) ]
(*)(x::Matrix, y::Scalar) = [ x[i,j] * y | i=1:size(x,1), j=1:size(x,2) ]
(/)(x::Matrix, y::Scalar) = [ x[i,j] / y | i=1:size(x,1), j=1:size(x,2) ]

(+)(x::Matrix, y::Matrix) = [ x[i,j] + y[i,j] | i=1:size(x,1), j=1:size(x,2) ]
(-)(x::Matrix, y::Matrix) = [ x[i,j] - y[i,j] | i=1:size(x,1), j=1:size(x,2) ]
(.*)(x::Matrix, y::Matrix) = [ x[i,j] * y[i,j] | i=1:size(x,1), j=1:size(x,2) ]
(./)(x::Matrix, y::Matrix) = [ x[i,j] / y[i,j] | i=1:size(x,1), j=1:size(x,2) ]

function (==)(x::Array, y::Array)
    if x.dims != y.dims
        return false
    end
    for i=1:numel(x)
        if arrayref(x,i) != arrayref(y,i)
            return false
        end
    end
    return true
end

transpose(x::Matrix) = [ x[j,i] | i=1:size(x,2), j=1:size(x,1) ]
ctranspose(x::Matrix) = [ conj(x[j,i]) | i=1:size(x,2), j=1:size(x,1) ]

dot(x::Vector, y::Vector) = sum(x.*y)
diag(A::Matrix) = [ A[i,i] | i=1:min(size(A)) ]
diagm(v::Vector) = (n=length(v);
                    a=zeros(n,n);
                    for i=1:n; a[i,i] = v[i]; end;
                    a)

(*)(A::Matrix, B::Vector) = [ dot(A[i,:],B) | i=1:size(A,1) ]
(*)(A::Matrix, B::Matrix) = [ dot(A[i,:],B[:,j]) | i=1:size(A,1), j=1:size(B,2) ]

kron(a::Matrix, b::Matrix) = reshape([ a[i,j]*b[k,l] | k=1:size(b,1),
                                                       i=1:size(a,1),
                                                       l=1:size(b,2),
                                                       j=1:size(a,2)],
                                     size(a,1)*size(b,1),
                                     size(a,2)*size(b,2))

repmat(a::Matrix, m::Size, n::Size) = reshape([ a[i,j] | i=1:size(a,1),
                                                         k=1:m,
                                                         j=1:size(a,2),
                                                         l=1:n],
                                              size(a,1)*m,
                                              size(a,2)*n)

triu(M) = triu(M,0)
tril(M) = tril(M,0)
triu(M::Matrix, k) = [ j-i >= k ? M[i,j] : convert(typeof(M[i,j]),0) |
                       i=1:size(M,1), j=1:size(M,2) ]
tril(M::Matrix, k) = [ j-i <= k ? M[i,j] : convert(typeof(M[i,j]),0) |
                       i=1:size(M,1), j=1:size(M,2) ]
## Indexing: ref()
#TODO: Out-of-bound checks
ref(a::Array, i::Index) = arrayref(a,i)
ref{T}(a::Array{T,2}, i::Index, j::Index) = arrayref(a, (j-1)*a.dims[1] + i)

jl_fill_endpts(A,n,R::RangeBy)   = Range(1,R.step,size(A,n))
jl_fill_endpts(A,n,R::RangeFrom) = Range(R.start,R.step,size(A,n))
jl_fill_endpts(A,n,R::RangeTo)   = Range(1,R.step,R.stop)
jl_fill_endpts(A,n,R)            = R

ref(A::Vector,I) = [ A[i] | i = jl_fill_endpts(A,1,I) ]
ref(A::Matrix,I,J) = [ A[i,j] | i = jl_fill_endpts(A,1,I),
                                j = jl_fill_endpts(A,2,J) ]

ref(A::Matrix,i::Index,J) = [ A[i,j] | j = jl_fill_endpts(A,2,J) ]
ref(A::Matrix,I,j::Index) = [ A[i,j] | i = jl_fill_endpts(A,1,I) ]

function ref(a::Array, I::Index...) 
    dims = a.dims
    ndims = length(I) - 1

    index = I[1]
    stride = 1
    for k=2:ndims
        stride = stride * dims[k-1]
        index += (I[k]-1) * stride
    end

    return arrayref(a,index)
end

# Indexing: set()
# TODO: Take care of growing
set(a::Array, x, i::Index) = (arrayset(a,i,x); a)
set{T}(a::Array{T,2}, x, i::Index, j::Index) =
    (arrayset(a, (j-1)*a.dims[1]+i, x); a)

function set(a::Array, x, I::Index...)
    # TODO: Need to take care of growing
    dims = a.dims
    ndims = length(I)

    index = I[1]
    stride = 1
    for k=2:ndims
        stride = stride * dims[k-1]
        index += (I[k]-1) * stride
    end

    a[index] = x
    return a
end

function set(A::Vector, x::Scalar, I)
    I = jl_fill_endpts(A, 1, I)
    for i=I; A[i] = x; end;
    return A
end

function set(A::Vector, X, I)
    I = jl_fill_endpts(A, 1, I)
    count = 1
    for i=I; A[i] = X[count]; count += 1; end
    return A
end

function set(A::Matrix, x::Scalar, I, J)
    I = jl_fill_endpts(A, 1, I)
    J = jl_fill_endpts(A, 2, J)
    for i=I; for j=J; A[i,j] = x; end; end
    return A
end

function set(A::Matrix, X, I, J)
    I = jl_fill_endpts(A, 1, I)
    J = jl_fill_endpts(A, 2, J)
    count = 1
    for i=I; for j=J; A[i,j] = X[count]; count += 1; end; end    
    return A
end

# Concatenation
hcat() = Array(Bottom,0)
hcat{T}(X::Scalar{T}...) = [ X[i] | i=1:length(X) ]
vcat{T}(X::Scalar{T}...) = [ X[i] | i=1:length(X) ]
vcat{T}(V::Vector{T}...) = [ V[i][j] | i=1:length(V), j=1:length(V[1]) ]
hcat{T}(V::Vector{T}...) = [ V[j][i] | i=1:length(V[1]), j=1:length(V) ]

start(a::Array) = 1
next(a::Array,i) = (a[i],i+1)
done(a::Array,i) = (i > numel(a))

# Other functions
diff(a::Vector) = [ a[i+1] - a[i] | i=1:length(a)-1 ]
diff(a::Matrix) = diff(a, 1)

function diff(a::Matrix, dim) 
    if dim == 1
        [ a[i+1,j] - a[i,j] | i=1:size(a,1)-1, j=1:size(a,2) ]
    else
        [ a[i,j+1] - a[i,j] | i=1:size(a,1), j=1:size(a,2)-1 ]
    end
end

# Sort
sort(a::Vector) = sort(a, 1, length(a))

function sort(a::Vector, lo, hi)
    i, j = lo, hi
    pivot = a[div((lo+hi),2)];
    # Partition
    while i <= j
        while a[i] < pivot; i += 1; end
        while a[j] > pivot; j -= 1; end
        if i <= j
            a[i], a[j] = a[j], a[i]
            i += 1;
            j -= 1;
        end
    end
    # Recursion for quicksort
    if lo < j; sort(a, lo, j); end
    if i < hi; sort(a, i, hi); end
    return a
end

# Print arrays
function printall{T}(a::Array{T,1})
    n = a.dims[1]
    for i = 1:n; print(a[i]); if i < n; print("\n"); end; end
end

function print{T}(a::Array{T,1})
    n = a.dims[1]
    if n < 10
        for i = 1:n; print(a[i]); if i < n; print("\n"); end; end
    else
        for i = 1:3; print(a[i]); print("\n"); end
        print(":\n");
        for i = n-2:n; print(a[i]); if i < n; print("\n"); end; end
    end
end

function printcols(a, start, stop, i)
    for j=start:stop; print(a[i,j]); print(" "); end
end

function print{T}(a::Array{T,2})
    m = a.dims[1]
    n = a.dims[2]
    print_hdots = false
    print_vdots = false
    if 10 < m; print_vdots = true; end
    if 10 < n; print_hdots = true; end
    if !print_vdots && !print_hdots
        for i=1:m
            printcols(a, 1, n, i)
            if i<m; print("\n"); end
        end
    elseif print_vdots && !print_hdots
        for i=1:3
            printcols(a, 1, n, i)
            print("\n")
        end
        print(":\n")
        for i=m-2:m
            printcols(a, 1, n, i)
            if i<m; print("\n"); end
        end
    elseif !print_vdots && print_hdots
        for i=1:m
            printcols (a, 1, 3, i)
            if i == 1 || i == m; print(": "); else; print("  "); end
            printcols (a, n-2, n, i)
            if i<m; print("\n"); end
        end
    else
        for i=1:3
            printcols (a, 1, 3, i)
            if i == 1; print(": "); else; print("  "); end
            printcols (a, n-2, n, i)
            print("\n")
        end
        print(":\n")
        for i=m-2:m
            printcols (a, 1, 3, i)
            if i == m; print(": "); else; print("  "); end
            printcols (a, n-2, n, i)
            if i<m; print("\n"); end
        end
    end
end # print()
