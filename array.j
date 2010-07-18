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
numel(a::Array) = arraylen(a)

jl_comprehension_zeros{T,n}(oneresult::Tensor{T,n}, dims...) = Array(T, dims...)
jl_comprehension_zeros{T}(oneresult::T, dims...) = Array(T, dims...)
jl_comprehension_zeros(oneresult::(), dims...) = Array(Bottom, dims...)

function zeros(T::Type, dims::Size...)
    a = Array(T, dims...)
    z = convert(T,0)
    for i=1:numel(a); a[i] = z; end
    return a
end

zeros(dims::Size...) = zeros(Float64, dims...)
zeros(T::Type, dims::Tuple) = zeros (T, dims...)
zeros(dims::Tuple) = zeros(dims...)

function ones(T::Type, dims::Size...)
    a = Array(T, dims...)
    o = convert(T,1)
    for i=1:numel(a); a[i] = o; end
    return a
end

ones(dims::Size...) = ones(Float64, dims...)
ones(T::Type, dims::Tuple) = ones (T, dims...)
ones(dims::Tuple) = ones(dims...)

function rand(dims::Size...)
    a = Array(Float64, dims...)
    for i=1:numel(a); a[i] = rand(); end
    return a
end

rand(dims::Tuple) = rand(dims...)

function randf(dims::Size...)
    a = Array(Float32, dims...)
    for i=1:numel(a); a[i] = randf(); end
    return a
end

randf(dims::Tuple) = randf(dims...)

function randn(dims::Size...)
    a = Array(Float64, dims...)
    for i=1:numel(a); a[i] = randn(); end
    return a
end

randn(dims::Tuple) = randn(dims...)

function copy{T}(a::Array{T})
    b = Array(T, size(a))
    for i=1:numel(a); b[i] = a[i]; end
    return b
end

copy(a::Array{Any,1}) = { copy(a[i]) | i=1:length(a) }

reshape{T}(a::Array{T}, dims...) = (b = zeros(T, dims...);
                                    for i=1:numel(a); b[i] = a[i]; end;
                                    b)
reshape(a::Array, dims::Tuple) = reshape(a, dims...)

eye(n::Size) = eye(n, n)
eye(m::Size, n::Size) = (a = zeros(m,n);
                         for i=1:min(m,n); a[i,i]=1; end;
                         a)

colon(start::Real, stop::Real, stride::Real) =
    ((start, stop, stride) = promote(start, stop, stride);
     [ i | i=start:stride:stop ])

(-)(x::Vector) = [ -x[i] | i=1:length(x) ]
(-)(x::Matrix) = [ -x[i,j] | i=1:size(x,1), j=1:size(x,2) ]

(+)(x::Scalar, y::Vector) = [ x + y[i] | i=1:length(y) ]
(-)(x::Scalar, y::Vector) = [ x - y[i] | i=1:length(y) ]
(*)(x::Scalar, y::Vector) = [ x * y[i] | i=1:length(y) ]
(/)(x::Scalar, y::Vector) = [ x / y[i] | i=1:length(y) ]
(<)(x::Scalar, y::Vector) = [ x < y[i] | i=1:length(y) ]
(==)(x::Scalar, y::Vector) = [ x == y[i] | i=1:length(y) ]

(+)(x::Vector, y::Scalar) = [ x[i] + y | i=1:length(x) ]
(-)(x::Vector, y::Scalar) = [ x[i] - y | i=1:length(x) ]
(*)(x::Vector, y::Scalar) = [ x[i] * y | i=1:length(x) ]
(/)(x::Vector, y::Scalar) = [ x[i] / y | i=1:length(x) ]
(<)(x::Vector, y::Scalar) = [ x[i] < y | i=1:length(x) ]
(==)(x::Vector, y::Scalar) = [ x[i] == y | i=1:length(x) ]

(+)(x::Vector, y::Vector) = [ x[i] + y[i] | i=1:length(x) ]
(-)(x::Vector, y::Vector) = [ x[i] - y[i] | i=1:length(x) ]
(.*)(x::Vector, y::Vector) = [ x[i] * y[i] | i=1:length(x) ]
(./)(x::Vector, y::Vector) = [ x[i] / y[i] | i=1:length(x) ]
(<)(x::Vector, y::Vector) = [ x[i] < y[i] | i=1:length(x) ]
(==)(x::Vector, y::Vector) = [ x[i] == y[i] | i=1:length(x) ]

(+)(x::Scalar, y::Matrix) = [ x + y[i,j] | i=1:size(y,1), j=1:size(y,2) ]
(-)(x::Scalar, y::Matrix) = [ x - y[i,j] | i=1:size(y,1), j=1:size(y,2) ]
(*)(x::Scalar, y::Matrix) = [ x * y[i,j] | i=1:size(y,1), j=1:size(y,2) ]
(/)(x::Scalar, y::Matrix) = [ x / y[i,j] | i=1:size(y,1), j=1:size(y,2) ]
(<)(x::Scalar, y::Matrix) = [ x < y[i,j] | i=1:size(y,1), j=1:size(y,2) ]
(==)(x::Scalar, y::Matrix) = [ x == y[i,j] | i=1:size(y,1), j=1:size(y,2) ]

(+)(x::Matrix, y::Scalar) = [ x[i,j] + y | i=1:size(x,1), j=1:size(x,2) ]
(-)(x::Matrix, y::Scalar) = [ x[i,j] - y | i=1:size(x,1), j=1:size(x,2) ]
(*)(x::Matrix, y::Scalar) = [ x[i,j] * y | i=1:size(x,1), j=1:size(x,2) ]
(/)(x::Matrix, y::Scalar) = [ x[i,j] / y | i=1:size(x,1), j=1:size(x,2) ]
(<)(x::Matrix, y::Scalar) = [ x[i,j] < y | i=1:size(x,1), j=1:size(x,2) ]
(==)(x::Matrix, y::Scalar) = [ x[i,j] == y | i=1:size(x,1), j=1:size(x,2) ]

(+)(x::Matrix, y::Matrix) = [ x[i,j] + y[i,j] | i=1:size(x,1), j=1:size(x,2) ]
(-)(x::Matrix, y::Matrix) = [ x[i,j] - y[i,j] | i=1:size(x,1), j=1:size(x,2) ]
(.*)(x::Matrix, y::Matrix) = [ x[i,j] * y[i,j] | i=1:size(x,1), j=1:size(x,2) ]
(./)(x::Matrix, y::Matrix) = [ x[i,j] / y[i,j] | i=1:size(x,1), j=1:size(x,2) ]
(<)(x::Matrix, y::Matrix) = [ x[i,j] < y[i,j] | i=1:size(x,1), j=1:size(x,2) ]
(==)(x::Matrix, y::Matrix) = [ x[i,j] == y[i,j] | i=1:size(x,1), j=1:size(x,2) ]

(>) {T,S,n}(x::Tensor{T,n}, y::Tensor{S,n}) = (y < x)
(>)        (x::Real  ,      y::Tensor)      = (y < x)
(>)        (x::Tensor,      y::Real  )      = (y < x)
(<=){T,S,n}(x::Tensor{T,n}, y::Tensor{S,n}) = (x < y) | (x == y)
(<=)       (x::Real  ,      y::Tensor)      = (x < y) | (x == y)
(<=)       (x::Tensor,      y::Real  )      = (x < y) | (x == y)
(>=){T,S,n}(x::Tensor{T,n}, y::Tensor{S,n}) = (x > y) | (x == y)
(>=)       (x::Real  ,      y::Tensor)      = (x > y) | (x == y)
(>=)       (x::Tensor,      y::Real  )      = (x > y) | (x == y)

(~)(x::Vector{Bool}) = [ ~x[i] | i=1:length(x) ]
(~)(x::Matrix{Bool}) = [ ~x[i,j] | i=1:size(x,1), j=1:=size(x,2) ]

(&)(x::Bool, y::Vector{Bool}) = [  x & y[i]  | i=1:length(y) ]
(|)(x::Bool, y::Vector{Bool}) = [ (x | y[i]) | i=1:length(y) ]
($)(x::Bool, y::Vector{Bool}) = [  x $ y[i]  | i=1:length(y) ]

(&)(x::Vector{Bool}, y::Bool) = [  x[i] & y  | i=1:length(x) ]
(|)(x::Vector{Bool}, y::Bool) = [ (x[i] | y) | i=1:length(x) ]
($)(x::Vector{Bool}, y::Bool) = [  x[i] $ y  | i=1:length(x) ]

(&)(x::Vector{Bool}, y::Vector{Bool}) = [  x[i] & y[i]  | i=1:length(x) ]
(|)(x::Vector{Bool}, y::Vector{Bool}) = [ (x[i] | y[i]) | i=1:length(x) ]
($)(x::Vector{Bool}, y::Vector{Bool}) = [  x[i] $ y[i]  | i=1:length(x) ]

function sum(x::Matrix, dim::Number)
    if dim == 1
        [ sum(x[:,i]) | i=1:size(x, 2) ]
    elseif dim == 2
        [ sum(x[i,:]) | i=1:size(x, 1) ]
    end
end

function (==)(x::Array, y::Array)
    if x.dims != y.dims; return false; end
    for i=1:numel(x); if arrayref(x,i) != arrayref(y,i); return false; end; end
    return true
end

transpose(x::Matrix) = [ x[j,i] | i=1:size(x,2), j=1:size(x,1) ]
ctranspose(x::Matrix) = [ conj(x[j,i]) | i=1:size(x,2), j=1:size(x,1) ]

diag(A::Matrix) = [ A[i,i] | i=1:min(size(A)) ]
diagm(v::Vector) = (n=length(v);
                    a=zeros(n,n);
                    for i=1:n; a[i,i] = v[i]; end;
                    a)

dot(x::Vector, y::Vector) = sum(x.*y)

trace(A::Matrix) = sum(diag(A))

mean(V::Vector) = sum(V) / length(V)

std(V::Vector) = (m = mean(V);
                  sqrt( sum([ (V[i] - m)^2 | i=1:length(V) ]) / length(V) ))

## blas.j definse these for floats
## This should be commented out for supporting the int cases
#(*)(A::Matrix, B::Vector) = [ dot(A[i,:],B) | i=1:size(A,1) ]
#(*)(A::Matrix, B::Matrix) = [ dot(A[i,:],B[:,j]) | i=1:size(A,1), j=1:size(B,2) ]

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
# disambiguating definitions
ref{T}(a::Array{T,1}, i::Index) = arrayref(a,i)
ref(a::Array{Any,1}, i::Index) = arrayref(a,i)

ref{T}(a::Array{T,2}, i::Index, j::Index) = arrayref(a, (j-1)*a.dims[1] + i)

jl_fill_endpts(A,n,R::RangeBy)   = Range(1,R.step,size(A,n))
jl_fill_endpts(A,n,R::RangeFrom) = Range(R.start,R.step,size(A,n))
jl_fill_endpts(A,n,R::RangeTo)   = Range(1,R.step,R.stop)
jl_fill_endpts(A,n,R)            = R

ref(A::Vector,I) = [ A[i] | i = jl_fill_endpts(A,1,I) ]
ref(A::Array{Any,1},I) = { A[i] | i = jl_fill_endpts(A,1,I) }
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

# Indexing: assign()
# TODO: Take care of growing
assign(a::Array, x, i::Index) = arrayset(a,i,x)
assign{T}(a::Array{T,2}, x, i::Index, j::Index) =
    arrayset(a, (j-1)*a.dims[1]+i, x)

function assign(a::Array, x, I::Index...)
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

function assign(A::Vector, x::Scalar, I)
    I = jl_fill_endpts(A, 1, I)
    for i=I; A[i] = x; end;
    return A
end

function assign(A::Vector, X, I)
    I = jl_fill_endpts(A, 1, I)
    count = 1
    for i=I; A[i] = X[count]; count += 1; end
    return A
end

function assign(A::Matrix, x::Scalar, I, J)
    I = jl_fill_endpts(A, 1, I)
    J = jl_fill_endpts(A, 2, J)
    for i=I, j=J; A[i,j] = x; end
    return A
end

function assign(A::Matrix, X, I, J)
    I = jl_fill_endpts(A, 1, I)
    J = jl_fill_endpts(A, 2, J)
    count = 1
    for i=I, j=J; A[i,j] = X[count]; count += 1; end
    return A
end

# Concatenation
hcat() = Array(Bottom,0)
hcat{T}(X::Scalar{T}...) = [ X[i] | i=1:length(X) ]
vcat{T}(X::Scalar{T}...) = [ X[i] | i=1:length(X) ]

hcat{T}(V::Vector{T}...) = [ V[j][i] | i=1:length(V[1]), j=1:length(V) ]
vcat{T}(V::Vector{T}...) = [ V[i][j] | i=1:length(V), j=1:length(V[1]) ]

function hcat{T}(A::Matrix{T}...)
    ncols = sum([ size(A[i], 2) | i=1:length(A) ])
    nrows = size(A[1], 1)
    B = zeros(typeof(A[1][1]), nrows, ncols)

    pos = 1
    for k=1:length(A), i=1:numel(A[k])
        B[pos] = A[k][i]
        pos = pos + 1
    end
    
    return B
end

function vcat{T}(A::Matrix{T}...)
    nrows = sum([size(A[i], 1) | i=1:length(A)])
    ncols = size(A[1], 2)
    B = zeros(typeof(A[1][1]), nrows, ncols)

    pos = 1
    for j=1:ncols, k=1:length(A), i=1:size(A[k], 1)
        B[pos] = A[k][i,j]
        pos = pos + 1
    end
    
    return B
end

# iteration support for arrays as ranges

start(a::Array) = 1
next(a::Array,i) = (a[i],i+1)
done(a::Array,i) = (i > numel(a))

# Other functions

reverse(v::Vector) = [ v[length(v)-i+1] | i=1:length(v) ]

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

# Knuth shuffle
function shuffle(a::Vector)
    for i = length(a):-1:2
        j = randint(i)
        a[i], a[j] = a[j], a[i]
    end
    return a
end

function randperm(n::Int)
    a = Array(typeof(n), n)
    a[1] = 1
    for i = 2:n
        j = randint(i)
        a[i] = a[j]
        a[j] = i
    end
    return a
end

function randcycle(n::Int)
    a = Array(typeof(n), n)
    a[1] = 1
    for i = 2:n
        j = randint(i-1)
        a[i] = a[j]
        a[j] = i
    end
    return a
end

function permute(A, k::Int)
    fac = convert(typeof(k),1)
    for i=2:length(A)
        fac *= (i-1)
        j = i - div(k,fac)%i
        A[i], A[j] = A[j], A[i]
    end
    A
end

# Print arrays
function printall{T}(a::Array{T,1})
    if is(T,Any)
        opn = "{"; cls = "}"
    else
        opn = "["; cls = "]";
    end
    print_comma_array(a, opn, cls)
end

function printall{T}(a::Array{T,2})
    for i=1:size(a,1)
        printcols(a, 1, size(a,2), i)
        print("\n")
    end
end

function print{T}(a::Array{T,1})
    if is(T,Any)
        opn = "{"; cls = "}"
    else
        opn = "["; cls = "]";
    end
    n = a.dims[1]
    if n <= 10
        print_comma_array(a, opn, cls)
    else
        print_comma_array(a[1:5], opn, "")
        print(",...,")
        print_comma_array(a[(n-4):n], "", cls)
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

map(f, a::Array{Any,1}) = { f(a[i]) | i=1:length(a) }
map(f, a::Array{Any,1}, b::Array{Any,1}) =
    { f(a[i],b[i]) | i=1:min(length(a),length(b)) }

function ndmap(body, t::Tuple, it...)
    idx = length(t)-length(it)
    if idx == 0
        body(it)
    else
        for i = t[idx]
            ndmap(body, t, i, it...)
        end
    end
end

print{T}(a::Array{T,0}) = print("Array(",T,")")

function print(a::Array)
    slice2d(a, idxs) = [ a[i, j, idxs...] | i=1:size(a,1), j=1:size(a,2) ]
    
    tail = size(a)[3:]
    
    ndmap(idxs->(print("[:, :, ");
                 for i=1:(length(idxs)-1); print(idxs[i],", "); end;
                 print(idxs[length(idxs)], "] =\n");
                 print(slice2d(a, idxs), idxs==tail?"":"\n\n")),
          map(x->Range(1,1,x), tail))
end
