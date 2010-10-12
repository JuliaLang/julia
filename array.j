typealias Vector{T} Tensor{T,1}
typealias Matrix{T} Tensor{T,2}
typealias Indices{T} Union(Range,Range1,RangeFrom,RangeBy,RangeTo,Vector{T})
typealias Indices2{T} Union(Index,Range,Range1,RangeFrom,RangeBy,RangeTo,Vector{T})

# Basic functions
size(a::Array) = a.dims

size(t::Tensor, d) = size(t)[d]
ndims(t::Tensor) = length(size(t))
numel(t::Tensor) = prod(size(t))
length(v::Vector) = numel(v)
nnz(a::Array) = (n = 0; for i=1:numel(a); n += a[i] != 0 ? 1 : 0; end; n)
numel(a::Array) = arraylen(a)

reshape{T}(a::Array{T}, dims...) = (b = Array(T, dims...);
                                    for i=1:numel(a); b[i] = a[i]; end;
                                    b)
reshape(a::Array, dims::Tuple) = reshape(a, dims...)

# Constructors

jl_comprehension_zeros{T,n}(oneresult::Tensor{T,n}, dims...) = Array(T, dims...)
jl_comprehension_zeros{T}(oneresult::T, dims...) = Array(T, dims...)
jl_comprehension_zeros(oneresult::(), dims...) = Array(None, dims...)

function zeros(T::Type, dims::Size...)
    a = Array(T, dims...)
    z = zero(T)
    for i=1:numel(a); a[i] = z; end
    return a
end

zeros(dims::Size...) = zeros(Float64, dims...)
zeros(T::Type, dims::Tuple) = zeros(T, dims...)
zeros(dims::Tuple) = zeros(dims...)

function ones(T::Type, dims::Size...)
    a = Array(T, dims...)
    o = one(T)
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
    for i=1:numel(a); b[i] = copy(a[i]); end
    return b
end

eye(n::Size) = eye(n, n)
eye(m::Size, n::Size) = (a = zeros(m,n);
                         for i=1:min(m,n); a[i,i]=1; end;
                         a)
one{T}(x::Array{T,2}) = (m=size(x,1); n=size(x,2);
                         a = zeros(T,m,n);
                         for i=1:min(m,n); a[i,i]=1; end;
                         a)
zero{T}(x::Array{T,2}) = zeros(T,size(x))

colon(start::Real, stop::Real, stride::Real) =
    ((start, stop, stride) = promote(start, stop, stride);
     [ i | i=start:stride:stop ])

# Unary operators 

(-)(x::Array) = reshape([ -x[i] | i=1:numel(x) ], size(x) )

(!)(x::Array{Bool}) = reshape( [ !x[i] | i=1:numel(x) ], size(x) )

(~)(x::Array{Bool}) = reshape( [ ~x[i] | i=1:numel(x) ], size(x) )

conj{T <: Number}(x::Array{T}) = x
conj(x::Array) = reshape( [ conj(x[i]) | i=1:numel(x) ], size(x) )

real{T <: Number}(x::Array{T}) = x
real(x::Array) = reshape( [ real(x[i]) | i=1:numel(x) ], size(x) )

imag{T <: Number}(x::Array{T}) = zeros(T, size(x))
imag(x::Array) = reshape( [ imag(x[i]) | i=1:numel(x) ], size(x) )

# Binary arithmetic operators

(+)(x::Array, y::Array)  = reshape( [ x[i] + y[i] | i=1:numel(x) ], size(x) )
(+)(x::Number, y::Array) = reshape( [ x    + y[i] | i=1:numel(y) ], size(y) )
(+)(x::Array, y::Number) = reshape( [ x[i] + y    | i=1:numel(x) ], size(x) )

(-)(x::Array, y::Array)  = reshape( [ x[i] - y[i] | i=1:numel(x) ], size(x) )
(-)(x::Number, y::Array) = reshape( [ x    - y[i] | i=1:numel(y) ], size(y) )
(-)(x::Array, y::Number) = reshape( [ x[i] - y    | i=1:numel(x) ], size(x) )

(.*)(x::Array, y::Array)  = reshape( [ x[i] .* y[i] | i=1:numel(x) ], size(x) )
(.*)(x::Number, y::Array) = reshape( [ x    .* y[i] | i=1:numel(y) ], size(y) )
(.*)(x::Array, y::Number) = reshape( [ x[i] .* y    | i=1:numel(x) ], size(x) )

(*)(x::Number, y::Array) = reshape( [ x    * y[i] | i=1:numel(y) ], size(y) )
(*)(x::Array, y::Number) = reshape( [ x[i] * y    | i=1:numel(x) ], size(x) )

(./)(x::Array, y::Array)  = reshape( [ x[i] ./ y[i] | i=1:numel(x) ], size(x) )
(./)(x::Number, y::Array) = reshape( [ x    ./ y[i] | i=1:numel(y) ], size(y) )
(./)(x::Array, y::Number) = reshape( [ x[i] ./ y    | i=1:numel(x) ], size(x) )

(/)(x::Number, y::Array) = reshape( [ x    / y[i] | i=1:numel(y) ], size(y) )
(/)(x::Array, y::Number) = reshape( [ x[i] / y    | i=1:numel(x) ], size(x) )

# Binary comparison operators

(<)(x::Array, y::Array)  = reshape( [ x[i] < y[i] | i=1:numel(x) ], size(x) )
(<)(x::Number, y::Array) = reshape( [ x    < y[i] | i=1:numel(y) ], size(y) )
(<)(x::Array, y::Number) = reshape( [ x[i] < y    | i=1:numel(x) ], size(x) )

(>)(x::Array, y::Array)  = reshape( [ x[i] > y[i] | i=1:numel(x) ], size(x) )
(>)(x::Number, y::Array) = reshape( [ x    > y[i] | i=1:numel(y) ], size(y) )
(>)(x::Array, y::Number) = reshape( [ x[i] > y    | i=1:numel(x) ], size(x) )

(<=)(x::Array, y::Array)  = reshape( [ x[i] <= y[i] | i=1:numel(x) ], size(x) )
(<=)(x::Number, y::Array) = reshape( [ x    <= y[i] | i=1:numel(y) ], size(y) )
(<=)(x::Array, y::Number) = reshape( [ x[i] <= y    | i=1:numel(x) ], size(x) )

(>=)(x::Array, y::Array)  = reshape( [ x[i] >= y[i] | i=1:numel(x) ], size(x) )
(>=)(x::Number, y::Array) = reshape( [ x    >= y[i] | i=1:numel(y) ], size(y) )
(>=)(x::Array, y::Number) = reshape( [ x[i] >= y    | i=1:numel(x) ], size(x) )

function (==)(x::Array, y::Array)
    if x.dims != y.dims; return false; end
    for i=1:numel(x); if x[i] != y[i]; return false; end; end
    return true
end

(==)(x::Number, y::Array) = reshape( [ x    == y[i] | i=1:numel(y) ], size(y) )
(==)(x::Array, y::Number) = reshape( [ x[i] == y    | i=1:numel(x) ], size(x) )

(!=)(x::Number, y::Array) = reshape( [ x    != y[i] | i=1:numel(y) ], size(y) )
(!=)(x::Array, y::Number) = reshape( [ x[i] != y    | i=1:numel(x) ], size(x) )

# Binary boolean operators

(&)(x::Array{Bool}, y::Array{Bool}) = reshape( [ x[i] & y[i] | i=1:numel(x) ], size(x) )
(&)(x::Bool, y::Array{Bool})        = reshape( [ x    & y[i] | i=1:numel(y) ], size(y) )
(&)(x::Array{Bool}, y::Bool)        = reshape( [ x[i] & y    | i=1:numel(x) ], size(x) )

(|)(x::Array{Bool}, y::Array{Bool}) = reshape( [ (x[i] | y[i]) | i=1:numel(x) ], size(x) )
(|)(x::Bool, y::Array{Bool})        = reshape( [ (x    | y[i]) | i=1:numel(y) ], size(y) )
(|)(x::Array{Bool}, y::Bool)        = reshape( [ (x[i] | y   ) | i=1:numel(x) ], size(x) )

($)(x::Array{Bool}, y::Array{Bool}) = reshape( [ x[i] $ y[i] | i=1:numel(x) ], size(x) )
($)(x::Bool, y::Array{Bool})        = reshape( [ x    $ y[i] | i=1:numel(y) ], size(y) )
($)(x::Array{Bool}, y::Bool)        = reshape( [ x[i] $ y    | i=1:numel(x) ], size(x) )

# Reductions

function sum(x::Matrix, dim::Int)
    if dim == 1
        [ sum(x[:,i]) | i=1:size(x, 2) ]
    elseif dim == 2
        [ sum(x[i,:]) | i=1:size(x, 1) ]
    end
end

function cumsum{T}(v::Vector{T})
    n = length(v)
    c = Array(T, n)
    
    c[1] = v[1]
    for i=2:n
        c[i] = v[i] + c[i-1]
    end
    return c
end

function cumprod{T}(v::Vector{T})
    n = length(v)
    c = Array(T, n)
    
    c[1] = v[1]
    for i=2:n
        c[i] = v[i] * c[i-1]
    end
    return c
end

transpose(x::Matrix) = [ x[j,i] | i=1:size(x,2), j=1:size(x,1) ]
ctranspose(x::Matrix) = [ conj(x[j,i]) | i=1:size(x,2), j=1:size(x,1) ]
transpose(x::Vector) = [ x[j] | i=1, j=1:size(x,1) ]
ctranspose(x::Vector) = [ conj(x[j]) | i=1, j=1:size(x,1) ]

diag(A::Matrix) = [ A[i,i] | i=1:min(size(A)) ]
diagm{T}(v::Vector{T}) = (n=length(v);
                          a=zeros(T,n,n);
                          for i=1:n; a[i,i] = v[i]; end;
                          a)

dot(x::Vector, y::Vector) = sum(x.*y)

trace(A::Matrix) = sum(diag(A))

mean(V::Vector) = sum(V) / length(V)

std(V::Vector) = (m = mean(V);
                  sqrt( sum([ (V[i] - m)^2 | i=1:length(V) ]) / (length(V)-1) ))

## blas.j defines these for floats; this handles other cases
(*)(A::Matrix, B::Vector) = [ dot(A[i,:],B) | i=1:size(A,1) ]
(*)(A::Matrix, B::Matrix) = [ dot(A[i,:],B[:,j]) | i=1:size(A,1), j=1:size(B,2) ]

# matrix multiply, treating a as a column, b as a row
kron(a::Vector, b::Vector) = [ a[i]*b[j] | i=1:length(a), j=1:length(b) ]

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


accumarray(I::Vector, J::Vector, V) = accumarray (I, J, V, max(I), max(J))

function accumarray{T}(I::Vector, J::Vector, V::Scalar{T}, m::Size, n::Size)
    A = zeros(T, m, n)
    for k=1:length(I)
        A[I[k], J[k]] += V
    end
    return A
end

function accumarray{T}(I::Vector, J::Vector, V::Vector{T}, m::Size, n::Size)
    A = zeros(T, m, n)
    for k=1:length(I)
        A[I[k], J[k]] += V[k]
    end
    return A
end

function find{T}(A::Vector{T})
    nnzA = nnz(A)
    I = Array(Size, nnzA)
    count = 1
    for i=1:length(A)
        if A[i] != 0
            I[count] = i
            count = count + 1
        end
    end
    return I
end

function find{T}(A::Matrix{T})
    nnzA = nnz(A)
    I = Array(Size, nnzA)
    J = Array(Size, nnzA)
    count = 1
    for i=1:size(A,1), j=1:size(A,2)
        if A[i,j] != 0
            I[count] = i
            J[count] = j
            count = count + 1
        end
    end
    return (I, J)
end

triu(M) = triu(M,0)
tril(M) = tril(M,0)
triu{T}(M::Matrix{T}, k) = [ j-i >= k ? M[i,j] : zero(T) |
                            i=1:size(M,1), j=1:size(M,2) ]
tril{T}(M::Matrix{T}, k) = [ j-i <= k ? M[i,j] : zero(T) |
                            i=1:size(M,1), j=1:size(M,2) ]

## Indexing: ref

ref(t::Tensor, r::Real...) = t[map(x->convert(Int32,round(x)),r)...]

ref(a::Array, i::Index) = arrayref(a,i)
ref{T}(a::Array{T,1}, i::Index) = arrayref(a,i)
ref(a::Array{Any,1}, i::Index) = arrayref(a,i)

ref(a::Matrix, i::Index, j::Index) = arrayref(a, (j-1)*a.dims[1] + i)

jl_fill_endpts(A,n,R::RangeBy)   = Range(1,R.step,size(A,n))
jl_fill_endpts(A,n,R::RangeFrom) = Range(R.start,R.step,size(A,n))
jl_fill_endpts(A,n,R::RangeTo)   = Range(1,R.step,R.stop)
jl_fill_endpts(A,n,R)            = R

ref(A::Vector,I::Indices) = [ A[i] | i = jl_fill_endpts(A,1,I) ]
ref(A::Array{Any,1},I::Indices) = { A[i] | i = jl_fill_endpts(A,1,I) }

ref(A::Matrix,I::Indices,J::Indices) = [ A[i,j] | i = jl_fill_endpts(A,1,I),
                                                  j = jl_fill_endpts(A,2,J) ]
ref(A::Matrix,i::Index,J::Indices) = [ A[i,j] | j = jl_fill_endpts(A,2,J) ]
ref(A::Matrix,I::Indices,j::Index) = [ A[i,j] | i = jl_fill_endpts(A,1,I) ]

function ref(A::Array, I::Index...)
    dims = A.dims
    ndims = length(I)

    index = I[1]
    stride = 1
    for k=2:ndims
        stride = stride * dims[k-1]
        index += (I[k]-1) * stride
    end

    return A[index]
end

ref(A::Array, I::Indices2, J::Indices2, K::Indices2...) =
    refND(A, I, J, K...)

function refND{T}(A::Array{T}, I::Indices2...)
    dims = A.dims
    ndimsA = length(dims)

    strides = zeros(Size, ndimsA)
    strides[1] = 1
    for d=2:ndimsA
        strides[d] = strides[d-1] * dims[d-1]
    end

    I_with_endpts = ()
    for i=1:length(I)
        I_with_endpts = tuple(I_with_endpts..., jl_fill_endpts(A, i, I[i]))
    end

    X = zeros(T, map(length, I_with_endpts))

    storeind = 1
    function store(ind)
        index = ind[1]
        for d=2:ndimsA
            index += (ind[d]-1) * strides[d]
        end
        X[storeind] = A[index]
        storeind += 1
    end

    ndmap(store, I_with_endpts)
    return X
end

## Indexing: assign

assign(t::Tensor, x, r::Real...) = (t[map(x->convert(Int32,round(x)),r)...] = x)

assign{T}(A::Array{T}, x, i::Index) = arrayset(A,i,convert(T,x))

assign(A::Array{Any}, x, i::Index) = arrayset(A,i,x)

function assign(A::Vector, x::Scalar, I::Indices)
    I = jl_fill_endpts(A, 1, I)
    for i=I
        A[i] = x
    end
    return A
end

function assign(A::Vector, X::Vector, I::Indices)
    I = jl_fill_endpts(A, 1, I)
    count = 1
    for i=I
        A[i] = X[count]
        count += 1
    end
    return A
end

assign(A::Matrix, x::Scalar, i::Index, j::Index) = A[(j-1)*A.dims[1] + i] = x

function assign(A::Matrix, x::Scalar, I::Indices2, J::Indices2)
    I = jl_fill_endpts(A, 1, I)
    J = jl_fill_endpts(A, 2, J)
    for i=I, j=J
        A[i,j] = x
    end
    return A
end

function assign(A::Matrix, X::Matrix, I::Indices2, J::Indices2)
    I = jl_fill_endpts(A, 1, I)
    J = jl_fill_endpts(A, 2, J)
    count = 1
    for i=I, j=J
        A[i,j] = X[count]
        count += 1
    end
    return A
end

assign(A::Array, x::Scalar, I::Index, J::Index, K::Index...) = 
   assignND_scalar(A, x, I, J, K...)

function assignND_scalar(A::Array, x::Scalar, I::Index...)
    dims = A.dims
    ndims = length(I)

    index = I[1]
    stride = 1
    for k=2:ndims
        stride = stride * dims[k-1]
        index += (I[k]-1) * stride
    end

    A[index] = x
    return A
end

assign(A::Array, X, I::Indices2, J::Indices2, K::Indices2...) = 
   assignND_all(A, X, I, J, K...)

function assignND_all(A::Array, X, I::Indices2...)
    dims = A.dims
    ndimsA = length(dims)

    strides = zeros(Size, ndimsA)
    strides[1] = 1
    for d=2:ndimsA
        strides[d] = strides[d-1] * dims[d-1]
    end

    I_with_endpts = ()
    for i=1:length(I)
        I_with_endpts = tuple(I_with_endpts..., jl_fill_endpts(A, i, I[i]))
    end

    if isa(X, Scalar)
        function store1(ind)
            index = ind[1]
            for d=2:ndimsA
                index += (ind[d]-1) * strides[d]
            end
            A[index] = X
        end
        
        ndmap(store1, I_with_endpts)
    else
        refind = 1
        function storeall(ind)
            index = ind[1]
            for d=2:ndimsA
                index += (ind[d]-1) * strides[d]
            end
            A[index] = X[refind]
            refind += 1
        end
        
        ndmap(storeall, I_with_endpts)
    end

    return A
end

# Concatenation
hcat() = Array(None,0)
hcat{T <: Scalar}(X::T...) = [ X[j] | i=1, j=1:length(X) ]
vcat() = Array(None,0)
vcat{T <: Scalar}(X::T...) = [ X[i] | i=1:length(X) ]

hcat{T}(V::Vector{T}...) = [ V[j][i] | i=1:length(V[1]), j=1:length(V) ]
function vcat{T}(V::Vector{T}...)
    a = Array(T, sum(map(length, V)))
    pos = 1
    for k=1:length(V), i=1:length(V[k])
        a[pos] = V[k][i]
        pos += 1
    end
    a
end

function hcat{T}(A::Matrix{T}...)
    ncols = sum([ size(A[i], 2) | i=1:length(A) ])
    nrows = size(A[1], 1)
    B = Array(T, nrows, ncols)

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
    B = Array(T, nrows, ncols)

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
isempty(a::Array) = (numel(a) == 0)

# map over vectors and matrices
map(f, v::Vector) = [ f(v[i]) | i=1:length(v) ]
map(f, M::Matrix) = [ f(M[i,j]) | i=1:size(M,1), j=1:size(M,2) ]

# other functions
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

# show arrays
function showall{T}(a::Array{T,1})
    if is(T,Any)
        opn = "{"; cls = "}"
    else
        opn = "["; cls = "]";
    end
    show_comma_array(a, opn, cls)
end

function showall{T}(a::Array{T,2})
    for i=1:size(a,1)
        show_cols(a, 1, size(a,2), i)
        print("\n")
    end
end

function show{T}(a::Array{T,1})
    if is(T,Any)
        opn = "{"; cls = "}"
    else
        opn = "["; cls = "]";
    end
    n = a.dims[1]
    if n <= 10
        show_comma_array(a, opn, cls)
    else
        show_comma_array(a[1:5], opn, "")
        print(",...,")
        show_comma_array(a[(n-4):n], "", cls)
    end
end

function show_cols(a, start, stop, i)
    for j = start:stop
        show(a[i,j])
        print(" ")
    end
end

function show{T}(a::Array{T,2})
    m = a.dims[1]
    n = a.dims[2]
    print_hdots = false
    print_vdots = false
    if 10 < m; print_vdots = true; end
    if 10 < n; print_hdots = true; end
    if !print_vdots && !print_hdots
        for i=1:m
            show_cols(a, 1, n, i)
            if i<m; print("\n"); end
        end
    elseif print_vdots && !print_hdots
        for i=1:3
            show_cols(a, 1, n, i)
            print("\n")
        end
        print(":\n")
        for i=m-2:m
            show_cols(a, 1, n, i)
            if i<m; print("\n"); end
        end
    elseif !print_vdots && print_hdots
        for i=1:m
            show_cols(a, 1, 3, i)
            if i == 1 || i == m; print(": "); else; print("  "); end
            show_cols(a, n-2, n, i)
            if i<m; print("\n"); end
        end
    else
        for i=1:3
            show_cols(a, 1, 3, i)
            if i == 1; print(": "); else; print("  "); end
            show_cols(a, n-2, n, i)
            print("\n")
        end
        print(":\n")
        for i=m-2:m
            show_cols(a, 1, 3, i)
            if i == m; print(": "); else; print("  "); end
            show_cols(a, n-2, n, i)
            if i<m; print("\n"); end
        end
    end
end

show{T}(a::Array{T,0}) = print("Array(",T,")")

function show(a::Array)
    slice2d(a, idxs) = [ a[i, j, idxs...] | i=1:size(a,1), j=1:size(a,2) ]
    tail = size(a)[3:]
    ndmap(idxs->(print("[:, :, ");
                 for i=1:(length(idxs)-1); print(idxs[i],", "); end;
                 print(idxs[length(idxs)], "] =\n");
                 print(slice2d(a, idxs), idxs==tail?"":"\n\n")),
          map(x->Range1(1,x), tail))
end

