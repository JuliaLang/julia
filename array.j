## array.j: Base Array functionality

## Type aliases for convenience ##

typealias Vector{T} Tensor{T,1}
typealias Matrix{T} Tensor{T,2}
typealias Indices{T} Union(Index,Vector{T})

## Basic functions ##
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

## Constructors ##

jl_comprehension_zeros{T,n}(oneresult::Tensor{T,n}, dims...) = Array(T, dims...)
jl_comprehension_zeros{T}(oneresult::T, dims...) = Array(T, dims...)
jl_comprehension_zeros(oneresult::(), dims...) = Array(None, dims...)

function array(t::Tuple)
    len = length(t)
    if len == 0; return []; end
    A = Array(typeof(t[1]), len)
    for i=1:len
        A[i] = t[i]
    end
    return A
end

function fill{T}(A::Array{T}, x::T)
    for i = 1:numel(A)
        A[i] = x
    end
    return A
end

for (t, f) = ((Float64, `rand), (Float32, `randf), (Float64, `randn))
    eval(`function ($f)(dims::(Size...))
              A = Array($t, dims)
              for i = 1:numel(A)
                  A[i] = ($f)()
              end
              return A
          end)
    eval(`( ($f)(dims::Size...) = ($f)(dims) ))
end

zeros{T}(::Type{T}, dims::Size...) = fill(Array(T, dims), zero(T))
zeros(dims::Size...) = zeros(Float64, dims...)

ones{T}(::Type{T}, dims::Size...) = fill(Array(T, dims), one(T))
ones(dims::Size...) = ones(Float64, dims...)

function copy_to(dest::Array, src::Array)
    for i=1:numel(src)
        dest[i] = copy(src[i])
    end
    return dest
end

copy{T}(a::Array{T}) = copy_to(Array(T, size(a)), a)

eye(n::Size) = eye(n, n)
eye(m::Size, n::Size) = (a = zeros(m,n);
                         for i=1:min(m,n); a[i,i]=1; end;
                         a)
one{T}(x::Array{T,2}) = (m=size(x,1); n=size(x,2);
                         a = zeros(T,m,n);
                         for i=1:min(m,n); a[i,i]=1; end;
                         a)
zero{T}(x::Array{T,2}) = zeros(T,size(x))

linspace(start::Real, stop::Real, stride::Real) =
    ((start, stop, stride) = promote(start, stop, stride);
     [ i | i=start:stride:stop ])

linspace(start::Real, stop::Real) =
    ((start, stop) = promote(start, stop);
     [ i | i=start:stop ])

## Unary operators ##

(-)(x::Array) = map(-, x)
(!)(x::Array{Bool}) = map(!, x)
(~)(x::Array{Bool}) = map(~, x)

conj{T <: Number}(x::Array{T}) = x
conj(x::Array) = map(conj, x)

real{T <: Number}(x::Array{T}) = x
real(x::Array) = map(real, x)

imag{T <: Number}(x::Array{T}) = zeros(T, size(x))
imag(x::Array) = map(imag, x)

## Binary arithmetic operators ##

( +)(x::Array, y::Array)  = map2(+ , x, y)
( +)(x::Number, y::Array) = map2(+ , x, y)
( +)(x::Array, y::Number) = map2(+ , x, y)

( -)(x::Array, y::Array)  = map2(- , x, y)
( -)(x::Number, y::Array) = map2(- , x, y)
( -)(x::Array, y::Number) = map2(- , x, y)

(.*)(x::Array, y::Array)  = map2(.*, x, y)
(.*)(x::Number, y::Array) = map2(.*, x, y)
(.*)(x::Array, y::Number) = map2(.*, x, y)

(./)(x::Array, y::Array)  = reshape( [ x[i] ./ y[i] | i=1:numel(x) ], size(x) )
(./)(x::Number, y::Array) = reshape( [ x    ./ y[i] | i=1:numel(y) ], size(y) )
(./)(x::Array, y::Number) = reshape( [ x[i] ./ y    | i=1:numel(x) ], size(x) )

(/)(x::Array, y::Number) = reshape( [ x[i] ./ y    | i=1:numel(x) ], size(x) )
(/)(x::Number, y::Array) = reshape( [ x    ./ y[i] | i=1:numel(y) ], size(y) )

(*)(x::Array, y::Number) = map2(.*, x, y)
(*)(x::Number, y::Array) = map2(.*, x, y)

# blas.j defines these for floats; this handles other cases
(*)(A::Matrix, B::Vector) = [ dot(A[i,:],B) | i=1:size(A,1) ]
(*)(A::Matrix, B::Matrix) = [ dot(A[i,:],B[:,j]) | i=1:size(A,1), j=1:size(B,2) ]

## Binary comparison operators ##

(==)(x::Array, y::Array)  = map2(Bool, ==, x, y)
(==)(x::Number, y::Array) = map2(Bool, ==, x, y)
(==)(x::Array, y::Number) = map2(Bool, ==, x, y)

(!=)(x::Array, y::Array)  = map2(Bool, !=, x, y)
(!=)(x::Number, y::Array) = map2(Bool, !=, x, y)
(!=)(x::Array, y::Number) = map2(Bool, !=, x, y)

( <)(x::Array, y::Array)  = map2(Bool, <, x, y)
( <)(x::Number, y::Array) = map2(Bool, <, x, y)
( <)(x::Array, y::Number) = map2(Bool, <, x, y)

( >)(x::Array, y::Array)  = map2(Bool, >, x, y)
( >)(x::Number, y::Array) = map2(Bool, >, x, y)
( >)(x::Array, y::Number) = map2(Bool, >, x, y)

(<=)(x::Array, y::Array)  = map2(Bool, <=, x, y)
(<=)(x::Number, y::Array) = map2(Bool, <=, x, y)
(<=)(x::Array, y::Number) = map2(Bool, <=, x, y)

(>=)(x::Array, y::Array)  = map2(Bool, >=, x, y)
(>=)(x::Number, y::Array) = map2(Bool, >=, x, y)
(>=)(x::Array, y::Number) = map2(Bool, >=, x, y)

## Binary boolean operators ##

(&)(x::Array, y::Array)  = map2(Bool, &, x, y)
(&)(x::Number, y::Array) = map2(Bool, &, x, y)
(&)(x::Array, y::Number) = map2(Bool, &, x, y)

(|)(x::Array, y::Array)  = map2(Bool, |, x, y)
(|)(x::Number, y::Array) = map2(Bool, |, x, y)
(|)(x::Array, y::Number) = map2(Bool, |, x, y)

($)(x::Array, y::Array)  = map2(Bool, $, x, y)
($)(x::Number, y::Array) = map2(Bool, $, x, y)
($)(x::Array, y::Number) = map2(Bool, $, x, y)

## Indexing: ref ##

ref(t::Tensor, r::Real...) = t[map(x->convert(Int32,round(x)),r)...]

ref(a::Array, i::Index) = arrayref(a,i)
ref{T}(a::Array{T,1}, i::Index) = arrayref(a,i)
ref(a::Array{Any,1}, i::Index) = arrayref(a,i)

ref{T}(A::Array{T,1},I::Indices) = [ A[i] | i = I ]
ref(A::Array{Any,1},I::Indices) = { A[i] | i = I }

ref{T}(a::Array{T,2}, i::Index, j::Index) = arrayref(a, (j-1)*a.dims[1] + i)
ref{T}(A::Array{T,2}, I::Indices, J::Indices) = [ A[i,j] | i = I, j = J ]

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

function ref{T}(A::Array{T}, I::Indices...)
    dims = A.dims
    ndimsA = length(dims)

    strides = Array(Size, ndimsA)
    strides[1] = 1
    for d=2:ndimsA
        strides[d] = strides[d-1] * dims[d-1]
    end

    X = Array(T, map(length, I))

    storeind = 1
    function store(ind)
        index = ind[1]
        for d=2:ndimsA
            index += (ind[d]-1) * strides[d]
        end
        X[storeind] = A[index]
        storeind += 1
    end

    cartesian_map(store, I)
    return X
end

## Indexing: assign ##

assign(t::Tensor, x, r::Real...) = (t[map(x->convert(Int32,round(x)),r)...] = x)
assign{T}(A::Array{T}, x, i::Index) = arrayset(A,i,convert(T, x))
assign(A::Array{Any}, x, i::Index) = arrayset(A,i,x)

function assign(A::Vector, x::Scalar, I::Indices)
    for i=I
        A[i] = x
    end
    return A
end

function assign(A::Vector, X::Vector, I::Indices)
    count = 1
    for i=I
        A[i] = X[count]
        count += 1
    end
    return A
end

assign(A::Matrix, x::Scalar, i::Index, j::Index) = 
    A[(j-1)*A.dims[1] + i] = x

function assign(A::Matrix, x::Scalar, I::Indices, J::Indices)
    for i=I, j=J
        A[i,j] = x
    end
    return A
end

function assign(A::Matrix, X::Array, I::Indices, J::Indices)
    count = 1
    for i=I, j=J
        A[i,j] = X[count]
        count += 1
    end
    return A
end

function assign(A::Array, x::Scalar, I::Index...)
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

function assign(A::Array, x::Scalar, I::Indices...)
    dims = A.dims
    ndimsA = length(dims)

    strides = Array(Size, ndimsA)
    strides[1] = 1
    for d=2:ndimsA
        strides[d] = strides[d-1] * dims[d-1]
    end

    function store_one(ind)
        index = ind[1]
        for d=2:ndimsA
            index += (ind[d]-1) * strides[d]
        end
        A[index] = x
    end
        
    cartesian_map(store_one, I)
    return A
end

function assign(A::Array, X::Array, I::Indices...)
    dims = A.dims
    ndimsA = length(dims)

    strides = Array(Size, ndimsA)
    strides[1] = 1
    for d=2:ndimsA
        strides[d] = strides[d-1] * dims[d-1]
    end

    refind = 1
    function store_all(ind)
        index = ind[1]
        for d=2:ndimsA
            index += (ind[d]-1) * strides[d]
        end
        A[index] = X[refind]
        refind += 1
    end
    
    cartesian_map(store_all, I)
    return A
end

## Concatenation ##

cat(catdim::Int) = Array(None,0)

vcat() = Array(None,0)
hcat() = Array(None,0)

## cat: special cases
hcat{T <: Scalar}(X::T...) = [ X[j] | i=1, j=1:length(X) ]
vcat{T <: Scalar}(X::T...) = [ X[i] | i=1:length(X) ]

hcat{T}(V::Array{T,1}...) = [ V[j][i] | i=1:length(V[1]), j=1:length(V) ]

function vcat{T}(V::Array{T,1}...)
    a = Array(T, sum(map(length, V)))
    pos = 1
    for k=1:length(V), i=1:length(V[k])
        a[pos] = V[k][i]
        pos += 1
    end
    a
end

function hcat{T}(A::Array{T,2}...)
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

function vcat{T}(A::Array{T,2}...)
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

## cat: general case

function cat(catdim::Int, X::Scalar...)
    typeC = promote_type(map(typeof, X)...)
    nargs = length(X)
    if catdim == 1
        dimsC = nargs
    elseif catdim == 2
        dimsC = (1, nargs)
    end
    C = Array(typeC, dimsC)

    for i=1:nargs
        C[i] = X[i]
    end
    return C
end

vcat(X::Scalar...) = cat(1, X...)
hcat(X::Scalar...) = cat(2, X...)

#function cat(catdim::Int, A::Union(Number,Array)...)
#    cat(catdim, map(A, x->(isscalar(x) ? [x] : x))...)
#end

function cat(catdim::Int, A::Array...)
    # ndims of all input arrays should be in [d-1, d]

    nargs = length(A)
    dimsA = ntuple(nargs, i->A[i].dims)
    ndimsA = ntuple(nargs, i->length(dimsA[i]))
    d_max = max(ndimsA)
    d_min = min(ndimsA)

    cat_ranges = ntuple(nargs, i->(catdim <= ndimsA[i] ? dimsA[i][catdim] : 1))

    function compute_dims(d)
        if d == catdim
            if catdim <= d_max
                return sum(cat_ranges)
            else
                return nargs
            end
        else
            if d <= d_max
                return dimsA[1][d]
            else
                return 1
            end
        end
    end

    ndimsC = max(catdim, d_max)
    dimsC = ntuple(ndimsC, compute_dims)
    typeC = promote_type(ntuple(nargs, i->typeof(A[i]).parameters[1])...)
    C = Array(typeC, dimsC)

    cat_ranges = cumsum(1, cat_ranges...)
    for k=1:nargs
        cat_one = ntuple(ndimsC, i->(i != catdim ? 
                                     Range1(1,dimsC[i]) :
                                     Range1(cat_ranges[k],cat_ranges[k+1]-1) ))
        C[cat_one...] = A[k]
    end
    return C
end

vcat(A::Array...) = cat(1, A...)
hcat(A::Array...) = cat(2, A...)

## Reductions ##

## Type inference doesn't work on op, resulting in cell arrays
# function reduce{T}(op, A::Array{T,2}, dim::Int)
#     if dim == 1
#         [ reduce(op, A[:,i]) | i=1:size(A, 2) ]
#     elseif dim == 2
#         [ reduce(op, A[i,:]) | i=1:size(A, 1) ]
#     end
# end

function reduce{T}(RType::Type, op, A::Array{T}, region)
    dimsA = A.dims
    ndimsA = length(dimsA)
    dimsR = ntuple(ndimsA, i->(contains(region, i) ? 1 : dimsA[i]))
    R = Array(RType, dimsR)

    function reduce_one(ind)
        sliceA = ntuple(ndimsA, i->(contains(region, i) ?
                                    Range1(1,dimsA[i]) :
                                    ind[i]))
        R[ind...] = reduce(op, A[sliceA...])
    end

    cartesian_map(reduce_one, ntuple(ndimsA, i->(Range1(1,dimsR[i]))) )
    return R
end

max{T}(A::Array{T}, region::Union(Int, Tuple)) = reduce(T, max, A, region)
min{T}(A::Array{T}, region::Union(Int, Tuple)) = reduce(T, min, A, region)
sum{T}(A::Array{T}, region::Union(Int, Tuple)) = reduce(T, +, A, region)
prod{T}(A::Array{T}, region::Union(Int, Tuple)) = reduce(T, .*, A, region)
all(A::Array{Bool}, region::Union(Int, Tuple)) = reduce(Bool, all, A, region)
any(A::Array{Bool}, region::Union(Int, Tuple)) = reduce(Bool, any, A, region)

function isequal(x::Array, y::Array)
    if x.dims != y.dims
        return false
    end

    for i=1:numel(x)
        if x[i] != y[i]
            return false
        end
    end
    return true
end

function scan{T}(op, v::Vector{T})
    n = length(v)
    c = Array(T, n)
    if n == 0; return c; end

    c[1] = v[1]
    for i=2:n
        c[i] = op(v[i], c[i-1])
    end
    return c
end

cumsum(v::Vector) = scan(+, v)
cumprod(v::Vector) = scan(*, v)

## iteration support for arrays as ranges ##

start(a::Array) = 1
next(a::Array,i) = (a[i],i+1)
done(a::Array,i) = (i > numel(a))
isempty(a::Array) = (numel(a) == 0)

## map over arrays ##

map(f, v::Vector) = [ f(v[i]) | i=1:length(v) ]
map(f, M::Matrix) = [ f(M[i,j]) | i=1:size(M,1), j=1:size(M,2) ]

function map{T}(f, A::Array{T})
    F = Array(T, size(A))
    for i=1:numel(A)
        F[i] = f(A[i])
    end
    return F
end

map2(f, A::Number, B::Number) = f(A,B)
map2(Ftype::Type, f, A::Number, B::Number) = convert(Ftype, f(A,B))

map2{S,T}(f, A::Array{S}, B::Array{T}) = map2(promote_type(S,T), f, A, B)

function map2{S,T}(Ftype::Type, f, A::Array{S}, B::Array{T})
    F = Array(Ftype, size(A))
    for i=1:numel(A)
        F[i] = f(A[i], B[i])
    end
    return F
end

map2{T}(f, A::Number, B::Array{T}) = map2(promote_type(typeof(A),T), f, A, B)

function map2{T}(Ftype::Type, f, A::Number, B::Array{T})
    F = Array(Ftype, size(B))
    for i=1:numel(B)
        F[i] = f(A, B[i])
    end
    return F
end

map2{T}(f, A::Array{T}, B::Number) = map2(promote_type(T,typeof(B)), f, A, B)

function map2{T}(Ftype::Type, f, A::Array{T}, B::Number)
    F = Array(Ftype, size(A))
    for i=1:numel(A)
        F[i] = f(A[i], B)
    end
    return F
end

function cartesian_map(body, t::Tuple, it...)
    idx = length(t)-length(it)
    if idx == 0
        body(it)
    else
        for i = t[idx]
            cartesian_map(body, t, i, it...)
        end
    end
end

## Transpose, Permute ##

reverse(v::Vector) = [ v[length(v)-i+1] | i=1:length(v) ]

transpose(x::Vector)  = [ x[j]         | i=1, j=1:size(x,1) ]
ctranspose(x::Vector) = [ conj(x[j])   | i=1, j=1:size(x,1) ]

#transpose(x::Matrix)  = [ x[j,i]       | i=1:size(x,2), j=1:size(x,1) ]
ctranspose(x::Matrix) = [ conj(x[j,i]) | i=1:size(x,2), j=1:size(x,1) ]

function transpose{T}(a::Matrix{T})
    b = Array(T, a.dims[2], a.dims[1])
    for i=1:a.dims[1], j=1:a.dims[2]
        b[j,i] = a[i,j]
    end
    return b
end

function permute{T}(A::Array{T}, perm)
    dimsA = A.dims
    ndimsA = length(dimsA)
    dimsP = ntuple(ndimsA, i->dimsA[perm[i]])
    P = Array(T, dimsP)

    count = 1
    function permute_one(ind)
        P[count] = A[ntuple(ndimsA, i->ind[perm[i]])...]
        count += 1
    end

    cartesian_map(permute_one, ntuple(ndimsA, i->(Range1(1,dimsP[i]))) )
    return P
end

function ipermute{T}(A::Array{T}, perm)
    dimsA = A.dims
    ndimsA = length(dimsA)
    dimsP = ntuple(ndimsA, i->dimsA[perm[i]])
    P = Array(T, dimsP)

    count = 1
    function permute_one(ind)
        P[ntuple(ndimsA, i->ind[perm[i]])...] = A[count]
        count += 1
    end

    cartesian_map(permute_one, ntuple(ndimsA, i->(Range1(1,dimsP[i]))) )
    return P
end

## Other array functions ##

repmat(a::Matrix, m::Size, n::Size) = reshape([ a[i,j] | i=1:size(a,1),
                                                         k=1:m,
                                                         j=1:size(a,2),
                                                         l=1:n],
                                              size(a,1)*m,
                                              size(a,2)*n)


accumarray(I::Vector, J::Vector, V) = accumarray (I, J, V, max(I), max(J))

function accumarray{T}(I::Vector, J::Vector, V::Scalar{T}, m::Size, n::Size)
    A = Array(T, m, n)
    for k=1:length(I)
        A[I[k], J[k]] += V
    end
    return A
end

function accumarray{T}(I::Vector, J::Vector, V::Vector{T}, m::Size, n::Size)
    A = Array(T, m, n)
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
            count += 1
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
            count += 1
        end
    end
    return (I, J)
end

function find{T}(A::Array{T})
    ndimsA = ndims(A)
    nnzA = nnz(A)
    I = ntuple(ndimsA, x->Array(Size, nnzA))

    count = 1
    function find_one(ind)
        Aind = A[ind...]
        if Aind != 0
            for i=1:ndimsA
                I[i][count] = ind[i]
            end
            count += 1
        end
    end

    cartesian_map(find_one, ntuple(ndims(A), d->(1:A.dims[d])) )
    return I
end

sub2ind(dims, i::Index) = i
sub2ind(dims, i::Index, j::Index) = (j-1)*dims[1] + i

function sub2ind(dims, I::Index...)
    ndims = length(dims)
    index = I[1]
    stride = 1
    for k=2:ndims
        stride = stride * dims[k-1]
        index += (I[k]-1) * stride
    end
    return index
end

sub2ind(dims, I::Vector...) =
    [ sub2ind(dims, map(X->X[i], I)...) | i=1:length(I[1]) ]

function ind2sub(dims, ind::Index)
    ndims = length(dims)
    x = tuple(1, cumprod(dims)...)

    sub = ()
    for i=ndims:-1:1
        rest = rem(ind-1, x[i]) + 1
        sub = tuple(div(ind - rest, x[i]) + 1, sub...)
        ind = rest
    end
    return sub
end
