## array.j: Base Array functionality

## Type aliases for convenience ##

typealias Vector{T} Tensor{T,1}
typealias Matrix{T} Tensor{T,2}
typealias Indices Union(Index, Vector{Index})
typealias Dims (Size...)

## Basic functions ##
size(a::Array) = a.dims
numel(a::Array) = arraylen(a)

size(t::Tensor, d) = size(t)[d]
ndims(t::Tensor) = length(size(t))
numel(t::Tensor) = prod(size(t))
length(v::Vector) = numel(v)
nnz(a::Tensor) = (n = 0; for i=1:numel(a); n += a[i] != 0 ? 1 : 0; end; n)

reshape{T}(a::Array{T}, dims...) = (b = Array(T, dims...);
                                    for i=1:numel(a); b[i] = a[i]; end;
                                    b)
reshape(a::Array, dims::Dims) = reshape(a, dims...)

## Constructors ##

jl_comprehension_zeros{T,n}(oneresult::Tensor{T,n}, dims...) = Array(T, dims...)
jl_comprehension_zeros{T}(oneresult::T, dims...) = Array(T, dims...)
jl_comprehension_zeros(oneresult::(), dims...) = Array(None, dims...)

function fill{T}(A::Array{T}, x::T)
    for i = 1:numel(A)
        A[i] = x
    end
    return A
end

for (t, f) = ((Float64, `rand), (Float32, `randf), (Float64, `randn))
    eval(`function ($f)(dims::Dims)
              A = Array($t, dims)
              for i = 1:numel(A)
                  A[i] = ($f)()
              end
              return A
          end)
    eval(`( ($f)(dims::Size...) = ($f)(dims) ))
end

zeros{T}(::Type{T}, dims::Dims) = fill(Array(T, dims), zero(T))
zeros(T::Type, dims::Size...) = zeros(T, dims)
zeros(dims::Dims) = zeros(Float64, dims)
zeros(dims::Size...) = zeros(dims)

ones{T}(::Type{T}, dims::Dims) = fill(Array(T, dims), one(T))
ones(T::Type, dims::Size...) = ones(T, dims)
ones(dims::Dims) = ones(Float64, dims)
ones(dims::Size...) = ones(dims)

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
                         a = zeros(T,size(x));
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

conj{T <: Number}(x::Array{T}) = x
real{T <: Number}(x::Array{T}) = x
imag{T <: Number}(x::Array{T}) = zeros(T, size(x))

for f=(`-, `~, `conj, `real, `imag)
    eval(`function ($f){T}(A::Array{T})
            F = Array(T, size(A))
            for i=1:numel(A)
               F[i] = ($f)(A[i])
            end
            return F
         end)
end

function !(A::Tensor{Bool})
    F = Array(Bool, size(A))
    for i=1:numel(A)
        F[i] = !A[i]
    end
    return F
end

## Binary arithmetic operators ##

# blas.j defines these for floats; this handles other cases
(*)(A::Matrix, B::Vector) = [ dot(A[i,:],B) | i=1:size(A,1) ]
(*)(A::Matrix, B::Matrix) = [ dot(A[i,:],B[:,j]) | i=1:size(A,1), j=1:size(B,2) ]

(*)(A::Number, B::Tensor) = A .* B
(*)(A::Tensor, B::Number) = A .* B

(./)(x::Array, y::Array)  = reshape( [ x[i] ./ y[i] | i=1:numel(x) ], size(x) )
(./)(x::Number, y::Array) = reshape( [ x    ./ y[i] | i=1:numel(y) ], size(y) )
(./)(x::Array, y::Number) = reshape( [ x[i] ./ y    | i=1:numel(x) ], size(x) )

(/)(A::Number, B::Tensor) = A ./ B
(/)(A::Tensor, B::Number) = A ./ B

(.\)(x::Array, y::Array)  = reshape( [ x[i] .\ y[i] | i=1:numel(x) ], size(x) )
(.\)(x::Number, y::Array) = reshape( [ x    .\ y[i] | i=1:numel(y) ], size(y) )
(.\)(x::Array, y::Number) = reshape( [ x[i] .\ y    | i=1:numel(x) ], size(x) )

(\)(A::Number, B::Tensor) = A .\ B
(\)(A::Tensor, B::Number) = A .\ B

for f=(`+, `-, `(.*), `(.^))
    eval(`function ($f){S,T}(A::Tensor{S}, B::Tensor{T})
            F = Array(promote_type(S,T), size(A))
            for i=1:numel(A)
               F[i] = ($f)(A[i], B[i])
            end
            return F
         end)

    eval(`function ($f){T}(A::Number, B::Tensor{T})
            F = Array(promote_type(typeof(A),T), size(B))
            for i=1:numel(B)
               F[i] = ($f)(A, B[i])
            end
            return F
         end)

    eval(`function ($f){T}(A::Tensor{T}, B::Number)
            F = Array(promote_type(T,typeof(B)), size(A))
            for i=1:numel(A)
               F[i] = ($f)(A[i], B)
            end
            return F
         end)
end

## Binary comparison operators ##

for f=(`(==), `(!=), `<, `>, `(<=), `(>=))
    eval(`function ($f)(A::Array, B::Array)
            F = Array(Bool, size(A))
            for i=1:numel(A)
               F[i] = ($f)(A[i], B[i])
            end
            return F
         end)

    eval(`function ($f)(A::Number, B::Array)
            F = Array(Bool, size(B))
            for i=1:numel(B)
               F[i] = ($f)(A, B[i])
            end
            return F
         end)

    eval(`function ($f)(A::Array, B::Number)
            F = Array(Bool, size(A))
            for i=1:numel(A)
               F[i] = ($f)(A[i], B)
            end
            return F
         end)
end


## Binary boolean operators ##

for f=(`&, `|, `$)
    eval(`function ($f)(A::Tensor{Bool}, B::Tensor{Bool})
            F = Array(Bool, size(A))
            for i=1:numel(A)
               F[i] = ($f)(A[i], B[i])
            end
            return F
         end)

    eval(`function ($f)(A::Bool, B::Tensor{Bool})
            F = Array(Bool, size(B))
            for i=1:numel(B)
               F[i] = ($f)(A, B[i])
            end
            return F
         end)

    eval(`function ($f)(A::Tensor{Bool}, B::Bool)
            F = Array(Bool, size(A))
            for i=1:numel(A)
               F[i] = ($f)(A[i], B)
            end
            return F
         end)
end

## Indexing: ref ##

ref(a::Array, i::Index) = arrayref(a,i)
ref{T}(a::Array{T,1}, i::Index) = arrayref(a,i)
ref(a::Array{Any,1}, i::Index) = arrayref(a,i)
ref{T}(a::Array{T,2}, i::Index, j::Index) = arrayref(a, (j-1)*a.dims[1] + i)

ref(t::Tensor, r::Real...) = t[map(x->convert(Int32,round(x)),r)...]

ref{T}(A::Tensor{T,1}, I::Vector{Index}) = [ A[i] | i = I ]
ref(A::Tensor{Any,1}, I::Vector{Index}) = { A[i] | i = I }

ref{T}(A::Tensor{T,2}, I::Indices, J::Indices) = [ A[i,j] | i = I, j = J ]

function ref(A::Tensor, I::Index...)
    dims = size(A)
    ndims = length(I)

    index = I[1]
    stride = 1
    for k=2:ndims
        stride = stride * dims[k-1]
        index += (I[k]-1) * stride
    end

    return A[index]
end

function ref{T}(A::Tensor{T}, I::Indices...)
    dims = size(A)
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

assign(A::Array{Any}, x, i::Index) = arrayset(A,i,x)
assign{T}(A::Array{T}, x, i::Index) = arrayset(A,i,convert(T, x))

assign(t::Tensor, x, r::Real...) = (t[map(x->convert(Int32,round(x)),r)...] = x)

function assign(A::Vector, x::Scalar, I::Vector{Index})
    for i=I
        A[i] = x
    end
    return A
end

function assign(A::Vector, X::Vector, I::Vector{Index})
    for i=1:length(I)
        A[I[i]] = X[i]
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

function assign(A::Matrix, X::Tensor, I::Indices, J::Indices)
    count = 1
    for i=I, j=J
        A[i,j] = X[count]
        count += 1
    end
    return A
end

function assign(A::Tensor, x::Scalar, I0::Index, I::Index...)
    dims = size(A)
    index = I0
    stride = 1
    for k=1:length(I)
        stride = stride * dims[k]
        index += (I[k]-1) * stride
    end
    A[index] = x
    return A
end


function assign(A::Tensor, x::Scalar, I0::Indices, I::Indices...)
    dims = size(A)
    ndimsA = length(dims)

    strides = cumprod(dims)

    function store_one(ind)
        index = ind[1]
        for d=2:ndimsA
            index += (ind[d]-1) * strides[d-1]
        end
        A[index] = x
    end
        
    cartesian_map(store_one, append(tuple(I0), I))
    return A
end

function assign(A::Tensor, X::Tensor, I0::Indices, I::Indices...)
    dims = size(A)
    ndimsA = length(dims)

    strides = cumprod(dims)

    refind = 1
    function store_all(ind)
        index = ind[1]
        for d=2:ndimsA
            index += (ind[d]-1) * strides[d-1]
        end
        A[index] = X[refind]
        refind += 1
    end
    
    cartesian_map(store_all, append(tuple(I0), I))
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
    for k=1:length(V)
        Vk = V[k]
        for i=1:length(Vk)
            a[pos] = Vk[i]
            pos += 1
        end
    end
    a
end

function hcat{T}(A::Array{T,2}...)
    ncols = sum([ size(A[i], 2) | i=1:length(A) ])
    nrows = size(A[1], 1)
    B = Array(T, nrows, ncols)
    pos = 1
    for k=1:length(A)
        Ak = A[k]
        for i=1:numel(Ak)
            B[pos] = Ak[i]
            pos = pos + 1
        end
    end
    return B
end

function vcat{T}(A::Array{T,2}...)
    nrows = sum([size(A[i], 1) | i=1:length(A)])
    ncols = size(A[1], 2)
    B = Array(T, nrows, ncols)
    pos = 1
    for j=1:ncols, k=1:length(A)
        Ak = A[k]
        for i=1:size(Ak, 1)
            B[pos] = Ak[i,j]
            pos = pos + 1
        end
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
    dimsA = ntuple(nargs, i->size(A[i]))
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

for f = (`max, `min, `sum, `prod) 
    eval(`function ($f){T}(A::Tensor{T,2}, dim::Union(Int,Tuple))
            if isinteger(dim)
               if dim == 1
                 [ ($f)(A[:,i]) | i=1:size(A, 2) ]
              elseif dim == 2
                 [ ($f)(A[i,:]) | i=1:size(A, 1) ]
              end
            elseif dim == (1,2)
                 ($f)(A)
            end
         end)
end

function areduce{T}(f::Function, A::Array{T}, region::Union(Int,Tuple))
    dimsA = size(A)
    ndimsA = length(dimsA)
    dimsR = ntuple(ndimsA, i->(contains(region, i) ? 1 : dimsA[i]))
    R = Array(T, dimsR)
    
    function reduce_one(ind)
        sliceA = ntuple(ndimsA, i->(contains(region, i) ?
                                    Range1(1,dimsA[i]) :
                                    ind[i]))
        R[ind...] = f(A[sliceA...])
    end
    
    cartesian_map(reduce_one, ntuple(ndimsA, i->(Range1(1,dimsR[i]))) )
    return R
end

max (A::Array,       region::Union(Int,Tuple)) = areduce(max,  A, region)
min (A::Array,       region::Union(Int,Tuple)) = areduce(min,  A, region)
sum (A::Array,       region::Union(Int,Tuple)) = areduce(sum,  A, region)
prod(A::Array,       region::Union(Int,Tuple)) = areduce(prod, A, region)
all (A::Array{Bool}, region::Union(Int,Tuple)) = areduce(all,  A, region)
any (A::Array{Bool}, region::Union(Int,Tuple)) = areduce(any,  A, region)

for f = (`all, `any)
    eval(`function ($f)(A::Tensor{Bool,2}, dim::Union(Int,Tuple))
            if isinteger(dim)
               if dim == 1
                 [ ($f)(A[:,i]) | i=1:size(A, 2) ]
              elseif dim == 2
                 [ ($f)(A[i,:]) | i=1:size(A, 1) ]
              end
            elseif dim == (1,2)
                 ($f)(A)
            end
         end)
end

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

for (f, op) = ((`cumsum, `+), (`cumprod, `(.*)) )

    eval(`function ($f){T}(v::Vector{T})
            n = length(v)
            c = Array(T, n)
            if n == 0; return c; end

            c[1] = v[1]
            for i=2:n
               c[i] = ($op)(v[i], c[i-1])
            end
            return c
         end)
end

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
    dims = size(a)
    b = Array(T, dims[2], dims[1])
    for i=1:dims[1], j=1:dims[2]
        b[j,i] = a[i,j]
    end
    return b
end

function permute{T}(A::Array{T}, perm)
    dimsA = size(A)
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
    dimsA = size(A)
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

    cartesian_map(find_one, ntuple(ndims(A), d->(1:size(A)[d])) )
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
