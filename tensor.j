## tensor.j : Functions over tensors, not specialized to specific implementation

## Type aliases for convenience ##

typealias Vector{T} Tensor{T,1}
typealias Matrix{T} Tensor{T,2}
typealias Indices Union(Index, Vector{Index})
typealias Dims (Size...)
typealias Region Union(Size,Dims)

## Basic functions ##

size(t::Tensor, d) = size(t)[d]
ndims{T,n}(::Tensor{T,n}) = n
numel(t::Tensor) = prod(size(t))
length(v::Vector) = numel(v)
nnz(a::Tensor) = (n = 0; for i=1:numel(a); n += a[i] != 0 ? 1 : 0; end; n)

## Constructors ##

reshape(a::Tensor, dims::Dims) = (b = clone(a, dims);
                                  for i=1:numel(a); b[i] = a[i]; end;
                                  b)
reshape(a::Tensor, dims::Size...) = reshape(a, dims)

function fill(A::Tensor, x)
    for i = 1:numel(A)
        A[i] = x
    end
    return A
end

function copy_to(dest::Tensor, src::Tensor)
    for i=1:numel(src)
        dest[i] = copy(src[i])
    end
    return dest
end

copy(a::Tensor) = copy_to(clone(a), a)

eye(n::Size) = eye(n, n)
eye(m::Size, n::Size) = (a = zeros(m,n);
                         for i=1:min(m,n); a[i,i]=1; end;
                         a)
one{T}(x::Tensor{T,2}) = (m=size(x,1); n=size(x,2);
                          a = zeros(T,size(x));
                          for i=1:min(m,n); a[i,i]=1; end;
                          a)
zero{T}(x::Tensor{T,2}) = zeros(T,size(x))

linspace(start::Real, stop::Real, stride::Real) =
    ((start, stop, stride) = promote(start, stop, stride);
     [ i | i=start:stride:stop ])

linspace(start::Real, stop::Real) =
    ((start, stop) = promote(start, stop);
     [ i | i=start:stop ])

## Unary operators ##

conj{T <: Real}(x::Tensor{T}) = x
real{T <: Real}(x::Tensor{T}) = x
imag{T <: Real}(x::Tensor{T}) = zero(x)

for f=(`-, `~, `conj, `real, `imag)
    eval(`function ($f)(A::Tensor)
            F = clone(A)
            for i=1:numel(A)
               F[i] = ($f)(A[i])
            end
            return F
         end)
end

(+){T<:Number}(x::Tensor{T}) = x
(*){T<:Number}(x::Tensor{T}) = x

function !(A::Tensor{Bool})
    F = clone(A)
    for i=1:numel(A)
        F[i] = !A[i]
    end
    return F
end

## Binary arithmetic operators ##

(*)(A::Number, B::Tensor) = A .* B
(*)(A::Tensor, B::Number) = A .* B

(./)(x::Tensor, y::Tensor) = reshape( [ x[i] ./ y[i] | i=1:numel(x) ], size(x) )
(./)(x::Number, y::Tensor) = reshape( [ x    ./ y[i] | i=1:numel(y) ], size(y) )
(./)(x::Tensor, y::Number) = reshape( [ x[i] ./ y    | i=1:numel(x) ], size(x) )

(/)(A::Number, B::Tensor) = A ./ B
(/)(A::Tensor, B::Number) = A ./ B

(.\)(x::Tensor, y::Tensor) = reshape( [ x[i] .\ y[i] | i=1:numel(x) ], size(x) )
(.\)(x::Number, y::Tensor) = reshape( [ x    .\ y[i] | i=1:numel(y) ], size(y) )
(.\)(x::Tensor, y::Number) = reshape( [ x[i] .\ y    | i=1:numel(x) ], size(x) )

(\)(A::Number, B::Tensor) = A .\ B
(\)(A::Tensor, B::Number) = A .\ B

for f=(`+, `-, `(.*), `(.^))
    eval(`function ($f){S,T}(A::Tensor{S}, B::Tensor{T})
            F = clone(A, promote_type(S,T))
            for i=1:numel(A)
               F[i] = ($f)(A[i], B[i])
            end
            return F
         end)

    eval(`function ($f){T}(A::Number, B::Tensor{T})
            F = clone(B, promote_type(typeof(A),T))
            for i=1:numel(B)
               F[i] = ($f)(A, B[i])
            end
            return F
         end)

    eval(`function ($f){T}(A::Tensor{T}, B::Number)
            F = clone(A, promote_type(T,typeof(B)))
            for i=1:numel(A)
               F[i] = ($f)(A[i], B)
            end
            return F
         end)
end

## Binary comparison operators ##

for f=(`(==), `(!=), `<, `>, `(<=), `(>=))
    eval(`function ($f)(A::Tensor, B::Tensor)
            F = clone(A, Bool)
            for i=1:numel(A)
               F[i] = ($f)(A[i], B[i])
            end
            return F
         end)

    eval(`function ($f)(A::Number, B::Tensor)
            F = clone(B, Bool)
            for i=1:numel(B)
               F[i] = ($f)(A, B[i])
            end
            return F
         end)

    eval(`function ($f)(A::Tensor, B::Number)
            F = clone(A, Bool)
            for i=1:numel(A)
               F[i] = ($f)(A[i], B)
            end
            return F
         end)
end


## Binary boolean operators ##

for f=(`&, `|, `$)
    eval(`function ($f)(A::Tensor{Bool}, B::Tensor{Bool})
            F = clone(A, Bool)
            for i=1:numel(A)
               F[i] = ($f)(A[i], B[i])
            end
            return F
         end)

    eval(`function ($f)(A::Bool, B::Tensor{Bool})
            F = clone(B, Bool)
            for i=1:numel(B)
               F[i] = ($f)(A, B[i])
            end
            return F
         end)

    eval(`function ($f)(A::Tensor{Bool}, B::Bool)
            F = clone(A, Bool)
            for i=1:numel(A)
               F[i] = ($f)(A[i], B)
            end
            return F
         end)
end

## Indexing: ref ##

ref(t::Tensor, r::Real...) = t[map(x->convert(Int32,round(x)),r)...]

ref(A::Vector, I::Vector{Index}) = [ A[i] | i = I ]
ref(A::Tensor{Any,1}, I::Vector{Index}) = { A[i] | i = I }

ref(A::Matrix, I::Indices, J::Indices) = [ A[i,j] | i = I, j = J ]

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

function ref(A::Tensor, I::Indices...)
    dims = size(A)
    ndimsA = length(dims)

    strides = cumprod(dims)
    X = clone(A, map(length, I))

    storeind = 1
    function store(ind)
        index = ind[1]
        for d=2:ndimsA
            index += (ind[d]-1) * strides[d-1]
        end
        X[storeind] = A[index]
        storeind += 1
    end

    cartesian_map(store, I)
    return X
end

## Indexing: assign ##

assign(t::Tensor, x, r::Real...) = (t[map(x->convert(Int32,round(x)),r)...] = x)

function assign(A::Vector, x, I::Vector{Index})
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

assign(A::Matrix, x, i::Index, j::Index) = (A[(j-1)*size(A,1) + i] = x)
assign(A::Matrix, x::Tensor, i::Index, j::Index) = (A[(j-1)*size(A,1) + i] = x)

function assign(A::Matrix, x, I::Indices, J::Indices)
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

assign(A::Tensor, x, I0::Index, I::Index...) = assign_scalarND(A,x,I0,I...)
assign(A::Tensor, x::Tensor, I0::Index, I::Index...) =
    assign_scalarND(A,x,I0,I...)

function assign_scalarND(A, x, I0, I...)
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


function assign(A::Tensor, x, I0::Indices, I::Indices...)
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

## Reductions ##

function areduce{T}(f::Function, A::Tensor{T}, region::Region)
    areduce(f, A, region, T)
end

function areduce(f::Function, A::Tensor, region::Region, RType::Type)
    dimsA = size(A)
    ndimsA = length(dimsA)
    dimsR = ntuple(ndimsA, i->(contains(region, i) ? 1 : dimsA[i]))
    R = clone(A, RType, dimsR)
    
    function reduce_one(ind)
        sliceA = ntuple(ndimsA, i->(contains(region, i) ?
                                    Range1(1,dimsA[i]) :
                                    ind[i]))
        R[ind...] = f(A[sliceA...])
    end
    
    cartesian_map(reduce_one, ntuple(ndimsA, i->(Range1(1,dimsR[i]))) )
    return R
end

for f = (`max, `min, `sum, `prod) 
    eval(`function ($f){T}(A::Tensor{T,2}, dim::Region)
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

 max(A::Tensor,       region::Region) = areduce(max,  A, region)
 min(A::Tensor,       region::Region) = areduce(min,  A, region)
 sum(A::Tensor,       region::Region) = areduce(sum,  A, region)
prod(A::Tensor,       region::Region) = areduce(prod, A, region)

for f = (`all, `any, `count)
    eval(`function ($f)(A::Tensor{Bool,2}, dim::Region)
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

all(A::Tensor{Bool}, region::Region) = areduce(all,  A, region)
any(A::Tensor{Bool}, region::Region) = areduce(any,  A, region)
count (A::Tensor{Bool}, region::Region) = areduce(count,  A, region, Int)

function isequal(x::Tensor, y::Tensor)
    if size(x) != size(y)
        return false
    end

    for i=1:numel(x)
        xi=x[i]; yi=y[i]
        if xi!=yi && !isequal(xi, yi)
            return false
        end
    end
    return true
end

for (f, op) = ((`cumsum, `+), (`cumprod, `(.*)) )

    eval(`function ($f)(v::Vector)
            n = length(v)
            c = clone(v, n)
            if n == 0; return c; end

            c[1] = v[1]
            for i=2:n
               c[i] = ($op)(v[i], c[i-1])
            end
            return c
         end)
end

## iteration support for arrays as ranges ##

start(a::Tensor) = 1
next(a::Tensor,i) = (a[i],i+1)
done(a::Tensor,i) = (i > numel(a))
isempty(a::Tensor) = (numel(a) == 0)

## map over arrays ##

map(f, v::Vector) = [ f(v[i]) | i=1:length(v) ]
map(f, M::Matrix) = [ f(M[i,j]) | i=1:size(M,1), j=1:size(M,2) ]

function map(f, A::Tensor)
    F = clone(A, size(A))
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

function transpose(a::Matrix)
    m,n = size(a)
    b = clone(a, n, m)
    for i=1:m, j=1:n
        b[j,i] = a[i,j]
    end
    return b
end

function permute(A::Tensor, perm)
    dimsA = size(A)
    ndimsA = length(dimsA)
    dimsP = ntuple(ndimsA, i->dimsA[perm[i]])
    P = clone(A, dimsP)

    count = 1
    function permute_one(ind)
        P[count] = A[ntuple(ndimsA, i->ind[perm[i]])...]
        count += 1
    end

    cartesian_map(permute_one, ntuple(ndimsA, i->(Range1(1,dimsP[i]))) )
    return P
end

function ipermute(A::Tensor, perm)
    dimsA = size(A)
    ndimsA = length(dimsA)
    dimsP = ntuple(ndimsA, i->dimsA[perm[i]])
    P = clone(A, dimsP)

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


function accumarray{T<:Number}(I::Vector, J::Vector, V::T, m::Size, n::Size)
    A = clone(V, m, n)
    for k=1:length(I)
        A[I[k], J[k]] += V
    end
    return A
end

function accumarray(I::Indices, J::Indices, V::Vector, m::Size, n::Size)
    A = clone(V, m, n)
    for k=1:length(I)
        A[I[k], J[k]] += V[k]
    end
    return A
end

function find(A::Vector)
    nnzA = nnz(A)
    I = zeros(Size, nnzA)
    count = 1
    for i=1:length(A)
        if A[i] != 0
            I[count] = i
            count += 1
        end
    end
    return I
end

function find(A::Matrix)
    nnzA = nnz(A)
    I = zeros(Size, nnzA)
    J = zeros(Size, nnzA)
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

function find(A::Tensor)
    ndimsA = ndims(A)
    nnzA = nnz(A)
    I = ntuple(ndimsA, x->zeros(Size, nnzA))

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
sub2ind(dims) = 1

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
