## tensor.j : Functions over tensors, not specialized to specific implementation

## Type aliases for convenience ##

typealias AbstractVector{T} AbstractArray{T,1}
typealias AbstractMatrix{T} AbstractArray{T,2}
typealias Vector{T} Array{T,1}
typealias Matrix{T} Array{T,2}
typealias VecOrMat{T} Union(Vector{T}, Matrix{T})

typealias Indices{T<:Int} Union(Int, AbstractVector{T})
typealias Region Union(Size,Dims)

## Basic functions ##

size(t::AbstractArray, d) = size(t)[d]
ndims{T,n}(::AbstractArray{T,n}) = n
numel(t::AbstractArray) = prod(size(t))
length(v::AbstractVector) = numel(v)
length(t::AbstractArray) = error("length not defined for ", typeof(t))
nnz(a::AbstractArray) = (n = 0; for i=1:numel(a); n += a[i] != 0 ? 1 : 0; end; n)
nnz(a::AbstractArray{Bool}) = (n = 0; for i=1:numel(a); n += a[i] == true ? 1 : 0; end; n)

## Constructors ##

# default arguments to similar()
similar{T}(a::AbstractArray{T})                      = similar(a, T, size(a))
similar   (a::AbstractArray, T::Type)                = similar(a, T, size(a))
similar{T}(a::AbstractArray{T}, dims::Dims)          = similar(a, T, dims)
similar{T}(a::AbstractArray{T}, dims::Size...)       = similar(a, T, dims)
similar   (a::AbstractArray, T::Type, dims::Size...) = similar(a, T, dims)

reshape(a::AbstractArray, dims::Dims) = (b = similar(a, dims);
                                  for i=1:numel(a); b[i] = a[i]; end;
                                  b)
reshape(a::AbstractArray, dims::Size...) = reshape(a, dims)

function fill(A::AbstractArray, x)
    for i = 1:numel(A)
        A[i] = x
    end
    return A
end

function copy_to(dest::AbstractArray, src::AbstractArray)
    for i=1:numel(src)
        dest[i] = copy(src[i])
    end
    return dest
end

copy(a::AbstractArray) = copy_to(similar(a), a)

eye(n::Size) = eye(n, n)
eye(m::Size, n::Size) = (a = zeros(m,n);
                         for i=1:min(m,n); a[i,i]=1; end;
                         a)
one{T}(x::AbstractArray{T,2}) = (m=size(x,1); n=size(x,2);
                          a = zeros(T,size(x));
                          for i=1:min(m,n); a[i,i]=1; end;
                          a)
zero{T}(x::AbstractArray{T,2}) = zeros(T,size(x))

function linspace(start::Real, stop::Real, n::Int)
    (start, stop) = promote(start, stop)
    a = Array(typeof(start), long(n))
    step = (stop-start)/(n-1)
    for i=1:n
        a[i] = start+(i-1)*step
    end
    a
end

linspace(start::Real, stop::Real) = [ i | i=start:stop ]

## Unary operators ##

conj{T <: Real}(x::AbstractArray{T}) = x
real{T <: Real}(x::AbstractArray{T}) = x
imag{T <: Real}(x::AbstractArray{T}) = zero(x)

macro unary_op(f)
    quote

        function ($f)(A::AbstractArray)
            F = similar(A)
            for i=1:numel(A)
                F[i] = ($f)(A[i])
            end
            return F
        end

    end # quote
end # macro

@unary_op (-)
@unary_op (~)
@unary_op (conj)

macro unary_c2r_op(f)
    quote

        function ($f){T}(A::AbstractArray{T})
            S = typeof(($f)(zero(T)))
            F = similar(A, S)
            for i=1:numel(A)
                F[i] = ($f)(A[i])
            end
            return F
        end

    end # quote
end # macro

@unary_c2r_op (real)
@unary_c2r_op (imag)

+{T<:Number}(x::AbstractArray{T}) = x
*{T<:Number}(x::AbstractArray{T}) = x

function !(A::AbstractArray{Bool})
    F = similar(A)
    for i=1:numel(A)
        F[i] = !A[i]
    end
    return F
end

## Binary arithmetic operators ##

*(A::Number, B::AbstractArray) = A .* B
*(A::AbstractArray, B::Number) = A .* B

./(x::AbstractArray, y::AbstractArray) = reshape( [ x[i] ./ y[i] | i=1:numel(x) ], size(x) )
./(x::Number, y::AbstractArray) = reshape( [ x    ./ y[i] | i=1:numel(y) ], size(y) )
./(x::AbstractArray, y::Number) = reshape( [ x[i] ./ y    | i=1:numel(x) ], size(x) )

/(A::Number, B::AbstractArray) = A ./ B
/(A::AbstractArray, B::Number) = A ./ B

\(A::Number, B::AbstractArray) = B ./ A
\(A::AbstractArray, B::Number) = B ./ A

macro binary_arithmetic_op(f)
    quote

        function ($f){S,T}(A::AbstractArray{S}, B::AbstractArray{T})
           F = similar(A, promote_type(S,T))
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

    end # quote
end # macro

@binary_arithmetic_op (+)
@binary_arithmetic_op (-)
@binary_arithmetic_op (.*)
@binary_arithmetic_op (.^)
@binary_arithmetic_op div
@binary_arithmetic_op mod

## promotion to complex ##

function complex{S<:Real,T<:Real}(A::AbstractArray{S}, B::AbstractArray{T})
    F = similar(A, typeof(complex(zero(S),zero(T))))
    for i=1:numel(A)
        F[i] = complex(A[i], B[i])
    end
    return F
end

function complex{T<:Real}(A::Real, B::AbstractArray{T})
    F = similar(B, typeof(complex(A,zero(T))))
    for i=1:numel(B)
        F[i] = complex(A, B[i])
    end
    return F
end

function complex{T<:Real}(A::AbstractArray{T}, B::Real)
    F = similar(A, typeof(complex(zero(T),B)))
    for i=1:numel(A)
        F[i] = complex(A[i], B)
    end
    return F
end

function complex{T<:Real}(A::AbstractArray{T})
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

        function ($f)(A::AbstractArray, B::AbstractArray)
           F = similar(A, Bool)
           for i=1:numel(A)
              F[i] = ($f)(A[i], B[i])
           end
           return F
        end
        function ($f)(A::Number, B::AbstractArray)
           F = similar(B, Bool)
           for i=1:numel(B)
              F[i] = ($f)(A, B[i])
           end
           return F
        end
        function ($f)(A::AbstractArray, B::Number)
           F = similar(A, Bool)
           for i=1:numel(A)
              F[i] = ($f)(A[i], B)
           end
           return F
        end
    end
end

@binary_comparison_op (==)
@binary_comparison_op (!=)
@binary_comparison_op (<)
@binary_comparison_op (>)
@binary_comparison_op (<=)
@binary_comparison_op (>=)

## Binary boolean operators ##

macro binary_boolean_op(f)
    quote

        function ($f)(A::AbstractArray{Bool}, B::AbstractArray{Bool})
           F = similar(A, Bool)
           for i=1:numel(A)
              F[i] = ($f)(A[i], B[i])
           end
           return F
        end
        function ($f)(A::Bool, B::AbstractArray{Bool})
           F = similar(B, Bool)
           for i=1:numel(B)
              F[i] = ($f)(A, B[i])
           end
           return F
        end
        function ($f)(A::AbstractArray{Bool}, B::Bool)
           F = similar(A, Bool)
           for i=1:numel(A)
              F[i] = ($f)(A[i], B)
           end
           return F
        end

    end # quote
end # macro

@binary_boolean_op (&)
@binary_boolean_op (|)
@binary_boolean_op ($)

## code generator for specializing on the number of dimensions ##

function make_loop_nest(vars, ranges, body)
    expr = body
    for i=1:length(vars)
        v = vars[i]
        r = ranges[i]
        expr = quote
            for ($v) = ($r)
                $expr
            end
        end
    end
    expr
end

function gen_cartesian_map(cache, genbody, dims, exargnames, exargs...)
    N = length(dims)
    if !has(cache,N)
        dimargnames = { gensym() | i=1:N }
        ivars = { gensym() | i=1:N }
        body = genbody(ivars)
        fexpr =
        quote
            let _dummy_=nothing
                local _F_
                function _F_($(dimargnames...), $(exargnames...))
                    $make_loop_nest(ivars, dimargnames, body)
                end
                _F_
            end
        end
        f = eval(fexpr)
        cache[N] = f
    else
        f = cache[N]
    end
    return f(dims..., exargs...)
end


## Indexing: ref ##

ref(t::AbstractArray) = t
ref(t::AbstractArray, r::Real...) = ref(t,map(x->long(round(x)),r)...)

ref{T<:Int}(A::AbstractVector, I::AbstractVector{T}) = [ A[i] | i = I ]
ref{T<:Int}(A::AbstractArray{Any,1}, I::AbstractVector{T}) = { A[i] | i = I }

ref{T<:Int}(A::AbstractMatrix, I::Int, J::AbstractVector{T})       = [ A[i,j] | i = I, j = J ]
ref{T<:Int}(A::AbstractMatrix, I::AbstractVector{T}, J::Int)       = [ A[i,j] | i = I, j = J ]
ref{T<:Int}(A::AbstractMatrix, I::AbstractVector{T}, J::AbstractVector{T}) = [ A[i,j] | i = I, j = J ]

function ref(A::AbstractArray, i0::Int, i1::Int)
    A[i0 + size(A,1)*(i1-1)]
end

function ref(A::AbstractArray, i0::Int, i1::Int, i2::Int)
    A[i0 + size(A,1)*((i1-1) + size(A,2)*(i2-1))]
end

function ref(A::AbstractArray, i0::Int, i1::Int, i2::Int, i3::Int)
    A[i0 + size(A,1)*((i1-1) + size(A,2)*((i2-1) + size(A,3)*(i3-1)))]
end

function ref(A::AbstractArray, I::Int...)
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

let ref_cache = nothing
global ref
function ref(A::AbstractArray, I::Indices...)
    X = similar(A, map(length, I))

    if is(ref_cache,nothing)
        ref_cache = HashTable()
    end
    gen_cartesian_map(ref_cache, ivars->:(X[storeind] = A[$(ivars...)];storeind += 1),
                      I,
                      {:A, :X, :storeind},
                      A, X, 1)
    return X
end
end

## Indexing: assign ##

# 1-d indexing is assumed defined on subtypes
assign(t::AbstractArray, x, i::Int) =
    error("assign not defined for ",typeof(t))
assign(t::AbstractArray, x::AbstractArray, i::Int) =
    error("assign not defined for ",typeof(t))

assign(t::AbstractArray, x, r::Real...) = (t[map(x->long(round(x)),r)...] = x)

function assign{T<:Int}(A::AbstractVector, x, I::AbstractVector{T})
    for i=I
        A[i] = x
    end
    return A
end

function assign{T<:Int}(A::AbstractVector, X::AbstractArray, I::AbstractVector{T})
    for i=1:length(I)
        A[I[i]] = X[i]
    end
    return A
end

assign(A::AbstractMatrix, x, i::Int, j::Int) = (A[(j-1)*size(A,1) + i] = x)
assign(A::AbstractMatrix, x::AbstractArray, i::Int, j::Int) = (A[(j-1)*size(A,1) + i] = x)

function assign(A::AbstractMatrix, x, I::Indices, J::Indices)
    for j=J, i=I
        A[i,j] = x
    end
    return A
end

function assign(A::AbstractMatrix, X::AbstractArray, I::Indices, J::Indices)
    count = 1
    for j=J, i=I
        A[i,j] = X[count]
        count += 1
    end
    return A
end

assign(A::AbstractArray, x, I0::Int, I::Int...) = assign_scalarND(A,x,I0,I...)
assign(A::AbstractArray, x::AbstractArray, I0::Int, I::Int...) =
    assign_scalarND(A,x,I0,I...)

assign(A::AbstractArray, x::AbstractArray, i0::Int, i1::Int) = A[i0 + size(A,1)*(i1-1)] = x
assign(A::AbstractArray, x, i0::Int, i1::Int) = A[i0 + size(A,1)*(i1-1)] = x

assign(A::AbstractArray, x, i0::Int, i1::Int, i2::Int) =
    A[i0 + size(A,1)*((i1-1) + size(A,2)*(i2-1))] = x
assign(A::AbstractArray, x::AbstractArray, i0::Int, i1::Int, i2::Int) =
    A[i0 + size(A,1)*((i1-1) + size(A,2)*(i2-1))] = x

assign(A::AbstractArray, x, i0::Int, i1::Int, i2::Int, i3::Int) =
    A[i0 + size(A,1)*((i1-1) + size(A,2)*((i2-1) + size(A,3)*(i3-1)))] = x
assign(A::AbstractArray, x::AbstractArray, i0::Int, i1::Int, i2::Int, i3::Int) =
    A[i0 + size(A,1)*((i1-1) + size(A,2)*((i2-1) + size(A,3)*(i3-1)))] = x

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

let assign_cache = nothing
global assign
function assign(A::AbstractArray, x, I0::Indices, I::Indices...)
    if is(assign_cache,nothing)
        assign_cache = HashTable()
    end
    gen_cartesian_map(assign_cache, ivars->:(A[$(ivars...)] = x),
                      append(tuple(I0), I),
                      {:A, :x},
                      A, x)
    return A
end
end

let assign_cache = nothing
global assign
function assign(A::AbstractArray, X::AbstractArray, I0::Indices, I::Indices...)
    if is(assign_cache,nothing)
        assign_cache = HashTable()
    end
    gen_cartesian_map(assign_cache, ivars->:(A[$(ivars...)] = X[refind];
                                             refind += 1),
                      append(tuple(I0), I),
                      {:A, :X, :refind},
                      A, X, 1)
    return A
end
end

## Reductions ##

function contains(itr, x)
    for y=itr
        if y==x
            return true
        end
    end
    return false
end

contains(s::Number, n::Int) = (s == n)

areduce{T}(f::Function, A::AbstractArray{T}, region::Region) = areduce(f,A,region,T)

let areduce_cache = nothing
# generate the body of the N-d loop to compute a reduction
function gen_areduce_core(ivars, f)
    n = length(ivars)
    # limits and vars for reduction loop
    lo    = { gensym() | i=1:n }
    hi    = { gensym() | i=1:n }
    rvars = { gensym() | i=1:n }
    setlims = { quote
        # each dim of reduction is either 1:sizeA or ivar:ivar
        if contains(region,$i)
            $lo[i] = 1
            $hi[i] = size(A,$i)
        else
            $lo[i] = $hi[i] = $ivars[i]
        end
               end | i=1:n }
    rranges = { :( ($lo[i]):($hi[i]) ) | i=1:n }  # lo:hi for all dims
    quote
        _tot = ($f)()
        $(setlims...)
        $make_loop_nest(rvars, rranges,
                        :(_tot = ($f)(_tot, A[$(rvars...)])))
        R[$(ivars...)] = _tot
    end
end

global areduce
function areduce(f::Function, A::AbstractArray, region::Region, RType::Type)
    dimsA = size(A)
    ndimsA = length(dimsA)
    dimsR = ntuple(ndimsA, i->(contains(region, i) ? 1 : dimsA[i]))
    R = similar(A, RType, dimsR)
    
    if is(areduce_cache,nothing)
        areduce_cache = HashTable()
    end
    gen_cartesian_map(areduce_cache, iv->gen_areduce_core(iv,:f),
                      ntuple(ndimsA, i->(Range1(1,dimsR[i]))),
                      {:f, :A, :region, :R},
                      f, A, region, R)
    return R
end
end

function max{T}(A::AbstractArray{T})
    if subtype(T,Int)
        v = typemin(T)
    else
        v = convert(T,-Inf)
    end
    for i=1:numel(A)
        v = max(v,A[i])
    end
    v
end

function min{T}(A::AbstractArray{T})
    if subtype(T,Int)
        v = typemax(T)
    else
        v = convert(T,Inf)
    end
    for i=1:numel(A)
        v = min(v,A[i])
    end
    v
end

function sum{T}(A::AbstractArray{T})
    v = zero(T)
    for i=1:numel(A)
        v = sum(v,A[i])
    end
    v
end

function prod{T}(A::AbstractArray{T})
    v = one(T)
    for i=1:numel(A)
        v = prod(v,A[i])
    end
    v
end

for f = (:max, :min, :sum, :prod)
    @eval function ($f){T}(A::AbstractArray{T,2}, dim::Region)
       if isinteger(dim)
          if dim == 1
            [ ($f)(A[:,i]) | i=1:size(A, 2) ]
         elseif dim == 2
            [ ($f)(A[i,:]) | i=1:size(A, 1) ]
         end
       elseif dim == (1,2)
            ($f)(A)
       end
    end
end

max (A::AbstractArray, region::Region) = areduce(max,  A, region)
min (A::AbstractArray, region::Region) = areduce(min,  A, region)
sum (A::AbstractArray, region::Region) = areduce(sum,  A, region)
prod(A::AbstractArray, region::Region) = areduce(prod, A, region)

for f = (:all, :any, :count)
    @eval function ($f)(A::AbstractArray{Bool,2}, dim::Region)
        if isinteger(dim)
           if dim == 1
             [ ($f)(A[:,i]) | i=1:size(A, 2) ]
          elseif dim == 2
             [ ($f)(A[i,:]) | i=1:size(A, 1) ]
          end
        elseif dim == (1,2)
             ($f)(A)
        end
    end
end

all(A::AbstractArray{Bool}, region::Region) = areduce(all, A, region)
any(A::AbstractArray{Bool}, region::Region) = areduce(any, A, region)
count(A::AbstractArray{Bool}, region::Region) = areduce(count, A, region, Size)

function isequal(x::AbstractArray, y::AbstractArray)
    if size(x) != size(y)
        return false
    end

    for i=1:numel(x)
        if !isequal(x[i], y[i])
            return false
        end
    end
    return true
end

for (f, op) = ((:cumsum, :+), (:cumprod, :(.*)) )
    @eval function ($f)(v::AbstractVector)
        n = length(v)
        c = similar(v, n)
        if n == 0; return c; end

        c[1] = v[1]
        for i=2:n
           c[i] = ($op)(v[i], c[i-1])
        end
        return c
    end
end

## iteration support for arrays as ranges ##

start(a::AbstractArray) = 1
next(a::AbstractArray,i) = (a[i],i+1)
done(a::AbstractArray,i) = (i > numel(a))
isempty(a::AbstractArray) = (numel(a) == 0)

## map over arrays ##

#map(f, v::AbstractVector) = [ f(v[i]) | i=1:length(v) ]
#map(f, M::AbstractMatrix) = [ f(M[i,j]) | i=1:size(M,1), j=1:size(M,2) ]

function map(f, A::AbstractArray)
    F = similar(A, size(A))
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

reverse(v::AbstractVector) = [ v[length(v)-i+1] | i=1:length(v) ]

transpose(x::AbstractVector)  = [ x[j]         | i=1, j=1:size(x,1) ]
ctranspose(x::AbstractVector) = [ conj(x[j])   | i=1, j=1:size(x,1) ]

transpose(x::AbstractMatrix)  = [ x[j,i]       | i=1:size(x,2), j=1:size(x,1) ]
ctranspose(x::AbstractMatrix) = [ conj(x[j,i]) | i=1:size(x,2), j=1:size(x,1) ]

let permute_cache = nothing

global permute
function permute(A::AbstractArray, perm)
	dimsA = size(A)
    ndimsA = length(dimsA)
    dimsP = ntuple(ndimsA, i->dimsA[perm[i]])
    P = similar(A, dimsP)
    ranges = ntuple(ndimsA, i->(Range1(1,dimsP[i])))


    strides = Array(Int32,0)
    for dim = 1:length(perm)
    	stride = 1
    	for dim_size = 1:(dim-1)
    		stride = stride*dimsA[dim_size]
    	end
    	push(strides, stride)
    end

    #must create offset, because indexing starts at 1
    offset = 0
		for i = strides
			offset+=i
		end
	offset = 1-offset

    function permute_one(ivars)
    s = { (x = ivars[i]; quote total+= $x*(strides[perm[$i]]) end) | i = 1:ndimsA}
		quote
			total=offset
			$(s...)
			#println(total)
			P[count] = A[total]
			count+=1

		end
	end

	if is(permute_cache,nothing)
		permute_cache = HashTable()
	end

	gen_cartesian_map(permute_cache, permute_one, ranges, {:A, :P, :perm, :count, :strides, :offset}, A, P, perm,1, strides, offset)
	return P

end
end

function ipermute(A::AbstractArray,perm)
	iperm = zeros(Int32,length(perm))
	for i = 1:length(perm)
		iperm[perm[i]]= i
	end
	return permute(A,iperm)

end

## Other array functions ##

repmat(a::AbstractMatrix, m::Size, n::Size) = reshape([ a[i,j] | i=1:size(a,1),
                                                         k=1:m,
                                                         j=1:size(a,2),
                                                         l=1:n],
                                              size(a,1)*m,
                                              size(a,2)*n)


accumarray(I::AbstractVector, J::AbstractVector, V) = accumarray (I, J, V, max(I), max(J))


function accumarray{T<:Number}(I::AbstractVector, J::AbstractVector, V::T, m::Size, n::Size)
    A = similar(V, m, n)
    for k=1:length(I)
        A[I[k], J[k]] += V
    end
    return A
end

function accumarray(I::Indices, J::Indices, V::AbstractVector, m::Size, n::Size)
    A = similar(V, m, n)
    for k=1:length(I)
        A[I[k], J[k]] += V[k]
    end
    return A
end

function find{T}(A::AbstractVector{T})
    nnzA = nnz(A)
    I = zeros(Size, nnzA)
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

function find{T}(A::AbstractMatrix{T})
    nnzA = nnz(A)
    I = zeros(Size, nnzA)
    J = zeros(Size, nnzA)
    z = zero(T)
    count = 1
    for i=1:size(A,1), j=1:size(A,2)
        if A[i,j] != z
            I[count] = i
            J[count] = j
            count += 1
        end
    end
    return (I, J)
end


let find_cache = nothing



function find_one(ivars)
	
	s = { quote I[$i][count] = $ivars[i] end | i = 1:length(ivars)}
	quote
		Aind = A[$(ivars...)]
		if Aind != z
			$(s...)
			count +=1
		end
	end

end

global find
function find{T}(A::AbstractArray{T})
	ndimsA = ndims(A)
    nnzA = nnz(A)
    I = ntuple(ndimsA, x->zeros(Size, nnzA))
    ranges = ntuple(ndims(A), d->(1:size(A,d)))

    if is(find_cache,nothing)
        find_cache = HashTable()
    end

    gen_cartesian_map(find_cache, find_one, ranges, {:A, :I, :count, :z}, A,I,1, zero(T))
    return I

end
end

sub2ind(dims, i::Int) = i
sub2ind(dims, i::Int, j::Int) = (j-1)*dims[1] + i
sub2ind(dims) = 1

function sub2ind(dims, I::Int...)
    ndims = length(dims)
    index = I[1]
    stride = 1
    for k=2:ndims
        stride = stride * dims[k-1]
        index += (I[k]-1) * stride
    end
    return index
end

sub2ind(dims, I::AbstractVector...) =
    [ sub2ind(dims, map(X->X[i], I)...) | i=1:length(I[1]) ]

ind2sub(dims::(), ind::Int) = throw(BoundsError())
ind2sub(dims::(Int,), ind::Int) = (ind,)
ind2sub(dims::(Int,Int), ind::Int) =
    (rem(ind-1,dims[1])+1, div(ind-1,dims[1])+1)

function ind2sub(dims, ind::Int)
    ndims = length(dims)
    x = cumprod(dims)

    sub = ()
    for i=(ndims-1):-1:1
        rest = rem(ind-1, x[i]) + 1
        sub = tuple(div(ind - rest, x[i]) + 1, sub...)
        ind = rest
    end
    return tuple(ind, sub...)
end

## subarrays ##

type SubArray{T,N,A<:AbstractArray,I<:(Indices...)} <: AbstractArray{T,N}
    parent::A
    indexes::I
    dims::Dims
    
    SubArray(p::A, i::I) = new(p, i, map(length, i))
end

sub{T,N}(A::AbstractArray{T,N}, i::NTuple{N,Indices}) =
    SubArray{T,N,typeof(A),typeof(i)}(A, i)

sub(A::AbstractArray, i::Indices...) = sub(A, i)

size(s::SubArray) = s.dims
ndims{T,N}(s::SubArray{T,N}) = N

copy(s::SubArray) = copy_to(similar(s.parent, size(s)), s)
similar(s::SubArray, T::Type, dims::Dims) = similar(s.parent, T, dims)

ref(s::SubArray) = s

ref{T}(s::SubArray{T,1}, i::Int) = s.parent[s.indexes[1][i]]

ref{T}(s::SubArray{T,2}, i::Int, j::Int) =
    s.parent[s.indexes[1][i], s.indexes[2][j]]

ref(s::SubArray, is::Int...) = s.parent[map(ref, s.indexes, is)...]

ref(s::SubArray, i::Int) = s[ind2sub(size(s), i)...]
