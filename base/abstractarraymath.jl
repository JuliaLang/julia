# This file is a part of Julia. License is MIT: http://julialang.org/license

 ## Basic functions ##

isinteger(x::AbstractArray) = all(isinteger,x)
isinteger{T<:Integer,n}(x::AbstractArray{T,n}) = true
isreal(x::AbstractArray) = all(isreal,x)
isreal{T<:Real,n}(x::AbstractArray{T,n}) = true
ctranspose(a::AbstractArray) = error("ctranspose not implemented for $(typeof(a)). Consider adding parentheses, e.g. A*(B*C') instead of A*B*C' to avoid explicit calculation of the transposed matrix.")
transpose(a::AbstractArray) = error("transpose not implemented for $(typeof(a)). Consider adding parentheses, e.g. A*(B*C.') instead of A*B*C' to avoid explicit calculation of the transposed matrix.")

## Constructors ##

vec(a::AbstractArray) = reshape(a,length(a))
vec(a::AbstractVector) = a

_sub(::Tuple{}, ::Tuple{}) = ()
_sub(t::Tuple, ::Tuple{}) = t
_sub(t::Tuple, s::Tuple) = _sub(tail(t), tail(s))

function squeeze(A::AbstractArray, dims::Dims)
    for i in 1:length(dims)
        1 <= dims[i] <= ndims(A) || throw(ArgumentError("squeezed dims must be in range 1:ndims(A)"))
        size(A, dims[i]) == 1 || throw(ArgumentError("squeezed dims must all be size 1"))
        for j = 1:i-1
            dims[j] == dims[i] && throw(ArgumentError("squeezed dims must be unique"))
        end
    end
    d = ()
    for i = 1:ndims(A)
        if !in(i, dims)
            d = tuple(d..., size(A, i))
        end
    end
    reshape(A, d::typeof(_sub(size(A), dims)))
end

squeeze(A::AbstractArray, dim::Integer) = squeeze(A, (Int(dim),))


## Unary operators ##

conj{T<:Real}(x::AbstractArray{T}) = x
conj!{T<:Real}(x::AbstractArray{T}) = x

real{T<:Real}(x::AbstractArray{T}) = x
imag{T<:Real}(x::AbstractArray{T}) = zero(x)

+{T<:Number}(x::AbstractArray{T}) = x
*{T<:Number}(x::AbstractArray{T,2}) = x

## Binary arithmetic operators ##

*(A::Number, B::AbstractArray) = A .* B
*(A::AbstractArray, B::Number) = A .* B

/(A::AbstractArray, B::Number) = A ./ B

\(A::Number, B::AbstractArray) = B ./ A

# index A[:,:,...,i,:,:,...] where "i" is in dimension "d"
# TODO: more optimized special cases
slicedim(A::AbstractArray, d::Integer, i) =
    A[[ n==d ? i : (1:size(A,n)) for n in 1:ndims(A) ]...]

function flipdim(A::AbstractVector, d::Integer)
    d > 0 || throw(ArgumentError("dimension to flip must be positive"))
    d == 1 || return copy(A)
    reverse(A)
end

function flipdim(A::AbstractArray, d::Integer)
    nd = ndims(A)
    sd = d > nd ? 1 : size(A, d)
    if sd == 1 || isempty(A)
        return copy(A)
    end
    B = similar(A)
    nnd = 0
    for i = 1:nd
        nnd += Int(size(A,i)==1 || i==d)
    end
    if nnd==nd
        # flip along the only non-singleton dimension
        for i = 1:sd
            B[i] = A[sd+1-i]
        end
        return B
    end
    alli = [ 1:size(B,n) for n in 1:nd ]
    for i = 1:sd
        B[[ n==d ? sd+1-i : alli[n] for n in 1:nd ]...] = slicedim(A, d, i)
    end
    return B
end

circshift(a::AbstractArray, shiftamt::Real) = circshift(a, [Integer(shiftamt)])
function circshift{T,N}(a::AbstractArray{T,N}, shiftamts)
    I = ()
    for i=1:N
        s = size(a,i)
        d = i<=length(shiftamts) ? shiftamts[i] : 0
        I = tuple(I..., d==0 ? [1:s;] : mod([-d:s-1-d;], s).+1)
    end
    a[(I::NTuple{N,Vector{Int}})...]
end

# Uses K-B-N summation
function cumsum_kbn{T<:AbstractFloat}(v::AbstractVector{T})
    n = length(v)
    r = similar(v, n)
    if n == 0; return r; end

    s = r[1] = v[1]
    c = zero(T)
    for i=2:n
        vi = v[i]
        t = s + vi
        if abs(s) >= abs(vi)
            c += ((s-t) + vi)
        else
            c += ((vi-t) + s)
        end
        s = t
        r[i] = s+c
    end
    return r
end

# Uses K-B-N summation
function cumsum_kbn{T<:AbstractFloat}(A::AbstractArray{T}, axis::Integer=1)
    dimsA = size(A)
    ndimsA = ndims(A)
    axis_size = dimsA[axis]
    axis_stride = 1
    for i = 1:(axis-1)
        axis_stride *= size(A,i)
    end

    if axis_size <= 1
        return A
    end

    B = similar(A)
    C = similar(A)

    for i = 1:length(A)
        if div(i-1, axis_stride) % axis_size == 0
            B[i] = A[i]
            C[i] = zero(T)
        else
            s = B[i-axis_stride]
            Ai = A[i]
            B[i] = t = s + Ai
            if abs(s) >= abs(Ai)
                C[i] = C[i-axis_stride] + ((s-t) + Ai)
            else
                C[i] = C[i-axis_stride] + ((Ai-t) + s)
            end
        end
    end

    return B + C
end

## ipermutedims in terms of permutedims ##

function ipermutedims(A::AbstractArray,perm)
    iperm = Array(Int,length(perm))
    for i = 1:length(perm)
        iperm[perm[i]] = i
    end
    return permutedims(A,iperm)
end

## Other array functions ##

function repmat(a::AbstractVecOrMat, m::Int, n::Int=1)
    o, p = size(a,1), size(a,2)
    b = similar(a, o*m, p*n)
    for j=1:n
        d = (j-1)*p+1
        R = d:d+p-1
        for i=1:m
            c = (i-1)*o+1
            b[c:c+o-1, R] = a
        end
    end
    return b
end

function repmat(a::AbstractVector, m::Int)
    o = length(a)
    b = similar(a, o*m)
    for i=1:m
        c = (i-1)*o+1
        b[c:c+o-1] = a
    end
    return b
end

# Generalized repmat
function repeat{T}(A::Array{T};
                   inner::Array{Int} = ones(Int, ndims(A)),
                   outer::Array{Int} = ones(Int, ndims(A)))
    ndims_in = ndims(A)
    length_inner = length(inner)
    length_outer = length(outer)
    ndims_out = max(ndims_in, length_inner, length_outer)

    if length_inner < ndims_in || length_outer < ndims_in
        throw(ArgumentError("inner/outer repetitions must be set for all input dimensions"))
    end

    inner = vcat(inner, ones(Int,ndims_out-length_inner))
    outer = vcat(outer, ones(Int,ndims_out-length_outer))

    size_in = size(A)
    size_out = ntuple(i->inner[i]*size(A,i)*outer[i],ndims_out)::Dims
    inner_size_out = ntuple(i->inner[i]*size(A,i),ndims_out)::Dims

    indices_in = Array(Int, ndims_in)
    indices_out = Array(Int, ndims_out)

    length_out = prod(size_out)
    R = Array(T, size_out)

    for index_out in 1:length_out
        ind2sub!(indices_out, size_out, index_out)
        for t in 1:ndims_in
            # "Project" outer repetitions into inner repetitions
            indices_in[t] = mod1(indices_out[t], inner_size_out[t])
            # Find inner repetitions using flooring division
            indices_in[t] = fld1(indices_in[t], inner[t])
        end
        index_in = sub2ind(size_in, indices_in...)
        R[index_out] = A[index_in]
    end

    return R
end
