# This file is a part of Julia. License is MIT: http://julialang.org/license

### Multidimensional iterators
module IteratorsMD

import Base: eltype, length, size, start, done, next, last, getindex, setindex!, linearindexing, min, max, zero, one, isless, eachindex, ndims, iteratorsize
importall ..Base.Operators
import Base: simd_outer_range, simd_inner_length, simd_index, @generated
import Base: @nref, @ncall, @nif, @nexprs, LinearFast, LinearSlow, to_index, AbstractCartesianIndex

export CartesianIndex, CartesianRange

# Traits for linear indexing
linearindexing{A<:BitArray}(::Type{A}) = LinearFast()

# CartesianIndex
immutable CartesianIndex{N} <: AbstractCartesianIndex{N}
    I::NTuple{N,Int}
    CartesianIndex(index::NTuple{N,Integer}) = new(index)
end

CartesianIndex{N}(index::NTuple{N,Integer}) = CartesianIndex{N}(index)
(::Type{CartesianIndex})(index::Integer...) = CartesianIndex(index)
(::Type{CartesianIndex{N}}){N}(index::Integer...) = CartesianIndex{N}(index)
# Allow passing tuples smaller than N
@generated function (::Type{CartesianIndex{N}}){N,M}(index::NTuple{M,Integer})
    M == N && return :(CartesianIndex(index))
    M > N && return :(throw(DimensionMismatch("Cannot create CartesianIndex{$N} from $M indexes")))
    args = [i <= M ? :(index[$i]) : 1 for i = 1:N]
    :(CartesianIndex(tuple($(args...))))
end
# Un-nest passed CartesianIndexes
CartesianIndex(index::Union{Integer, CartesianIndex}...) = CartesianIndex(index)
@generated function CartesianIndex{N}(index::NTuple{N, Union{Integer, CartesianIndex}})
    ex = Expr(:tuple)
    for (i, T) in enumerate(index.parameters)
        if T <: Integer
            push!(ex.args, :(index[$i]))
        else
            push!(ex.args, Expr(:..., :(index[$i].I)))
        end
    end
    :($(Expr(:meta, :inline)); CartesianIndex($ex))
end

# length
length{N}(::CartesianIndex{N})=N
length{N}(::Type{CartesianIndex{N}})=N

# indexing
getindex(index::CartesianIndex, i::Integer) = index.I[i]

# zeros and ones
zero{N}(::CartesianIndex{N}) = zero(CartesianIndex{N})
zero{N}(::Type{CartesianIndex{N}}) = CartesianIndex(ntuple(x -> 0, Val{N}))
one{N}(::CartesianIndex{N}) = one(CartesianIndex{N})
one{N}(::Type{CartesianIndex{N}}) = CartesianIndex(ntuple(x -> 1, Val{N}))

# arithmetic, min/max
for op in (:+, :-, :min, :max)
    @eval begin
        @generated function ($op){N}(index1::CartesianIndex{N}, index2::CartesianIndex{N})
            I = index1
            args = [:($($op)(index1[$d],index2[$d])) for d = 1:N]
            :($I($(args...)))
        end
    end
end

(+){N}(index::CartesianIndex{N}, i::Integer) = CartesianIndex{N}(map(x->x+i, index.I))
(+){N}(i::Integer, index::CartesianIndex{N}) = index+i
(-){N}(index::CartesianIndex{N}, i::Integer) = CartesianIndex{N}(map(x->x-i, index.I))
(-){N}(i::Integer, index::CartesianIndex{N}) = CartesianIndex{N}(map(x->i-x, index.I))

@generated function *{N}(a::Integer, index::CartesianIndex{N})
    I = index
    args = [:(a*index[$d]) for d = 1:N]
    :($I($(args...)))
end
*(index::CartesianIndex,a::Integer)=*(a,index)

# comparison
@inline isless{N}(I1::CartesianIndex{N}, I2::CartesianIndex{N}) = _isless(0, I1.I, I2.I)
@inline function _isless{N}(ret, I1::NTuple{N,Int}, I2::NTuple{N,Int})
    newret = ifelse(ret==0, icmp(I1[N], I2[N]), ret)
    _isless(newret, Base.front(I1), Base.front(I2))
end
_isless(ret, ::Tuple{}, ::Tuple{}) = ifelse(ret==1, true, false)
icmp(a, b) = ifelse(isless(a,b), 1, ifelse(a==b, 0, -1))

# Iteration
immutable CartesianRange{I<:CartesianIndex}
    start::I
    stop::I
end

@generated function CartesianRange{N}(I::CartesianIndex{N})
    startargs = fill(1, N)
    :(CartesianRange($I($(startargs...)), I))
end
CartesianRange(::Tuple{}) = CartesianRange{CartesianIndex{0}}(CartesianIndex{0}(()),CartesianIndex{0}(()))
CartesianRange{N}(sz::NTuple{N,Int}) = CartesianRange(CartesianIndex(sz))
CartesianRange{N}(rngs::NTuple{N,Union{Int,UnitRange{Int}}}) = CartesianRange(CartesianIndex(map(r->first(r), rngs)), CartesianIndex(map(r->last(r), rngs)))

ndims(R::CartesianRange) = length(R.start)
ndims{I<:CartesianIndex}(::Type{CartesianRange{I}}) = length(I)

@generated function eachindex{T,N}(::LinearSlow, A::AbstractArray{T,N})
    startargs = fill(1, N)
    stopargs = [:(size(A,$i)) for i=1:N]
    meta = Expr(:meta, :inline)
    :($meta; CartesianRange(CartesianIndex{$N}($(startargs...)), CartesianIndex{$N}($(stopargs...))))
end

@generated function eachindex(::LinearSlow, A::AbstractArray, B::AbstractArray...)
    K = max(ndims(A), map(ndims, B)...)
    startargs = fill(1, K)
    stopargs = Array{Expr}(K)
    for i = 1:K
        Bargs = [:(size(B[$j],$i)) for j = 1:length(B)]
        stopargs[i] = :(max(size(A,$i),$(Bargs...)))
    end
    meta = Expr(:meta, :inline)
    :($meta; CartesianRange(CartesianIndex{$K}($(startargs...)), CartesianIndex{$K}($(stopargs...))))
end

eltype{I}(::Type{CartesianRange{I}}) = I
iteratorsize{I}(::Type{CartesianRange{I}}) = Base.HasShape()

@generated function start{I<:CartesianIndex}(iter::CartesianRange{I})
    N = length(I)
    cmp = [:(iter.start[$d] > iter.stop[$d]) for d = 1:N]
    extest = Expr(:||, cmp...)
    inc = [d < N ? :(iter.start[$d]) : :(iter.stop[$N]+1) for d = 1:N]
    exstop = :(CartesianIndex{$N}($(inc...)))
    meta = Expr(:meta, :inline)
    quote
        $meta
        $extest ? $exstop : iter.start
    end
end
@generated function next{I<:CartesianIndex}(iter::CartesianRange{I}, state)
    N = length(I)
    meta = Expr(:meta, :inline)
    quote
        $meta
        index=state
        @nif $N d->(index[d] < iter.stop[d]) d->(@nexprs($N, k->(ind_k = ifelse(k>=d, index[k] + (k==d), iter.start[k]))))
        newindex = @ncall $N $I ind
        index, newindex
    end
end
@generated function done{I<:CartesianIndex}(iter::CartesianRange{I}, state)
    N = length(I)
    :(state[$N] > iter.stop[$N])
end

# 0-d cartesian ranges are special-cased to iterate once and only once
start{I<:CartesianIndex{0}}(iter::CartesianRange{I}) = false
next{I<:CartesianIndex{0}}(iter::CartesianRange{I}, state) = iter.start, true
done{I<:CartesianIndex{0}}(iter::CartesianRange{I}, state) = state

@generated function size{I<:CartesianIndex}(iter::CartesianRange{I})
    N = length(I)
    N == 0 && return ()
    args = [:(iter.stop[$i]-iter.start[$i]+1) for i=1:N]
    Expr(:tuple,args...)
end

length(iter::CartesianRange) = prod(size(iter))

last(iter::CartesianRange) = iter.stop

@generated function simd_outer_range{I}(iter::CartesianRange{I})
    N = length(I)
    N == 0 && return :(CartesianRange(CartesianIndex{0}(),CartesianIndex{0}()))
    startargs = [:(iter.start[$i]) for i=2:N]
    stopargs  = [:(iter.stop[$i]) for i=2:N]
    :(CartesianRange(CartesianIndex{$(N-1)}($(startargs...)), CartesianIndex{$(N-1)}($(stopargs...))))
end

simd_inner_length{I<:CartesianIndex{0}}(iter::CartesianRange{I}, ::CartesianIndex) = 1
simd_inner_length(iter::CartesianRange, I::CartesianIndex) = iter.stop[1]-iter.start[1]+1

simd_index{I<:CartesianIndex{0}}(iter::CartesianRange{I}, ::CartesianIndex, I1::Int) = iter.start
@generated function simd_index{N}(iter::CartesianRange, Ilast::CartesianIndex{N}, I1::Int)
    args = [d == 1 ? :(I1+iter.start[1]) : :(Ilast[$(d-1)]) for d = 1:N+1]
    meta = Expr(:meta, :inline)
    :($meta; CartesianIndex{$(N+1)}($(args...)))
end

end  # IteratorsMD

using .IteratorsMD

# Bounds-checking specialization
# Specializing for a fixed number of arguments provides a ~25%
# improvement over the general definitions in abstractarray.jl

# This is annoying, but we must first define logical indexing to avoid ambiguities
_internal_checkbounds(A::AbstractVector, I::AbstractVector{Bool}) = length(A) == length(I) || throw_boundserror(A, I)
_internal_checkbounds(A::AbstractVector, I::AbstractArray{Bool}) = size(A) == size(I) || throw_boundserror(A, I)

for N = 1:5
    args = [:($(Symbol(:I, d))) for d = 1:N]
    targs = [:($(Symbol(:I, d))::Union{Colon,Number,AbstractArray}) for d = 1:N]  # prevent co-opting the CartesianIndex version
    exs = [:(checkbounds(Bool, size(A, $d), $(args[d]))) for d = 1:N]
    cbexpr = exs[1]
    for d = 2:N
        cbexpr = :($(exs[d]) & $cbexpr)
    end
    @eval begin
        function checkbounds(A::AbstractArray, $(args...))
            @_inline_meta
            _internal_checkbounds(A, $(args...))
        end
        function _internal_checkbounds{T}(A::AbstractArray{T,$N}, $(targs...))
            @_inline_meta
            ($cbexpr) || throw_boundserror(A, ($(args...),))
        end
    end
end

# Bounds-checking with CartesianIndex
@inline function checkbounds(::Type{Bool}, ::Tuple{}, I1::CartesianIndex)
    checkbounds(Bool, (), I1.I...)
end
@inline function checkbounds(::Type{Bool}, sz::Tuple{}, I1::CartesianIndex, I...)
    checkbounds(Bool, (), I1.I..., I...)
end
@inline function checkbounds(::Type{Bool}, sz::Dims, I1::CartesianIndex, I...)
    checkbounds(Bool, sz, I1.I..., I...)
end

# Recursively compute the lengths of a list of indices, without dropping scalars
# These need to be inlined for more than 3 indexes
index_lengths(A::AbstractArray, I::Colon) = (length(A),)
@inline index_lengths(A::AbstractArray, I...) = index_lengths_dim(A, 1, I...)
index_lengths_dim(A, dim) = ()
index_lengths_dim(A, dim, ::Colon) = (trailingsize(A, dim),)
@inline index_lengths_dim(A, dim, ::Colon, i, I...) = (size(A, dim), index_lengths_dim(A, dim+1, i, I...)...)
@inline index_lengths_dim(A, dim, ::Real, I...) = (1, index_lengths_dim(A, dim+1, I...)...)
@inline index_lengths_dim{N}(A, dim, ::CartesianIndex{N}, I...) = (1, index_shape_dim(A, dim+N, I...)...)
@inline index_lengths_dim(A, dim, i::AbstractArray, I...) = (length(i), index_lengths_dim(A, dim+1, I...)...)
@inline index_lengths_dim(A, dim, i::AbstractArray{Bool}, I...) = (sum(i), index_lengths_dim(A, dim+1, I...)...)
@inline index_lengths_dim{N}(A, dim, i::AbstractArray{CartesianIndex{N}}, I...) = (length(i), index_lengths_dim(A, dim+N, I...)...)

# shape of array to create for getindex() with indexes I, dropping scalars
index_shape(A::AbstractArray, I::Colon) = (length(A),)
@inline index_shape(A::AbstractArray, I...) = index_shape_dim(A, 1, I...)
index_shape_dim(A, dim, I::Real...) = ()
index_shape_dim(A, dim, ::Colon) = (trailingsize(A, dim),)
@inline index_shape_dim(A, dim, ::Colon, i, I...) = (size(A, dim), index_shape_dim(A, dim+1, i, I...)...)
@inline index_shape_dim(A, dim, ::Real, I...) = (index_shape_dim(A, dim+1, I...)...)
@inline index_shape_dim{N}(A, dim, ::CartesianIndex{N}, I...) = (index_shape_dim(A, dim+N, I...)...)
@inline index_shape_dim(A, dim, i::AbstractArray, I...) = (size(i)..., index_shape_dim(A, dim+1, I...)...)
@inline index_shape_dim(A, dim, i::AbstractArray{Bool}, I...) = (sum(i), index_shape_dim(A, dim+1, I...)...)
@inline index_shape_dim{N}(A, dim, i::AbstractArray{CartesianIndex{N}}, I...) = (size(i)..., index_shape_dim(A, dim+N, I...)...)

### From abstractarray.jl: Internal multidimensional indexing definitions ###
# These are not defined on directly on getindex to avoid
# ambiguities for AbstractArray subtypes. See the note in abstractarray.jl

# Note that it's most efficient to call checkbounds first, and then to_index
@inline function _getindex{T,N}(l::LinearIndexing, A::AbstractArray{T,N}, I::Vararg{Union{Real, AbstractArray, Colon},N})
    @boundscheck checkbounds(A, I...)
    _unsafe_getindex(l, A, I...)
end
# Explicitly allow linear indexing with one non-scalar index
@inline function _getindex(l::LinearIndexing, A::AbstractArray, i::Union{Real, AbstractArray, Colon})
    @boundscheck checkbounds(A, i)
    _unsafe_getindex(l, _maybe_linearize(l, A), i)
end
# But we can speed up LinearSlow arrays by reshaping them to vectors:
_maybe_linearize(::LinearFast, A::AbstractArray) = A
_maybe_linearize(::LinearSlow, A::AbstractVector) = A
_maybe_linearize(::LinearSlow, A::AbstractArray) = reshape(A, length(A))

@inline function _getindex{N}(l::LinearIndexing, A::AbstractArray, I::Vararg{Union{Real, AbstractArray, Colon},N}) # TODO: DEPRECATE FOR #14770
    @boundscheck checkbounds(A, I...)
    _unsafe_getindex(l, reshape(A, Val{N}), I...)
end

@generated function _unsafe_getindex(::LinearIndexing, A::AbstractArray, I::Union{Real, AbstractArray, Colon}...)
    N = length(I)
    quote
        # This is specifically *not* inlined.
        @nexprs $N d->(I_d = to_index(I[d]))
        shape = @ncall $N index_shape A I
        dest = similar(A, shape)
        size(dest) == shape || throw_checksize_error(dest, shape)
        @ncall $N _unsafe_getindex! dest A I
    end
end

# logical indexing optimization - don't use find (within to_index)
function _unsafe_getindex(::LinearIndexing, src::AbstractArray, I::AbstractArray{Bool})
    shape = index_shape(src, I)
    dest = similar(src, shape)
    size(dest) == shape || throw_checksize_error(dest, shape)

    D = eachindex(dest)
    Ds = start(D)
    for (i, s) in zip(eachindex(I), eachindex(src))
        @inbounds Ii = I[i]
        if Ii
            d, Ds = next(D, Ds)
            @inbounds dest[d] = src[s]
        end
    end
    dest
end

# Always index with the exactly indices provided.
@generated function _unsafe_getindex!(dest::AbstractArray, src::AbstractArray, I::Union{Real, AbstractArray, Colon}...)
    N = length(I)
    quote
        $(Expr(:meta, :inline))
        D = eachindex(dest)
        Ds = start(D)
        idxlens = index_lengths(src, I...)
        @nloops $N i d->(1:idxlens[d]) d->(@inbounds j_d = getindex(I[d], i_d)) begin
            d, Ds = next(D, Ds)
            @inbounds dest[d] = @ncall $N getindex src j
        end
        dest
    end
end

@noinline throw_checksize_error(A, sz) = throw(DimensionMismatch("output array is the wrong size; expected $sz, got $(size(A))"))

## setindex! ##
# For multi-element setindex!, we check bounds, convert the indices (to_index),
# and ensure the value to set is either an AbstractArray or a Repeated scalar
# before redispatching to the _unsafe_batchsetindex!
_iterable(v::AbstractArray) = v
_iterable(v) = repeated(v)
@inline function _setindex!{T,N}(l::LinearIndexing, A::AbstractArray{T,N}, x, J::Vararg{Union{Real,AbstractArray,Colon},N})
    @boundscheck checkbounds(A, J...)
    _unsafe_setindex!(l, A, x, J...)
end
@inline function _setindex!(l::LinearIndexing, A::AbstractArray, x, j::Union{Real,AbstractArray,Colon})
    @boundscheck checkbounds(A, j)
    _unsafe_setindex!(l, _maybe_linearize(l, A), x, j)
    A
end
@inline function _setindex!{N}(l::LinearIndexing, A::AbstractArray, x, J::Vararg{Union{Real, AbstractArray, Colon},N}) # TODO: DEPRECATE FOR #14770
    @boundscheck checkbounds(A, J...)
    _unsafe_setindex!(l, reshape(A, Val{N}), x, J...)
    A
end

@inline function _unsafe_setindex!(::LinearIndexing, A::AbstractArray, x, J::Union{Real,AbstractArray,Colon}...)
    _unsafe_batchsetindex!(A, _iterable(x), to_indexes(J...)...)
end

# 1-d logical indexing: override the above to avoid calling find (in to_index)
function _unsafe_setindex!(::LinearIndexing, A::AbstractArray, x, I::AbstractArray{Bool})
    X = _iterable(x)
    Xs = start(X)
    c = 0
    for (iA, i) in zip(eachindex(A), eachindex(I))
        @inbounds Ii = I[i]
        if Ii
            done(X, Xs) && throw_setindex_mismatch(x, c+1)
            (v, Xs) = next(X, Xs)
            @inbounds A[iA] = v
            c += 1
        end
    end
    setindex_shape_check(X, c)
    A
end

@generated function _unsafe_batchsetindex!(A::AbstractArray, X, I::Union{Real,AbstractArray,Colon}...)
    N = length(I)
    quote
        @nexprs $N d->(I_d = I[d])
        idxlens = @ncall $N index_lengths A I
        @ncall $N setindex_shape_check X (d->idxlens[d])
        Xs = start(X)
        @nloops $N i d->(1:idxlens[d]) d->(@inbounds j_d = I_d[i_d]) begin
            v, Xs = next(X, Xs)
            @inbounds @ncall $N setindex! A v j
        end
        A
    end
end

# Cartesian indexing
function cartindex_exprs(indexes, syms)
    exprs = Any[]
    for (i,ind) in enumerate(indexes)
        if ind <: CartesianIndex
            for j = 1:length(ind)
                push!(exprs, :($syms[$i][$j]))
            end
        else
            push!(exprs, :($syms[$i]))
        end
    end
    if isempty(exprs)
        push!(exprs, 1)  # Handle the zero-dimensional case
    end
    exprs
end
@generated function _getindex{T,N}(l::LinearIndexing, A::AbstractArray{T,N}, I::Union{Real,AbstractArray,Colon,CartesianIndex}...)
    :(@_propagate_inbounds_meta; getindex(A, $(cartindex_exprs(I, :I)...)))
end
@generated function _setindex!{T,N}(l::LinearIndexing, A::AbstractArray{T,N}, v, I::Union{Real,AbstractArray,Colon,CartesianIndex}...)
    :(@_propagate_inbounds_meta; setindex!(A, v, $(cartindex_exprs(I, :I)...)))
end


##

@generated function findn{T,N}(A::AbstractArray{T,N})
    quote
        nnzA = countnz(A)
        @nexprs $N d->(I_d = Array{Int}(nnzA))
        k = 1
        @nloops $N i A begin
            @inbounds if (@nref $N A i) != zero(T)
                @nexprs $N d->(I_d[k] = i_d)
                k += 1
            end
        end
        @ntuple $N I
    end
end

for (f, fmod, op) = ((:cummin, :_cummin!, :min), (:cummax, :_cummax!, :max))
    @eval function ($f)(v::AbstractVector)
        n = length(v)
        cur_val = v[1]
        res = similar(v, n)
        res[1] = cur_val
        for i in 2:n
            cur_val = ($op)(v[i], cur_val)
            res[i] = cur_val
        end
        return res
    end

    @eval function ($f)(A::AbstractArray, axis::Integer)
        res = similar(A)
        if size(A, axis) < 1
            return res
        end
        R1 = CartesianRange(size(A)[1:axis-1])
        R2 = CartesianRange(size(A)[axis+1:end])
        ($fmod)(res, A, R1, R2, axis)
    end

    @eval @noinline function ($fmod)(res, A::AbstractArray, R1::CartesianRange, R2::CartesianRange, axis::Integer)
        for I2 in R2
            for I1 in R1
                res[I1, 1, I2] = A[I1, 1, I2]
            end
            for i = 2:size(A, axis)
                for I1 in R1
                    res[I1, i, I2] = ($op)(A[I1, i, I2], res[I1, i-1, I2])
                end
            end
        end
        res
    end

    @eval ($f)(A::AbstractArray) = ($f)(A, 1)
end

 cumsum(A::AbstractArray, axis::Integer=1) =  cumsum!(similar(A, Base._cumsum_type(A)), A, axis)
cumsum!(B, A::AbstractArray) = cumsum!(B, A, 1)
cumprod(A::AbstractArray, axis::Integer=1) = cumprod!(similar(A), A, axis)
cumprod!(B, A) = cumprod!(B, A, 1)

for (f, op) in ((:cumsum!, :+),
                (:cumprod!, :*))
    @eval begin
        @generated function ($f){T,N}(B, A::AbstractArray{T,N}, axis::Integer)
            quote
                if size(B, axis) < 1
                    return B
                end
                size(B) == size(A) || throw(DimensionMismatch("Size of B must match A"))
                if axis > N
                    copy!(B, A)
                    return B
                end
                if axis == 1
                    # We can accumulate to a temporary variable, which allows register usage and will be slightly faster
                    @inbounds @nloops $N i d->(d > 1 ? (1:size(A,d)) : (1:1)) begin
                        tmp = convert(eltype(B), @nref($N, A, i))
                        @nref($N, B, i) = tmp
                        for i_1 = 2:size(A,1)
                            tmp = ($($op))(tmp, @nref($N, A, i))
                            @nref($N, B, i) = tmp
                        end
                    end
                else
                    @nexprs $N d->(isaxis_d = axis == d)
                    # Copy the initial element in each 1d vector along dimension `axis`
                    @inbounds @nloops $N i d->(d == axis ? (1:1) : (1:size(A,d))) @nref($N, B, i) = @nref($N, A, i)
                    # Accumulate
                    @inbounds @nloops $N i d->((1+isaxis_d):size(A, d)) d->(j_d = i_d - isaxis_d) begin
                        @nref($N, B, i) = ($($op))(@nref($N, B, j), @nref($N, A, i))
                    end
                end
                B
            end
        end
    end
end

### from abstractarray.jl

function fill!{T}(A::AbstractArray{T}, x)
    xT = convert(T, x)
    for I in eachindex(A)
        @inbounds A[I] = xT
    end
    A
end

function copy!{T,N}(dest::AbstractArray{T,N}, src::AbstractArray{T,N})
    length(dest) >= length(src) || throw(BoundsError())
    for (Isrc, Idest) in zip(eachindex(src), eachindex(dest))
        @inbounds dest[Idest] = src[Isrc]
    end
    dest
end

### BitArrays

## getindex

# contiguous multidimensional indexing: if the first dimension is a range,
# we can get some performance from using copy_chunks!
@inline function _unsafe_getindex!(X::BitArray, B::BitArray, I0::Union{UnitRange{Int},Colon})
    copy_chunks!(X.chunks, 1, B.chunks, first(I0), index_lengths(B, I0)[1])
    return X
end

# Optimization where the inner dimension is contiguous improves perf dramatically
@generated function _unsafe_getindex!(X::BitArray, B::BitArray, I0::Union{Colon,UnitRange{Int}}, I::Union{Int,UnitRange{Int},Colon}...)
    N = length(I)
    quote
        $(Expr(:meta, :inline))
        @nexprs $N d->(I_d = I[d])

        f0 = first(I0)
        l0 = size(X, 1)

        gap_lst_1 = 0
        @nexprs $N d->(gap_lst_{d+1} = size(X, d+1))
        stride = 1
        ind = f0
        @nexprs $N d->begin
            stride *= size(B, d)
            stride_lst_d = stride
            ind += stride * (first(I_d) - 1)
            gap_lst_{d+1} *= stride
        end

        storeind = 1
        Xc, Bc = X.chunks, B.chunks
        idxlens = @ncall $N index_lengths B I0 d->I[d]
        @nloops($N, i, d->(1:idxlens[d+1]),
                d->nothing, # PRE
                d->(ind += stride_lst_d - gap_lst_d), # POST
                begin # BODY
                    copy_chunks!(Xc, storeind, Bc, ind, l0)
                    storeind += l0
                end)
        return X
    end
end

# in the general multidimensional non-scalar case, can we do about 10% better
# in most cases by manually hoisting the bitarray chunks access out of the loop
# (This should really be handled by the compiler or with an immutable BitArray)
@generated function _unsafe_getindex!(X::BitArray, B::BitArray, I::Union{Int,AbstractArray{Int},Colon}...)
    N = length(I)
    quote
        $(Expr(:meta, :inline))
        stride_1 = 1
        @nexprs $N d->(stride_{d+1} = stride_d*size(B, d))
        $(Symbol(:offset_, N)) = 1
        ind = 0
        Xc, Bc = X.chunks, B.chunks
        idxlens = @ncall $N index_lengths B d->I[d]
        @nloops $N i d->(1:idxlens[d]) d->(@inbounds offset_{d-1} = offset_d + (I[d][i_d]-1)*stride_d) begin
            ind += 1
            unsafe_bitsetindex!(Xc, unsafe_bitgetindex(Bc, offset_0), ind)
        end
        return X
    end
end

## setindex!

# contiguous multidimensional indexing: if the first dimension is a range,
# we can get some performance from using copy_chunks!

@inline function setindex!(B::BitArray, X::Union{BitArray,Array}, I0::Union{Colon,UnitRange{Int}})
    @boundscheck checkbounds(B, I0)
    l0 = index_lengths(B, I0)[1]
    setindex_shape_check(X, l0)
    l0 == 0 && return B
    f0 = first(I0)
    copy_to_bitarray_chunks!(B.chunks, f0, X, 1, l0)
    return B
end

@inline function setindex!(B::BitArray, x, I0::Union{Colon,UnitRange{Int}})
    @boundscheck checkbounds(B, I0)
    y = Bool(x)
    l0 = index_lengths(B, I0)[1]
    l0 == 0 && return B
    f0 = first(I0)
    fill_chunks!(B.chunks, y, f0, l0)
    return B
end

@inline function setindex!(B::BitArray, X::Union{BitArray,Array}, I0::Union{Colon,UnitRange{Int}}, I::Union{Int,UnitRange{Int},Colon}...)
    @boundscheck checkbounds(B, I0, I...)
    _unsafe_setindex!(B, X, I0, I...)
end
@generated function _unsafe_setindex!(B::BitArray, X::Union{BitArray,Array}, I0::Union{Colon,UnitRange{Int}}, I::Union{Int,UnitRange{Int},Colon}...)
    N = length(I)
    rangeexp = [I[d] === Colon ? :(1:size(B, $(d+1))) : :(I[$d]) for d = 1:N]
    quote
        idxlens = @ncall $N index_lengths B I0 d->I[d]
        @ncall $N setindex_shape_check X idxlens[1] d->idxlens[d+1]
        isempty(X) && return B
        f0 = first(I0)
        l0 = idxlens[1]

        gap_lst_1 = 0
        @nexprs $N d->(gap_lst_{d+1} = idxlens[d+1])
        stride = 1
        ind = f0
        @nexprs $N d->begin
            stride *= size(B, d)
            stride_lst_d = stride
            ind += stride * (first(I[d]) - 1)
            gap_lst_{d+1} *= stride
        end

        refind = 1
        Bc = B.chunks
        @nloops($N, i, d->$rangeexp[d],
                d->nothing, # PRE
                d->(ind += stride_lst_d - gap_lst_d), # POST
                begin # BODY
                    copy_to_bitarray_chunks!(Bc, ind, X, refind, l0)
                    refind += l0
                end)

        return B
    end
end

@inline function setindex!(B::BitArray, x, I0::Union{Colon,UnitRange{Int}}, I::Union{Int,UnitRange{Int},Colon}...)
    @boundscheck checkbounds(B, I0, I...)
    _unsafe_setindex!(B, x, I0, I...)
end
@generated function _unsafe_setindex!(B::BitArray, x, I0::Union{Colon,UnitRange{Int}}, I::Union{Int,UnitRange{Int},Colon}...)
    N = length(I)
    rangeexp = [I[d] === Colon ? :(1:size(B, $(d+1))) : :(I[$d]) for d = 1:N]
    quote
        y = Bool(x)
        idxlens = @ncall $N index_lengths B I0 d->I[d]

        f0 = first(I0)
        l0 = idxlens[1]
        l0 == 0 && return B
        @nexprs $N d->(isempty(I[d]) && return B)

        gap_lst_1 = 0
        @nexprs $N d->(gap_lst_{d+1} = idxlens[d+1])
        stride = 1
        ind = f0
        @nexprs $N d->begin
            stride *= size(B, d)
            stride_lst_d = stride
            ind += stride * (first(I[d]) - 1)
            gap_lst_{d+1} *= stride
        end

        @nloops($N, i, d->$rangeexp[d],
                d->nothing, # PRE
                d->(ind += stride_lst_d - gap_lst_d), # POST
                fill_chunks!(B.chunks, y, ind, l0) # BODY
                )

        return B
    end
end

## findn

@generated function findn{N}(B::BitArray{N})
    quote
        nnzB = countnz(B)
        I = ntuple(x->Array{Int}(nnzB), $N)
        if nnzB > 0
            count = 1
            @nloops $N i B begin
                if (@nref $N B i) # TODO: should avoid bounds checking
                    @nexprs $N d->(I[d][count] = i_d)
                    count += 1
                end
            end
        end
        return I
    end
end

## isassigned

@generated function isassigned(B::BitArray, I_0::Int, I::Int...)
    N = length(I)
    quote
        @nexprs $N d->(I_d = I[d])
        stride = 1
        index = I_0
        @nexprs $N d->begin
            l = size(B,d)
            stride *= l
            1 <= I_{d-1} <= l || return false
            index += (I_d - 1) * stride
        end
        return isassigned(B, index)
    end
end

## permutedims

## Permute array dims ##

function permutedims(B::StridedArray, perm)
    dimsB = size(B)
    ndimsB = length(dimsB)
    (ndimsB == length(perm) && isperm(perm)) || throw(ArgumentError("no valid permutation of dimensions"))
    dimsP = ntuple(i->dimsB[perm[i]], ndimsB)::typeof(dimsB)
    P = similar(B, dimsP)
    permutedims!(P, B, perm)
end

function checkdims_perm{TP,TB,N}(P::AbstractArray{TP,N}, B::AbstractArray{TB,N}, perm)
    dimsB = size(B)
    length(perm) == N || throw(ArgumentError("expected permutation of size $N, but length(perm)=$(length(perm))"))
    isperm(perm) || throw(ArgumentError("input is not a permutation"))
    dimsP = size(P)
    for i = 1:length(perm)
        dimsP[i] == dimsB[perm[i]] || throw(DimensionMismatch("destination tensor of incorrect size"))
    end
    nothing
end

for (V, PT, BT) in [((:N,), BitArray, BitArray), ((:T,:N), Array, StridedArray)]
    @eval @generated function permutedims!{$(V...)}(P::$PT{$(V...)}, B::$BT{$(V...)}, perm)
        quote
            checkdims_perm(P, B, perm)

            #calculates all the strides
            strides_1 = 0
            @nexprs $N d->(strides_{d+1} = stride(B, perm[d]))

            #Creates offset, because indexing starts at 1
            offset = 1 - sum(@ntuple $N d->strides_{d+1})

            if isa(B, SubArray)
                offset += first_index(B::SubArray) - 1
                B = B.parent
            end

            ind = 1
            @nexprs 1 d->(counts_{$N+1} = strides_{$N+1}) # a trick to set counts_($N+1)
            @nloops($N, i, P,
                    d->(counts_d = strides_d), # PRE
                    d->(counts_{d+1} += strides_{d+1}), # POST
                    begin # BODY
                        sumc = sum(@ntuple $N d->counts_{d+1})
                        @inbounds P[ind] = B[sumc+offset]
                        ind += 1
                    end)

            return P
        end
    end
end

## unique across dim

# TODO: this doesn't fit into the new hashing scheme in any obvious way

immutable Prehashed
    hash::UInt
end
hash(x::Prehashed) = x.hash

"""
    unique(itr[, dim])

Returns an array containing only the unique elements of the iterable `itr`, in
the order that the first of each set of equivalent elements originally appears.
If `dim` is specified, returns unique regions of the array `itr` along `dim`.
"""
@generated function unique{T,N}(A::AbstractArray{T,N}, dim::Int)
    quote
        1 <= dim <= $N || return copy(A)
        hashes = zeros(UInt, size(A, dim))

        # Compute hash for each row
        k = 0
        @nloops $N i A d->(if d == dim; k = i_d; end) begin
            @inbounds hashes[k] = hash(hashes[k], hash((@nref $N A i)))
        end

        # Collect index of first row for each hash
        uniquerow = Array{Int}(size(A, dim))
        firstrow = Dict{Prehashed,Int}()
        for k = 1:size(A, dim)   # fixme (iter): use `eachindex(A, dim)` after #15459 is implemented
            uniquerow[k] = get!(firstrow, Prehashed(hashes[k]), k)
        end
        uniquerows = collect(values(firstrow))

        # Check for collisions
        collided = falses(size(A, dim))
        @inbounds begin
            @nloops $N i A d->(if d == dim
                k = i_d
                j_d = uniquerow[k]
            else
                j_d = i_d
            end) begin
                if (@nref $N A j) != (@nref $N A i)
                    collided[k] = true
                end
            end
        end

        if any(collided)
            nowcollided = BitArray(size(A, dim))
            while any(collided)
                # Collect index of first row for each collided hash
                empty!(firstrow)
                for j = 1:size(A, dim)  # fixme (iter): use `eachindex(A, dim)` after #15459 is implemented
                    collided[j] || continue
                    uniquerow[j] = get!(firstrow, Prehashed(hashes[j]), j)
                end
                for v in values(firstrow)
                    push!(uniquerows, v)
                end

                # Check for collisions
                fill!(nowcollided, false)
                @nloops $N i A d->begin
                    if d == dim
                        k = i_d
                        j_d = uniquerow[k]
                        (!collided[k] || j_d == k) && continue
                    else
                        j_d = i_d
                    end
                end begin
                    if (@nref $N A j) != (@nref $N A i)
                        nowcollided[k] = true
                    end
                end
                (collided, nowcollided) = (nowcollided, collided)
            end
        end

        @nref $N A d->d == dim ? sort!(uniquerows) : (1:size(A, d))
    end
end
