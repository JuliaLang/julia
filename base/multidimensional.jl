# This file is a part of Julia. License is MIT: http://julialang.org/license

### Multidimensional iterators
module IteratorsMD

import Base: eltype, length, start, done, next, last, getindex, setindex!, linearindexing, min, max, eachindex, ndims
import Base: simd_outer_range, simd_inner_length, simd_index, @generated
import Base: @nref, @ncall, @nif, @nexprs, LinearFast, LinearSlow, to_index

export CartesianIndex, CartesianRange

# Traits for linear indexing
linearindexing{A<:BitArray}(::Type{A}) = LinearFast()

# CartesianIndex
abstract CartesianIndex{N}

@generated function Base.call{N}(::Type{CartesianIndex},index::NTuple{N,Integer})
    indextype = gen_cartesian(N)
    return Expr(:call,indextype,[:(to_index(index[$i])) for i=1:N]...)
end
@generated function Base.call{N}(::Type{CartesianIndex{N}},index::Integer...)
    length(index) == N && return :(CartesianIndex(index))
    length(index) > N && throw(DimensionMismatch("Cannot create CartesianIndex{$N} from $(length(index)) indexes"))
    args = [i <= length(index) ? :(index[$i]) : 1 for i = 1:N]
    :(CartesianIndex(tuple($(args...))))
end
Base.call{M,N}(::Type{CartesianIndex{N}},index::NTuple{M,Integer}) = CartesianIndex{N}(index...)

let implemented = IntSet()
    global gen_cartesian
    function gen_cartesian(N::Int)
        # Create the types
        indextype = symbol("CartesianIndex_$N")
        if !in(N,implemented)
            fnames = [symbol("I_$i") for i = 1:N]
            fields = [Expr(:(::), fnames[i], :Int) for i = 1:N]
            extype = Expr(:type, false, Expr(:(<:), indextype, Expr(:curly, :CartesianIndex, N)), Expr(:block, fields...))
            eval(extype)
            argsleft = [Expr(:(::), fnames[i], :Integer) for i = 1:N]
            argsright = [Expr(:call,:to_index,fnames[i]) for i=1:N]
            exconstructor = Expr(:(=),Expr(:call,:(Base.call),:(::Type{CartesianIndex{$N}}),argsleft...),Expr(:call,indextype,argsright...))
            eval(exconstructor)
            push!(implemented,N)
        end
        return indextype
    end
end

# length
length{N}(::CartesianIndex{N})=N
length{N}(::Type{CartesianIndex{N}})=N
length{I<:CartesianIndex}(::Type{I})=length(super(I))

# indexing
getindex(index::CartesianIndex, i::Integer) = getfield(index, i)::Int

@generated function getindex{N}(A::Array, index::CartesianIndex{N})
    :(Base.arrayref(A, $(cartindex_exprs((index,), (:index,))...)))
end
@generated function getindex{N}(A::Array, i::Integer, index::CartesianIndex{N})
    :(Base.arrayref(A, $(cartindex_exprs((i, index), (:i, :index))...)))
end
@generated function getindex{M,N}(A::Array, index1::CartesianIndex{M}, i::Integer, index2::CartesianIndex{N})
    :(Base.arrayref(A, $(cartindex_exprs((index1, i, index2), (:index1, :i, :index2))...)))
end
@generated function setindex!{T,N}(A::Array{T}, v, index::CartesianIndex{N})
    :(Base.arrayset(A, convert($T,v), $(cartindex_exprs((index,), (:index,))...)))
end
@generated function setindex!{T,N}(A::Array{T}, v, i::Integer, index::CartesianIndex{N})
    :(Base.arrayset(A, convert($T,v), $(cartindex_exprs((i, index), (:i, :index))...)))
end
@generated function setindex!{T,M,N}(A::Array{T}, v, index1::CartesianIndex{M}, i::Integer, index2::CartesianIndex{N})
    :(Base.arrayset(A, convert($T,v), $(cartindex_exprs((index1, i, index2), (:index1, :i, :index2))...)))
end

@generated function getindex{N}(A::AbstractArray, index::CartesianIndex{N})
    :(getindex(A, $(cartindex_exprs((index,), (:index,))...)))
end
@generated function getindex{N}(A::AbstractArray, i::Integer, index::CartesianIndex{N})
    :(getindex(A, $(cartindex_exprs((i, index), (:i, :index))...)))
end
@generated function setindex!{T,N}(A::AbstractArray{T}, v, index::CartesianIndex{N})
    :(setindex!(A, v, $(cartindex_exprs((index,), (:index,))...)))
end
@generated function setindex!{T,N}(A::AbstractArray{T}, v, i::Integer, index::CartesianIndex{N})
    :(setindex!(A, v, $(cartindex_exprs((i, index), (:i, :index))...)))
end
for AT in (AbstractVector, AbstractMatrix, AbstractArray)  # nix ambiguity warning
    @eval begin
        @generated function getindex{M,N}(A::$AT, index1::CartesianIndex{M}, i::Integer, index2::CartesianIndex{N})
            :(getindex(A, $(cartindex_exprs((index1, i, index2), (:index1, :i, :index2))...)))
        end
        @generated function setindex!{M,N}(A::$AT, v, index1::CartesianIndex{M}, i::Integer, index2::CartesianIndex{N})
            :(setindex!(A, v, $(cartindex_exprs((index1, i, index2), (:index1, :i, :index2))...)))
        end
    end
end

function cartindex_exprs(indexes, syms)
    exprs = Any[]
    for (i,ind) in enumerate(indexes)
        if ind <: Number
            push!(exprs, :($(syms[i])))
        else
            for j = 1:length(ind)
                push!(exprs, :($(syms[i])[$j]))
            end
        end
    end
    if isempty(exprs)
        push!(exprs, 1)  # Handle the zero-dimensional case
    end
    exprs
end

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

@generated function *{N}(a::Integer, index::CartesianIndex{N})
    I = index
    args = [:(a*index[$d]) for d = 1:N]
    :($I($(args...)))
end
*(index::CartesianIndex,a::Integer)=*(a,index)

# Iteration
immutable CartesianRange{I<:CartesianIndex}
    start::I
    stop::I
end

@generated function CartesianRange{N}(I::CartesianIndex{N})
    startargs = fill(1, N)
    :(CartesianRange($I($(startargs...)), I))
end
CartesianRange{N}(sz::NTuple{N,Int}) = CartesianRange(CartesianIndex(sz))

ndims(R::CartesianRange) = length(R.start)

@generated function eachindex{T,N}(::LinearSlow, A::AbstractArray{T,N})
    startargs = fill(1, N)
    stopargs = [:(size(A,$i)) for i=1:N]
    meta = Expr(:meta, :inline)
    :($meta; CartesianRange(CartesianIndex{$N}($(startargs...)), CartesianIndex{$N}($(stopargs...))))
end

@generated function eachindex{S,T,M,N}(::LinearSlow, A::AbstractArray{S,M}, B::AbstractArray{T,N})
    K = max(M,N)
    startargs = fill(1, K)
    stopargs = [:(max(size(A,$i),size(B,$i))) for i=1:K]
    meta = Expr(:meta, :inline)
    :($meta; CartesianRange(CartesianIndex{$K}($(startargs...)), CartesianIndex{$K}($(stopargs...))))
end

eltype{I}(::Type{CartesianRange{I}}) = I
eltype{I}(::CartesianRange{I}) = I

@generated function start{I<:CartesianIndex}(iter::CartesianRange{I})
    N = length(I)
    cmp = [:(iter.start[$d] > iter.stop[$d]) for d = 1:N]
    extest = Expr(:||, cmp...)
    inc = [d < N ? :(iter.start[$d]) : :(iter.stop[$N]+1) for d = 1:N]
    exstop = :(CartesianIndex{$N}($(inc...)))
    quote
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

@generated function length{I<:CartesianIndex}(iter::CartesianRange{I})
    N = length(I)
    N == 0 && return 1
    args = [:(iter.stop[$i]-iter.start[$i]+1) for i=1:N]
    Expr(:call,:*,args...)
end

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


### From array.jl

@generated function checksize(A::AbstractArray, I...)
    N = length(I)
    quote
        @nexprs $N d->(size(A, d) == length(I[d]) || throw(DimensionMismatch("index $d has length $(length(I[d])), but size(A, $d) = $(size(A,d))")))
        nothing
    end
end

@inline unsafe_getindex(v::BitArray, ind::Int) = Base.unsafe_bitgetindex(v.chunks, ind)

@inline unsafe_setindex!{T}(v::Array{T}, x::T, ind::Int) = (@inbounds v[ind] = x; v)
@inline unsafe_setindex!{T}(v::AbstractArray{T}, x::T, ind::Int) = (v[ind] = x; v)
@inline unsafe_setindex!(v::BitArray, x::Bool, ind::Int) = (Base.unsafe_bitsetindex!(v.chunks, x, ind); v)
@inline unsafe_setindex!{T}(v::AbstractArray{T}, x::T, ind::Real) = unsafe_setindex!(v, x, to_index(ind))

# Version that uses cartesian indexing for src
@generated function _getindex!(dest::Array, src::AbstractArray, I::Union(Int,AbstractVector)...)
    N = length(I)
    Isplat = Expr[:(I[$d]) for d = 1:N]
    quote
        checksize(dest, $(Isplat...))
        k = 1
        @nloops $N i dest d->(@inbounds j_d = unsafe_getindex(I[d], i_d)) begin
            @inbounds dest[k] = (@nref $N src j)
            k += 1
        end
        dest
    end
end

# Version that uses linear indexing for src
@generated function _getindex!(dest::Array, src::Array, I::Union(Int,AbstractVector)...)
    N = length(I)
    Isplat = Expr[:(I[$d]) for d = 1:N]
    quote
        checksize(dest, $(Isplat...))
        stride_1 = 1
        @nexprs $N d->(stride_{d+1} = stride_d*size(src,d))
        @nexprs $N d->(offset_d = 1)  # only really need offset_$N = 1
        k = 1
        @nloops $N i dest d->(@inbounds offset_{d-1} = offset_d + (unsafe_getindex(I[d], i_d)-1)*stride_d) begin
            @inbounds dest[k] = src[offset_0]
            k += 1
        end
        dest
    end
end

# It's most efficient to call checkbounds first, then to_index, and finally
# allocate the output. Hence the different variants.
_getindex(A, I::Tuple{Vararg{Union(Int,AbstractVector),}}) =
    _getindex!(similar(A, index_shape(I...)), A, I...)

# The @generated function here is just to work around the performance hit
# of splatting
@generated function getindex(A::Array, I::Union(Real,AbstractVector)...)
    N = length(I)
    Isplat = Expr[:(I[$d]) for d = 1:N]
    quote
        checkbounds(A, $(Isplat...))
        _getindex(A, to_index($(Isplat...)))
    end
end

# Also a safe version of getindex!
@generated function getindex!(dest, src, I::Union(Real,AbstractVector)...)
    N = length(I)
    Isplat = Expr[:(I[$d]) for d = 1:N]
    Jsplat = Expr[:(to_index(I[$d])) for d = 1:N]
    quote
        checkbounds(src, $(Isplat...))
        _getindex!(dest, src, $(Jsplat...))
    end
end


@generated function setindex!(A::Array, x, J::Union(Real,AbstractArray)...)
    N = length(J)
    if x<:AbstractArray
        ex=quote
            X = x
            @ncall $N setindex_shape_check X I
            Xs = start(X)
            @nloops $N i d->(1:length(I_d)) d->(@inbounds offset_{d-1} = offset_d + (unsafe_getindex(I_d, i_d)-1)*stride_d) begin
                v, Xs = next(X, Xs)
                @inbounds A[offset_0] = v
            end
        end
    else
        ex=quote
            @nloops $N i d->(1:length(I_d)) d->(@inbounds offset_{d-1} = offset_d + (unsafe_getindex(I_d, i_d)-1)*stride_d) begin
                @inbounds A[offset_0] = x
            end
        end
    end
    quote
        @nexprs $N d->(J_d = J[d])
        @ncall $N checkbounds A J
        @nexprs $N d->(I_d = to_index(J_d))
        stride_1 = 1
        @nexprs $N d->(stride_{d+1} = stride_d*size(A,d))
        @nexprs $N d->(offset_d = 1)  # really only need offset_$N = 1
        $ex
        A
    end
end


@generated function findn{T,N}(A::AbstractArray{T,N})
    quote
        nnzA = countnz(A)
        @nexprs $N d->(I_d = Array(Int, nnzA))
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


### subarray.jl

function gen_setindex_body(N::Int)
    quote
        Base.Cartesian.@nexprs $N d->(J_d = J[d])
        Base.Cartesian.@ncall $N checkbounds V J
        Base.Cartesian.@nexprs $N d->(I_d = Base.to_index(J_d))
        if !isa(x, AbstractArray)
            Base.Cartesian.@nloops $N i d->(1:length(I_d)) d->(@inbounds j_d = Base.unsafe_getindex(I_d, i_d)) begin
                @inbounds (Base.Cartesian.@nref $N V j) = x
            end
        else
            X = x
            Base.Cartesian.@ncall $N Base.setindex_shape_check X I
            k = 1
            Base.Cartesian.@nloops $N i d->(1:length(I_d)) d->(@inbounds j_d = Base.unsafe_getindex(I_d, i_d)) begin
                @inbounds (Base.Cartesian.@nref $N V j) = X[k]
                k += 1
            end
        end
        V
    end
end

## SubArray index merging
# A view created like V = A[2:3:8, 5:2:17] can later be indexed as V[2:7],
# creating a new 1d view.
# In such cases we have to collapse the 2d space spanned by the ranges.
#
# API:
#    merge_indexes(V, indexes::NTuple, dims::Dims, linindex)
# where dims encodes the trailing sizes of the parent array,
# indexes encodes the view's trailing indexes into the parent array,
# and linindex encodes the subset of these elements that we'll select.
#
# The generic algorithm makes use of div to convert elements
# of linindex into a cartesian index into indexes, looks up
# the corresponding cartesian index into the parent, and then uses
# dims to convert back to a linear index into the parent array.
#
# However, a common case is linindex::Range.
# Since div is slow and in(j::Int, linindex::Range) is fast,
# it can be much faster to generate all possibilities and
# then test whether the corresponding linear index is in linindex.
# One exception occurs when only a small subset of the total
# is desired, in which case we fall back to the div-based algorithm.
#@generated function merge_indexes{T<:Integer}(V, parentindexes::NTuple, parentdims::Dims, linindex::Union(Colon,Range{T}), lindim)
@generated function merge_indexes_in{TT}(V, parentindexes::TT, parentdims::Dims, linindex, lindim)
    N = length(parentindexes.parameters)   # number of parent axes we're merging
    N > 0 || throw(ArgumentError("cannot merge empty indexes"))
    lengthexpr = linindex == Colon ? (:(prod(size(V)[lindim:end]))) : (:(length(linindex)))
    L = symbol(string("Istride_", N+1))  # length of V's trailing dimensions
    quote
        n = $lengthexpr
        Base.Cartesian.@nexprs $N d->(I_d = parentindexes[d])
        pdimoffset = ndims(V.parent) - length(parentdims)
        Istride_1 = 1   # parentindexes strides
        Base.Cartesian.@nexprs $N d->(Istride_{d+1} = Istride_d*dimsize(V.parent, d+pdimoffset, I_d))
        Istridet = Base.Cartesian.@ntuple $(N+1) d->Istride_d
        if n < 0.1*$L   # this has not been carefully tuned
            return merge_indexes_div(V, parentindexes, parentdims, linindex, lindim)
        end
        Pstride_1 = 1   # parent strides
        Base.Cartesian.@nexprs $(N-1) d->(Pstride_{d+1} = Pstride_d*parentdims[d])
        Base.Cartesian.@nexprs $N d->(counter_d = 1) # counter_0 is a linear index into parentindexes
        Base.Cartesian.@nexprs $N d->(offset_d = 1)  # offset_0 is a linear index into parent
        k = 0
        index = Array(Int, n)
        Base.Cartesian.@nloops $N i d->(1:dimsize(V.parent, d+pdimoffset, I_d)) d->(offset_{d-1} = offset_d + (I_d[i_d]-1)*Pstride_d; counter_{d-1} = counter_d + (i_d-1)*Istride_d) begin
            if in(counter_0, linindex)
                index[k+=1] = offset_0
            end
        end
        index
    end
end

# HACK: dispatch seemingly wasn't working properly
function merge_indexes(V, parentindexes::NTuple, parentdims::Dims, linindex, lindim)
    if isa(linindex, Colon) || isa(linindex, Range)
        return merge_indexes_in(V, parentindexes, parentdims, linindex, lindim)
    end
    merge_indexes_div(V, parentindexes, parentdims, linindex, lindim)
end

# This could be written as a regular function, but performance
# will be better using Cartesian macros to avoid the heap and
# an extra loop.
@generated function merge_indexes_div{TT}(V, parentindexes::TT, parentdims::Dims, linindex, lindim)
    N = length(parentindexes.parameters)
    N > 0 || throw(ArgumentError("cannot merge empty indexes"))
    Istride_N = symbol("Istride_$N")
    lengthexpr = :(length(linindex))
    quote
        Base.Cartesian.@nexprs $N d->(I_d = parentindexes[d])
        Pstride_1 = 1   # parent strides
        Base.Cartesian.@nexprs $(N-1) d->(Pstride_{d+1} = Pstride_d*parentdims[d])
        Istride_1 = 1   # parentindexes strides
        pdimoffset = ndims(V.parent) - length(parentdims)
        Base.Cartesian.@nexprs $(N-1) d->(Istride_{d+1} = Istride_d*dimsize(V.parent, d+pdimoffset, I_d))
        n = $lengthexpr
        L = $(Istride_N) * dimsize(V.parent, $N+pdimoffset, parentindexes[end])
        index = Array(Int, n)
        for i = 1:n
            k = linindex[i] # k is the parentindexes-centered linear index
            1 <= k <= L || throw(BoundsError())
            k -= 1
            j = 0  # j will be the new parent-centered linear index
            Base.Cartesian.@nexprs $N d->(d < $N ?
                begin
                    c, k = divrem(k, Istride_{$N-d+1})
                    j += (Base.unsafe_getindex(I_{$N-d+1}, c+1)-1)*Pstride_{$N-d+1}
                end : begin
                    j += Base.unsafe_getindex(I_1, k+1)
                end)
            index[i] = j
        end
        index
    end
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
    samesize = true
    for d = 1:N
        if size(dest,d) != size(src,d)
            samesize = false
            break
        end
    end
    if samesize && linearindexing(dest) == linearindexing(src)
        for I in eachindex(dest)
            @inbounds dest[I] = src[I]
        end
    else
        length(dest) >= length(src) || throw(BoundsError())
        iterdest = eachindex(dest)
        sdest = start(iterdest)
        for Isrc in eachindex(src)
            Idest, sdest = next(iterdest, sdest)
            @inbounds dest[Idest] = src[Isrc]
        end
    end
    dest
end

### BitArrays

## getindex

# general scalar indexing with two or more indices
# (uses linear indexing, which is defined in bitarray.jl)
# (code is duplicated for safe and unsafe versions for performance reasons)

@generated function unsafe_getindex(B::BitArray, I_0::Int, I::Int...)
    N = length(I)
    quote
        stride = 1
        index = I_0
        @nexprs $N d->begin
            stride *= size(B,d)
            index += (I[d] - 1) * stride
        end
        return unsafe_getindex(B, index)
    end
end

@generated function getindex(B::BitArray, I_0::Int, I::Int...)
    N = length(I)
    quote
        stride = 1
        index = I_0
        @nexprs $N d->(I_d = I[d])
        @nexprs $N d->begin
            l = size(B,d)
            stride *= l
            1 <= I_{d-1} <= l || throw(BoundsError())
            index += (I_d - 1) * stride
        end
        return B[index]
    end
end

# contiguous multidimensional indexing: if the first dimension is a range,
# we can get some performance from using copy_chunks!

function unsafe_getindex(B::BitArray, I0::UnitRange{Int})
    X = BitArray(length(I0))
    copy_chunks!(X.chunks, 1, B.chunks, first(I0), length(I0))
    return X
end

function getindex(B::BitArray, I0::UnitRange{Int})
    checkbounds(B, I0)
    return unsafe_getindex(B, I0)
end

getindex{T<:Real}(B::BitArray, I0::UnitRange{T}) = getindex(B, to_index(I0))

@generated function unsafe_getindex(B::BitArray, I0::UnitRange{Int}, I::Union(Int,UnitRange{Int})...)
    N = length(I)
    Isplat = Expr[:(I[$d]) for d = 1:N]
    quote
        @nexprs $N d->(I_d = I[d])
        X = BitArray(index_shape(I0, $(Isplat...)))

        f0 = first(I0)
        l0 = length(I0)

        gap_lst_1 = 0
        @nexprs $N d->(gap_lst_{d+1} = length(I_d))
        stride = 1
        ind = f0
        @nexprs $N d->begin
            stride *= size(B, d)
            stride_lst_d = stride
            ind += stride * (first(I_d) - 1)
            gap_lst_{d+1} *= stride
        end

        storeind = 1
        @nloops($N, i, d->I_d,
                d->nothing, # PRE
                d->(ind += stride_lst_d - gap_lst_d), # POST
                begin # BODY
                    copy_chunks!(X.chunks, storeind, B.chunks, ind, l0)
                    storeind += l0
                end)
        return X
    end
end

# general multidimensional non-scalar indexing

@generated function unsafe_getindex(B::BitArray, I::Union(Int,AbstractVector{Int})...)
    N = length(I)
    Isplat = Expr[:(I[$d]) for d = 1:N]
    quote
        @nexprs $N d->(I_d = I[d])
        X = BitArray(index_shape($(Isplat...)))
        Xc = X.chunks

        stride_1 = 1
        @nexprs $N d->(stride_{d+1} = stride_d * size(B, d))
        @nexprs 1 d->(offset_{$N} = 1)
        ind = 1
        @nloops($N, i, d->I_d,
                d->(offset_{d-1} = offset_d + (i_d-1)*stride_d), # PRE
                begin
                    unsafe_bitsetindex!(Xc, B[offset_0], ind)
                    ind += 1
                end)
        return X
    end
end

# general version with Real (or logical) indexing which dispatches on the appropriate method

@generated function getindex(B::BitArray, I::Union(Real,AbstractVector)...)
    N = length(I)
    Isplat = Expr[:(I[$d]) for d = 1:N]
    Jsplat = Expr[:(to_index(I[$d])) for d = 1:N]
    quote
        checkbounds(B, $(Isplat...))
        return unsafe_getindex(B, $(Jsplat...))
    end
end

## setindex!

# general scalar indexing with two or more indices
# (uses linear indexing, which - in the safe version - performs the final
# bounds check and is defined in bitarray.jl)
# (code is duplicated for safe and unsafe versions for performance reasons)

@generated function unsafe_setindex!(B::BitArray, x::Bool, I_0::Int, I::Int...)
    N = length(I)
    quote
        stride = 1
        index = I_0
        @nexprs $N d->begin
            stride *= size(B,d)
            index += (I[d] - 1) * stride
        end
        unsafe_setindex!(B, x, index)
        return B
    end
end

@generated function setindex!(B::BitArray, x::Bool, I_0::Int, I::Int...)
    N = length(I)
    quote
        stride = 1
        index = I_0
        @nexprs $N d->(I_d = I[d])
        @nexprs $N d->begin
            l = size(B,d)
            stride *= l
            1 <= I_{d-1} <= l || throw(BoundsError())
            index += (I_d - 1) * stride
        end
        B[index] = x
        return B
    end
end

# contiguous multidimensional indexing: if the first dimension is a range,
# we can get some performance from using copy_chunks!

function unsafe_setindex!(B::BitArray, X::BitArray, I0::UnitRange{Int})
    l0 = length(I0)
    l0 == 0 && return B
    f0 = first(I0)
    copy_chunks!(B.chunks, f0, X.chunks, 1, l0)
    return B
end

function unsafe_setindex!(B::BitArray, x::Bool, I0::UnitRange{Int})
    l0 = length(I0)
    l0 == 0 && return B
    f0 = first(I0)
    fill_chunks!(B.chunks, x, f0, l0)
    return B
end

@generated function unsafe_setindex!(B::BitArray, X::BitArray, I0::UnitRange{Int}, I::Union(Int,UnitRange{Int})...)
    N = length(I)
    quote
        length(X) == 0 && return B
        f0 = first(I0)
        l0 = length(I0)

        gap_lst_1 = 0
        @nexprs $N d->(gap_lst_{d+1} = length(I[d]))
        stride = 1
        ind = f0
        @nexprs $N d->begin
            stride *= size(B, d)
            stride_lst_d = stride
            ind += stride * (first(I[d]) - 1)
            gap_lst_{d+1} *= stride
        end

        refind = 1
        @nloops($N, i, d->I[d],
                d->nothing, # PRE
                d->(ind += stride_lst_d - gap_lst_d), # POST
                begin # BODY
                    copy_chunks!(B.chunks, ind, X.chunks, refind, l0)
                    refind += l0
                end)

        return B
    end
end

@generated function unsafe_setindex!(B::BitArray, x::Bool, I0::UnitRange{Int}, I::Union(Int,UnitRange{Int})...)
    N = length(I)
    quote
        f0 = first(I0)
        l0 = length(I0)
        l0 == 0 && return B
        @nexprs $N d->(length(I[d]) == 0 && return B)

        gap_lst_1 = 0
        @nexprs $N d->(gap_lst_{d+1} = length(I[d]))
        stride = 1
        ind = f0
        @nexprs $N d->begin
            stride *= size(B, d)
            stride_lst_d = stride
            ind += stride * (first(I[d]) - 1)
            gap_lst_{d+1} *= stride
        end

        @nloops($N, i, d->I[d],
                d->nothing, # PRE
                d->(ind += stride_lst_d - gap_lst_d), # POST
                fill_chunks!(B.chunks, x, ind, l0) # BODY
                )

        return B
    end
end


# general multidimensional non-scalar indexing

@generated function unsafe_setindex!(B::BitArray, X::AbstractArray, I::Union(Int,AbstractArray{Int})...)
    N = length(I)
    quote
        refind = 1
        @nexprs $N d->(I_d = I[d])
        @nloops $N i d->I_d @inbounds begin
            @ncall $N unsafe_setindex! B convert(Bool,X[refind]) i
            refind += 1
        end
        return B
    end
end

@generated function unsafe_setindex!(B::BitArray, x::Bool, I::Union(Int,AbstractArray{Int})...)
    N = length(I)
    quote
        @nexprs $N d->(I_d = I[d])
        @nloops $N i d->I_d begin
            @ncall $N unsafe_setindex! B x i
        end
        return B
    end
end

# general versions with Real (or logical) indexing which dispatch on the appropriate method

# this one is for disambiguation only
function setindex!(B::BitArray, x, i::Real)
    checkbounds(B, i)
    return unsafe_setindex!(B, convert(Bool,x), to_index(i))
end

@generated function setindex!(B::BitArray, x, I::Union(Real,AbstractArray)...)
    N = length(I)
    quote
        checkbounds(B, I...)
        #return unsafe_setindex!(B, convert(Bool,x), to_index(I...)...) # segfaults! (???)
        @nexprs $N d->(J_d = to_index(I[d]))
        return @ncall $N unsafe_setindex! B convert(Bool,x) J
    end
end


# this one is for disambiguation only
function setindex!(B::BitArray, X::AbstractArray, i::Real)
    checkbounds(B, i)
    j = to_index(i)
    setindex_shape_check(X, j)
    return unsafe_setindex!(B, X, j)
end

@generated function setindex!(B::BitArray, X::AbstractArray, I::Union(Real,AbstractArray)...)
    N = length(I)
    quote
        checkbounds(B, I...)
        @nexprs $N d->(J_d = to_index(I[d]))
        @ncall $N setindex_shape_check X J
        return @ncall $N unsafe_setindex! B X J
    end
end



## findn

@generated function findn{N}(B::BitArray{N})
    quote
        nnzB = countnz(B)
        I = ntuple(x->Array(Int, nnzB), $N)
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

for (V, PT, BT) in [((:N,), BitArray, BitArray), ((:T,:N), Array, StridedArray)]
    @eval @generated function permutedims!{$(V...)}(P::$PT{$(V...)}, B::$BT{$(V...)}, perm)
        quote
            dimsB = size(B)
            length(perm) == N || throw(ArgumentError("expected permutation of size $N, but length(perm)=$(length(perm))"))
            isperm(perm) || throw(ArgumentError("input is not a permutation"))
            dimsP = size(P)
            for i = 1:length(perm)
                dimsP[i] == dimsB[perm[i]] || throw(DimensionMismatch("destination tensor of incorrect size"))
            end

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
        uniquerow = Array(Int, size(A, dim))
        firstrow = Dict{Prehashed,Int}()
        for k = 1:size(A, dim)
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
                for j = 1:size(A, dim)
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
