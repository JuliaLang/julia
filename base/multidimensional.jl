### Multidimensional iterators
module IteratorsMD

import Base: eltype, length, start, _start, done, next, last, getindex, setindex!, linearindexing, min, max
import Base: simd_outer_range, simd_inner_length, simd_index
import Base: @nref, @ncall, @nif, @nexprs, LinearFast, LinearSlow, to_index

export CartesianIndex, CartesianRange, eachindex

# Traits for linear indexing
linearindexing(::BitArray) = LinearFast()
linearindexing{A<:BitArray}(::Type{A}) = LinearFast()

# CartesianIndex
abstract CartesianIndex{N}

stagedfunction Base.call{N}(::Type{CartesianIndex},index::NTuple{N,Real})
    indextype = gen_cartesian(N)
    return Expr(:call,indextype,[:(to_index(index[$i])) for i=1:N]...)
end
Base.call{N}(::Type{CartesianIndex{N}},index::Real...) = CartesianIndex(index::NTuple{N,Real})

let implemented = IntSet()
    global gen_cartesian
    function gen_cartesian(N::Int)
        # Create the types
        indextype = symbol("CartesianIndex_$N")
        if !in(N,implemented)
            fieldnames = [symbol("I_$i") for i = 1:N]
            fields = [Expr(:(::), fieldnames[i], :Int) for i = 1:N]
            extype = Expr(:type, false, Expr(:(<:), indextype, Expr(:curly, :CartesianIndex, N)), Expr(:block, fields...))
            eval(extype)
            argsleft = [Expr(:(::), fieldnames[i], :Real) for i = 1:N]
            argsright = [Expr(:call,:to_index,fieldnames[i]) for i=1:N]
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

stagedfunction getindex{N}(A::Array, index::CartesianIndex{N})
    N==0 ? :(Base.arrayref(A, 1)) : :(@ncall $N Base.arrayref A d->index[d])
end
stagedfunction setindex!{T,N}(A::Array{T}, v, index::CartesianIndex{N})
    N==0 ? :(Base.arrayset(A, convert($T,v), 1)) : :(@ncall $N Base.arrayset A convert($T,v) d->index[d])
end

stagedfunction getindex{N}(A::AbstractArray, index::CartesianIndex{N})
    :(@nref $N A d->index[d])
end
stagedfunction setindex!{N}(A::AbstractArray, v, index::CartesianIndex{N})
    :((@nref $N A d->index[d]) = v)
end

# arithmetic, min/max
for op in (:+, :-, :min, :max)
    @eval begin
        stagedfunction ($op){N}(index1::CartesianIndex{N}, index2::CartesianIndex{N})
            I = index1
            args = [:($($op)(index1[$d],index2[$d])) for d = 1:N]
            :($I($(args...)))
        end
    end
end

stagedfunction *{N}(a::Integer, index::CartesianIndex{N})
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

stagedfunction CartesianRange{N}(I::CartesianIndex{N})
    startargs = fill(1, N)
    :(CartesianRange($I($(startargs...)), I))
end
CartesianRange{N}(sz::NTuple{N,Int}) = CartesianRange(CartesianIndex(sz))

stagedfunction eachindex{T,N}(A::AbstractArray{T,N})
    startargs = fill(1, N)
    stopargs = [:(size(A,$i)) for i=1:N]
    :(CartesianRange(CartesianIndex{$N}($(startargs...)), CartesianIndex{$N}($(stopargs...))))
end

eltype{I}(::Type{CartesianRange{I}}) = I
eltype{I}(::CartesianRange{I}) = I

stagedfunction start{I}(iter::CartesianRange{I})
    N=length(I)
    finishedex = Expr(:(||), [:(iter.stop[$i] < iter.start[$i]) for i = 1:N]...)
    :(return $finishedex, iter.start)
end

stagedfunction _start{T,N}(A::AbstractArray{T,N}, ::LinearSlow)
    args = fill(1, N)
    :(return isempty(A), CartesianIndex{$N}($(args...)))
end

# Prevent an ambiguity warning
next(R::StepRange, state::(Bool, CartesianIndex{1})) = (index=state[2]; return R[index], (index[1]==length(R), CartesianIndex{1}(index[1]+1)))
next{T}(R::UnitRange{T}, state::(Bool, CartesianIndex{1})) = (index=state[2]; return R[index], (index[1]==length(R), CartesianIndex{1}(index[1]+1)))
done(R::StepRange, state::(Bool, CartesianIndex{1})) = state[1]
done(R::UnitRange, state::(Bool, CartesianIndex{1})) = state[1]

stagedfunction next{T,N}(A::AbstractArray{T,N}, state::(Bool, CartesianIndex{N}))
    I = state[2]
    finishedex = (N==0 ? true : :(newindex[$N] > size(A, $N)))
    meta = Expr(:meta, :inline)
    quote
        $meta
        index=state[2]
        @inbounds v = A[index]
        newindex=@nif $N d->(index[d] < size(A, d)) d->@ncall($N, $I, k->(k>d ? index[k] : k==d ? index[k]+1 : 1))
        finished=$finishedex
        v, (finished,newindex)
    end
end
stagedfunction next{I<:CartesianIndex}(iter::CartesianRange{I}, state::(Bool, I))
    N = length(I)
    finishedex = (N==0 ? true : :(newindex[$N] > iter.stop[$N]))
    meta = Expr(:meta, :inline)
    quote
        $meta
        index=state[2]
        newindex=@nif $N d->(index[d] < iter.stop[d]) d->@ncall($N, $I, k->(k>d ? index[k] : k==d ? index[k]+1 : iter.start[k]))
        finished=$finishedex
        index, (finished,newindex)
    end
end

done{T,N}(A::AbstractArray{T,N}, state::(Bool, CartesianIndex{N})) = state[1]
done{I<:CartesianIndex}(iter::CartesianRange{I}, state::(Bool, I)) = state[1]

stagedfunction length{I<:CartesianIndex}(iter::CartesianRange{I})
    N = length(I)
    N == 0 && return 1
    args = [:(iter.stop[$i]-iter.start[$i]+1) for i=1:N]
    Expr(:call,:*,args...)
end

last(iter::CartesianRange) = iter.stop

stagedfunction simd_outer_range{I}(iter::CartesianRange{I})
    N = length(I)
    N == 0 && return :(CartesianRange(CartesianIndex{0}(),CartesianIndex{0}()))
    startargs = [:(iter.start[$i]) for i=2:N]
    stopargs  = [:(iter.stop[$i]) for i=2:N]
    :(CartesianRange(CartesianIndex{$(N-1)}($(startargs...)), CartesianIndex{$(N-1)}($(stopargs...))))
end

simd_inner_length{I<:CartesianIndex{0}}(iter::CartesianRange{I}, ::CartesianIndex) = 1
simd_inner_length(iter::CartesianRange, I::CartesianIndex) = iter.stop[1]-iter.start[1]+1

simd_index{I<:CartesianIndex{0}}(iter::CartesianRange{I}, ::CartesianIndex, I1::Int) = iter.start
stagedfunction simd_index{N}(iter::CartesianRange, Ilast::CartesianIndex{N}, I1::Int)
    args = [d == 1 ? :(I1+iter.start[1]) : :(Ilast[$(d-1)]) for d = 1:N+1]
    meta = Expr(:meta, :inline)
    :($meta; CartesianIndex{$(N+1)}($(args...)))
end

end  # IteratorsMD

using .IteratorsMD


### From array.jl

@ngenerate N Void function checksize(A::AbstractArray, I::NTuple{N, Any}...)
    @nexprs N d->(size(A, d) == length(I_d) || throw(DimensionMismatch("index $d has length $(length(I_d)), but size(A, $d) = $(size(A,d))")))
    nothing
end

@inline unsafe_getindex(v::BitArray, ind::Int) = Base.unsafe_bitgetindex(v.chunks, ind)

@inline unsafe_setindex!{T}(v::Array{T}, x::T, ind::Int) = (@inbounds v[ind] = x; v)
@inline unsafe_setindex!{T}(v::AbstractArray{T}, x::T, ind::Int) = (v[ind] = x; v)
@inline unsafe_setindex!(v::BitArray, x::Bool, ind::Int) = (Base.unsafe_bitsetindex!(v.chunks, x, ind); v)
@inline unsafe_setindex!{T}(v::AbstractArray{T}, x::T, ind::Real) = unsafe_setindex!(v, x, to_index(ind))

# Version that uses cartesian indexing for src
@ngenerate N typeof(dest) function _getindex!(dest::Array, src::AbstractArray, I::NTuple{N,Union(Int,AbstractVector)}...)
    checksize(dest, I...)
    k = 1
    @nloops N i dest d->(@inbounds j_d = unsafe_getindex(I_d, i_d)) begin
        @inbounds dest[k] = (@nref N src j)
        k += 1
    end
    dest
end

# Version that uses linear indexing for src
@ngenerate N typeof(dest) function _getindex!(dest::Array, src::Array, I::NTuple{N,Union(Int,AbstractVector)}...)
    checksize(dest, I...)
    stride_1 = 1
    @nexprs N d->(stride_{d+1} = stride_d*size(src,d))
    @nexprs N d->(offset_d = 1)  # only really need offset_$N = 1
    k = 1
    @nloops N i dest d->(@inbounds offset_{d-1} = offset_d + (unsafe_getindex(I_d, i_d)-1)*stride_d) begin
        @inbounds dest[k] = src[offset_0]
        k += 1
    end
    dest
end

# It's most efficient to call checkbounds first, then to_index, and finally
# allocate the output. Hence the different variants.
_getindex(A, I::(Union(Int,AbstractVector)...)) =
    _getindex!(similar(A, index_shape(I...)), A, I...)

@nsplat N function getindex(A::Array, I::NTuple{N,Union(Real,AbstractVector)}...)
    checkbounds(A, I...)
    _getindex(A, to_index(I...))
end

# Also a safe version of getindex!
@nsplat N function getindex!(dest, src, I::NTuple{N,Union(Real,AbstractVector)}...)
    checkbounds(src, I...)
    _getindex!(dest, src, to_index(I...)...)
end


@ngenerate N typeof(A) function setindex!(A::Array, x, J::NTuple{N,Union(Real,AbstractArray)}...)
    @ncall N checkbounds A J
    @nexprs N d->(I_d = to_index(J_d))
    stride_1 = 1
    @nexprs N d->(stride_{d+1} = stride_d*size(A,d))
    @nexprs N d->(offset_d = 1)  # really only need offset_$N = 1
    if !isa(x, AbstractArray)
        @nloops N i d->(1:length(I_d)) d->(@inbounds offset_{d-1} = offset_d + (unsafe_getindex(I_d, i_d)-1)*stride_d) begin
            @inbounds A[offset_0] = x
        end
    else
        X = x
        @ncall N setindex_shape_check X I
        # TODO? A variant that can use cartesian indexing for RHS
        k = 1
        @nloops N i d->(1:length(I_d)) d->(@inbounds offset_{d-1} = offset_d + (unsafe_getindex(I_d, i_d)-1)*stride_d) begin
            @inbounds A[offset_0] = X[k]
            k += 1
        end
    end
    A
end


@ngenerate N NTuple{N,Vector{Int}} function findn{T,N}(A::AbstractArray{T,N})
    nnzA = countnz(A)
    @nexprs N d->(I_d = Array(Int, nnzA))
    k = 1
    @nloops N i A begin
        @inbounds if (@nref N A i) != zero(T)
            @nexprs N d->(I_d[k] = i_d)
            k += 1
        end
    end
    @ntuple N I
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
# However, a common case is linindex::UnitRange.
# Since div is slow and in(j::Int, linindex::UnitRange) is fast,
# it can be much faster to generate all possibilities and
# then test whether the corresponding linear index is in linindex.
# One exception occurs when only a small subset of the total
# is desired, in which case we fall back to the div-based algorithm.
stagedfunction merge_indexes(V, indexes::NTuple, dims::Dims, linindex::UnitRange{Int})
    N = length(indexes)
    N > 0 || throw(ArgumentError("cannot merge empty indexes"))
    quote
        n = length(linindex)
        Base.Cartesian.@nexprs $N d->(I_d = indexes[d])
        L = 1
        dimoffset = ndims(V.parent) - length(dims)
        Base.Cartesian.@nexprs $N d->(L *= dimsize(V.parent, d+dimoffset, I_d))
        if n < 0.1L   # this has not been carefully tuned
            return merge_indexes_div(V, indexes, dims, linindex)
        end
        Pstride_1 = 1   # parent strides
        Base.Cartesian.@nexprs $(N-1) d->(Pstride_{d+1} = Pstride_d*dims[d])
        Istride_1 = 1   # indexes strides
        Base.Cartesian.@nexprs $(N-1) d->(Istride_{d+1} = Istride_d*dimsize(V, d+dimoffset, I_d))
        Base.Cartesian.@nexprs $N d->(counter_d = 1) # counter_0 is a linear index into indexes
        Base.Cartesian.@nexprs $N d->(offset_d = 1)  # offset_0 is a linear index into parent
        k = 0
        index = Array(Int, n)
        Base.Cartesian.@nloops $N i d->(1:dimsize(V, d+dimoffset, I_d)) d->(offset_{d-1} = offset_d + (I_d[i_d]-1)*Pstride_d; counter_{d-1} = counter_d + (i_d-1)*Istride_d) begin
            if in(counter_0, linindex)
                index[k+=1] = offset_0
            end
        end
        index
    end
end
merge_indexes(V, indexes::NTuple, dims::Dims, linindex) = merge_indexes_div(V, indexes, dims, linindex)

# This could be written as a regular function, but performance
# will be better using Cartesian macros to avoid the heap and
# an extra loop.
stagedfunction merge_indexes_div(V, indexes::NTuple, dims::Dims, linindex)
    N = length(indexes)
    N > 0 || throw(ArgumentError("cannot merge empty indexes"))
    Istride_N = symbol("Istride_$N")
    quote
        Base.Cartesian.@nexprs $N d->(I_d = indexes[d])
        Pstride_1 = 1   # parent strides
        Base.Cartesian.@nexprs $(N-1) d->(Pstride_{d+1} = Pstride_d*dims[d])
        Istride_1 = 1   # indexes strides
        dimoffset = ndims(V.parent) - length(dims)
        Base.Cartesian.@nexprs $(N-1) d->(Istride_{d+1} = Istride_d*dimsize(V.parent, d+dimoffset, I_d))
        n = length(linindex)
        L = $(Istride_N) * dimsize(V.parent, $N+dimoffset, indexes[end])
        index = Array(Int, n)
        for i = 1:n
            k = linindex[i] # k is the indexes-centered linear index
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
cumprod(A::AbstractArray, axis::Integer=1) = cumprod!(similar(A), A, axis)

for (f, op) in ((:cumsum!, :+),
                (:cumprod!, :*))
    @eval begin
        @ngenerate N typeof(B) function ($f){T,N}(B, A::AbstractArray{T,N}, axis::Integer=1)
            if size(B, axis) < 1
                return B
            end
            size(B) == size(A) || throw(DimensionMismatch("size of B must match A"))
            if axis == 1
                # We can accumulate to a temporary variable, which allows register usage and will be slightly faster
                @inbounds @nloops N i d->(d > 1 ? (1:size(A,d)) : (1:1)) begin
                    tmp = convert(eltype(B), @nref(N, A, i))
                    @nref(N, B, i) = tmp
                    for i_1 = 2:size(A,1)
                        tmp = ($op)(tmp, @nref(N, A, i))
                        @nref(N, B, i) = tmp
                    end
                end
            else
                @nexprs N d->(isaxis_d = axis == d)
                # Copy the initial element in each 1d vector along dimension `axis`
                @inbounds @nloops N i d->(d == axis ? (1:1) : (1:size(A,d))) @nref(N, B, i) = @nref(N, A, i)
                # Accumulate
                @inbounds @nloops N i d->((1+isaxis_d):size(A, d)) d->(j_d = i_d - isaxis_d) begin
                    @nref(N, B, i) = ($op)(@nref(N, B, j), @nref(N, A, i))
                end
            end
            B
        end
    end
end

### from abstractarray.jl

@ngenerate N typeof(A) function fill!{T,N}(A::AbstractArray{T,N}, x)
    xT = convert(T, x)
    @nloops N i A begin
        @inbounds (@nref N A i) = xT
    end
    A
end

@ngenerate N typeof(dest) function copy!{T,N}(dest::AbstractArray{T,N}, src::AbstractArray{T,N})
    if @nall N d->(size(dest,d) == size(src,d))
        @nloops N i dest begin
            @inbounds (@nref N dest i) = (@nref N src i)
        end
    else
        invoke(copy!, (typeof(dest), Any), dest, src)
    end
    dest
end

### BitArrays

## getindex

# general scalar indexing with two or more indices
# (uses linear indexing, which is defined in bitarray.jl)
# (code is duplicated for safe and unsafe versions for performance reasons)

@ngenerate N Bool function unsafe_getindex(B::BitArray, I_0::Int, I::NTuple{N,Int}...)
    stride = 1
    index = I_0
    @nexprs N d->begin
        stride *= size(B,d)
        index += (I_d - 1) * stride
    end
    return unsafe_getindex(B, index)
end

@ngenerate N Bool function getindex(B::BitArray, I_0::Int, I::NTuple{N,Int}...)
    stride = 1
    index = I_0
    @nexprs N d->begin
        l = size(B,d)
        stride *= l
        1 <= I_{d-1} <= l || throw(BoundsError(B, tuple(I_0, @ntuple(N,I)...)))
        index += (I_d - 1) * stride
    end
    1 <= index <= length(B) || throw(BoundsError(B, tuple(I_0, @ntuple(N,I)...)))
    return unsafe_getindex(B, index)
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

@ngenerate N BitArray{length(index_shape(I0, I...))} function unsafe_getindex(B::BitArray, I0::UnitRange{Int}, I::NTuple{N,Union(Int,UnitRange{Int})}...)
    X = BitArray(index_shape(I0, I...))

    f0 = first(I0)
    l0 = length(I0)

    gap_lst_1 = 0
    @nexprs N d->(gap_lst_{d+1} = length(I_d))
    stride = 1
    ind = f0
    @nexprs N d->begin
        stride *= size(B, d)
        stride_lst_d = stride
        ind += stride * (first(I_d) - 1)
        gap_lst_{d+1} *= stride
    end

    storeind = 1
    @nloops(N, i, d->I_d,
        d->nothing, # PRE
        d->(ind += stride_lst_d - gap_lst_d), # POST
        begin # BODY
            copy_chunks!(X.chunks, storeind, B.chunks, ind, l0)
            storeind += l0
        end)
    return X
end

# general multidimensional non-scalar indexing

@ngenerate N BitArray{length(index_shape(I...))} function unsafe_getindex(B::BitArray, I::NTuple{N,Union(Int,AbstractVector{Int})}...)
    X = BitArray(index_shape(I...))
    Xc = X.chunks

    ind = 1
    @nloops N i d->I_d begin
        unsafe_bitsetindex!(Xc, (@ncall N unsafe_getindex B i), ind)
        ind += 1
    end
    return X
end

# general version with Real (or logical) indexing which dispatches on the appropriate method

@ngenerate N BitArray{length(index_shape(I...))} function getindex(B::BitArray, I::NTuple{N,Union(Real,AbstractVector)}...)
    checkbounds(B, I...)
    return unsafe_getindex(B, to_index(I...)...)
end

## setindex!

# general scalar indexing with two or more indices
# (uses linear indexing, which - in the safe version - performs the final
# bounds check and is defined in bitarray.jl)
# (code is duplicated for safe and unsafe versions for performance reasons)

@ngenerate N typeof(B) function unsafe_setindex!(B::BitArray, x::Bool, I_0::Int, I::NTuple{N,Int}...)
    stride = 1
    index = I_0
    @nexprs N d->begin
        stride *= size(B,d)
        index += (I_d - 1) * stride
    end
    unsafe_setindex!(B, x, index)
    return B
end

@ngenerate N typeof(B) function setindex!(B::BitArray, x::Bool, I_0::Int, I::NTuple{N,Int}...)
    stride = 1
    index = I_0
    @nexprs N d->begin
        l = size(B,d)
        stride *= l
        1 <= I_{d-1} <= l || throw(BoundsError(B, tuple(I_0, @ntuple(N,I)...)))
        index += (I_d - 1) * stride
    end
    1 <= index <= length(B) || throw(BoundsError(B, tuple(I_0, @ntuple(N,I)...)))
    unsafe_setindex!(B, x, index)
    return B
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

@ngenerate N typeof(B) function unsafe_setindex!(B::BitArray, X::BitArray, I0::UnitRange{Int}, I::NTuple{N,Union(Int,UnitRange{Int})}...)
    length(X) == 0 && return B
    f0 = first(I0)
    l0 = length(I0)

    gap_lst_1 = 0
    @nexprs N d->(gap_lst_{d+1} = length(I_d))
    stride = 1
    ind = f0
    @nexprs N d->begin
        stride *= size(B, d)
        stride_lst_d = stride
        ind += stride * (first(I_d) - 1)
        gap_lst_{d+1} *= stride
    end

    refind = 1
    @nloops(N, i, d->I_d,
        d->nothing, # PRE
        d->(ind += stride_lst_d - gap_lst_d), # POST
        begin # BODY
            copy_chunks!(B.chunks, ind, X.chunks, refind, l0)
            refind += l0
        end)

    return B
end

@ngenerate N typeof(B) function unsafe_setindex!(B::BitArray, x::Bool, I0::UnitRange{Int}, I::NTuple{N,Union(Int,UnitRange{Int})}...)
    f0 = first(I0)
    l0 = length(I0)
    l0 == 0 && return B
    @nexprs N d->(length(I_d) == 0 && return B)

    gap_lst_1 = 0
    @nexprs N d->(gap_lst_{d+1} = length(I_d))
    stride = 1
    ind = f0
    @nexprs N d->begin
        stride *= size(B, d)
        stride_lst_d = stride
        ind += stride * (first(I_d) - 1)
        gap_lst_{d+1} *= stride
    end

    @nloops(N, i, d->I_d,
        d->nothing, # PRE
        d->(ind += stride_lst_d - gap_lst_d), # POST
        begin # BODY
            fill_chunks!(B.chunks, x, ind, l0)
        end)

    return B
end


# general multidimensional non-scalar indexing

@ngenerate N typeof(B) function unsafe_setindex!(B::BitArray, X::AbstractArray, I::NTuple{N,Union(Int,AbstractArray{Int})}...)
    refind = 1
    @nloops N i d->I_d @inbounds begin
        @ncall N unsafe_setindex! B convert(Bool,X[refind]) i
        refind += 1
    end
    return B
end

@ngenerate N typeof(B) function unsafe_setindex!(B::BitArray, x::Bool, I::NTuple{N,Union(Int,AbstractArray{Int})}...)
    @nloops N i d->I_d begin
        @ncall N unsafe_setindex! B x i
    end
    return B
end

# general versions with Real (or logical) indexing which dispatch on the appropriate method

# this one is for disambiguation only
function setindex!(B::BitArray, x, i::Real)
    checkbounds(B, i)
    return unsafe_setindex!(B, convert(Bool,x), to_index(i))
end

@ngenerate N typeof(B) function setindex!(B::BitArray, x, I::NTuple{N,Union(Real,AbstractArray)}...)
    checkbounds(B, I...)
    #return unsafe_setindex!(B, convert(Bool,x), to_index(I...)...) # segfaults! (???)
    @nexprs N d->(J_d = to_index(I_d))
    return @ncall N unsafe_setindex! B convert(Bool,x) J
end


# this one is for disambiguation only
function setindex!(B::BitArray, X::AbstractArray, i::Real)
    checkbounds(B, i)
    j = to_index(i)
    setindex_shape_check(X, j)
    return unsafe_setindex!(B, X, j)
end

@ngenerate N typeof(B) function setindex!(B::BitArray, X::AbstractArray, I::NTuple{N,Union(Real,AbstractArray)}...)
    checkbounds(B, I...)
    @nexprs N d->(J_d = to_index(I_d))
    @ncall N setindex_shape_check X J
    return @ncall N unsafe_setindex! B X J
end



## findn

@ngenerate N NTuple{N,Vector{Int}} function findn{N}(B::BitArray{N})
    nnzB = countnz(B)
    I = ntuple(N, x->Array(Int, nnzB))
    if nnzB > 0
        count = 1
        @nloops N i B begin
            if (@nref N B i) # TODO: should avoid bounds checking
                @nexprs N d->(I[d][count] = i_d)
                count += 1
            end
        end
    end
    return I
end

## isassigned

@ngenerate N Bool function isassigned(B::BitArray, I_0::Int, I::NTuple{N,Int}...)
    stride = 1
    index = I_0
    @nexprs N d->begin
        l = size(B,d)
        stride *= l
        1 <= I_{d-1} <= l || return false
        index += (I_d - 1) * stride
    end
    return isassigned(B, index)
end

## permutedims

for (V, PT, BT) in [((:N,), BitArray, BitArray), ((:T,:N), Array, StridedArray)]
    @eval @ngenerate N typeof(P) function permutedims!{$(V...)}(P::$PT{$(V...)}, B::$BT{$(V...)}, perm)
        dimsB = size(B)
        length(perm) == N || throw(ArgumentError("expected permutation of size $N, but length(perm)=$(length(perm))"))
        isperm(perm) || throw(ArgumentError("input is not a permutation"))
        dimsP = size(P)
        for i = 1:length(perm)
            dimsP[i] == dimsB[perm[i]] || throw(DimensionMismatch("destination tensor of incorrect size"))
        end

        #calculates all the strides
        strides_1 = 0
        @nexprs N d->(strides_{d+1} = stride(B, perm[d]))

        #Creates offset, because indexing starts at 1
        offset = 1 - sum(@ntuple N d->strides_{d+1})

        if isa(B, SubArray)
            offset += first_index(B::SubArray) - 1
            B = B.parent
        end

        ind = 1
        @nexprs 1 d->(counts_{N+1} = strides_{N+1}) # a trick to set counts_($N+1)
        @nloops(N, i, P,
            d->(counts_d = strides_d), # PRE
            d->(counts_{d+1} += strides_{d+1}), # POST
            begin # BODY
                sumc = sum(@ntuple N d->counts_{d+1})
                @inbounds P[ind] = B[sumc+offset]
                ind += 1
            end)

        return P
    end
end

## unique across dim

# TODO: this doesn't fit into the new hashing scheme in any obvious way

immutable Prehashed
    hash::UInt
end
hash(x::Prehashed) = x.hash

@ngenerate N typeof(A) function unique{T,N}(A::AbstractArray{T,N}, dim::Int)
    1 <= dim <= N || return copy(A)
    hashes = zeros(UInt, size(A, dim))

    # Compute hash for each row
    k = 0
    @nloops N i A d->(if d == dim; k = i_d; end) begin
       @inbounds hashes[k] = hash(hashes[k], hash((@nref N A i)))
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
        @nloops N i A d->(if d == dim
                              k = i_d
                              j_d = uniquerow[k]
                          else
                              j_d = i_d
                          end) begin
            if (@nref N A j) != (@nref N A i)
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
            @nloops N i A d->begin
                                 if d == dim
                                     k = i_d
                                     j_d = uniquerow[k]
                                     (!collided[k] || j_d == k) && continue
                                 else
                                     j_d = i_d
                                 end
                             end begin
                if (@nref N A j) != (@nref N A i)
                    nowcollided[k] = true
                end
            end
            (collided, nowcollided) = (nowcollided, collided)
        end
    end

    @nref N A d->d == dim ? sort!(uniquerows) : (1:size(A, d))
end
