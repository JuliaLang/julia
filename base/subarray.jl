# This file is a part of Julia. License is MIT: http://julialang.org/license

typealias NonSliceIndex Union{Colon, AbstractVector}
typealias ViewIndex Union{Int, NonSliceIndex}

# LD is the last dimension up through which this object has efficient
# linear indexing. If LD==length(I), then the object itself has efficient
# linear indexing.
immutable SubArray{T,N,P<:AbstractArray,I<:Tuple{Vararg{ViewIndex}},LD} <: AbstractArray{T,N}
    parent::P
    indexes::I
    dims::NTuple{N,Int}
    first_index::Int   # for linear indexing and pointer
    stride1::Int       # used only for linear indexing
end

typealias StridedArray{T,N,A<:DenseArray,I<:Tuple{Vararg{RangeIndex}}} Union{DenseArray{T,N}, SubArray{T,N,A,I}}
typealias StridedVector{T,A<:DenseArray,I<:Tuple{Vararg{RangeIndex}}}  Union{DenseArray{T,1}, SubArray{T,1,A,I}}
typealias StridedMatrix{T,A<:DenseArray,I<:Tuple{Vararg{RangeIndex}}}  Union{DenseArray{T,2}, SubArray{T,2,A,I}}
typealias StridedVecOrMat{T} Union{StridedVector{T}, StridedMatrix{T}}

# Simple utilities
eltype{T,N,P,I}(::Type{SubArray{T,N,P,I}}) = T
size(V::SubArray) = V.dims
# size(V::SubArray, d::Integer) = d <= ndims(V) ? (@inbounds ret = V.dims[d]; ret) : 1
length(V::SubArray) = prod(V.dims)

similar(V::SubArray, T, dims::Dims) = similar(V.parent, T, dims)
copy(V::SubArray) = copy!(similar(V.parent, size(V)), V)

parent(V::SubArray) = V.parent
parentindexes(V::SubArray) = V.indexes

parent(a::AbstractArray) = a
parentindexes(a::AbstractArray) = ntuple(i->1:size(a,i), ndims(a))

## SubArray creation
# Drops singleton dimensions (those indexed with a scalar)
slice(A::AbstractArray, I::ViewIndex...) = _slice(A, to_indexes(I...))
slice(A::AbstractArray, I::Tuple{Vararg{ViewIndex}}) = _slice(A, to_indexes(I...))
function _slice(A, I)
    checkbounds(A, I...)
    slice_unsafe(A, I)
end

# The most complicated part of this is matching the axes between the
# input index tuples (denoted by J), the index tuples that get stored
# in the view (denoted by I), and the overall dimensionality of the
# view.
# The complexities increase when you create a view-of-a-view, because
# then there is also the index tuple of the parent view (denoted IV)
# to consider.
#
# Examples:
#    S1 = sub(A::Matrix, 2, 3:5)    ndims(S1) == length(I) == length(J) == 2
#    S2 = slice(A::Matrix, 2, 3:5)  ndims(S2) == 1, length(I) == length(J) == 2
#    S3 = sub(A::Matrix, 4:17)      ndims(S3) == length(I) == length(J) == 1
#    S4 = sub(S2, 1:2)              ndims(S4) == length(J) == 1, length(I) == 2
# S3 addresses the trailing dimensions of the parent by linear indexing.
# For S4, J[1] corresponds to I[2], because of the slice along
# dimension 1 in S2

slice_unsafe(A::AbstractArray, J) = _slice_unsafe(A, to_indexes(J...))
@generated function _slice_unsafe{T,NP,IndTypes}(A::AbstractArray{T,NP}, J::IndTypes)
    N = 0
    sizeexprs = Array(Any, 0)
    Jp = J.parameters
    for Jindex = 1:length(Jp)
        j = Jp[Jindex]
        if !(j <: Real)
            N += 1
            push!(sizeexprs, dimsizeexpr(j, Jindex, length(Jp), :A, :J))
        end
    end
    dims = :(tuple($(sizeexprs...)))
    LD = subarray_linearindexing_dim(A, J)
    strideexpr = stride1expr(A, Jp, :A, :J, LD)
    exfirst = first_index_expr(:A, :J, length(Jp))
    quote
        $exfirst
        SubArray{$T,$N,$A,$J,$LD}(A, J, $dims, f, $strideexpr)
    end
end

# Conventional style (drop trailing singleton dimensions, keep any
# other singletons by converting them to ranges, e.g., 3:3)
sub(A::AbstractArray, I::ViewIndex...) = _sub(A, I)
sub(A::AbstractArray, I::Tuple{Vararg{ViewIndex}}) = _sub(A, I)
function _sub(A, I)
    checkbounds(A, I...)
    sub_unsafe(A, I)
end

sub_unsafe(A::AbstractArray, J) = _sub_unsafe(A, to_indexes(J...))
@generated function _sub_unsafe{T,NP,IndTypes}(A::AbstractArray{T,NP}, J::IndTypes)
    sizeexprs = Array(Any, 0)
    Itypes = Array(Any, 0)
    Iexprs = Array(Any, 0)
    Jp = J.parameters
    N = length(Jp)
    while N > 0 && Jp[N] <: Real
        N -= 1
    end
    for Jindex = 1:length(Jp)
        j = Jp[Jindex]
        if Jindex <= N
            push!(sizeexprs, dimsizeexpr(j, Jindex, length(Jp), :A, :J))
        end
        if Jindex < N && j <: Real
            push!(Itypes, UnitRange{Int})
            push!(Iexprs, :(Int(J[$Jindex]):Int(J[$Jindex])))
        else
            push!(Itypes, j)
            push!(Iexprs, :(J[$Jindex]))
        end
    end
    dims = :(tuple($(sizeexprs...)))
    Iext = :(tuple($(Iexprs...)))
    It = Tuple{Itypes...}
    LD = subarray_linearindexing_dim(A, J)
    strideexpr = stride1expr(A, Jp, :A, :J, LD)
    exfirst = first_index_expr(:A, :J, length(Itypes))
    quote
        $exfirst
        SubArray{$T,$N,$A,$It,$LD}(A, $Iext, $dims, f, $strideexpr)
    end
end

# Constructing from another SubArray
# This "pops" the old SubArray and creates a more compact one
@generated function _slice_unsafe{T,NV,PV,IV,PLD,IndTypes}(V::SubArray{T,NV,PV,IV,PLD}, J::IndTypes)
    N = 0
    sizeexprs = Array(Any, 0)
    indexexprs = Array(Any, 0)
    Itypes = Array(Any, 0)
    Jp = J.parameters
    # The next two Ints, if nonzero, record information about the place
    # in the index tuple at which trailing dimensions got packed into a
    # single Vector{Int}. For stride1 computation, we need to keep track
    # of whether the index that triggered this had uniform stride.
    #   Iindex_lin is the spot in the resulting index tuple
    #   Jindex_lin is the corresponding spot in the input index tuple
    Iindex_lin = Jindex_lin = 0
    # Linear indexing inference makes use of the following variables:
    #   LD: the last dimension up through which linear indexing is efficient
    #   isLDdone: true if we've quit incrementing LD
    #   die_next_vector: if true, stop incrementing LD on the next
    #      "extended" input index
    #   jprev: holds the previous input index type
    LD, die_next_vector, jprev, isLDdone = 0, false, Void, false  # for linear indexing inference
    Jindex = 0
    IVp = IV.parameters
    for IVindex = 1:length(IVp)
        iv = IVp[IVindex]
        if iv <: Real
            push!(indexexprs, :(V.indexes[$IVindex]))
            push!(Itypes, iv)
            if !isLDdone
                LD += 1
            end
        else
            Jindex += 1
            j = Jp[Jindex]
            if Jindex < length(Jp) || Jindex == NV || IVindex == length(IVp)
                if !(j <: Real)
                    N += 1
                    push!(sizeexprs, dimsizeexpr(j, Jindex, length(Jp), :V, :J))
                end
                push!(indexexprs, :(reindex(V.indexes[$IVindex], J[$Jindex])))
                push!(Itypes, rangetype(iv, j))
            else
                # We have a linear index that spans more than one
                # dimension of the parent
                N += 1
                push!(sizeexprs, dimsizeexpr(j, Jindex, length(Jp), :V, :J))
                push!(indexexprs, :(merge_indexes(V, V.indexes[$IVindex:end], size(V.parent)[$IVindex:end], J[$Jindex], $Jindex)))
                push!(Itypes, Array{Int, 1})
                Iindex_lin = length(Itypes)
                Jindex_lin = Jindex
                break
            end
            if !isLDdone
                if LD < PLD
                    LD += 1
                    jprev, LD, die_next_vector, isdone = nextLD(jprev, j, LD, die_next_vector)
                    isLDdone |= isdone
                else
                    if j <: Real
                        LD += 1
                    else
                        isLDdone = true
                    end
                end
            end
        end
    end
    for Jind = Jindex+1:length(Jp)
        j = Jp[Jind]
        if !(j <: Real)
            N += 1
            push!(sizeexprs, dimsizeexpr(j, Jind, length(Jp), :V, :J))
            isLDdone = true
        elseif !isLDdone
            LD += 1
        end
        push!(indexexprs, :(J[$Jind]))
        push!(Itypes, j)
    end
    Inew = :(tuple($(indexexprs...)))
    dims = :(tuple($(sizeexprs...)))
    It = Tuple{Itypes...}
    LD = max(LD, subarray_linearindexing_dim(PV, It))
    strideexpr = stride1expr(PV, Itypes, :(V.parent), :Inew, LD, :J, Iindex_lin, Jindex_lin)
    exfirst = first_index_expr(:(V.parent), :Inew, length(Itypes))
    quote
        Inew = $Inew
        $exfirst
        SubArray{$T,$N,$PV,$It,$LD}(V.parent, Inew, $dims, f, $strideexpr)
    end
end

@generated function _sub_unsafe{T,NV,PV,IV,PLD,IndTypes}(V::SubArray{T,NV,PV,IV,PLD}, J::IndTypes)
    Jp = J.parameters
    IVp = IV.parameters
    N = length(Jp)
    while N > 0 && Jp[N] <: Real
        N -= 1
    end
    sizeexprs = Array(Any, 0)
    indexexprs = Array(Any, 0)
    Itypes = Array(Any, 0)
    ItypesLD = Array(Any, 0)
    preexprs = Array(Any, 0)
    LD, die_next_vector, jprev, isLDdone = 0, false, Void, false
    Jindex = 0
    for IVindex = 1:length(IVp)
        iv = IVp[IVindex]
        if iv <: Real
            push!(indexexprs, :(V.indexes[$IVindex]))
            push!(Itypes, iv)
            push!(ItypesLD, iv)
            if !isLDdone
                LD += 1
            end
        else
            Jindex += 1
            j = Jp[Jindex]
            if Jindex <= N
                push!(sizeexprs, dimsizeexpr(j, Jindex, length(Jp), :V, :J))
            end
            if Jindex < N && j <: Real
                # convert scalar to a range
                sym = gensym()
                push!(preexprs, :($sym = reindex(V.indexes[$IVindex], Int(J[$Jindex]))))
                push!(indexexprs, :($sym:$sym))
                push!(Itypes, UnitRange{Int})
                push!(ItypesLD, j)
            elseif Jindex < length(Jp) || Jindex == NV || IVindex == length(IVp)
                # simple indexing
                push!(indexexprs, :(reindex(V.indexes[$IVindex], J[$Jindex])))
                push!(Itypes, rangetype(iv, j))
                push!(ItypesLD, Itypes[end])
            else
                # We have a linear index that spans more than one dimension of the parent
                push!(indexexprs, :(merge_indexes(V, V.indexes[$IVindex:end], size(V.parent)[$IVindex:end], J[$Jindex], $Jindex)))
                push!(Itypes, Array{Int, 1})
                push!(ItypesLD, Itypes[end])
                break
            end
            if !isLDdone
                if LD < PLD
                    LD += 1
                    jprev, LD, die_next_vector, isdone = nextLD(jprev, j, LD, die_next_vector)
                    isLDdone |= isdone
                else
                    if j <: Real
                        LD += 1
                    else
                        isLDdone = true
                    end
                end
            end
        end
    end
    for Jind = Jindex+1:length(Jp)
        j = Jp[Jind]
        if Jind <= N
            push!(sizeexprs, dimsizeexpr(j, Jind, length(Jp), :V, :J))
        end
        push!(indexexprs, :(J[$Jind]))
        push!(Itypes, j)
        push!(ItypesLD, Itypes[end])
    end
    Inew = :(tuple($(indexexprs...)))
    dims = :(tuple($(sizeexprs...)))
    It = Tuple{Itypes...}
    LD = max(LD, subarray_linearindexing_dim(PV, It))
    strideexpr = stride1expr(PV, ItypesLD, :(V.parent), :Inew, LD)
    preex = isempty(preexprs) ? nothing : Expr(:block, preexprs...)
    exfirst = first_index_expr(:(V.parent), :Inew, length(Itypes))
    quote
        $preex
        Inew = $Inew
        $exfirst
        SubArray{$T,$N,$PV,$It,$LD}(V.parent, Inew, $dims, f, $strideexpr)
    end
end

function rangetype(T1, T2)
    rt = return_types(getindex, Tuple{T1, T2})
    length(rt) == 1 || error("Can't infer return type")
    rt[1]
end

reindex(a, b) = a[b]
reindex(a::UnitRange, b::UnitRange{Int}) = range(oftype(first(a), first(a)+first(b)-1), length(b))
reindex(a::UnitRange, b::StepRange{Int}) = range(oftype(first(a), first(a)+first(b)-1), step(b), length(b))
reindex(a::StepRange, b::Range{Int}) = range(oftype(first(a), first(a)+(first(b)-1)*step(a)), step(a)*step(b), length(b))
reindex(a, b::Int) = unsafe_getindex(a, b)

dimsizeexpr(Itype, d::Int, len::Int, Asym::Symbol, Isym::Symbol) = :(length($Isym[$d]))
function dimsizeexpr(Itype::Type{Colon}, d::Int, len::Int, Asym::Symbol, Isym::Symbol)
    if d < len
        :(size($Asym, $d))
    else
        :(tailsize($Asym, $d))
    end
end
dimsize(P, d, I) = length(I)
dimsize(P, d::Int, ::Colon) = size(P, d)
dimsize(P, d::Dims, ::Colon) = prod(size(P)[d])
function tailsize(P, d)
    s = 1
    for i = d:ndims(P)
        s *= size(P, i)
    end
    s
end

@generated function linearindexing{T,N,P,I,LD}(A::SubArray{T,N,P,I,LD})
    length(I.parameters) == LD ? (:(LinearFast())) : (:(LinearSlow()))
end
@generated function linearindexing{A<:SubArray}(::Type{A})
    T,N,P,I,LD = A.parameters
    length(I.parameters) == LD ? (:(LinearFast())) : (:(LinearSlow()))
end

getindex(::Colon, i) = to_index(i)
unsafe_getindex(v::Colon, i) = to_index(i)

step(::Colon) = 1
first(::Colon) = 1
isempty(::Colon) = false
in(::Int, ::Colon) = true

## Strides
@generated function strides{T,N,P,I}(V::SubArray{T,N,P,I})
    Ip = I.parameters
    all(x->x<:Union{RangeIndex,Colon}, Ip) || throw(ArgumentError("strides valid only for RangeIndex indexing"))
    strideexprs = Array(Any, N+1)
    strideexprs[1] = 1
    i = 1
    Vdim = 1
    for i = 1:length(Ip)
        if Ip[i] != Int
            strideexprs[Vdim+1] = copy(strideexprs[Vdim])
            strideexprs[Vdim] = :(step(V.indexes[$i])*$(strideexprs[Vdim]))
            Vdim += 1
        end
        strideexprs[Vdim] = :(size(V.parent, $i) * $(strideexprs[Vdim]))
    end
    :(tuple($(strideexprs[1:N]...)))
end

stride(V::SubArray, d::Integer) = d <= ndims(V) ? strides(V)[d] : strides(V)[end] * size(V)[end]

function stride1expr(Atype::Type, Itypes, Aexpr, Isym, LD, Jsym=Isym, Iindex_lin=0, Jindex_lin=0)
    if LD == 0
        return 0
    end
    ex = 1
    for d = 1:min(LD, length(Itypes))
        I = Itypes[d]
        if I <: Real
            ex = :($ex * size($Aexpr, $d))
        else
            if d == Iindex_lin
                ex = :($ex * step_sa($Jsym[$Jindex_lin]))
            else
                ex = :($ex * step($Isym[$d]))
            end
            break
        end
    end
    ex
end

step_sa(arg) = step(arg)
step_sa(::Integer) = 1

# This might be conservative, but better safe than sorry
function iscontiguous{T,N,P,I,LD}(::Type{SubArray{T,N,P,I,LD}})
    Ip = I.parameters
    LD == length(Ip) || return false
    length(Ip) < 1 && return true
    Ip[1] == Colon && return true
    if Ip[1] <: UnitRange
        # It might be stride1 == 1, or this might be because `sub` was
        # used with an integer for the first index
        for j = 2:length(Ip)
            (Ip[j] == Colon || (Ip[j] <: AbstractVector)) && return false
        end
        return true
    end
    false
end
iscontiguous(S::SubArray) = iscontiguous(typeof(S))

first_index(V::SubArray) = first_index(V.parent, V.indexes)
function first_index(P::AbstractArray, indexes::Tuple)
    f = 1
    s = 1
    for i = 1:length(indexes)
        f += (first(indexes[i])-1)*s
        s *= size(P, i)
    end
    f
end

function first_index_expr(Asym, Isym::Symbol, n::Int)
    ex = :(f = s = 1)
    for i = 1:n
        ex = quote
            $ex
            if isempty($Isym[$i])
                f = s = 0
            else
                f += (first($Isym[$i])-1)*s
                s *= size($Asym, $i)
            end
        end
    end
    ex
end

# Detecting whether one can support fast linear indexing
function nextLD(jprev, j, LD, die_next_vector)
    isdone = false
    if j <: Real
        if jprev != Void && !(jprev <: Real)
            die_next_vector = true
        end
    elseif die_next_vector
        LD -= 1
        isdone = true
    elseif j == Colon
    elseif j <: UnitRange
        die_next_vector = true
    elseif j <: Range
        if !(jprev == Void || jprev <: Real)
            LD -= 1
            isdone = true
        end
        die_next_vector = true
    elseif j <: AbstractVector
        LD -= 1
        isdone = true
    else
        error("unsupported SubArray index type $j")
    end
    jprev = j
    return jprev, LD, die_next_vector, isdone
end

function subarray_linearindexing_dim{A<:AbstractArray}(::Type{A}, It::Type)
    isa(Base.linearindexing(A), Base.LinearSlow) && return 0
    isempty(It.parameters) && return 0
    jprev = Void
    LD = 0
    die_next_vector = false
    while LD < length(It.parameters)
        LD += 1
        I = It.parameters[LD]
        jprev, LD, die_next_vector, isdone = nextLD(jprev, I, LD, die_next_vector)
        if isdone
            break
        end
    end
    LD
end

unsafe_convert{T,N,P<:Array,I<:Tuple{Vararg{RangeIndex}}}(::Type{Ptr{T}}, V::SubArray{T,N,P,I}) =
    pointer(V.parent) + (V.first_index-1)*sizeof(T)

unsafe_convert{T,N,P<:Array,I<:Tuple{Vararg{RangeIndex}}}(::Type{Ptr{Void}}, V::SubArray{T,N,P,I}) =
    convert(Ptr{Void}, unsafe_convert(Ptr{T}, V))

pointer(V::SubArray, i::Int) = pointer(V, ind2sub(size(V), i))

function pointer{T,N,P<:Array,I<:Tuple{Vararg{RangeIndex}}}(V::SubArray{T,N,P,I}, is::Tuple{Vararg{Int}})
    index = first_index(V)
    strds = strides(V)
    for d = 1:length(is)
        index += (is[d]-1)*strds[d]
    end
    return pointer(V.parent, index)
end

## Convert
convert{T,S,N}(::Type{Array{T,N}}, V::SubArray{S,N}) = copy!(Array(T, size(V)), V)


## Compatability
# deprecate?
function parentdims(s::SubArray)
    nd = ndims(s)
    dimindex = Array(Int, nd)
    sp = strides(s.parent)
    sv = strides(s)
    j = 1
    for i = 1:ndims(s.parent)
        r = s.indexes[i]
        if j <= nd && (isa(r,Union{Colon,Range}) ? sp[i]*step(r) : sp[i]) == sv[j]
            dimindex[j] = i
            j += 1
        end
    end
    dimindex
end

## Scalar indexing

# While it'd be nice to explicitly check bounds against the SubArray dimensions,
# the lack of an extensible @inbounds mechanism makes it difficult for users to
# avoid the cost of the bounds check without rewriting their syntax to use the
# unwieldy unsafe_getindex/unsafe_setindex! function calls. So instead we define
# getindex to rely upon the bounds checks in the parent array. It's still
# advantageous to define the unsafe_ variants without any bounds checks since
# the abstract indexing fallbacks can make use of them.
@generated function getindex{T,N,P,IV,LD}(V::SubArray{T,N,P,IV,LD}, I::Int...)
    ni = length(I)
    if ni == 1 && length(IV.parameters) == LD  # linear indexing
        meta = Expr(:meta, :inline)
        if iscontiguous(V)
            return :($meta; getindex(V.parent, V.first_index + I[1] - 1))
        end
        return :($meta; getindex(V.parent, V.first_index + V.stride1*(I[1]-1)))
    end
    Isyms = [:(I[$d]) for d = 1:ni]
    exhead, idxs = index_generate(ndims(P), IV, :V, Isyms)
    quote
        $exhead
        getindex(V.parent, $(idxs...))
    end
end
@generated function unsafe_getindex{T,N,P,IV,LD}(V::SubArray{T,N,P,IV,LD}, I::Int...)
    ni = length(I)
    if ni == 1 && length(IV.parameters) == LD  # linear indexing
        meta = Expr(:meta, :inline)
        if iscontiguous(V)
            return :($meta; unsafe_getindex(V.parent, V.first_index + I[1] - 1))
        end
        return :($meta; unsafe_getindex(V.parent, V.first_index + V.stride1*(I[1]-1)))
    end
    Isyms = [:(I[$d]) for d = 1:ni]
    exhead, idxs = index_generate(ndims(P), IV, :V, Isyms)
    quote
        $exhead
        unsafe_getindex(V.parent, $(idxs...))
    end
end
@generated function setindex!{T,N,P,IV,LD}(V::SubArray{T,N,P,IV,LD}, v, I::Int...)
    ni = length(I)
    if ni == 1 && length(IV.parameters) == LD  # linear indexing
        meta = Expr(:meta, :inline)
        if iscontiguous(V)
            return :($meta; setindex!(V.parent, v, V.first_index + I[1] - 1))
        end
        return :($meta; setindex!(V.parent, v, V.first_index + V.stride1*(I[1]-1)))
    end
    Isyms = Any[:(I[$d]) for d = 1:ni]
    exhead, idxs = index_generate(ndims(P), IV, :V, Isyms)
    quote
        $exhead
        setindex!(V.parent, v, $(idxs...))
    end
end
@generated function unsafe_setindex!{T,N,P,IV,LD}(V::SubArray{T,N,P,IV,LD}, v, I::Int...)
    ni = length(I)
    if ni == 1 && length(IV.parameters) == LD  # linear indexing
        meta = Expr(:meta, :inline)
        if iscontiguous(V)
            return :($meta; unsafe_setindex!(V.parent, v, V.first_index + I[1] - 1))
        end
        return :($meta; unsafe_setindex!(V.parent, v, V.first_index + V.stride1*(I[1]-1)))
    end
    Isyms = Any[:(I[$d]) for d = 1:ni]
    exhead, idxs = index_generate(ndims(P), IV, :V, Isyms)
    quote
        $exhead
        unsafe_setindex!(V.parent, v, $(idxs...))
    end
end

# Indexing with non-scalars. For now, this returns a copy, but changing that
# is just a matter of deleting the explicit call to copy.
getindex{T,N,P,IV}(V::SubArray{T,N,P,IV}, I::ViewIndex...) = copy(sub(V, I...))
unsafe_getindex{T,N,P,IV}(V::SubArray{T,N,P,IV}, I::ViewIndex...) = copy(sub_unsafe(V, I))

# Nonscalar setindex! falls back to the AbstractArray versions

# NP is parent dimensionality, Itypes is the tuple typeof(V.indexes)
# NP may not be equal to length(Itypes), because a view of a 2d matrix A
# can be constructed as V = A[5:13] or as V = A[2:4, 1:3, 1].
function index_generate(NP, Itypes, Vsym, Isyms)
    Itypes = Itypes.parameters
    if isempty(Isyms)
        Isyms = Any[1]  # this handles the syntax getindex(V)
    end
    exhead = :nothing
    NV = 0
    for I in Itypes
        NV += !(I == Int)
    end
    if length(Isyms) < NV
        # Linear indexing in the last index
        n = NV - length(Isyms)
        m = length(Isyms)
        strides = [gensym() for i = 1:n]
        indexes = [gensym() for i = 1:n+1]
        resid = gensym()
        linblock = Array(Expr, 2n+2)
        linblock[1] = :($(strides[1]) = size($Vsym, $m))
        for k = 2:n
            m += 1
            linblock[k] = :($(strides[k]) = $(strides[k-1]) * size($Vsym, $m))
        end
        k = n+1
        linblock[k] = :($resid = $(Isyms[end])-1)
        for i = n:-1:1
            k += 1
            linblock[k] = quote
                $(indexes[i+1]), $resid = divrem($resid, $(strides[i]))
                $(indexes[i+1]) += 1
            end
        end
        linblock[end] = :($(indexes[1]) = $resid+1)
        exhead = Expr(:block, linblock...)
        pop!(Isyms)
        append!(Isyms, indexes)
    end
    L = length(Itypes)
    indexexprs = Array(Any, L)
    j = 0
    for i = 1:L
        if Itypes[i] <: Real
            indexexprs[i] = :($Vsym.indexes[$i])
        else
            j += 1
            indexexprs[i] = :(unsafe_getindex($Vsym.indexes[$i], $(Isyms[j])))
        end
    end
    # Note that we drop any extra indices. We're trusting that the indices are
    # already checked to be in-bounds, so any extra indices must be 1 (and no-op)
    if exhead == :nothing
        exhead = Expr(:meta, :inline)
    end
    exhead, indexexprs
end
