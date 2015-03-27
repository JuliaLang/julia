typealias NonSliceIndex Union(Colon, Range{Int}, UnitRange{Int}, Array{Int,1})
typealias ViewIndex Union(Int, NonSliceIndex)
typealias RangeIndex Union(Int, Range{Int}, UnitRange{Int}, Colon)

# LD is the last dimension up through which this object has efficient
# linear indexing. If LD==length(I), then the object itself has efficient
# linear indexing.
immutable SubArray{T,N,P<:AbstractArray,I<:(ViewIndex...),LD} <: AbstractArray{T,N}
    parent::P
    indexes::I
    dims::NTuple{N,Int}
    first_index::Int   # for linear indexing and pointer
    stride1::Int       # used only for linear indexing
end

# Simple utilities
eltype{T,N,P,I}(V::SubArray{T,N,P,I}) = T
eltype{T,N,P,I}(::Type{SubArray{T,N,P,I}}) = T
ndims{T,N,P,I}(V::SubArray{T,N,P,I}) = N
ndims{T,N,P,I}(::Type{SubArray{T,N,P,I}}) = N
size(V::SubArray) = V.dims
# size(V::SubArray, d::Integer) = d <= ndims(V) ? (@inbounds ret = V.dims[d]; ret) : 1
length(V::SubArray) = prod(V.dims)

similar(V::SubArray, T, dims::Dims) = similar(V.parent, T, dims)
copy(V::SubArray) = copy!(similar(V.parent, size(V)), V)

parent(V::SubArray) = V.parent
parentindexes(V::SubArray) = V.indexes

parent(a::AbstractArray) = a
parentindexes(a::AbstractArray) = ntuple(ndims(a), i->1:size(a,i))

## SubArray creation
# Drops singleton dimensions (those indexed with a scalar)
slice(A::AbstractArray, I::ViewIndex...) = _slice(A, I)
slice(A::AbstractArray, I::(ViewIndex...)) = _slice(A, I)
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

stagedfunction slice_unsafe{T,NP,IndTypes}(A::AbstractArray{T,NP}, J::IndTypes)
    N = 0
    sizeexprs = Array(Any, 0)
    for Jindex = 1:length(J)
        j = J[Jindex]
        if !(j <: Real)
            N += 1
            push!(sizeexprs, dimsizeexpr(j, Jindex, length(J), :A, :J))
        end
    end
    dims = :(tuple($(sizeexprs...)))
    LD = subarray_linearindexing_dim(A, J)
    strideexpr = stride1expr(A, J, :A, :J, LD)
    exfirst = first_index_expr(:A, :J, length(J))
    quote
        $exfirst
        SubArray{$T,$N,$A,$J,$LD}(A, J, $dims, f, $strideexpr)
    end
end

# Conventional style (drop trailing singleton dimensions, keep any
# other singletons by converting them to ranges, e.g., 3:3)
sub(A::AbstractArray, I::ViewIndex...) = _sub(A, I)
sub(A::AbstractArray, I::(ViewIndex...)) = _sub(A, I)
function _sub(A, I)
    checkbounds(A, I...)
    sub_unsafe(A, I)
end

stagedfunction sub_unsafe{T,NP,IndTypes}(A::AbstractArray{T,NP}, J::IndTypes)
    sizeexprs = Array(Any, 0)
    Itypes = Array(Any, 0)
    Iexprs = Array(Any, 0)
    N = length(J)
    while N > 0 && J[N] <: Real
        N -= 1
    end
    for Jindex = 1:length(J)
        j = J[Jindex]
        if Jindex <= N
            push!(sizeexprs, dimsizeexpr(j, Jindex, length(J), :A, :J))
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
    It = tuple(Itypes...)
    LD = subarray_linearindexing_dim(A, J)
    strideexpr = stride1expr(A, J, :A, :J, LD)
    exfirst = first_index_expr(:A, :J, length(It))
    quote
        $exfirst
        SubArray{$T,$N,$A,$It,$LD}(A, $Iext, $dims, f, $strideexpr)
    end
end

# Constructing from another SubArray
# This "pops" the old SubArray and creates a more compact one
stagedfunction slice_unsafe{T,NV,PV,IV,PLD,IndTypes}(V::SubArray{T,NV,PV,IV,PLD}, J::IndTypes)
    N = 0
    sizeexprs = Array(Any, 0)
    indexexprs = Array(Any, 0)
    Itypes = Array(Any, 0)
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
    for IVindex = 1:length(IV)
        iv = IV[IVindex]
        if iv <: Real
            push!(indexexprs, :(V.indexes[$IVindex]))
            push!(Itypes, iv)
            if !isLDdone
                LD += 1
            end
        else
            Jindex += 1
            j = J[Jindex]
            if Jindex < length(J) || Jindex == NV || IVindex == length(IV)
                if !(j <: Real)
                    N += 1
                    push!(sizeexprs, dimsizeexpr(j, Jindex, length(J), :V, :J))
                end
                push!(indexexprs, :(reindex(V.indexes[$IVindex], J[$Jindex])))
                push!(Itypes, rangetype(iv, j))
            else
                # We have a linear index that spans more than one
                # dimension of the parent
                N += 1
                push!(sizeexprs, dimsizeexpr(j, Jindex, length(J), :V, :J))
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
    for Jind = Jindex+1:length(J)
        j = J[Jind]
        if !(j <: Real)
            N += 1
            push!(sizeexprs, dimsizeexpr(j, Jind, length(J), :V, :J))
            isLDdone = true
        elseif !isLDdone
            LD += 1
        end
        push!(indexexprs, :(J[$Jind]))
        push!(Itypes, j)
    end
    Inew = :(tuple($(indexexprs...)))
    dims = :(tuple($(sizeexprs...)))
    It = tuple(Itypes...)
    LD = max(LD, subarray_linearindexing_dim(PV, It))
    strideexpr = stride1expr(PV, It, :(V.parent), :Inew, LD, :J, Iindex_lin, Jindex_lin)
    exfirst = first_index_expr(:(V.parent), :Inew, length(It))
    quote
        Inew = $Inew
        $exfirst
        SubArray{$T,$N,$PV,$It,$LD}(V.parent, Inew, $dims, f, $strideexpr)
    end
end

stagedfunction sub_unsafe{T,NV,PV,IV,PLD,IndTypes}(V::SubArray{T,NV,PV,IV,PLD}, J::IndTypes)
    N = length(J)
    while N > 0 && J[N] <: Real
        N -= 1
    end
    sizeexprs = Array(Any, 0)
    indexexprs = Array(Any, 0)
    Itypes = Array(Any, 0)
    ItypesLD = Array(Any, 0)
    preexprs = Array(Any, 0)
    LD, die_next_vector, jprev, isLDdone = 0, false, Void, false
    Jindex = 0
    for IVindex = 1:length(IV)
        iv = IV[IVindex]
        if iv <: Real
            push!(indexexprs, :(V.indexes[$IVindex]))
            push!(Itypes, iv)
            push!(ItypesLD, iv)
            if !isLDdone
                LD += 1
            end
        else
            Jindex += 1
            j = J[Jindex]
            if Jindex <= N
                push!(sizeexprs, dimsizeexpr(j, Jindex, length(J), :V, :J))
            end
            if Jindex < N && j <: Real
                # convert scalar to a range
                sym = gensym()
                push!(preexprs, :($sym = reindex(V.indexes[$IVindex], Int(J[$Jindex]))))
                push!(indexexprs, :($sym:$sym))
                push!(Itypes, UnitRange{Int})
                push!(ItypesLD, j)
            elseif Jindex < length(J) || Jindex == NV || IVindex == length(IV)
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
    for Jind = Jindex+1:length(J)
        j = J[Jind]
        if Jind <= N
            push!(sizeexprs, dimsizeexpr(j, Jind, length(J), :V, :J))
        end
        push!(indexexprs, :(J[$Jind]))
        push!(Itypes, j)
        push!(ItypesLD, Itypes[end])
    end
    Inew = :(tuple($(indexexprs...)))
    dims = :(tuple($(sizeexprs...)))
    It = tuple(Itypes...)
    ItLD = tuple(ItypesLD...)
    LD = max(LD, subarray_linearindexing_dim(PV, It))
    strideexpr = stride1expr(PV, ItLD, :(V.parent), :Inew, LD)
    preex = isempty(preexprs) ? nothing : Expr(:block, preexprs...)
    exfirst = first_index_expr(:(V.parent), :Inew, length(It))
    quote
        $preex
        Inew = $Inew
        $exfirst
        SubArray{$T,$N,$PV,$It,$LD}(V.parent, Inew, $dims, f, $strideexpr)
    end
end

function rangetype(T1, T2)
    rt = return_types(getindex, (T1, T2))
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

stagedfunction linearindexing{T,N,P,I,LD}(A::SubArray{T,N,P,I,LD})
    length(I) == LD ? (:(LinearFast())) : (:(LinearSlow()))
end
stagedfunction linearindexing{A<:SubArray}(::Type{A})
    T,N,P,I,LD = A.parameters
    length(I) == LD ? (:(LinearFast())) : (:(LinearSlow()))
end

getindex(::Colon, ::Colon) = Colon()
getindex{T}(v::AbstractArray{T,1}, ::Colon) = v
getindex(::Colon, i) = i

step(::Colon) = 1
first(::Colon) = 1
in(::Int, ::Colon) = true

## Strides
stagedfunction strides{T,N,P,I}(V::SubArray{T,N,P,I})
    all(map(x->x<:Union(RangeIndex,Colon), I)) || throw(ArgumentError("strides valid only for RangeIndex indexing"))
    strideexprs = Array(Any, N+1)
    strideexprs[1] = 1
    i = 1
    Vdim = 1
    for i = 1:length(I)
        if !(I[i]==Int)
            strideexprs[Vdim+1] = copy(strideexprs[Vdim])
            strideexprs[Vdim] = :(step(V.indexes[$i])*$(strideexprs[Vdim]))
            Vdim += 1
        end
        strideexprs[Vdim] = :(size(V.parent, $i) * $(strideexprs[Vdim]))
    end
    :(tuple($(strideexprs[1:N]...)))
end

stride(V::SubArray, d::Integer) = d <= ndims(V) ? strides(V)[d] : strides(V)[end] * size(V)[end]

function stride1expr(Atype::Type, Itypes::Tuple, Aexpr, Isym, LD, Jsym=Isym, Iindex_lin=0, Jindex_lin=0)
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
    LD == length(I) || return false
    length(I) < 1 && return true
    I[1] == Colon && return true
    if I[1] <: UnitRange
        # It might be stride1 == 1, or this might be because `sub` was
        # used with an integer for the first index
        for j = 2:length(I)
            (I[j] == Colon || (I[j] <: AbstractVector)) && return false
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
            f += (first($Isym[$i])-1)*s
            s *= size($Asym, $i)
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
    elseif die_next_vector || j <: Vector
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
    else
        error("This shouldn't happen (linear indexing inference)")
    end
    jprev = j
    return jprev, LD, die_next_vector, isdone
end

function subarray_linearindexing_dim{A<:AbstractArray}(::Type{A}, It::Tuple)
    isa(Base.linearindexing(A), Base.LinearSlow) && return 0
    isempty(It) && return 0
    jprev = Void
    LD = 0
    die_next_vector = false
    while LD < length(It)
        LD += 1
        I = It[LD]
        jprev, LD, die_next_vector, isdone = nextLD(jprev, I, LD, die_next_vector)
        if isdone
            break
        end
    end
    LD
end

unsafe_convert{T,N,P<:Array,I<:(RangeIndex...)}(::Type{Ptr{T}}, V::SubArray{T,N,P,I}) =
    pointer(V.parent) + (V.first_index-1)*sizeof(T)

unsafe_convert{T,N,P<:Array,I<:(RangeIndex...)}(::Type{Ptr{Void}}, V::SubArray{T,N,P,I}) =
    convert(Ptr{Void}, unsafe_convert(Ptr{T}, V))

pointer(V::SubArray, i::Int) = pointer(V, ind2sub(size(V), i))

function pointer{T,N,P<:Array,I<:(RangeIndex...)}(V::SubArray{T,N,P,I}, is::(Int...))
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
        if j <= nd && (isa(r,Union(Colon,Range)) ? sp[i]*step(r) : sp[i]) == sv[j]
            dimindex[j] = i
            j += 1
        end
    end
    dimindex
end
