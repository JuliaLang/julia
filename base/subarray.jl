typealias NonSliceIndex Union(Colon, Range{Int}, UnitRange{Int}, Array{Int,1})
typealias ViewIndex Union(Int, NonSliceIndex)
typealias RangeIndex Union(Int, Range{Int}, UnitRange{Int}, Colon)

# LD is the last dimension up through which this object has efficient
# linear indexing. If LD==N, then the object itself has efficient
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
stagedfunction _slice{T,NP,IndTypes}(A::AbstractArray{T,NP}, I::IndTypes)
    N = 0
    sizeexprs = Array(Any, 0)
    for k = 1:length(I)
        i = I[k]
        if !(i <: Real)
            N += 1
            push!(sizeexprs, dimsizeexpr(I[k], k, length(I), :A, :I))
        end
    end
    dims = :(tuple($(sizeexprs...)))
    LD = subarray_linearindexing_dim(A, I)
    strideexpr = stride1expr(A, I, :A, :I, LD)
    exfirst = first_index_expr(:A, :I, length(I))
    quote
        $exfirst
        SubArray{$T,$N,$A,$I,$LD}(A, I, $dims, f, $strideexpr)
    end
end

# Conventional style (drop trailing singleton dimensions, keep any
# other singletons)
sub(A::AbstractArray, I::ViewIndex...) = _sub(A, I)
sub(A::AbstractArray, I::(ViewIndex...)) = _sub(A, I)
stagedfunction _sub{T,NP,IndTypes}(A::AbstractArray{T,NP}, I::IndTypes)
    sizeexprs = Array(Any, 0)
    Itypes = Array(Any, 0)
    Iexprs = Array(Any, 0)
    N = length(I)
    while N > 0 && I[N] <: Real
        N -= 1
    end
    for k = 1:length(I)
        if k <= N
            push!(sizeexprs, dimsizeexpr(I[k], k, length(I), :A, :I))
        end
        if k < N && I[k] <: Real
            push!(Itypes, UnitRange{Int})
            push!(Iexprs, :(int(I[$k]):int(I[$k])))
        else
            push!(Itypes, I[k])
            push!(Iexprs, :(I[$k]))
        end
    end
    dims = :(tuple($(sizeexprs...)))
    Iext = :(tuple($(Iexprs...)))
    It = tuple(Itypes...)
    LD = subarray_linearindexing_dim(A, I)
    strideexpr = stride1expr(A, I, :A, :I, LD)
    exfirst = first_index_expr(:A, :I, length(It))
    quote
        $exfirst
        SubArray{$T,$N,$A,$It,$LD}(A, $Iext, $dims, f, $strideexpr)
    end
end

# Constructing from another SubArray
# This "pops" the old SubArray and creates a more compact one
stagedfunction _slice{T,NV,PV,IV,PLD,IndTypes}(V::SubArray{T,NV,PV,IV,PLD}, I::IndTypes)
    N = 0
    sizeexprs = Array(Any, 0)
    indexexprs = Array(Any, 0)
    Itypes = Array(Any, 0)
    k = 0  # index into I
    LD, die_next_vector, Ilast, isLDdone = 0, false, Void, false  # for linear indexing inference
    for j = 1:length(IV)
        if IV[j] <: Real
            push!(indexexprs, :(V.indexes[$j]))
            push!(Itypes, IV[j])
            if !isLDdone
                LD += 1
            end
        else
            k += 1
            if !isLDdone
                if LD < PLD
                    LD += 1
                    Ilast, LD, die_next_vector, isdone = nextLD(Ilast, I[k], LD, die_next_vector)
                    isLDdone |= isdone
                else
                    if I[k] <: Real
                        LD += 1
                    else
                        isLDdone = true
                    end
                end
            end
            if k < length(I) || k == NV || j == length(IV)
                if !(I[k] <: Real)
                    N += 1
                    push!(sizeexprs, dimsizeexpr(I[k], k, length(I), :V, :I))
                end
                push!(indexexprs, :(V.indexes[$j][I[$k]]))
                push!(Itypes, rangetype(IV[j], I[k]))
            else
                # We have a linear index that spans more than one dimension of the parent
                N += 1
                push!(sizeexprs, dimsizeexpr(I[k], k, length(I), :V, :I))
                push!(indexexprs, :(merge_indexes(V, V.indexes[$j:end], size(V.parent)[$j:end], I[$k])))
                push!(Itypes, Array{Int, 1})
                break
            end
        end
    end
    for i = k+1:length(I)
        if !(I[i] <: Real)
            N += 1
            push!(sizeexprs, dimsizeexpr(I[i], i, length(I), :V, :I))
            isLDdone = true
        elseif !isLDdone
            LD += 1
        end
        push!(indexexprs, :(I[$i]))
        push!(Itypes, I[i])
    end
    Inew = :(tuple($(indexexprs...)))
    dims = :(tuple($(sizeexprs...)))
    It = tuple(Itypes...)
    LD = max(LD, subarray_linearindexing_dim(PV, It))
    strideexpr = stride1expr(PV, It, :(V.parent), :Inew, LD)
    exfirst = first_index_expr(:(V.parent), :Inew, length(It))
    quote
        Inew = $Inew
        $exfirst
        SubArray{$T,$N,$PV,$It,$LD}(V.parent, Inew, $dims, f, $strideexpr)
    end
end

stagedfunction _sub{T,NV,PV,IV,PLD,IndTypes}(V::SubArray{T,NV,PV,IV,PLD}, I::IndTypes)
    N = length(I)
    while N > 0 && I[N] <: Real
        N -= 1
    end
    sizeexprs = Array(Any, 0)
    indexexprs = Array(Any, 0)
    Itypes = Array(Any, 0)
    preexprs = Array(Any, 0)
    k = 0
    LD, die_next_vector, Ilast, isLDdone = 0, false, Void, false  # for linear indexing inference
    for j = 1:length(IV)
        if IV[j] <: Real
            push!(indexexprs, :(V.indexes[$j]))
            push!(Itypes, IV[j])
            if !isLDdone
                LD += 1
            end
        else
            k += 1
            if k <= N
                push!(sizeexprs, dimsizeexpr(I[k], k, length(I), :V, :I))
            end
            if k < N && I[k] <: Real
                # convert scalar to a range
                sym = gensym()
                push!(preexprs, :($sym = V.indexes[$j][int(I[$k])]))
                push!(indexexprs, :($sym:$sym))
                push!(Itypes, UnitRange{Int})
            elseif k < length(I) || k == NV || j == length(IV)
                # simple indexing
                push!(indexexprs, :(V.indexes[$j][I[$k]]))
                push!(Itypes, rangetype(IV[j], I[k]))
            else
                # We have a linear index that spans more than one dimension of the parent
                push!(indexexprs, :(merge_indexes(V, V.indexes[$j:end], size(V.parent)[$j:end], I[$k])))
                push!(Itypes, Array{Int, 1})
                break
            end
            if !isLDdone
                if LD < PLD
                    LD += 1
                    Ilast, LD, die_next_vector, isdone = nextLD(Ilast, I[k], LD, die_next_vector)
                    isLDdone |= isdone
                else
                    if I[k] <: Real
                        LD += 1
                    else
                        isLDdone = true
                    end
                end
            end
        end
    end
    for i = k+1:length(I)
        if i <= N
            push!(sizeexprs, dimsizeexpr(I[i], i, length(I), :V, :I))
        end
        push!(indexexprs, :(I[$i]))
        push!(Itypes, I[i])
    end
    Inew = :(tuple($(indexexprs...)))
    dims = :(tuple($(sizeexprs...)))
    It = tuple(Itypes...)
    LD = max(LD, subarray_linearindexing_dim(PV, It))
    strideexpr = stride1expr(PV, It, :(V.parent), :Inew, LD)
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

## Strides
stagedfunction strides{T,N,P,I}(V::SubArray{T,N,P,I})
    all(map(x->x<:Union(RangeIndex,Colon), I)) || error("strides valid only for RangeIndex indexing")
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

function stride1expr(Atype::Type, Itypes::Tuple, Aexpr, Inewsym, LD)
    if LD == 0
        return 0
    end
    ex = 1
    for d = 1:min(LD, length(Itypes))
        I = Itypes[d]
        if I <: Real
            ex = :($ex * size($Aexpr, $d))
        else
            ex = :($ex * step($Inewsym[$d]))
            break
        end
    end
    ex
end

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
function nextLD(Ilast, I, LD, die_next_vector)
    isdone = false
    if I <: Real
        if Ilast != Void && !(Ilast <: Real)
            die_next_vector = true
        end
    elseif die_next_vector || I <: Vector
        LD -= 1
        isdone = true
    elseif I == Colon
    elseif I <: UnitRange
        die_next_vector = true
    elseif I <: Range
        if !(Ilast == Void || Ilast <: Real)
            LD -= 1
            isdone = true
        end
        die_next_vector = true
    else
        error("This shouldn't happen (linear indexing inference)")
    end
    Ilast = I
    return Ilast, LD, die_next_vector, isdone
end

function subarray_linearindexing_dim{A<:AbstractArray}(::Type{A}, It::Tuple)
    isa(Base.linearindexing(A), Base.LinearSlow) && return 0
    isempty(It) && return 0
    Ilast = Void
    LD = 0
    die_next_vector = false
    while LD < length(It)
        LD += 1
        I = It[LD]
        Ilast, LD, die_next_vector, isdone = nextLD(Ilast, I, LD, die_next_vector)
        if isdone
            break
        end
    end
    LD
end

convert{T,N,P<:Array,I<:(RangeIndex...)}(::Type{Ptr{T}}, V::SubArray{T,N,P,I}) =
    pointer(V.parent) + (V.first_index-1)*sizeof(T)

convert{T,N,P<:Array,I<:(RangeIndex...)}(::Type{Ptr{Void}}, V::SubArray{T,N,P,I}) =
    convert(Ptr{Void}, convert(Ptr{T}, V))

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
