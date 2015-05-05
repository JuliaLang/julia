# This file is a part of Julia. License is MIT: http://julialang.org/license

## Scalar indexing
@inline getindex(V::SubArray, I::Int...) = (checkbounds(V, I...); unsafe_getindex(V, I...))
@generated function unsafe_getindex{T,N,P,IV,LD}(V::SubArray{T,N,P,IV,LD}, I::Int...)
    ni = length(I)
    if ni == 1 && length(IV.parameters) == LD  # linear indexing
        meta = Expr(:meta, :inline)
        if iscontiguous(V)
            return :($meta; V.parent[V.first_index + I[1] - 1])
        end
        return :($meta; V.parent[V.first_index + V.stride1*(I[1]-1)])
    end
    Isyms = [:(I[$d]) for d = 1:ni]
    exhead, idxs = index_generate(ndims(P), IV, :V, Isyms)
    quote
        $exhead
        unsafe_getindex(V.parent, $(idxs...))
    end
end
@inline setindex!(V::SubArray, v, I::Int...) = (checkbounds(V, I...); unsafe_setindex!(V, v, I...))
@generated function unsafe_setindex!{T,N,P,IV,LD}(V::SubArray{T,N,P,IV,LD}, v, I::Int...)
    ni = length(I)
    if ni == 1 && length(IV.parameters) == LD  # linear indexing
        meta = Expr(:meta, :inline)
        if iscontiguous(V)
            return :($meta; V.parent[V.first_index + I[1] - 1] = v)
        end
        return :($meta; V.parent[V.first_index + V.stride1*(I[1]-1)] = v)
    end
    Isyms = [:(I[$d]) for d = 1:ni]
    exhead, idxs = index_generate(ndims(P), IV, :V, Isyms)
    quote
        $exhead
        unsafe_setindex!(V.parent, v, $(idxs...))
    end
end

# Indexing with non-scalars. For now, this returns a copy, but changing that
# is just a matter of deleting the explicit call to copy.
getindex{T,N,P,IV}(V::SubArray{T,N,P,IV}, I::ViewIndex...) = copy(sub(V, I...))
getindex{T,N,P,IV}(V::SubArray{T,N,P,IV}, I::Union(Real, AbstractArray, Colon)...) = getindex(V, to_index(I)...)
unsafe_getindex{T,N,P,IV}(V::SubArray{T,N,P,IV}, I::ViewIndex...) = copy(sub_unsafe(V, I))
unsafe_getindex{T,N,P,IV}(V::SubArray{T,N,P,IV}, I::Union(Real, AbstractArray, Colon)...) = unsafe_getindex(V, to_index(I)...)

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
