module Views

import Base: copy, eltype, getindex, length, ndims, setindex!, similar, size

export
    # types
    View,
    # functions
    sliceview,
    subview

DIMS = 1:6

typealias ViewIndex Union(Int, UnitRange{Int}, StepRange{Int,Int}, Vector{Int})
typealias NonSliceIndex Union(UnitRange{Int}, StepRange{Int,Int}, Vector{Int})

# Since there are no multidimensional range objects, we only permit 1d indexes
immutable View{T,N,P<:AbstractArray,I<:(ViewIndex...)} <: AbstractArray{T,N}
    parent::P
    indexes::I
    dims::NTuple{N,Int}
end

# Simple utilities
eltype{T,N,P,I}(V::View{T,N,P,I}) = T
eltype{T,N,P,I}(::Type{View{T,N,P,I}}) = T
ndims{T,N,P,I}(V::View{T,N,P,I}) = N
ndims{T,N,P,I}(::Type{View{T,N,P,I}}) = N
size(V::View) = V.dims
size(V::View, d::Integer) = d <= ndims(V) ? (@inbounds ret = V.dims[d]; ret) : 1
length(V::View) = prod(V.dims)
similar(V::View, T, dim::Dims) = similar(V.parent, T, dims)

# View creation
# We have two modes of creation: subview and sliceview. subview drops trailing singleton
# dimensions, sliceview drops all singleton dimensions.
for N in DIMS
    vars  = Symbol[symbol(string("I_",k)) for k = 1:N]
    # Input arguments to subview and sliceview
    createargtypes = Array(Any, N)
    createargs  = Array(Any, N)
    # Expressions for creating the indexes field
    indexesslice  = Array(Any, N)
    indexessub    = Array(Any, N)
    # Size of each supplied argument, and info about whether to drop it
    sizeexprs = Array(Any, N)
    keepslice = Array(Bool, N)
    keepsub   = Array(Bool, N)
    for i = 0:2^N-1   # treat scalar and nonscalar, leading to 2^N possibilities
        Mslice = 0  # dimensionality of created slice object
        Msub   = 0  # dimensionality of created sub object
        trailing = true   # is the current dimension still a trailing singleton?
        fill!(keepslice, false)
        fill!(keepsub, false)
        for k = N:-1:1  # starting from the back lets us update trailing
            if (i>>(k-1))&0x01 > 0
                # Non-scalar argument
                createargtypes[k] = NonSliceIndex
                indexesslice[k] = indexessub[k] = vars[k]
                keepslice[k] = keepsub[k] = true
                trailing = false
                sizeex = :(length($(vars[k])))
                sizeexprs[k] = sizeex
                Mslice += 1
                Msub   += 1
            else
                # Scalar argument
                createargtypes[k] = Real
                sizeexprs[k] = 1
                keepsub[k] = !trailing
                Msub += !trailing
                indexesslice[k] = indexessub[k] = :(int($(vars[k])))
                if !trailing
                    indexessub[k] = :(int($(vars[k])):int($(vars[k])))
                end
            end
            createargs[k] = :($(vars[k])::$(createargtypes[k]))
        end
        indexesslice_t = Expr(:tuple, indexesslice...)
        indexessub_t   = Expr(:tuple, indexessub...)
        dimsslice = Expr(:tuple, sizeexprs[keepslice]...)
        dimssub   = Expr(:tuple, sizeexprs[keepsub]...)
        eval(quote
            sliceview{T}(A::AbstractArray{T,$N}, $(createargs...)) =
                Views.View{T,$Mslice,typeof(A),typeof($indexesslice_t)}(A, $indexesslice_t, $dimsslice)
            subview{T}(A::AbstractArray{T,$N}, $(createargs...))   =
                Views.View{T,$Msub,  typeof(A),typeof($indexessub_t)}(A, $indexessub_t, $dimssub)
        end)
    end
end

unsafe_getindex(v::Real, ind::Int) = v
unsafe_getindex(v::Range, ind::Int) = first(v) + (ind-1)*step(v)
unsafe_getindex(v::BitArray, ind::Int) = Base.unsafe_bitgetindex(v.chunks, ind)
unsafe_getindex(v::AbstractArray, ind::Int) = v[ind]
unsafe_getindex(v, ind::Real) = unsafe_getindex(v, to_index(ind))


# TODO: Constructing from another View


function index_generate(Nd, Itypes, Vsym, Isyms)
    if isempty(Isyms)
        Isyms = Any[1]  # this handles the syntax getindex(V)
    end
    exhead = :nothing
    if length(Isyms) < Nd
        # Linear indexing in the last index
        n = Nd - length(Isyms)
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
    NP = length(Itypes)
    indexexprs = Array(Any, NP)
    j = 0
    for i = 1:NP
        if Itypes[i] <: Real
            indexexprs[i] = :($Vsym.indexes[$i])
        else
            j += 1
            indexexprs[i] = :(unsafe_getindex($Vsym.indexes[$i], $(Isyms[j])))  # TODO: make Range bounds-checking respect @inbounds
        end
    end
    # Append any extra indexes. Must be trailing 1s or it will cause a BoundsError.
    for k = j+1:length(Isyms)
        push!(indexexprs, :($(Isyms[k])))
    end
    exhead, :($Vsym.parent[$(indexexprs...)])
end

# Scalar indexing
# It's possible to index a 3D object with only 2 indexes, in which case the last index is interpreted as a linear index.
# Likewise, it's possible to index a 2D object with 3 indexes, in which case the last had better be 1 or
# you'll get a bounds error.
# So we have to support DIMS x DIMS possibilities, specializing each on all possible typeof(V.indexes).
# That's a lot of methods.
for Nargs = 0:maximum(DIMS)
    args  = Symbol[symbol(string("I_",k)) for k = 1:Nargs]
    typedargs = map(x->:($x::Real), args)
    for Nd in DIMS
        Vindextypes = Array(Any, Nd)
        for i = 0:2^Nd-1   # treat scalar and nonscalar, leading to 2^Nd possibilities
            np = 0
            for k = 1:Nd
                if (i>>(k-1))&0x01 > 0
                    Vindextypes[k] = NonSliceIndex
                    np += 1
                    s = symbol(string("P", np))
                else
                    Vindextypes[k] = Int
                end
            end
            body = index_generate(np, Vindextypes, :V, copy(args))
            tupleexpr = Expr(:tuple, Vindextypes...)
            eval(quote
                function getindex{T,A,TT<:$tupleexpr}(V::View{T,$np,A,TT}, $(typedargs...))
                    $(body[1])
                    $(body[2])
                end
                function setindex!{T,A,TT<:$tupleexpr}(V::View{T,$np,A,TT}, val, $(typedargs...))
                    $(body[1])
                    $(body[2]) = val
                end
            end)
        end
    end
end


# # More utility functions
# stagedfunction copy(V::View)
#     T, N = eltype(V), ndims(V)
#     quote
#         A = Array($T, V.dims)
#         k = 1
#         Base.Cartesian.@nloops $N i A begin
#             @inbounds A[k] = Base.Cartesian.@nref($N, V, i)
#             k += 1
#         end
#         A
#     end
# end

end