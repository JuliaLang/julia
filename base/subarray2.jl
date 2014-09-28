## Scalar indexing
# Low dimensions: avoid splatting
newsym = (:i_1, :i_2, :i_3, :i_4)
vars = Array(Expr, 0)
varsInt = Array(Expr, 0)
varsOther = Array(Expr, 0)
vars_toindex = Array(Expr, 0)
for i = 1:4
    sym = newsym[i]
    push!(vars, Expr(:quote, sym))
    push!(varsInt, :($sym::Int))
    push!(varsOther, :($sym::Union(Real, AbstractVector)))
    push!(vars_toindex, :(Base.to_index($sym)))
    ex = i == 1 ? quote
         getindex(V::SubArray, $sym::Real) = getindex(V, Base.to_index($sym))
        setindex!(V::SubArray, v, $sym::Real) = setindex!(V, v, Base.to_index($sym))
         getindex(V::SubArray, $sym::AbstractVector{Bool}) = getindex(V, Base.to_index($sym))
        setindex!(V::SubArray, v, $sym::AbstractVector{Bool}) = setindex!(V, v, Base.to_index($sym))
    end : quote
         getindex(V::SubArray, $(varsOther...)) = getindex(V, $(vars_toindex...))
        setindex!(V::SubArray, v, $(varsOther...)) = setindex!(V, v, $(vars_toindex...))
    end
    @eval begin
        stagedfunction getindex(V::SubArray, $(varsInt...))
            T, N, P, IV = V.parameters
            exhead, ex = index_generate(ndims(P), IV, :V, [$(vars...)])
            quote
                $exhead
                $ex
            end
        end
        stagedfunction setindex!(V::SubArray, v, $(varsInt...))
            T, N, P, IV = V.parameters
            exhead, ex = index_generate(ndims(P), IV, :V, [$(vars...)])
            quote
                $exhead
                $ex = v
            end
        end
        $ex
    end
end
# V[] notation (extracts the first element)
stagedfunction getindex(V::SubArray)
    T, N, P, IV = V.parameters
    Isyms = ones(Int, N)
    exhead, ex = index_generate(ndims(P), IV, :V, Isyms)
    quote
        $exhead
        $ex
    end
end
# Splatting variants
stagedfunction getindex(V::SubArray, I::Int...)
    T, N, P, IV = V.parameters
    Isyms = [:(I[$d]) for d = 1:length(I)]
    exhead, ex = index_generate(ndims(P), IV, :V, Isyms)
    quote
        $exhead
        $ex
    end
end
stagedfunction setindex!(V::SubArray, v, I::Int...)
    T, N, P, IV = V.parameters
    Isyms = [:(I[$d]) for d = 1:length(I)]
    exhead, ex = index_generate(ndims(P), IV, :V, Isyms)
    quote
        $exhead
        $ex = v
    end
end

# Indexing with non-scalars. For now, this returns a copy, but changing that
# is just a matter of deleting the explicit call to copy.
getindex(V::SubArray, I::ViewIndex...) = copy(sub(V, I...))
getindex{T,N}(V::SubArray{T,N}, I::AbstractArray{Bool,N}) = copy(sub(V, find(I)))   # this could be much better optimized
getindex{T,N}(V::SubArray{T,N}, I::Union(Real, AbstractVector)...) = getindex(V, Base.to_index(I)...)

function setindex!{T}(V::SubArray{T,1}, v, I::AbstractArray{Bool,1})
    length(I) == length(V) || throw(DimensionMismatch("logical vector must match array length"))
    setindex!(V, v, Base.to_index(I))
end
function setindex!{T,N}(V::SubArray{T,N}, v, I::AbstractArray{Bool,1})
    length(I) == length(V) || throw(DimensionMismatch("logical vector must match array length"))
    setindex!(V, v, Base.to_index(I))
end
function setindex!{T,N}(V::SubArray{T,N}, v, I::AbstractArray{Bool,N})
    size(I) == size(V) || throw(DimensionMismatch("size of Boolean mask must match array size"))
    _setindex!(V, v, find(I))  # this could be better optimized
end
setindex!{T,N}(V::SubArray{T,N}, v, I::Union(Real,AbstractVector)...) = setindex!(V, v, Base.to_index(I)...)
setindex!(V::SubArray, x, J::Union(Int,AbstractVector)...) = _setindex!(V, x, J...)
stagedfunction _setindex!(V::SubArray, x, J::Union(Real,AbstractVector)...)
    gen_setindex_body(length(J))
end

# NP is parent dimensionality, Itypes is the tuple typeof(V.indexes)
# NP may not be equal to length(Itypes), because a view of a 2d matrix A
# can be constructed as V = A[5:13] or as V = A[2:4, 1:3, 1].
function index_generate(NP, Itypes, Vsym, Isyms)
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
            indexexprs[i] = :(Base.unsafe_getindex($Vsym.indexes[$i], $(Isyms[j])))  # TODO: make Range bounds-checking respect @inbounds
        end
    end
    # Append any extra indexes. Must be trailing 1s or it will cause a BoundsError.
    if L < NP && j < length(Isyms)
        # This view was created as V = A[5:13], so appending them would generate interpretive confusion.
        # Instead, use double-indexing, i.e., A[indexes1...][indexes2...], where indexes2 contains the leftovers.
        return exhead, :($Vsym.parent[$(indexexprs...)][$(Isyms[j+1:end]...)])
    end
    for k = j+1:length(Isyms)
        push!(indexexprs, Isyms[k])
    end
    exhead, :($Vsym.parent[$(indexexprs...)])
end

unsafe_getindex(v::Real, ind::Int) = v
unsafe_getindex(v::Range, ind::Int) = first(v) + (ind-1)*step(v)
unsafe_getindex(v::AbstractArray, ind::Int) = v[ind]
unsafe_getindex(v, ind::Real) = unsafe_getindex(v, Base.to_index(ind))
