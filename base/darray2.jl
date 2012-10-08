type DArray{T,N,A} <: AbstractArray{T,N}
    dims::NTuple{N,Int}

    chunks::Array{RemoteRef,N}

    # pmap[i]==p ⇒ processor p has piece i
    pmap::Array{Int,1}

    # indexes held by piece i
    indexes::Array{NTuple{N,Range1{Int}},1}
end
