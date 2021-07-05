# This file is a part of Julia. License is MIT: https://julialang.org/license

## copy between abstract arrays - generally more efficient
## since a single index variable can be used.
## copyto_unaliased! use @simd to speed up, so these definition is seperated from abstractarray.jl

function copyto!(dest::AbstractArray, src::AbstractArray)
    isempty(src) && return dest
    src′ = unalias(dest, src)
    copyto_unaliased!(IndexStyle(dest), dest, IndexStyle(src′), src′)
end

function copyto!(deststyle::IndexStyle, dest::AbstractArray, srcstyle::IndexStyle, src::AbstractArray)
    isempty(src) && return dest
    src′ = unalias(dest, src)
    copyto_unaliased!(deststyle, dest, srcstyle, src′)
end

function copyto_unaliased!(deststyle::IndexStyle, dest::AbstractArray, srcstyle::IndexStyle, src::AbstractArray)
    isempty(src) && return dest
    length(dest) < length(src) && throw(BoundsError(dest, LinearIndices(src)))
    if deststyle isa IndexLinear
        # Cartesian --> Linear
        srcstyle isa IndexCartesian && return C2Lcopyto!(dest, src)
        # Linear --> Linear
        Δi = firstindex(dest) - firstindex(src)
        for i in eachindex(src)
            @inbounds dest[i + Δi] = src[i]
        end
    else
        # Linear --> Cartesian
        srcstyle isa IndexLinear && return L2Ccopyto!(dest, src)
        # Cartesian --> Cartesian
        iterdest, itersrc = eachindex(dest), eachindex(src)
        if iterdest == itersrc #shared iterator
            @inbounds @simd for I in iterdest
                dest[I] = src[I]
            end
        else
            @inbounds for (J, I) in zip(iterdest, itersrc)
                dest[J] = src[I]
            end
        end
    end
    return dest
end

# Manually expanded SIMD kernal for Cartesian to Linear copy
@eval @inline function C2Lcopyto!(dest::AbstractArray, src::AbstractArray)
    iter = eachindex(src)
    Δj = firstindex(dest)
    @inbounds for I in simd_outer_range(iter)
        n = simd_inner_length(iter, I)
        j = zero(n)
        while j < n
            I′ = simd_index(iter, I, j)
            dest[Δj + j] = src[I′]
            j += 1
            $(Expr(:loopinfo, Symbol("julia.simdloop"), nothing))
        end
        Δj += n
    end
    dest
end

# Manually expanded SIMD kernal for Linear to Cartesian copy
@eval @inline function L2Ccopyto!(dest::AbstractArray, src::AbstractArray)
    iter = eachindex(dest)
    Δi, final = firstindex(src), lastindex(src) + 1
    @inbounds for J in simd_outer_range(iter)
        n = min(final - Δi, simd_inner_length(iter, J))
        i = zero(n)
        while i < n
            J′ = simd_index(iter, J, i)
            dest[J′] = src[i + Δi]
            i += 1
            $(Expr(:loopinfo, Symbol("julia.simdloop"), nothing))
        end
        Δi += n >= final && break
    end
    dest
end
