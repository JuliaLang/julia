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
    length(dest) >= length(src) || throw(BoundsError(dest, LinearIndices(src)))
    if deststyle isa IndexLinear
        if srcstyle isa IndexLinear
            Δi = firstindex(dest) - firstindex(src)
            for i in eachindex(src)
                @inbounds dest[i + Δi] = src[i]
            end
        else
            j = firstindex(dest) - 1
            @inbounds @simd for I in eachindex(src)
                dest[j+=1] = src[I]
            end
        end
    else
        if srcstyle isa IndexLinear
            i = firstindex(src) - 1
            @inbounds @simd for J in eachindex(dest)
                dest[J] = src[i+=1]
            end
        else
            iterdest, itersrc = eachindex(dest), eachindex(src)
            if iterdest == itersrc
                # Shared-iterator implementation
                @inbounds @simd for I in itersrc
                    dest[I] = src[I]
                end
            else
                for (I,J) in zip(itersrc, iterdest)
                    @inbounds dest[J] = src[I]
                end
            end
        end
    end
    return dest
end
