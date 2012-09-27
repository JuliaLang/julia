
# purpose : unwrap any internal subarrayed structure
#           without disturbing the data organization
#

# Carlo Baldassi contributed this (2012-Jun-19)
#
# use: flatten1d(a::Array{T})

function deep_numel{T}(a::Array{T})
    if T <: Array
        n = 0
        for i = 1:length(a)
            n += deep_numel(a[i])
        end
        return n
    end
    return numel(a)
end

deep_numel(a) = 1

function deep_eltype(T::Type)
    if isa(T, BitsKind)
        return T
    elseif T <: Array
        return deep_eltype(eltype(T))
    else
        return Any
    end
end

deep_eltype{T}(a::Array{T}) = deep_eltype(T)

function flat_copy{T}(dest::Array, dest_ind::Int, src::Array{T})
    if T <: Array
        new_dest_ind = dest_ind
        for i = 1:length(src)
            new_dest_ind += flat_copy(dest, new_dest_ind, src[i])
        end
        return new_dest_ind - dest_ind
    end
    copy_to(dest, dest_ind, src, 1, length(src))
    return length(src)
end

function flatten1d(a::Array, T::Type)
    n = deep_numel(a)
    b = Array(T, n)
    flat_copy(b, 1, a)
    return b
end

flatten1d(a::Array) = flatten1d(a, deep_eltype(a))


