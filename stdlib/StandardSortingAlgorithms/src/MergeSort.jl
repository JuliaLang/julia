function sort!(v::AbstractVector{T}, lo::Integer, hi::Integer, a::MergeSortAlg, o::Ordering,
    t0::Union{AbstractVector{T}, Nothing}=nothing) where T
@inbounds if lo < hi
    hi-lo <= SMALL_THRESHOLD && return sort!(v, lo, hi, SMALL_ALGORITHM, o)

    m = midpoint(lo, hi)
    t = workspace(v, t0, m-lo+1)

    sort!(v, lo,  m,  a, o, t)
    sort!(v, m+1, hi, a, o, t)

    i, j = 1, lo
    while j <= m
        t[i] = v[j]
        i += 1
        j += 1
    end

    i, k = 1, lo
    while k < j <= hi
        if lt(o, v[j], t[i])
            v[k] = v[j]
            j += 1
        else
            v[k] = t[i]
            i += 1
        end
        k += 1
    end
    while k < j
        v[k] = t[i]
        k += 1
        i += 1
    end
end

return v
end
