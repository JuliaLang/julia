# sort! for vectors of few unique integers
function sort_int_range!(x::AbstractVector{<:Integer}, rangelen, minval, maybereverse,
                         lo=firstindex(x), hi=lastindex(x))
    offs = 1 - minval

    counts = fill(0, rangelen)
    @inbounds for i = lo:hi
        counts[x[i] + offs] += 1
    end

    idx = lo
    @inbounds for i = maybereverse(1:rangelen)
        lastidx = idx + counts[i] - 1
        val = i-offs
        for j = idx:lastidx
            x[j] = val
        end
        idx = lastidx + 1
    end

    return x
end
