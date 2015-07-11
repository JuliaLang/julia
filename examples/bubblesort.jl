# This file is a part of Julia. License is MIT: http://julialang.org/license

import Base.Sort
immutable BubbleSortAlg <: Sort.Algorithm end
const BubbleSort = BubbleSortAlg()

function Base.sort!(v::AbstractVector, lo::Int, hi::Int, ::BubbleSortAlg, o::Sort.Ordering)
    while true
        clean = true
        for i = lo:hi-1
            if Sort.lt(o, v[i+1], v[i])
                v[i+1], v[i] = v[i], v[i+1]
                clean = false
            end
        end
        clean && break
    end
    return v
end
