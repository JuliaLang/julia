# http://en.wikipedia.org/wiki/Selection_algorithm#Partition-based_general_selection_algorithm

function _jl_quickselect(a::AbstractVector, k::Integer, lo::Integer, hi::Integer)
    while true
        #partition(a, lo, hi, m)
        i, j = lo, hi
        pivot = a[div(lo+hi, 2)]
        while i <= j
            while a[i] < pivot; i += 1; end
            while pivot < a[j]; j -= 1; end
            if i <= j
                a[i], a[j] = a[j], a[i]
                i += 1
                j -= 1
            end
        end

        pivot_new = j + 1
        pivot_dist = pivot_new - lo + 1
        if pivot_dist == k
            return a[pivot_new]
        elseif k < pivot_dist
            hi = pivot_new - 1
        else
            k = k - pivot_dist
            lo = pivot_new + 1
        end

    end
end

select(a::AbstractVector, k::Integer) = _jl_quickselect(copy(a), k, 1, length(a))
select!(a::AbstractVector, k::Integer) = _jl_quickselect(a, k, 1, length(a))
