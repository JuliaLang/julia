# This file is a part of Julia. License is MIT: https://julialang.org/license

# Heap operations on flat vectors
# -------------------------------

# Binary heap indexing
heapleft(i::Integer) = 2i
heapright(i::Integer) = 2i + 1
heapparent(i::Integer) = div(i, 2)

# Binary min-heap percolate down.
function percolate_down!(xs::Vector, i::Integer, x, o::Ordering, len::Integer=length(xs))
    @inbounds while (l = heapleft(i)) <= len
        r = heapright(i)
        j = r > len || lt(o, xs[l], xs[r]) ? l : r
        lt(o, xs[j], x) || break
        xs[i] = xs[j]
        i = j
    end
    xs[i] = x
end

# Binary min-heap percolate up.
function percolate_up!(xs::Vector, i::Integer, x, o::Ordering)
    @inbounds while (j = heapparent(i)) >= 1
        lt(o, x, xs[j]) || break
        xs[i] = xs[j]
        i = j
    end
    xs[i] = x
end

"""
    heappop!(v, ord)

Given a binary heap-ordered array, remove and return the lowest ordered element.
For efficiency, this function does not check that the array is indeed heap-ordered.
"""
function heappop!(xs::Vector, o::Ordering)
    x = xs[1]
    y = pop!(xs)
    if !isempty(xs)
        percolate_down!(xs, 1, y, o)
    end
    return x
end

"""
    heappush!(v, x, ord)

Given a binary heap-ordered array, push a new element `x`, preserving the heap property.
For efficiency, this function does not check that the array is indeed heap-ordered.
"""
function heappush!(xs::Vector, x, o::Ordering)
    push!(xs, x)
    i = lastindex(xs)
    percolate_up!(xs, i, @inbounds(xs[i]), o)
    return xs
end

"""
    heapify!(v, ord::Ordering)

Turn an arbitrary vector into a binary min-heap in linear time.
"""
function heapify!(xs::Vector, o::Ordering)
    for i in heapparent(lastindex(xs)):-1:1
        percolate_down!(xs, i, @inbounds(xs[i]), o)
    end
    return xs
end
