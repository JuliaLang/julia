# These functions define a heap
# Example usage of the Heap object:
# You can create an empty heap this way:
#   h = Heap(Float64)
# This defaults to using < for comparison, so is a min-heap.
#
# A more complex and complete example, yielding a max-heap:
#   h = Heap(>,rand(3))  # initialize heap with 3 random points
#   for i = 1:10
#     push!(h,rand())    # add more points
#   end
#   length(h)
#   max_val = pop!(h)
#  
# Example using pure vectors (avoids making any copies):
#   z = rand(8)
#   vector2heap!(z)
#   isheap(z)
#   min_val = heap_pop!(z)
#   heap2rsorted!(z)

## Function definitions for operating on vectors ##


# This is a "percolate down" function, used by several other
# functions. "percolate up" is implemented in heap_push.
function _heapify!{T}(cmp::Function,x::Vector{T},i::Int,len::Int)
    il = 2*i      # index of left child
    while il <= len
        # Among node i and its two children, find the extreme value
        #(e.g., the smallest when using < for comparison)
        iextreme = cmp(x[il],x[i]) ? il : i  # index of extreme value
        if il < len
            iextreme = cmp(x[il+1],x[iextreme]) ? il+1 : iextreme
        end
        if iextreme == i
            return   # The heap below this node is fine
        end
        # Put the extreme value at i via a swap
        tmp = x[iextreme]
        x[iextreme] = x[i]
        x[i] = tmp
        # Descend to the modified child
        i = iextreme
        il = 2*i
    end
end


# Convert an arbitrary vector into heap storage format
function vector2heap!{T}(cmp::Function, x::Vector{T})
    for i = convert(Int,ifloor(length(x)/2)):-1:1
        _heapify!(cmp,x,i,length(x))
    end
end
function vector2heap!{T}(x::Vector{T})
    vector2heap!(<,x)
end

# Test whether a vector is a valid heap
function isheap{T}(cmp::Function, x::Vector{T})
    for i = 1:convert(Int,ifloor(length(x)/2))
        i2 = 2*i
        if !cmp(x[i],x[i2])
            return false
        end
        if i2 < length(x) && !cmp(x[i],x[i2+1])
            return false
        end
    end
    return true
end
function isheap{T}(x::Vector{T})
    isheap(<,x)
end

# Add a new item to a heap
function heap_push!{T}(cmp::Function, x::Vector{T},item::T)
    # Append new element at the bottom
    push(x,item)
    # Let the new item percolate up until stopped by a more-extreme parent
    i = length(x)
    ip = convert(Int,ifloor(i/2))  # index of parent
    while i > 1 && cmp(x[i],x[ip])
        # Swap i and its parent
        tmp = x[ip]
        x[ip] = x[i]
        x[i] = tmp
        # Traverse up the tree
        i = ip
        ip = convert(Int,ifloor(i/2))
    end
end
function heap_push!{T}(x::Vector{T},item::T)
    heap_push!(<,x,item)
end

# Remove the root node from the heap, leaving the remaining values in
# a valid heap
function heap_pop!{T}(cmp::Function, x::Vector{T})
    # Save the value we want to return
    extreme_item = x[1]
    # We need to shorten the list, so replace the former top with the
    # last item, then let it percolate down
    x[1] = x[end]
    pop(x)
    _heapify!(cmp,x,1,length(x))
    return extreme_item
end
function heap_pop!{T}(x::Vector{T})
    extreme_item = heap_pop!(<,x)
    return extreme_item
end

# From a heap, return a sorted vector. This is implemented efficiently
# if the sorting is in the reverse order of the comparison function.
function heap2rsorted!{T}(cmp::Function, x::Vector{T})
    for i = length(x):-1:2
        # Swap the root with i, the last unsorted position
        tmp = x[1]
        x[1] = x[i]
        x[i] = tmp
        # The heap portion now has length i-1, but needs fixing up
        # starting with the root
        _heapify!(cmp,x,1,i-1)
    end
end
function heap2rsorted!{T}(x::Vector{T})
    heap2rsorted!(<,x)
end



## Heap object ##

type Heap{T}
    cmp::Function
    data::Array{T,1}

    function Heap(cmp::Function,x::Array{T,1})
        data = copy(x)
        vector2heap!(cmp,data)
        new(cmp,data)
    end
end
Heap{T}(cmp::Function,x::Vector{T}) = Heap{T}(cmp,x)
Heap{T}(x::Vector{T}) = Heap{T}(<,x)
Heap{T}(cmp::Function,::Type{T}) = Heap{T}(cmp,zeros(T,0))
Heap{T}(::Type{T}) = Heap{T}(<,zeros(T,0))

function push!{T}(h::Heap{T},item::T)
    heap_push!(h.cmp,h.data,item)
end

function pop!{T}(h::Heap{T})
    extreme_item = heap_pop!(h.cmp,h.data)
    return extreme_item
end

function length{T}(h::Heap{T})
    return length(h.data)
end
