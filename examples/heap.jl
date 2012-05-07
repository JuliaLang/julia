# heap.jl
#
# This file defines Heap objects and a variety of utility functions.
#
# Example usage of the Heap object:
# You can create an empty min-heap this way:
#   h = MinHeap(Float64)
#
# A more complex and complete example with a max-heap:
#   h = MaxHeap(rand(3))  # initialize heap with 3 random points
#   for i = 1:10
#     push!(h,rand())    # add more points
#   end
#   length(h)
#   max_val = pop!(h)
# 
# You can also work with indexed heaps:
#   z = rand(8)
#   h = MinHeapIndirect(eltype(z))
#   for i = 1:length(z)
#     push!(h,z[i])
#   end
#   min_index, min_val = pop!(h)
#
# Finally, you can do min-heaps using pure vectors. This avoids the
# need to make a copy of the data:
#   z = rand(8)
#   vector2heap!(z)
#   isheap(z)
#   min_val = heap_pop!(z)
#   heap2rsorted!(z)
# You can also do indirect min-heaps this way, by supplying an index
# vector as the first argument.

# Timothy E. Holy, 2012

## Function definitions for operating on vectors ##

# A "direct heap" is represented as a vector, each entry containing
# the "value" (key) of a particular item.  An "indirect heap" is
# stored as a vector of "integer pointers" (denoted by the variable
# iptr), and the key of node i is value[iptr[i]]. Representing the
# heap indirectly (requiring lookup of the value) has some cost, but
# is useful when the item index is the more fundamentally-interesting
# quantity.

# For an indirect heap, note that value stores the entire set of
# values ever added to the heap, whereas iptr reflects the current
# state of the heap.  Hence, value may be a longer vector than iptr,
# if items have been popped off the heap.

# These functions implement a min-heap. You can get a max-heap using
# the Heap objects below.


# This is a "percolate down" function, used by several other
# functions. "percolate up" is implemented in heap_push.
# direct version:
function _heapify!{T}(value::Vector{T},i::Int,len::Int)
    il = 2*i      # node index of left child
    while il <= len
        # Among node i and its two children, find the smallest value
        ismallest = isless(value[il],value[i]) ? il : i  # node index of smallest value
        if il < len
            ismallest = isless(value[il+1],value[ismallest]) ? il+1 : ismallest
        end
        if ismallest == i
            return   # The heap below this node is fine
        end
        # Put the smallest value at i via a swap of their iptrs
        value[ismallest], value[i] = value[i], value[ismallest]
        # Descend to the modified child
        i = ismallest
        il = 2*i
    end
end
# indirect version:
function _heapify!{T}(iptr::Vector{Int},value::Vector{T},i::Int,len::Int)
    il = 2*i
    while il <= len
        ismallest = isless(value[iptr[il]],value[iptr[i]]) ? il : i
        if il < len
            ismallest = isless(value[iptr[il+1]],value[iptr[ismallest]]) ? il+1 : ismallest
        end
        if ismallest == i
            return
        end
        iptr[ismallest], iptr[i] = iptr[i], iptr[ismallest]
        i = ismallest
        il = 2*i
    end
end


# Convert an arbitrary vector into heap storage format
function vector2heap!{T}(value::Vector{T})
    for i = convert(Int,ifloor(length(value)/2)):-1:1
        _heapify!(value,i,length(value))
    end
end
function vector2heap!{T}(iptr::Vector{Int},value::Vector{T})
    for i = convert(Int,ifloor(length(iptr)/2)):-1:1
        _heapify!(iptr,value,i,length(iptr))
    end
end

# Test whether a vector is a valid heap
function isheap{T}(value::Vector{T})
    for i = 1:convert(Int,ifloor(length(value)/2))
        i2 = 2*i
        if isless(value[i2],value[i])
            return false
        end
        if i2 < length(value) && isless(value[i2+1],value[i])
            return false
        end
    end
    return true
end
function isheap{T}(iptr::Vector{Int},value::Vector{T})
    for i = 1:convert(Int,ifloor(length(iptr)/2))
        i2 = 2*i
        if isless(value[iptr[i2]],value[iptr[i]])
            return false
        end
        if i2 < length(iptr) && isless(value[iptr[i2+1]],value[iptr[i]])
            return false
        end
    end
    return true
end

# Add a new item to a heap
function heap_push!{T}(value::Vector{T},newvalue::T)
    # Append the new value at the bottom
    push(value,newvalue)
    # Let the new item percolate up until stopped by a more-extreme parent
    i = length(value)
    ip = convert(Int,ifloor(i/2))  # index of parent
    while i > 1 && isless(value[i],value[ip])
        # Swap i and its parent
        value[i], value[ip] = value[ip], value[i]
        # Traverse up the tree
        i = ip
        ip = convert(Int,ifloor(i/2))
    end
end
function heap_push!{T}(iptr::Vector{Int},value::Vector{T},newvalue::T)
    push(value,newvalue)
    push(iptr,length(value))
    i = length(iptr)
    ip = convert(Int,ifloor(i/2))
    while i > 1 && isless(value[iptr[i]],value[iptr[ip]])
        iptr[i], iptr[ip] = iptr[ip], iptr[i]
        i = ip
        ip = convert(Int,ifloor(i/2))
    end
end

# Remove the root node from the heap, leaving the remaining values in
# a valid heap
function heap_pop!{T}(value::Vector{T})
    # Save the item we want to return
    extreme_item = value[1]
    # We need to shorten the list, so replace the former top with the
    # last item, then let it percolate down
    value[1] = value[end]
    pop(value)
    _heapify!(value,1,length(value))
    return extreme_item
end
function heap_pop!{T}(iptr::Vector{Int},value::Vector{T})
    extreme_item = iptr[1]
    iptr[1] = iptr[end]
    pop(iptr)
    _heapify!(iptr,value,1,length(iptr))
    return extreme_item
end

# From a heap, return a sorted vector. This is implemented efficiently
# if the sorting is in the reverse order of the comparison function.
function heap2rsorted!{T}(value::Vector{T})
    for i = length(value):-1:2
        # Swap the root with i, the last unsorted position
        value[1], value[i] = value[i], value[1]
        # The heap portion now has length i-1, but needs fixing up
        # starting with the root
        _heapify!(value,1,i-1)
    end
end
function heap2rsorted!{T}(iptr::Vector{Int},value::Vector{T})
    for i = length(iptr):-1:2
        iptr[1], iptr[i] = iptr[i], iptr[1]
        _heapify!(iptr,value,1,i-1)
    end
end



## Heap objects ##

abstract Heap
abstract HeapDirect <: Heap
abstract HeapIndirect <: Heap

# MinHeap
type MinHeap{T} <: HeapDirect
    value::Vector{T}
    
    function MinHeap(v::Vector{T})
        value = copy(v)
        vector2heap!(value)
        new(value)
    end
end
MinHeap{T}(v::Vector{T}) = MinHeap{T}(v)
MinHeap{T}(::Type{T}) = MinHeap{T}(zeros(T,0))

function push!{T}(h::MinHeap{T},item::T)
    heap_push!(h.value,item)
end

function pop!{T}(h::MinHeap{T})
    min_item = heap_pop!(h.value)
    return min_item
end

function length(h::HeapDirect)
    return length(h.value)
end

function isempty(h::HeapDirect)
    return isempty(h.value)
end

# MaxHeap
type MaxHeap{T} <: HeapDirect
    value::Vector{T}
    
    function MaxHeap(v::Vector{T})
        value = copy(v)
        vector2heap!(value)
        new(value)
    end
end
MaxHeap{T}(v::Vector{T}) = MaxHeap{T}(v)
MaxHeap{T}(::Type{T}) = MaxHeap{T}(zeros(T,0))

function push!{T}(h::MaxHeap{T},item::T)
    heap_push!(h.value,-item)
end

function pop!{T}(h::MaxHeap{T})
    max_item = -heap_pop!(h.value)
    return max_item
end

# MinHeapIndirect (an indexed heap)
type MinHeapIndirect{T} <: HeapIndirect
    index::Vector{Int}
    value::Vector{T}
    
    function MinHeapIndirect(v::Vector{T})
        value = copy(v)
        index = linspace(1,length(v),length(v))
        vector2heap!(index,value)
        new(index,value)
    end
end
#MinHeapIndirect{T}(i::Vector{Int},v::Vector{T}) = MinHeapIndirect{T}(i,v)
MinHeapIndirect{T}(v::Vector{T}) = MinHeapIndirect{T}(v)
MinHeapIndirect{T}(::Type{T}) = MinHeapIndirect{T}(zeros(T,0))

function push!{T}(h::MinHeapIndirect{T},newvalue::T)
    heap_push!(h.index,h.value,newvalue)
end

function pop!{T}(h::MinHeapIndirect{T})
    min_index = heap_pop!(h.index,h.value)
    return (min_index, h.value[min_index])
end

function length(h::HeapIndirect)
    return length(h.index)
end
function isempty(h::HeapIndirect)
    return isempty(h.index)
end

# MaxHeapIndirect (an indexed heap)
type MaxHeapIndirect{T} <: HeapIndirect
    index::Vector{Int}
    value::Vector{T}
    
    function MaxHeapIndirect(v::Vector{T})
        value = copy(v)
        index = linspace(1,length(v),length(v))
        vector2heap!(index,value)
        new(index,value)
    end
end
MaxHeapIndirect{T}(v::Vector{T}) = MaxHeapIndirect{T}(v)
MaxHeapIndirect{T}(::Type{T}) = MaxHeapIndirect{T}(zeros(T,0))

function push!{T}(h::MaxHeapIndirect{T},newvalue::T)
    heap_push!(h.index,h.value,-newvalue)
end

function pop!{T}(h::MaxHeapIndirect{T})
    min_index = heap_pop!(h.index,h.value)
    return min_index, -h.value[min_index]
end
