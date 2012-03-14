# These functions define a min-heap
# Example usage of the Heap object:
# You can create an empty min-heap this way:
#   h = Heap(Float64)
# This defaults to using isless for comparison.
# A more complex and complete example, yielding a max-heap:
#   h = Heap(>,rand(3))
#   for i = 1:10
#     push!(h,rand())
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

function _heapify!{T}(lessthan::Function,x::Vector{T},i::Int,len::Int)
    # Among node i and its two children, find the smallest
    il = 2*i;  # index of left child
    if il > len
        return
    end
    ismallest = lessthan(x[il],x[i]) ? il : i  # index of smallest child
    if il < len
        ismallest = lessthan(x[il+1],x[ismallest]) ? il+1 : ismallest
    end
    if ismallest != i
        # Put the smallest at i
        tmp = x[ismallest]
        x[ismallest] = x[i]
        x[i] = tmp
        # Recursively re-heapify
        _heapify!(lessthan,x,ismallest,len)
    end
end

function vector2heap!{T}(lessthan::Function, x::Vector{T})
    for i = convert(Int,ifloor(length(x)/2)):-1:1
        _heapify!(lessthan,x,i,length(x))
    end
end
function vector2heap!{T}(x::Vector{T})
    vector2heap!(isless,x)
end

function isheap{T}(lessthan::Function, x::Vector{T})
    for i = 1:convert(Int,ifloor(length(x)/2))
        i2 = 2*i
        if !lessthan(x[i],x[i2])
            return false
        end
        if i2 < length(x) && !lessthan(x[i],x[i2+1])
            return false
        end
    end
    return true
end
function isheap{T}(x::Vector{T})
    isheap(isless,x)
end

function heap_push!{T}(lessthan::Function, x::Vector{T},item::T)
    # append new element at the bottom
    push(x,item)
    # let the new item percolate up until it has a smaller parent
    i = length(x)
    ip = convert(Int,ifloor(i/2))  # index of parent
    while i > 1 && lessthan(x[i],x[ip])
        # swap i and its parent
        tmp = x[ip]
        x[ip] = x[i]
        x[i] = tmp
        # traverse up the tree
        i = ip
        ip = convert(Int,ifloor(i/2))  # index of parent
    end
end
function heap_push!{T}(x::Vector{T},item::T)
    heap_push!(isless,x,item)
end

function heap_pop!{T}(lessthan::Function, x::Vector{T})
    min_item = x[1]
    x[1] = x[end]
    pop(x)
    _heapify!(lessthan,x,1,length(x))
    return min_item
end
function heap_pop!{T}(x::Vector{T})
    min_item = heap_pop!(isless,x)
    return min_item
end

function heap2rsorted!{T}(lessthan::Function, x::Vector{T})
    for i = length(x):-1:2
        tmp = x[1]
        x[1] = x[i]
        x[i] = tmp
        _heapify!(lessthan,x,1,i-1)
    end
end
function heap2rsorted!{T}(x::Vector{T})
    heap2rsorted!(isless,x)
end



## Heap object ##

type Heap{T}
    lessthan::Function
    data::Array{T,1}

    function Heap(lessthan::Function,x::Array{T,1})
        data = copy(x)
        vector2heap!(lessthan,data)
        new(lessthan,data)
    end
end
Heap{T}(lessthan::Function,x::Vector{T}) = Heap{T}(lessthan,x)
Heap{T}(x::Vector{T}) = Heap{T}(isless,x)
Heap{T}(lessthan::Function,::Type{T}) = Heap{T}(lessthan,zeros(T,0))
Heap{T}(::Type{T}) = Heap{T}(isless,zeros(T,0))

function push!{T}(h::Heap{T},item::T)
    heap_push!(h.lessthan,h.data,item)
end

function pop!{T}(h::Heap{T})
    min_item = heap_pop!(h.lessthan,h.data)
    return min_item
end

function length{T}(h::Heap{T})
    return length(h.data)
end