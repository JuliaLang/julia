# This file is a part of Julia. License is MIT: https://julialang.org/license

mutable struct IntrusiveLinkedList{T}
    # Invasive list requires that T have a field `.next >: U{T, Nothing}` and `.queue >: U{ILL{T}, Nothing}`
    head::Union{T, Nothing}
    tail::Union{T, Nothing}
    IntrusiveLinkedList{T}() where {T} = new{T}(nothing, nothing)
end

#const list_append!! = append!
#const list_deletefirst! = delete!

eltype(::Type{<:IntrusiveLinkedList{T}}) where {T} = @isdefined(T) ? T : Any

iterate(q::IntrusiveLinkedList) = (h = q.head; h === nothing ? nothing : (h, h))
iterate(q::IntrusiveLinkedList{T}, v::T) where {T} = (h = v.next; h === nothing ? nothing : (h, h))

isempty(q::IntrusiveLinkedList) = (q.head === nothing)

function length(q::IntrusiveLinkedList)
    i = 0
    head = q.head
    while head !== nothing
        i += 1
        head = head.next
    end
    return i
end

function list_append!!(q::IntrusiveLinkedList{T}, q2::IntrusiveLinkedList{T}) where T
    q === q2 && error("can't append list to itself")
    head2 = q2.head
    if head2 !== nothing
        tail2 = q2.tail::T
        q2.head = nothing
        q2.tail = nothing
        tail = q.tail
        q.tail = tail2
        if tail === nothing
            q.head = head2
        else
            tail.next = head2
        end
        while head2 !== nothing
            head2.queue = q
            head2 = head2.next
        end
    end
    return q
end

function push!(q::IntrusiveLinkedList{T}, val::T) where T
    val.queue === nothing || error("val already in a list")
    val.queue = q
    tail = q.tail
    if tail === nothing
        q.head = q.tail = val
    else
        tail.next = val
        q.tail = val
    end
    return q
end

function pushfirst!(q::IntrusiveLinkedList{T}, val::T) where T
    val.queue === nothing || error("val already in a list")
    val.queue = q
    head = q.head
    if head === nothing
        q.head = q.tail = val
    else
        val.next = head
        q.head = val
    end
    return q
end

function pop!(q::IntrusiveLinkedList{T}) where {T}
    val = q.tail::T
    list_deletefirst!(q, val) # expensive!
    return val
end

function popfirst!(q::IntrusiveLinkedList{T}) where {T}
    val = q.head::T
    list_deletefirst!(q, val) # cheap
    return val
end

# this function assumes `val` is found in `q`
function list_deletefirst!(q::IntrusiveLinkedList{T}, val::T) where T
    val.queue === q || return
    head = q.head::T
    if head === val
        if q.tail::T === val
            q.head = q.tail = nothing
        else
            q.head = val.next::T
        end
    else
        head_next = head.next::T
        while head_next !== val
            head = head_next
            head_next = head.next::T
        end
        if q.tail::T === val
            head.next = nothing
            q.tail = head
        else
            head.next = val.next::T
        end
    end
    val.next = nothing
    val.queue = nothing
    return q
end

#function list_deletefirst!(q::Array{T}, val::T) where T
#    i = findfirst(isequal(val), q)
#    i === nothing || deleteat!(q, i)
#    return q
#end


mutable struct LinkedListItem{T}
    # Adapter class to use any `T` in a LinkedList
    next::Union{LinkedListItem{T}, Nothing}
    queue::Union{IntrusiveLinkedList{LinkedListItem{T}}, Nothing}
    value::T
    LinkedListItem{T}(value::T) where {T} = new{T}(nothing, nothing, value)
end
const LinkedList{T} = IntrusiveLinkedList{LinkedListItem{T}}

# delegate methods, as needed
eltype(::Type{<:LinkedList{T}}) where {T} = @isdefined(T) ? T : Any
iterate(q::LinkedList) = (h = q.head; h === nothing ? nothing : (h.value, h))
iterate(q::IntrusiveLinkedList{LLT}, v::LLT) where {LLT<:LinkedListItem} = (h = v.next; h === nothing ? nothing : (h.value, h))
push!(q::LinkedList{T}, val::T) where {T} = push!(q, LinkedListItem{T}(val))
pushfirst!(q::LinkedList{T}, val::T) where {T} = pushfirst!(q, LinkedListItem{T}(val))
pop!(q::LinkedList) = invoke(pop!, Tuple{IntrusiveLinkedList,}, q).value
popfirst!(q::LinkedList) = invoke(popfirst!, Tuple{IntrusiveLinkedList,}, q).value
function list_deletefirst!(q::LinkedList{T}, val::T) where T
    h = q.head
    while h !== nothing
        if isequal(h.value, val)
            list_deletefirst!(q, h)
            break
        end
        h = h.next
    end
    return q
end
