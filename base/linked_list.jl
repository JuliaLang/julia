# This file is a part of Julia. License is MIT: https://julialang.org/license

mutable struct IntrusiveLinkedList{T}
    # Invasive list requires that T have a field `.next >: U{T, Nothing}` and `.queue::Any`
    head::Union{T, Nothing}
    tail::Union{T, Nothing}
    IntrusiveLinkedList{T}() where {T} = new{T}(nothing, nothing)
end

# A reference to an intrusive list together with the identity recorded in the
# elements' `queue` field while they are enqueued (the element's "which queue am
# I on" witness). Plain lists use the list object itself as the identity; lists
# that are owned by some other object (a synchronized workqueue wrapper, a
# condition, a waited-on object) record that owner instead, so that code
# holding only the element can identify the owner - and thus the lock
# protecting the list - from the element alone.
struct ILLRef{T}
    list::IntrusiveLinkedList{T}
    waitee::Any # Invariant: waitqueue(waitee).list === list
end

# waitqueue(x) returns the ILLRef for the queue of waiters registered on `x`.
# Methods are added for each waitee type (conditions, tasks, workqueues, ...).
function waitqueue end

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

isempty(qr::ILLRef) = isempty(qr.list)
length(qr::ILLRef) = length(qr.list)

function push!(qr::ILLRef{T}, val::T) where T
    val.queue === nothing || error("val already in a list")
    val.queue = qr.waitee
    q = qr.list
    tail = q.tail
    if tail === nothing
        q.head = q.tail = val
    else
        tail.next = val
        q.tail = val
    end
    return q
end

function pushfirst!(qr::ILLRef{T}, val::T) where T
    val.queue === nothing || error("val already in a list")
    val.queue = qr.waitee
    q = qr.list
    head = q.head
    if head === nothing
        q.head = q.tail = val
    else
        val.next = head
        q.head = val
    end
    return q
end

function pop!(qr::ILLRef{T}) where {T}
    val = qr.list.tail::T
    _list_deletefirst!(qr.list, val) # expensive!
    return val
end

function popfirst!(qr::ILLRef{T}) where {T}
    val = qr.list.head::T
    _list_deletefirst!(qr.list, val) # cheap
    return val
end

# Delete `val` from the list, but only if it is actually in it, as witnessed by
# `val.queue` holding the ILLRef's waitee. This makes deletion a no-op if `val`
# was concurrently popped, which various cleanup paths rely upon.
function list_deletefirst!(qr::ILLRef{T}, val::T) where T
    val.queue === qr.waitee || return qr.list
    return _list_deletefirst!(qr.list, val)
end

push!(q::IntrusiveLinkedList{T}, val::T) where T = push!(ILLRef(q, q), val)
pushfirst!(q::IntrusiveLinkedList{T}, val::T) where T = pushfirst!(ILLRef(q, q), val)
pop!(q::IntrusiveLinkedList{T}) where T = pop!(ILLRef(q, q))
popfirst!(q::IntrusiveLinkedList{T}) where T = popfirst!(ILLRef(q, q))
list_deletefirst!(q::IntrusiveLinkedList{T}, val::T) where T = list_deletefirst!(ILLRef(q, q), val)

# this function assumes `val` is found in `q`
function _list_deletefirst!(q::IntrusiveLinkedList{T}, val::T) where T
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
