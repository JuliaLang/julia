# This file is a part of Julia. License is MIT: https://julialang.org/license

mutable struct InvasiveLinkedList{T}
    # Invasive list requires that T have a field `.next >: U{T, Nothing}` and `.queue >: U{ILL{T}, Nothing}`
    head::Union{T, Nothing}
    tail::Union{T, Nothing}
    InvasiveLinkedList{T}() where {T} = new{T}(nothing, nothing)
end

#const list_append!! = append!
#const list_deletefirst! = delete!

eltype(::Type{<:InvasiveLinkedList{T}}) where {T} = @isdefined(T) ? T : Any

iterate(q::InvasiveLinkedList) = (h = q.head; h === nothing ? nothing : (h, h))
iterate(q::InvasiveLinkedList{T}, v::T) where {T} = (h = v.next; h === nothing ? nothing : (h, h))

isempty(q::InvasiveLinkedList) = (q.head === nothing)

function length(q::InvasiveLinkedList)
    i = 0
    head = q.head
    while head !== nothing
        i += 1
        head = head.next
    end
    return i
end

function list_append!!(q::InvasiveLinkedList{T}, q2::InvasiveLinkedList{T}) where T
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

function push!(q::InvasiveLinkedList{T}, val::T) where T
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

function pushfirst!(q::InvasiveLinkedList{T}, val::T) where T
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

function pop!(q::InvasiveLinkedList{T}) where {T}
    val = q.tail::T
    list_deletefirst!(q, val) # expensive!
    return val
end

function popfirst!(q::InvasiveLinkedList{T}) where {T}
    val = q.head::T
    list_deletefirst!(q, val) # cheap
    return val
end

function list_deletefirst!(q::InvasiveLinkedList{T}, val::T) where T
    val.queue === q || return
    head = q.head::T
    if head === val
        if q.tail::T === val
            q.head = q.tail = nothing
        else
            q.head = val.next::T
        end
    else
        head_next = head.next
        while head_next !== val
            head = head_next
            head_next = head.next::Union{T, Nothing}
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
    queue::Union{InvasiveLinkedList{LinkedListItem{T}}, Nothing}
    value::T
    LinkedListItem{T}(value::T) where {T} = new{T}(nothing, nothing, value)
end
const LinkedList{T} = InvasiveLinkedList{LinkedListItem{T}}

# delegate methods, as needed
eltype(::Type{<:LinkedList{T}}) where {T} = @isdefined(T) ? T : Any
iterate(q::LinkedList) = (h = q.head; h === nothing ? nothing : (h.value, h))
iterate(q::InvasiveLinkedList{LLT}, v::LLT) where {LLT<:LinkedListItem} = (h = v.next; h === nothing ? nothing : (h.value, h))
push!(q::LinkedList{T}, val::T) where {T} = push!(q, LinkedListItem{T}(val))
pushfirst!(q::LinkedList{T}, val::T) where {T} = pushfirst!(q, LinkedListItem{T}(val))
pop!(q::LinkedList) = invoke(pop!, Tuple{InvasiveLinkedList,}, q).value
popfirst!(q::LinkedList) = invoke(popfirst!, Tuple{InvasiveLinkedList,}, q).value
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
