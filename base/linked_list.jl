# This file is a part of Julia. License is MIT: https://julialang.org/license

mutable struct IntrusiveLinkedList{T}
    # Invasive list requires that T have a field `.next >: U{T, Nothing}` and `.queue::Any`
    head::Union{T, Nothing}
    tail::Union{T, Nothing}
    IntrusiveLinkedList{T}() where {T} = new{T}(nothing, nothing)
end

struct ILLRef{T}
    list::IntrusiveLinkedList{T}
    waitee::Any # Invariant: waitqueue(waitee).list === list
end
ILLRef(ref::ILLRef, @nospecialize(waitee)) = typeof(ref)(ref.list, waitee)
waitqueue(list::IntrusiveLinkedList{T}) where {T} = ILLRef(list, list)

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

isempty(qr::ILLRef{T}) where T = isempty(qr.list)
length(qr::ILLRef{T}) where T = length(qr.list)

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

# Delete `val` from the list, but only if it is actually in it, as indicated by
# `val.queue` holding the ILLRef's waitee. This makes deletion a no-op if `val`
# was concurrently notified/popped, matching the behavior various cleanup paths
# rely upon.
function list_deletefirst!(qr::ILLRef{T}, val::T) where T
    val.queue === qr.waitee || return qr.list
    return _list_deletefirst!(qr.list, val)
end

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

function in(val::T, list::IntrusiveLinkedList{T}) where T
    head = list.head
    while head !== nothing
        if val === head
            return true
        end
        head = head.next
    end
    return false
end

# TODO: Delete this compatibility wrapper
list_deletefirst!(q::IntrusiveLinkedList{T}, val::T) where T = list_deletefirst!(ILLRef(q, q), val)
push!(q::IntrusiveLinkedList{T}, val::T) where T = push!(ILLRef(q, q), val)
pushfirst!(q::IntrusiveLinkedList{T}, val::T) where T = pushfirst!(ILLRef(q, q), val)
pop!(q::IntrusiveLinkedList{T}) where T = pop!(ILLRef(q, q))
popfirst!(q::IntrusiveLinkedList{T}) where T = popfirst!(ILLRef(q, q))

# Generic cleanup entry point: delete `val` from the wait queue of whatever
# waitee it is currently queued on (no-op if it is not queued). Requires
# `waitqueue(waitee)` to be defined for the waitee.
list_deletefirst!(@nospecialize(waitee), val) = list_deletefirst!(withwaitee(waitqueue(waitee), waitee), val)

## The wait-queue flavor of the intrusive list: the elements are parked
## `Task`s, linked through their dedicated `wait_next`/`wait_queue` fields
## (the scheduler owns `next`/`queue`; see the wait-state comment in
## cancellation.jl for why the two link sets must be disjoint). Structure
## and semantics mirror IntrusiveLinkedList/ILLRef above.

mutable struct WaitQueue
    head::Union{Task, Nothing}
    tail::Union{Task, Nothing}
    WaitQueue() = new(nothing, nothing)
end

struct WaitQueueRef
    list::WaitQueue
    waitee::Any # Invariant: waitqueue(waitee).list === list
end

withwaitee(r::ILLRef, @nospecialize(waitee)) = typeof(r)(r.list, waitee)
withwaitee(r::WaitQueueRef, @nospecialize(waitee)) = WaitQueueRef(r.list, waitee)

eltype(::Type{WaitQueue}) = Task

iterate(q::WaitQueue) = (h = q.head; h === nothing ? nothing : (h, h))
iterate(q::WaitQueue, v::Task) = (h = v.wait_next; h === nothing ? nothing : (h::Task, h::Task))

isempty(q::WaitQueue) = (q.head === nothing)

function length(q::WaitQueue)
    i = 0
    head = q.head
    while head !== nothing
        i += 1
        head = (head::Task).wait_next
    end
    return i
end

isempty(qr::WaitQueueRef) = isempty(qr.list)
length(qr::WaitQueueRef) = length(qr.list)

function push!(qr::WaitQueueRef, val::Task)
    val.wait_queue === nothing || error("val already in a list")
    val.wait_queue = qr.waitee
    q = qr.list
    tail = q.tail
    if tail === nothing
        q.head = q.tail = val
    else
        tail.wait_next = val
        q.tail = val
    end
    return q
end

function pushfirst!(qr::WaitQueueRef, val::Task)
    val.wait_queue === nothing || error("val already in a list")
    val.wait_queue = qr.waitee
    q = qr.list
    head = q.head
    if head === nothing
        q.head = q.tail = val
    else
        val.wait_next = head
        q.head = val
    end
    return q
end

function pop!(qr::WaitQueueRef)
    val = qr.list.tail::Task
    _list_deletefirst!(qr.list, val) # expensive!
    return val
end

function popfirst!(qr::WaitQueueRef)
    val = qr.list.head::Task
    _list_deletefirst!(qr.list, val) # cheap
    return val
end

# Delete `val` from the list, but only if it is actually in it, as indicated
# by `val.wait_queue` holding the ref's waitee; a no-op if `val` was
# concurrently notified/popped.
function list_deletefirst!(qr::WaitQueueRef, val::Task)
    val.wait_queue === qr.waitee || return qr.list
    return _list_deletefirst!(qr.list, val)
end

# Raw-list convenience (the unbuffered-channel handoff pops the first
# parked taker without caring about the recorded waitee identity).
popfirst!(q::WaitQueue) = popfirst!(WaitQueueRef(q, q))

# this function assumes `val` is found in `q`
function _list_deletefirst!(q::WaitQueue, val::Task)
    head = q.head::Task
    if head === val
        if q.tail::Task === val
            q.head = q.tail = nothing
        else
            q.head = val.wait_next::Task
        end
    else
        head_next = head.wait_next::Task
        while head_next !== val
            head = head_next
            head_next = head.wait_next::Task
        end
        if q.tail::Task === val
            head.wait_next = nothing
            q.tail = head
        else
            head.wait_next = val.wait_next::Task
        end
    end
    val.wait_next = nothing
    val.wait_queue = nothing
    return q
end

## The lock-contention flavor of the wait queue: parked acquirers of a
## ReentrantLock, linked through the tasks' dedicated `lock_next`/`lock_queue`
## fields. This is a *third* link set (after the scheduler's `next`/`queue`
## and the condition-wait node's `wait_next`/`wait_queue`): a task whose
## cancelled condition wait left a stale, lazily-collected entry in the
## condition's queue must still be able to park to reacquire the lock, so
## lock parking may not depend on the condition-wait links.

mutable struct LockWaitQueue
    head::Union{Task, Nothing}
    tail::Union{Task, Nothing}
    LockWaitQueue() = new(nothing, nothing)
end

struct LockQueueRef
    list::LockWaitQueue
    waitee::Any # Invariant: waitqueue(waitee).list === list
end

withwaitee(r::LockQueueRef, @nospecialize(waitee)) = LockQueueRef(r.list, waitee)

eltype(::Type{LockWaitQueue}) = Task

isempty(q::LockWaitQueue) = (q.head === nothing)
isempty(qr::LockQueueRef) = isempty(qr.list)

function length(q::LockWaitQueue)
    i = 0
    head = q.head
    while head !== nothing
        i += 1
        head = (head::Task).lock_next
    end
    return i
end
length(qr::LockQueueRef) = length(qr.list)

function push!(qr::LockQueueRef, val::Task)
    val.lock_queue === nothing || error("val already in a list")
    val.lock_queue = qr.waitee
    q = qr.list
    tail = q.tail
    if tail === nothing
        q.head = q.tail = val
    else
        tail.lock_next = val
        q.tail = val
    end
    return q
end

function popfirst!(qr::LockQueueRef)
    val = qr.list.head::Task
    _list_deletefirst!(qr.list, val) # cheap
    return val
end

# Delete `val` from the list, but only if it is actually in it; a no-op if
# `val` was concurrently popped by a release.
function list_deletefirst!(qr::LockQueueRef, val::Task)
    val.lock_queue === qr.waitee || return qr.list
    return _list_deletefirst!(qr.list, val)
end

# this function assumes `val` is found in `q`
function _list_deletefirst!(q::LockWaitQueue, val::Task)
    head = q.head::Task
    if head === val
        if q.tail::Task === val
            q.head = q.tail = nothing
        else
            q.head = val.lock_next::Task
        end
    else
        head_next = head.lock_next::Task
        while head_next !== val
            head = head_next
            head_next = head.lock_next::Task
        end
        if q.tail::Task === val
            head.lock_next = nothing
            q.tail = head
        else
            head.lock_next = val.lock_next::Task
        end
    end
    val.lock_next = nothing
    val.lock_queue = nothing
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
