
module ConcurrentList #Concurrent Doubly Linked List

mutable struct Node{T}
    const value::Union{T, Nothing}
    @atomic next::Union{Node{T}, Nothing}
    @atomic prev::Union{Node{T}, Nothing}

    Node{T}(value, next, prev) where T = new{T}(value, next, prev)
    function Node(next::Node{T}) where T # Marker
        this = new{T}(nothing, next, nothing)
        @atomic :release this.prev = this
        return this
    end
end

Node(value::T, next, prev) where T = Node{T}(value, next, prev)

get_next(node::Node) = @atomic :acquire node.next
set_next(node::Node, next) = @atomic :release node.next = next
get_prev(node::Node) = @atomic :acquire node.prev
set_prev(node::Node, prev) = @atomic :release node.prev = prev
function cas_next(node::Node, exp::Node, desired::Node)
    _,success = @atomicreplace :acquire_release :monotonic node.next exp => desired
    return success
end
is_special(node::Node) = node.value === nothing
is_trailer(node::Node) = get_next(node) === nothing
is_header(node::Node) = get_prev(node) === nothing
is_marker(node::Node) = get_prev(node) === node

function is_deleted(node::Node)
    f = get_next(node)
    return f !== nothing && is_marker(f)
end

function next_nonmarker(node::Node)
    f = get_next(node)
    return (f === nothing || !is_marker(f)) ? f : get_next(f)
end

function Base.show(io::IO, node::Node)
    if is_special(node)
        if is_marker(node)
            print(io, "MarkerNode")
            return
        elseif is_header(node)
            next = get_next(node)
            if next === nothing
                print(io, "BrokenNode()")
                return
            elseif is_marker(node)
                print(io, "HeaderNode(next: MarkerNode)")
                return
            elseif is_trailer(next)
                print(io, "HeaderNode(next: TrailerNode)")
                return
            end
            print(io, "HeaderNode(next: ", next,")")
            return
        elseif is_trailer(node)
            prev = get_prev(node)
            if prev === nothing
                print(io, "BrokenNode()")
                return
            elseif is_marker(node)
                print(io, "TrailerNode(prev: MarkerNode)")
                return
            elseif is_header(prev)
                print(io, "TrailerNode(prev: HeaderNode)")
                return
            end
            print(io, "TrailerNode(prev: ", prev,")")
            return
        end
    end
    print(io, "Node(", node.value,")")
end

function successor(node::Node)
    f = next_nonmarker(node)
    while true
        if f === nothing
            return nothing
        end
        if !is_deleted(f)
            if get_prev(f) !== node && !is_deleted(node)
                set_prev(f, node) # relink f to node
            end
            return f
        end
        s = next_nonmarker(f)
        if f === get_next(node)
            cas_next(node, f, s)
        end
        f = s
    end
end

function find_predecessor_of(node::Node{T}, target::Node{T}) where {T}
    n = node
    while true
        f = successor(n)
        if (f === target)
            return n
        end
        if (f === nothing)
            return nothing
        end
        n = f
    end
end

function predecessor(node::Node)
    n = node
    while true
        b = get_prev(n)
        if (b === nothing)
            return find_predecessor_of(n, node)
        end
        s = get_next(b)
        if (s === node)
            return b
        end
        if (s === nothing || !is_marker(s))
            p = find_predecessor_of(b, node)
            if (p !== nothing)
                return p
            end
        end
        n = b
    end
end

function forward(node::Node)
    f = successor(node)
    return (f === nothing || is_special(f)) ? nothing : f
end

function back(node::Node)
    f = predecessor(node)
    return (f === nothing || is_special(f)) ? nothing : f
end

function append!(node::Node{T}, val::T) where {T}
    while true
        f = get_next(node)
        if (f === nothing || is_marker(f))
            return nothing
        end
        x = Node(val, f, node)
        if cas_next(node, f, x)
            set_prev(f, x)
            return x
        end
    end
end

function prepend!(node::Node{T}, val::T) where {T}
    while true
        b = predecessor(node)
        if b === nothing
            return nothing
        end
        x = Node(val, node, b)
        if cas_next(b, node, x)
            set_prev(node, x)
            return x
        end
    end
end

function delete!(node::Node)
    b = get_prev(node)
    f = get_next(node)
    if (b !== nothing && f !== nothing && !is_marker(f) && cas_next(node, f, Node(f)))
        if (cas_next(b, node, f))
            set_prev(f, b)
        end
        return true
    end
    return false
end

function replace!(node::Node{T}, val::T) where {T}
    while true
        b = get_prev(node)
        f = get_next(node)
        if (b === nothing || f === nothing || is_marker(f))
            return nothing
        end
        x = Node(val, f, b)
        if cas_next(node, f, Node(x))
            successor(b)
            successor(x)
            return x
        end
    end
end

function usable(node::Node)
    return node !== nothing && !is_special(node)
end

mutable struct ConcurrentDoublyLinkedList{T}
    @atomic header::Union{Node{T}, Nothing}
    @atomic trailer::Union{Node{T}, Nothing}
end

function ConcurrentDoublyLinkedList{T}() where {T}
    h = Node{T}(nothing, nothing, nothing)
    t = Node{T}(nothing, nothing, h)
    set_next(h, t)
    ConcurrentDoublyLinkedList{T}(h, t)
end

const CDLL = ConcurrentDoublyLinkedList

function Base.pushfirst!(cdll::CDLL{T}, val::T) where {T}
    while (append!((@atomic :acquire cdll.header), val) === nothing)
    end
end

function pushlast!(cdll::CDLL{T}, val::T) where {T}
    while (prepend!((@atomic :acquire cdll.trailer), val) === nothing)
    end
end

function Base.popfirst!(cdll::CDLL)
    while true
        n = successor((@atomic :acquire cdll.header))
        if !usable(n)
            return nothing
        end
        if delete!(n)
            return n.value
        end
    end
end

function poplast!(cdll::CDLL)
    while true
        n = predecessor((@atomic :acquire cdll.trailer))
        if !usable(n)
            return nothing
        end
        if delete!(n)
            return n.value
        end
    end
end

Base.push!(cdll::CDLL{T}, val::T) where {T} = pushfirst!(cdll, val)
Base.pop!(cdll::CDLL) = poplast!(cdll)
steal!(cdll::CDLL) = popfirst!(cdll)
Base.isempty(cdll::CDLL) = !usable(successor(@atomic :acquire cdll.header))

const Queue = CDLL

end