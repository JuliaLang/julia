module Trees

import Base: eltype, get, show

export # types
    AbstractNode,
    AryNode,
    # functions
    ancestor,
    children,
    destroy!,
    isleaf,
    isroot,
    parent,
    root

abstract AbstractNode

# An AryNode will always have parent and children defined; data might not be. Use get() to access data.
type AryNode{T} <: AbstractNode
    parent
    children::Set
    data::T

    function AryNode{T}(p, data::T)  # if we declare p::AryNode, it seems to assume AryNode{T}
        if !isa(p, AryNode)
            error("Invalid parent of type ", typeof(p))
        end
        s = new(p, Set(), data)
        add!(p.children, s)
        s
    end
    function AryNode(p::AryNode)
        if !isa(p, AryNode)
            error("Invalid parent of type ", typeof(p))
        end
        s = new(p, Set())
        add!(p.children, s)
        s
    end
    # For the root node
    function AryNode()
        s = new()
        s.parent = s
        s.children = Set()
        s
    end
end
AryNode{T}(p::AryNode, data::T) = AryNode{T}(p, data)
AryNode{T}(::Type{T}) = AryNode{T}()
AryNode() = AryNode{Nothing}()

parent(node::AbstractNode) = node.parent

children(node::AryNode) = node.children

get(node::AryNode) = isdefined(node, :data) ? node.data : nothing

isroot(node::AbstractNode) = parent(node) == node

isleaf(node::AbstractNode) = isempty(children(node))

root(node::AryNode) = isroot(node) ? node : root(parent(node))

ancestor{T}(node::AryNode, ::Type{T}) = isa(get(node), T) || isroot(node) ? node : ancestor(parent(node), T)

function destroy!(node::AryNode)
    for c in node.children
        destroy!(c)
    end
    if !isroot(node)
        delete!(node.parent.children, node)
    end
    node.parent = node  # detach from the tree
    # Also call any data-specific cleanup
    if isdefined(node, :data)
        node.data = destroy!(node.data)
    end
end

destroy!(x) = x

# eltype(node::AryNode) = typeof(get(node))
eltype{T}(node::AryNode{T}) = T

function show(io::IO, node::AryNode)
    println(io, eltype(node), " node:")
    print(io, "  parent: ")
    if isroot(node)
        println(io, "<self>")
    elseif isroot(node.parent)
        println(io, "root node")
    else
        println(io, eltype(node.parent), " node")
    end
    print(io, "  children: ", childrenstring(node.children))
    data = get(node)
    if data != nothing
        print(io, "\n  data: ", data)
    end
end

function childrenstring(s::Set)
    nmax = 3
    if isempty(s)
        return "<none>"
    end
    buf = IOBuffer()
    i = 1
    state = start(s)
    while !done(s, state) && i <= nmax
        item, state = next(s, state)
        if i > 1
            print(buf, ", ")
        end
        print(buf, eltype(item))
        i += 1
    end
    if !done(s, state)
        print(buf, ", ...")
    end
    takebuf_string(buf)
end

end