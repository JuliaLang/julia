"""
    GreenNode(head, span)
    GreenNode(head, children...)

A "green tree" is a lossless syntax tree which overlays all the source text.
The most basic properties of a green tree are that:

* Nodes cover a contiguous span of bytes in the text
* Sibling nodes are ordered in the same order as the text

As implementation choices, we choose that:

* Nodes are immutable and don't know their parents or absolute position, so can
  be cached and reused
* Nodes are homogeneously typed at the language level so they can be stored
  concretely, with the `head` defining the node type. Normally this would
  include a "syntax kind" enumeration, but it can also include flags and record
  information the parser knew about the layout of the child nodes.
* For simplicity and uniformity, leaf nodes cover a single token in the source.
  This is like rust-analyzer, but different from Roslyn where leaves can
  include syntax trivia.
"""
struct GreenNode{Head}
    head::Head
    span::UInt32
    children::Union{Nothing,Vector{GreenNode{Head}}}
end

function GreenNode(head::Head, span::Integer, children=nothing) where {Head}
    GreenNode{Head}(head, span, children)
end

# Accessors / predicates
is_leaf(node::GreenNode)     = isnothing(node.children)
children(node::GreenNode)    = node.children
numchildren(node::GreenNode) = isnothing(node.children) ? 0 : length(node.children)
head(node::GreenNode)        = node.head

"""
    span(node)

Get the number of bytes this node covers in the source text.
"""
span(node::GreenNode) = node.span

Base.getindex(node::GreenNode, i::Int) = children(node)[i]
Base.getindex(node::GreenNode, rng::UnitRange) = view(children(node), rng)
Base.firstindex(node::GreenNode) = 1
Base.lastindex(node::GreenNode) = length(children(node))

"""
Get absolute position and span of the child of `node` at the given tree `path`.
"""
function child_position_span(node::GreenNode, path::Int...)
    n = node
    p = 1
    for index in path
        cs = children(n)
        for i = 1:index-1
            p += span(cs[i])
        end
        n = cs[index]
    end
    return n, p, n.span
end

function highlight(io::IO, source::SourceFile, node::GreenNode, path::Int...; kws...)
    _, p, span = child_position_span(node, path...)
    q = p + span - 1
    highlight(io, source, p:q; kws...)
end

Base.summary(node::GreenNode) = summary(node.head)

Base.hash(node::GreenNode, h::UInt) = hash((node.head, node.span, node.children), h)
function Base.:(==)(n1::GreenNode, n2::GreenNode)
    n1.head == n2.head && n1.span == n2.span && n1.children == n2.children
end

# Pretty printing
function _show_green_node(io, node, indent, pos, str, show_trivia)
    if !show_trivia && is_trivia(node)
        return
    end
    posstr = "$(lpad(pos, 6)):$(rpad(pos+span(node)-1, 6)) │"
    leaf = is_leaf(node)
    if leaf
        line = string(posstr, indent, summary(node))
    else
        line = string(posstr, indent, '[', summary(node), ']')
    end
    if !is_trivia(node) && leaf
        line = rpad(line, 40) * "✔"
    end
    if is_error(node)
        line = rpad(line, 41) * "✘"
    end
    if leaf && !isnothing(str)
        line = string(rpad(line, 43), ' ', repr(str[pos:prevind(str, pos + span(node))]))
    end
    line = line*"\n"
    if is_error(node)
        printstyled(io, line, color=:light_red)
    else
        print(io, line)
    end
    if !leaf
        new_indent = indent*"  "
        p = pos
        for x in children(node)
            _show_green_node(io, x, new_indent, p, str, show_trivia)
            p += x.span
        end
    end
end

function Base.show(io::IO, ::MIME"text/plain", node::GreenNode)
    _show_green_node(io, node, "", 1, nothing, true)
end

function Base.show(io::IO, ::MIME"text/plain", node::GreenNode, str::AbstractString; show_trivia=true)
    _show_green_node(io, node, "", 1, str, show_trivia)
end

function build_tree(::Type{GreenNode}, stream::ParseStream; kws...)
    build_tree(GreenNode{SyntaxHead}, stream; kws...) do h, srcrange, cs
        span = length(srcrange)
        isnothing(cs) ? GreenNode(h, span) :
                        GreenNode(h, span, collect(GreenNode{SyntaxHead}, cs))
    end
end

