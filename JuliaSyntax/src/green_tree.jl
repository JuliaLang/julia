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
* Nodes are homogenously typed at the language level so they can be stored
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
    args::Union{Tuple{},Vector{GreenNode{Head}}}
end

function GreenNode(head::Head, span::Integer, args) where {Head}
    GreenNode{Head}(head, span, args)
end

# Accessors / predicates
haschildren(node::GreenNode) = !(node.args isa Tuple{})
children(node::GreenNode)    = node.args
span(node::GreenNode)        = node.span
head(node::GreenNode)        = node.head

Base.summary(node::GreenNode) = summary(node.head)

function Base.:(==)(n1::GreenNode, n2::GreenNode)
    n1.head == n2.head && n1.span == n2.span && n1.args == n2.args
end

# Pretty printing
function _show_green_node(io, node, indent, pos, str, show_trivia)
    if !show_trivia && is_trivia(node)
        return
    end
    posstr = "$(lpad(pos, 6)):$(rpad(pos+span(node)-1, 6)) │"
    is_leaf = !haschildren(node)
    if is_leaf
        line = string(posstr, indent, summary(node))
    else
        line = string(posstr, indent, '[', summary(node), ']')
    end
    if !is_trivia(node) && is_leaf
        line = rpad(line, 40) * "✔"
    end
    if is_error(node)
        line = rpad(line, 41) * "✘"
    end
    if is_leaf && !isnothing(str)
        line = string(rpad(line, 43), ' ', repr(str[pos:prevind(str, pos + span(node))]))
    end
    line = line*"\n"
    if is_error(node)
        printstyled(io, line, color=:light_red)
    else
        print(io, line)
    end
    if !is_leaf
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
        isnothing(cs) ? GreenNode(h, span, ()) :
                        GreenNode(h, span, collect(GreenNode{SyntaxHead}, cs))
    end
end

