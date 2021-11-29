#-------------------------------------------------------------------------------
# Syntax tree types

# Desired rules of lossless syntax trees:
#
# * Every source byte is covered by the tree
# * The children (including trivia) cover the full span of the parent
# * Children occur in source order
#
# Additionally
# * Nodes should be position-independent so that reparsing doesn't disturb them,
#   and so that it's possible to pool and reuse them (especially leaf nodes!)

# The rawest version of a parse tree node.
struct RawSyntaxNode
    kind::Kind
    span::Int
    args::Vector{RawSyntaxNode}
    # has_diagnostics::Bool
end

function RawSyntaxNode(kind::Kind, span::Int)
    RawSyntaxNode(kind, span, RawSyntaxNode[])
end

function RawSyntaxNode(raw::TzTokens.RawToken)
    span = 1 + raw.endbyte - raw.startbyte
    RawSyntaxNode(kind(raw), span)
end

function RawSyntaxNode(kind::Kind, args::RawSyntaxNode...)
    span = sum(x.span for x in args)
    RawSyntaxNode(kind, span, RawSyntaxNode[args...])
end

function _show_node(io, node, indent, pos, str)
    if isempty(node.args)
        line = string(rpad(node.span, 4), indent, _kind_str(node.kind))
        if isnothing(str)
            println(io, line)
        else
            println(io, rpad(line, 40), repr(str[pos:pos + node.span - 1]))
        end
    else
        println(io, rpad(node.span, 4), indent, '[', _kind_str(node.kind), "]")
        new_indent = indent*"  "
        p = pos
        for a in node.args
            _show_node(io, a, new_indent, p, str)
            p += a.span
        end
    end
end

function Base.show(io::IO, ::MIME"text/plain", node::RawSyntaxNode)
    _show_node(io, node, "", 1, nothing)
end

function Base.show(io::IO, ::MIME"text/plain", node::RawSyntaxNode, str::String)
    _show_node(io, node, "", 1, str)
end

kind(node::RawSyntaxNode) = node.kind


