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
    flags::UInt32
    args::Union{Tuple{},Vector{RawSyntaxNode}}
    # has_diagnostics::Bool
end

const _RawFlags = UInt32
TRIVIA_FLAG = 0x00000001
INFIX_FLAG = 0x00000002

function raw_flags(; trivia::Bool=false, infix::Bool=false)
    flags = _RawFlags(0)
    trivia && (flags |= TRIVIA_FLAG)
    infix  && (flags |= INFIX_FLAG)
    return flags::_RawFlags
end

function RawSyntaxNode(kind::Kind, span::Int, flags::_RawFlags=0x00000000)
    RawSyntaxNode(kind, span, flags, ())
end

function RawSyntaxNode(raw::TzTokens.RawToken)
    span = 1 + raw.endbyte - raw.startbyte
    RawSyntaxNode(kind(raw), span, 0, FIXME)
end

function RawSyntaxNode(kind::Kind, flags::_RawFlags, args::RawSyntaxNode...)
    span = sum(x.span for x in args)
    RawSyntaxNode(kind, span, flags, RawSyntaxNode[args...])
end

function RawSyntaxNode(kind::Kind, args::RawSyntaxNode...)
    RawSyntaxNode(kind, _RawFlags(0), args...)
end

# Acessors / predicates
haschildren(node::RawSyntaxNode) = !(node.args isa Tuple{})
children(node::RawSyntaxNode) = node.args

istrivia(node::RawSyntaxNode) = node.flags & TRIVIA_FLAG != 0
isinfix(node::RawSyntaxNode)  = node.flags & INFIX_FLAG != 0

kind(node::RawSyntaxNode) = node.kind

# Pretty printing
function _show_raw_node(io, node, indent, pos, str, show_trivia)
    if !show_trivia && istrivia(node)
        return
    end
    if !haschildren(node)
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
            _show_raw_node(io, a, new_indent, p, str, show_trivia)
            p += a.span
        end
    end
end

function Base.show(io::IO, ::MIME"text/plain", node::RawSyntaxNode)
    _show_raw_node(io, node, "", 1, nothing, true)
end

function Base.show(io::IO, ::MIME"text/plain", node::RawSyntaxNode, str::String; show_trivia=true)
    _show_raw_node(io, node, "", 1, str, show_trivia)
end

#-------------------------------------------------------------------------------
# AST interface, built on top of raw tree

mutable struct SyntaxNode
    raw::RawSyntaxNode
    position::Int
    parent::Union{Nothing,SyntaxNode}
    head::Symbol
    val::Any
end

function SyntaxNode(raw::RawSyntaxNode, position::Int, code::String)
    if !haschildren(raw)
        # Leaf node
        k = raw.kind
        val_range = position:position + raw.span - 1
        val_str = @view code[val_range]
        # Here we parse the values eagerly rather than representing them as
        # strings. Maybe this is good. Maybe not.
        if k == K"Integer"
            val = Base.parse(Int, val_str)
        elseif k == K"Identifier"
            val = Symbol(val_str)
        elseif isoperator(k)
            val = Symbol(val_str)
        else
            error("Can't parse literal of kind $k")
        end
        return SyntaxNode(raw, position, nothing, :leaf, val)
    else
        k = raw.kind
        head = k == K"call"     ? :call     :
               k == K"toplevel" ? :toplevel :
               k == K"block"    ? :block    :
               k == K"for"      ? :for      :
               k == K"="        ? :(=)      :
               error("Unknown head of kind $k")
        cs = SyntaxNode[]
        pos = position
        for (i,rawchild) in enumerate(children(raw))
            if !istrivia(rawchild)
                push!(cs, SyntaxNode(rawchild, pos, code))
            end
            pos += rawchild.span
        end
        # Julia's standard `Expr` ASTs have children stored in a canonical
        # order which is not always source order.
        #
        # Swizzle the children here as necessary to get the canonical order.
        if isinfix(raw)
            cs[2], cs[1] = cs[1], cs[2]
        end
        node = SyntaxNode(raw, position, nothing, head, cs)
        for c in cs
            c.parent = node
        end
        return node
    end
end

haschildren(node::SyntaxNode) = node.head !== :leaf
children(node::SyntaxNode) = haschildren(node) ? node.val::Vector{SyntaxNode} : ()

function _show_syntax_node(io, node, indent)
    if !haschildren(node)
        line = string(rpad(node.position, 4), indent, node.val)
        println(io, line)
        # rpad(line, 40), repr(str[node.position:node.position + node.span - 1]))
    else
        println(io, rpad(node.position, 4), indent, '[', _kind_str(kind(node.raw)), ']')
        new_indent = indent*"  "
        for n in children(node)
            _show_syntax_node(io, n, new_indent)
        end
    end
end

function Base.show(io::IO, ::MIME"text/plain", node::SyntaxNode)
    _show_syntax_node(io, node, "")
end


#-------------------------------------------------------------------------------
# Tree utilities
"""
    child(node, i1, i2, ...)

Get child at a tree path. If indexing accessed children, it would be
`node[i1][i2][...]`
"""
function child(node, path::Integer...)
    n = node
    for index in path
        n = children(n)[index]
    end
    return n
end

"""
Get absolute position and span of the child of `node` at the given tree `path`.
"""
function child_position_span(node::RawSyntaxNode, path::Int...)
    n = node
    p = 1
    for index in path
        cs = children(n)
        for i = 1:index-1
            p += cs[i].span
        end
        n = cs[index]
    end
    return n, p, n.span
end

function child_position_span(node::SyntaxNode, path::Int...)
    n = child(node, path...)
    n, n.position, n.raw.span
end

"""
Print the code, highlighting the part covered by `node` at tree `path`.
"""
function highlight(code::String, node, path::Int...)
    node, p, span = child_position_span(node, path...)
    q = p + span
    print(code[1:p-1])
    first = true
    for linepart in split(code[p:q-1], '\n')
        first || print('\n')
        print("\e[48;2;20;50;20m", linepart, "\e[0;0m")
        first = false
    end
    print(code[q:end])
end
