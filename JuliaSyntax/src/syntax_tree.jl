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
    span::UInt32
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
    posstr = "$(lpad(pos, 6)):$(rpad(pos+node.span, 6)) |"
    if !haschildren(node)
        line = string(posstr, indent, _kind_str(node.kind))
        if isnothing(str)
            println(io, line)
        else
            println(io, rpad(line, 40), repr(str[pos:pos + node.span - 1]))
        end
    else
        println(io, posstr, indent, '[', _kind_str(node.kind), "]")
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
    source::SourceFile
    raw::RawSyntaxNode
    position::Int
    parent::Union{Nothing,SyntaxNode}
    head::Symbol
    val::Any
end

function SyntaxNode(source::SourceFile, raw::RawSyntaxNode, position::Integer=1)
    if !haschildren(raw)
        # Leaf node
        k = raw.kind
        val_range = position:position + raw.span - 1
        val_str = source[val_range]
        # Here we parse the values eagerly rather than representing them as
        # strings. Maybe this is good. Maybe not.
        if k == K"Integer"
            val = Base.parse(Int, val_str)
        elseif k == K"Identifier"
            val = Symbol(val_str)
        elseif k == K"String"
            val = unescape_string(source[position+1:position+raw.span-2])
        elseif isoperator(k)
            val = Symbol(val_str)
        else
            error("Can't parse literal of kind $k")
        end
        return SyntaxNode(source, raw, position, nothing, :leaf, val)
    else
        k = raw.kind
        head = k == K"call"     ? :call     :
               k == K"toplevel" ? :toplevel :
               k == K"block"    ? :block    :
               k == K"for"      ? :for      :
               k == K"="        ? :(=)      :
               k == K"$"        ? :$        :
               k == K"quote"    ? :quote    :
               error("Unknown head of kind $k")
        cs = SyntaxNode[]
        pos = position
        for (i,rawchild) in enumerate(children(raw))
            if !istrivia(rawchild)
                push!(cs, SyntaxNode(source, rawchild, pos))
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
        node = SyntaxNode(source, raw, position, nothing, head, cs)
        for c in cs
            c.parent = node
        end
        return node
    end
end

function interpolate_literal(node::SyntaxNode, val)
    @assert node.head == :$
    SyntaxNode(node.source, node.raw, node.position, node.parent, :leaf, val)
end

haschildren(node::SyntaxNode) = node.head !== :leaf
children(node::SyntaxNode) = haschildren(node) ? node.val::Vector{SyntaxNode} : ()

function _show_syntax_node(io, current_filename, node, indent)
    fname = node.source.filename
    #@info "" fname print_fname current_filename[] 
    line, col = source_location(node.source, node.position)
    posstr = "$(lpad(line, 4)):$(rpad(col,3))│$(lpad(node.position,6)):$(rpad(node.position+node.raw.span,6))│"
    nodestr = !haschildren(node) ?
              repr(node.val) :
              "[$(_kind_str(kind(node.raw)))]"
    treestr = string(indent, nodestr)
    # Add filename if it's changed from the previous node
    if fname != current_filename[]
        #println(io, "# ", fname)
        treestr = string(rpad(treestr, 40), "│$fname")
        current_filename[] = fname
    end
    println(io, posstr, treestr)
    if haschildren(node)
        new_indent = indent*"  "
        for n in children(node)
            _show_syntax_node(io, current_filename, n, new_indent)
        end
    end
end

function _show_syntax_node_compact(io, node)
    if !haschildren(node)
        print(io, repr(node.val))
    else
        print(io, "($(_kind_str(kind(node.raw))) ")
        first = true
        for n in children(node)
            first || print(io, ' ')
            _show_syntax_node_compact(io, n)
            first = false
        end
        print(io, ')')
    end
end

function Base.show(io::IO, ::MIME"text/plain", node::SyntaxNode)
    println(io, "line:col│ byte_range  │ tree                                   │ file_name")
    _show_syntax_node(io, Ref{Union{Nothing,String}}(nothing), node, "")
end

function Base.show(io::IO, node::SyntaxNode)
    _show_syntax_node_compact(io, node)
end

function Base.push!(node::SyntaxNode, child::SyntaxNode)
    if !haschildren(node)
        error("Cannot add children")
    end
    args = node.val::Vector{SyntaxNode}
    push!(args, child)
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

function setchild!(node::SyntaxNode, path, x)
    n1 = child(node, path[1:end-1]...)
    n1.val[path[end]] = x
end

# We can overload multidimensional Base.getindex / Base.setindex! for node
# types.
#
# The justification for this is to view a tree as a multidimensional ragged
# array, where descending depthwise into the tree corresponds to dimensions of
# the array.
#
# However... this analogy is only good for complete trees at a given depth (=
# dimension). But the syntax is oh-so-handy!
function Base.getindex(node::Union{SyntaxNode,RawSyntaxNode}, path::Int...)
    child(node, path...)
end
function Base.setindex!(node::SyntaxNode, x::SyntaxNode, path::Int...)
    setchild!(node, path, x)
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
function highlight(code::String, node, path::Int...; color=(40,40,70))
    node, p, span = child_position_span(node, path...)
    q = p + span
    print(stdout, code[1:p-1])
    _printstyled(stdout, code[p:q-1]; color)
    print(stdout, code[q:end])
end
