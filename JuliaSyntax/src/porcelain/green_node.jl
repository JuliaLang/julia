"""
    struct GreenNode

An explicit pointer-y representation of the green tree produced by the parser.
See [`RawGreenNode`](@ref) for documentation on working with the implicit green
tree directly. However, this representation is useful for introspection as it
provides O(1) access to the children (as well as forward iteration).
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
Base.lastindex(node::GreenNode) = children(node) === nothing ? 0 : length(children(node))

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

function Base.hash(node::GreenNode, h::UInt)
    children = node.children
    if children === nothing
        h = hash(nothing, h)
    else # optimization - avoid extra allocations from `hash(::AbstractVector, ::UInt)`
        for child in children
            h = hash(child, h)
        end
    end
    hash(node.head, hash(node.span, h))
end
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

function _show_green_node_sexpr(io, node::GreenNode, position)
    if is_leaf(node)
        print(io, position, "-", position+node.span-1, "::", untokenize(head(node); unique=false))
    else
        print(io, "(", untokenize(head(node); unique=false))
        p = position
        for n in children(node)
            print(io, ' ')
            _show_green_node_sexpr(io, n, p)
            p += n.span
        end
        print(io, ')')
    end
end

function Base.show(io::IO, node::GreenNode)
    _show_green_node_sexpr(io, node, 1)
end

function GreenNode(cursor::GreenTreeCursor)
    chead = head(cursor)
    T = typeof(chead)
    if is_leaf(cursor)
        return GreenNode{T}(head(cursor), span(cursor), nothing)
    else
        children = GreenNode{T}[]
        for child in reverse(cursor)
            pushfirst!(children, GreenNode(child))
        end
        return GreenNode{T}(head(cursor), span(cursor), children)
    end
end

function build_tree(::Type{GreenNode}, stream::ParseStream;
                    # unused, but required since `_parse` is written generic
                    filename=nothing, first_line=1, keep_parens=false)
    cursor = GreenTreeCursor(stream)
    if has_toplevel_siblings(cursor)
        # There are multiple toplevel nodes, e.g. because we're using this
        # to test a partial parse. Wrap everything in K"wrapper"
        all_processed = 0
        local cs
        for child in reverse_toplevel_siblings(cursor)
            c = GreenNode(child)
            if !@isdefined(cs)
                cs = GreenNode{SyntaxHead}[c]
            else
                pushfirst!(cs, c)
            end
        end
        @assert length(cs) != 1
        return GreenNode(SyntaxHead(K"wrapper", NON_TERMINAL_FLAG), stream.next_byte-1, cs)
    else
        return GreenNode(cursor)
    end
end
