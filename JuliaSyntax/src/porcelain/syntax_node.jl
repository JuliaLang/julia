#-------------------------------------------------------------------------------
# AST interface, built on top of raw tree

abstract type AbstractSyntaxData end

mutable struct TreeNode{NodeData}   # ? prevent others from using this with NodeData <: AbstractSyntaxData?
    parent::Union{Nothing,TreeNode{NodeData}}
    children::Union{Nothing,Vector{TreeNode{NodeData}}}
    data::Union{Nothing,NodeData}

    # Use this constructor rather than the automatically generated one to pass
    # Test.detect_unbound_args() test in Base.
    function TreeNode{NodeData}(parent::Union{Nothing,TreeNode{NodeData}},
                                children::Union{Nothing,Vector{TreeNode{NodeData}}},
                                data::Union{Nothing,NodeData}) where {NodeData}
        new{NodeData}(parent, children, data)
    end
end

# Exclude parent from hash and equality checks. This means that subtrees can compare equal.
function Base.hash(node::TreeNode, h::UInt)
    h = hash(node.data, h)
    children = node.children
    if children === nothing
        return hash(nothing, h)
    else # optimization - avoid extra allocations from `hash(::AbstractVector, ::UInt)`
        for child in children
            h = hash(child, h)
        end
        return h
    end
end
function Base.:(==)(a::TreeNode{T}, b::TreeNode{T}) where T
    a.children == b.children && a.data == b.data
end

# Implement "pass-through" semantics for field access: access fields of `data`
# as if they were part of `TreeNode`
function Base.getproperty(node::TreeNode, name::Symbol)
    name === :parent && return getfield(node, :parent)
    name === :children && return getfield(node, :children)
    d = getfield(node, :data)
    name === :data && return d
    return getproperty(d, name)
end

function Base.setproperty!(node::TreeNode, name::Symbol, x)
    name === :parent && return setfield!(node, :parent, x)
    name === :children && return setfield!(node, :children, x)
    name === :data && return setfield!(node, :data, x)
    d = getfield(node, :data)
    return setfield!(d, name, x)
end

const AbstractSyntaxNode = TreeNode{<:AbstractSyntaxData}

struct SyntaxData <: AbstractSyntaxData
    source::SourceFile
    raw::GreenNode{SyntaxHead}
    byte_end::UInt32
    val::Any
end
function Base.getproperty(data::SyntaxData, name::Symbol)
    if name === :position
        # Previous versions of JuliaSyntax had `position::Int`.
        # Allow access for compatibility. It was renamed (with changed) semantics
        # to `byte_end::UInt32` to match the rest of the code base, which identified
        # nodes, by their last byte.
        return Int(getfield(data, :byte_end) - getfield(data, :raw).span + UInt32(1))
    end
    return getfield(data, name)
end

Base.hash(data::SyntaxData, h::UInt) =
    hash(data.source, hash(data.raw, hash(data.byte_end,
        # Avoid dynamic dispatch:
        # This does not support custom `hash` implementation that may be defined for `typeof(data.val)`,
        # However, such custom user types should not generally appear in the AST.
        Core.invoke(hash, Tuple{Any,UInt}, data.val, h))))
function Base.:(==)(a::SyntaxData, b::SyntaxData)
    a.source == b.source && a.raw == b.raw && a.byte_end == b.byte_end && a.val === b.val
end

"""
    SyntaxNode(source::SourceFile, cursor::RedTreeCursor,
               raw::GreenNode{SyntaxHead}; keep_parens=false)

A pointer-y AST constructed by removing all trivia from the raw parser output
(see [`RawGreenNode`](@ref)).  The layout of `SyntaxNode` is different from that
of `Expr` due to the invariant that each node's children are in the order they
appeared in the source text.
"""
const SyntaxNode = TreeNode{SyntaxData}

function SyntaxNode(source::SourceFile, cursor::RedTreeCursor;
                    keep_parens=false)
    # Build the full GreenNode tree once upfront (including trivia)
    green = GreenNode(cursor.green)

    GC.@preserve source begin
        raw_offset, txtbuf = _unsafe_wrap_substring(source.code)
        offset = raw_offset - source.byte_offset
        _to_SyntaxNode(source, txtbuf, offset, cursor, green, keep_parens)
    end
end

function SyntaxNode(source::SourceFile, cursor::RedTreeCursor, green::GreenNode{SyntaxHead};
                    keep_parens=false)
    GC.@preserve source begin
        raw_offset, txtbuf = _unsafe_wrap_substring(source.code)
        offset = raw_offset - source.byte_offset
        _to_SyntaxNode(source, txtbuf, offset, cursor, green, keep_parens)
    end
end

should_include_node(child) = !is_trivia(child) || is_error(child)

function _to_SyntaxNode(source::SourceFile, txtbuf::Vector{UInt8}, offset::Int,
                        cursor::RedTreeCursor, green::GreenNode{SyntaxHead}, keep_parens::Bool)
    if is_leaf(cursor)
        # Here we parse the values eagerly rather than representing them as
        # strings. Maybe this is good. Maybe not.
        valrange = byte_range(cursor)
        val = parse_julia_literal(txtbuf, head(cursor), valrange .+ offset)
        return SyntaxNode(nothing, nothing, SyntaxData(source, green, cursor.byte_end, val))
    else
        cs = SyntaxNode[]
        green_children = children(green)

        # We need to match up the filtered SyntaxNode children with the unfiltered GreenNode children
        # Both cursor and green children need to be traversed in the same order
        # Since cursor iterates in reverse, we need to match from the end of green_children
        green_idx = green_children === nothing ? 0 : length(green_children)

        for (i, child_cursor) in enumerate(reverse(cursor))
            if should_include_node(child_cursor)
                pushfirst!(cs, _to_SyntaxNode(source, txtbuf, offset, child_cursor, green[end-i+1], keep_parens))
            end
        end

        if !keep_parens && kind(cursor) == K"parens" && length(cs) == 1
            return cs[1]
        end
        node = SyntaxNode(nothing, cs, SyntaxData(source, green, cursor.byte_end, nothing))
        for c in cs
            c.parent = node
        end
        return node
    end
end

"""
    is_leaf(node)

Determine whether the node is a leaf of the tree. In our trees a "leaf"
corresponds to a single token in the source text.
"""
is_leaf(node::TreeNode) = node.children === nothing

"""
    children(node)

Return an iterable list of children for the node. For leaves, return `nothing`.
"""
children(node::TreeNode) = node.children

"""
    numchildren(node)

Return `length(children(node))` but possibly computed in a more efficient way.
"""
numchildren(node::TreeNode) = (isnothing(node.children) ? 0 : length(node.children))

Base.getindex(node::AbstractSyntaxNode, i::Int) = children(node)[i]
Base.getindex(node::AbstractSyntaxNode, rng::UnitRange) = view(children(node), rng)
Base.firstindex(node::AbstractSyntaxNode) = 1
Base.length(node::AbstractSyntaxNode) = length(children(node))
Base.lastindex(node::AbstractSyntaxNode) = length(node)

function Base.setindex!(node::SN, x::SN, i::Int) where {SN<:AbstractSyntaxNode}
    children(node)[i] = x
end

"""
    head(x)

Get the [`SyntaxHead`](@ref) of a node of a tree or other syntax-related data
structure.
"""
head(node::AbstractSyntaxNode) = head(node.raw)

span(node::AbstractSyntaxNode) = node.raw.span

byte_range(node::AbstractSyntaxNode) = (node.byte_end - span(node) + 1):node.byte_end

first_byte(node::AbstractSyntaxNode) = first(byte_range(node))
last_byte(node::AbstractSyntaxNode) = last(byte_range(node))

sourcefile(node::AbstractSyntaxNode) = node.source

function leaf_string(ex)
    if !is_leaf(ex)
        throw(ArgumentError("leaf_string should be used for leaf nodes only"))
    end
    k = kind(ex)
    value = ex.val
    # TODO: Dispatch on kind extension module (??)
    return k == K"Placeholder" ? "□"*string(value) :
           is_identifier(k)    ? string(value)     :
           value isa Symbol    ? string(value)     : # see parse_julia_literal for other cases which go here
           repr(value)
end

function _show_syntax_node(io, current_filename, node::AbstractSyntaxNode,
                           indent, show_location, show_kind)
    line, col = source_location(node)
    if show_location
        fname = filename(node)
        # Add filename if it's changed from the previous node
        if fname != current_filename[]
            println(io, indent, " -file- │ ", repr(fname))
            current_filename[] = fname
        end
        posstr = "$(lpad(line, 4)):$(rpad(col,3))│$(lpad(first_byte(node),6)):$(rpad(last_byte(node),6))│"
    else
        posstr = ""
    end
    val = node.val
    nodestr = is_leaf(node) ? leaf_string(node) : "[$(untokenize(head(node)))]"
    treestr = string(indent, nodestr)
    if show_kind && is_leaf(node)
        treestr = rpad(treestr, 40)*" :: "*string(kind(node))
    end
    println(io, posstr, treestr)
    if !is_leaf(node)
        new_indent = indent*"  "
        for n in children(node)
            _show_syntax_node(io, current_filename, n, new_indent, show_location, show_kind)
        end
    end
end

function _show_syntax_node_sexpr(io, node::AbstractSyntaxNode, show_kind)
    if is_leaf(node)
        if is_error(node)
            print(io, "(", untokenize(head(node)), ")")
        else
            str = leaf_string(node)
            k = kind(node)
            if is_identifier(k) && !show_kind
                str = lower_identifier_name(str, k)
            end
            print(io, str)
            if show_kind
                print(io, "::", kind(node))
            end
        end
    else
        print(io, "(", untokenize(head(node)))
        for n in children(node)
            print(io, ' ')
            _show_syntax_node_sexpr(io, n, show_kind)
        end
        print(io, ')')
    end
end

function Base.show(io::IO, ::MIME"text/plain", node::AbstractSyntaxNode; show_location=false, show_kind=true)
    println(io, "SyntaxNode:")
    if show_location
        println(io, "line:col│ byte_range  │ tree")
    end
    _show_syntax_node(io, Ref(""), node, "", show_location, show_kind)
end

function Base.show(io::IO, ::MIME"text/x.sexpression", node::AbstractSyntaxNode; show_kind=false)
    _show_syntax_node_sexpr(io, node, show_kind)
end

function Base.show(io::IO, node::AbstractSyntaxNode)
    _show_syntax_node_sexpr(io, node, false)
end

function Base.push!(node::SN, child::SN) where SN<:AbstractSyntaxNode
    if is_leaf(node)
        error("Cannot add children")
    end
    args = children(node)
    push!(args, child)
end

function Base.copy(node::TreeNode)
    # copy the container but not the data (ie, deep copy the tree, shallow copy the data). copy(::Expr) is similar
    # copy "un-parents" the top-level `node` that you're copying
    newnode = typeof(node)(nothing, is_leaf(node) ? nothing : typeof(node)[], copy(node.data))
    if !is_leaf(node)
        for child in children(node)
            newchild = copy(child)
            newchild.parent = newnode
            push!(newnode, newchild)
        end
    end
    return newnode
end

# shallow-copy the data
Base.copy(data::SyntaxData) = SyntaxData(data.source, data.raw, data.byte_end, data.val)

function build_tree(::Type{SyntaxNode}, stream::ParseStream;
                    filename=nothing, first_line=1, keep_parens=false)
    source = SourceFile(stream, filename=filename, first_line=first_line)
    cursor = RedTreeCursor(stream)
    if has_toplevel_siblings(cursor)
        # There are multiple toplevel nodes, e.g. because we're using this
        # to test a partial parse. Wrap everything in K"wrapper"

        # First build the full green tree for all children (including trivia)
        green_children = GreenNode{SyntaxHead}[]
        for child in reverse_toplevel_siblings(cursor)
            pushfirst!(green_children, GreenNode(child.green))
        end

        # Create a wrapper GreenNode with children
        green = GreenNode(SyntaxHead(K"wrapper", NON_TERMINAL_FLAG),
                                  stream.next_byte-1, green_children)

        # Now build SyntaxNodes, iterating through cursors and green nodes together
        cs = SyntaxNode[]
        for (i, child) in enumerate(reverse_toplevel_siblings(cursor))
            if should_include_node(child)
                pushfirst!(cs, SyntaxNode(source, child, green[end-i+1], keep_parens=keep_parens))
            end
        end

        length(cs) == 1 && return only(cs)

        node = SyntaxNode(nothing, cs, SyntaxData(source, green,
                                                   stream.next_byte-1, nothing))
        for c in cs
            c.parent = node
        end
        return node
    else
        return SyntaxNode(source, cursor, keep_parens=keep_parens)
    end
end

@deprecate haschildren(x) !is_leaf(x) false
