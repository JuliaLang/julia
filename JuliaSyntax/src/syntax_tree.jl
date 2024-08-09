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
Base.hash(node::TreeNode, h::UInt) = hash((node.children, node.data), h)
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
    position::Int
    val::Any
end

Base.hash(data::SyntaxData, h::UInt) = hash((data.source, data.raw, data.position, data.val), h)
function Base.:(==)(a::SyntaxData, b::SyntaxData)
    a.source == b.source && a.raw == b.raw && a.position == b.position && a.val == b.val
end

"""
    SyntaxNode(source::SourceFile, raw::GreenNode{SyntaxHead};
               keep_parens=false, position::Integer=1)

An AST node with a similar layout to `Expr`. Typically constructed from source
text by calling one of the parser API functions such as [`parseall`](@ref)
"""
const SyntaxNode = TreeNode{SyntaxData}

function SyntaxNode(source::SourceFile, raw::GreenNode{SyntaxHead};
                    keep_parens=false, position::Integer=1)
    GC.@preserve source begin
        raw_offset, txtbuf = _unsafe_wrap_substring(source.code)
        offset = raw_offset - source.byte_offset
        _to_SyntaxNode(source, txtbuf, offset, raw, convert(Int, position), keep_parens)
    end
end

function _to_SyntaxNode(source::SourceFile, txtbuf::Vector{UInt8}, offset::Int,
                        raw::GreenNode{SyntaxHead},
                        position::Int, keep_parens::Bool)
    if is_leaf(raw)
        # Here we parse the values eagerly rather than representing them as
        # strings. Maybe this is good. Maybe not.
        valrange = position:position + span(raw) - 1
        val = parse_julia_literal(txtbuf, head(raw), valrange .+ offset)
        return SyntaxNode(nothing, nothing, SyntaxData(source, raw, position, val))
    else
        cs = SyntaxNode[]
        pos = position
        for (i,rawchild) in enumerate(children(raw))
            # FIXME: Allowing trivia is_error nodes here corrupts the tree layout.
            if !is_trivia(rawchild) || is_error(rawchild)
                push!(cs, _to_SyntaxNode(source, txtbuf, offset, rawchild, pos, keep_parens))
            end
            pos += Int(rawchild.span)
        end
        if !keep_parens && kind(raw) == K"parens" && length(cs) == 1
            return cs[1]
        end
        if kind(raw) == K"wrapper" && length(cs) == 1
            return cs[1]
        end
        node = SyntaxNode(nothing, cs, SyntaxData(source, raw, position, nothing))
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
Base.lastindex(node::AbstractSyntaxNode) = length(children(node))

function Base.setindex!(node::SN, x::SN, i::Int) where {SN<:AbstractSyntaxNode}
    children(node)[i] = x
end

"""
    head(x)

Get the [`SyntaxHead`](@ref) of a node of a tree or other syntax-related data
structure.
"""
head(node::AbstractSyntaxNode) = head(node.raw)

span(node::AbstractSyntaxNode) = span(node.raw)

byte_range(node::AbstractSyntaxNode) = node.position:(node.position + span(node) - 1)

sourcefile(node::AbstractSyntaxNode) = node.source

function leaf_string(ex)
    if !is_leaf(ex)
        throw(ArgumentError("_value_string should be used for leaf nodes only"))
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
            print(io, leaf_string(node))
            if show_kind
                print(io, "::", kind(node))
            end
        end
    else
        print(io, "(", untokenize(head(node)))
        first = true
        for n in children(node)
            print(io, ' ')
            _show_syntax_node_sexpr(io, n, show_kind)
            first = false
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
Base.copy(data::SyntaxData) = SyntaxData(data.source, data.raw, data.position, data.val)

function build_tree(::Type{SyntaxNode}, stream::ParseStream;
                    filename=nothing, first_line=1, keep_parens=false, kws...)
    green_tree = build_tree(GreenNode, stream; kws...)
    source = SourceFile(stream, filename=filename, first_line=first_line)
    SyntaxNode(source, green_tree, position=first_byte(stream), keep_parens=keep_parens)
end

@deprecate haschildren(x) !is_leaf(x) false
