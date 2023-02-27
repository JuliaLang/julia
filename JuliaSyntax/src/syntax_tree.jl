#-------------------------------------------------------------------------------
# AST interface, built on top of raw tree

abstract type AbstractSyntaxData end

mutable struct TreeNode{NodeData}   # ? prevent others from using this with NodeData <: AbstractSyntaxData?
    parent::Union{Nothing,TreeNode{NodeData}}
    children::Union{Nothing,Vector{TreeNode{NodeData}}}
    data::Union{Nothing,NodeData}
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

const SyntaxNode = TreeNode{SyntaxData}

# Value of an error node with no children
struct ErrorVal
end

Base.show(io::IO, ::ErrorVal) = printstyled(io, "✘", color=:light_red)

function SyntaxNode(source::SourceFile, raw::GreenNode{SyntaxHead}, position::Integer=1)
    if !haschildren(raw) && !(is_syntax_kind(raw) || is_keyword(raw))
        # Leaf node
        k = kind(raw)
        val_range = position:position + span(raw) - 1
        val_str = view(source, val_range)
        # Here we parse the values eagerly rather than representing them as
        # strings. Maybe this is good. Maybe not.
        #
        # Any errors parsing literals are represented as ErrorVal() - this can
        # happen when the user sets `ignore_errors=true` during parsing.
        val = if k == K"Integer"
            parse_int_literal(val_str)
        elseif k == K"Float"
            v, code = parse_float_literal(Float64, source.code, position,
                                          position+span(raw))
            (code == :ok || code == :underflow) ? v : ErrorVal()
        elseif k == K"Float32"
            v, code = parse_float_literal(Float32, source.code, position,
                                          position+span(raw))
            (code == :ok || code == :underflow) ? v : ErrorVal()
        elseif k in KSet"BinInt OctInt HexInt"
            parse_uint_literal(val_str, k)
        elseif k == K"true"
            true
        elseif k == K"false"
            false
        elseif k == K"Char"
            io = IOBuffer()
            had_error = unescape_julia_string(io, source.code, position,
                                              position+span(raw), Diagnostic[])
            if had_error
                ErrorVal()
            else
                seek(io, 0)
                c = read(io, Char)
                eof(io) ? c : ErrorVal()
            end
        elseif k == K"Identifier"
            if has_flags(head(raw), RAW_STRING_FLAG)
                io = IOBuffer()
                unescape_raw_string(io, val_str, false)
                Symbol(normalize_identifier(String(take!(io))))
            else
                Symbol(normalize_identifier(val_str))
            end
        elseif is_keyword(k)
            # This should only happen for tokens nested inside errors
            Symbol(val_str)
        elseif k in KSet"String CmdString"
            io = IOBuffer()
            had_error = false
            if has_flags(head(raw), RAW_STRING_FLAG)
                unescape_raw_string(io, val_str, k == K"CmdString")
            else
                had_error = unescape_julia_string(io, source.code, position,
                                                  position+span(raw), Diagnostic[])
            end
            had_error ? ErrorVal() : String(take!(io))
        elseif is_operator(k)
            isempty(val_range)  ?
                Symbol(untokenize(k)) : # synthetic invisible tokens
                Symbol(normalize_identifier(val_str))
        elseif k == K"nothing"
            nothing
        elseif k == K"error"
            ErrorVal()
        elseif k == K"MacroName"
            Symbol("@$(normalize_identifier(val_str))")
        elseif k == K"StringMacroName"
            Symbol("@$(normalize_identifier(val_str))_str")
        elseif k == K"CmdMacroName"
            Symbol("@$(normalize_identifier(val_str))_cmd")
        elseif k == K"core_@doc"
            Symbol("core_@doc")
        elseif k == K"core_@cmd"
            Symbol("core_@cmd")
        elseif is_syntax_kind(raw)
            nothing
        else
            # FIXME: this allows us to recover from trivia is_error nodes
            # that we insert below
            @debug "Leaf node of kind $k unknown to SyntaxNode"
            ErrorVal()
        end
        return SyntaxNode(nothing, nothing, SyntaxData(source, raw, position, val))
    else
        cs = SyntaxNode[]
        pos = position
        for (i,rawchild) in enumerate(children(raw))
            # FIXME: Allowing trivia is_error nodes here corrupts the tree layout.
            if !is_trivia(rawchild) || is_error(rawchild)
                push!(cs, SyntaxNode(source, rawchild, pos))
            end
            pos += rawchild.span
        end
        node = SyntaxNode(nothing, cs, SyntaxData(source, raw, position, nothing))
        for c in cs
            c.parent = node
        end
        return node
    end
end

haschildren(node::TreeNode) = node.children !== nothing
children(node::TreeNode) = (c = node.children; return c === nothing ? () : c)


head(node::AbstractSyntaxNode) = head(node.raw)

span(node::AbstractSyntaxNode) = span(node.raw)

first_byte(node::AbstractSyntaxNode) = node.position
last_byte(node::AbstractSyntaxNode)  = node.position + span(node) - 1

"""
    sourcetext(node)

Get the full source text of a node.
"""
function sourcetext(node::AbstractSyntaxNode)
    val_range = (node.position-1) .+ (1:span(node))
    view(node.source, val_range)
end

source_line(node::AbstractSyntaxNode) = source_line(node.source, node.position)
source_location(node::AbstractSyntaxNode) = source_location(node.source, node.position)

function interpolate_literal(node::SyntaxNode, val)
    @assert kind(node) == K"$"
    SyntaxNode(node.source, node.raw, node.position, node.parent, true, val)
end

function _show_syntax_node(io, current_filename, node::AbstractSyntaxNode, indent, show_byte_offsets)
    fname = node.source.filename
    line, col = source_location(node.source, node.position)
    posstr = "$(lpad(line, 4)):$(rpad(col,3))│"
    if show_byte_offsets
        posstr *= "$(lpad(first_byte(node),6)):$(rpad(last_byte(node),6))│"
    end
    val = node.val
    nodestr = haschildren(node) ? "[$(untokenize(head(node)))]" :
              isa(val, Symbol) ? string(val) : repr(val)
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
            _show_syntax_node(io, current_filename, n, new_indent, show_byte_offsets)
        end
    end
end

function _show_syntax_node_sexpr(io, node::AbstractSyntaxNode)
    if !haschildren(node)
        if is_error(node)
            print(io, "(", untokenize(head(node)), ")")
        else
            val = node.val
            print(io, val isa Symbol ? string(val) : repr(val))
        end
    else
        print(io, "(", untokenize(head(node)))
        first = true
        for n in children(node)
            print(io, ' ')
            _show_syntax_node_sexpr(io, n)
            first = false
        end
        print(io, ')')
    end
end

function Base.show(io::IO, ::MIME"text/plain", node::AbstractSyntaxNode; show_byte_offsets=false)
    println(io, "line:col│$(show_byte_offsets ? " byte_range  │" : "") tree                                   │ file_name")
    _show_syntax_node(io, Ref{Union{Nothing,String}}(nothing), node, "", show_byte_offsets)
end

function Base.show(io::IO, ::MIME"text/x.sexpression", node::AbstractSyntaxNode)
    _show_syntax_node_sexpr(io, node)
end

function Base.show(io::IO, node::AbstractSyntaxNode)
    _show_syntax_node_sexpr(io, node)
end

function Base.push!(node::SN, child::SN) where SN<:AbstractSyntaxNode
    if !haschildren(node)
        error("Cannot add children")
    end
    args = children(node)
    push!(args, child)
end

function Base.copy(node::TreeNode)
    # copy the container but not the data (ie, deep copy the tree, shallow copy the data). copy(::Expr) is similar
    # copy "un-parents" the top-level `node` that you're copying
    newnode = typeof(node)(nothing, haschildren(node) ? typeof(node)[] : nothing, copy(node.data))
    for child in children(node)
        newchild = copy(child)
        newchild.parent = newnode
        push!(newnode, newchild)
    end
    return newnode
end

# shallow-copy the data
Base.copy(data::SyntaxData) = SyntaxData(data.source, data.raw, data.position, data.val)

function build_tree(::Type{SyntaxNode}, stream::ParseStream; filename=nothing, first_line=1, kws...)
    green_tree = build_tree(GreenNode, stream; kws...)
    source = SourceFile(sourcetext(stream), filename=filename, first_line=first_line)
    SyntaxNode(source, green_tree, first_byte(stream))
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
    n1.children[path[end]] = x
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
function Base.getindex(node::Union{SyntaxNode,GreenNode}, path::Int...)
    child(node, path...)
end
function Base.lastindex(node::Union{SyntaxNode,GreenNode})
    length(children(node))
end

function Base.setindex!(node::SyntaxNode, x::SyntaxNode, path::Int...)
    setchild!(node, path, x)
end

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

function child_position_span(node::SyntaxNode, path::Int...)
    n = child(node, path...)
    n, n.position, span(n)
end

"""
Print the code, highlighting the part covered by `node` at tree `path`.
"""
function highlight(io::IO, code::String, node, path::Int...; color=(40,40,70))
    node, p, span = child_position_span(node, path...)
    q = p + span
    print(io, code[1:p-1])
    _printstyled(io, code[p:q-1]; bgcolor=color)
    print(io, code[q:end])
end
